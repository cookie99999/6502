mod cpu;
mod bus;
mod ppu;

use std::env;
use sdl2::pixels::PixelFormatEnum;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

fn draw_screen(ppu: &mut ppu::Ppu, tex: &mut sdl2::render::Texture) {
    tex.with_lock(None, |buf: &mut [u8], pitch: usize| {
	for x in (0..=255).enumerate() {
	    for y in (0..240).enumerate() {
		let px = ppu.read_pixel(x.1, y.1);
		let offset = y.1 as usize * pitch + x.1 as usize * 3;
		buf[offset] = (px >> 16) as u8;
		buf[offset + 1] = (px >> 8) as u8;
		buf[offset + 2] = px as u8;
	    }
	}
    }).unwrap();
}

const SYS_PALETTE: [u32; 64] = [
    0x585858, 0x00237C, 0x0D1099, 0x300092, 0x4F006C, 0x600035,
    0x5C0500, 0x461800, 0x272D00, 0x093E00, 0x004500, 0x004106,
    0x003545, 0x000000, 0x000000, 0x000000, 0xA1A1A1, 0x0B53D7,
    0x3337FE, 0x6621F7, 0x9515BE, 0xAC166E, 0xA62721, 0x864300,
    0x596200, 0x2D7A00, 0x0C8500, 0x007F2A, 0x006D85, 0x000000,
    0x000000, 0x000000, 0xFFFFFF, 0x51A5FE, 0x8084FE, 0xBC6AFE,
    0xF15BFE, 0xFE5EC4, 0xFE7269, 0xE19321, 0xADB600, 0x79D300,
    0x51DF21, 0x3AD974, 0x39C3DF, 0x424242, 0x000000, 0x000000,
    0xFFFFFF, 0xB5D9FE, 0xCACAFE, 0xE3BEFE, 0xF9B8FE, 0xFEBAE7,
    0xFEC3BC, 0xF4D199, 0xDEE086, 0xC6EC87, 0xB2F29D, 0xA7F0C3,
    0xA8E7F0, 0xACACAC, 0x000000, 0x000000,
];

fn dump_tile(ppu: &mut ppu::Ppu, tex: &mut sdl2::render::Texture, x: u32, y: u32, tile: u8, table: u8, pal: u8) {
    let table: u8 = match table {
	    0 => 0,
	    _ => 1,
	};
	let mut p0_addr: u16 = (tile as u16) << 4;
	p0_addr |= (table as u16) << 12;
	let p1_addr: u16 = p0_addr | (1u16 << 3);
    tex.with_lock(None, |buf: &mut [u8], pitch: usize| {
	for i in 0..8 {
	    let p0 = ppu.read_byte(p0_addr + i as u16);
	    let p1 = ppu.read_byte(p1_addr + i as u16);
	    for (b, b2) in (0..=7).rev().enumerate() {
		let c = ((p0 >> b2) & 1) | (((p1 >> b2) & 1) << 1);
		let palbase: u16 = 0x3f00 + 1 + (pal * 4) as u16;
		let px: u32 = match c { //todo: intensity bits
		    0 => SYS_PALETTE[ppu.read_byte(0x3f00) as usize % 64],
		    1 => SYS_PALETTE[ppu.read_byte(palbase) as usize % 64],
		    2 => SYS_PALETTE[ppu.read_byte(palbase + 1) as usize % 64],
		    _ => SYS_PALETTE[ppu.read_byte(palbase + 2) as usize % 64],
		};
		let offset = (y as usize + i) * pitch + (x as usize + b) * 3;
		buf[offset] = (px >> 16) as u8;
		buf[offset + 1] = (px >> 8) as u8;
		buf[offset + 2] = px as u8;
	    }
	}
    }).unwrap();
}
    
fn dump_chr(ppu: &mut ppu::Ppu, tex: &mut sdl2::render::Texture) {
    for x in 0..32 {
	for y in 0..16 {
	    dump_tile(ppu, tex, x * 8, y * 8, (y as u8 * 16) + (x as u8 % 16), (x >= 16) as u8, 0);
	}
    }
}

fn dump_nt(ppu: &mut ppu::Ppu, tex: &mut sdl2::render::Texture) {
    let base = 0x2000;
    for i in 0..4 {
	for j in 0..0x3c0 {
	    let index = base + (i * 0x400) + j;
	    let tile = ppu.read_byte(index);
	    let mut tile_x = j % 32;
	    let mut tile_y = j / 32;
	    let attr_index = (tile_y / 4) * 8 + (tile_x / 4);
	    let attr_byte = ppu.read_byte(base + 0x3c0 + attr_index);
	    let pal_num = match (tile_x % 4 / 2, tile_y % 4 / 2) {
		(0, 0) => attr_byte & 3,
		(1, 0) => (attr_byte >> 2) & 3,
		(0, 1) => (attr_byte >> 4) & 3,
		(1, 1) => (attr_byte >> 6) & 3,
		(_, _) => panic!("impossible math anomaly o_O"),
	    };
	    if i == 1 || i == 3 {
		tile_x += 32;
	    }
	    if i == 2 || i == 3 {
		tile_y += 30;
	    }
	    dump_tile(ppu, tex, tile_x as u32 * 8, tile_y as u32 * 8, tile, 1, pal_num);
	}
    }
}

fn main() {
    let mut cpu = cpu::Cpu::new();

    let path = env::args().nth(1).expect("Usage: emu6502 <path>");
    let buf: Vec<u8> = std::fs::read(path).unwrap();
    cpu.bus.load_ines(&buf);
    cpu.reset(true);

    let context = sdl2::init().unwrap();
    let video = context.video().unwrap();
    let width = 256 * 3;
    let height = 240 * 3;
    let main_win = video.window("emu6502 NES", width, height)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut main_canvas = main_win.into_canvas().build().unwrap();
    let main_tex_create = main_canvas.texture_creator();
    let mut main_tex = main_tex_create
	.create_texture_streaming(PixelFormatEnum::RGB24, 256, 240)
	.unwrap();

    let chr_win = video.window("Pattern Tables", 8 * 16 * 3 * 2, 8 * 16 * 3)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut chr_canv = chr_win.into_canvas().build().unwrap();
    let chr_tex_create = chr_canv.texture_creator();
    let mut chr_tex = chr_tex_create
	.create_texture_streaming(PixelFormatEnum::RGB24, 8*16*2, 8*16)
	.unwrap();

    let nt_win = video.window("Nametables", 512, 480)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut nt_canv = nt_win.into_canvas().build().unwrap();
    let nt_tex_create = nt_canv.texture_creator();
    let mut nt_tex = nt_tex_create
	.create_texture_streaming(PixelFormatEnum::RGB24, 512, 480)
	.unwrap();

    let mut event_pump = context.event_pump().unwrap();

    main_canvas.clear();
    main_canvas.present();
    chr_canv.clear();
    dump_chr(&mut cpu.bus.ppu, &mut chr_tex);
    chr_canv.copy(&chr_tex, None, None).unwrap();
    chr_canv.present();
    nt_canv.clear();

    'running: loop {
	for e in event_pump.poll_iter() {
	    match e {
		Event::Quit {..} |
		Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
		    break 'running;
		},
		_ => {},
	    }
	}

	let cyc = cpu.step();
	if cyc == 0 {
	    break 'running;
	}
	cpu.bus.step(cyc);
	if cpu.nmi_left {
	    dump_chr(&mut cpu.bus.ppu, &mut chr_tex);
	    chr_canv.copy(&chr_tex, None, None).unwrap();
	    chr_canv.present();
	    dump_nt(&mut cpu.bus.ppu, &mut nt_tex);
	    nt_canv.copy(&nt_tex, None, None).unwrap();
	    nt_canv.present();
	    cpu.nmi_left = false;
	}
	//draw_screen(&mut cpu.bus.ppu, &mut main_tex);
	//main_canvas.copy(&main_tex, None, None).unwrap();
	//main_canvas.present();
    }
}
