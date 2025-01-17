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

fn dump_tile(ppu: &mut ppu::Ppu, tex: &mut sdl2::render::Texture, x: u32, y: u32, tile: u8, table: u8) {
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
		let px: u32 = match c {
		    0 => 0x000000,
		    1 => 0x333333,
		    2 => 0x888888,
		    _ => 0xdddddd,
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
	    dump_tile(ppu, tex, x * 8, y * 8, (y as u8 * 16) + (x as u8 % 16), (x >= 16) as u8);
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

    let mut event_pump = context.event_pump().unwrap();

    main_canvas.clear();
    main_canvas.present();
    chr_canv.clear();
    dump_chr(&mut cpu.bus.ppu, &mut chr_tex);
    chr_canv.copy(&chr_tex, None, None).unwrap();
    chr_canv.present();

    cpu.pc = 0xc000;
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
	//draw_screen(&mut cpu.bus.ppu, &mut main_tex);
	//main_canvas.copy(&main_tex, None, None).unwrap();
	//main_canvas.present();
    }
}
