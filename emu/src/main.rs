mod cpu;
mod bus;
mod ppu;

use std::env;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use crate::bus::Bus;

fn draw_screen(ppu: &mut ppu::Ppu, canvas: &mut sdl2::render::WindowCanvas) {
    for x in (0..=255).enumerate() {
	for y in (0..240).enumerate() {
	    let c: Color = Color::from_u32(&sdl2::pixels::PixelFormat::try_from(sdl2::pixels::PixelFormatEnum::RGBA8888).unwrap(), ppu.read_pixel(x.1, y.1));
	    canvas.set_draw_color(c);
	    canvas.fill_rect(Rect::new(x.1 as i32 * 3, y.1 as i32 * 3, 3, 3)).unwrap();
	}
    }
}

fn dump_tile(ppu: &mut ppu::Ppu, canvas: &mut sdl2::render::WindowCanvas, x: u32, y: u32, tile: u8, table: u8) {
    let table: u8 = match table {
	    0 => 0,
	    _ => 1,
	};
	let mut p0_addr: u16 = (tile as u16) << 4;
	p0_addr |= ((table as u16) << 12);
	let p1_addr: u16 = p0_addr | (1u16 << 3);

    for i in 0..8 {
	let p0 = ppu.read_byte(p0_addr + i as u16);
	let p1 = ppu.read_byte(p1_addr + i as u16);
	for (b, b2) in (0..=7).rev().enumerate() {
	    let c = ((p0 >> b2) & 1) | (((p1 >> b2) & 1) << 1);
	    let px: u32 = match c {
		0 => 0x000000ff,
		1 => 0x333333ff,
		2 => 0x888888ff,
		_ => 0xddddddff,
		};
	    let c: Color = Color::from_u32(&sdl2::pixels::PixelFormat::try_from(sdl2::pixels::PixelFormatEnum::RGBA8888).unwrap(), px);
	    canvas.set_draw_color(c);
	    canvas.fill_rect(Rect::new((x as i32 + b as i32) * 3, (y as i32 + i as i32) * 3, 3, 3)).unwrap();
	}
    }
}
    
fn dump_chr(ppu: &mut ppu::Ppu, canvas: &mut sdl2::render::WindowCanvas) {
    for x in 0..32 {
	for y in 0..16 {
	    dump_tile(ppu, canvas, x * 8, y * 8, (y as u8 * 16) + (x as u8 % 16), (x >= 16) as u8);
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
    main_canvas.clear();

    let chr_win = video.window("Pattern Tables", 8 * 16 * 3 * 2, 8 * 16 * 3)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut chr_canv = chr_win.into_canvas().build().unwrap();
    chr_canv.clear();

    let mut event_pump = context.event_pump().unwrap();

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
	
	if cpu.step() {
	    break 'running;
	}
	draw_screen(&mut cpu.bus.ppu, &mut main_canvas);
	dump_chr(&mut cpu.bus.ppu, &mut chr_canv);
	main_canvas.present();
	chr_canv.present();
    }
}
