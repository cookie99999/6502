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
    let window = video.window("emu6502 NES", width, height)
	.position_centered()
	.opengl()
	.build()
	.unwrap();
    let mut canvas = window.into_canvas().build().unwrap();
    canvas.clear();

    let mut event_pump = context.event_pump().unwrap();

    cpu.bus.ppu.dump_chr();
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
	draw_screen(&mut cpu.bus.ppu, &mut canvas);
	canvas.present();
    }
}
