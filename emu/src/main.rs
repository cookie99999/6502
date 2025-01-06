mod cpu;
mod bus;
mod ppu;

use std::env;
use crate::bus::Bus;

fn main() {
    let mut cpu = cpu::Cpu::new();

    let path = env::args().nth(1).expect("Usage: emu6502 <path>");
    let buf: Vec<u8> = std::fs::read(path).unwrap();
    cpu.bus.load_ines(&buf);
    cpu.pc = 0xc000; //reset to headless start
    return;
    'running: loop {
	if cpu.step() {
	    break 'running;
	}
    }
    println!("$0002 = {:02X} $0003 = {:02X}", cpu.bus.read_byte(0x0002), cpu.bus.read_byte(0x0003));
}
