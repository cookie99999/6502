mod cpu;
mod bus;

use std::env;
use crate::bus::Bus;

fn main() {
    let mut cpu = cpu::Cpu::new();

    let path = env::args().nth(1).expect("Usage: emu6502 <path>");
    let buf: Vec<u8> = std::fs::read(path).unwrap();
    let ines_prg_sz: usize = buf[4] as usize * 0x4000;
    cpu.bus.load_prg(&buf[16..], ines_prg_sz);
    cpu.pc = 0xc000; //reset to headless start
    'running: loop {
	if cpu.step() {
	    break 'running;
	}
    }
    println!("$0002 = {:02X} $0003 = {:02X}", cpu.bus.read_byte(0x0002), cpu.bus.read_byte(0x0003));
}
