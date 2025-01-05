pub trait Bus {
    fn read_byte(&mut self, addr: u16) -> u8;
    fn read_word(&mut self, addr: u16) -> u16;
    fn read_addr(&mut self, addr: u16) -> u16;
    fn write_byte(&mut self, addr: u16, data: u8);
    fn write_word(&mut self, addr: u16, data: u16);
}

const WRAM_START: u16 = 0x0000;
const WRAM_END: u16 = 0x1fff;
const PPUREG_START: u16 = 0x2000;
const PPUREG_END: u16 = 0x3fff;
const APUREG_START: u16 = 0x4000;
const APUREG_END: u16 = 0x401f;
const PRG_START: u16 = 0x4020;
const PRG_END: u16 = 0xffff;

pub struct NesBus {
    wram: [u8; 0x800],
    //todo: ppu apu and io registers
    prg: [u8; 0xbfe0],
}

impl Bus for NesBus {
    fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    WRAM_START ..= WRAM_END =>
		self.wram[(addr & 0x7fff) as usize],
	    PPUREG_START ..= PPUREG_END =>
		0, //todo
	    APUREG_START ..= APUREG_END =>
		0, //todo
	    PRG_START ..= PRG_END =>
		self.prg[(addr - PRG_START) as usize] //todo: mappers etc
	}
    }

    fn read_word(&mut self, addr: u16) -> u16 {
	let result: u16 = self.read_byte(addr) as u16 | ((self.read_byte(addr + 1) as u16) << 8);
	result
    }

    fn read_addr(&mut self, addr: u16) -> u16 {
	let mut addr = addr;
	let lo = self.read_byte(addr);
	if addr < 0x100 { //zero page word reads work different
	    addr = (addr + 1) % 0x100;
	} else {
	    addr = addr + 1;
	}
	let hi = self.read_byte(addr);
	(hi as u16) << 8 | lo as u16
    }

    fn write_byte(&mut self, addr: u16, data: u8) {
	match addr {
	    WRAM_START ..= WRAM_END =>
		self.wram[(addr & 0x7fff) as usize] = data,
	    PPUREG_START ..= PPUREG_END =>
	    {}, //todo
	    APUREG_START ..= APUREG_END =>
	    {}, //todo
	    PRG_START ..= PRG_END =>
		println!("Attempted write to PRG ROM at {addr}"), //todo: mappers etc
	};
    }

    fn write_word(&mut self, addr: u16, data: u16) {
	self.write_byte(addr, (data >> 8) as u8);
	self.write_byte(addr + 1, (data & 0x00ff) as u8);
    }
}

impl NesBus {
    pub fn new() -> Self {
	NesBus {
	    wram: [0; 0x800], //todo: not initialized to zero
	    prg: [0; 0xbfe0],
	}
    }

    pub fn load_prg(&mut self, prg: &[u8], size: usize) {
	for i in 0..size {
	    self.prg[(0x8000 - PRG_START) as usize + i] = prg[i];
	}

	for i in 0..size {
	    self.prg[(0xc000 - PRG_START) as usize + i] = self.prg[(0x8000 - PRG_START) as usize + i];
	}
    }
}
