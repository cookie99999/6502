use crate::ppu;

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

#[derive(Debug)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    Both,
}

pub enum Mapper {
    NROM,
    Unsupported,
}

pub struct NesBus {
    mirroring: Mirroring,
    mapper: Mapper,
    has_sram: bool,
    has_trainer: bool,
    alt_nametable: bool,
    wram: [u8; 0x800],
    //todo: ppu apu and io registers
    prg: [u8; 0xbfe0],
    pub ppu: ppu::Ppu,
}

impl Bus for NesBus {
    fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    WRAM_START ..= WRAM_END =>
		self.wram[(addr & 0x7fff) as usize],
	    PPUREG_START ..= PPUREG_END =>
		self.ppu.read_reg(addr),
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
		self.ppu.write_reg(addr, data),
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
	    mirroring: Mirroring::Vertical,
	    mapper: Mapper::NROM,
	    has_sram: false,
	    has_trainer: false,
	    alt_nametable: false,
	    wram: [0; 0x800], //todo: not initialized to zero
	    prg: [0; 0xbfe0],
	    ppu: ppu::Ppu::new(),
	}
    }

    fn load_chr(&mut self, buf: &[u8], size: usize) {
	for i in 0..size {
	    self.ppu.write_byte(i as u16, buf[i]);
	}
    }
    
    pub fn load_ines(&mut self, buf: &[u8]) {
	match buf[0..4] {
	    [0x4e, 0x45, 0x53, 0x1a] =>
		println!("valid iNES"),
	    _ => println!("invalid iNES"),
	};
	let prg_sz: usize = buf[4] as usize * 0x4000;
	let chr_sz: usize = buf[5] as usize * 0x2000;
	let fl6 = buf[6];
	let fl7 = buf[7];
	self.mirroring = match fl6 & 1 {
	    0 => Mirroring::Vertical,
	    1 => Mirroring::Horizontal,
	    _ => Mirroring::Vertical, //fix
	};

	self.has_sram = ((fl6 & 2) >> 1) != 0;
	self.has_trainer = ((fl6 & 4) >> 2) != 0;
	self.alt_nametable = ((fl6 & 8) >> 3) != 0;
	let mut mapper = (fl6 & 0xf0) >> 4;
	mapper |= fl7 & 0xf0;
	self.mapper = match mapper {
	    0 => Mapper::NROM,
	    _ => Mapper::Unsupported,
	};
	println!("mapper {mapper} prg size {prg_sz:04X} chr size {chr_sz:04X} mirroring {:?}", self.mirroring);

	let buf_prg_start = match self.has_trainer {
	    true => 16 + 512,
	    false => 16,
	};

	//todo: load depending on mapper
	for i in 0..prg_sz {
	    self.prg[(0x8000 - PRG_START) as usize + i] = buf[i + buf_prg_start as usize];
	}

	for i in 0..prg_sz {
	    self.prg[(0xc000 - PRG_START) as usize + i] = self.prg[(0x8000 - PRG_START) as usize + i];
	}

	let chr_start = buf_prg_start + prg_sz;
	self.load_chr(&buf[chr_start..], chr_sz);
    }	
}
