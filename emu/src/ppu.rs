const CHR_START: u16 = 0x0000;
const CHR_END: u16 = 0x1fff;
const VRAM_START: u16 = 0x2000;
const VRAM_END: u16 = 0x2fff;
const PAL_START: u16 = 0x3f00;
const PAL_END: u16 = 0x3fff;

pub struct Ppu {
    vram: [u8; 0x1000],
    chr: [u8; 0x2000],
    palette: [u8; 0x20],
    oam: [u8; 0x100],
}

impl Ppu {
    pub fn new() -> Self {
	Ppu {
	    vram: [0; 0x1000],
	    chr: [0; 0x2000],
	    palette: [0; 0x20],
	    oam: [0; 0x100],
	}
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
	match addr {
	    CHR_START ..= CHR_END =>
		self.chr[addr as usize] = byte,
	    _ =>
		println!("ppu write {addr:04x} todo"),
	};
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    CHR_START ..= CHR_END =>
		self.chr[addr as usize],
	    _ =>
		0,
	}
    }
}
