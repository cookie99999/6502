const CHR_START: u16 = 0x0000;
const CHR_END: u16 = 0x1fff;
const VRAM_START: u16 = 0x2000;
const VRAM_END: u16 = 0x2fff;
const PAL_START: u16 = 0x3f00;
const PAL_END: u16 = 0x3fff;

pub struct Ppu {
    pub vram: [u8; 0x1000],
    pub chr: [u8; 0x2000],
    pub palette: [u8; 0x20],
    pub oam: [u8; 0x100],
    framebuffer: [u32; 256 * 240],
}

impl Ppu {
    pub fn new() -> Self {
	Ppu {
	    vram: [0; 0x1000],
	    chr: [0; 0x2000],
	    palette: [0; 0x20],
	    oam: [0; 0x100],
	    framebuffer: [0; 256 * 240],
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

    pub fn write_pixel(&mut self, x: u8, y: u8, pix: u32) {
	self.framebuffer[(y as usize % 240) * 256 + (x as usize % 256)] = pix;
    }

    pub fn read_pixel(&mut self, x: u8, y: u8) -> u32 {
	self.framebuffer[(y as usize % 240) * 256 + (x as usize % 256)]
    }

    pub fn draw_tile(&mut self, x: u8, y: u8, tile: u8, table: u8) {
	let table: u8 = match table {
	    0 => 0,
	    _ => 1,
	};
	let mut p0_addr: u16 = (tile as u16) << 4;
	p0_addr |= (table as u16) << 12;
	let p1_addr: u16 = p0_addr | (1u16 << 3);

	for i in 0..8 {
	    let p0 = self.read_byte(p0_addr + i as u16);
	    let p1 = self.read_byte(p1_addr + i as u16);
	    for (b, b2) in (0..=7).rev().enumerate() {
		let c = ((p0 >> b2) & 1) | (((p1 >> b2) & 1) << 1);
		let px: u32 = match c {
		    0 => 0x000000,
		    1 => 0x333333,
		    2 => 0x888888,
		    _ => 0xdddddd,
		};
		self.write_pixel(x + b as u8, y + i as u8, px);
	    }
	}
    }

    pub fn dump_chr(&mut self) {
	for x in 0..32 {
	    for y in 0..16 {
		self.draw_tile(x * 8, y * 8, (y * 16) + (x % 16), (x >= 16) as u8);
	    }
	}
    }
}
