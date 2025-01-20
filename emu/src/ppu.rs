use crate::bus::Mirroring;

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
    ppuaddr: u16,
    ppustatus: u8,
    ppuctrl: u8,
    ppumask: u8,
    oamaddr: u8,
    ppuscroll_x: u8,
    ppuscroll_y: u8,
    oamdma: u8,
    latch: u8,
    write_2: bool,
    pub int_pending: bool,
    pub nmi_pending: bool,
    pub scanline: u16,
    pub pixel: u16,
    pub mirroring: Mirroring,
}

impl Ppu {
    pub fn new() -> Self {
	Ppu {
	    vram: [0; 0x1000],
	    chr: [0; 0x2000],
	    palette: [0; 0x20],
	    oam: [0; 0x100],
	    framebuffer: [0; 256 * 240],
	    ppuaddr: 0,
	    ppustatus: 0x80,
	    ppuctrl: 0,
	    ppumask: 0,
	    oamaddr: 0,
	    ppuscroll_x: 0,
	    ppuscroll_y: 0,
	    oamdma: 0,
	    latch: 0,
	    write_2: false,
	    int_pending: false,
	    nmi_pending: false,
	    scanline: 0,
	    pixel: 21,
	    mirroring: Mirroring::Vertical,
	}
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
	match addr {
	    CHR_START ..= CHR_END => //todo: only allow if chr ram
	    //self.chr[addr as usize] = byte,
		println!("attempted write to chr at {addr:04X}"),
	    0x2000 ..= 0x23ff =>
		self.vram[(addr - VRAM_START) as usize] = byte,
	    0x2400 ..= 0x27ff => {
		if self.mirroring == Mirroring::Horizontal {
		    self.vram[(addr - VRAM_START - 0x400) as usize] = byte;
		} else {
		    self.vram[(addr - VRAM_START) as usize] = byte;
		}
	    },
	    0x2800 ..= 0x2bff => {
		if self.mirroring == Mirroring::Vertical {
		    self.vram[(addr - VRAM_START - 0x800) as usize] = byte;
		} else {
		    self.vram[(addr - VRAM_START) as usize] = byte;
		}
	    },
	    0x2c00 ..= 0x2fff => {
		if self.mirroring == Mirroring::Horizontal {
		    self.vram[(addr - VRAM_START - 0x400) as usize] = byte;
		} else {
		    self.vram[(addr - VRAM_START - 0x800) as usize] = byte;
		}
	    },
	    0x3000 ..= 0x3eff => //mirror of vram according to nesdev
		self.vram[(addr - 0x3000) as usize] = byte,
	    PAL_START ..= PAL_END =>
		self.write_pal(addr - PAL_START, byte),
	    _ =>
		todo!("unmatched ppu write {addr:04x}"),
	};
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    CHR_START ..= CHR_END =>
		self.chr[addr as usize],
	    0x2000 ..= 0x23ff =>
		self.vram[(addr - VRAM_START) as usize],
	    0x2400 ..= 0x27ff => {
		if self.mirroring == Mirroring::Horizontal {
		    self.vram[(addr - VRAM_START - 0x400) as usize]
		} else {
		    self.vram[(addr - VRAM_START) as usize]
		}
	    },
	    0x2800 ..= 0x2bff => {
		if self.mirroring == Mirroring::Vertical {
		    self.vram[(addr - VRAM_START - 0x800) as usize]
		} else {
		    self.vram[(addr - VRAM_START) as usize]
		}
	    },
	    0x2c00 ..= 0x2fff => {
		if self.mirroring == Mirroring::Horizontal {
		    self.vram[(addr - VRAM_START - 0x400) as usize]
		} else {
		    self.vram[(addr - VRAM_START - 0x800) as usize]
		}
	    },
	    0x3000 ..= 0x3eff => //mirror of vram according to nesdev
		self.vram[(addr - 0x3000) as usize],
	    PAL_START ..= PAL_END =>
		self.read_pal(addr - PAL_START),
	    _ =>
		todo!("unmatched ppu read at {addr:04X}"),
	}
    }

    pub fn write_oam(&mut self, addr: usize, data: u8) {
	self.oam[addr] = data;
    }

    pub fn read_oam(&mut self, addr: usize) -> u8 {
	self.oam[addr]
    }

    pub fn write_pal(&mut self, addr: u16, data: u8) {
	let addr = addr % 0x20;
	if (addr & 0xf) == 0 { //bg and sprite color 0 are shared
	    self.palette[0] = data;
	    return;
	}
	self.palette[addr as usize] = data;
    }

    pub fn read_pal(&mut self, addr: u16) -> u8 {
	let addr = addr % 0x20;
	if (addr & 0xf) == 0 {
	    return self.palette[0];
	}
	self.palette[addr as usize]
    }

    pub fn write_reg(&mut self, addr: u16, byte: u8) {
	match addr {
	    0x2000 => { //PPUCTRL
		self.ppuctrl = byte;
	    },
	    0x2001 => //PPUMASK
		self.ppumask = byte,
	    0x2003 => //OAMADDR
		self.oamaddr = byte,
	    0x2005 => //PPUSCROLL
		self.write_ppuscroll(byte),
	    0x2006 => //PPUADDR
		self.write_ppuaddr(byte),
	    0x2007 => { //PPUDATA
		self.write_byte(self.ppuaddr, byte);
		if (self.ppuctrl & 4) == 0 {
		    self.ppuaddr += 1;
		} else {
		    self.ppuaddr += 32;
		}
	    },
	    _ => //OAMDMA is handled in the cpu
		todo!("unmatched ppu reg write at {addr:04X}"),
	};
    }

    pub fn read_reg(&mut self, addr: u16) -> u8 {
	match addr {
	    0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => //write only registers
		self.latch,
	    0x2002 => { //PPUSTATUS
		self.write_2 = false;
		let tmp = self.ppustatus;
		//self.ppustatus = self.ppustatus & !0x80; //vblank flag cleared on read
		tmp
	    },
	    0x2007 => { //PPUDATA
		let result = self.latch;
		self.latch = self.read_byte(self.ppuaddr);
		if (self.ppuctrl & 4) == 0 {
		    //self.ppuaddr += 1;
		} else {
		    //self.ppuaddr += 32;
		}
		result
	    },
	    _ =>
		todo!("unmatched ppu reg read at {addr:04X}"),
	}
    }

    fn write_ppuaddr(&mut self, data: u8) {
	if self.write_2 {
	    self.ppuaddr &= 0xff00;
	    self.ppuaddr |= data as u16;
	} else {
	    self.ppuaddr &= 0x00ff;
	    self.ppuaddr |= (data as u16) << 8;
	}
	self.write_2 = !self.write_2;
    }

    fn write_ppuscroll(&mut self, data: u8) {
	if self.write_2 {
	    self.ppuscroll_y = data;
	} else {
	    self.ppuscroll_x = data;
	}
	self.write_2 = !self.write_2;
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

    pub fn step(&mut self, cyc: u128) {
	self.pixel += cyc as u16;
	if self.pixel >= 341 {
	    self.pixel = self.pixel - 341;
	    self.scanline += 1;

	    if self.scanline == 241 && self.pixel > 0 {
		if (self.ppuctrl & 0x80) != 0 {
		    self.nmi_pending = true;
		    self.ppustatus = self.ppustatus | 0x80;
		}
	    }

	    if self.scanline >= 262 {
		self.scanline = 0;
		self.ppustatus = self.ppustatus & !0x80;
	    }
	}
    }
}
