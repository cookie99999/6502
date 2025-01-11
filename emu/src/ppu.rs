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
	}
    }

    pub fn write_byte(&mut self, addr: u16, byte: u8) {
	match addr {
	    CHR_START ..= CHR_END => //todo: only allow if chr ram
		self.chr[addr as usize] = byte,
	    VRAM_START ..= VRAM_END =>
		self.vram[(addr - VRAM_START) as usize] = byte,
	    PAL_START ..= PAL_END =>
		self.palette[(addr - PAL_START) as usize % 0x20] = byte,
	    _ =>
		println!("ppu write {addr:04x} todo"),
	};
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
	match addr {
	    CHR_START ..= CHR_END =>
		self.chr[addr as usize],
	    VRAM_START ..= VRAM_END =>
		self.vram[(addr - VRAM_START) as usize],
	    PAL_START ..= PAL_END =>
		self.palette[(addr - PAL_START) as usize % 0x20],
	    _ =>
		0,
	}
    }

    pub fn write_reg(&mut self, addr: u16, byte: u8) {
	match addr {
	    0x2000 => { //PPUCTRL
		self.ppuctrl = byte;
		println!("ppuctrl set to {:02X}", self.ppuctrl);
	    },
	    0x2006 => //PPUADDR
		self.write_ppuaddr(byte),
	    0x2007 => { //PPUDATA
		self.write_byte(self.ppuaddr, byte);
		self.ppuaddr += 1; //todo use ppuctrl value
	    },
	    _ =>
		println!("ppu reg write {addr:04x} todo"),
	};
    }

    pub fn read_reg(&mut self, addr: u16) -> u8 {
	match addr {
	    0x2002 => { //PPUSTATUS
		self.write_2 = false;
		self.ppustatus
	    },
	    0x2007 => { //PPUDATA
		let result = self.latch;
		println!("latch was {:02X}", self.latch);
		self.latch = self.read_byte(self.ppuaddr);
		println!("latch is now {:02X} from {:04X}", self.latch, self.ppuaddr);
		self.ppuaddr += 1; //todo use value from ppuctrl
		println!("read {result:02X} from ppu");
		result
	    },
	    _ =>
		0,
	}
    }

    fn write_ppuaddr(&mut self, data: u8) {
	if self.write_2 {
	    self.ppuaddr &= 0xff00;
	    self.ppuaddr |= data as u16;
	    println!("ppuaddr set to {:04X}", self.ppuaddr);
	} else {
	    self.ppuaddr &= 0x00ff;
	    self.ppuaddr |= (data as u16) << 8;
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
}
