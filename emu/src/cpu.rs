extern crate bitflags;

use crate::bus;
use crate::bus::Bus;

#[derive(Debug, PartialEq)]
enum AddrMode {
    Impl, Imm, Abs, ZP,
    AbsX, AbsY, ZPX, ZPY,
    Ind, XInd, IndY, Rel, Acc,
}

struct Instruction {
    opcode: u8,
    bytes: u8,
    cycles: u8,
    mode: AddrMode,
    mnemonic: &'static str,
}

macro_rules! instr_set {
    ($({ $o: expr, $b: expr, $c: expr, $m: expr, $mn: expr }),* $(,)?) => {
	[
	    $(Instruction { opcode: $o, bytes: $b, cycles: $c, mode: $m, mnemonic: $mn }),*
	]
    };
}

const INSTR_SET_NMOS: [Instruction; 256] = instr_set![
    {0x00, 1, 7, AddrMode::Impl, "BRK"}, {0x01, 2, 6, AddrMode::XInd, "ORA"}, {0x02, 1, 1, AddrMode::Impl, "*JAM"}, {0x03, 2, 8, AddrMode::XInd, "*SLO"},
  {0x04, 2, 3, AddrMode::ZP, "*NOP"}, {0x05, 2, 3, AddrMode::ZP, "ORA"}, {0x06, 2, 5, AddrMode::ZP, "ASL"}, {0x07, 2, 5, AddrMode::ZP, "*SLO"},
  {0x08, 1, 3, AddrMode::Impl, "PHP"}, {0x09, 2, 2, AddrMode::Imm, "ORA"}, {0x0a, 1, 2, AddrMode::Acc, "ASL"}, {0x0b, 2, 2, AddrMode::Imm, "*ANC"},
  {0x0c, 3, 4, AddrMode::Abs, "*NOP"}, {0x0d, 3, 4, AddrMode::Abs, "ORA"}, {0x0e, 3, 6, AddrMode::Abs, "ASL"}, {0x0f, 3, 6, AddrMode::Abs, "*SLO"},
  {0x10, 2, 2, AddrMode::Rel, "BPL"}, {0x11, 2, 5, AddrMode::IndY, "ORA"}, {0x12, 1, 1, AddrMode::Impl, "*JAM"}, {0x13, 2, 8, AddrMode::IndY, "*SLO"},
  {0x14, 2, 4, AddrMode::ZPX, "*NOP"}, {0x15, 2, 4, AddrMode::ZPX, "ORA"}, {0x16, 2, 6, AddrMode::ZPX, "ASL"}, {0x17, 2, 6, AddrMode::ZPX, "*SLO"},
  {0x18, 1, 2, AddrMode::Impl, "CLC"}, {0x19, 3, 4, AddrMode::AbsY, "ORA"}, {0x1a, 1, 2, AddrMode::Impl, "*NOP"}, {0x1b, 3, 7, AddrMode::AbsY, "*SLO"},
  {0x1c, 3, 4, AddrMode::AbsX, "*NOP"}, {0x1d, 3, 4, AddrMode::AbsX, "ORA"}, {0x1e, 3, 7, AddrMode::AbsX, "ASL"}, {0x1f, 3, 7, AddrMode::AbsX, "*SLO"},
  {0x20, 3, 6, AddrMode::Abs, "JSR"}, {0x21, 2, 6, AddrMode::XInd, "AND"}, {0x22, 1, 1, AddrMode::Impl, "*JAM"}, {0x23, 2, 8, AddrMode::XInd, "*RLA"},
  {0x24, 2, 3, AddrMode::ZP, "BIT"}, {0x25, 2, 3, AddrMode::ZP, "AND"}, {0x26, 2, 5, AddrMode::ZP, "ROL"}, {0x27, 2, 5, AddrMode::ZP, "*RLA"},
  {0x28, 1, 4, AddrMode::Impl, "PLP"}, {0x29, 2, 2, AddrMode::Imm, "AND"}, {0x2a, 1, 2, AddrMode::Acc, "ROL"}, {0x2b, 2, 2, AddrMode::Imm, "*ANC"},
  {0x2c, 3, 4, AddrMode::Abs, "BIT"}, {0x2d, 3, 4, AddrMode::Abs, "AND"}, {0x2e, 3, 6, AddrMode::Abs, "ROL"}, {0x2f, 3, 6, AddrMode::Abs, "*RLA"},
  {0x30, 2, 2, AddrMode::Rel, "BMI"}, {0x31, 2, 5, AddrMode::IndY, "AND"}, {0x32, 1, 1, AddrMode::Impl, "*JAM"}, {0x33, 2, 8, AddrMode::IndY, "*RLA"},
  {0x34, 2, 4, AddrMode::ZPX, "*NOP"}, {0x35, 2, 4, AddrMode::ZPX, "AND"}, {0x36, 2, 6, AddrMode::ZPX, "ROL"}, {0x37, 2, 6, AddrMode::ZPX, "*RLA"},
  {0x38, 1, 2, AddrMode::Impl, "SEC"}, {0x39, 3, 4, AddrMode::AbsY, "AND"}, {0x3a, 1, 2, AddrMode::Impl, "*NOP"}, {0x3b, 3, 7, AddrMode::AbsY, "*RLA"},
  {0x3c, 3, 4, AddrMode::AbsX, "*NOP"}, {0x3d, 3, 4, AddrMode::AbsX, "AND"}, {0x3e, 3, 7, AddrMode::AbsX, "ROL"}, {0x3f, 3, 7, AddrMode::AbsX, "*RLA"},
  {0x40, 1, 6, AddrMode::Impl, "RTI"}, {0x41, 2, 6, AddrMode::XInd, "EOR"}, {0x42, 1, 1, AddrMode::Impl, "*JAM"}, {0x43, 2, 8, AddrMode::XInd, "*SRE"},
  {0x44, 2, 3, AddrMode::ZP, "*NOP"}, {0x45, 2, 3, AddrMode::ZP, "EOR"}, {0x46, 2, 5, AddrMode::ZP, "LSR"}, {0x47, 2, 5, AddrMode::ZP, "*SRE"},
  {0x48, 1, 3, AddrMode::Impl, "PHA"}, {0x49, 2, 2, AddrMode::Imm, "EOR"}, {0x4a, 1, 2, AddrMode::Acc, "LSR"}, {0x4b, 2, 2, AddrMode::Imm, "*ASR"},
  {0x4c, 3, 3, AddrMode::Abs, "JMP"}, {0x4d, 3, 4, AddrMode::Abs, "EOR"}, {0x4e, 3, 6, AddrMode::Abs, "LSR"}, {0x4f, 3, 6, AddrMode::Abs, "*SRE"},
  {0x50, 2, 2, AddrMode::Rel, "BVC"}, {0x51, 2, 5, AddrMode::IndY, "EOR"}, {0x52, 1, 1, AddrMode::Impl, "*JAM"}, {0x53, 2, 8, AddrMode::IndY, "*SRE"},
  {0x54, 2, 4, AddrMode::ZPX, "*NOP"}, {0x55, 2, 4, AddrMode::ZPX, "EOR"}, {0x56, 2, 6, AddrMode::ZPX, "LSR"}, {0x57, 2, 6, AddrMode::ZPX, "*SRE"},
  {0x58, 1, 2, AddrMode::Impl, "CLI"}, {0x59, 3, 4, AddrMode::AbsY, "EOR"}, {0x5a, 1, 2, AddrMode::Impl, "*NOP"}, {0x5b, 3, 7, AddrMode::AbsY, "*SRE"},
  {0x5c, 3, 4, AddrMode::AbsX, "*NOP"}, {0x5d, 3, 4, AddrMode::AbsX, "EOR"}, {0x5e, 3, 7, AddrMode::AbsX, "LSR"}, {0x5f, 3, 7, AddrMode::AbsX, "*SRE"},
  {0x60, 1, 6, AddrMode::Impl, "RTS"}, {0x61, 2, 6, AddrMode::XInd, "ADC"}, {0x62, 1, 1, AddrMode::Impl, "*JAM"}, {0x63, 2, 8, AddrMode::XInd, "*RRA"},
  {0x64, 2, 3, AddrMode::ZP, "*NOP"}, {0x65, 2, 3, AddrMode::ZP, "ADC"}, {0x66, 2, 5, AddrMode::ZP, "ROR"}, {0x67, 2, 5, AddrMode::ZP, "*RRA"},
  {0x68, 1, 4, AddrMode::Impl, "PLA"}, {0x69, 2, 2, AddrMode::Imm, "ADC"}, {0x6a, 1, 2, AddrMode::Acc, "ROR"}, {0x6b, 2, 2, AddrMode::Imm, "*ARR"},
  {0x6c, 3, 5, AddrMode::Ind, "JMP"}, {0x6d, 3, 4, AddrMode::Abs, "ADC"}, {0x6e, 3, 6, AddrMode::Abs, "ROR"}, {0x6f, 3, 6, AddrMode::Abs, "*RRA"},
  {0x70, 2, 2, AddrMode::Rel, "BVS"}, {0x71, 2, 5, AddrMode::IndY, "ADC"}, {0x72, 1, 1, AddrMode::Impl, "*JAM"}, {0x73, 2, 8, AddrMode::IndY, "*RRA"},
  {0x74, 2, 4, AddrMode::ZPX, "*NOP"}, {0x75, 2, 4, AddrMode::ZPX, "ADC"}, {0x76, 2, 6, AddrMode::ZPX, "ROR"}, {0x77, 2, 6, AddrMode::ZPX, "*RRA"},
  {0x78, 1, 2, AddrMode::Impl, "SEI"}, {0x79, 3, 4, AddrMode::AbsY, "ADC"}, {0x7a, 1, 2, AddrMode::Impl, "*NOP"}, {0x7b, 3, 7, AddrMode::AbsY, "*RRA"},
  {0x7c, 3, 4, AddrMode::AbsX, "*NOP"}, {0x7d, 3, 4, AddrMode::AbsX, "ADC"}, {0x7e, 3, 7, AddrMode::AbsX, "ROR"}, {0x7f, 3, 7, AddrMode::AbsX, "*RRA"},
  {0x80, 2, 2, AddrMode::Imm, "*NOP"}, {0x81, 2, 6, AddrMode::XInd, "STA"}, {0x82, 2, 2, AddrMode::Imm, "NOP"}, {0x83, 2, 6, AddrMode::XInd, "*SAX"},
  {0x84, 2, 3, AddrMode::ZP, "STY"}, {0x85, 2, 3, AddrMode::ZP, "STA"}, {0x86, 2, 3, AddrMode::ZP, "STX"}, {0x87, 2, 3, AddrMode::ZP, "*SAX"},
  {0x88, 1, 2, AddrMode::Impl, "DEY"}, {0x89, 2, 2, AddrMode::Imm, "NOP"}, {0x8a, 1, 2, AddrMode::Impl, "TXA"}, {0x8b, 2, 2, AddrMode::Imm, "*XAA"},
  {0x8c, 3, 4, AddrMode::Abs, "STY"}, {0x8d, 3, 4, AddrMode::Abs, "STA"}, {0x8e, 3, 4, AddrMode::Abs, "STX"}, {0x8f, 3, 4, AddrMode::Abs, "*SAX"},
  {0x90, 2, 2, AddrMode::Rel, "BCC"}, {0x91, 2, 6, AddrMode::IndY, "STA"}, {0x92, 1, 1, AddrMode::Impl, "*JAM"}, {0x93, 2, 6, AddrMode::IndY, "*SHA"},
  {0x94, 2, 4, AddrMode::ZPX, "STY"}, {0x95, 2, 4, AddrMode::ZPX, "STA"}, {0x96, 2, 4, AddrMode::ZPY, "STX"}, {0x97, 2, 4, AddrMode::ZPY, "*SAX"},
  {0x98, 1, 2, AddrMode::Impl, "TYA"}, {0x99, 3, 5, AddrMode::AbsY, "STA"}, {0x9a, 1, 2, AddrMode::Impl, "TXS"}, {0x9b, 3, 5, AddrMode::AbsX, "*SHS"},
  {0x9c, 3, 5, AddrMode::AbsX, "*SHY"}, {0x9d, 3, 5, AddrMode::AbsX, "STA"}, {0x9e, 3, 5, AddrMode::AbsY, "*SHX"}, {0x9f, 3, 5, AddrMode::AbsY, "*SHA"},
  {0xa0, 2, 2, AddrMode::Imm, "LDY"}, {0xa1, 2, 6, AddrMode::XInd, "LDA"}, {0xa2, 2, 2, AddrMode::Imm, "LDX"}, {0xa3, 2, 6, AddrMode::XInd, "*LAX"},
  {0xa4, 2, 3, AddrMode::ZP, "LDY"}, {0xa5, 2, 3, AddrMode::ZP, "LDA"}, {0xa6, 2, 3, AddrMode::ZP, "LDX"}, {0xa7, 2, 3, AddrMode::ZP, "*LAX"},
  {0xa8, 1, 2, AddrMode::Impl, "TAY"}, {0xa9, 2, 2, AddrMode::Imm, "LDA"}, {0xaa, 1, 2, AddrMode::Impl, "TAX"}, {0xab, 2, 2, AddrMode::Imm, "*LAX"},
  {0xac, 3, 4, AddrMode::Abs, "LDY"}, {0xad, 3, 4, AddrMode::Abs, "LDA"}, {0xae, 3, 4, AddrMode::Abs, "LDX"}, {0xaf, 3, 4, AddrMode::Abs, "*LAX"},
  {0xb0, 2, 2, AddrMode::Rel, "BCS"}, {0xb1, 2, 5, AddrMode::IndY, "LDA"}, {0xb2, 1, 1, AddrMode::Impl, "*JAM"}, {0xb3, 2, 5, AddrMode::IndY, "*LAX"},
  {0xb4, 2, 4, AddrMode::ZPX, "LDY"}, {0xb5, 2, 4, AddrMode::ZPX, "LDA"}, {0xb6, 2, 4, AddrMode::ZPY, "LDX"}, {0xb7, 2, 4, AddrMode::ZPY, "*LAX"},
  {0xb8, 1, 2, AddrMode::Impl, "CLV"}, {0xb9, 3, 4, AddrMode::AbsY, "LDA"}, {0xba, 1, 2, AddrMode::Impl, "TSX"}, {0xbb, 3, 4, AddrMode::AbsY, "*LAS"},
  {0xbc, 3, 4, AddrMode::AbsX, "LDY"}, {0xbd, 3, 4, AddrMode::AbsX, "LDA"}, {0xbe, 3, 4, AddrMode::AbsY, "LDX"}, {0xbf, 3, 4, AddrMode::AbsY, "*LAX"},
  {0xc0, 2, 2, AddrMode::Imm, "CPY"}, {0xc1, 2 ,6, AddrMode::XInd, "CMP"}, {0xc2, 2, 2, AddrMode::Imm, "NOP"}, {0xc3, 2, 8, AddrMode::XInd, "*DCP"},
  {0xc4, 2, 3, AddrMode::ZP, "CPY"}, {0xc5, 2, 3, AddrMode::ZP, "CMP"}, {0xc6, 2, 5, AddrMode::ZP, "DEC"}, {0xc7, 2, 5, AddrMode::ZP, "*DCP"},
  {0xc8, 1, 2, AddrMode::Impl, "INY"}, {0xc9, 2, 2, AddrMode::Imm, "CMP"}, {0xca, 1, 2, AddrMode::Impl, "DEX"}, {0xcb, 2, 2, AddrMode::Imm, "*SBX"},
  {0xcc, 3, 4, AddrMode::Abs, "CPY"}, {0xcd, 3, 4, AddrMode::Abs, "CMP"}, {0xce, 3, 6, AddrMode::Abs, "DEC"}, {0xcf, 3, 6, AddrMode::Abs, "*DCP"},
  {0xd0, 2, 2, AddrMode::Rel, "BNE"}, {0xd1, 2, 5, AddrMode::IndY, "CMP"}, {0xd2, 1, 1, AddrMode::Impl, "*JAM"}, {0xd3, 2, 8, AddrMode::IndY, "*DCP"},
  {0xd4, 2, 4, AddrMode::ZPX, "*NOP"}, {0xd5, 2, 4, AddrMode::ZPX, "CMP"}, {0xd6, 2, 6, AddrMode::ZPX, "DEC"}, {0xd7, 2, 6, AddrMode::ZPX, "*DCP"},
  {0xd8, 1, 2, AddrMode::Impl, "CLD"}, {0xd9, 3, 4, AddrMode::AbsY, "CMP"}, {0xda, 1, 2, AddrMode::Impl, "*NOP"}, {0xdb, 3, 7, AddrMode::AbsY, "*DCP"},
  {0xdc, 3, 4, AddrMode::AbsX, "*NOP"}, {0xdd, 3, 4, AddrMode::AbsX, "CMP"}, {0xde, 3 ,7, AddrMode::AbsX, "DEC"}, {0xdf, 3, 7, AddrMode::AbsX, "*DCP"},
  {0xe0, 2, 2, AddrMode::Imm, "CPX"}, {0xe1, 2, 6, AddrMode::XInd, "SBC"}, {0xe2, 2, 2, AddrMode::Imm, "NOP"}, {0xe3, 2, 8, AddrMode::XInd, "*ISC"},
  {0xe4, 2, 3, AddrMode::ZP, "CPX"}, {0xe5, 2, 3, AddrMode::ZP, "SBC"}, {0xe6, 2, 5, AddrMode::ZP, "INC"}, {0xe7, 2, 5, AddrMode::ZP, "*ISC"},
  {0xe8, 1, 2, AddrMode::Impl, "INX"}, {0xe9, 2, 2, AddrMode::Imm, "SBC"}, {0xea, 1, 2, AddrMode::Impl, "NOP"}, {0xeb, 2, 2, AddrMode::Imm, "*SBC"},
  {0xec, 3, 4, AddrMode::Abs, "CPX"}, {0xed, 3, 4, AddrMode::Abs, "SBC"}, {0xee, 3, 6, AddrMode::Abs, "INC"}, {0xef, 3, 6, AddrMode::Abs, "*ISC"},
  {0xf0, 2, 2, AddrMode::Rel, "BEQ"}, {0xf1, 2, 5, AddrMode::IndY, "SBC"}, {0xf2, 1, 1, AddrMode::Impl, "*JAM"}, {0xf3, 2, 8, AddrMode::IndY, "*ISC"},
  {0xf4, 2, 4, AddrMode::ZPX, "*NOP"}, {0xf5, 2, 4, AddrMode::ZPX, "SBC"}, {0xf6, 2, 6, AddrMode::ZPX, "INC"}, {0xf7, 2, 6, AddrMode::ZPX, "*ISC"},
  {0xf8, 1, 2, AddrMode::Impl, "SED"}, {0xf9, 3, 4, AddrMode::AbsY, "SBC"}, {0xfa, 1, 2, AddrMode::Impl, "*NOP"}, {0xfb, 3, 7, AddrMode::AbsY, "*ISC"},
  {0xfc, 3, 4, AddrMode::AbsX, "*NOP"}, {0xfd, 3, 4, AddrMode::AbsX, "SBC"}, {0xfe, 3, 7, AddrMode::AbsX, "INC"}, {0xff, 3, 7, AddrMode::AbsX, "*ISC"}
];

enum CPUModel {
    NMOS, R2A03, CMOS, WDC,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    struct PSW: u8 {
	const N = 0b10000000;
	const V = 0b01000000;
	const U = 0b00100000;
	const B = 0b00010000;
	const D = 0b00001000;
	const I = 0b00000100;
	const Z = 0b00000010;
	const C = 0b00000001;
    }
}

impl PSW {
    pub fn as_u8(&self) -> u8 {
	self.bits() as u8
    }
}

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    p: PSW,
    sp: u8,
    pub pc: u16,
    pub bus: bus::NesBus,
    model: CPUModel,
    instr_set: &'static [Instruction; 256],
    cycles: u128,
}

impl Cpu {
    pub fn new() -> Self {
	Cpu {
	    a: 0,
	    x: 0,
	    y: 0,
	    p: PSW::I | PSW::U,
	    sp: 0xfd,
	    pc: 0,
	    bus: bus::NesBus::new(),
	    model: CPUModel::R2A03,
	    instr_set: &INSTR_SET_NMOS,
	    cycles: 7,
	}
    }

    pub fn reset(&mut self, cold: bool) {
	self.cycles = 7;
	self.pc = self.bus.read_word(0xfffc);
	self.sp -= 3; //todo: why? i don't remember
	if cold {
	    self.a = 0;
	    self.x = 0;
	    self.y = 0;
	    self.sp = 0xfd;
	    self.p = PSW::I | PSW::U;
	}
    }

    pub fn irq(&mut self) {
	if (self.p & PSW::I).as_u8() == 0 {
	    return;
	}
	self.push_word(self.pc);
	self.push_byte(self.p.as_u8());
	self.p |= PSW::I;
	self.pc = self.bus.read_word(0xfffe);
    }

    fn push_byte(&mut self, byte: u8) {
	self.bus.write_byte(0x100 + self.sp as u16, byte);
	self.sp = self.sp.wrapping_sub(1);
    }

    fn push_word(&mut self, word: u16) {
	self.push_byte(((word >> 8) & 0x00ff) as u8);
	self.push_byte((word & 0x00ff) as u8);
    }

    fn pop_byte(&mut self) -> u8 {
	self.sp = self.sp.wrapping_add(1);
	self.bus.read_byte(0x100 + self.sp as u16)
    }

    fn pop_word(&mut self) -> u16 {
	let lo = self.pop_byte();
	let hi = self.pop_byte();
	let result: u16 = ((hi as u16) << 8) | (lo as u16);
	result
    }

    fn page_cross(&mut self, new: u16, inc: u8) -> bool {
	let old: u16 = new.wrapping_sub(inc as u16);
	(old & 0x00ff) > (new & 0x00ff)
    }

    pub fn disas(&mut self, opcode: u8) {
	let instr: &Instruction = &self.instr_set[opcode as usize];
	print!("{:04X} {opcode:02X}", self.pc);
	if instr.bytes > 1 {
	    print!(" {:02X}", self.bus.read_byte(self.pc + 1));
	}
	if instr.bytes > 2 {
	    print!(" {:02X}", self.bus.read_byte(self.pc + 2));
	}
	print!(" {}", instr.mnemonic);

	let operand_byte: u8 = self.bus.read_byte(self.pc + 1);
	let operand_word: u16 = self.bus.read_word(self.pc + 1);
	let tmp_xind: u16 = self.bus.read_addr((operand_byte.wrapping_add(self.x)) as u16);
	let tmp_indy: u16 = self.bus.read_addr(operand_byte as u16).wrapping_add(self.y as u16);

	match instr.mode {
	    AddrMode::Imm => print!(" #${:02X}", operand_byte),
	    AddrMode::Abs => {
		print!(" ${:04X}", operand_word);
		if instr.mnemonic != "JMP" && instr.mnemonic != "JSR" {
		    print!(" = {:02X}", self.bus.read_byte(operand_word));
		}
	    },
	    AddrMode::ZP => print!(" ${:02X} = {:02X}", operand_byte, self.bus.read_byte(operand_byte as u16)),
	    AddrMode::AbsX => print!(" ${:04X},X @ {:04X} = {:02X}", operand_word, operand_word + self.x as u16,
				     self.bus.read_byte(operand_word + self.x as u16)),
	    AddrMode::AbsY => print!(" ${:04X},Y @ {:04X} = {:02X}", operand_word, operand_word.wrapping_add(self.y as u16),
				     self.bus.read_byte(operand_word.wrapping_add(self.y as u16))),
	    AddrMode::ZPX => print!(" ${:02X},X @ {:02X} = {:02X}", operand_byte, operand_byte.wrapping_add(self.x),
				    self.bus.read_byte((operand_byte.wrapping_add(self.x)) as u16)),
	    AddrMode::ZPY => print!(" ${:02X},Y @ {:02X} = {:02X}", operand_byte, operand_byte.wrapping_add(self.y),
				    self.bus.read_byte((operand_byte.wrapping_add(self.y)) as u16)),
	    AddrMode::Ind => {
		let mut tmpw: u16 = operand_word;
		if (tmpw & 0x00ff) == 0x00ff {
		    tmpw = self.bus.read_byte(tmpw) as u16 | (self.bus.read_byte(tmpw - 0x00ff) as u16).wrapping_shl(8);
		} else {
		    tmpw = self.bus.read_word(tmpw);
		}
		print!(" (${:04X}) = {:04X}", operand_word, tmpw);
	    },
	    AddrMode::XInd => print!(" (${:02X},X) @ {:02X} = {:04X} = {:02X}", operand_byte,
				     operand_byte.wrapping_add(self.x),
				     self.bus.read_addr((operand_byte.wrapping_add(self.x)) as u16),
				     self.bus.read_byte(tmp_xind)),
	    AddrMode::IndY => print!(" (${:02X}),Y = {:04X} @ {:04X} = {:02X}", operand_byte,
				     self.bus.read_addr(operand_byte as u16),
				     self.bus.read_addr(operand_byte as u16).wrapping_add(self.y as u16),
				     self.bus.read_byte(tmp_indy)),
	    AddrMode::Rel => print!(" ${:04X}", (self.pc.wrapping_add_signed((operand_byte as i8) as i16)) as u16 + instr.bytes as u16),
	    AddrMode::Acc => print!(" A"),
	    AddrMode::Impl => {},
	}
	print!(" A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} CYC:{}\n", self.a, self.x, self.y, self.p.as_u8(), self.sp, self.cycles);
    }

    pub fn step(&mut self) -> bool {
	let opcode: u8 = self.bus.read_byte(self.pc);
	self.disas(opcode);
	let instr: &Instruction = &self.instr_set[opcode as usize];
	self.cycles += instr.cycles as u128;

	let mut operand: u8 = 0;
	let mut store_addr: u16 = 0;
	let mut tmp: u8;
	let mut tmpw: u16;

	//calculate operands
	match instr.mode {
	    AddrMode::Impl => {},
	    AddrMode::Acc => operand = self.a,
	    AddrMode::Rel | AddrMode::Imm => operand = self.bus.read_byte(self.pc + 1),
	    AddrMode::Abs => {
		store_addr = self.bus.read_word(self.pc + 1);
		operand = self.bus.read_byte(store_addr);
	    },
	    AddrMode::AbsX => {
		store_addr = self.bus.read_word(self.pc + 1) + self.x as u16;
		operand = self.bus.read_byte(store_addr);
		match instr.mnemonic {
		    "ADC" | "AND" | "CMP" | "EOR" | "LDA" | "LDY" |
		    "ORA" | "SBC" | "*NOP" => {
			self.cycles += self.page_cross(store_addr, self.x) as u128;
		    },
		    _ => {},
		};
	    },
	    AddrMode::AbsY => {
		store_addr = self.bus.read_word(self.pc + 1).wrapping_add(self.y as u16);
		operand = self.bus.read_byte(store_addr);
		match instr.mnemonic {
		    "STA" | "*DCP" | "*ISC" | "*RLA" | "*RRA" | "*SHA" |
		    "*SHS" | "*SHX" | "*SLO" | "*SRE" => {},
		    _ => {
			self.cycles += self.page_cross(store_addr, self.y) as u128;
		    },
		};
	    },
	    AddrMode::ZP => {
		store_addr = self.bus.read_byte(self.pc + 1) as u16;
		operand = self.bus.read_byte(store_addr);
	    },
	    AddrMode::ZPX => {
		store_addr = self.bus.read_byte(self.pc + 1).wrapping_add(self.x) as u16;
		operand = self.bus.read_byte(store_addr);
	    },
	    AddrMode::ZPY => {
		store_addr = self.bus.read_byte(self.pc + 1).wrapping_add(self.y) as u16;
		operand = self.bus.read_byte(store_addr);
	    },
	    AddrMode::XInd => {
		tmpw = self.bus.read_byte(self.pc + 1).wrapping_add(self.x) as u16;
		store_addr = self.bus.read_addr(tmpw);
		operand = self.bus.read_byte(store_addr);
	    },
	    AddrMode::IndY => {
		tmpw = self.bus.read_byte(self.pc + 1) as u16;
		store_addr = self.bus.read_addr(tmpw).wrapping_add(self.y as u16);
		operand = self.bus.read_byte(store_addr);
		match instr.mnemonic {
		    "STA" | "*DCP" | "*ISC" | "*RLA" | "*RRA" | "*SHA" |
		    "*SLO" | "*SRE" => {},
		    _ => {
			self.cycles += self.page_cross(store_addr, self.y) as u128;
		    },
		};
	    },
	    AddrMode::Ind => { //only used for JMP ($nnnn), wraps at page boundary
		tmpw = self.bus.read_word(self.pc + 1);
		if (tmpw & 0x00ff) == 0x00ff {
		    store_addr = (self.bus.read_byte(tmpw) as u16) | (self.bus.read_byte(tmpw - 0x00ff) as u16) << 8;
		} else {
		    store_addr = self.bus.read_word(tmpw);
		}
	    },
	};

	self.pc += instr.bytes as u16;
	match instr.mnemonic {
	    "NOP" | "*NOP" => {},
	    "BRK" => {
		self.pc += 1; //1 byte instruction, increase 2 bytes total
		return true; //todo get rid
		self.irq();
	    },
	    "JMP" => self.pc = store_addr,
	    "JSR" => {
		self.push_word(self.pc - 1); //3 byte instruction, want pc + 2
		self.pc = store_addr;
	    },
	    "RTI" => {
		self.p = PSW::from_bits(self.pop_byte()).expect("oop");
		self.p |= PSW::U;
		self.pc = self.pop_word();
	    },
	    "RTS" => {
		self.pc = self.pop_word();
		self.pc += 1;
	    },
	    "BCC" => {
		if !(self.p.contains(PSW::C)) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BCS" => {
		if self.p.contains(PSW::C) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BNE" => {
		if !(self.p.contains(PSW::Z)) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BEQ" => {
		if self.p.contains(PSW::Z) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BPL" => {
		if !(self.p.contains(PSW::N)) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BMI" => {
		if self.p.contains(PSW::N) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BVC" => {
		if !(self.p.contains(PSW::V)) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "BVS" => {
		if self.p.contains(PSW::V) {
		    if (self.pc & 0xff00) != (self.pc.wrapping_add_signed((operand as i8) as i16) as u16) & 0xff00 {
			self.cycles += 2;
		    } else {
			self.cycles += 1;
		    }
		    self.pc = self.pc.wrapping_add_signed((operand as i8) as i16) as u16;
		}
	    },
	    "CLC" => self.p.remove(PSW::C),
	    "CLD" => self.p.remove(PSW::D),
	    "CLI" => self.p.remove(PSW::I),
	    "CLV" => self.p.remove(PSW::V),
	    "SEC" => self.p.insert(PSW::C),
	    "SED" => self.p.insert(PSW::D),
	    "SEI" => self.p.insert(PSW::I),
	    "PHA" => self.push_byte(self.a),
	    "PHP" => self.push_byte((self.p | PSW::U | PSW::B).as_u8()),
	    "PLA" => {
		self.a = self.pop_byte();
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "PLP" => {
		self.p = PSW::from_bits(self.pop_byte()).expect("oop");
		self.p.insert(PSW::U);
		self.p.remove(PSW::B);
	    },
	    "AND" => {
		self.a &= operand;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "BIT" => {
		tmp = self.a & operand;
		self.p.set(PSW::N, operand & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::V, operand & (PSW::V.as_u8()) != 0);
		self.p.set(PSW::Z, tmp == 0);
	    },
	    "EOR" => {
		self.a ^= operand;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "ORA" => {
		self.a |= operand;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "LDA" => {
		self.a = operand;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "LDX" => {
		self.x = operand;
		self.p.set(PSW::N, self.x & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.x == 0);
	    },
	    "LDY" => {
		self.y = operand;
		self.p.set(PSW::N, self.y & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.y == 0);
	    },
	    "STA" => self.bus.write_byte(store_addr, self.a),
	    "STX" => self.bus.write_byte(store_addr, self.x),
	    "STY" => self.bus.write_byte(store_addr, self.y),
	    "TAX" => {
		self.x = self.a;
		self.p.set(PSW::N, self.x & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.x == 0);
	    },
	    "TAY" => {
		self.y = self.a;
		self.p.set(PSW::N, self.y & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.y == 0);
	    },
	    "TSX" => {
		self.x = self.sp;
		self.p.set(PSW::N, self.x & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.x == 0);
	    },
	    "TXA" => {
		self.a = self.x;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "TXS" => self.sp = self.x,
	    "TYA" => {
		self.a = self.y;
		self.p.set(PSW::N, self.a & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "ASL" => {
		self.p.set(PSW::C, (operand & 0x80) != 0);
		operand = operand << 1;
		self.p.set(PSW::N, operand & (PSW::N.as_u8()) != 0);
		self.p.set(PSW::Z, operand == 0);
		if instr.mode == AddrMode::Acc {
		    self.a = operand;
		} else {
		    self.bus.write_byte(store_addr, operand);
		}
	    },
	    "LSR" => {
		self.p.set(PSW::C, (operand & 0x01) != 0);
		operand = operand >> 1;
		self.p.remove(PSW::N);
		self.p.set(PSW::Z, operand == 0);
		if instr.mode == AddrMode::Acc {
		    self.a = operand;
		} else {
		    self.bus.write_byte(store_addr, operand);
		}
	    },
	    "ROL" => {
		tmp = self.p.as_u8();
		self.p.set(PSW::C, (operand & 0x80) != 0);
		operand = operand << 1;
		operand |= tmp & 1;
		self.p.set(PSW::Z, operand == 0);
		self.p.set(PSW::N, operand & (PSW::N.as_u8()) != 0);
		if instr.mode == AddrMode::Acc {
		    self.a = operand;
		} else {
		    self.bus.write_byte(store_addr, operand);
		}
	    },
	    "ROR" => {
		tmp = self.p.as_u8();
		self.p.set(PSW::C, (operand & 0x01) != 0);
		operand = operand >> 1;
		operand |= (tmp & 0x01) << 7;
		self.p.set(PSW::Z, operand == 0);
		self.p.set(PSW::N, (tmp & 1) != 0);
		if instr.mode == AddrMode::Acc {
		    self.a = operand;
		} else {
		    self.bus.write_byte(store_addr, operand);
		}
	    },
	    "ADC" => {
		tmp = self.a;
		self.a = self.a.wrapping_add(operand);
		let cflag = self.p.contains(PSW::C) as u8;
		self.a = self.a.wrapping_add(cflag);

		self.p.set(PSW::Z, self.a == 0);
		let carry: bool = (tmp as u16 + operand as u16 + cflag as u16) > 0xff;
		self.p.set(PSW::C, carry); //todo: decimal mode
		let ovf: bool = (!(tmp ^ operand) & (tmp ^ self.a) & 0x80) != 0;
		self.p.set(PSW::V, ovf);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "CMP" => {
		tmp = self.a.wrapping_sub(operand);
		self.p.set(PSW::Z, self.a == operand);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
		self.p.set(PSW::C, operand <= self.a);
	    },
	    "CPX" => {
		tmp = self.x.wrapping_sub(operand);
		self.p.set(PSW::Z, self.x == operand);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
		self.p.set(PSW::C, operand <= self.x);
	    },
	    "CPY" => {
		tmp = self.y.wrapping_sub(operand);
		self.p.set(PSW::Z, self.y == operand);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
		self.p.set(PSW::C, operand <= self.y);
	    },
	    "SBC" | "*SBC" => {
		operand ^= 0xff;
		tmp = self.a;
		self.a = self.a.wrapping_add(operand);
		let cflag = self.p.contains(PSW::C) as u8;
		self.a = self.a.wrapping_add(cflag);
		self.p.set(PSW::Z, self.a == 0);
		let carry: bool = (tmp as u16 + operand as u16 + cflag as u16) > 0xff;
		self.p.set(PSW::C, carry); //todo: decimal mode
		let ovf: bool = (!(tmp ^ operand) & (tmp ^ self.a) & 0x80) != 0;
		self.p.set(PSW::V, ovf);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "DEC" => {
		tmp = operand.wrapping_sub(1);
		self.p.set(PSW::Z, tmp == 0);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
		self.bus.write_byte(store_addr, tmp);
	    },
	    "DEX" => {
		self.x = self.x.wrapping_sub(1);
		self.p.set(PSW::Z, self.x == 0);
		self.p.set(PSW::N, (self.x & 0x80) != 0);
	    },
	    "DEY" => {
		self.y = self.y.wrapping_sub(1);
		self.p.set(PSW::Z, self.y == 0);
		self.p.set(PSW::N, (self.y & 0x80) != 0);
	    },
	    "INC" => {
		tmp = operand.wrapping_add(1);
		self.p.set(PSW::Z, tmp == 0);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
		self.bus.write_byte(store_addr, tmp);
	    },
	    "INX" => {
		self.x = self.x.wrapping_add(1);
		self.p.set(PSW::Z, self.x == 0);
		self.p.set(PSW::N, (self.x & 0x80) != 0);
	    },
	    "INY" => {
		self.y = self.y.wrapping_add(1);
		self.p.set(PSW::Z, self.y == 0);
		self.p.set(PSW::N, (self.y & 0x80) != 0);
	    },
	    "*LAX" => {
		self.a = operand;
		self.x = operand;
		self.p.set(PSW::Z, operand == 0);
		self.p.set(PSW::N, (operand & 0x80) != 0);
	    },
	    "*SAX" => {
		tmp = self.a & self.x;
		self.bus.write_byte(store_addr, tmp);
	    },
	    "*LAS" => {
		tmp = self.sp & operand;
		self.a = tmp;
		self.x = tmp;
		self.sp = tmp;
		self.p.set(PSW::Z, tmp == 0);
		self.p.set(PSW::N, (tmp & 0x80) != 0);
	    },
	    "*SHA" => {
		if instr.mode == AddrMode::AbsY {
		    tmpw = self.bus.read_addr(self.pc - 2); //3 byte instr
		    operand = (tmpw & 0xff00).wrapping_shr(8) as u8;
		    operand = operand.wrapping_add(1);
		} else {
		    tmp = self.bus.read_byte(self.pc - 1); //2 byte instr
		    tmpw = self.bus.read_addr(tmp as u16);
		    operand = self.bus.read_byte(tmpw);
		}
		tmp = self.a & self.x & operand;
		self.bus.write_byte(store_addr, tmp);
	    },
	    "*SHX" => {
		tmpw = self.bus.read_addr(self.pc - 2); //3 byte instr
		operand = (tmpw & 0xff00).wrapping_shr(8) as u8;
		operand = operand.wrapping_add(1);
		tmp = self.x & operand;
		self.bus.write_byte(store_addr, tmp);
	    },
	    "*SHY" => {
		tmpw = self.bus.read_addr(self.pc - 2); //3 byte instr
		operand = (tmpw & 0xff00).wrapping_shr(8) as u8;
		operand = operand.wrapping_add(1);
		tmp = self.y & operand;
		self.bus.write_byte(store_addr, tmp);
	    },
	    "*SHS" => {
		self.sp = self.a & self.x;
		tmpw = self.bus.read_addr(self.pc - 2); //3 byte instr
		operand = (tmpw & 0xff00).wrapping_shr(8) as u8;
		operand = operand.wrapping_add(1);
		tmp = self.sp & operand;
		self.bus.write_byte(store_addr, tmp);
	    },
	    "*ANC" => {
		self.a &= operand;
		self.p.set(PSW::Z, self.a == 0);
		self.p.set(PSW::N | PSW::C, (self.a & 0x80) != 0);
	    },
	    "*ARR" => {
		tmp = (self.a & operand) >> 1;
		tmp &= ((self.p.contains(PSW::C) as u8) << 7) | 0x7f;
		self.a = tmp;
		self.p.set(PSW::V, ((self.a >> 6) & 1) != ((self.a >> 5) & 1));
		self.p.set(PSW::C, (self.a & (1u8 << 6)) != 0);
	    },
	    "*ASR" => {
		tmp = self.a & operand;
		self.p.set(PSW::C, (self.a & 1) != 0);
		self.a = tmp >> 1;
		self.p.remove(PSW::N);
		self.p.set(PSW::Z, self.a == 0);
	    },
	    "*DCP" => {
		operand = operand.wrapping_sub(1);
		self.bus.write_byte(store_addr, operand);
		self.p.set(PSW::Z, operand == self.a);
		self.p.set(PSW::C, operand <= self.a);
		self.p.set(PSW::N, (self.a.wrapping_sub(operand) & 0x80) != 0);
	    },
	    "*ISC" => {
		operand = operand.wrapping_add(1);
		self.bus.write_byte(store_addr, operand);
		operand ^= 0xff;
		tmp = self.a;
		self.a = self.a.wrapping_add(operand);
		let cflag = self.p.contains(PSW::C) as u8;
		self.a = self.a.wrapping_add(cflag);
		self.p.set(PSW::Z, self.a == 0);
		let carry: bool = (tmp as u16 + operand as u16 + cflag as u16) > 0xff;
		self.p.set(PSW::C, carry); //todo: decimal mode
		let ovf: bool = (!(tmp ^ operand) & (tmp ^ self.a) & 0x80) != 0;
		self.p.set(PSW::V, ovf);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*RLA" => {
		tmp = self.p.as_u8();
		self.p.set(PSW::C, (operand & 0x80) != 0);
		operand = operand << 1;
		operand |= tmp & 1;
		self.bus.write_byte(store_addr, operand);
		self.a &= operand;
		self.p.set(PSW::Z, self.a == 0);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*RRA" => {
		tmp = self.p.as_u8();
		self.p.set(PSW::C, (operand & 1) != 0);
		operand = operand >> 1;
		operand |= (tmp & 1) << 7;
		self.bus.write_byte(store_addr, operand);
		tmp = self.a;
		self.a = self.a.wrapping_add(operand);
		let cflag = self.p.contains(PSW::C) as u8;
		self.a = self.a.wrapping_add(cflag);
		self.p.set(PSW::Z, self.a == 0);
		let carry: bool = (tmp as u16 + operand as u16 + cflag as u16) > 0xff;
		self.p.set(PSW::C, carry); //todo: decimal mode
		let ovf: bool = (!(tmp ^ operand) & (tmp ^ self.a) & 0x80) != 0;
		self.p.set(PSW::V, ovf);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*SBX" => {
		tmp = self.a & self.x;
		tmp = tmp.wrapping_sub(operand);
		self.x = tmp;
		self.p.set(PSW::C, self.x >= 0);
		self.p.set(PSW::N, (self.x & 0x80) != 0);
		self.p.set(PSW::Z, self.x == 0);
	    },
	    "*SLO" => {
		self.p.set(PSW::C, (operand & 0x80) != 0);
		operand = operand << 1;
		self.bus.write_byte(store_addr, operand);
		self.a |= operand;
		self.p.set(PSW::Z, self.a == 0);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*SRE" => {
		self.p.set(PSW::C, (operand & 1) != 0);
		operand = operand >> 1;
		self.bus.write_byte(store_addr, operand);
		self.a ^= operand;
		self.p.set(PSW::Z, self.a == 0);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*XAA" => {
		tmp = self.a & 0xee; //on real hardware this value is random
		self.a = tmp & self.x & operand;
		self.p.set(PSW::Z, operand == 0);
		self.p.set(PSW::N, (self.a & 0x80) != 0);
	    },
	    "*JAM" => {
		self.pc += 2;
		println!("JAM instruction encountered");
		return true;
	    },
	    _ => println!("Unimplemented instruction {}", instr.mnemonic),
	};
	false
    }
}
