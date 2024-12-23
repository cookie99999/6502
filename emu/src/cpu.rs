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

const instr_set_nmos: [Instruction; 1] = instr_set![
    {0x00, 1, 7, AddrMode::Impl, "BRK"},
];

enum CPUModel {
    NMOS, R2A03, CMOS, WDC,
}

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    sp: u8,
    pc: u16,
    memory: [u8; 0x10000],
    model: CPUModel,
    instr_set: &'static [Instruction; 1],
    cycles: u128,
}
