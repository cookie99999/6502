#include <stdint.h>
#include <iostream>
#include <cstdio>

#define F_N 0b10000000
#define F_V 0b01000000
#define F_5 0b00100000
#define F_B 0b00010000
#define F_D 0b00001000
#define F_I 0b00000100
#define F_Z 0b00000010
#define F_C 0b00000001

enum AddrModes {
  AD_IMPL, AD_IMM, AD_ABS, AD_ZP, AD_ABSX, AD_ABSY, AD_ZPX, AD_ZPY, AD_IND, AD_XIND, AD_INDY, AD_REL, AD_ACC
};

struct Instruction {
    uint8_t opcode;
    short bytes;
    short cycles;
    AddrModes mode;
    const char *mnemonic;

    Instruction(uint8_t o, short b, short c, const char *m) : opcode(o), bytes(b), cycles(c), mode(AD_IMM), mnemonic(m) {}
    Instruction(uint8_t o, short b, short c, AddrModes mo, const char *m) : opcode(o), bytes(b), cycles(c), mode(mo), mnemonic(m) {}
    Instruction() {}
};

const Instruction instr_set_nmos[256] = {
  {0x00, 1, 7, AD_IMPL, "BRK"}, {0x01, 2, 6, AD_XIND, "ORA"}, {0x02, 1, 1, AD_IMPL, "JAM"}, {0x03, 2, 8, AD_XIND, "SLO"},
  {0x04, 2, 3, AD_ZP, "NOP"}, {0x05, 2, 3, AD_ZP, "ORA"}, {0x06, 2, 5, AD_ACC, "ASL"}, {0x07, 2, 5, AD_ZP, "SLO"},
  {0x08, 1, 3, AD_IMPL, "PHP"}, {0x09, 2, 2, AD_IMM, "ORA"}, {0x0a, 1, 2, AD_ACC, "ASL"}, {0x0b, 2, 2, AD_IMM, "ANC"},
  {0x0c, 3, 4, AD_ABS, "NOP"}, {0x0d, 3, 4, AD_ABS, "ORA"}, {0x0e, 3, 6, AD_ABS, "ASL"}, {0x0f, 3, 6, AD_ABS, "SLO"},
  {0x10, 2, 2, AD_REL, "BPL"}, {0x11, 2, 5, AD_INDY, "ORA"}, {0x12, 1, 1, AD_IMPL, "JAM"}, {0x13, 2, 8, AD_INDY, "SLO"},
  {0x14, 2, 8, AD_ZPX, "NOP"}, {0x15, 2, 4, AD_ZPX, "ORA"}, {0x16, 2, 6, AD_ZPX, "ASL"}, {0x17, 2, 6, AD_ZPX, "SLO"},
  {0x18, 1, 2, AD_IMPL, "CLC"}, {0x19, 3, 4, AD_ABSY, "ORA"}, {0x1a, 1, 2, AD_IMPL, "NOP"}, {0x1b, 3, 7, AD_ABSY, "SLO"},
  {0x1c, 3, 4, AD_ABSX, "NOP"}, {0x1d, 3, 4, AD_ABSX, "ORA"}, {0x1e, 3, 7, AD_ABSX, "ASL"}, {0x1f, 3, 7, AD_ABSX, "SLO"},
  {0x20, 3, 6, AD_ABS, "JSR"}, {0x21, 2, 6, AD_XIND, "AND"}, {0x22, 1, 1, AD_IMPL, "JAM"}, {0x23, 2, 8, AD_XIND, "RLA"},
  {0x24, 2, 3, AD_ZP, "BIT"}, {0x25, 2, 3, AD_ZP, "AND"}, {0x26, 2, 5, AD_ZP, "ROL"}, {0x27, 2, 5, AD_ZP, "RLA"},
  {0x28, 1, 4, AD_IMPL, "PLP"}, {0x29, 2, 2, AD_IMM, "AND"}, {0x2a, 1, 2, AD_ACC, "ROL"}, {0x2b, 2, 2, AD_IMM, "ANC"},
  {0x2c, 3, 4, AD_ABS, "BIT"}, {0x2d, 3, 4, AD_ABS, "AND"}, {0x2e, 3, 6, AD_ABS, "ROL"}, {0x2f, 3, 6, AD_ABS, "RLA"},
  {0x30, 2, 2, AD_REL, "BMI"}, {0x31, 2, 5, AD_INDY, "AND"}, {0x32, 1, 1, AD_IMPL, "JAM"}, {0x33, 2, 8, AD_INDY, "RLA"},
  {0x34, 2, 4, AD_ZPX, "NOP"}, {0x35, 2, 4, AD_ZPX, "AND"}, {0x36, 2, 6, AD_ZPX, "ROL"}, {0x37, 2, 6, AD_ZPX, "RLA"},
  {0x38, 1, 2, AD_IMPL, "SEC"}, {0x39, 3, 4, AD_ABSY, "AND"}, {0x3a, 2, 1, AD_IMPL, "NOP"}, {0x3b, 3, 7, AD_ABSY, "RLA"},
  {0x3c, 3, 4, AD_ABSX, "NOP"}, {0x3d, 3, 4, AD_ABSX, "AND"}, {0x3e, 3, 7, AD_ABSX, "ROL"}, {0x3f, 3, 7, AD_ABSX, "RLA"},
  {0x40, 1, 6, AD_IMPL, "RTI"}, {0x41, 2, 6, AD_XIND, "EOR"}, {0x42, 1, 1, AD_IMPL, "JAM"}, {0x43, 2, 8, AD_XIND, "SRE"},
  {0x44, 2, 3, AD_ZP, "NOP"}, {0x45, 2, 4, AD_ZP, "EOR"}, {0x46, 2, 5, AD_ZP, "LSR"}, {0x47, 2, 5, AD_ZP, "SRE"},
  {0x48, 1, 3, AD_IMPL, "PHA"}, {0x49, 2, 2, AD_IMM, "EOR"}, {0x4a, 1, 2, AD_ACC, "LSR"}, {0x4b, 2, 2, AD_IMM, "ASR"},
  {0x4c, 3, 3, AD_ABS, "JMP"}, {0x4d, 3, 4, AD_ABS, "EOR"}, {0x4e, 3, 6, AD_ABS, "LSR"}, {0x4f, 3, 6, AD_ABS, "SRE"},
  {0x50, 2, 2, AD_REL, "BVC"}, {0x51, 2, 5, AD_INDY, "EOR"}, {0x52, 1, 1, AD_IMPL, "JAM"}, {0x53, 2, 8, AD_INDY, "SRE"},
  {0x54, 2, 4, AD_ZPX, "NOP"}, {0x55, 2, 4, AD_ZPX, "EOR"}, {0x56, 2, 6, AD_ZPX, "LSR"}, {0x57, 2, 6, AD_ZPX, "SRE"},
  {0x58, 1, 2, AD_IMPL, "CLI"}, {0x59, 3, 4, AD_ABSY, "EOR"}, {0x5a, 1, 2, AD_IMPL, "NOP"}, {0x5b, 3, 7, AD_ABSY, "SRE"},
  {0x5c, 3, 4, AD_ABSX, "NOP"}, {0x5d, 3, 4, AD_ABSX, "EOR"}, {0x5e, 3, 7, AD_ABSX, "LSR"}, {0x5f, 3, 7, AD_ABSX, "SRE"},
  {0x60, 1, 6, AD_IMPL, "RTS"}, {0x61, 2, 6, AD_XIND, "ADC"}, {0x62, 1, 1, AD_IMPL, "JAM"}, {0x63, 2, 8, AD_XIND, "RRA"},
  {0x64, 2, 3, AD_ZP, "NOP"}, {0x65, 2, 3, AD_ZP, "ADC"}, {0x66, 2, 5, AD_ZP, "ROR"}, {0x67, 2, 5, AD_ZP, "RRA"},
  {0x68, 1, 4, AD_IMPL, "PLA"}, {0x69, 2, 2, AD_IMM, "ADC"}, {0x6a, 1, 2, AD_ACC, "ROR"}, {0x6b, 2, 2, AD_IMM, "ARR"},
  {0x6c, 3, 5, AD_IND, "JMP"}, {0x6d, 3, 4, AD_ABS, "ADC"}, {0x6e, 3, 6, AD_ABS, "ROR"}, {0x6f, 3, 6, AD_ABS, "RRA"},
  {0x70, 2, 2, AD_REL, "BVS"}, {0x71, 2, 5, AD_INDY, "ADC"}, {0x72, 1, 1, AD_IMPL, "JAM"}, {0x73, 2, 8, AD_INDY, "RRA"},
  {0x74, 2, 4, AD_ZPX, "NOP"}, {0x75, 2, 4, AD_ZPX, "ADC"}, {0x76, 2, 6, AD_ZPX, "ROR"}, {0x77, 2, 6, AD_ZPX, "RRA"},
  {0x78, 1, 2, AD_IMPL, "SEI"}, {0x79, 3, 4, AD_ABSY, "ADC"}, {0x7a, 1, 2, AD_IMPL, "NOP"}, {0x7b, 3, 7, AD_ABSY, "RRA"},
  {0x7c, 3, 4, AD_ABSX, "NOP"}, {0x7d, 3, 4, AD_ABSX, "ADC"}, {0x7e, 3, 7, AD_ABSX, "ROR"}, {0x7f, 3, 7, AD_ABSX, "RRA"},
  {0x80, 2, 2, AD_IMM, "NOP"}, {0x81, 2, 6, AD_XIND, "STA"}, {0x82, 2, 2, AD_IMM, "NOP"}, {0x83, 2, 6, AD_XIND, "SAX"},
  {0x84, 2, 3, AD_ZP, "STY"}, {0x85, 2, 3, AD_ZP, "STA"}, {0x86, 2, 3, AD_ZP, "STX"}, {0x87, 2, 3, AD_ZP, "SAX"},
  {0x88, 1, 2, AD_IMPL, "DEY"}, {0x89, 2, 2, AD_IMM, "NOP"}, {0x8a, 1, 2, AD_IMPL, "TXA"}, {0x8b, 2, 2, AD_IMM, "XAA"},
  {0x8c, 3, 4, AD_ABS, "STY"}, {0x8d, 3, 4, AD_ABS, "STA"}, {0x8e, 3, 4, AD_ABS, "STX"}, {0x8f, 3, 4, AD_ABS, "SAX"},
  {0x90, 2, 2, AD_REL, "BCC"}, {0x91, 2, 6, AD_INDY, "STA"}, {0x92, 1, 1, AD_IMPL, "JAM"}, {0x93, 2, 6, AD_INDY, "SHA"},
  {0x94, 2, 4, AD_ZPX, "STY"}, {0x95, 2, 4, AD_ZPX, "STA"}, {0x96, 2, 4, AD_ZPY, "STX"}, {0x97, 2, 4, AD_ZPY, "SAX"},
  {0x98, 1, 2, AD_IMPL, "TYA"}, {0x99, 3, 5, AD_ABSY, "STA"}, {0x9a, 1, 2, AD_IMPL, "TXS"}, {0x9b, 3, 5, AD_ABSX, "SHS"},
  {0x9c, 3, 5, AD_ABSX, "SHY"}, {0x9d, 3, 5, AD_ABSX, "STA"}, {0x9e, 3, 5, AD_ABSY, "SHX"}, {0x9f, 3, 5, AD_ABSY, "SHA"},
  {0xa0, 2, 2, AD_IMM, "LDY"}, {0xa1, 2, 6, AD_XIND, "LDA"}, {0xa2, 2, 2, AD_IMM, "LDX"}, {0xa3, 2, 6, AD_XIND, "LAX"},
  {0xa4, 2, 3, AD_ZP, "LDY"}, {0xa5, 2, 3, AD_ZP, "LDA"}, {0xa6, 2, 3, AD_ZP, "LDX"}, {0xa7, 2, 3, AD_ZP, "LAX"},
  {0xa8, 1, 2, AD_IMPL, "TAY"}, {0xa9, 2, 2, AD_IMM, "LDA"}, {0xaa, 1, 2, AD_IMPL, "TAX"}, {0xab, 2, 2, AD_IMM, "LAX"},
  {0xac, 3, 4, AD_ABS, "LDY"}, {0xad, 3, 4, AD_ABS, "LDA"}, {0xae, 3, 4, AD_ABS, "LDX"}, {0xaf, 3, 4, AD_ABS, "LAX"},
  {0xb0, 2, 2, AD_REL, "BCS"}, {0xb1, 2, 5, AD_INDY, "LDA"}, {0xb2, 1, 1, AD_IMPL, "JAM"}, {0xb3, 2, 5, AD_INDY, "LAX"},
  {0xb4, 2, 4, AD_ZPX, "LDY"}, {0xb5, 2, 4, AD_ZPX, "LDA"}, {0xb6, 2, 4, AD_ZPY, "LDX"}, {0xb7, 2, 4, AD_ZPY, "LAX"},
  {0xb8, 1, 2, AD_IMPL, "CLV"}, {0xb9, 3, 4, AD_ABSY, "LDA"}, {0xba, 1, 2, AD_IMPL, "TSX"}, {0xbb, 3, 4, AD_ABSY, "LAS"},
  {0xbc, 3, 4, AD_ABSX, "LDY"}, {0xbd, 3, 4, AD_ABSX, "LDA"}, {0xbe, 3, 4, AD_ABSY, "LDX"}, {0xbf, 3, 4, AD_ABSY, "LAX"},
  {0xc0, 2, 2, AD_IMM, "CPY"}, {0xc1, 2 ,6, AD_XIND, "CMP"}, {0xc2, 2, 2, AD_IMM, "NOP"}, {0xc3, 2, 8, AD_XIND, "DCP"},
  {0xc4, 2, 3, AD_ZP, "CPY"}, {0xc5, 2, 3, AD_ZP, "CMP"}, {0xc6, 2, 5, AD_ZP, "DEC"}, {0xc7, 2, 5, AD_ZP, "DCP"},
  {0xc8, 1, 2, AD_IMPL, "INY"}, {0xc9, 2, 2, AD_IMM, "CMP"}, {0xca, 1, 2, AD_IMPL, "DEX"}, {0xcb, 2, 2, AD_IMM, "SBX"},
  {0xcc, 3, 4, AD_ABS, "CPY"}, {0xcd, 3, 4, AD_ABS, "CMP"}, {0xce, 3, 6, AD_ABS, "DEC"}, {0xcf, 3, 6, AD_ABS, "DCP"},
  {0xd0, 2, 2, AD_REL, "BNE"}, {0xd1, 2, 5, AD_INDY, "CMP"}, {0xd2, 1, 1, AD_IMPL, "JAM"}, {0xd3, 2, 8, AD_INDY, "DCP"},
  {0xd4, 2, 4, AD_ZPX, "NOP"}, {0xd5, 2, 4, AD_ZPX, "CMP"}, {0xd6, 2, 6, AD_ZPX, "DEC"}, {0xd7, 2, 6, AD_ZPX, "DCP"},
  {0xd8, 1, 2, AD_IMPL, "CLD"}, {0xd9, 3, 4, AD_ABSY, "CMP"}, {0xda, 1, 2, AD_IMPL, "NOP"}, {0xdb, 3, 7, AD_ABSY, "DCP"},
  {0xdc, 3, 4, AD_ABSX, "NOP"}, {0xdd, 3, 4, AD_ABSX, "CMP"}, {0xde, 3 ,7, AD_ABSX, "DEC"}, {0xdf, 3, 7, AD_ABSX, "DCP"},
  {0xe0, 2, 2, AD_IMM, "CPX"}, {0xe1, 2, 6, AD_XIND, "SBC"}, {0xe2, 2, 2, AD_IMM, "NOP"}, {0xe3, 2, 8, AD_XIND, "ISC"},
  {0xe4, 2, 3, AD_ZP, "CPX"}, {0xe5, 2, 3, AD_ZP, "SBC"}, {0xe6, 2, 5, AD_ZP, "INC"}, {0xe7, 2, 5, AD_ZP, "ISC"},
  {0xe8, 1, 2, AD_IMPL, "INX"}, {0xe9, 2, 2, AD_IMM, "SBC"}, {0xea, 1, 2, AD_IMPL, "NOP"}, {0xeb, 2, 2, AD_IMM, "SBC"},
  {0xec, 3, 4, AD_ABS, "CPX"}, {0xed, 3, 4, AD_ABS, "SBC"}, {0xee, 3, 6, AD_ABS, "INC"}, {0xef, 3, 6, AD_ABS, "ISC"},
  {0xf0, 2, 2, AD_REL, "BEQ"}, {0xf1, 2, 5, AD_INDY, "SBC"}, {0xf2, 1, 1, AD_IMPL, "JAM"}, {0xf3, 2, 8, AD_INDY, "ISC"},
  {0xf4, 2, 4, AD_ZPX, "NOP"}, {0xf5, 2, 4, AD_ZPX, "SBC"}, {0xf6, 2, 6, AD_ZPX, "INC"}, {0xf7, 2, 6, AD_ZPX, "ISC"},
  {0xf8, 1, 2, AD_IMPL, "SED"}, {0xf9, 3, 4, AD_ABSY, "SBC"}, {0xfa, 1, 2, AD_IMPL, "NOP"}, {0xfb, 3, 7, AD_ABSY, "ISC"},
  {0xfc, 3, 4, AD_ABSX, "NOP"}, {0xfd, 3, 4, AD_ABSX, "SBC"}, {0xfe, 3, 7, AD_ABSX, "INC"}, {0xff, 3, 7, AD_ABSX, "ISC"}
};

class CPU {
public:
  uint8_t a;
  uint8_t x;
  uint8_t y;
  uint8_t p; //flags NV1BDIZC on cmos and nes. bit 5 probably also 1 on nmos
  uint8_t sp;
  uint16_t pc;

  uint8_t *memory;
  int type;
  const Instruction *instr_set;

  unsigned long long cycles;

  CPU() : a(0), x(0), y(0), p(0), sp(0xff), memory(nullptr), type(0), cycles(0) {
    memory = new uint8_t[0x10000];
    instr_set = instr_set_nmos;
  }

  ~CPU() {
    delete[] memory;
  }

  uint16_t fetch_addr(uint16_t addr) {
    uint8_t lo = memory[addr];
    uint8_t hi = memory[addr + 1];
    return (uint16_t)((hi << 8) | lo);
  }

  uint8_t fetch_mem_byte(uint16_t addr) {
    //todo: consult memory map
    return memory[addr];
  }

  void store_mem_byte(uint16_t addr, uint8_t byte) {
    //todo: consult memory map
    memory[addr] = byte;
  }

  void push_addr(uint16_t addr) {
    memory[0x0100 + sp] = (uint8_t)(addr >> 8) & 0xff;
    memory[0x0100 + sp - 1] = (uint8_t)(addr & 0xff);
    sp -= 2;
  }

  void push_byte(uint8_t byte) {
    memory[0x0100 + sp] = byte;
    sp--;
  }

  uint16_t pop_addr() {
    sp += 2;
    uint16_t addr = (uint16_t)((memory[0x0100 + sp] << 8) | (memory[0x0100 + sp - 1]));
    return addr;
  }

  uint8_t pop_byte() {
    sp++;
    return memory[0x0100 + sp];
  }

  void branch(uint8_t opcode, uint8_t flag, int8_t offset) {
    if ((p & flag)) {
      cycles += 1;
      pc += offset;
      //todo: add another cycle if page boundary is crossed
    } else {
      pc += instr_set[opcode].bytes;
    }
  }

  void irq() {
    if (p & F_I)
      return;
    push_addr(pc);
    push_byte(p);
    p |= F_I;
    pc = fetch_addr(0xfffe);
  }

  void reset(bool cold) {
    cycles = 7; //7 cycles to start up
    pc = (uint16_t)(memory[0xfffd]) << 8 | (uint16_t)(memory[0xfffc]);
    sp -= 3;
    if (cold) {
      a = x = y = 0;
      sp = 0xfd;
      p = 0 | F_I | F_5;
    }
  }

  void dbg_print() {
    printf("NV-BDIZC PC $%04X SP $01%02X P #$%02X\n", pc, sp, p);
    printf("%08b A #$%02X X #$%02X Y #$%02X CYC %llu\n", p, a, x, y, cycles);
    printf("STACK LAST BYTE #$%02X STACK LAST WORD #$%04X", memory[0x0100 + sp + 1],
           (uint16_t)((memory[0x0100 + sp + 2] << 8) | (memory[0x0100 + sp + 1])));
  }

  void disas(uint8_t opcode) {
    printf("$%04X: %s ", pc, instr_set[opcode].mnemonic);

    switch (instr_set[opcode].mode) {
      case AD_IMM:
        printf("#$%02X", memory[pc + 1]);
        break;
      case AD_ABS:
        printf("$%04X", fetch_addr(pc + 1));
        break;
      case AD_ZP:
        printf("$%04X", memory[pc + 1]);
        break;
      case AD_ABSX:
        printf("$%04X, X", fetch_addr(pc + 1));
        break;
      case AD_ABSY:
        printf("$%04X, Y", fetch_addr(pc + 1));
        break;
      case AD_ZPX:
        printf("$%04X, X", memory[pc + 1]);
        break;
      case AD_ZPY:
        printf("$%04X, Y", memory[pc + 1]);
        break;
      case AD_IND:
        printf("($%04X)", fetch_addr(pc + 1));
        break;
      case AD_XIND:
        printf("($%04X, X)", memory[pc + 1]);
        break;
      case AD_INDY:
        printf("($%04X), Y", memory[pc + 1]);
        break;
      case AD_REL:
        printf("$%04X", pc + (int8_t)memory[pc + 1] + instr_set[opcode].bytes);
        break;
      case AD_ACC:
        printf("A");
        break;
      case AD_IMPL:
        break;
    }
    printf("\n");
  }

  void disas2(uint8_t opcode) {
    printf("%04X %02X", pc, opcode);
    if (instr_set[opcode].bytes > 1) {
      printf(" %02X", memory[pc + 1]);
    }
    if (instr_set[opcode].bytes > 2) {
      printf(" %02X", memory[pc + 2]);
    }
    printf(" %s", instr_set[opcode].mnemonic);

    switch (instr_set[opcode].mode) {
      case AD_IMM:
        printf(" #$%02X", memory[pc + 1]);
        break;
      case AD_ABS:
        printf(" $%04X", fetch_addr(pc + 1));
        break;
      case AD_ZP:
        printf(" $%02X = %02X", memory[pc + 1], fetch_mem_byte(memory[pc + 1]));
        break;
      case AD_ABSX:
        printf(" $%04X,X @ %04X = %02X", fetch_addr(pc + 1), fetch_addr(pc + 1) + x, fetch_mem_byte(fetch_addr(pc + 1) + x));
        break;
      case AD_ABSY:
        printf(" $%04X,Y @ %04X = %02X", fetch_addr(pc + 1), fetch_addr(pc + 1) + y, fetch_mem_byte(fetch_addr(pc + 1) + y));
        break;
      case AD_ZPX:
        printf(" $%02X,X @ %02X = %02X", memory[pc + 1], memory[pc + 1] + x, fetch_mem_byte(memory[pc + 1] + x));
        break;
      case AD_ZPY:
        printf(" $%02X,Y @ %02X = %02X", memory[pc + 1], memory[pc + 1] + y, fetch_mem_byte(memory[pc + 1] + y));
        break;
      case AD_IND:
        printf(" ($%04X) = %04X", fetch_addr(pc + 1), fetch_addr(fetch_addr(pc + 1)));
        break;
      case AD_XIND:
        printf(" ($%02X,X) @ %02X = %04X = %02X", memory[pc + 1], memory[pc + 1] + x, fetch_addr(memory[pc + 1] + x), fetch_mem_byte(fetch_addr(memory[pc + 1] + x)));
        break;
      case AD_INDY:
        printf(" ($%02X),Y = %04X @ %04X = %02X", memory[pc + 1], fetch_addr(memory[pc + 1]), fetch_addr(memory[pc + 1]) + y, fetch_mem_byte(fetch_addr(memory[pc + 1]) + y));
        break;
      case AD_REL:
        printf(" $%04X", pc + (int8_t)memory[pc + 1] + instr_set[opcode].bytes);
        break;
      case AD_ACC:
        //printf("A");
        break;
      case AD_IMPL:
        break;
    }
    printf(" ");

    printf("A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%d\n", a, x, y, p, sp, cycles);
  }

  int eval() {
    //fetch
    uint8_t opcode = memory[pc];
    disas2(opcode);
    cycles += instr_set[opcode].cycles;

    uint8_t operand, tmp;
    uint16_t tmpw;
    uint16_t store_addr;
    //todo: extra cycles on page cross for post indexed modes
    //TODO IMPORTANT zero page wrap just put a modulo dumb shit
    switch (instr_set[opcode].mode) {
      case AD_IMPL:
        break;
      case AD_ACC:
        operand = a;
        break;
      case AD_REL: //remember to cast to signed
      case AD_IMM:
        operand = fetch_mem_byte(pc + 1);
        break;
      case AD_ABS:
        operand = fetch_mem_byte(fetch_addr(pc + 1));
        break;
      case AD_ABSX:
        operand = fetch_mem_byte(fetch_addr(pc + 1) + x);
        break;
      case AD_ABSY:
        operand = fetch_mem_byte(fetch_addr(pc + 1) + y);
        break;
      case AD_ZP:
        operand = fetch_mem_byte(fetch_mem_byte(pc + 1));
        break;
      case AD_ZPX:
        operand = fetch_mem_byte((fetch_mem_byte(pc + 1) + x) % 0xff);
        break;
      case AD_ZPY:
        operand = fetch_mem_byte((fetch_mem_byte(pc + 1) + y) % 0xff);
        break;
      case AD_XIND:
        operand = fetch_mem_byte(fetch_addr((fetch_mem_byte(pc + 1) + x) % 0xff));
        break;
      case AD_INDY:
        operand = fetch_mem_byte(fetch_addr(fetch_mem_byte(pc + 1)) + y);
        break;
      case AD_IND: //only used by JMP ($nnnn)
        break;
      default:
        printf("Illegal addressing mode\n");
        break;
    }

    switch (instr_set[opcode].mode) {
      case AD_ABS:
        store_addr = fetch_addr(pc + 1);
        break;
      case AD_ABSX:
        store_addr = fetch_addr(pc + 1) + x;
        break;
      case AD_ABSY:
        store_addr = fetch_addr(pc + 1) + y;
        break;
      case AD_ZP:
        store_addr = (uint16_t)fetch_mem_byte(pc + 1);
        break;
      case AD_ZPX:
        store_addr = (uint16_t)fetch_mem_byte(pc + 1) + x;
        break;
      case AD_ZPY:
        store_addr = (uint16_t)fetch_mem_byte(pc + 1) + y;
        break;
      case AD_XIND:
        store_addr = fetch_addr(fetch_mem_byte(pc + 1) + x);
        break;
      case AD_INDY:
        store_addr = fetch_addr(fetch_mem_byte(pc + 1)) + y;
        break;
      default:
        break;
    }

    switch (opcode) {
      case 0xea: //NOP
        pc += instr_set[opcode].bytes;
        break;
      case 0x00: //BRK
        pc += 2;
        irq();
        return 1;
        break;
      case 0x4c:
      case 0x6c: //JMP
        //on NMOS 6502 incl 2a03, the indirect addr hi byte doesnt increment on page cross
        //eg jmp ($02ff) gets hi from 0200 and lo from 02ff
        if (instr_set[opcode].mode == AD_ABS) {
          pc = fetch_addr(pc + 1);
          break;
        }
        tmpw = fetch_addr(pc + 1);
        if ((tmpw & 0x00ff) == 0x00ff) {
          pc = (uint16_t)fetch_mem_byte(tmpw) | (uint16_t)(fetch_mem_byte(tmpw - 0x00ff)) << 8;
        } else {
          pc = fetch_addr(tmpw);
        }
        break;
      case 0x20: //JSR
        push_addr(pc + 2);
        pc = fetch_addr(pc + 1);
        break;
      case 0x40: //RTI
        p = pop_byte();
        pc = pop_addr();
        break;
      case 0x60: //RTS
        pc = pop_addr();
        pc++;
        break;
      case 0x90: //BCC
        pc += instr_set[opcode].bytes;
        if (!(p & F_C)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0xB0: //BCS
        pc += instr_set[opcode].bytes;
        if ((p & F_C)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0xF0: //BEQ
        pc += instr_set[opcode].bytes;
        if ((p & F_Z)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0x30: //BMI
        pc += instr_set[opcode].bytes;
        if ((p & F_N)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0xD0: //BNE
        pc += instr_set[opcode].bytes;
        if (!(p & F_Z)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0x10: //BPL
        pc += instr_set[opcode].bytes;
        if (!(p & F_N)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0x50: //BVC
        pc += instr_set[opcode].bytes;
        if (!(p & F_V)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0x70: //BVS
        pc += instr_set[opcode].bytes;
        if ((p & F_V)) {
          cycles += 1;
          pc += (int8_t)operand;
          //todo: add another cycle if page boundary is crossed
        }
        break;
      case 0x18: //CLC
        p &= ~(F_C);
        pc += instr_set[opcode].bytes;
        break;
      case 0xD8: //CLD
        p &= ~(F_D);
        pc += instr_set[opcode].bytes;
        break;
      case 0x58: //CLI
        p &= ~(F_I);
        pc += instr_set[opcode].bytes;
        break;
      case 0xB8: //CLV
        p &= ~(F_V);
        pc += instr_set[opcode].bytes;
        break;
      case 0x38: //SEC
        p |= F_C;
        pc += instr_set[opcode].bytes;
        break;
      case 0xF8: //SED
        p |= F_D;
        pc += instr_set[opcode].bytes;
        break;
      case 0x78: //SEI
        p |= F_I;
        pc += instr_set[opcode].bytes;
        break;
      case 0x48: //PHA
        push_byte(a);
        pc += instr_set[opcode].bytes;
        break;
      case 0x08: //PHP
        push_byte(p | F_5 | F_B);
        pc += instr_set[opcode].bytes;
        break;
      case 0x68: //PLA
        a = pop_byte();
        p = (a & F_N) ? p | F_N : p & ~F_N;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        pc += instr_set[opcode].bytes;
        break;
      case 0x28: //PLP
        p = pop_byte();
        p |= F_5; //this bit is always set
        p &= ~F_B; //this bit is only set when pushed, by hardware or by BRK
        pc += instr_set[opcode].bytes;
        break;
      case 0x29:
      case 0x2d:
      case 0x3d:
      case 0x39:
      case 0x25:
      case 0x35:
      case 0x21:
      case 0x31: //AND
        a &= operand;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        pc += instr_set[opcode].bytes;
        break;
      case 0x2c:
      case 0x24: //BIT
        tmp = a & operand;
        p = (operand & F_N) ? p | F_N : p & ~F_N;
        p = (operand & F_V) ? p | F_V : p & ~F_V;
        p = (tmp == 0) ? p | F_Z : p & ~F_Z;
        pc += instr_set[opcode].bytes;
        break;
      case 0x49:
      case 0x4d:
      case 0x5d:
      case 0x59:
      case 0x45:
      case 0x55:
      case 0x41:
      case 0x51: //EOR
        a ^= operand;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        pc += instr_set[opcode].bytes;
        break;
      case 0x09:
      case 0x0d:
      case 0x1d:
      case 0x19:
      case 0x05:
      case 0x15:
      case 0x01:
      case 0x11: //ORA
        a |= operand;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        pc += instr_set[opcode].bytes;
        break;
      case 0xa9:
      case 0xad:
      case 0xbd:
      case 0xb9:
      case 0xa5:
      case 0xb5:
      case 0xa1:
      case 0xb1: //LDA
        a = operand;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xa2:
      case 0xae:
      case 0xbe:
      case 0xa6:
      case 0xb6: //LDX
        x = operand;
        p = (x == 0) ? p | F_Z : p & ~F_Z;
        p = (x & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xa0:
      case 0xac:
      case 0xbc:
      case 0xa4:
      case 0xb4: //LDY
        y = operand;
        p = (y == 0) ? p | F_Z : p & ~F_Z;
        p = (y & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0x8d:
      case 0x9d:
      case 0x99:
      case 0x85:
      case 0x95:
      case 0x81:
      case 0x91: //STA
        store_mem_byte(store_addr, a);
        pc += instr_set[opcode].bytes;
        break;
      case 0x8e:
      case 0x86:
      case 0x96: //STX
        store_mem_byte(store_addr, x);
        pc += instr_set[opcode].bytes;
        break;
      case 0x8c:
      case 0x84:
      case 0x94: //STY
        store_mem_byte(store_addr, y);
        pc += instr_set[opcode].bytes;
        break;
      case 0xaa: //TAX
        x = a;
        p = (x == 0) ? p | F_Z : p & ~F_Z;
        p = (x & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xa8: //TAY
        y = a;
        p = (y == 0) ? p | F_Z : p & ~F_Z;
        p = (y & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xba: //TSX
        x = sp;
        p = (x == 0) ? p | F_Z : p & ~F_Z;
        p = (x & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0x8a: //TXA
        a = x;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0x9a: //TXS
        sp = x;
        pc += instr_set[opcode].bytes;
        break;
      case 0x98: //TYA
        a = y;
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0x0a:
      case 0x0e:
      case 0x1e:
      case 0x06:
      case 0x16: //ASL
        if (instr_set[opcode].mode == AD_ACC) {
          p = (a & F_N) ? p | F_C : p & ~F_C;
          a = a << 1;
          p = (a == 0) ? p | F_Z : p & ~F_Z;
          p = (a & F_N) ? p | F_N : p & ~F_N;
          pc += instr_set[opcode].bytes;
          break;
        }
        p = (operand & F_N) ? p | F_C : p & ~F_C;
        operand = operand << 1;
        p = (operand == 0) ? p | F_Z : p & ~F_Z;
        p = (operand & F_N) ? p | F_N : p & ~F_N;
        store_mem_byte(store_addr, operand);
        pc += instr_set[opcode].bytes;
        break;
      case 0x4a:
      case 0x4e:
      case 0x5e:
      case 0x46:
      case 0x56: //LSR
        if (instr_set[opcode].mode == AD_ACC) {
          p = (a & F_C) ? p | F_C : p & ~F_C;
          a = a >> 1;
          p = (a == 0) ? p | F_Z : p & ~F_Z;
          p = p & ~F_N;
          pc += instr_set[opcode].bytes;
          break;
        }
        p = (operand & F_C) ? p | F_C : p & ~F_C;
        operand = operand >> 1;
        p = (operand == 0) ? p | F_Z : p & ~F_Z;
        p = p & ~F_N;
        store_mem_byte(store_addr, operand);
        pc += instr_set[opcode].bytes;
        break;
      case 0x2a:
      case 0x2e:
      case 0x3e:
      case 0x26:
      case 0x36: //ROL
        tmp = p;
        if (instr_set[opcode].mode == AD_ACC) {
          p = (a & F_N) ? p | F_C : p & ~F_C;
          a = a << 1;
          a |= tmp & F_C;
          p = (a == 0) ? p | F_Z : p & ~F_Z;
          p = (a & F_N) ? p | F_N : p & ~F_N;
          pc += instr_set[opcode].bytes;
          break;
        }
        p = (operand & F_N) ? p | F_C : p & ~F_C;
        operand = operand << 1;
        operand |= tmp & F_C;
        p = (operand == 0) ? p | F_Z : p & ~F_Z;
        p = (operand & F_N) ? p | F_N : p & ~F_N;
        store_mem_byte(store_addr, operand);
        pc += instr_set[opcode].bytes;
        break;
      case 0x6a:
      case 0x6e:
      case 0x7e:
      case 0x66:
      case 0x76: //ROR
        tmp = p;
        if (instr_set[opcode].mode == AD_ACC) {
          p = (a & F_C) ? p | F_C : p & ~F_C;
          a = a >> 1;
          a |= (tmp & F_C) << 7;
          p = (a == 0) ? p | F_Z : p & ~F_Z;
          p = (tmp & F_C) ? p | F_N : p & ~F_N;
          pc += instr_set[opcode].bytes;
          break;
        }
        p = (operand & F_C) ? p | F_C : p & ~F_C;
        operand = operand >> 1;
        operand |= (tmp & F_C) << 7;
        p = (operand == 0) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_C) ? p | F_N : p & ~F_N;
        store_mem_byte(store_addr, operand);
        pc += instr_set[opcode].bytes;
        break;
      case 0x69:
      case 0x6d:
      case 0x7d:
      case 0x79:
      case 0x65:
      case 0x75:
      case 0x61:
      case 0x71: //ADC
        tmp = a;
        a = a + operand + ((p & F_C) ? 1 : 0);
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        p = (tmp + operand + ((p & F_C) ? 1 : 0) > 255) ? p | F_C : p & ~F_C; //todo: decimal mode carry
        p = ((tmp & 0x80) != (a & 0x80)) ? p | F_V : p & ~F_V;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xc9:
      case 0xcd:
      case 0xdd:
      case 0xd9:
      case 0xc5:
      case 0xd5:
      case 0xc1:
      case 0xd1: //CMP
        tmp = a - operand;
        p = (a == operand) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_N) ? p | F_N : p & ~F_N;
        p = (operand <= a) ? p | F_C : p & ~F_C;
        pc += instr_set[opcode].bytes;
        break;
      case 0xe0:
      case 0xec:
      case 0xe4: //CPX
        tmp = x - operand;
        p = (x == operand) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_N) ? p | F_N : p & ~F_N;
        p = (operand <= x) ? p | F_C : p & ~F_C;
        pc += instr_set[opcode].bytes;
        break;
      case 0xc0:
      case 0xcc:
      case 0xc4: //CPY
        tmp = y - operand;
        p = (y == operand) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_N) ? p | F_N : p & ~F_N;
        p = (operand <= y) ? p | F_C : p & ~F_C;
        pc += instr_set[opcode].bytes;
        break;
      case 0xe9:
      case 0xed:
      case 0xfd:
      case 0xf9:
      case 0xe5:
      case 0xf5:
      case 0xe1:
      case 0xf1: //SBC
        a = a - operand - ((p & F_C) ? 0 : 1);
        p = (a == 0) ? p | F_Z : p & ~F_Z;
        p = (a & F_N) ? p | F_N : p & ~F_N;
        p = ((int8_t)a >= 0) ? p | F_C : p & ~F_C;
        //todo: overflow
        pc += instr_set[opcode].bytes;
        break;
      case 0xce:
      case 0xde:
      case 0xc6:
      case 0xd6: //DEC
        tmp = operand - 1;
        p = (tmp == 0) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_N) ? p | F_N : p & ~F_N;
        store_mem_byte(store_addr, tmp);
        pc += instr_set[opcode].bytes;
        break;
      case 0xca: //DEX
        x--;
        p = (x == 0) ? p | F_Z : p & ~F_Z;
        p = (x & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0x88: //DEY
        y--;
        p = (y == 0) ? p | F_Z : p & ~F_Z;
        p = (y & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xee:
      case 0xfe:
      case 0xe6:
      case 0xf6: //INC
        tmp = operand + 1;
        p = (tmp == 0) ? p | F_Z : p & ~F_Z;
        p = (tmp & F_N) ? p | F_N : p & ~F_N;
        store_mem_byte(store_addr, tmp);
        pc += instr_set[opcode].bytes;
        break;
      case 0xe8: //INX
        x++;
        p = (x == 0) ? p | F_Z : p & ~F_Z;
        p = (x & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      case 0xc8: //INY
        y++;
        p = (y == 0) ? p | F_Z : p & ~F_Z;
        p = (y & F_N) ? p | F_N : p & ~F_N;
        pc += instr_set[opcode].bytes;
        break;
      default:
        //printf("Unimplemented %s\n", instr_set[opcode].mnemonic);
        pc += instr_set[opcode].bytes;
        break;
    }
    return 0;
  }
};
