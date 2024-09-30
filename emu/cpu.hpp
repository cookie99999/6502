#include <stdint.h>
#include <iostream>

#define F_N 0b10000000
#define F_V 0b01000000
#define F_5 0b00100000
#define F_B 0b00010000
#define F_D 0b00001000
#define F_I 0b00000100
#define F_Z 0b00000010
#define F_C 0b00000001

struct Instruction {
    uint8_t opcode;
    short bytes;
    short cycles;
    const char *mnemonic;

    Instruction(uint8_t o, short b, short c, const char *m) : opcode(o), bytes(b), cycles(c), mnemonic(m) {}
    Instruction() {}
};

Instruction instr_set[256] = {
  {0x00, 1, 7, "BRK"}, {0x01, 2, 6, "ORA"}, {0x02, 1, 1, "UNP"}, {0x03, 1, 1, "UNP"},
  {0x04, 1, 1, "UNP"}, {0x05, 2, 3, "ORA"}, {0x06, 2, 5, "ASL"}, {0x07, 1, 1, "UNP"},
  {0x08, 1, 3, "PHP"}, {0x09, 2, 2, "ORA"}, {0x0a, 1, 2, "ASL"}, {0x0b, 1, 1, "UNP"},
  {0x0c, 1, 1, "UNP"}, {0x0d, 3, 4, "ORA"}, {0x0e, 3, 6, "ASL"}, {0x0f, 1, 1, "UNP"},
  {0x10, 2, 2, "BPL"}, {0x11, 2, 5, "ORA"}, {0x12, 1, 1, "UNP"}, {0x13, 1, 1, "UNP"},
  {0x14, 1, 1, "UNP"}, {0x15, 2, 4, "ORA"}, {0x16, 2, 6, "ASL"}, {0x17, 1, 1, "UNP"},
  {0x18, 1, 2, "CLC"}, {0x19, 3, 4, "ORA"}, {0x1a, 1, 1, "UNP"}, {0x1b, 1, 1, "UNP"},
  {0x1c, 1, 1, "UNP"}, {0x1d, 3, 4, "ORA"}, {0x1e, 3, 7, "ASL"}, {0x1f, 1, 1, "UNP"}
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

  CPU() : a(0), x(0), y(0), p(0), sp(0xff), memory(nullptr), type(0) {
    memory = new uint8_t[0x10000];


  }

  ~CPU() {
    delete[] memory;
  }

  void reset(bool cold) {
    pc = (uint16_t)(memory[0xfffd]) << 8 | (uint16_t)(memory[0xfffc]);
    sp -= 3;
    if (cold) {
      a = x = y = 0;
      sp = 0xfd;
      p = 0 | F_I | F_5;
    }
  }

  void eval() {
    //fetch
    uint8_t opcode = memory[pc];

    //ora 01 11 05 15 09 19 0d 1d
    // 00000001
    // 00010001
    // 00000101
    // 00010101
    // 00001001
    // 00011001
    // 00001101
    // 00011101

    switch (opcode) {
      case 0x00:
      case 0x01:
      case 0x02:
      case 0x03:
      case 0x04:
      case 0x05:
      case 0x06:
      case 0x07:
      case 0x08:
      case 0x09:
      case 0x0a:
      case 0x0b:
      case 0x0c:
      case 0x0d:
      case 0x0e:
      case 0x0f:
        std::cout << instr_set[opcode].mnemonic << std::endl;
        pc += instr_set[opcode].bytes;
        break;
      default:
        //std::cout << "UNIMP" << std::endl;
        pc += 1;
        break;
    }
  }
};
