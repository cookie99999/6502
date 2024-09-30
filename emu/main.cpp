#include <stdint.h>
#include <iostream>
#include <fstream>
#include "cpu.hpp"

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cout << "Usage: emu6502 <file>" << std::endl;
    exit(1);
  }

  std::ifstream binfile;
  binfile.open(argv[1], std::ios::binary);
  if (!binfile.is_open()) {
    std::cout << "Error opening file" << std::endl;
    exit(1);
  }

  CPU cpu;
  binfile.read((char*)cpu.memory, 0x10000);
  binfile.close();

  for (;;) {
    cpu.eval();
  }

  return 0;
}
