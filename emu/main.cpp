#include <stdint.h>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>
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

  uint8_t ines_prg_sz;
  unsigned int nes_size = 0;
  if (std::string(argv[1]).find(".nes") != std::string::npos) {
    binfile.seekg(4, std::ios::beg);
    binfile.read((char*)&ines_prg_sz, 1);
    nes_size = ines_prg_sz * 0x4000;
    binfile.seekg(0, std::ios::beg);
  }

  CPU cpu;
  unsigned int offset = 0;
  if (argc >= 4 && std::string(argv[2]) == "--org") {
    offset = std::stoi(argv[3], nullptr, 16);
    if (offset < 0 || offset > 0xffff) {
      std::cout << "Error: offset out of bounds" << std::endl;
      exit(1);
    }
  }

  if (nes_size > 0) {
    binfile.seekg(16, std::ios::beg);
    binfile.read((char*)(cpu.memory + offset), nes_size);
  } else {
    binfile.read((char*)(cpu.memory + offset), std::filesystem::file_size(argv[1]));
  }
  binfile.close();

  bool ss = false; //single step mode
  if (argc >= 5 && std::string(argv[4]) == "--ss")
    ss = true;

  cpu.reset(true);
  if (std::string(argv[1]).find(".nes") != std::string::npos)
    cpu.pc = 0xc000;
  for (;;) {
    if (cpu.eval())
      break;
    if (ss) {
      //cpu.dbg_print();
      std::cin.ignore();
    }
  }
  if (std::string(argv[1]).find(".nes") != std::string::npos)
    printf("$0002 = #$%02X $0003 = #$%02X\n", cpu.memory[0x02], cpu.memory[0x03]);

  return 0;
}
