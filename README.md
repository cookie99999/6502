These are the relevant files for my ongoing WDC 65C816 based homebrew computer. It started off pretty similar to Ben Eater's 65C02 computer, but I've made significant changes since then and will continue to do so.

## Current Specs:
- WDC 65C816 clocked at 2MHz
- 512K SRAM (slightly less than that available to the user due to the memory mapping used)
- 8K NOR flash accessible to the user
- 2 6522 VIAs providing 2 8-bit GPIO ports and a couple timers and shift registers each
- 6551 ACIA UART for serial I/O

## To do list:
- Get stable performance at higher clock speeds
- Replace the 6551 with a UART that isn't so terrible
- Add a PS/2 keyboard interface (easily done with an ATTiny and one of the VIA ports)
- Currently in progress: use a Raspberry Pi Pico 2 as a video coprocessor to provide VGA output, as well as handling a microSD card for mass storage
- Once all of the above are done, design and manufacture a PCB to replace my wire-wrapped protoboard

I've used and modified ideas from many sources, notably Ben Eater, Garth Wilson, Daryl Rictor, Grant Seale, and many users and articles on the 6502.org forum.
