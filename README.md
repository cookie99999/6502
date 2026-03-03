These are the relevant files for my ongoing WDC 65C816 based homebrew computer. It started off pretty similar to Ben Eater's 65C02 computer, but I've made significant changes since then and will continue to do so.

## Current Specs:
- WDC 65C816 clocked at 4MHz (reasonably stable at 6MHz, fails to boot at 8 likely due to slow memory/logic)
- 512K SRAM
- 8K NOR flash accessible to the user
- 2 6522 VIAs providing 2 8-bit GPIO ports and a couple timers and shift registers each
- 6551 ACIA UART for serial I/O. Rockwell part works only up to about 4MHz, but avoids the TX empty flag bug of the WDC part
- PS/2 interface provided by ATTiny26L on one of the VIA ports
- CompactFlash storage (via PATA adapter)
- VGA video output from a Pi Pico 2 providing text display and (very slow) bitmapped graphics
- expensive and oversized 4-layer PCB

## To do list:
- Bitbang an SPI RTC on a remaining VIA port
- possibly look into sound and/or joysticks on the other 2
- new PCB layout using more SMD/PLCC parts to reduce size and cost, with more SRAM and a proper DE-9 serial port

I've used and modified ideas from many sources, notably Ben Eater, Garth Wilson, Daryl Rictor, Grant Seale, and many users and articles on the 6502.org forum.
