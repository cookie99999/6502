#include <stdio.h>
#include <stdint.h>
#include "pico/stdlib.h"
#include "hardware/gpio.h"
#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "hardware/structs/bus_ctrl.h"

#include "dbus.pio.h"
#include "vga_graphics.h"

#define D0 6
#define D1 7
#define D2 8
#define D3 9
#define D4 10
#define D5 11
#define D6 12
#define D7 13

#define RS0 14
#define RS1 15
#define RS2 26
#define RS3 27

#define RWB 0
#define CSB 1
#define PH2 21
#define RESB 22
#define IRQB 28

static uint8_t dbus_buf[5120];
static uint8_t *dbus_bufend = dbus_buf + sizeof(dbus_buf);
volatile static uint8_t *dbus_rptr = dbus_buf;
volatile static uint8_t *dbus_wptr = dbus_buf;
static PIO dbus_pio;
static uint dbus_sm;
static uint dbus_offset;
static uint dbus_pio_irq;

static void __time_critical_func(read_dbus)(void) {
  *dbus_wptr++ = dbus_pio->rxf[dbus_sm] >> 24;
  dbus_pio->irq = (1u << 0);
  if (dbus_wptr >= dbus_bufend)
    dbus_wptr = dbus_buf;
}

bool get_dbus_byte(uint8_t *byte) {
  if (dbus_rptr != dbus_wptr) {
    *byte = *dbus_rptr++;
    if (dbus_rptr >= dbus_bufend)
      dbus_rptr = dbus_buf;
    return true;
  }
  return false;
}

uint8_t get_dbus_byte_blocking() {
  while (dbus_rptr == dbus_wptr)
    sleep_us(1);
  uint8_t byte;
  byte = *dbus_rptr++;
  if (dbus_rptr >= dbus_bufend)
    dbus_rptr = dbus_buf;
  return byte;
}

void dbus_task(void) {
  uint8_t byte;
  if (get_dbus_byte(&byte)) {
    switch (byte) {
    case 0x00: //draw pixel xlo xhi ylo yhi color
      uint16_t x = (uint16_t)get_dbus_byte_blocking();
      x = x | ((uint16_t)get_dbus_byte_blocking() << 8);
      uint16_t y = (uint16_t)get_dbus_byte_blocking();
      y = y | ((uint16_t)get_dbus_byte_blocking() << 8);
      uint8_t color = get_dbus_byte_blocking();
      draw_pixel(x, y, color);
      break;
    default:
      draw_char_cursor(byte);
      break;
    }
  }
}

void main() {
  set_sys_clock_khz(150000, true);
  stdio_init_all();
  printf("start\n");

  init_VGA();
  fill_rect(0, 0, 640, 480, CYAN);
  set_text_size(1);
  set_text_wrap(1);
  set_cursor(0, 0);
  set_text_color(BLACK, CYAN);
  write_string("test\n");

  dbus_rptr = dbus_wptr = dbus_buf;
  dbus_pio = pio0;
  dbus_pio_irq = PIO0_IRQ_0;
  dbus_offset = pio_add_program(dbus_pio, &dbus_program);
  dbus_sm = pio_claim_unused_sm(dbus_pio, true);
  dbus_program_init(dbus_pio, dbus_sm, dbus_offset, D0);
  pio_gpio_init(dbus_pio, CSB);
  pio_gpio_init(dbus_pio, PH2);
  pio_gpio_init(dbus_pio, RWB);
  gpio_set_dir(CSB, GPIO_IN);
  gpio_set_dir(PH2, GPIO_IN);
  gpio_set_dir(RWB, GPIO_IN);
  pio_sm_set_jmp_pin(dbus_pio, dbus_sm, RWB);
  pio_set_irq0_source_enabled(dbus_pio, pis_interrupt0, true);
  irq_set_exclusive_handler(dbus_pio_irq, read_dbus);
  irq_set_enabled(dbus_pio_irq, true);
  pio_sm_set_enabled(dbus_pio, dbus_sm, true);
  
  while (true) {
    dbus_task();
    sleep_ms(1);
  }
}
