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
#define PH2 26
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

volatile static bool cursor_state = false;
volatile struct repeating_timer timer;
extern char textcolor;
extern char textbgcolor;
extern unsigned short textsize;
extern unsigned short cursor_x;
extern unsigned short cursor_y;
#define WIDTH 640
#define HEIGHT 480

enum states {
  TEXT_MODE,
  BITMAP_MODE,
  TILEMAP_MODE
};
volatile static char cur_state = TEXT_MODE;

enum cmds {
  DRAW_PIXEL = 0, GO_TEXT, GO_TILE, GO_BITMAP, CLEAR, DRAW_TRI, RES0,
  RES1, BACKSPACE, RES2, LF, RES3, RES4, CR
};

static void __not_in_flash_func(read_dbus)(void) {
  *dbus_wptr++ = dbus_pio->rxf[dbus_sm] >> 24;
  dbus_pio->irq = (1u << 0);
  if (dbus_wptr >= dbus_bufend)
    dbus_wptr = dbus_buf;
}

__attribute__((optimize("O0"))) bool get_dbus_byte(uint8_t *byte) {
  if (dbus_rptr != dbus_wptr) {
    *byte = *dbus_rptr++;
    if (dbus_rptr >= dbus_bufend)
      dbus_rptr = dbus_buf;
    return true;
  }
  return false;
}

__attribute__((optimize("O0"))) uint8_t get_dbus_byte_blocking() {
  while (dbus_rptr == dbus_wptr) {
    ;
  }
  uint8_t byte;
  byte = *dbus_rptr++;
  if (dbus_rptr >= dbus_bufend)
    dbus_rptr = dbus_buf;
  return byte;
}

bool toggle_cursor(__unused struct repeating_timer *t) {
  if (cursor_state) {
    fill_rect(cursor_x, cursor_y, 6, 8, textcolor);
  } else {
    fill_rect(cursor_x, cursor_y, 6, 8, textbgcolor);
  }
  cursor_state = !cursor_state;
  return true;
}

void dbus_task(void) {
  uint8_t byte;
  if (get_dbus_byte(&byte)) {
    switch (cur_state) {
    case TEXT_MODE:
      switch (byte) {
      case GO_BITMAP:
	cancel_repeating_timer(&timer);
	fill_rect(cursor_x, cursor_y, 6, 8, textbgcolor);
	cur_state = BITMAP_MODE;
	break;
      case CLEAR:
	fill_rect(0, 0, 640, 480, textbgcolor);
	cursor_x = 0;
	cursor_y = 0;
	break;
      case BACKSPACE:
	cursor_x -= textsize * 6;
	if (cursor_x < 0) {
	  cursor_y -= textsize * 8;
	  cursor_x = (WIDTH - (textsize * 6));
	}
	fill_rect(cursor_x, cursor_y, 6 * textsize, 8 * textsize, textbgcolor);
	break;
      default:
	cancel_repeating_timer(&timer);
	fill_rect(cursor_x, cursor_y, 6, 8, textbgcolor);
	draw_char_cursor(byte);
	add_repeating_timer_ms(500, toggle_cursor, NULL, &timer);
	break;
      }
      break;
    case BITMAP_MODE:
      switch (byte) {
      case DRAW_PIXEL: { //draw pixel xlo xhi ylo yhi color
	uint16_t x = (uint16_t)get_dbus_byte_blocking();
	x = x | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint16_t y = (uint16_t)get_dbus_byte_blocking();
	y = y | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint8_t color = get_dbus_byte_blocking();
	draw_pixel(x, y, color);
      }
	break;
      case DRAW_TRI: { //draw_tri x1[2] y1[2] x2[2] y2[2] x3[2] y3[2] color
	uint16_t x1 = (uint16_t)get_dbus_byte_blocking();
	x1 = x1 | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint16_t y1 = (uint16_t)get_dbus_byte_blocking();
	y1 = y1 | ((uint16_t)get_dbus_byte_blocking() << 8);

	uint16_t x2 = (uint16_t)get_dbus_byte_blocking();
	x2 = x2 | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint16_t y2 = (uint16_t)get_dbus_byte_blocking();
	y2 = y2 | ((uint16_t)get_dbus_byte_blocking() << 8);

	uint16_t x3 = (uint16_t)get_dbus_byte_blocking();
	x3 = x3 | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint16_t y3 = (uint16_t)get_dbus_byte_blocking();
	y3 = y3 | ((uint16_t)get_dbus_byte_blocking() << 8);
	uint8_t color = get_dbus_byte_blocking();

	draw_tri(x1, y1, x2, y2, x3, y3, color);
      }
	break;
      case CLEAR: { //clear color
	uint8_t color = get_dbus_byte_blocking();
	fill_rect(0, 0, 640, 480, color);
      }
	break;
      case GO_TEXT:
	add_repeating_timer_ms(500, toggle_cursor, NULL, &timer);
	cur_state = TEXT_MODE;
	break;
      default:
	break;
      }
      break;
    default:
      break;
    }
  }
}

void main() {
  set_sys_clock_khz(150000, true);
  stdio_init_all();

  init_VGA();
  fill_rect(0, 0, 640, 480, CYAN);
  set_text_size(1);
  set_text_wrap(1);
  set_cursor(0, 0);
  set_text_color(BLACK, CYAN);

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

  add_repeating_timer_ms(500, toggle_cursor, NULL, &timer);
  
  while (true) {
    dbus_task();
  }
}
