/**
 * Hunter Adams (vha3@cornell.edu)
 * 
 *
 * HARDWARE CONNECTIONS
 *  - GPIO 16 ---> VGA Hsync
 *  - GPIO 17 ---> VGA Vsync
 *  - GPIO 18 ---> 330 ohm resistor ---> VGA Red
 *  - GPIO 19 ---> 330 ohm resistor ---> VGA Green
 *  - GPIO 20 ---> 330 ohm resistor ---> VGA Blue
 *  - RP2040 GND ---> VGA GND
 *
 * RESOURCES USED
 *  - PIO state machines 0, 1, and 2 on PIO instance 0
 *  - DMA channels 0, 1, 2, and 3
 *  - 153.6 kBytes of RAM (for pixel color data)
 *
 * NOTE
 *  - This is a translation of the display primitives
 *    for the PIC32 written by Bruce Land and students
 *
 */


// Give the I/O pins that we're using some names that make sense - usable in main()
enum vga_pins {HSYNC=16, VSYNC, RED_PIN, GREEN_PIN, BLUE_PIN} ;

// We can only produce 8 (3-bit) colors, so let's give them readable names - usable in main()
enum colors {BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE} ;

// VGA primitives - usable in main
void init_VGA(void) ;
void draw_pixel(short x, short y, uint8_t color) ;
void draw_v_line(short x, short y, short h, uint8_t color) ;
void draw_h_line(short x, short y, short w, uint8_t color) ;
void draw_line(short x0, short y0, short x1, short y1, uint8_t color) ;
void draw_rect(short x, short y, short w, short h, uint8_t color);
void drawCircle(short x0, short y0, short r, char color) ;
void drawCircleHelper( short x0, short y0, short r, unsigned char cornername, char color) ;
void fillCircle(short x0, short y0, short r, char color) ;
void fillCircleHelper(short x0, short y0, short r, unsigned char cornername, short delta, char color) ;
void drawRoundRect(short x, short y, short w, short h, short r, char color) ;
void fillRoundRect(short x, short y, short w, short h, short r, char color) ;
void fill_rect(short x, short y, short w, short h, uint8_t color) ;
void draw_char(short x, short y, uint8_t c, uint8_t color, uint8_t bg, unsigned char size) ;
void set_cursor(short x, short y);
void setTextColor(uint8_t c);
void set_text_color(uint8_t c, uint8_t bg);
void set_text_size(unsigned char s);
void set_text_wrap(char w);
void draw_char_cursor(uint8_t c) ;
void write_string(char* str) ;
