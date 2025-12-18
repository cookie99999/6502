cmake_minimum_required(VERSION 3.13)

set(PICO_BOARD pico2_w)
include(pico_sdk_import.cmake)

project(montest C CXX ASM)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
pico_sdk_init()

add_executable(montest
  montest.c vga_graphics.c
)

pico_generate_pio_header(montest ${CMAKE_CURRENT_LIST_DIR}/dbus.pio)
pico_generate_pio_header(montest ${CMAKE_CURRENT_LIST_DIR}/hsync.pio)
pico_generate_pio_header(montest ${CMAKE_CURRENT_LIST_DIR}/vsync.pio)
pico_generate_pio_header(montest ${CMAKE_CURRENT_LIST_DIR}/rgb.pio)
pico_enable_stdio_usb(montest 1)
pico_enable_stdio_uart(montest 0)

pico_add_extra_outputs(montest)

target_link_libraries(montest pico_stdlib hardware_spi hardware_pio hardware_dma hardware_adc hardware_irq)
