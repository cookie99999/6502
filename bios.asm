  .setcpu "65C02"
  .include "hardware.inc"

  .segment "BIOS"
  tv_idle = $01ff ; 1 byte

  .include "protoforth.asm"

  .export init
  .export tv_output
  .export tv_output2
  .export getchar

RESET:
  sei
  cld
  ldx #$fa
  txs
  jsr init
  jmp COLD_S

init:
  ; port b (tv text output avr)
  lda VIA2_DDRB
  and #$7f ; bit 7 is input
  sta VIA2_DDRB
  ;lda VIA2_ACR
  ;and #$e3
  ;ora #$18
  ;sta VIA2_ACR ; shift out under control of phi2
  lda #$1b
  sta VIA2_ACR
  lda #$04
  sta VIA2_IER ; disable shift register interrupts

  ; port a (avr keyboard controller)
  stz VIA2_DDRA ; all inputs
  lda VIA2_PCR
  ora #$0a
  and #$fa
  sta VIA2_PCR ; ca2 pulse output, ca1 negative edge
  ;lda VIA2_ACR
  ;ora #1
  ;sta VIA2_ACR ; enable latching on port a
  lda #$82
  sta VIA2_IER ; enable ca1 interrupts

  lda #$00
  sta tv_idle
  PUT $2000
  jsr DELAY
  lda #$0c
  jsr tv_output ; form feed to clear screen
  ldy #$7f
  lda #%10100111
  rts

getchar:
  lda VIA2_IFR
  and #$02
  beq getchar
  lda #$02
  sta VIA2_IFR
  lda VIA2_PA
  beq getchar
  rts

LOAD:
  rts

SAVE:
  rts

ISCNTC:
  rts

tv_output:
  php
  pha
  lda tv_idle
  eor #$80
  sta tv_idle
@1:
  lda VIA2_PB
  eor tv_idle
  bpl @1 			; wait until avr is idle and ready
  pla
  sta VIA2_SR		; shift out command/data
  plp
  rts

tv_output2:
  bit VIA2_PB
  bmi tv_output2 ; wait for ready
  sta VIA2_SR
  rts

tv_puts:
  lda (0, x)
  beq @quit		; zero terminator
  jsr tv_output
  jsr INC_16
  jmp tv_puts
@quit:
  DROP
  rts

teststr:
  .byte "Peepee poopoo", 0

irq:
nmi:
  lda #'X'
  jsr tv_output
  rti

  ;.include "wozmon.asm"
  .include "TinyBasic.s"

  .segment "RESETVEC"
  .word nmi, RESET, irq
