  .setcpu "65C02"
  .include "hardware.inc"
  VIA_IFR_CA1 = $02

  .segment "CODE"
  tv_idle = $2000 	; 1 byte
  work = $2001
  inbuf = $0200

  jmp main

  .include "protoforth.asm"
teststr:
  .byte "peepee poopoo", 0

main:
  jsr init
  ldx #$00
loop:
  jsr getchar
  cmp #$0d ; cr
  bne loopskip0
  jsr putline
  ldx #$00
  bra loop
loopskip0:
  sta inbuf, x
  jsr tv_output
  inx
  jmp loop

putline:
  lda #$0d
  jsr tv_output
  phx
  phy
  ldy #$00
plloop0:
  lda inbuf, y
  jsr tv_output
  iny
  sty work
  cpx work
  beq plend
  ldy work
  bra plloop0
plend:
  lda #$0d
  jsr tv_output
  ply
  plx
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

tv_output:
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

  lda #$aa
  sta $0200
  lda #$55
  sta $0201
  lda $0200
  cmp #$aa
  bne erri
  lda $0201
  cmp #$55
  bne errii
  lda #'O'
  jsr tv_output
  rts
erri:
  lda #'X'
  jsr tv_output
  rts
errii:
  lda #'Y'
  jsr tv_output
  rts

  .segment "RESETVEC"
  .word $0000, main, $0000
