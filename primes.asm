  .setcpu "65816"
  .segment "CODE"
  .include "hardware.inc"
  .include "bios.inc"

  t = $0030
  tm = $0032
  f = $0034
  
start:
  lda #2
  sta t
@loop: ; for 2..200
  lsr a ; upper limit = y/2
  sta tm
  lda #0
  sta f
  ldy #2
@inner:	; for 2..y/2
  lda t
  sta $40
  stz $41
  sty $42
  stz $43
  ldx #$40
  phy
  jsr div_16
  ACC_8
  ply
  lda 6,x
  cmp #0
  bne @skip
  pha
  lda #1
  sta f
  pla
@skip:
  iny
  cpy tm
  bcc @inner
  lda f
  bne @skip2
  pha
  lda #'.'
  SVC SVC_PUTCHAR
  pla
@skip2:
  inc t
  lda t
  cmp #200
  bne @loop
  rts

div_16: ; x = ptr to 8 bytes of zp: num den quo rem
  num = 0
  den = 2
  quo = 4
  rem = 6
  ACC_16
  stz quo, x
  stz rem, x
  ldy #15
@loop:
  asl num, x ; c = num[i]
  rol rem, x ; rem[0] = c
  lda rem, x
  cmp den, x
  bcc @skip ; if rem >= den
  sbc den, x
  sta rem, x ; rem -= den
  phy
  lda #1
  cpy #0
  beq @shiftskip
@shiftloop:
  asl a
  dey
  bne @shiftloop
@shiftskip:
  ply
  ora quo, x ; quo[i] = 1
  sta quo, x
@skip:
  dey
  bpl @loop ; n=1 after underflow
  rts
