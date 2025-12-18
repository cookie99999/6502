  .setcpu "65816"
  .segment "CODE"
  .include "bios.inc"

  jsr mandel
  rts
  
mul_8:	; factors in workb and workb2, low result in workb high result in workb2
  .A8
  .I8
  ldx #$08
  lda #$00
  lsr workb
@loop:
  bcc @no_add
  clc
  adc workb2
@no_add:
  ror
  ror workb
  dex
  bne @loop
  sta workb2
  rts

mul_16:	; factors in workw and workw2, low result in workw high result in workw2
  .A16
  .I8
  phx
  ldx #16
  lda #$0000
  lsr workw
@loop:
  bcc @no_add
  clc
  adc workw2
@no_add:
  ror
  ror workw
  dex
  bne @loop
  sta workw2
  plx
  rts

mul_16_fix: ; takes signed numbers
  .A16
  .I8
  phx
  phy
  ldy workwh
  cpy #$80
  bcc @skipcmp ; multiply absolute values if negative, fix sign later
  lda workw
  eor #$ffff
  sta workw
@skipcmp:
  ldx workw2h
  cpx #$80
  bcc @skipcmp2
  lda workw2
  eor #$ffff
  sta workw2
@skipcmp2:
  jsr mul_16
  .repeat 12 ; divide by 2^12 to get back to 4.12 precision
  lda workw2
  cmp #$8000 ; for sign extend
  ror workw2
  ror workw
  .endrep
  cpx #$80
  bcs @oneneg
  cpy #$80
  bcs @oneneg_skip
  ply
  plx
  rts ; pos x pos = pos
@oneneg:
  cpy #$80
  bcs @twoneg
@oneneg_skip:
  lda workw
  eor #$ffff
  sta workw ; neg x pos = neg
  ply
  plx
  rts
@twoneg:
  ply
  plx
  rts ; neg x neg = pos

  x0 = $e000
  x1 = $1000
  y0 = $f000
  y1 = $1000
  xstep = $0013
  ystep = $0011

  cx = $30
  cy = $32
  xv = $34
  yv = $36
  x2 = $38
  y2 = $3a
  xy = $3c
  
mandel:
  ACC_16
  lda #y0
  sta cy
  IND_16
  ldx #480
yloop:
  lda #x0
  sta cx
  ldy #640
xloop:
  lda cx
  sta xv
  lda cy
  sta yv
  phx
  phy
  IND_8
  ldx #0
nloop:
  lda yv
  sta workw
  sta workw2
  jsr mul_16_fix
  lda workw
  sta y2
  lda xv
  sta workw
  sta workw2
  jsr mul_16_fix
  lda workw
  sta x2
  clc
  adc y2
  cmp #$4000
  bcs @nend
  lda xv
  sta workw
  lda yv
  sta workw2
  jsr mul_16_fix
  lda workw
  sta xy
  clc
  adc xy
  adc cy
  sta yv
  lda x2
  sec
  sbc y2
  adc cx
  sta xv
  inx
  cpx #20
  bne nloop
@nend:
  ACC_8
  lda chartab, x
  sta $44
  ACC_16
  lda cx
  clc
  adc #xstep
  sta cx
  IND_16
  ply
  plx
  ACC_8
  stx $42
  ;stz $43
  sty $40
  ;stz $41
  jsr put_pixel
  ACC_16
  dey
  bne xloop
  lda cy
  clc
  adc #ystep
  sta cy
  dex
  beq :+
  brl yloop
:	
  ACC_8
  IND_8
  rts
  
chartab:
  .byte 7, 1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 0

put_pixel: ; at $40: xlo xhi ylo yhi color
  lda #0
  sta $df30
  lda $40
  sta $df30
  lda $41
  sta $df30
  lda $42
  sta $df30
  lda $43
  sta $df30
  lda $44
  sta $df30
  rts

pico_write:
  sta $df30
  nop
  nop
  rts
  
crlf:
  ACC_8
  ;lda #CR
  ;SVC SVC_PUTCHAR
  lda #LF
  sta $df30
  ACC_16
  rts
