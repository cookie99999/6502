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

mul_16_fix:
  .A16
  .I8
  jsr mul_16
  .repeat 12
  lsr workw
  .endrep
  lda workw2
  and #$f000
  ora workw
  sta workw
  rts

  x0 = $e000
  x1 = $1000
  y0 = $f000
  y1 = $1000
  xstep = $0099
  ystep = $0147

  cx = $30
  cy = $32
  xv = $34
  yv = $36
  x2 = $38
  y2 = $3a
  xy = $3c
mandel: ; todo check 16 bit lda byte order
  ACC_16
  lda #y0
  sta cy
  ldx #25
yloop:
  lda #x0
  sta cx
  ldy #80
xloop:
  lda cx
  sta xv
  lda cy
  sta yv
  phx
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
  adc xy
  adc cy
  sta yv
  lda x2
  sbc y2
  adc cx
  sta xv
  inx
  cpx #20
  bne nloop
@nend:
  ACC_8
  lda chartab, x
  phx
  SVC SVC_PUTCHAR
  plx
  ACC_16
  lda cx
  adc #xstep
  sta cx
  plx
  dey
  bne xloop
  lda cy
  adc #ystep
  sta cy
  phx
  jsr crlf
  plx
  dex
  bne yloop
  ACC_8
  rts
chartab:
  .byte '.', '.', '.', ',', ',', ',', ':', ':', ':', 'i', 'i', 'i', 'w', 'w', 'w', 'W', 'W', 'W', '#', '#', ' '

crlf:
  ACC_8
  lda #CR
  SVC SVC_PUTCHAR
  lda #LF
  SVC SVC_PUTCHAR
  ACC_16
  rts
