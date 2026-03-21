  .export mul_8
  .A8
  .I8

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

div_16: ; x = ptr to 8 bytes of zp: num den quo rem
  num = workw
  den = workw2
  quo = retw
  rem = retw2
  ACC_16
  stz quo, x
  stz rem, x
  ldy #15
@loop:
  asl num ; c = num[i]
  rol rem ; rem[0] = c
  lda rem
  cmp den
  bcc @skip ; if rem >= den
  sbc den
  sta rem ; rem -= den
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
  ora quo ; quo[i] = 1
  sta quo
@skip:
  dey
  bpl @loop ; n=1 after underflow
  rts
  
sqrt_table:
  .byte 0, 2, 6, 12, 20, 30, 42, 56, 72, 90, 110, 132, 156, 182, 210, 240

sqrt8:	; argument in a
  phx
  ldx #$00
@loop:
  cmp sqrt_table, x
  bcs @skip
  inx
  bra @loop
@skip:
  lda sqrt_table, x
  plx
  rts
  
  ACC = $80
  ARG = $82
  TMP = $84
sqrt16:
  phx
  ACC_16
  lda ACC
  sta ARG         ; Move ACC to ARG
  stz ACC         ; Zero ACC & TMP
  stz TMP
  ldx #8         ; Gen X bits of sqrt
  bne start       ; (always)
loop:
  asl ARG         ; Left shift TMP
  rol TMP
  asl ARG         ; by 2 bits.
  rol TMP
  lda TMP       ; Compare __ bits of TMP
  cmp ACC       ; with current sqrt.
  bne check
  lda TMP         ; Compare next byte
  cmp ACC
  bne check
start:
  lda ARG       ; Compare final byte
  and #$ff00
  cmp #$4000
check:
  bcc shift0      ; TMP > sqrt
  lda ARG       ; TMP <= sqrt, so subtract
  sbc #$4000
  sta ARG
  lda TMP
  sbc ACC
  sta TMP
shift0:
  rol ACC         ; Rotate C into sqrt
  dex             ; Done?
  bne loop        ; -No, keep looping.
  plx
  ACC_8
  rts
