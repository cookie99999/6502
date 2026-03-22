  .setcpu "65816"
  .include "bios.inc"
  .include "hardware.inc"
  .include "vga.inc"

  .A8
  .I8

  buf_a = $5000
  buf_b = $8000
  front = $36
  back = $38
  mul_8 = $e964
  rand_8 = $e291
  getchar = $edd5
  prbyte = $ed91
  putchar = $ed44

  ACC_16
  lda #buf_a
  sta front
  lda #buf_b
  sta back
dont_zero:	
  lda jifs
  beq dont_zero ; zero kills lfsrs
  sta prng_state ; seed with time
  ACC_8
  lda #GO_BITMAP
  sta VGA_DATA
  lda #GO_LORES
  sta VGA_DATA

  jsr seed_field
loop:
  jsr redraw
  jsr tick
  ACC_16
  lda front
  pha
  lda back
  sta front
  pla
  sta back
  ACC_8
  jsr getchar
  bcc loop
quit:
  lda #GO_TEXT
  sta VGA_DATA
  rts

redraw:
  ACC_16
  lda front
  pha
  ACC_8
  ldx #$00
  ldy #$00
@loop:
  lda (front)
  ora #DRAW_PIX_SEQ
  sta VGA_DATA
  ACC_16
  inc front
  ACC_8
  inx
  cpx #$80
  bne @loop
  ldx #$00
  iny
  cpy #$60
  bne @loop
  ACC_16
  pla
  sta front
  ACC_8
  rts

tick:
  count = $80
  index = $82
  stz count
  ACC_16
  lda front
  pha
  lda back
  pha
  stz index
  ACC_8
tick_loop:
  ; count live neighbors
checkleft:
  ACC_16
  lda index
  sec
  sbc #$0001
  sta workw
  eor index
  cmp #$0010
  bcc @dontwrap ; bit 7 didn't flip
  lda workw
  clc
  adc #$0080
  sta workw
@dontwrap:
  lda workw
  sta workw2 ; save for tl+bl
  clc
  adc 3, S ; front buffer base
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checktopleft
  inc count
checktopleft:
  ACC_16
  lda workw2
  sec
  sbc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  lda workw
  clc
  adc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checkbotleft
  inc count
checkbotleft:
  ACC_16
  lda workw2
  clc
  adc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  sec
  sbc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checkright
  inc count
checkright:
  ACC_16
  lda index
  clc
  adc #$0001
  sta workw
  eor index
  cmp #$0010
  bcc @dontwrap ; bit 7 didn't flip
  lda workw
  sec
  sbc #$0080
  sta workw
@dontwrap:
  lda workw
  sta workw2 ; save for tr+br
  clc
  adc 3, S ; front buffer base
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checktopright
  inc count
checktopright:
  ACC_16
  lda workw2
  sec
  sbc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  lda workw
  clc
  adc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checkbotright
  inc count
checkbotright:
  ACC_16
  lda workw2
  clc
  adc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  sec
  sbc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checktop
  inc count
checktop:
  ACC_16
  lda index
  sec
  sbc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  lda workw
  clc
  adc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne checkbottom
  inc count
checkbottom:	
  ACC_16
  lda index
  clc
  adc #$0080
  sta workw
  cmp #$3000
  bcc @dontwrap
  sec
  sbc #$3000
@dontwrap:
  clc
  adc 3, S
  sta workw
  ACC_8
  lda (workw)
  cmp #WHITE
  bne dorules
  inc count
dorules:
  ; neighbors counted, apply rules
  lda (front)
  cmp #BLACK
  beq @dead
  lda count
  cmp #2
  bcs :+
  lda #BLACK ; dead from underpopulation
  sta (back)
  bra end_cell
:
  cmp #4
  bcc @skip
  lda #BLACK ; dead from overpopulation
  sta (back)
  bra end_cell
@dead:
  lda count
  cmp #3
  bne @skip
  lda #WHITE ; new cell birthed
  sta (back)
  bra end_cell
@skip:
  ; live cells with 2 or 3 neighbors remain untouched
  lda (front)
  sta (back)
end_cell:
  stz count
  ACC_16
  inc front
  inc back
  inc index
  lda index
  cmp #$3000
  bcs :+
  brl tick_loop
:	
  pla
  sta back
  pla
  sta front
  ACC_8  
  rts

seed_field:
  ACC_16
  lda front
  pha
  lda back
  pha
  ACC_8
  ldx #$00
  ldy #$00
@loop:
  jsr rand_8
  cmp #$4f
  bcc :+
  lda #BLACK
  sta (front)
  sta (back)
  bra @skip
:
  lda #WHITE
  sta (front)
  sta (back)
@skip:
  ACC_16
  inc front
  inc back
  ACC_8
  inx
  cpx #$80
  bne @loop
  ldx #$00
  iny
  cpy #$60
  bne @loop
  ACC_16
  pla
  sta back
  pla
  sta front
  ACC_8
  rts
