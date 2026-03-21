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
  ldx #$00
  ldy #$00
  stz count
  ACC_16
  lda front
  pha
  lda back
  pha
  ACC_8
tick_loop:
  ; count live neighbors
  phy
  phx
checkleft:	
  txa
  sec
  sbc #1 ; left
  bpl @skip
  lda #BLACK
  bra :+
@skip:
  tax
  jsr get_cell_at
:	
  cmp #WHITE
  bne checkright
  inc count
checkright:	
  txa
  cmp #$7e
  bcc @skip
  lda #BLACK
  bra :+
@skip:
  clc
  adc #2 ; right
  tax
  jsr get_cell_at
:	
  cmp #WHITE
  bne checktop
  inc count
checktop:
  plx ; restore original x
  phx ; but save again for later
  tya
  sec
  sbc #1 ; top
  bpl @skip
  lda #BLACK
  bra :+
@skip:
  tay
  jsr get_cell_at
:	
  cmp #WHITE
  bne checkbottom
  inc count
checkbottom:	
  tya
  cmp #$5e
  bcc @skip
  lda #BLACK
  bra :+
@skip:
  clc
  adc #2 ; bottom
  tay
  jsr get_cell_at
:	
  cmp #WHITE
  bne checkbotleft
  inc count
checkbotleft:	
  txa
  sec
  sbc #1 ; bottom left
  bpl @skip
  lda #BLACK
  bra :+
@skip:
  tax
  jsr get_cell_at
:	
  cmp #WHITE
  bne checkbotright
  inc count
checkbotright:
  txa
  cmp #$7e
  bcc @skip
  lda #BLACK
  bra :+
@skip:
  clc
  adc #2 ; bottom right
  and #$7f
  tax
  jsr get_cell_at
:	
  cmp #WHITE
  bne checktopright
  inc count
checktopright:
  tya
  sec
  sbc #2 ; top right
  bpl @skip
  lda #BLACK
  bra :+
@skip:
  tay
  jsr get_cell_at
:	
  cmp #WHITE
  bne checktopleft
  inc count
checktopleft:
  txa
  sec
  sbc #2 ; top left
  bpl @skip
  lda #BLACK
  bra :+
@skip:
  tax
  jsr get_cell_at
:	
  cmp #WHITE
  bne dorules
  inc count
dorules:
  ; neighbors counted, apply rules
  plx
  ply
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
  ACC_16
  inc front
  inc back
  ACC_8
  stz count
  inx
  cpx #$80
  beq :+
  brl tick_loop
:	
  ldx #$00
  iny
  cpy #$60
  beq :+
  brl tick_loop
:
  ACC_16
  pla
  sta back
  pla
  sta front
  ACC_8  
  rts

get_cell_at: ; return byte in framebuffer at x,y
  index = workw
  phx
  phy
  tya
  sta workb
  lda #$80
  sta workb2
  jsr mul_8
  ACC_16
  lda workb
  sta index
  ply
  plx
  phx
  txa
  and #$00ff
  clc
  adc index
  sta index
  lda index
  clc
  adc $08, S ; saved copy of front base, kind of a pain to get it this way
  sta index
  ACC_8
  lda (index)
  plx
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
