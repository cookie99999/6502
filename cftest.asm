  .setcpu "65816"
  .segment "CODE"
  .include "bios.inc"
  .include "hardware.inc"
  .include "vga.inc"
  .include "cf.inc"
  .A8
  .I8

  prbyte = $e519
  puts = $e4d0
  putchar = $e4cc
  sec_buf = $b000
  
.macro CF_READ_REG reg
  stz VIA2_DDRA
  lda #%01000011
  sta VIA2_PB
  ora #(reg << 2)
  sta VIA2_PB
  and #%11111101
  sta VIA2_PB
  lda VIA2_PA
  pha
  lda VIA2_PB
  ora #%01100011
  sta VIA2_PB
  pla
.endmacro

.macro CF_WRITE_REG reg, byte
  lda #%01000011
  sta VIA2_PB
  lda #$ff
  sta VIA2_DDRA
  lda #byte
  sta VIA2_PA
  lda VIA2_PB
  ora #(reg << 2)
  sta VIA2_PB
  and #%11111110
  sta VIA2_PB
  ora #%00000011
  sta VIA2_PB
.endmacro
  
  lda #$01 ; ca1 rising edge
  sta VIA2_PCR
  lda #$7f
  sta VIA2_IER
  stz VIA2_ACR
  lda #$ff
  sta VIA2_DDRB
@wait_rdy:
  CF_READ_REG CF_STAT
  and #CF_STAT_RDY
  beq @wait_rdy
  CF_WRITE_REG CF_HEAD, $e0 ; set lba mode
  CF_WRITE_REG CF_CMD, CF_INIT_PARAMS
@status_wait:
  CF_READ_REG CF_STAT
  and #CF_STAT_BSY
  bne @status_wait
  CF_READ_REG CF_HEAD
  and #$40 ; lba bit set?
  bne :+
  jmp @lba_err
:	
  CF_WRITE_REG CF_FEATURE, $01 ; set 8 bit mode
  CF_WRITE_REG CF_CMD, CF_SET_FEATURE
@status_wait2:
  CF_READ_REG CF_STAT
  and #CF_STAT_BSY
  bne @status_wait2
  CF_READ_REG CF_ERR
  and #$04 ; abrt/invalid cmd
  beq :+
  jmp @8bit_err
:
  
  CF_WRITE_REG CF_SEC_COUNT, $00
  CF_WRITE_REG CF_LBA_7_0, $00
  CF_WRITE_REG CF_LBA_15_8, $00
  CF_WRITE_REG CF_LBA_23_16, $00
  CF_WRITE_REG CF_CMD, CF_IDENTIFY
@id_wait:
  CF_READ_REG CF_STAT
  and #CF_STAT_DRQ
  beq @id_wait

  IND_16
  ldx #0
@read_loop:
  CF_READ_REG CF_DATA
  sta sec_buf, x
  inx
  cpx #512
  bne @read_loop
  IND_8

  LD_PTR str_model
  jsr puts
  ldx #27 * 2
@model_loop:
  lda sec_buf, x
  xba
  inx
  lda sec_buf, x
  jsr putchar
  xba
  jsr putchar
  inx
  cpx #47 * 2
  bne @model_loop
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
  rts
@lba_err:
  LD_PTR str_err_lba
  jsr puts
  rts
@8bit_err:
  LD_PTR str_err_8bit
  jsr puts
  rts

str_model:
  .byte "Model number: ", CR, LF, 0
str_err_lba:
  .byte "couldn't set lba mode", CR, LF, 0
str_err_8bit:
  .byte "couldn't set 8 bit mode", CR, LF, 0
