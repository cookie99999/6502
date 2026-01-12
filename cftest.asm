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

  jmp start
  
cf_read_reg:
  @reg = 3 ; reg number on stack
  lda @reg, s
  asl
  asl
  sta @reg, s ; shift it for masking in
  stz VIA2_DDRA
  lda #%01000011
  sta VIA2_PB
  ora @reg, s
  sta VIA2_PB
  and #%11111101
  sta VIA2_PB
  lda VIA2_PA
  pha
  lda VIA2_PB
  ora #%01100011
  sta VIA2_PB
  ; s
  ; 1- a backup
  ; 2- rts lo
  ; 3- rts hi
  ; 4- reg
  ; stack cleanup
  ; could've used mvp but for a small stack frame it's not much faster
  lda 3, s
  sta 4, s
  lda 2, s
  sta 3, s
  lda 1, s
  sta 2, s
  pla ; s++
  pla
  rts

cf_write_reg:	; reg, byte on stack (rtl)
  @reg = 3
  @byte = 4
  lda #%01000011
  sta VIA2_PB
  lda #$ff
  sta VIA2_DDRA
  lda @byte, s
  sta VIA2_PA
  lda @reg, s
  asl
  asl
  sta @reg, s
  lda VIA2_PB
  ora @reg, s
  sta VIA2_PB
  and #%11111110
  sta VIA2_PB
  ora #%00000011
  sta VIA2_PB
  ; stack cleanup
  lda 2, s
  sta 4, s
  lda 1, s
  sta 3, s
  pla
  pla
  rts

cf_busy_wait:
  lda #CF_STAT
  pha
  jsr cf_read_reg
  and #CF_STAT_BSY
  bne cf_busy_wait
  rts

cf_drq_wait:
  lda #CF_STAT
  pha
  jsr cf_read_reg
  and #CF_STAT_DRQ
  beq cf_drq_wait
  rts

cf_read_sector:	; 32 bit sector number, buffer ptr, count
  @count = 9
  @buf = 7
  @lba = 3 ; lo-lo, lo-hi, hi-lo, hi-hi
  ; todo probably want to save all the registers i clobber
  lda @lba+3, s
  ora #$e0 ; mask in other part of head reg
  pha
  lda #CF_LBA_27_24
  pha
  jsr cf_write_reg
  lda @lba+2, s
  pha
  lda #CF_LBA_23_16
  pha
  jsr cf_write_reg
  lda @lba+1, s
  pha
  lda #CF_LBA_15_8
  pha
  jsr cf_write_reg
  lda @lba, s
  pha
  lda #CF_LBA_7_0
  pha
  jsr cf_write_reg
  lda @count, s
  pha
  lda #CF_SEC_COUNT
  pha
  jsr cf_write_reg
  pea CF_READ_SEC << 8 | CF_CMD
  jsr cf_write_reg
  jsr cf_drq_wait

  IND_16
  ldy #$0000
@read_loop:
  lda #CF_DATA
  pha
  jsr cf_read_reg
  sta (@buf, s), y
  iny
  cpy #512
  bne @read_loop
  ; stack cleanup
  ACC_16
  tsc
  clc
  adc #2 ; source end: 2nd byte of return address
  tax
  adc #7
  tay ; dest: top of stack frame
  lda #1 ; copying 2 bytes (just rts address)
  mvp #0, #0 ; mvp stops when c is below zero, so c must be size-1
  tya
  tcs
  ACC_8
  IND_8
  rts

start:	
  lda #$01 ; ca1 rising edge
  sta VIA2_PCR
  lda #$7f
  sta VIA2_IER
  stz VIA2_ACR
  lda #$ff
  sta VIA2_DDRB
  jsr cf_busy_wait
  pea $e0 << 8 | CF_HEAD ; faster way to push 2 byte constant
  jsr cf_write_reg ; set lba mode
  pea CF_INIT_PARAMS << 8 | CF_CMD
  jsr cf_write_reg
  jsr cf_busy_wait
  lda #CF_HEAD
  pha
  jsr cf_read_reg
  and #$40 ; lba bit set?
  bne :+
  jmp @lba_err
:
  pea $01 << 8 | CF_FEATURE
  jsr cf_write_reg ; set 8 bit mode
  pea CF_SET_FEATURE << 8 | CF_CMD
  jsr cf_write_reg
  jsr cf_busy_wait
  lda #CF_ERR
  pha
  jsr cf_read_reg
  and #$04 ; abrt/invalid cmd
  beq :+
  jmp @8bit_err
:

  pea $00 << 8 | CF_SEC_COUNT
  jsr cf_write_reg
  pea $00 << 8 | CF_LBA_7_0
  jsr cf_write_reg
  pea $00 << 8 | CF_LBA_15_8
  jsr cf_write_reg
  pea $00 << 8 | CF_LBA_23_16
  jsr cf_write_reg
  pea CF_IDENTIFY << 8 | CF_CMD
  jsr cf_write_reg
  jsr cf_drq_wait

  IND_16
  ldx #0
@read_loop:
  lda #CF_DATA
  pha
  jsr cf_read_reg
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

  lda #1 ; 1 sector
  pha
  pea sec_buf ; dest buffer
  lda #0
  pha
  pha
  pha
  pha ; lba $00000000
  jsr cf_read_sector
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
