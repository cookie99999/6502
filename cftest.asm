  .setcpu "65816"
  .segment "CODE"
  .include "bios.inc"
  .include "hardware.inc"
  .include "vga.inc"
  .include "cf.inc"
  .include "fat16.inc"
  .A8
  .I8

  prbyte = $ed91
  puts = $ed48
  putchar = $ed44
  cf_busy_wait = $e29d
  cf_drq_wait = $e2a5
  cf_read_sector = $e2ad
  fat_load_file = $e47c
  fat_read_params = $e327
  pr_current_dir = $e52e
  print_dir = $e5d7
  file_buf = $5000

  UTC_OFFS = <-5

  jmp start

start:	
  jsr cf_busy_wait
  lda #$e0
  sta CF_HEAD ; set lba mode
  lda #CF_INIT_PARAMS
  sta CF_CMD
  jsr cf_busy_wait
  lda CF_HEAD
  and #$40 ; lba bit set?
  bne :+
  jmp @lba_err
:
  lda #$01
  sta CF_FEATURE ; set 8 bit mode
  lda #CF_SET_FEATURE
  sta CF_CMD
  jsr cf_busy_wait
  lda CF_ERR
  and #$04 ; abrt/invalid cmd
  beq :+
  jmp @8bit_err
:

  stz CF_SEC_COUNT
  stz CF_LBA_7_0
  stz CF_LBA_15_8
  stz CF_LBA_23_16
  lda #CF_IDENTIFY
  sta CF_CMD
  jsr cf_drq_wait

  IND_16
  ldx #0
@read_loop:
  lda CF_DATA
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
  CRLF
  
  lda #1 ; 1 sector
  pha
  pea sec_buf ; dest buffer
  lda #0
  pha
  pha
  pha
  pha ; lba $00000000
  jsr cf_read_sector

  ; now we have mbr, find first partition and load its boot sector
  lda #1
  pha
  pea sec_buf
  IND_16
  ldx #FAT_MBR_P1 + FAT_PART_START_LBA
  lda sec_buf+3, x
  sta vol_start+3
  pha
  lda sec_buf+2, x
  sta vol_start+2
  pha
  lda sec_buf+1, x
  sta vol_start+1
  pha
  lda sec_buf, x
  sta vol_start
  pha
  IND_8
  jsr cf_read_sector

  jsr fat_read_params ; calculate region starts etc
  
  ; boot sector loaded, get to the fat
  LD_PTR str_vol_label
  jsr puts
  IND_16
  ldx #FAT_BPB_VOL_LABEL
  ldy #$0000
@loop:
  lda sec_buf, x
  jsr putchar
  inx
  iny
  cpy #11
  bne @loop
  IND_8
  CRLF
  
  lda #1
  pha
  pea fat_buf
  lda fat_start+3
  pha
  lda fat_start+2
  pha
  lda fat_start+1
  pha
  lda fat_start
  pha
  jsr cf_read_sector

  ; load root directory
  lda #1
  pha
  pea sec_buf
  lda root_start+3
  pha
  lda root_start+2
  pha
  lda root_start+1
  pha
  lda root_start
  pha
  jsr cf_read_sector

  jsr print_dir
  stz current_dir
  stz current_dir+1
  jsr pr_current_dir

  LD_PTR str_find_test
  lda #$50
  xba
  lda #$00
  jsr fat_load_file

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
str_vol_label:
  .byte "1st Volume label:", CR, LF, 0
str_find_test:
  .byte "MEDIUM  ASM"
str_err_lba:
  .byte "couldn't set lba mode", CR, LF, 0
str_err_8bit:
  .byte "couldn't set 8 bit mode", CR, LF, 0

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
