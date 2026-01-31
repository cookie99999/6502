  .setcpu "65816"
  .segment "CODE"
  .include "bios.inc"
  .include "hardware.inc"
  .include "vga.inc"
  .include "cf.inc"
  .include "fat16.inc"
  .A8
  .I8

  prbyte = $e523
  puts = $e4da
  putchar = $e4d6
  sec_buf = $b000
  fat_buf = $c000
  vol_start = $a000
  fat_start = $a004
  root_start = $a008
  data_start = $a00c
  sec_per_cluster = $a00d

  UTC_OFFS = <-5

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
  jsr puts ; dies here
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
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
  
  lda #1
  pha
  pea fat_buf
  IND_16
  ldx #FAT_BPB_RES_SECTORS
  lda sec_buf, x
  clc
  adc vol_start
  sta fat_start
  lda sec_buf+1, x
  adc vol_start+1
  sta fat_start+1
  lda #0
  adc vol_start+2
  sta fat_start+2
  lda #0
  adc vol_start+3
  sta fat_start+3
  
  pha
  lda fat_start+2
  pha
  lda fat_start+1
  pha
  lda fat_start
  pha
  IND_8
  jsr cf_read_sector

  ; get root directory
  ACC_16
  lda sec_buf + FAT_BPB_NUM_FATS
  and #$00ff ; only one byte
  sta workw
  lda sec_buf + FAT_BPB_SECTORS_PER_FAT
  sta workw2
  jsr mul_16
  clc
  lda fat_start
  adc workw
  sta root_start
  lda fat_start+2
  adc workw2
  sta root_start+2 ; root_start = fat_start + (num_fats * sec_per_fat)

  ; calculate data start too
  lda sec_buf + FAT_BPB_NUM_ROOT_ENTRIES
  sta workw
  lda #$0032
  sta workw2
  jsr mul_16
  ; assume bytes per sector is 512 to avoid a divide, fix later
  .repeat 9
  clc
  ror workw2
  ror workw
  .endrep
  clc
  lda root_start
  adc workw
  sta data_start
  lda root_start+2
  adc workw2
  sta data_start+2

  ACC_8
  lda sec_buf + FAT_BPB_SECTORS_PER_CLUSTER
  sta sec_per_cluster

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
  num = workw2
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
  
cluster_to_sector: ; cluster in c, sector returned in workw+workw2
  .A16
  sec
  sbc #2
  sta workw
  lda sec_per_cluster
  and #$00ff
  sta workw2
  jsr mul_16
  clc
  lda data_start
  adc workw
  sta workw
  lda data_start+2
  adc workw2
  sta workw2
  rts

print_dir: ; assuming root dir in sec_buf for now
  ACC_8
  IND_16
  ldx #$0000
@loop:
  lda sec_buf + FAT_DIR_NAME, x
  bne :+
  brl @quit; names never have zeroes
:
  cmp #$e5 ; deleted files start with e5
  bne :+
  brl @skip
:	
  lda sec_buf + FAT_DIR_ATTR, x
  and #%00001010 ; don't display hidden or volume name entries
  beq :+
  brl @skip
:	
  lda sec_buf + FAT_DIR_ATTR, x
  and #$10 ; directory?
  beq :+
  lda #'D'
  jsr putchar
  bra :++
:
  lda #' '
  jsr putchar
:
  lda #' '
  jsr putchar
  phx
  ACC_16
  txa
  clc
  adc #FAT_DIR_NAME
  tax
  ACC_8
  ldy #0
@nameloop:
  lda sec_buf, x
  jsr putchar
  inx
  iny
  cpy #11
  bne @nameloop
  plx
  lda #TAB
  jsr putchar
  lda sec_buf + FAT_DIR_MTIME, x
  xba
  lda sec_buf + FAT_DIR_MTIME+1, x
  xba
  phx
  IND_8
  jsr pr_fat_time
  IND_16
  plx

  lda #' '
  jsr putchar
  lda sec_buf + FAT_DIR_MTIME_DATE, x
  xba
  lda sec_buf + FAT_DIR_MTIME_DATE+1, x
  jsr prbyte
  xba
  jsr prbyte
  lda #TAB
  jsr putchar
  lda #TAB
  jsr putchar
  lda sec_buf + FAT_DIR_SIZE+3, x
  jsr prbyte
  lda sec_buf + FAT_DIR_SIZE+2, x
  jsr prbyte
  lda sec_buf + FAT_DIR_SIZE+1, x
  jsr prbyte
  lda sec_buf + FAT_DIR_SIZE, x
  jsr prbyte
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
@skip:
  ACC_16
  txa
  clc
  adc #$0020
  tax
  ACC_8
  brl @loop
@quit:
  rts

pr_bcd:	; bcd byte in a, does not check validity
  .A8
  .I8
  pha
  and #$f0
  lsr
  lsr
  lsr
  lsr
  clc
  adc #$30
  jsr putchar
  pla
  and #$0f
  clc
  adc #$30
  jsr putchar
  rts

  ; credit Lee Davison (http://6502.org/users/mycorner/6502/shorts/bin2bcd.html)
b2b_table:	
  .byte $63,$31,$15,$07,$03,$01,$00
  
bin_to_bcd: ; assumes input <= 99, result in a
  .A8
  .I8
  phx
  sed
  sta workb
  lda #$00
  ldx #$07
@bit_loop:
  lsr workb
  bcc @skip_add
  adc b2b_table - 1, x
@skip_add:
  dex
  bne @bit_loop

  plx
  cld
  rts
  
pr_fat_time: ; time in c, 15..11 H 10..5 M 4..0 S/2
  .A8
  pha
  xba
  pha
  and #%11111000
  lsr
  lsr
  lsr
  ; ignoring timezone fixing for now
  ;clc
  ;adc #UTC_OFFS ; linux stores mtimes as utc; not sure about other os
  jsr bin_to_bcd
  jsr pr_bcd
  lda #':'
  jsr putchar
  
  pla
  and #%00000111
  asl
  asl
  asl
  pha
  lda 2, s
  lsr
  lsr
  lsr
  lsr
  lsr
  ora 1, s
  jsr bin_to_bcd
  jsr pr_bcd
  lda #':'
  jsr putchar
  
  pla
  pla
  and #%00011111
  asl a ; seconds stored divided by 2
  jsr bin_to_bcd
  jsr pr_bcd
  rts
