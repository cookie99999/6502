  .setcpu "65816"
  .segment "CODE"
  .include "bios.inc"
  .include "hardware.inc"
  .include "vga.inc"
  .include "cf.inc"
  .include "fat16.inc"
  .A8
  .I8

  prbyte = $e527
  puts = $e4de
  putchar = $e4da
  sec_buf = $b000
  fat_buf = $c000
  vol_start = $a000
  fat_start = $a004
  root_start = $a008
  data_start = $a00c
  sec_per_cluster = $a010

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

fat_read_params: ; vbr should be in secbuf
  .A8
  IND_16
  ; fat start sector
  ldx #FAT_BPB_RES_SECTORS
  lda sec_buf, x
  clc
  adc vol_start
  sta fat_start
  lda sec_buf+1, x
  adc vol_start+1
  sta fat_start+1
  lda #0 ; res sectors is only 2 bytes
  adc vol_start+2
  sta fat_start+2
  lda #0
  adc vol_start+3
  sta fat_start+3
  IND_8

  ; root start sector
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

  ; data region start sector
  lda sec_buf + FAT_BPB_NUM_ROOT_ENTRIES
  sta workw
  lda #$0020 ; bytes per entry
  sta workw2
  jsr mul_16
  ; assume bytes per sector is 512 to avoid a divide, fix later
  .repeat 9
  lsr workw2
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
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
  
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
  ;jsr pr_current_dir

  LD_PTR str_find_test
  jsr fat_find_relative
  ACC_16
  jsr fat_load_cluster
  ACC_8
  LD_PTR str_test2
  jsr fat_find_relative
  bcc :+
  LD_PTR str_find_fail
  jsr puts
  rts
:	
  sta current_dir
  xba
  sta current_dir+1
  ;jsr pr_current_dir

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
str_crazy_error:
  .byte "somehow .. doesn't exist", CR, LF, 0
str_dot_dot_filename:
  .byte "..         "
str_find_test:
  .byte "TURD       "
str_find_fail:
  .byte "couldn't find", CR, LF, 0
str_test2:
  .byte "WEEWEE  TXT"

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

fat_find_relative: ; returns cluster in c if found, carry set and c invalid otherwise
  ; current directory entry assumed to be in sec_buf
  ; pointer to 11 byte filename in workw
  ; TODO: handle directory bigger than 512B, dedicated buffer for current dir data,
  ; search subdirectories, etc etc etc
  .A8
  .I8
  phx
  phy
  IND_16
  ldx #$0000
  ldy #$0000
@outer:
  stx workw2
@loop:
  lda sec_buf+FAT_DIR_NAME, x
  beq @bad_quit ; no more entries, no match
  cmp (workw), y
  bne @mismatch
  inx
  iny
  cpy #$000b ; 11 characters
  bne @loop
  ; all characters matched, get cluster and quit
  ldx workw2
  lda sec_buf+FAT_DIR_CLUSTER+1, x
  xba
  lda sec_buf+FAT_DIR_CLUSTER, x
  clc
  bra @quit
@mismatch: ; move to next entry and try again
  ldx workw2
  ACC_16
  txa
  clc
  adc #$0020
  tax
  ACC_8
  bra @outer
@bad_quit:
  sec
@quit:
  IND_8
  ply
  plx
  rts
  
fat_get_parent:	; directory cluster num in c, returns parent cluster in c
  .A8
  .I8
  phx
  phy
  xba
  jsr prbyte
  xba
  jsr prbyte
  ACC_16
  jsr fat_load_cluster
  ACC_8
  LD_PTR str_dot_dot_filename
  jsr fat_find_relative
  bcc :+
  LD_PTR str_crazy_error
  jsr puts
  sec
  :
  ply
  plx
  rts ; parent's cluster should be in c now

fat_load_cluster: ; cluster in c, first sector loaded to secbuf
  .A16
  .I8
  jsr cluster_to_sector
  ACC_8
  lda #$01
  pha
  pea sec_buf
  lda workw2h
  pha
  lda workw2l
  pha
  lda workwh
  pha
  lda workwl
  pha
  jsr cf_read_sector
  ACC_16
  rts
  
pr_current_dir:
  .A8
  .I8
  lda #'/'
  jsr putchar
  ACC_16
  lda current_dir
  beq @quit
  jsr fat_load_cluster
  ldx #$00
@parent_loop: ; follow .. entries until we get back to root
  pha ; push them so we can then traverse back down, printing names
  inx ; x = num levels
  ACC_8
  jsr fat_get_parent
  ACC_16
  beq @end_loop ; parent of zero means it's just under / which we already printed
  bra @parent_loop
@end_loop: ; stack should now contain each cluster all the way down to current dir
  ; load root dir to start finding and printing names
  ACC_8
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
  ACC_16
  txy ; y = total number of cluster numbers pushed
  IND_16
@name_loop:
  ldx #$0000
  pla
  pha ; need it in a but still saved
@find_entry: ; get to the entry matching the cluster
  cmp sec_buf+FAT_DIR_CLUSTER, x
  ; since old entries can stick around after deletion
  ; i should probably check it's not deleted before
  ; assuming it's the right one
  beq @found
  txa
  clc
  adc #$0020
  tax
  bra @find_entry
@found: ; now print the name
  ACC_8
  phy
  ldy #$0000
@print_loop:
  lda sec_buf+FAT_DIR_NAME, x
  cmp #' '
  beq @done_print
  jsr putchar
  inx
  iny
  cpy #11
  bne @print_loop
@done_print:
  lda #'/'
  jsr putchar
  ACC_16
  ply
  dey
  beq @quit ; all levels popped and printed, done
  pla
  jsr fat_load_cluster
  bra @name_loop
@quit:
  ACC_8
  IND_8
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
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
  xba
  phx
  IND_8
  jsr pr_fat_date
  IND_16
  plx
  
  lda #TAB
  jsr putchar
  lda #TAB
  jsr putchar
  lda sec_buf + FAT_DIR_SIZE+3, x
  sta workqh+1
  lda sec_buf + FAT_DIR_SIZE+2, x
  sta workqh
  lda sec_buf + FAT_DIR_SIZE+1, x
  sta workql+1
  lda sec_buf + FAT_DIR_SIZE, x
  sta workql
  phx
  IND_8
  jsr pr_fat_filesize
  IND_16
  plx
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
  IND_8
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

  ; credit Andrew Jacobs (http://6502.org/source/integers/hex2dec-more.htm)
bin_to_bcd_16: ; input in workw
  .A8
  .I8
  phx
  sed
  stz workw2l
  stz workw2h
  ldx #16
@loop:
  asl workwl
  rol workwh
  lda workw2l
  adc workw2l
  sta workw2l
  lda workw2h
  adc workw2h
  sta workw2h
  dex
  bne @loop

  lda workw2h
  xba
  lda workw2l
  plx
  cld
  rts

bin_to_bcd_32: ; input in workq, output in workb2+workq2
  .A8
  .I8
  phx
  sed
  stz workq2l
  stz workq2l+1
  stz workq2h
  stz workq2h+1
  stz workb2
  ldx #32
@loop:
  asl workql
  rol workql+1
  rol workqh
  rol workqh+1
  lda workq2l
  adc workq2l
  sta workq2l
  lda workq2l+1
  adc workq2l+1
  sta workq2l+1
  lda workq2h
  adc workq2h
  sta workq2h
  lda workq2h+1
  adc workq2h+1
  sta workq2h+1
  lda workb2
  adc workb2
  sta workb2
  dex
  bne @loop

  plx
  cld
  rts
  
  ; credit Lee Davison (http://6502.org/users/mycorner/6502/shorts/bin2bcd.html)
b2b_table:	
  .byte $63, $31, $15, $07, $03, $01, $00
  
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

pr_fat_date: ; date in c, 15..9 y-1980, 8..5 m 4..0 d
  pha
  xba
  pha
  lsr
  ACC_16
  and #$00ff
  clc
  adc #1980
  sta workw
  ACC_8
  jsr bin_to_bcd_16
  xba
  jsr pr_bcd
  xba
  jsr pr_bcd
  lda #'-'
  jsr putchar

  pla
  and #$01
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
  lda #'-'
  jsr putchar

  pla
  pla
  and #%00011111
  jsr bin_to_bcd
  jsr pr_bcd
  rts

pr_fat_filesize: ; size in workq
  ACC_16
  lda #$ffff
  sta workq2l
  lda #$000f
  sta workq2h ; over 1mb?
  lda workqh
  cmp workq2h
  bne @donemb
  lda workql
  cmp workq2l
  bne @donemb
  bra @skipmb ; == f,ffff
@donemb:
  bcc @skipmb
  brl @pr_mb
@skipmb:
  lda #$03ff
  sta workq2l
  stz workq2h ; over 1kb?
  lda workqh
  cmp workq2h
  bne @donekb
  lda workql
  cmp workq2l
  beq @pr_b ; == 3ff
@donekb:
  bcs @pr_kb
  
@pr_b:
  ACC_8
  jsr bin_to_bcd_32
  lda workb2
  jsr pr_bcd
  lda workq2h+1
  jsr pr_bcd
  lda workq2h
  jsr pr_bcd
  lda workq2l+1
  jsr pr_bcd
  lda workq2
  jsr pr_bcd
  lda #' '
  jsr putchar
  lda #'B'
  jsr putchar
  brl @quit

@pr_kb:
  ACC_16
  .repeat 10
  lsr workqh
  ror workql
  .endrep
  ACC_8
  jsr bin_to_bcd_32
  lda workb2
  jsr pr_bcd
  lda workq2h+1
  jsr pr_bcd
  lda workq2h
  jsr pr_bcd
  lda workq2l+1
  jsr pr_bcd
  lda workq2
  jsr pr_bcd
  lda #' '
  jsr putchar
  lda #'K'
  jsr putchar
  lda #'i'
  jsr putchar
  lda #'B'
  jsr putchar
  brl @quit

@pr_mb:
  ACC_16
  .repeat 20
  lsr workqh
  ror workql
  .endrep
  ACC_8
  jsr bin_to_bcd_32
  lda workb2
  jsr pr_bcd
  lda workq2h+1
  jsr pr_bcd
  lda workq2h
  jsr pr_bcd
  lda workq2l+1
  jsr pr_bcd
  lda workq2
  jsr pr_bcd
  lda #' '
  jsr putchar
  lda #'M'
  jsr putchar
  lda #'i'
  jsr putchar
  lda #'B'
  jsr putchar

@quit:
  rts
