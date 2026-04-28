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
  cluster_to_sector = $e309
  fat_find_relative = $e434
  fat_load_cluster = $e3e7
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
  jsr fat_create_file

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
  .byte "NEW1    TXT"
str_err_lba:
  .byte "couldn't set lba mode", CR, LF, 0
str_err_8bit:
  .byte "couldn't set 8 bit mode", CR, LF, 0
str_err_fat_sec:
  .byte "no free cluster in first sector of FAT", CR, LF, 0
str_err_dir_sec:
  .byte "no free entry in first sector of directory", CR, LF, 0
test_buf:
  .byte "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt"

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

clock_to_fat_time: ; todo actually implement
  lda #$00
  xba
  lda #$00
  rts

cf_write_sector: ; todo any error checking at all
  .A8
  .I8
  @count = 9
  @buf = 7
  @lba = 3 ; lo-lo, lo-hi, hi-lo, hi-hi
  ; todo probably want to save all the registers i clobber
  lda @lba+3, s
  ora #$e0 ; mask in other part of head reg
  sta CF_LBA_27_24
  lda @lba+2, s
  sta CF_LBA_23_16
  lda @lba+1, s
  sta CF_LBA_15_8
  lda @lba, s
  sta CF_LBA_7_0
  lda @count, s
  tax ; x = sector count
  sta CF_SEC_COUNT
  lda #CF_WRITE_SEC
  sta CF_CMD
  jsr cf_drq_wait

  stx workwl
  stz workwh
  ACC_16
  lda #512
  sta workw2
  jsr mul_16
  ACC_8
  IND_16
  ldy #$0000
@write_loop:
  lda (@buf, s), y
  sta CF_DATA
  iny
  cpy workw
  bne @write_loop
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

fat_create_file:
  ; workw: ptr to filename
  ; return cluster in c

  ; load current dir
  ; load current fat sector (skipping for now due to laziness)
  ; find free entry
  ; find free cluster
  ; fill out entry in buffer
  ; update fat
  ; write both back to disk

  .A8
  .I8
  ACC_16
  lda current_dir
  bne :+
  ; handle root differently
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
  bra :++
:	
  jsr fat_load_cluster
:	
  ACC_8
  IND_16
  ldx #$0000
@free_search: ; first byte: 00 = end of entries, e5 = free
  lda sec_buf, x
  beq @end_dir
  cmp #$e5
  beq @found
  ; continue
  ACC_16
  txa
  clc
  adc #$0020 ; next entry
  tax
  cmp #$0200 ; end of this sector?
  bne @free_search
@next_dir_sec: ; todo: actually do this
  ACC_8
  IND_8
  LD_PTR str_err_dir_sec
  jsr puts
  sec
  rts
  .A16
  .I16
@end_dir:
  phx
  ACC_16
  txa
  clc
  adc #$0020 ; todo check for end of sector
  tax
  ACC_8
  stz sec_buf, x ; mark next entry as new end
  plx
@found:
  ; now we can fill out a new directory entry
  ; unrolled name copy because i dont feel like juggling indices
  ACC_16
  lda (workw)
  sta sec_buf, x
  inc workw
  inc workw ; 2
  lda (workw)
  sta sec_buf+2, x
  inc workw
  inc workw ; 4
  lda (workw)
  sta sec_buf+4, x
  inc workw
  inc workw ; 6
  lda (workw)
  sta sec_buf+6, x
  inc workw
  inc workw ; 8
  lda (workw)
  sta sec_buf+8, x
  inc workw
  inc workw ; 10
  ACC_8
  lda (workw)
  sta sec_buf+10, x
  ; todo let caller decide (esp for directory)
  stz sec_buf+FAT_DIR_ATTR, x
  stz sec_buf+FAT_DIR_NT_CASE, x
  lda jifs
  sta sec_buf+FAT_DIR_CTIME_CSECS, x
  ;jsr clock_to_fat_time
  ACC_16
  sta sec_buf+FAT_DIR_CTIME, x
  sta sec_buf+FAT_DIR_MTIME, x
  ; todo have an actual date
  stz sec_buf+FAT_DIR_CTIME_DATE, x
  stz sec_buf+FAT_DIR_ATIME_DATE, x
  stz sec_buf+FAT_DIR_MTIME_DATE, x
  stz sec_buf+FAT_DIR_CLUSTER_HIGH, x ; 0 on fat12/16
  stz sec_buf+FAT_DIR_SIZE, x
  stz sec_buf+FAT_DIR_SIZE+2, x

  ; now we must find a free cluster for this file
  phx ; save directory offset
  ldx #$0002 ; skip reserved entries
  ; don't forget, still on 16-bit acc+mem
@fat_loop:
  lda fat_buf, x
  beq @found_fat ; 0000 = free
  inx
  inx
  cpx #$0200
  bne @fat_loop
  ; end of current fat sector, todo allow more
  ACC_8
  IND_8
  LD_PTR str_err_fat_sec
  jsr puts
  sec
  rts
  .A16
  .I16
@found_fat:
  lda #$ffff
  sta fat_buf, x ; mark eof
  txa
  sec
  sbc #2 ; skip reserved entries
  plx
  pha ; save for return value
  sta sec_buf+FAT_DIR_CLUSTER, x

  ; finally have a complete directory entry, write back both buffers
  ACC_8
  IND_8
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
  jsr cf_write_sector

  lda #1
  pha
  pea sec_buf
  ACC_16
  lda current_dir
  bne :+
  ; handle root differently
  lda root_start
  sta workw
  lda root_start+2
  sta workw2
  bra :++
:
  jsr cluster_to_sector ; todo important multi sector dirs
:
  ACC_8
  lda workw2h
  pha
  lda workw2l
  pha
  lda workwh
  pha
  lda workwl
  pha
  jsr cf_write_sector

  ACC_16
  pla
  ACC_8
  clc
  rts
  
