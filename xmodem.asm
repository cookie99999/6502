  blknum = workb
  crc = workw2
  blkbuf = $0300

  .A8
  .I8
xmodem_recv: ; workw = addr to store received program at
  sei ; polling for characters is more reliable than the irq+buffer idk why
  lda workwh
  pha
  lda workwl
  pha
  LD_PTR str_xmodem_start
  jsr puts
  pla
  sta workwl
  pla
  sta workwh
  lda #1 ; 1 indexed
  sta blknum ; block number storage
@open_conn:
  lda #'C'
  jsr putchar
  jsr getchar_timeout
  bcs @open_conn
  cmp #SOH
  bne @open_conn
  bra @skip_soh
  
@get_block:
  jsr getchar_timeout
  bcc :+
  jsr retry_blk
  bra @get_block
:
  cmp #SOH
  beq @skip_soh
  cmp #EOT
  bne :+
  brl @done
:	
  jsr retry_blk
  bra @get_block
  
@skip_soh:
  ldy #$00
  jsr getchar_timeout ; block number
  bcc :+
  jsr retry_blk
  bra @get_block
:
  sta blkbuf, y
  iny	
  jsr getchar_timeout ; negated block number
  bcc :+
  jsr retry_blk
  bra @get_block
:
  sta blkbuf, y
  iny
  
@loop:
  jsr getchar_timeout
  bcc :+
  jsr retry_blk
  bra @get_block
:
  sta blkbuf, y
  iny
  cpy #$82 ; 128 data bytes per packet
  bne @loop

  jsr getchar_timeout ; crc lo
  bcc :+
  jsr retry_blk
  bra @get_block
:
  sta blkbuf, y
  iny

  jsr getchar_timeout ; crc hi
  bcc :+
  jsr retry_blk
  bra @get_block
:
  sta blkbuf, y

  stz crc
  stz crc + 1
@validate_blk:
  ldy #$00
  lda blkbuf, y ; block num
  cmp blknum
  bne @err
  iny
  lda blkbuf, y ; complemented block num
  eor #$ff
  cmp blknum
  bne @err
  iny
@chkloop: ; todo: try acc_16 before loop and only switch back after
  lda blkbuf, y
  jsr update_crc
  iny
  cpy #$82
  bne @chkloop
  ACC_16
  lda crc
  xba ; xmodem sends the crc big endian
  sta crc
  ACC_8
  lda crc
  cmp blkbuf, y
  beq @chkgood
  jsr retry_blk
  brl @get_block
@chkgood:
  iny
  lda crc + 1
  cmp blkbuf, y
  beq @chkgood2
  jsr retry_blk
  brl @get_block
@chkgood2:

  ldy #$00
@cpyloop:
  lda blkbuf + 2, y
  sta (workw), y
  iny
  cpy #$80
  bne @cpyloop

  ; increment pointer
  ACC_16
  clc
  lda workw
  adc #$80
  sta workw
  ACC_8
  inc blknum
  
  lda #ACK
  jsr putchar
  brl @get_block

@done:
  lda #ACK
  jsr putchar
  jsr delay_sec
  LD_PTR str_xmodem_finish
  jsr puts
  cli
  rts

@err:
  jsr xmodem_purge
  lda #CAN ; todo: does this even work in xmodem/crc
  jsr putchar
  jsr delay_sec
  LD_PTR str_err_xmodem_recv
  jsr puts
  cli
  rts

xmodem_send:
  ; start = workw
  ; count = workw2
  rts

xmodem_purge:
  jsr getchar_timeout
  bcc xmodem_purge
  rts

retry_blk:
  ;inc errcount
  jsr xmodem_purge
  lda #NAK
  jsr putchar
  rts

update_crc:	
  ; byte to update in a
  ; crc must be initialized to 0000
  ; clobbers x and a
  ; example calling code:
  ; lda buffer, x
  ; jsr update_crc
  ACC_16
  xba
  and #$ff00
  ldx #8
  eor crc
@rotloop:
  clc
  rol a
  bcc @clear
  eor #$1021
  sta crc
@clear:
  dex
  bne @rotloop
  sta crc
  ACC_8
  rts
