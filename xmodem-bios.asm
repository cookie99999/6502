  .setcpu "65816"
  .include "hardware.inc"
  .include "bios.inc"

  .export putchar
  .export getchar
  .export puts
  .export workwh
  .export workwl
  .export reset
  
  .segment "CODE"

  ; main ram vars
  inbuf = $0200 ; 256 bytes, line buffer
  readbuf = $0300 ; read buffer for xmodem packets
  user_start = $0400 ; start of loaded program

  .macro CRLF
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
  .endmacro

  .A8
  .I8
reset:
  clc
  xce
  ACC_16
  lda #$0100
  tcs
  lda #$0000
  tcd
  ACC_8
  jsr init

  LD_PTR str_boot
  jsr puts

  ; monitor line fetch
  lda #']'
  jsr putchar
  ldx #$00
@loop:
  jsr getchar
  cmp #CR
  bne @skip
  cpx #$00
  beq @loop
  lda #$00
  sta inbuf, x ; terminate input string
  CRLF

  ; now process line
  jsr toupper
  jsr parseline
  lda #']'
  jsr putchar
  ldx #$00
  bra @loop

@skip:
  sta inbuf, x
  jsr putchar
  inx
  beq @line_ovf
  bra @loop

@line_ovf:
  LD_PTR str_err_line_ovf
  jsr puts
  bra @loop

parseline:
  ldx #$00
  lda inbuf, x
  ; xmodem recv command?
  cmp #'X'
  bne @next1
  jmp do_xmodem
@next1:
  ; run user program command?
  cmp #'R'
  bne @next2
  jmp do_run
@next2:
  ; other non hex digit - error
  jsr isdigit
  bcc bad_input
  ; inbuf must contain a hex address, get it
  pha
  inx
  lda inbuf, x
  sta workb
  pla
  jsr asc2byte
  sta workwh
  inx
  lda inbuf, x
  jsr isdigit
  bcs @shortskip
  ; short zero page addr
  dex
  lda workwh
  sta workwl
  lda #$00
  sta workwh
  bra @skip3
@shortskip:
  pha
  inx
  lda inbuf, x
  sta workb
  pla
  jsr asc2byte
  sta workwl
@skip3:
  inx
  lda inbuf, x
  cmp #':'
  beq do_poke
  cmp #'.'
  bne @skiprange
  ; get second addr, max of 1 page for now
  inx
  lda inbuf, x
  pha
  inx
  lda inbuf, x
  sta workb
  pla
  jsr asc2byte
  sta workw2h
  inx
  lda inbuf, x
  jsr isdigit
  bcs @shortskip2
  ; short zero page addr
  dex
  lda workw2h
  sta workw2l
  lda #$00
  sta workw2h
  bra @skips2
@shortskip2:
  pha
  inx
  lda inbuf, x
  sta workb
  pla
  jsr asc2byte
  sta workw2l
@skips2:
  sec
  lda workw2l
  sbc workwl
  tax
  bra @skipr2
@skiprange:
  ldx #$01
@skipr2:
  jsr peek
  rts

bad_input:
  rts ; just do nothing and return to line collect, who cares

do_poke: ; starts with x pointing at the :
  inx
  lda inbuf, x
  cmp #' '
  bne @skip1
  inx
  lda inbuf, x
@skip1:
  ; TODO validate that input is digits
  pha
  inx
  lda inbuf, x
  sta workb
  pla
  jsr asc2byte
  sta (workw)
  inx
  lda inbuf, x
  cmp #' '
  bne @end
  inc workw
  inx
  lda inbuf, x
  bra @skip1
@end:
  rts

do_run:
  CRLF
  ;ld_ptr user_start
  jmp user_start ; for now we'll just have user programs hit reset when done

do_xmodem:
  LD_PTR str_xmodem_start
  jsr puts
  lda #1 ; 1 indexed
  sta workb ; block number storage
  stz workb2 ; checksum
@sohwait:
  LD_PTR user_start
  lda #NAK
  jsr putchar
  jsr getchar_timeout
  bcs @sohwait
  cmp #SOH
  bne @sohwait
  
@get_block:
  jsr getchar ; block number
  cmp workb
  bne @err ; block num mismatch
  jsr getchar ; negated block number
  eor #$ff
  cmp workb
  bne @err ; negated block num mismatch
  inc workb
  ldy #$00
@loop:
  jsr getchar
  sta (workw), y
  clc
  adc workb2
  sta workb2
  iny
  cpy #$80 ; 128 data bytes per packet
  bne @loop

  jsr getchar ; checksum
  sta $00d0
  cmp workb2
  bra @chkgood ; minicom seems to do the checksum wrong
  jsr xmodem_purge
  lda #NAK
  jsr putchar
  bra @get_block
@chkgood:
  lda #ACK
  jsr putchar

  jsr getchar
  cmp #EOT
  beq @done
  cmp #SOH
  bne @err

  ; increment pointer
  lda workwl
  beq @half_inc
  lda #$00
  sta workwl
  inc workwh
  bra @skip0
@half_inc:
  lda #$80
  sta workwl
@skip0:
  bra @get_block

@done:
  lda #ACK
  jsr putchar
  jsr delay_sec
  LD_PTR str_xmodem_finish
  jsr puts
  rts

@err:
  lda #CAN
  jsr putchar
  LD_PTR str_err_xmodem_recv
  jsr puts
  rts

xmodem_purge:
  jsr getchar_timeout
  bcc xmodem_purge
  rts

init:
  lda #$00
  sta ACIA_STAT ;reset
  lda #$1f ;8n1, 19200
  sta ACIA_CTRL
  lda #$0b ;all of that stuff turned off
  sta ACIA_CMD

  lda VIA1_ACR
  and #%00111111 ; one shot pb7 disabled
  sta VIA1_ACR

  lda #%11000000 ; set t1 interrupt
  sta VIA1_IER
  
  rts

tx_delay:
  phx
  ldx #$ff
@loop:
  dex
  bne @loop
  plx
  rts

delay_sec:
  phx
  pha
  ldx #$10
@outer:
  lda #$ff
  sta VIA1_T1C_L
  sta VIA1_T1C_H
@loop:
  lda VIA1_IFR
  and #$40
  beq @loop
  lda VIA1_T1C_L
  dex
  bne @outer
  pla
  plx
  rts

putchar:
  sta ACIA_DATA
  jsr tx_delay
  rts

getchar:
  lda ACIA_STAT
  and #$08
  beq getchar
  lda ACIA_DATA
  rts

getchar_timeout:
  ldy #$ff
@outer:
  ldx #$ff
@inner:
  lda ACIA_STAT
  and #$08
  bne @break
  dex
  bne @inner
  dey
  bne @outer
  lda #$00
  sec
  rts
@break:
  lda ACIA_DATA
  clc
  rts

puts: ; address in workw, zero terminated
  lda (workw)
  beq @quit ; zero terminator
  jsr putchar
  inc workwl
  bne @end
  inc workwh
@end:
  bra puts
@quit:
  rts

puts_len: ; address in workw, length in x
  stx workb
  ldy #$00
@loop:
  cpy workb
  beq @quit
  lda (workw), y
  jsr putchar
  iny
  bra @loop
@quit:
  rts

isdigit: ; char to test in a, carry set if true
  cmp #'0'
  bcc @bad ; a < 0
  cmp #'F' + 1
  bcs @bad ; a > F
  cmp #'9' + 1
  bcs @middle
  bra @good
@middle:
  cmp #'A'
  bcc @bad ; a is between 9 and A, nondigit
@good:
  sec
  rts
@bad:
  clc
  rts

peek: ; addr in workw, count in x
  lda workwh
  jsr prbyte
  lda workwl
  jsr prbyte
  lda #':'
  jsr putchar
@loop:
  lda #' '
  jsr putchar
  lda (workw)
  jsr prbyte
  inc workw
  dex
  bne @loop
  CRLF
  rts

prword:
  ACC_16
  jsr prbyte
  xba
  jsr prbyte
  ACC_8
  rts
  
prbyte: ; byte to print in a, clobbers a
  pha
  lsr
  lsr
  lsr
  lsr
  and #$0f
  clc
  adc #$30
  cmp #$3a
  bcc @skip
  adc #$06
@skip:
  jsr putchar
  pla
  and #$0f
  clc
  adc #$30 ; '0' ascii
  cmp #$3a
  bcc @skip2
  adc #$06 ; difference between ascii '9' and 'A'
@skip2:
  jsr putchar
  rts

toupper: ; converts inbuf to uppercase, ignoring non letters
  ldx #$00
@loop:
  lda inbuf, x
  beq @quit
  cmp #'A'
  bcc @skip2 ; a < A
  cmp #'z' + 1
  bcs @skip2 ; a > z
  cmp #'a'
  bcs @skip1 ; a >= a
  cmp #'Z' + 1
  bcc @skip1 ; a <= Z
  bra @skip2 ; a is not letter
@skip1: ; A <= a <= Z or a <= a <= z
  cmp #'a'
  bcc @quit ; already uppercase
  sec
  sbc #$20
  sta inbuf, x
@skip2:
  inx
  bra @loop
@quit:
  rts


asc2nyb: ; ascii in a, returned nibble is least significant part of a
  sec
  sbc #$30 ; '0' ascii
  cmp #CR
  bcc @skip
  sbc #$07 ; difference between ascii '9' and 'A'
@skip:
  and #$0f
  rts

asc2byte: ; first nyb in a, second in workb, returned in a
  jsr asc2nyb
  asl
  asl
  asl
  asl
  pha
  lda workb
  jsr asc2nyb
  sta workb
  pla
  ora workb
  rts

brkvec:
  jsr (svc_table, x)
  rti

svc_table:
  .word reset
  .word putchar
  .word puts
  .word getchar
  .word prbyte
  .word delay_sec

str_boot:
  .byte "ROM kernel ready", CR, LF, 0
str_xmodem_start:
  .byte "Receiving Xmodem file...", CR, LF, 0
str_xmodem_finish:
  .byte "Successfully received Xmodem file", CR, LF, 0
str_err_line_ovf:
  .byte "ERR: Line too long", CR, LF, 0
str_err_xmodem_recv:
  .byte "ERR: Xmodem receive failure", CR, LF, 0

  .segment "RESETVEC"	
  .word 0 ; cop 816
  .word brkvec
  .word 0 ; abort 816
  .word 0 ; nmi 816
  .word 0 ; reserved
  .word 0 ; irq 816
  .word 0 ; reserved
  .word 0 ; reserved
  .word 0 ; cop 02
  .word 0 ; reserved
  .word 0 ; abort 02
  .word $0000, reset, $0000
