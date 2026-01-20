  .setcpu "65816"
  .include "hardware.inc"
  .include "bios.inc"
  .include "vga.inc"
  .include "cf.inc"

  MHZ_MULT = 4 ; current installed clock speed
  JIF_COUNT = 9999 * MHZ_MULT ; 100hz counter

  .export putchar
  .export putchar_serial
  .export prbyte
  .export getchar
  .export getchar_old_serial
  .export puts
  .export reset
  
  .segment "CODE"
  .include "xmodem.asm"

  .A8
  .I8

init:
  lda #$40 ; RTI
  ;sta irqvec ; no handlers by default
  sta nmivec ; user can add a jmp or jml to their own
  lda #$4c
  sta irqvec
  lda #<irq_handler
  sta irqvec + 1
  lda #>irq_handler
  sta irqvec + 2

  sta inbuf_read
  sta inbuf_write ; init buffer pointers
  
  lda #$00
  sta ACIA_STAT ;reset
  lda #$1e ;8n1, 9600
  sta ACIA_CTRL
  lda #$0b ; dtrb low rx irq disabled rtsb low no parity normal echo mode
  sta ACIA_CMD

  stz jifs
  stz secs
  stz mins
  stz hrs

  lda #%11000000
  sta VIA1_IER ; enable t1 interrupts
  lda #$43
  sta VIA1_ACR ; t1 continuous pb7 off

  stz VIA1_DDRA ; all inputs
  lda #%00001000
  sta VIA1_PCR ; ca2 handshake output, ca1 negative edge triggered
  lda #$82
  sta VIA1_IER ; enable ca1 interrupts
  
  rts
  
reset:
  clc
  xce
  ACC_16
  lda #$01ff
  tcs
  lda #$0000
  tcd
  ACC_8
  jsr init
  cli
  lda #<JIF_COUNT
  sta VIA1_T1C_L
  lda #>JIF_COUNT
  sta VIA1_T1C_H

  LD_PTR str_boot
  jsr puts

  ; monitor line fetch
@ready:
  lda #']'
  jsr putchar
  jsr readline
  bcs @ready ; try again on error

  ; now process line
  jsr toupper
  jsr parseline
  bra @ready

parseline:
  txy ; y = line length backup, not including null
  ldx #$00
  lda linebuf, x
  ; xmodem recv command?
  cmp #'X'
  bne @notx
  inx
  lda linebuf, x
  cmp #'R'
  bne @notxr
  inx
  lda linebuf, x
  jsr isdigit
  bcc @default_load
  jsr getaddr
  bra :+
@default_load: ; receive files at 0400 if not otherwise specified
  lda #$04
  sta workwh
  stz workwl
:
  jmp xmodem_recv
@notxr:
  cmp #'S'
  bne :+
  jmp xmodem_send
:
  dex
  lda linebuf, x
@notx:
  ; other non hex digit = error
  jsr isdigit
  bcs :+
  jmp bad_input
:	
  ; linebuf must contain a hex address, get it
  jsr getaddr
  bcs :+
  jmp bad_input
:	
  inx
  lda linebuf, x
  cmp #':'
  bne :+
  jmp do_poke
:	
  cmp #'G'
  bne :+
  jmp do_run
:	
  cmp #'.'
  bne @skiprange
  ; TODO refactor this into another getaddr call
  ; get second addr, max of 1 page for now
  inx
  lda linebuf, x
  pha
  inx
  lda linebuf, x
  sta workb
  pla
  jsr asc2byte
  sta workw2h
  inx
  lda linebuf, x
  jsr isdigit
  bcs @shortskip2
  ; short zero page addr
  dex
  lda workw2h
  sta workw2l
  stz workw2h
  bra @skips2
@shortskip2:
  pha
  inx
  lda linebuf, x
  sta workb
  pla
  jsr asc2byte
  sta workw2l
@skips2:
  sec
  lda workw2l
  sbc workwl
  tax
  inx ; make the range inclusive
  bra @skipr2
@skiprange:
  ldx #$01
@skipr2:
  jsr peek
@quit:
  rts

bad_input:
  LD_PTR str_err_bad_input
  jsr puts
  rts

peek: ; addr in workw, count in x
  ; todo: should be able to use 16 bit range easily
  lda workwh
  xba
  lda workwl
  jsr prword
  lda #':'
  jsr putchar
  ldy #24
@loop:
  lda #' '
  jsr putchar
  lda (workw)
  jsr prbyte
  inc workw
  dey
  bne :+
  CRLF
  .repeat 5
  lda #' '
  jsr putchar
  .endrep
  ldy #24
:	
  dex
  bne @loop
  CRLF
  rts

do_poke: ; starts with x pointing at the :
  inx
  lda linebuf, x
  cmp #' '
  bne @skip1
  inx
  lda linebuf, x
@skip1:
  jsr isdigit
  bcs :+
  jmp bad_input
:	
  xba
  inx
  lda linebuf, x
  jsr isdigit
  bcs :+
  jmp bad_input
:	
  sta workb
  xba
  jsr asc2byte
  sta (workw)
  inx
  lda linebuf, x
  cmp #' '
  bne @end
  inc workw
  inx
  lda linebuf, x
  bra @skip1
@end:
  rts

do_run:
  ldx #$00
  jsr (workw, x)
  sep #$30
  rts

  ;---------------------------------------
  ;  Line buffer manipulation procedures
  ;---------------------------------------

readline: ; return line length in x, carry set on error
  ldx #$00
@loop:
  jsr getchar
  bcc @loop
  cmp #CR
  bne @skip ; regular character, add to buffer
  cpx #$00 ; return pressed on empty buffer, ignore
  beq @loop
  stz linebuf, x ; null terminate and finish
  CRLF
  clc
  rts
@skip:
  cmp #$1f
  bcc @loop
  cmp #$7f
  bcs @loop ; ignore unprintable characters
  sta linebuf, x
  jsr putchar ; echo typed char
  inx
  beq @line_ovf
  bra @loop
@line_ovf:
  LD_PTR str_err_line_ovf
  jsr puts
  sec
  rts

toupper: ; converts linebuf to uppercase, ignoring non letters
  phx
  ldx #$00
@loop:
  lda linebuf, x
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
  sta linebuf, x
@skip2:
  inx
  bra @loop
@quit:
  plx
  rts

getaddr: ; get 2 or 4 digit addr from linebuf[x] and put it in workw
  ; carry set = ok, carry clear = err
  xba ; first digit should already be in a
  inx ; parseline will only allow a digit through so no need to check
  lda linebuf, x
  sta workb
  xba
  jsr isdigit
  bcc @err ; bad char in 2nd digit
  jsr asc2byte
  sta workwh
  inx
  lda linebuf, x
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
  xba
  inx
  lda linebuf, x
  jsr isdigit
  bcc @err ; only 3 digits typed
  sta workb
  xba
  jsr isdigit
  bcc @err ; bad char in the middle
  jsr asc2byte
  sta workwl
@skip3:
  sec
  rts
@err:
  clc
  rts

  ;---------------------------------------
  ;  Text manipulation/Output procedures
  ;---------------------------------------

putchar_serial:
  sta ACIA_DATA
  jsr tx_delay
  rts

putchar: ; TODO maybe make it a macro since it's just 1 instruction
  sta VGA_DATA
  rts

puts: ; address in workw, zero terminated
  lda (workw)
  beq @quit ; zero terminator
  jsr putchar
  inc workwl
  bne @skip
  inc workwh
@skip:
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
  xba
  lda workb
  jsr asc2nyb
  sta workb
  xba
  ora workb
  rts
  
prword:
  xba
  jsr prbyte
  xba
  jsr prbyte
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

  ;---------------------------------------
  ;  Input procedures
  ;---------------------------------------

getchar_old_serial:
  lda ACIA_STAT
  and #$08
  beq getchar_old_serial
  lda ACIA_DATA
  rts

getchar:
  phx
  ldx inbuf_read
  cpx inbuf_write
  beq @nochar
  lda inbuf, x
  inc inbuf_read
  sec
  plx
  rts
@nochar:
  clc
  plx
  rts

getchar_timeout:
  phy
  phx
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
  plx
  ply
  sec
  rts
@break:
  lda ACIA_DATA
  plx
  ply
  clc
  rts

  ;---------------------------------------
  ;  Misc procedures
  ;---------------------------------------

tx_delay:
  phy
  ldy #MHZ_MULT
@outer:
  phx
  ldx #$ff
@loop:
  dex
  bne @loop
  plx
  dey
  bne @outer
  ply
  rts

delay_sec:
  phx
  pha
  ldx #($10 * MHZ_MULT)
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

irq_handler:
  ACC_16
  IND_16
  pha
  phx
  phy
  IND_8
  ACC_8
  lda VIA1_IFR
  and #$40
  beq @not_timer
  bit VIA1_T1C_L
  inc jifs
  lda jifs
  cmp #100
  bne @quit
  stz jifs
  inc secs
  lda secs
  cmp #60
  bne @quit
  stz secs
  inc mins
  lda mins
  cmp #60
  bne @quit
  stz mins
  inc hrs
  lda hrs
  cmp #24
  bne @quit
  stz hrs
  bra @quit
  
@not_timer:
  lda VIA1_IFR
  and #$02
  beq @via_quit
  ; data ready from keyboard
  lda #$02
  lda VIA1_PA
  beq @quit ; 0 = no data
  ldx inbuf_write
  sta inbuf, x
  inc inbuf_write
  bra @quit
  
@via_quit:
  lda VIA1_IFR
  sta VIA1_IFR
@quit:
  ACC_16
  IND_16
  ply
  plx
  pla
  rti
  
irq:
  jmp irqvec

nmi:
  jmp nmivec
  
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
  .byte "Ready", CR, LF, 0
str_err_line_ovf:
  .byte "ERR: Line too long", CR, LF, 0
str_err_bad_input:
  .byte "ERR: Bad input", CR, LF, 0

  .segment "RESETVEC"	
  .word 0 ; cop 816
  .word brkvec
  .word 0 ; abort 816
  .word nmi ; nmi 816
  .word 0 ; reserved
  .word irq ; irq 816
  .word 0 ; reserved
  .word 0 ; reserved
  .word 0 ; cop 02
  .word 0 ; reserved
  .word 0 ; abort 02
  .word nmi, reset, irq
