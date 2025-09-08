  ; bios services
  SVC_RESET = 0
  SVC_PUTCHAR = 1
  SVC_PUTS = 2
  SVC_GETCHAR = 3
  SVC_PRBYTE = 4
  SVC_DELAY_SEC = 5

  .macro SVC offs
  ACC_8
  IND_8
  ldx #(offs * 2)
  brk
  nop
  .endmacro

  ; zp vars
  inbuf_read = $00
  inbuf_write = $01
  workb = $02 ; 1 byte
  workb2 = $03 ; 1 byte
  workw = $04 ; 2 bytes
  workwl = $04
  workwh = $05
  workw2 = $06 ; 2 bytes
  workw2l = $06
  workw2h = $07
  exitflag = $08 ; 1 byte
  ; $07 reserved
  irqvec = $0a ; 4 bytes
  nmivec = $0e ; 4 bytes

  ; main ram vars
  inbuf = $0200 ; 256 bytes, keyboard buffer
  linebuf = $0300 ; 256 bytes, monitor line buffer

  ; ascii constants
  ACK = $06
  NAK = $15
  CR = $0d
  LF = $0a
  ESC = $1b
  SOH = $01
  EOT = $04
  CAN = $18
  SUB = $1a
  BS = $08

  ; utility macros
  .macro ACC_16
  rep #$20
  .A16
  .endmacro

  .macro IND_16
  rep #$10
  .I16
  .endmacro

  .macro ACC_8
  sep #$20
  .A8
  .endmacro

  .macro IND_8
  sep #$10
  .I8
  .endmacro

  .macro LD_PTR addr ; puts 16 bit pointer into workw
  lda #<addr
  sta workwl
  lda #>addr
  sta workwh
  .endmacro
  
  .macro CRLF
  lda #CR
  jsr putchar
  lda #LF
  jsr putchar
  .endmacro
