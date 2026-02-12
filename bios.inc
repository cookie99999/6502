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
  exitflag = $02 ; 1 byte
  jifs = $03 ; clock variables
  secs = $04
  mins = $05
  hrs = $06
  ; 07 reserved
  current_dir = $08 ; 2 byte cluster num of current directory, 0=root
  irqvec = $0a ; 4 bytes
  nmivec = $0e ; 4 bytes
  ; 12-1f reserved
  workb = $20 ; 1 byte
  workb2 = $21 ; 1 byte
  workw = $22 ; 2 bytes
  workwl = $22
  workwh = $23
  workw2 = $24 ; 2 bytes
  workw2l = $24
  workw2h = $25
  workq = $26
  workql = $26
  workqh = $28
  workq2 = $2a
  workq2l = $2a
  workq2h = $2c
  retw = $2e
  retw2 = $30
  ; 31 reserved
  scratch = $32 ; beginning of general purpose scratch space
  
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
  TAB = $09

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
