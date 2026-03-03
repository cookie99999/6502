  ; bios services
  SVC_RESET = 0
  SVC_PUTCHAR = 1
  SVC_PUTS = 2
  SVC_GETCHAR = 3
  SVC_PRBYTE = 4
  SVC_DELAY_SEC = 5
  SVC_RAND8 = 6
  SVC_RAND16 = 7

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
  sec_per_cluster = $07
  current_dir = $08 ; 2 byte cluster num of current directory, 0=root
  irqvec = $0a ; 4 bytes
  nmivec = $0e ; 4 bytes
  vol_start = $12
  fat_start = $16
  root_start = $1a
  data_start = $1d
  prng_state = $22 ; 2 bytes, current lfsr state to get randoms from
  workb = $24 ; 1 byte
  workb2 = $25 ; 1 byte
  workw = $26 ; 2 bytes
  workwl = $26
  workwh = $27
  workw2 = $28 ; 2 bytes
  workw2l = $28
  workw2h = $29
  workq = $2a
  workql = $2a
  workqh = $2c
  workq2 = $2e
  workq2l = $2e
  workq2h = $30
  retw = $32
  retw2 = $34
  scratch = $36 ; beginning of general purpose scratch space
  
  ; main ram vars
  inbuf = $0200 ; 256 bytes, keyboard buffer
  linebuf = $0300 ; 256 bytes, monitor line buffer
  sec_buf = $0400 ; 512 bytes for currently worked on disk sector
  fat_buf = $0600 ; 512 bytes for current fat sector
  user_start = $0800 ; start of programs loaded from disk/serial
  
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
