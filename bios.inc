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
  workb = $00 ; 1 byte
  workb2 = $01 ; 1 byte
  workw = $02 ; 2 bytes
  workwl = $02
  workwh = $03
  workw2 = $04 ; 2 bytes
  workw2l = $04
  workw2h = $05
  exitflag = $06 ; 1 byte

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
  ACC_16
  lda #addr
  sta workw
  ACC_8
  .endmacro

  
