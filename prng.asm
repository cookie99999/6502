  .A8
  .I8

  .export rand8
  .export rand16

  ; seed prng_state with a nonzero value before using
  
rand16: ; 7 9 13 taps
  .A16
  lda prng_state
  .repeat 7
  lsr
  .endrep
  eor prng_state
  sta prng_state
  .repeat 9
  asl
  .endrep
  eor prng_state
  sta prng_state
  .repeat 13
  lsr
  .endrep
  eor prng_state
  sta prng_state
  rts

rand8:
  ACC_16
  jsr rand16
  lsr
  lsr
  lsr
  lsr ; low bits may have shorter period
  ACC_8
  rts
