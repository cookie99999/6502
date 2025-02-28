  .setcpu "65816"
  .segment "CODE"
  .include "hardware.inc"
  .include "bios.inc"
  
start:
  ACC_8
  IND_8
  lda #$00
  sta y_im
  sta x_real
  
  sta screen_count
  
y_loop:
  ldy y_im
  lda i0_tab, y
  sta i0+1
  
  ; sign must be checked. If number is negative, 16 bit complement must be evaluated
  bpl no_16bit_complement_i0
  
  ; performs 16 bit complement
  lda #$ff
  sta i0
  
  jmp skip_no_16bit_complement_i0
  
no_16bit_complement_i0:	
  
  lda #$00
  sta i0                  ;high byte is 0 (the number is positive)
  
skip_no_16bit_complement_i0:	
x_loop:
  lda #$01
  sta mf                  ;sets mandelbrot flag to 1
  sta k                   ;initializes counter for iterations
  
  ldx x_real              
  lda r0_tab, x
  sta r0+1
  
  bpl no_16bit_complement_r0
  
  ;performs 16 bit complement
  lda #$ff
  sta r0
  
  jmp skip_no16bit_complement_r0

no_16bit_complement_r0:	
  lda #$00
  sta r0
  
skip_no16bit_complement_r0:	
  lda #$00
  sta real
  sta real+1
  sta im
  sta im+1                ;clears out 16 bit real and imaginary parts
  
  ;starts iterations
iter_start:	
  lda real
  sta multiplicand
  lda real+1
  sta multiplicand+1
  lda real 
  sta multiplier
  lda real+1 
  sta multiplier+1
  
  jsr multiply_ab
  
  lda sum
  sta r2                  ;high byte
  
  lda sum+1
  sta r2+1                ;low byte
  
  ;computes R2 = real * real (16 bit) !!test real*im
  lda im
  sta multiplicand
  lda im+1 
  sta multiplicand+1
  lda im
  sta multiplier
  lda im+1
  sta multiplier+1
  
  jsr multiply_ab
  
  lda sum
  sta i2                  ;high byte
  
  lda sum+1
  sta i2+1                ;low byte
  
  ;computes I2 = im * im (16 bit)
  clc
  lda r2+1
  adc i2+1
  sta r2_plus_i2+1
  
  lda i2
  adc r2
  sta r2_plus_i2          ;computes I2 + R2 
  
  ;4 * offset is 4 * 64 = 256
  ;if 256 < r2_plus_i2 then mf = 0:rn = k-1: goto ...
  ;as we need greater than (NOT including equal), we must revert the condition
  ;so that we can use BCC
  
  lda #>256
  cmp r2_plus_i2
  bcc no_mandelbrot_set
  bne skip_no_mandelbrot_set
  lda #<256
  cmp r2_plus_i2+1
  bcc no_mandelbrot_set   ;16 bit comparison, mandelbrot set condition
  
  jmp skip_no_mandelbrot_set
  
no_mandelbrot_set:
  ldx k
  dex
  dex ; needed because table indexing starts from 0
  
  lda color_table, x
  ldy screen_count
mod1:
  phx
  phy
  pha
  SVC SVC_PUTCHAR
  pla
  ply
  plx
  
  jmp next_loop
  
skip_no_mandelbrot_set:	
  ; COMPUTES I = ((2 * R) * I) + I0(which is 2 * real * im + i0)
  asl real+1
  rol real                ;computes real * 2 (works with signed numbers too)
  lda real
  sta temp2
  
  lda real+1
  sta temp2+1             ;temp2 = real * 2
  
  lda temp2
  sta multiplicand
  lda temp2+1 
  sta multiplicand+1
  lda im
  sta multiplier
  lda im+1
  sta multiplier+1
  
  jsr multiply_ab
  
  lda sum
  sta im
  lda sum+1
  sta im+1                ; I = (2 * R) * I
  
  clc
  
  lda im+1
  adc i0+1
  sta im+1
  
  lda im
  adc i0
  sta im                  ; I = I + i0 = (2 * R) * I + i0
  
  ; computes R = R2-I2 + r0
  sec
  lda r2+1
  sbc i2+1
  sta real+1
  
  lda r2
  sbc i2
  sta real                ; R = R2 - I2
  
  clc
  lda real+1
  adc r0+1
  sta real+1
  
  lda real
  adc r0
  sta real                ; R = R + r0 = R2-I2 +r0
  
  inc k
  
  lda k
  cmp #21
  bne jump_iter_start
  
mandelbrot_set:
  ;lda #32
  ;jsr $ffd2
  
  lda #0
  ldy screen_count
mod2:
  lda #' '
  phx
  phy
  pha
  SVC SVC_PUTCHAR
  pla
  ply
  plx
  
  jmp next_loop
  
jump_iter_start:
  jmp iter_start
  
next_loop:	
  inc x_real
  
  inc screen_count
  bne continue
  
  ;inc mod1+2
  ;inc mod2+2
  
continue:
  lda x_real
  cmp #40
  bne x_loop_jump              ;next x loop (x has been already incremented).
  
  lda #$00
  sta x_real                   ;resets x counter
  
  inc y_im
  lda #CR
  SVC SVC_PUTCHAR
  lda #LF
  SVC SVC_PUTCHAR
  
  lda y_im
  cmp #25
  bne y_loop_jump              ;next y loop
  
spacechk:
  ;lda 	$dc01 
  ;cmp 	#$ef 
  ;bne 	spacechk 
  rts
  
x_loop_jump:
  jmp x_loop

y_loop_jump:
  jmp y_loop

  ; routine: signed 16 bit multiply - 16 bit result
multiply_ab:
  lda	#$00
  sta	sum
  sta   sum+1
  
  sta   multiplicand_sign
  ;multiplicand sign positive
  sta   multiplier_sign ;multiplier sign positive

  ldx	#16		;number of bits
  
  lda   multiplicand    ;checks sign on high byte
  bpl   skip_multiplicand_comp
  
  sec
  
  lda     #<65536
  sbc     multiplicand+1
  sta     multiplicand+1  ;takes complement of multiplicand (low byte first)
  lda     #>65536
  sbc     multiplicand
  sta     multiplicand    ;high byte
  
  lda     #$01
  sta     multiplicand_sign  
  ;multiplicand sign set to negative
  
skip_multiplicand_comp:	
  lda     multiplier
  bpl     loop            ;checks sign on high byte
  
  sec
  
  lda     #<65536
  sbc     multiplier+1
  sta     multiplier+1    ;takes complement of multiplier (low byte first)
  lda     #>65536
  sbc     multiplier
  sta     multiplier      ;high byte
  
  lda     #$01
  sta     multiplier_sign  
  ;multiplicand sign set to negative

loop:	
  asl	sum+1
  rol   sum
  rol	multiplier+1
  rol   multiplier
  bcc	skip_add

  clc
  lda	sum+1
  adc	multiplicand+1
  sta   sum+1
  lda   sum
  adc   multiplicand
  sta   sum
  
skip_add:
  dex
  bne	loop
  
  ;sum is high bite, sum+1 is lower byte
  ;lower bytes are simply discarded
  
  ; divide by offset (64) ; 
  lsr sum
  ror sum+1
  lsr sum
  ror sum+1
  lsr sum
  ror sum+1
  lsr sum
  ror sum+1
  lsr sum
  ror sum+1
  lsr sum
  ror sum+1
  
  ; sign of product evaluation
  lda multiplicand_sign
  eor multiplier_sign         
  
  cmp #$01
  bne skip_product_complement
  ;if product is not negative, skip product complement
  sec
  lda #< 65536
  sbc sum+1
  sta sum+1
  lda #> 65536
  sbc sum
  sta sum          ;takes 2 complement of product (16 bit)
  
skip_product_complement:
  rts
  
  ;storage locations for 16 bit multiply
multiplicand:
  .byte	0,0
multiplier:
  .byte	0,0             ;high bytes of product
sum:
  .byte	0,0             ;low bytes of product (unused)
multiplicand_sign:	
  .byte    0
multiplier_sign:
  .byte    0
product_sign:
  .byte    0
  
  ;result on multiplier and sum
  
  ;storage locations for main program
i0:
  .byte    0,0
r0:
  .byte    0,0
y_im:
  .byte    0
x_real:
  .byte    0
mf:
  .byte    0
real:
  .byte    0,0
im:
  .byte    0,0
r2:
  .byte    0,0
i2:
  .byte    0,0
r2_plus_i2:
  .byte    0,0  
k:
  .byte    0
screen_count:
  .byte    0 
temp1:
  .byte    0
temp2:
  .byte    0,0

  ; tables

  ; tables should be 16 bit in two's complement form
  ; otherwise, 8 bit 2's complement numbers must be converted to 16 bit 2's complement on the code
i0_tab:
  .byte    185 , 191 , 196 , 202 , 208 , 213 , 219 , 225 
  .byte    230 , 236 , 241 , 247 , 253 , 2  , 8 
  .byte    14 , 19 , 25 , 30 , 36 , 42 , 47 , 53 
  .byte    59 , 64

r0_tab:
  .byte    129 , 133 , 137 , 141 , 145 , 149 , 153
  .byte    157 , 161 , 165 , 169 , 173 , 177 , 181
  .byte    185 , 189 , 193 , 197 , 201 , 205 
  .byte    208 , 212 , 216 , 220 , 224 , 228 , 232
  .byte    236 , 240 , 244 , 248 , 252 , 0 , 4 
  .byte    8 , 12 , 16 , 20 , 24 , 28
  
color_table:
  .byte    ':' , 'w' , 'w' , ':' , '.' , 'w' , ':' , 'W' , ',' , '#'
  .byte    'i' , '#' , '#' , 'W' , ':' , 'w' , 'w' , ':' , '.', 'w'
