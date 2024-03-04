	.include "hardware.inc"

	.macro DROP
	inx
	inx
	.endmacro

	.macro PUT arg
	dex
	dex
	lda #<arg
	sta 0, x
	lda #>arg
	sta 1, x
	.endmacro

	.macro DUP
	dex
	dex
	lda 2, x
	sta 0, x
	lda 3, x
	sta 1, x
	.endmacro

	.segment "CODE"
	jmp reset
	.include "7seg.asm"

start:
	ldx #$ff
	txs ; return stack top of page 1
				; X will be data stack ptr, top of zp
	stx VIA1_DDRA ; all outputs

loop_outer:	
	PUT 0

loop:
	jsr SEV_SEG_PUTCHAR
	PUT $ffff
	jsr DELAY
	PUT 1
	jsr PLUS
	PUT $10
	jsr CMP_8
	bne loop
	DROP
	jmp loop_outer
	

	;; ( count - )
DELAY:
	dec 0, x ; decrement low byte
	bne DELAY
	dec 0, x ; back to ff
	dec 1, x ; decrement high byte
	bne DELAY
	DROP
	rts

	;; ( char )
SEV_SEG_PUTCHAR:
	ldy 0, x
	lda sev_seg_table, y
	sta VIA1_PA
	rts

	;; ( a b -- b )
CMP_8:	
	lda 0, x
	DROP
	cmp 0, x
	rts

	;; ( a b -- c )
PLUS:
	clc
	lda 0, x
	adc 2, x ; low bytes added
	sta 2, x
	lda 1, x
	adc 3, x ; high bytes added
	sta 3, x
	DROP
	rts

	.segment "RESETVEC"
nmi:	.word $0000
reset:	.word start
irq:	.word $0000
