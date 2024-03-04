	.segment "CODE"
main:	
	lda #$40
loop:	
	sta $2000
	jmp loop

	.segment "RESETVEC"
	.word $0000		;nmi
	.word main		;reset
	.word $0000		;brk/irq
