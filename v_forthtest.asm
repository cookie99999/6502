	.org 0x8000
	.equ IP, 0x0002 	; 2 bytes
	.equ SCRATCH, 0x0006 	;2 bytes

	jmp start
	
	.macro INC16 addr
	inc addr
	bne 1f
	inc addr + 1
1:
	.endm

	.macro NEXT
	ldy #1
	lda (IP), y
	sta SCRATCH + 1
	dey
	lda (IP), y
	sta SCRATCH
	clc
	lda IP
	adc #2
	sta IP
	bcc 1f
	inc IP + 1
1:
	jmp SCRATCH - 1		;JMP opcode at SCRATCH - 1
	.endm

	.macro PUSHRSP
	lda IP+1
	pha
	lda IP
	pha
	.endm

	.macro POPRSP
	pla
	sta IP
	pla
	sta IP+1
	.endm

DOCOL:
	PUSHRSP
	clc
	lda SCRATCH
	adc #2
	sta IP
	bcc 1f
	inc IP + 1
1:
	NEXT

	start:
	ldx #0xff
	txs
	lda #>cold_start
	sta IP + 1
	lda #<cold_start
	sta IP
	NEXT

cold_start:
	.word QUIT

	.set link, 0
	
	.macro defword name, namelen, flags=0, label
name_\label:
	.word link
	.set link, name_\label
	.byte \flags + \namelen
	.ascii "\name"
\label:
	.word DOCOL
	;; other word pointers after
	.endm

	.macro defcode name, namelen, flags=0, label
name_\label:
	.word link
	.set link, name_\label
	.byte \flags + \namelen
	.ascii "\name"
\label:
	.word code_\label
code_\label:
	.endm

	defcode "DROP", 4, , DROP ; ( -- )
	inx
	inx
	NEXT

	defcode "DUP", 3, , DUP	; ( a -- a a )
	dex
	dex
	lda 2, x
	sta 0, x
	lda 3, x
	sta 1, x
	NEXT

	defcode "SWAP", 4, , SWAP ; ( a b -- b a )
	lda 0, x
	ldy 2, x
	sta 2, x
	sty 0, x
	lda 1, x
	ldy 3, x
	sta 3, x
	sty 1, x
	NEXT

	defcode "+", 1, , PLUS 	; ( a b -- a+b)
	clc
	lda 0, x
	adc 2, x 		; low bytes added
	sta 2, x
	lda 1, x
	adc 3, x
	sta 3, x
	inx
	inx 			; DROP
	NEXT

	defcode "-", 1, , MINUS ; ( a b -- b-a )
	clc
	lda 0, x
	sbc 2, x
	sta 2, x
	lda 1, x
	sbc 3, x
	sta 3, x
	inx
	inx
	NEXT

	defcode "OVER", 4, , OVER ; ( a b -- a b a )
	dex
	dex
	lda 4, x
	sta 0, x
	lda 5, x
	sta 1, x
	NEXT

	defcode "ROT", 3, , ROT ; ( a b c -- c a b )
	ldy 0, x
	lda 4, x
	sta 0, x
	lda 2, x
	sta 4, x
	sty 2, x
	ldy 1, x
	lda 5, x
	sta 1, x
	lda 3, x
	sta 5, x
	sty 1, x
	NEXT

	defcode "-ROT", 4, , NROT ; ( a b c -- b c a )
	ldy 0, x
	lda 2, x
	sta 0, x
	lda 4, x
	sta 2, x
	sty 4, x
	ldy 1, x
	lda 3, x
	sta 1, x
	lda 5, x
	sta 3, x
	sty 5, x
	NEXT

	defcode "2DROP", 5, , TWODROP ; ( a b -- )
	inx
	inx
	inx
	inx
	NEXT

	defcode "2DUP", 4, , TWODUP ; ( a b -- a b a b )
	dex
	dex
	dex
	dex
	lda 6, x
	sta 2, x
	lda 7, x
	sta 3, x
	lda 4, x
	sta 0, x
	lda 5, x
	sta 1, x
	NEXT

	defcode "2SWAP", 5, , TWOSWAP ; ( a b c d -- b a d c )
	lda 0, x
	ldy 2, x
	sta 2, x
	sty 0, x
	lda 1, x
	ldy 3, x
	sta 3, x
	sty 1, x

	lda 4, x
	ldy 6, x
	sta 6, x
	sty 4, x
	lda 5, x
	ldy 7, x
	sta 7, x
	sty 5, x
	NEXT

	defcode "?DUP", 4, , QDUP ; dup if top of stack nonzero
	lda 1, x
	bne 1f
	lda 0, x
	bne 1f
	NEXT
1:	dex
	dex
	lda 2, x
	sta 0, x
	lda 3, x
	sta 1, x
	NEXT

	defcode "1+", 2, , INCR ; ( a -- a+1 )
	inc 0, x
	bne 1f
	inc 1, x
1:	NEXT

	defcode "1-", 2, , DECR ; ( a -- a-1 )
	clc
	lda 0, x
	sbc #1
	sta 0, x
	bcs 1f
	dec 1, x
1:	NEXT

	defcode "2+", 2, , INCR2 ; ( a -- a+2 )
	clc
	lda 0, x
	adc #2
	sta 0, x
	bcc 1f
	inc 1, x
1:	NEXT

	defcode "2-", 2, , DECR2 ; ( a -- a-2 )
	clc
	lda 0, x
	sbc #2
	sta 0, x
	bcs 1f
	dec 1, x
1:	NEXT

	
	
	.org 0xfffa
	.word 0, 0, 0
