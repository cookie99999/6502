	.segment "CODE"

	IP = $0002 		; 2 bytes
	SCRATCH = $0006 	;2 bytes

	.macro INC16 addr
	inc addr
	bne @skip
	inc addr + 1
@skip:
	.endmacro

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
	bcc @skip
	inc IP + 1
@skip:
	jmp SCRATCH - 1		;JMP opcode at SCRATCH - 1
	.endmacro

	.macro PUSHRSP
	lda IP+1
	pha
	lda IP
	pha
	.endmacro

	.macro POPRSP
	pla
	sta IP
	pla
	sta IP+1
	.endmacro

	link .set 0
	
	.macro defword name, namelen, flags, label
.ident(.sprintf("name_%s", label)):
	.word link
	link .set .sprintf("name_%s", label)
	.ifnblank flags
	.byte flags + namelen
	.else
	.byte namelen
	.endif
	.byte name
.ident(label):
	.word DOCOL
	;; other word pointers after
	.endmacro

	.macro defcode name, namelen, flags, label
.ident(.sprintf("name_%s", label)):
	.word link
	link .set .sprintf("name_%s", label)
	.ifnblank flags
	.byte flags + namelen
	.else
	.byte namelen
	.endif
	.byte name
.ident(label):
	.word .ident(.sprintf("code_%s", label))
	.ident(.sprintf("code_%s", label)):
.endmacro

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
	
DOCOL:
	.byte $55, $aa

	.segment "RESETVEC"
	.word 0, 0, 0
