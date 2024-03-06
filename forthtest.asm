	.segment "CODE"

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

	defcode "DROP", 4, , DROP
	inx
	inx
	NEXT

	defcode "SWAP", 4, , SWAP
	lda 0, x
	ldy 2, x
	sta 2, x
	sty 0, x
	lda 1, x
	ldy 3, x
	sta 3, x
	sty 1, x
	NEXT
DOCOL:
	.byte $55, $aa

	.segment "RESETVEC"
	.word 0, 0, 0
