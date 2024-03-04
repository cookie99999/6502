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

	.macro PUT_A
	dex
	dex
	sta 0, x
	stz 1, x
	.endmacro

	;; ( addr val -- )
STORE:
	lda 2, x
	sta (0, x)
	jsr INC_16
	lda 3, x
	sta (0, x)
	DROP
	DROP
	rts

	;; ( addr val -- )
STORE_8:
	lda 2, x
	sta (0, x)
	DROP
	DROP
	rts

	;; ( addr - val)
FETCH_8:
	lda (0, x)
	sta 0, x
	stz 1, x
	rts
	
	;; ( n - n+1 )
INC_8:
	inc 0, x
	rts
	
	;; ( n - n+1 )
INC_16:
	inc 0, x ; low byte
	bne @end
	inc 1, x ; high byte
@end:
	rts
	
	;; ( count - )
DELAY:
	dec 0, x ; decrement low byte
	bne DELAY
	dec 1, x ; decrement high byte
	bne DELAY
	DROP
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

	;; ( a - a a )
DUP:
	dex
	dex
	lda 2, x
	sta 0, x
	lda 3, x
	sta 1, x
	rts
