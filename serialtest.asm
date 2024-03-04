	.include "hardware.inc"

	cmdready = $800 ; 1 byte
	inputindex = $1000 ; 2 bytes, current writing location in inputbuf
	inputbuf = $2000 ; 256 bytes?
	
	.segment "CODE"

	jmp start

	.include "protoforth.asm"

teststr:
	.byte "You typed: ", 0

irq:
	lda VIA1_PA
	cmp #$0a ; \n
	bne @continue
	PUT 0 ; \0
	PUT inputbuf
	lda inputindex
	PUT_A
	jsr PLUS
	jsr STORE_8 ; write null terminator
	stz inputindex
	stz inputindex + 1
	lda #1 ; received whole string, reset index, let main loop know
	sta cmdready
	jmp @skip
	

@continue:
	PUT_A
	PUT inputbuf
	lda inputindex
	PUT_A
	jsr PLUS
	jsr STORE_8 ; byte from port into current index in inputbuf

	inc inputindex
	
@skip:
	inc VIA2_PB
	PUT $0101
	jsr DELAY
	dec VIA2_PB
	
	rti

start:
	ldx #$ff
	txs
	stx VIA1_DDRB ; all output
	stx VIA2_DDRB			; keep x = top of data stack in zp
	stz VIA1_DDRA ; all input
	lda #%10101010 ; cb2 pulse output cb1 neg edge ca2 pulse output ca1 neg edge
	sta VIA1_PCR
	lda #%01111111
	sta VIA1_IER ; disable other interrupts
	lda #%10000010 ; ca1 on 
	sta VIA1_IER
	stz cmdready
	stz inputindex
	stz inputindex + 1
	cli

loop:
	lda cmdready
	beq loop
	jsr parsecmd
	jmp loop

	;; should be fine not to sei so long as terminal code doesn't do anything foolish
parsecmd:
	PUT teststr
	jsr parallel_puts
	PUT inputbuf
	jsr parallel_puts
	stz cmdready
	rts
	
	;; ( char - )
parallel_putc:
	lda 0, x
	sta VIA1_PB
	DROP
	lda #$10 ;cb1 bit
@busy:
	bit VIA1_IFR
	bne @busy ; wait for data taken signal from peripheral
	PUT $01ff
	jsr DELAY ; needs a delay so arduino can catch up
	rts

	;;( strptr - )
parallel_puts:	
	lda (0, x)
	beq @quit ; zero terminator
	PUT_A
	jsr parallel_putc
	jsr INC_16
	jmp parallel_puts
@quit:
	DROP
	rts

	.segment "RESETVEC"
	.word $0000 ; nmi
	.word start ; reset
	.word irq   ; irq
