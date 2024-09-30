	.include "hardware.inc"

	.segment "CODE"
	tv_idle = $2000 	; 1 byte

	jmp init

	.include "protoforth.asm"
teststr:
	.byte "peepee poopoo", 0
	
init:
	ldx #$ff
	txs
	lda VIA2_DDRB
	and #$7f
	sta VIA2_DDRB 		; bit 7 input
	lda VIA2_ACR
	and #$e3
	ora #$18
	sta VIA2_ACR		; shift out under control of phi2
	lda #$04
	sta VIA2_IER		; disable shift register interrupts
	stz tv_idle

main:
	PUT $200
	jsr DELAY 		; wait for avr to warm up
	lda #$0c		; form feed
	jsr tv_output
	lda 'X'
	jsr tv_output
	PUT teststr
	jsr tv_puts
loop:
	jmp loop

tv_output:
	pha
	lda tv_idle
	eor #$80
	sta tv_idle
@1:
	lda VIA2_PB
	eor tv_idle
	bpl @1 			; wait until avr is idle and ready
	pla
	sta VIA2_SR		; shift out command/data
	rts

tv_puts:
	lda (0, x)
	beq @quit		; zero terminator
	jsr tv_output
	jsr INC_16
	jmp tv_puts
@quit:
	DROP
	rts

	.segment "RESETVEC"
	.word $0000, init, $0000
