	VIA1 = $9000
	VIA1_PB = VIA1
	VIA1_PA = VIA1 + 1
	VIA1_DDRB = VIA1 + 2
	VIA1_DDRA = VIA1 + 3
	VIA1_T1C_L = VIA1 + 4
	VIA1_T1C_H = VIA1 + 5
	VIA1_T1L_L = VIA1 + 6
	VIA1_T1L_H = VIA1 + 7
	VIA1_T2C_L = VIA1 + 8
	VIA1_T2C_H = VIA1 + 9
	VIA1_SR = VIA1 + 10
	VIA1_ACR = VIA1 + 11
	VIA1_PCR = VIA1 + 12
	VIA1_IFR = VIA1 + 13
	VIA1_IER = VIA1 + 14
	VIA1_PA_NO_HS = VIA1 + 15
	
	VIA2 = $8800
	VIA2_PB = VIA2
	VIA2_PA = VIA2 + 1
	VIA2_DDRB = VIA2 + 2
	VIA2_DDRA = VIA2 + 3
	VIA2_T1C_L = VIA2 + 4
	VIA2_T1C_H = VIA2 + 5
	VIA2_T1L_L = VIA2 + 6
	VIA2_T1L_H = VIA2 + 7
	VIA2_T2C_L = VIA2 + 8
	VIA2_T2C_H = VIA2 + 9
	VIA2_SR = VIA2 + 10
	VIA2_ACR = VIA2 + 11
	VIA2_PCR = VIA2 + 12
	VIA2_IFR = VIA2 + 13
	VIA2_IER = VIA2 + 14
	VIA2_PA_NO_HS = VIA2 + 15

	.segment "CODE"
	jmp start
	.include "7seg.asm"

	ROMSTART = $e000
	IOSTART = $8000

	ctr1 = $0000
	ctr2 = $0004

start:	
	lda #$ff
	sta VIA1_DDRA
	tax
	stx ctr1
	stx ctr2
	inx			;now zero
loopouter:
	lda sev_seg_table, x
	sta VIA1_PA
loop1:
	dec ctr1
	bne loop1
loop2:
	dec ctr1		;back to ff
	dec ctr2
	bne loop1
	dec ctr2		;back to ff
	inx
	cpx #$10
	bcc loopouter
	lda #$00
	tax
	jmp loopouter

	.segment "RESETVEC"
nmi:	.word $0000
reset:	.word start
irq:	.word $0000
