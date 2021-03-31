*************************************************************************
*									*
*	Z-80 Emulator for MC68000					*
*									*
*	This file contains the target processor (Z-80)			*
*	emulation routines.						*
*									*
*************************************************************************
* Last revised January 9, 1989.

	xdef	optabl,flags,mloop,mloopt,tracesad,traceead,traceflg,start2
	xref	illegl,service,dump,inp,outp,movear

	include	"options.i"
	include "ecpmdefs.i"
	page
*************************************************************************
*									*
*	Macros used by opcode simulation routines			*
*									*
*	These generate a lot of repetitive code sequences,		*
*	but it's faster than doing subroutine calls.			*
*									*
*************************************************************************

jrd0	macro			;Jump relative (add D0 to program counter).
	ext.w	d0
	add.w	d0,pseudopc
	jmp	(return)
	endm

setflag macro			;Set Z-80 flags from 68000 flags.
	ifeq	x680x0		;68000 only
	move	sr,d1
	endc
	ifne	x680x0		;68010 and higher
	dc.w	$42C1		;MOVE CCR,D1
	endc
	and.w	regcon0f,d1
	move.b	0(flagptr,d1.w),regf
	ifc	'\1','SETONLY'
	mexit
	endc
	ifnc	'\1',''
	move.b	d0,\1
	endc
	jmp	(return)
	endm

inrflag macro			;Set flags after increment or decrement.
	ifeq	x680x0		;68000 only
	move	sr,d0
	endc
	ifne	x680x0		;68010 and higher
	dc.w	$42C0		;MOVE CCR,D0
	endc
	and.w	regcon0e,d0
	and.w	regcon01,regf
	or.b	0(flagptr,d0.w),regf
	jmp	(return)
	endm

lsxxd	macro
	move.b	1(pseudopc),d0
	rol.w	#8,d0
	move.b	(pseudopc),d0
	addq.l	#2,pseudopc
	move.l	d0,a0
	adda.l	targbase,a0
	endm

addflag macro			;Add and set flags.
	move.b	d0,regop1(regs)
	move.b	rega,regop2(regs)
	move.b	regcon0e,regop3(regs)
	add.b	d0,rega
	setflag
	endm

adcflag macro			;Add with carry and set flags.
	move.b	d0,regop1(regs)
	move.b	rega,regop2(regs)
	moveq	#0,d1		;Set Zero flag for ADDX.
	addx.b	d0,rega
	setflag
	endm

sbbflag macro			;Subtract with borrow and set flags.
	moveq	#0,d1		;Set Zero flag for SUBX.
	subx.b	d0,rega
	setflag
	endm

ashl	macro			;16-bit add to or subtract from HL, set flags
	move.w	reg\2(regs),d0
	move.w	regh(regs),d1
	asr.b	#1,regf 	;Put simulated carry flag in 68000's X flag.
	ori	#4,ccr		;Set the 68000's Z flag for ADDX/SUBX.
	\1x.w	d0,d1
	ifeq	x680x0		;68000 only
	move	sr,d0
	endc
	ifne	x680x0		;68010 and higher
	dc.w	$42C0		;MOVE CCR,D0
	endc
	and.w	regcon0f,d0
	move.b	0(flagptr,d0.w),regf
	move.w	d1,regh(regs)
	jmp	(return)
	endm

ret80	macro			;Return from subroutine.
	move.b	1(pseudosp),d0
	rol.w	#8,d0
	move.b	(pseudosp),d0
	addq.l	#2,pseudosp
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)
	endm

jmpaddr macro			;Get address for possible jump or call.
	move.b	1(pseudopc),d0
	rol.w	#8,d0
	move.b	(pseudopc),d0
	addq.l	#2,pseudopc
	endm

call80	macro			;Call a Z-80 subroutine.
	move.l	pseudopc,d1
	sub.l	targbase,d1
	move.b	d1,-2(pseudosp)
	rol.w	#8,d1
	move.b	d1,-1(pseudosp)
	subq.l	#2,pseudosp
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)
	endm

rst80	macro			;Z-80 RST instruction
	move.l	pseudopc,d1
	sub.l	targbase,d1
	move.b	d1,-2(pseudosp)
	rol.w	#8,d1
	move.b	d1,-1(pseudosp)
	subq.l	#2,pseudosp
	lea	\1(targbase),pseudopc
	jmp	(return)
	endm

docyf	macro			;Copy 68000's carry flag to Z-80.
	bcs.s	1$
	bclr	#0,regf
	jmp	(return)
1$	bset	#0,regf
	jmp	(return)
	endm

dozf	macro			;Copy 68000's zero flag to Z-80.
	beq.s	1$
	bclr	#6,regf
	jmp	(return)
1$	bset	#6,regf
	jmp	(return)
	endm

calcind macro			;Calculate (IX+d) or (IY+d).
	moveq	#0,d0
	move.b	(pseudopc)+,d0
	add.w	regi\1(regs),d0
	endm
	page
*************************************************************************
*									*
*	Opcode dispatcher						*
*									*
*************************************************************************

	code
start2:

* Instruction trace routine (only executed if RETURN points here).
mloopt	tst.b	traceflg	;Is tracing active?
	bne.s	dotrace 	;No.
	cmpa.l	tracesad,pseudopc ;Start tracing here?
	bne.s	mloop		;No.
	move.b	#1,traceflg	;Set trace flag.
dotrace jsr	dump		;Dump current instruction information.
	cmpa.l	traceead,pseudopc ;End tracing here?
	bne.s	mloop		;No.
	clr.b	traceflg	;Reset trace flag.

* Instruction simulation routine
mloop	moveq	#0,d0		;Execute appropriate simulation subroutine.
	move.b	(pseudopc)+,d0	;Grab the next opcode.
	asl	#2,d0		;(D0 high word is still zero!)
	move.l	0(opptr,d0.w),a0
	jmp	(a0)		;Do the subroutine.
	page
*************************************************************************
*									*
*	Opcode simulation routines					*
*									*
*	Note:  I/O instructions are based as 68000 address $FF0000	*
*		as is appropriate for the Compupro CPU-68K card.	*
*									*
*	Also, all routines assume that the high word of D0 is zero!	*
*									*
*************************************************************************

	even

nop00	jmp	(return)		;00 NOP

lxib	move.b	(pseudopc)+,regc(regs)	;01 LXI B,nnnn
	move.b	(pseudopc)+,regb(regs)
	jmp	(return)

staxb	move.w	regb(regs),d0		;02 STAX B
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

inxb	addq.w	#1,regb(regs)		;03 INX B
	jmp	(return)

inrb	addq.b	#1,regb(regs)		;04 INR B
	inrflag

dcrb	subq.b	#1,regb(regs)		;05 DCR B
	inrflag

mvib	move.b	(pseudopc)+,regb(regs)	;06 MVI B,nn
	jmp	(return)

rlc	rol.b	#1,rega 		;07 RLC
	docyf

exaf	move.b	rega,d0 		;08 EXAF (Z-80)
	move.b	rega2(regs),rega
	move.b	d0,rega2(regs)
	move.b	regf,d0
	move.b	regf2(regs),regf
	move.b	d0,regf2(regs)
	jmp	(return)

dadb	move.w	regb(regs),d0		;09 DAD B
	add.w	d0,regh(regs)
	docyf

ldaxb	move.w	regb(regs),d0		;0A LDAX B
	move.b	0(targbase,d0.l),rega
	jmp	(return)

dcxb	subq.w	#1,regb(regs)		;0B DCX B
	jmp	(return)

inrc	addq.b	#1,regc(regs)		;0C INR C
	inrflag

dcrc	subq.b	#1,regc(regs)		;0D DCR C
	inrflag

mvic	move.b	(pseudopc)+,regc(regs)	;0E MVI C,nn
	jmp	(return)

rrc	ror.b	#1,rega 		;0F RRC
	docyf

djnz	move.b	(pseudopc)+,d0		;10 DJNZ dd (Z-80)
	subq.b	#1,regb(regs)
	beq.s	lxidx
	jrd0

lxid	move.b	(pseudopc)+,rege(regs)	;11 LXI D,nnnn
	move.b	(pseudopc)+,regd(regs)
lxidx	jmp	(return)

staxd	move.w	regd(regs),d0		;12 STAX D
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

inxd	addq.w	#1,regd(regs)		;13 INX D
	jmp	(return)

inrd	addq.b	#1,regd(regs)		;14 INR D
	inrflag

dcrd	subq.b	#1,regd(regs)		;15 DCR D
	inrflag

mvid	move.b	(pseudopc)+,regd(regs)	;16 MVI D,nn
	jmp	(return)

ral	roxr.b	#1,regf 		;17 RAL
	roxl.b	#1,rega
	roxl.b	#1,regf
	jmp	(return)

jr	move.b	(pseudopc)+,d0		;18 JR dd (Z-80)
	jrd0

dadd	move.w	regd(regs),d0		;19 DAD D
	add.w	d0,regh(regs)
	docyf

ldaxd	move.w	regd(regs),d0		;1A LDAX D
	move.b	0(targbase,d0.l),rega
	jmp	(return)

dcxd	subq.w	#1,regd(regs)		;1B DCX D
	jmp	(return)

inre	addq.b	#1,rege(regs)		;1C INR E
	inrflag

dcre	subq.b	#1,rege(regs)		;1D DCR E
	inrflag

mvie	move.b	(pseudopc)+,rege(regs)	;1E MVI E,nn
	jmp	(return)

rar	roxr.b	#1,regf 		;1F RAR
	roxr.b	#1,rega
	roxl.b	#1,regf
	jmp	(return)

jrnz	move.b	(pseudopc)+,d0		;20 JRNZ dd (Z-80)
	btst	#6,regf
	bne.s	lxihx
	jrd0

lxih	move.b	(pseudopc)+,regl(regs)	;21 LXI H,nnnn
	move.b	(pseudopc)+,regh(regs)
lxihx	jmp	(return)

shld	lsxxd				;22 SHLD addr
	move.b	regl(regs),(a0)+
	move.b	regh(regs),(a0)
	jmp	(return)

inxh	addq.w	#1,regh(regs)		;23 INX H
	jmp	(return)

inrh	addq.b	#1,regh(regs)		;24 INR H
	inrflag

dcrh	subq.b	#1,regh(regs)		;25 DCR H
	inrflag

mvih	move.b	(pseudopc)+,regh(regs)	;26 MVI H,nn
	jmp	(return)

daa	move.b	regop3(regs),d0 	;27 DAA
	roxr.b	#1,d0
	move.b	regop2(regs),d0
	move.b	regop1(regs),d1
	swap	regcon0e
	move.b	rega,regcon0e
	and.b	regcon0f,regcon0e
	cmp.b	#9,regcon0e
	bhi.s	halfcy
	and.b	regcon0f,d0
	and.b	regcon0f,d1
	ori.b	#$F0,d1
	addx.b	d0,d1
	bcc.s	nohalf
halfcy	add.b	#6,rega
	bcs.s	fullcy
nohalf	btst	#0,regf
	bne.s	fullcy
	move.b	rega,regcon0e
	and.b	#$F0,regcon0e
	cmp.b	#$90,regcon0e
	bls.s	nofull
fullcy	add.b	#$60,rega
	ori	#1,ccr
	ifeq	x680x0		;68000 only
enddaa	move	sr,regf
	endc
	ifne	x680x0		;68010 and higher
enddaa	dc.w	$42C3		;MOVE CCR,D3
	endc
	swap	regcon0e
	and.w	regcon0f,regf
	move.b	0(flagptr,regf.w),regf
	jmp	(return)
nofull	tst.b	rega
	bra.s	enddaa

jrz	move.b	(pseudopc)+,d0		;28 JRZ dd (Z-80)
	btst	#6,regf
	beq.s	lhldx
	jrd0

dadh	asl.w	regh(regs)		;29 DAD H (multiply by 2)
	docyf

lhld	lsxxd				;2A LHLD addr
	move.b	(a0)+,regl(regs)
	move.b	(a0),regh(regs)
lhldx	jmp	(return)

dcxh	subq.w	#1,regh(regs)		;2B DCX H
	jmp	(return)

inrl	addq.b	#1,regl(regs)		;2C INR L
	inrflag

dcrl	subq.b	#1,regl(regs)		;2D DCR L
	inrflag

mvil	move.b	(pseudopc)+,regl(regs)	;2E MVI L,nn
	jmp	(return)

cma	not.b	rega			;2F CMA
	jmp	(return)

jrnc	move.b	(pseudopc)+,d0		;30 JRNC dd (Z-80)
	btst	#0,regf
	bne.s	lxisx
	jrd0

lxis	move.b	1(pseudopc),d0		;31 LXI SP,nnnn
	rol.w	#8,d0
	move.b	(pseudopc),d0
	addq.l	#2,pseudopc
	move.l	d0,pseudosp
	adda.l	targbase,pseudosp
lxisx	jmp	(return)

sta	move.b	1(pseudopc),d0		;32 STA addr
	rol.w	#8,d0
	move.b	(pseudopc),d0
	addq.l	#2,pseudopc
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

inxs	addq.l	#1,pseudosp		;33 INX SP
	jmp	(return)

inrm	move.w	regh(regs),d0		;34 INR M
	addq.b	#1,0(targbase,d0.l)
	inrflag

dcrm	move.w	regh(regs),d0		;35 DCR M
	subq.b	#1,0(targbase,d0.l)
	inrflag

mvim	move.w	regh(regs),d0		;36 MVI M,nn
	move.b	(pseudopc)+,0(targbase,d0.l)
	jmp	(return)

stc	bset	#0,regf 		;37 STC
	jmp	(return)

jrc	move.b	(pseudopc)+,d0		;38 JRC dd (Z-80)
	btst	#0,regf
	beq.s	lxisx
	jrd0

dads	move.l	pseudosp,d0		;39 DAD SP
	sub.l	targbase,d0
	add.w	d0,regh(regs)
	docyf

lda	move.b	1(pseudopc),d0		;3A LDA addr
	rol.w	#8,d0
	move.b	(pseudopc),d0
	addq.l	#2,pseudopc
	move.b	0(targbase,d0.l),rega
	jmp	(return)

dcxs	subq.l	#1,pseudosp		;3B DCX SP
	jmp	(return)

inra	move.b	rega,regop1(regs)	;3C INR A
	move.b	regcon01,regop2(regs)
	move.b	regcon0e,regop3(regs)
	addq.b	#1,rega
	inrflag

dcra	subq.b	#1,rega 		;3D DCR A
	inrflag

mvia	move.b	(pseudopc)+,rega	;3E MVI A,nn
	jmp	(return)

cmc	bchg	#0,regf 		;3F CMC
	jmp	(return)

movebb	move.b	regb(regs),regb(regs)	;40 MOV B,B
	jmp	(return)

movebc	move.b	regc(regs),regb(regs)	;41 MOV B,C
	jmp	(return)

movebd	move.b	regd(regs),regb(regs)	;42 MOV B,D
	jmp	(return)

movebe	move.b	rege(regs),regb(regs)	;43 MOV B,E
	jmp	(return)

movebh	move.b	regh(regs),regb(regs)	;44 MOV B,H
	jmp	(return)

movebl	move.b	regl(regs),regb(regs)	;45 MOV B,L
	jmp	(return)

movebm	move.w	regh(regs),d0		;46 MOV B,M
	move.b	0(targbase,d0.l),regb(regs)
	jmp	(return)

moveba	move.b	rega,regb(regs) 	;47 MOV B,A
	jmp	(return)

movecb	move.b	regb(regs),regc(regs)	;48 MOV C,B
	jmp	(return)

movecc	move.b	regc(regs),regc(regs)	;49 MOV C,C
	jmp	(return)

movecd	move.b	regd(regs),regc(regs)	;4A MOV C,D
	jmp	(return)

movece	move.b	rege(regs),regc(regs)	;4B MOV C,E
	jmp	(return)

movech	move.b	regh(regs),regc(regs)	;4C MOV C,H
	jmp	(return)

movecl	move.b	regl(regs),regc(regs)	;4D MOV C,L
	jmp	(return)

movecm	move.w	regh(regs),d0		;4E MOV C,M
	move.b	0(targbase,d0.l),regc(regs)
	jmp	(return)

moveca	move.b	rega,regc(regs) 	;4F MOV C,A
	jmp	(return)

movedb	move.b	regb(regs),regd(regs)	;50 MOV D,B
	jmp	(return)

movedc	move.b	regc(regs),regd(regs)	;51 MOV D,C
	jmp	(return)

movedd	move.b	regd(regs),regd(regs)	;52 MOV D,D
	jmp	(return)

movede	move.b	rege(regs),regd(regs)	;53 MOV D,E
	jmp	(return)

movedh	move.b	regh(regs),regd(regs)	;54 MOV D,H
	jmp	(return)

movedl	move.b	regl(regs),regd(regs)	;55 MOV D,L
	jmp	(return)

movedm	move.w	regh(regs),d0		;56 MOV D,M
	move.b	0(targbase,d0.l),regd(regs)
	jmp	(return)

moveda	move.b	rega,regd(regs) 	;57 MOV D,A
	jmp	(return)

moveeb	move.b	regb(regs),rege(regs)	;58 MOV E,B
	jmp	(return)

moveec	move.b	regc(regs),rege(regs)	;59 MOV E,C
	jmp	(return)

moveed	move.b	regd(regs),rege(regs)	;5A MOV E,D
	jmp	(return)

moveee	move.b	rege(regs),rege(regs)	;5B MOV E,E
	jmp	(return)

moveeh	move.b	regh(regs),rege(regs)	;5C MOV E,H
	jmp	(return)

moveel	move.b	regl(regs),rege(regs)	;5D MOV E,L
	jmp	(return)

moveem	move.w	regh(regs),d0		;5E MOV E,M
	move.b	0(targbase,d0.l),rege(regs)
	jmp	(return)

moveea	move.b	rega,rege(regs) 	;5F MOV E,A
	jmp	(return)

movehb	move.b	regb(regs),regh(regs)	;60 MOV H,B
	jmp	(return)

movehc	move.b	regc(regs),regh(regs)	;61 MOV H,C
	jmp	(return)

movehd	move.b	regd(regs),regh(regs)	;62 MOV H,D
	jmp	(return)

movehe	move.b	rege(regs),regh(regs)	;63 MOV H,E
	jmp	(return)

movehh	move.b	regh(regs),regh(regs)	;64 MOV H,H
	jmp	(return)

movehl	move.b	regl(regs),regh(regs)	;65 MOV H,L
	jmp	(return)

movehm	move.w	regh(regs),d0		;66 MOV H,M
	move.b	0(targbase,d0.l),regh(regs)
	jmp	(return)

moveha	move.b	rega,regh(regs) 	;67 MOV H,A
	jmp	(return)

movelb	move.b	regb(regs),regl(regs)	;68 MOV L,B
	jmp	(return)

movelc	move.b	regc(regs),regl(regs)	;69 MOV L,C
	jmp	(return)

moveld	move.b	regd(regs),regl(regs)	;6A MOV L,D
	jmp	(return)

movele	move.b	rege(regs),regl(regs)	;6B MOV L,E
	jmp	(return)

movelh	move.b	regh(regs),regl(regs)	;6C MOV L,H
	jmp	(return)

movell	move.b	regl(regs),regl(regs)	;6D MOV L,L
	jmp	(return)

movelm	move.w	regh(regs),d0		;6E MOV L,M
	move.b	0(targbase,d0.l),regl(regs)
	jmp	(return)

movela	move.b	rega,regl(regs) 	;6F MOV L,A
	jmp	(return)

movemb	move.w	regh(regs),d0		;70 MOV M,B
	move.b	regb(regs),0(targbase,d0.l)
	jmp	(return)

movemc	move.w	regh(regs),d0		;71 MOV M,C
	move.b	regc(regs),0(targbase,d0.l)
	jmp	(return)

movemd	move.w	regh(regs),d0		;72 MOV M,D
	move.b	regd(regs),0(targbase,d0.l)
	jmp	(return)

moveme	move.w	regh(regs),d0		;73 MOV M,E
	move.b	rege(regs),0(targbase,d0.l)
	jmp	(return)

movemh	move.w	regh(regs),d0		;74 MOV M,H
	move.b	regh(regs),0(targbase,d0.l)
	jmp	(return)

moveml	move.w	regh(regs),d0		;75 MOV M,L
	move.b	regl(regs),0(targbase,d0.l)
	jmp	(return)

halt	jsr	service 		;76 HLT
	jmp	(return)

movema	move.w	regh(regs),d0		;77 MOV M,A
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

moveab	move.b	regb(regs),rega 	;78 MOV A,B
	jmp	(return)

moveac	move.b	regc(regs),rega 	;79 MOV A,C
	jmp	(return)

movead	move.b	regd(regs),rega 	;7A MOV A,D
	jmp	(return)

moveae	move.b	rege(regs),rega 	;7B MOV A,E
	jmp	(return)

moveah	move.b	regh(regs),rega 	;7C MOV A,H
	jmp	(return)

moveal	move.b	regl(regs),rega 	;7D MOV A,L
	jmp	(return)

moveam	move.w	regh(regs),d0		;7E MOV A,M
	move.b	0(targbase,d0.l),rega
	jmp	(return)

moveaa	jmp	(return)		;7F MOV A,A

addb	move.b	regb(regs),d0		;80 ADD B
	addflag

addc	move.b	regc(regs),d0		;81 ADD C
	addflag

addd	move.b	regd(regs),d0		;82 ADD D
	addflag

adde	move.b	rege(regs),d0		;83 ADD E
	addflag

addh	move.b	regh(regs),d0		;84 ADD H
	addflag

addl	move.b	regl(regs),d0		;85 ADD L
	addflag

addm	move.w	regh(regs),d0		;86 ADD M
	move.b	0(targbase,d0.l),d0
	addflag

adda	move.b	rega,regop1(regs)	;87 ADD A
	move.b	rega,regop2(regs)
	move.b	regcon0e,regop3(regs)
	add.b	rega,rega
	setflag

adcb	move.b	regf,regop3(regs)	;88 ADC B
	asr.b	#1,regf
	move.b	regb(regs),d0
	adcflag

adcc	move.b	regf,regop3(regs)	;89 ADC C
	asr.b	#1,regf
	move.b	regc(regs),d0
	adcflag

adcd	move.b	regf,regop3(regs)	;8A ADC D
	asr.b	#1,regf
	move.b	regd(regs),d0
	adcflag

adce	move.b	regf,regop3(regs)	;8B ADC E
	asr.b	#1,regf
	move.b	rege(regs),d0
	adcflag

adch	move.b	regf,regop3(regs)	;8C ADC H
	asr.b	#1,regf
	move.b	regh(regs),d0
	adcflag

adcl	move.b	regf,regop3(regs)	;8D ADC L
	asr.b	#1,regf
	move.b	regl(regs),d0
	adcflag

adcm	move.b	regf,regop3(regs)	;8E ADC M
	move.w	regh(regs),d0
	move.l	d0,a0
	adda.l	targbase,a0
	asr.b	#1,regf
	move.b	(a0),d0
	adcflag

adca	move.b	regf,regop3(regs)	;8F ADC A
	asr.b	#1,regf
	move.b	rega,d0
	adcflag

subb	sub.b	regb(regs),rega 	;90 SUB B
	setflag

subc	sub.b	regc(regs),rega 	;91 SUB C
	setflag

subd	sub.b	regd(regs),rega 	;92 SUB D
	setflag

sube	sub.b	rege(regs),rega 	;93 SUB E
	setflag

subh	sub.b	regh(regs),rega 	;94 SUB H
	setflag

subl	sub.b	regl(regs),rega 	;95 SUB L
	setflag

subm	move.w	regh(regs),d0		;96 SUB M
	sub.b	0(targbase,d0.l),rega
	setflag

suba	sub.b	rega,rega		;97 SUB A
	setflag

sbbb	asr.b	#1,regf 		;98 SBB B
	move.b	regb(regs),d0
	sbbflag

sbbc	asr.b	#1,regf 		;99 SBB C
	move.b	regc(regs),d0
	sbbflag

sbbd	asr.b	#1,regf 		;9A SBB D
	move.b	regd(regs),d0
	sbbflag

sbbe	asr.b	#1,regf 		;9B SBB E
	move.b	rege(regs),d0
	sbbflag

sbbh	asr.b	#1,regf 		;9C SBB H
	move.b	regh(regs),d0
	sbbflag

sbbl	asr.b	#1,regf 		;9D SBB L
	move.b	regl(regs),d0
	sbbflag

sbbm	move.w	regh(regs),d0		;9E SBB M
	move.l	d0,a0
	adda.l	targbase,a0
	asr.b	#1,regf
	move.b	(a0),d0
	sbbflag

sbba	asr.b	#1,regf 		;9F SBB A
	move.b	rega,d0
	sbbflag

andb	and.b	regb(regs),rega 	;A0 ANA B
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

andc	and.b	regc(regs),rega 	;A1 ANA C
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

andd	and.b	regd(regs),rega 	;A2 ANA D
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

ande	and.b	rege(regs),rega 	;A3 ANA E
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

andh	and.b	regh(regs),rega 	;A4 ANA H
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

andl	and.b	regl(regs),rega 	;A5 ANA L
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

andm	move.w	regh(regs),d0		;A6 ANA M
	and.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

anda	move.b	16(flagptr,rega.w),regf ;A7 ANA A
	jmp	(return)

xrab	move.b	regb(regs),d0		;A8 XRA B
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrac	move.b	regc(regs),d0		;A9 XRA C
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrad	move.b	regd(regs),d0		;AA XRA D
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrae	move.b	rege(regs),d0		;AB XRA E
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrah	move.b	regh(regs),d0		;AC XRA H
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xral	move.b	regl(regs),d0		;AD XRA L
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xram	move.w	regh(regs),d0		;AE XRA M
	move.b	0(targbase,d0.l),d0
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xraa	moveq	#0,rega 		;AF XRA A (clears accumulator)
	move.b	16(flagptr),regf
	jmp	(return)

orab	or.b	regb(regs),rega 	;B0 ORA B
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orac	or.b	regc(regs),rega 	;B1 ORA C
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orad	or.b	regd(regs),rega 	;B2 ORA D
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orae	or.b	rege(regs),rega 	;B3 ORA E
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orah	or.b	regh(regs),rega 	;B4 ORA H
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oral	or.b	regl(regs),rega 	;B5 ORA L
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oram	move.w	regh(regs),d0		;B6 ORA M
	or.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oraa	move.b	16(flagptr,rega.w),regf ;B7 ORA A
	jmp	(return)

cmpb	cmp.b	regb(regs),rega 	;B8 CMP B
	setflag

cmpc	cmp.b	regc(regs),rega 	;B9 CMP C
	setflag

cmpd	cmp.b	regd(regs),rega 	;BA CMP D
	setflag

cmpe	cmp.b	rege(regs),rega 	;BB CMP E
	setflag

cmph	cmp.b	regh(regs),rega 	;BC CMP H
	setflag

cmpl	cmp.b	regl(regs),rega 	;BD CMP L
	setflag

cmpam	move.w	regh(regs),d0		;BE CMP M
	cmp.b	0(targbase,d0.l),rega
	setflag

cmpaa	cmp.b	rega,rega		;BF CMP A
	setflag

rnz	btst	#6,regf 		;C0 RNZ
	bne.s	popbx
	ret80

popb	move.b	(pseudosp)+,regc(regs)	;C1 POP B
	move.b	(pseudosp)+,regb(regs)
popbx	jmp	(return)

jnz	jmpaddr 			;C2 JNZ addr
	btst	#6,regf
	bne.s	jnzx
	lea	0(targbase,d0.l),pseudopc
jnzx	jmp	(return)

jmpa	jmpaddr 			;C3 JMP addr
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)

cnz	jmpaddr 			;C4 CNZ addr
	btst	#6,regf
	bne.s	jnzx
	call80

pushb	move.b	regb(regs),-(pseudosp)	;C5 PUSH B
	move.b	regc(regs),-(pseudosp)
	jmp	(return)

adi	move.b	(pseudopc)+,d0		;C6 ADI nn
	addflag

rst0	rst80	$0			;C7 RST 0

rz	btst	#6,regf 		;C8 RZ
	beq.s	jzx
ret	ret80				;C9 RET

jz	jmpaddr 			;CA JZ addr
	btst	#6,regf
	beq.s	jzx
	lea	0(targbase,d0.l),pseudopc
jzx	jmp	(return)

* CB-prefix Z-80 instructions are simulated in the next module.

cz	jmpaddr 			;CC CZ addr
	btst	#6,regf
	beq.s	jzx
	call80

call	jmpaddr 			;CD CALL addr
	call80

aci	move.b	regf,regop3(regs)	;CE ACI nn
	asr.b	#1,regf
	move.b	(pseudopc)+,d0
	adcflag

rst1	rst80	$8			;CF RST 1

rnc	btst	#0,regf 		;D0 RNC
	bne.s	jncx
	ret80

popd	move.b	(pseudosp)+,rege(regs)	;D1 POP D
	move.b	(pseudosp)+,regd(regs)
	jmp	(return)

jnc	jmpaddr 			;D2 JNC addr
	btst	#0,regf
	bne.s	jncx
	lea	0(targbase,d0.l),pseudopc
jncx	jmp	(return)

out	moveq	#0,d0			;D3 OUT nn
	move.b	(pseudopc)+,d0	;Port number
	lea	tmprega,a0	;Simulated accumulator is in a 68000 register -
	move.b	rega,(a0)	; move it to a memory location.
	jsr	outp		;Send the byte.
	jmp	(return)

cnc					;D4 CNC addr
	jmpaddr
	btst	#0,regf
	bne.s	jncx
	call80

pushd	move.b	regd(regs),-(pseudosp)	;D5 PUSH D
	move.b	rege(regs),-(pseudosp)
	jmp	(return)

sui	move.b	(pseudopc)+,d0		;D6 SUI nn
	sub.b	d0,rega
	setflag

rst2	rst80	$10			;D7 RST 2

rc	btst	#0,regf 		;D8 RC
	beq.s	jcx
	ret80

exx	move.l	regb(regs),d0		;D9 EXX (Z-80)
	move.l	regb2(regs),regb(regs)
	move.l	d0,regb2(regs)
	move.w	regh(regs),d0
	move.w	regh2(regs),regh(regs)
	move.w	d0,regh2(regs)
	jmp	(return)

jc	jmpaddr 			;DA JC addr
	btst	#0,regf
	beq.s	jcx
	lea	0(targbase,d0.l),pseudopc
jcx	jmp	(return)

in	moveq	#0,d0			;DB IN nn
	move.b	(pseudopc)+,d0	;Port number
	lea	tmprega,a0	;Get the byte here for now
	jsr	inp
	move.b	tmprega,rega	;Move byte to accumulator (real register!)
	jmp	(return)

cc	jmpaddr 			;DC CC addr
	btst	#0,regf
	beq.s	jpox
	call80

* DD-prefix Z-80 instructions are simulated in the next module.

sbi	asr.b	#1,regf 		;DE SBI nn
	move.b	(pseudopc)+,d0
	sbbflag

rst3	rst80	$18			;DF RST 3

rpo	btst	#2,regf 		;E0 RPO
	bne.s	jpox
	ret80

poph	move.b	(pseudosp)+,regl(regs)	;E1 POP H
	move.b	(pseudosp)+,regh(regs)
	jmp	(return)

jpo	jmpaddr 			;E2 JPO addr
	btst	#2,regf
	bne.s	jpox
	lea	0(targbase,d0.l),pseudopc
jpox	jmp	(return)

xthl	move.b	regl(regs),d0		;E3 XTHL
	move.b	(pseudosp),regl(regs)
	move.b	d0,(pseudosp)
	move.b	regh(regs),d0
	move.b	1(pseudosp),regh(regs)
	move.b	d0,1(pseudosp)
	jmp	(return)

cpo	jmpaddr 			;E4 CPO addr
	btst	#2,regf
	bne.s	jpox
	call80

pushh	move.b	regh(regs),-(pseudosp)	;E5 PUSH H
	move.b	regl(regs),-(pseudosp)
	jmp	(return)

ani	and.b	(pseudopc)+,rega	;E6 ANI nn
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

rst4	rst80	$20			;E7 RST 4

rpe	btst	#2,regf 		;E8 RPE
	beq.s	jpex
	ret80

pchl	move.w	regh(regs),d0		;E9 PCHL
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)

jpe	jmpaddr 			;EA JPE addr
	btst	#2,regf
	beq.s	jpex
	lea	0(targbase,d0.l),pseudopc
jpex	jmp	(return)

xchg	move.w	regd(regs),d0		;EB XCHG
	move.w	regh(regs),regd(regs)
	move.w	d0,regh(regs)
	jmp	(return)

cpe	jmpaddr 			;EC CPE addr
	btst	#2,regf
	beq.s	jpex
	call80

* ED-prefix Z-80 instructions are simulated in the next module.

xri	move.b	(pseudopc)+,d0		;EE XRI nn
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

rst5	rst80	$28			;EF RST 5

rp	btst	#7,regf 		;F0 RP
	bne.s	jpx
	ret80

popp	move.b	(pseudosp)+,regf	;F1 POP P
	move.b	(pseudosp)+,rega
	jmp	(return)

jp	jmpaddr 			;F2 JP addr
	btst	#7,regf
	bne.s	jpx
	lea	0(targbase,d0.l),pseudopc
jpx	jmp	(return)

di	jmp	(return)		;F3 DI (treated as no-op)

cp	jmpaddr 			;F4 CP addr
	btst	#7,regf
	bne.s	jpx
	call80

pushp	move.b	rega,-(pseudosp)	;F5 PUSH PSW
	move.b	regf,-(pseudosp)
	jmp	(return)

oria	or.b	(pseudopc)+,rega	;F6 ORI nn
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

rst6	rst80	$30			;F7 RST 6

rm	btst	#7,regf 		;F8 RM
	beq.s	jmx
	ret80

sphl	move.w	regh(regs),d0		;F9 SPHL
	lea	0(targbase,d0.l),pseudosp
	jmp	(return)

jm	jmpaddr 			;FA JM addr
	btst	#7,regf
	beq.s	jmx
	lea	0(targbase,d0.l),pseudopc
jmx	jmp	(return)

ei	jmp	(return)		;FB EI (treated as a no-op)

cm	jmpaddr 			;FC CM addr
	btst	#7,regf
	beq.s	jmx
	call80

* FD-prefix Z-80 instructions are simulated in the next module.

cpi	cmp.b	(pseudopc)+,rega	;FE CPI nn
	setflag

rst7	rst80	$38			;FF RST 7
	page
*************************************************************************
*									*
*	Z-80 opcode simulation routines 				*
*									*
*************************************************************************

preCB	moveq	#0,d1			;Zero-fill high bits.
	move.b	(pseudopc)+,d1		;Grab sub-opcode.
	asl	#2,d1
	lea	CBoptab,a0
	move.l	0(a0,d1.w),-(sp)	;Address of simulation routine
	rts				;Do it!

preDD	moveq	#0,d1
	move.b	(pseudopc)+,d1
	asl	#2,d1
	lea	DDoptab,a0
	move.l	0(a0,d1.w),-(sp)
	rts

prDDCB	calcind x			;Calculate operand address.
prDDCBs moveq	#0,d1
	move.b	(pseudopc)+,d1		;Sub-sub-opcode
	and.b	#7,d1
	cmp.b	#6,d1			;Is it valid?
	bne.s	ilgDDCB 		;No.
	move.b	-1(pseudopc),d1 	;Get the sub-sub-opcode again.
	lsr.b	#3,d1			;Kill low-order 3 bits.
	lsl.w	#2,d1			;Convert to longword displacement.
	lea	DDCBopt,a0
	move.l	0(a0,d1.w),-(sp)
	rts

preED	moveq	#0,d1
	move.b	(pseudopc)+,d1
	asl	#2,d1
	lea	EDoptab,a0
	move.l	0(a0,d1.w),-(sp)
	rts

preFD	moveq	#0,d1
	move.b	(pseudopc)+,d1
	asl	#2,d1
	lea	FDoptab,a0
	move.l	0(a0,d1.w),-(sp)
	rts

prFDCB	calcind y			;Calculate operand address.
	bra.s	prDDCBs 		;Now we can use DDCB routines.

ilgDDCB subq.l	#2,pseudopc	;Enter here for DDCB/FDCB prefix errors.	
illgED	move.l	(sp)+,d1	;Trash the address.
	subq.l	#1,pseudopc	;Fix pseudo-PC for ILLEGAL.
	jmp	illegl

rlcb	move.b	regb(regs),d0		;CB00 RLC B
	rol.b	#1,d0
	setflag	regb(regs)

rlcc	move.b	regc(regs),d0		;CB01 RLC C
	rol.b	#1,d0
	setflag	regc(regs)

rlcd	move.b	regd(regs),d0		;CB02 RLC D
	rol.b	#1,d0
	setflag	regd(regs)

rlce	move.b	rege(regs),d0		;CB03 RLC E
	rol.b	#1,d0
	setflag	rege(regs)

rlch	move.b	regh(regs),d0		;CB04 RLC H
	rol.b	#1,d0
	setflag	regh(regs)

rlcl	move.b	regl(regs),d0		;CB05 RLC L
	rol.b	#1,d0
	setflag	regl(regs)

rlcm	move.w	regh(regs),d0		;CB06 RLC M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	rol.b	#1,d0
	setflag (a0)

rlca	rol.b	#1,d0			;CB07 RLC A
	setflag

rrcb	move.b	regb(regs),d0		;CB08 RRC B
	ror.b	#1,d0
	setflag	regb(regs)

rrcc	move.b	regc(regs),d0		;CB09 RRC C
	ror.b	#1,d0
	setflag	regc(regs)

rrcd	move.b	regd(regs),d0		;CB0A RRC D
	ror.b	#1,d0
	setflag	regd(regs)

rrce	move.b	rege(regs),d0		;CB0B RRC E
	ror.b	#1,d0
	setflag	rege(regs)

rrch	move.b	regh(regs),d0		;CB0C RRC H
	ror.b	#1,d0
	setflag	regh(regs)

rrcl	move.b	regl(regs),d0		;CB0D RRC L
	ror.b	#1,d0
	setflag	regl(regs)

rrcm	move.w	regh(regs),d0		;CB0E RRC M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	ror.b	#1,d0
	setflag (a0)

rrca	ror.b	#1,rega			;CB0F RRC A
	setflag

rlrb	move.b	regb(regs),d0		;CB10 RL B
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag regb(regs)

rlrc	move.b	regc(regs),d0		;CB11 RL C
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag regc(regs)

rlrd	move.b	regd(regs),d0		;CB12 RL D
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag regd(regs)

rlre	move.b	rege(regs),d0		;CB13 RL E
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag rege(regs)

rlrh	move.b	regh(regs),d0		;CB14 RL H
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag regh(regs)

rlrl	move.b	regl(regs),d0		;CB15 RL L
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag regl(regs)

rlrm	move.w	regh(regs),d0		;CB16 RL M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	roxr.b	#1,regf
	roxl.b	#1,d0
	setflag (a0)

rlra	roxr.b	#1,regf			;CB17 RL A
	roxl.b	#1,rega
	setflag

rrrb	move.b	regb(regs),d0		;CB18 RR B
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag regb(regs)

rrrc	move.b	regc(regs),d0		;CB19 RR C
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag regc(regs)

rrrd	move.b	regd(regs),d0		;CB1A RR D
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag regd(regs)

rrre	move.b	rege(regs),d0		;CB1B RR E
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag rege(regs)

rrrh	move.b	regh(regs),d0		;CB1C RR H
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag regh(regs)

rrrl	move.b	regl(regs),d0		;CB1D RR L
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag regl(regs)

rrrm	move.w	regh(regs),d0		;CB1E RR M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	roxr.b	#1,regf
	roxr.b	#1,d0
	setflag (a0)

rrra	roxr.b	#1,regf			;CB1F RR A
	roxr.b	#1,rega
	setflag

slab	move.b	regb(regs),d0		;CB20 SLA B
	asl.b	#1,d0
	setflag	regb(regs)

slac	move.b	regc(regs),d0		;CB21 SLA C
	asl.b	#1,d0
	setflag	regc(regs)

slad	move.b	regd(regs),d0		;CB22 SLA D
	asl.b	#1,d0
	setflag	regd(regs)

slae	move.b	rege(regs),d0		;CB23 SLA E
	asl.b	#1,d0
	setflag	rege(regs)

slah	move.b	regh(regs),d0		;CB24 SLA H
	asl.b	#1,d0
	setflag	regh(regs)

slal	move.b	regl(regs),d0		;CB25 SLA L
	asl.b	#1,d0
	setflag	regl(regs)

slam	move.w	regh(regs),d0		;CB26 SLA M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	asl.b	#1,d0
	setflag (a0)

slaa	asl.b	#1,rega 		;CB27 SLA A
	setflag

srab	move.b	regb(regs),d0		;CB28 SRA B
	asr.b	#1,d0
	setflag	regb(regs)

srac	move.b	regc(regs),d0		;CB29 SRA C
	asr.b	#1,d0
	setflag	regc(regs)

srad	move.b	regd(regs),d0		;CB2A SRA D
	asr.b	#1,d0
	setflag	regd(regs)

srae	move.b	rege(regs),d0		;CB2B SRA E
	asr.b	#1,d0
	setflag	rege(regs)

srah	move.b	regh(regs),d0		;CB2C SRA H
	asr.b	#1,d0
	setflag	regh(regs)

sral	move.b	regl(regs),d0		;CB2D SRA L
	asr.b	#1,d0
	setflag	regl(regs)

sram	move.w	regh(regs),d0		;CB2E SRA M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	setflag (a0)

sraa	asr.b	#1,rega 		;CB2F SRA A
	setflag

srlb	move.b	regb(regs),d0		;CB38 SRL B
	lsr.b	#1,d0
	setflag	regb(regs)

srlc	move.b	regc(regs),d0		;CB39 SRL C
	lsr.b	#1,d0
	setflag	regc(regs)

srld	move.b	regd(regs),d0		;CB3A SRL D
	lsr.b	#1,d0
	setflag	regd(regs)

srle	move.b	rege(regs),d0		;CB3B SRL E
	lsr.b	#1,d0
	setflag	rege(regs)

srlh	move.b	regh(regs),d0		;CB3C SRL H
	lsr.b	#1,d0
	setflag	regh(regs)

srll	move.b	regl(regs),d0		;CB3D SRL L
	lsr.b	#1,d0
	setflag	regl(regs)

srlm	move.w	regh(regs),d0		;CB3E SRL M
	lea	0(targbase,d0.l),a0
	move.b	(a0),d0
	lsr.b	#1,d0
	setflag (a0)

srla	lsr.b	#1,rega 		;CB3F SRL A
	setflag

bit0b	btst	#0,regb(regs)		;CB40 BIT 0,B
	dozf

bit0c	btst	#0,regc(regs)		;CB41 BIT 0,C
	dozf

bit0d	btst	#0,regd(regs)		;CB42 BIT 0,D
	dozf

bit0e	btst	#0,rege(regs)		;CB43 BIT 0,E
	dozf

bit0h	btst	#0,regh(regs)		;CB44 BIT 0,H
	dozf

bit0l	btst	#0,regl(regs)		;CB45 BIT 0,L
	dozf

bit0m	move.w	regh(regs),d0		;CB46 BIT 0,M
	btst	#0,0(targbase,d0.l)
	dozf

bit0a	btst	#0,rega 		;CB47 BIT 0,A
	dozf

bit1b	btst	#1,regb(regs)		;CB48 BIT 1,B
	dozf

bit1c	btst	#1,regc(regs)		;CB49 BIT 1,C
	dozf

bit1d	btst	#1,regd(regs)		;CB4A BIT 1,D
	dozf

bit1e	btst	#1,rege(regs)		;CB4B BIT 1,E
	dozf

bit1h	btst	#1,regh(regs)		;CB4C BIT 1,H
	dozf

bit1l	btst	#1,regl(regs)		;CB4D BIT 1,L
	dozf

bit1m	move.w	regh(regs),d0		;CB4E BIT 1,M
	btst	#1,0(targbase,d0.l)
	dozf

bit1a	btst	#1,rega 		;CB4F BIT 1,A
	dozf

bit2b	btst	#2,regb(regs)		;CB50 BIT 2,B
	dozf

bit2c	btst	#2,regc(regs)		;CB51 BIT 2,C
	dozf

bit2d	btst	#2,regd(regs)		;CB52 BIT 2,D
	dozf

bit2e	btst	#2,rege(regs)		;CB53 BIT 2,E
	dozf

bit2h	btst	#2,regh(regs)		;CB54 BIT 2,H
	dozf

bit2l	btst	#2,regl(regs)		;CB55 BIT 2,L
	dozf

bit2m	move.w	regh(regs),d0		;CB56 BIT 2,M
	btst	#2,0(targbase,d0.l)
	dozf

bit2a	btst	#2,rega 		;CB57 BIT 2,A
	dozf

bit3b	btst	#3,regb(regs)		;CB58 BIT 3,B
	dozf

bit3c	btst	#3,regc(regs)		;CB59 BIT 3,C
	dozf

bit3d	btst	#3,regd(regs)		;CB5A BIT 3,D
	dozf

bit3e	btst	#3,rege(regs)		;CB5B BIT 3,E
	dozf

bit3h	btst	#3,regh(regs)		;CB5C BIT 3,H
	dozf

bit3l	btst	#3,regl(regs)		;CB5D BIT 3,L
	dozf

bit3m	move.w	regh(regs),d0		;CB5E BIT 3,M
	btst	#3,0(targbase,d0.l)
	dozf

bit3a	btst	#3,rega 		;CB5F BIT 3,A
	dozf

bit4b	btst	#4,regb(regs)		;CB60 BIT 4,B
	dozf

bit4c	btst	#4,regc(regs)		;CB61 BIT 4,C
	dozf

bit4d	btst	#4,regd(regs)		;CB62 BIT 4,D
	dozf

bit4e	btst	#4,rege(regs)		;CB63 BIT 4,E
	dozf

bit4h	btst	#4,regh(regs)		;CB64 BIT 4,H
	dozf

bit4l	btst	#4,regl(regs)		;CB65 BIT 4,L
	dozf

bit4m	move.w	regh(regs),d0		;CB66 BIT 4,M
	btst	#4,0(targbase,d0.l)
	dozf

bit4a	btst	#4,rega 		;CB67 BIT 4,A
	dozf

bit5b	btst	#5,regb(regs)		;CB68 BIT 5,B
	dozf

bit5c	btst	#5,regc(regs)		;CB69 BIT 5,C
	dozf

bit5d	btst	#5,regd(regs)		;CB6A BIT 5,D
	dozf

bit5e	btst	#5,rege(regs)		;CB6B BIT 5,E
	dozf

bit5h	btst	#5,regh(regs)		;CB6C BIT 5,H
	dozf

bit5l	btst	#5,regl(regs)		;CB6D BIT 5,L
	dozf

bit5m	move.w	regh(regs),d0		;CB6E BIT 5,M
	btst	#5,0(targbase,d0.l)
	dozf

bit5a	btst	#5,rega 		;CB6F BIT 5,A
	dozf

bit6b	btst	#6,regb(regs)		;CB70 BIT 6,B
	dozf

bit6c	btst	#6,regc(regs)		;CB71 BIT 6,C
	dozf

bit6d	btst	#6,regd(regs)		;CB72 BIT 6,D
	dozf

bit6e	btst	#6,rege(regs)		;CB73 BIT 6,E
	dozf

bit6h	btst	#6,regh(regs)		;CB74 BIT 6,H
	dozf

bit6l	btst	#6,regl(regs)		;CB75 BIT 6,L
	dozf

bit6m	move.w	regh(regs),d0		;CB76 BIT 6,M
	btst	#6,0(targbase,d0.l)
	dozf

bit6a	btst	#6,rega 		;CB77 BIT 6,A
	dozf

bit7b	btst	#7,regb(regs)		;CB78 BIT 7,B
	dozf

bit7c	btst	#7,regc(regs)		;CB79 BIT 7,C
	dozf

bit7d	btst	#7,regd(regs)		;CB7A BIT 7,D
	dozf

bit7e	btst	#7,rege(regs)		;CB7B BIT 7,E
	dozf

bit7h	btst	#7,regh(regs)		;CB7C BIT 7,H
	dozf

bit7l	btst	#7,regl(regs)		;CB7D BIT 7,L
	dozf

bit7m	move.w	regh(regs),d0		;CB7E BIT 7,M
	btst	#7,0(targbase,d0.l)
	dozf

bit7a	btst	#7,rega 		;CB7F BIT 7,A
	dozf

res0b	bclr	#0,regb(regs)		;CB80 RES 0,B
	jmp	(return)

res0c	bclr	#0,regc(regs)		;CB81 RES 0,C
	jmp	(return)

res0d	bclr	#0,regd(regs)		;CB82 RES 0,D
	jmp	(return)

res0e	bclr	#0,rege(regs)		;CB83 RES 0,E
	jmp	(return)

res0h	bclr	#0,regh(regs)		;CB84 RES 0,H
	jmp	(return)

res0l	bclr	#0,regl(regs)		;CB85 RES 0,L
	jmp	(return)

res0m	move.w	regh(regs),d0		;CB86 RES 0,M
	bclr	#0,0(targbase,d0.l)
	jmp	(return)

res0a	bclr	#0,rega 		;CB87 RES 0,A
	jmp	(return)

res1b	bclr	#1,regb(regs)		;CB88 RES 1,B
	jmp	(return)

res1c	bclr	#1,regc(regs)		;CB89 RES 1,C
	jmp	(return)

res1d	bclr	#1,regd(regs)		;CB8A RES 1,D
	jmp	(return)

res1e	bclr	#1,rege(regs)		;CB8B RES 1,E
	jmp	(return)

res1h	bclr	#1,regh(regs)		;CB8C RES 1,H
	jmp	(return)

res1l	bclr	#1,regl(regs)		;CB8D RES 1,L
	jmp	(return)

res1m	move.w	regh(regs),d0		;CB8E RES 1,M
	bclr	#1,0(targbase,d0.l)
	jmp	(return)

res1a	bclr	#1,rega 		;CB8F RES 1,A
	jmp	(return)

res2b	bclr	#2,regb(regs)		;CB90 RES 2,B
	jmp	(return)

res2c	bclr	#2,regc(regs)		;CB91 RES 2,C
	jmp	(return)

res2d	bclr	#2,regd(regs)		;CB92 RES 2,D
	jmp	(return)

res2e	bclr	#2,rege(regs)		;CB93 RES 2,E
	jmp	(return)

res2h	bclr	#2,regh(regs)		;CB94 RES 2,H
	jmp	(return)

res2l	bclr	#2,regl(regs)		;CB95 RES 2,L
	jmp	(return)

res2m	move.w	regh(regs),d0		;CB96 RES 2,M
	bclr	#2,0(targbase,d0.l)
	jmp	(return)

res2a	bclr	#2,rega 		;CB97 RES 2,A
	jmp	(return)

res3b	bclr	#3,regb(regs)		;CB98 RES 3,B
	jmp	(return)

res3c	bclr	#3,regc(regs)		;CB99 RES 3,C
	jmp	(return)

res3d	bclr	#3,regd(regs)		;CB9A RES 3,D
	jmp	(return)

res3e	bclr	#3,rege(regs)		;CB9B RES 3,E
	jmp	(return)

res3h	bclr	#3,regh(regs)		;CB9C RES 3,H
	jmp	(return)

res3l	bclr	#3,regl(regs)		;CB9D RES 3,L
	jmp	(return)

res3m	move.w	regh(regs),d0		;CB9E RES 3,M
	bclr	#3,0(targbase,d0.l)
	jmp	(return)

res3a	bclr	#3,rega 		;CB9F RES 3,A
	jmp	(return)

res4b	bclr	#4,regb(regs)		;CBA0 RES 4,B
	jmp	(return)

res4c	bclr	#4,regc(regs)		;CBA1 RES 4,C
	jmp	(return)

res4d	bclr	#4,regd(regs)		;CBA2 RES 4,D
	jmp	(return)

res4e	bclr	#4,rege(regs)		;CBA3 RES 4,E
	jmp	(return)

res4h	bclr	#4,regh(regs)		;CBA4 RES 4,H
	jmp	(return)

res4l	bclr	#4,regl(regs)		;CBA5 RES 4,L
	jmp	(return)

res4m	move.w	regh(regs),d0		;CBA6 RES 4,M
	bclr	#4,0(targbase,d0.l)
	jmp	(return)

res4a	bclr	#4,rega 		;CBA7 RES 4,A
	jmp	(return)

res5b	bclr	#5,regb(regs)		;CBA8 RES 5,B
	jmp	(return)

res5c	bclr	#5,regc(regs)		;CBA9 RES 5,C
	jmp	(return)

res5d	bclr	#5,regd(regs)		;CBAA RES 5,D
	jmp	(return)

res5e	bclr	#5,rege(regs)		;CBAB RES 5,E
	jmp	(return)

res5h	bclr	#5,regh(regs)		;CBAC RES 5,H
	jmp	(return)

res5l	bclr	#5,regl(regs)		;CBAD RES 5,L
	jmp	(return)

res5m	move.w	regh(regs),d0		;CBAE RES 5,M
	bclr	#5,0(targbase,d0.l)
	jmp	(return)

res5a	bclr	#5,rega 		;CBAF RES 5,A
	jmp	(return)

res6b	bclr	#6,regb(regs)		;CBB0 RES 6,B
	jmp	(return)

res6c	bclr	#6,regc(regs)		;CBB1 RES 6,C
	jmp	(return)

res6d	bclr	#6,regd(regs)		;CBB2 RES 6,D
	jmp	(return)

res6e	bclr	#6,rege(regs)		;CBB3 RES 6,E
	jmp	(return)

res6h	bclr	#6,regh(regs)		;CBB4 RES 6,H
	jmp	(return)

res6l	bclr	#6,regl(regs)		;CBB5 RES 6,L
	jmp	(return)

res6m	move.w	regh(regs),d0		;CBB6 RES 6,M
	bclr	#6,0(targbase,d0.l)
	jmp	(return)

res6a	bclr	#6,rega 		;CBB7 RES 6,A
	jmp	(return)

res7b	bclr	#7,regb(regs)		;CBB8 RES 7,B
	jmp	(return)

res7c	bclr	#7,regc(regs)		;CBB9 RES 7,C
	jmp	(return)

res7d	bclr	#7,regd(regs)		;CBBA RES 7,D
	jmp	(return)

res7e	bclr	#7,rege(regs)		;CBBB RES 7,E
	jmp	(return)

res7h	bclr	#7,regh(regs)		;CBBC RES 7,H
	jmp	(return)

res7l	bclr	#7,regl(regs)		;CBBD RES 7,L
	jmp	(return)

res7m	move.w	regh(regs),d0		;CBBE RES 7,M
	bclr	#7,0(targbase,d0.l)
	jmp	(return)

res7a	bclr	#7,rega 		;CBBF RES 7,A
	jmp	(return)

set0b	bset	#0,regb(regs)		;CBC0 SET 0,B
	jmp	(return)

set0c	bset	#0,regc(regs)		;CBC1 SET 0,C
	jmp	(return)

set0d	bset	#0,regd(regs)		;CBC2 SET 0,D
	jmp	(return)

set0e	bset	#0,rege(regs)		;CBC3 SET 0,E
	jmp	(return)

set0h	bset	#0,regh(regs)		;CBC4 SET 0,H
	jmp	(return)

set0l	bset	#0,regl(regs)		;CBC5 SET 0,L
	jmp	(return)

set0m	move.w	regh(regs),d0		;CBC6 SET 0,M
	bset	#0,0(targbase,d0.l)
	jmp	(return)

set0a	bset	#0,rega 		;CBC7 SET 0,A
	jmp	(return)

set1b	bset	#1,regb(regs)		;CBC8 SET 1,B
	jmp	(return)

set1c	bset	#1,regc(regs)		;CBC9 SET 1,C
	jmp	(return)

set1d	bset	#1,regd(regs)		;CBCA SET 1,D
	jmp	(return)

set1e	bset	#1,rege(regs)		;CBCB SET 1,E
	jmp	(return)

set1h	bset	#1,regh(regs)		;CBCC SET 1,H
	jmp	(return)

set1l	bset	#1,regl(regs)		;CBCD SET 1,L
	jmp	(return)

set1m	move.w	regh(regs),d0		;CBCE SET 1,M
	bset	#1,0(targbase,d0.l)
	jmp	(return)

set1a	bset	#1,rega 		;CBCF SET 1,A
	jmp	(return)

set2b	bset	#2,regb(regs)		;CBD0 SET 2,B
	jmp	(return)

set2c	bset	#2,regc(regs)		;CBD1 SET 2,C
	jmp	(return)

set2d	bset	#2,regd(regs)		;CBD2 SET 2,D
	jmp	(return)

set2e	bset	#2,rege(regs)		;CBD3 SET 2,E
	jmp	(return)

set2h	bset	#2,regh(regs)		;CBD4 SET 2,H
	jmp	(return)

set2l	bset	#2,regl(regs)		;CBD5 SET 2,L
	jmp	(return)

set2m	move.w	regh(regs),d0		;CBD6 SET 2,M
	bset	#2,0(targbase,d0.l)
	jmp	(return)

set2a	bset	#2,rega 		;CBD7 SET 2,A
	jmp	(return)

set3b	bset	#3,regb(regs)		;CBD8 SET 3,B
	jmp	(return)

set3c	bset	#3,regc(regs)		;CBD9 SET 3,C
	jmp	(return)

set3d	bset	#3,regd(regs)		;CBDA SET 3,D
	jmp	(return)

set3e	bset	#3,rege(regs)		;CBDB SET 3,E
	jmp	(return)

set3h	bset	#3,regh(regs)		;CBDC SET 3,H
	jmp	(return)

set3l	bset	#3,regl(regs)		;CBDD SET 3,L
	jmp	(return)

set3m	move.w	regh(regs),d0		;CBDE SET 3,M
	bset	#3,0(targbase,d0.l)
	jmp	(return)

set3a	bset	#3,rega 		;CBDF SET 3,A
	jmp	(return)

set4b	bset	#4,regb(regs)		;CBE0 SET 4,B
	jmp	(return)

set4c	bset	#4,regc(regs)		;CBE1 SET 4,C
	jmp	(return)

set4d	bset	#4,regd(regs)		;CBE2 SET 4,D
	jmp	(return)

set4e	bset	#4,rege(regs)		;CBE3 SET 4,E
	jmp	(return)

set4h	bset	#4,regh(regs)		;CBE4 SET 4,H
	jmp	(return)

set4l	bset	#4,regl(regs)		;CBE5 SET 4,L
	jmp	(return)

set4m	move.w	regh(regs),d0		;CBE6 SET 4,M
	bset	#4,0(targbase,d0.l)
	jmp	(return)

set4a	bset	#4,rega 		;CBE7 SET 4,A
	jmp	(return)

set5b	bset	#5,regb(regs)		;CBE8 SET 5,B
	jmp	(return)

set5c	bset	#5,regc(regs)		;CBE9 SET 5,C
	jmp	(return)

set5d	bset	#5,regd(regs)		;CBEA SET 5,D
	jmp	(return)

set5e	bset	#5,rege(regs)		;CBEB SET 5,E
	jmp	(return)

set5h	bset	#5,regh(regs)		;CBEC SET 5,H
	jmp	(return)

set5l	bset	#5,regl(regs)		;CBED SET 5,L
	jmp	(return)

set5m	move.w	regh(regs),d0		;CBEE SET 5,M
	bset	#5,0(targbase,d0.l)
	jmp	(return)

set5a	bset	#5,rega 		;CBEF SET 5,A
	jmp	(return)

set6b	bset	#6,regb(regs)		;CBF0 SET 6,B
	jmp	(return)

set6c	bset	#6,regc(regs)		;CBF1 SET 6,C
	jmp	(return)

set6d	bset	#6,regd(regs)		;CBF2 SET 6,D
	jmp	(return)

set6e	bset	#6,rege(regs)		;CBF3 SET 6,E
	jmp	(return)

set6h	bset	#6,regh(regs)		;CBF4 SET 6,H
	jmp	(return)

set6l	bset	#6,regl(regs)		;CBF5 SET 6,L
	jmp	(return)

set6m	move.w	regh(regs),d0		;CBF6 SET 6,M
	bset	#6,0(targbase,d0.l)
	jmp	(return)

set6a	bset	#6,rega 		;CBF7 SET 6,A
	jmp	(return)

set7b	bset	#7,regb(regs)		;CBF8 SET 7,B
	jmp	(return)

set7c	bset	#7,regc(regs)		;CBF9 SET 7,C
	jmp	(return)

set7d	bset	#7,regd(regs)		;CBFA SET 7,D
	jmp	(return)

set7e	bset	#7,rege(regs)		;CBFB SET 7,E
	jmp	(return)

set7h	bset	#7,regh(regs)		;CBFC SET 7,H
	jmp	(return)

set7l	bset	#7,regl(regs)		;CBFD SET 7,L
	jmp	(return)

set7m	move.w	regh(regs),d0		;CBFE SET 7,M
	bset	#7,0(targbase,d0.l)
	jmp	(return)

set7a	bset	#7,rega 		;CBFF SET 7,A
	jmp	(return)

dadixb	move.w	regb(regs),d0		;DD09 DAD IX,B
	add.w	d0,regix(regs)
	docyf

dadixd	move.w	regd(regs),d0		;DD19 DAD IX,D
	add.w	d0,regix(regs)
	docyf

lxiix	move.b	(pseudopc)+,regxl(regs) ;DD21 LXI IX,nnnn
	move.b	(pseudopc)+,regxh(regs)
	jmp	(return)

sixd	lsxxd				;DD22 SIXD addr
	move.b	regxl(regs),(a0)+
	move.b	regxh(regs),(a0)
	jmp	(return)

inxix	addq.w	#1,regix(regs)		;DD23 INX IX
	jmp	(return)

inrxh	addq.b	#1,regxh(regs)		;DD24 INR XH (undocumented)
	inrflag

dcrxh	subq.b	#1,regxh(regs)		;DD25 DCR XH (undocumented)
	inrflag

mvixh	move.b	(pseudopc)+,regxh(regs) ;DD26 MVI XH,nn (undocumented)
	jmp	(return)

dadixx	asl.w	regix(regs)		;DD29 DAD IX,IX (multiply by 2)
	docyf

lixd	lsxxd				;DD2A LIXD addr
	move.b	(a0)+,regxl(regs)
	move.b	(a0),regxh(regs)
	jmp	(return)

dcxix	subq.w	#1,regix(regs)		;DD2B DCX IX
	jmp	(return)

inrxl	addq.b	#1,regxl(regs)		;DD2C INR XL (undocumented)
	inrflag

dcrxl	subq.b	#1,regxl(regs)		;DD2D DCR XL (undocumented)
	inrflag

mvixl	move.b	(pseudopc)+,regxl(regs) ;DD2E MVI XL,nn (undocumented)
	jmp	(return)

inrix	calcind x			;DD34 INR (IX+d)
	addq.b	#1,0(targbase,d0.l)
	inrflag

dcrix	calcind x			;DD35 DCR (IX+d)
	subq.b	#1,0(targbase,d0.l)
	inrflag

mviix	calcind x			;DD36 MVI (IX+d),nn
	move.b	(pseudopc)+,0(targbase,d0.l)
	jmp	(return)

dadixs	move.l	pseudosp,d0		;DD39 DAD IX,SP
	sub.l	targbase,d0
	add.w	d0,regix(regs)
	docyf

movbxh	move.b	regxh(regs),regb(regs)	;DD44 MOV B,XH (undocumented)
	jmp	(return)

movbxl	move.b	regxl(regs),regb(regs)	;DD45 MOV B,XL (undocumented)
	jmp	(return)

movbix	calcind x			;DD46 MOV B,(IX+d)
	move.b	0(targbase,d0.l),regb(regs)
	jmp	(return)

movcxh	move.b	regxh(regs),regc(regs)	;DD4C MOV C,XH (undocumented)
	jmp	(return)

movcxl	move.b	regxl(regs),regc(regs)	;DD4D MOV C,XL (undocumented)
	jmp	(return)

movcix	calcind x			;DD4E MOV C,(IX+d)
	move.b	0(targbase,d0.l),regc(regs)
	jmp	(return)

movdxh	move.b	regxh(regs),regd(regs)	;DD54 MOV D,XH (undocumented)
	jmp	(return)

movdxl	move.b	regxl(regs),regd(regs)	;DD55 MOV D,XL (undocumented)
	jmp	(return)

movdix	calcind x			;DD56 MOV D,(IX+d)
	move.b	0(targbase,d0.l),regd(regs)
	jmp	(return)

movexh	move.b	regxh(regs),rege(regs)	;DD5C MOV E,XH (undocumented)
	jmp	(return)

movexl	move.b	regxl(regs),rege(regs)	;DD5D MOV E,XL (undocumented)
	jmp	(return)

moveix	calcind x			;DD5E MOV E,(IX+d)
	move.b	0(targbase,d0.l),rege(regs)
	jmp	(return)

movxhb	move.b	regb(regs),regxh(regs)	;DD60 MOV XH,B (undocumented)
	jmp	(return)

movxhc	move.b	regc(regs),regxh(regs)	;DD61 MOV XH,C (undocumented)
	jmp	(return)

movxhd	move.b	regd(regs),regxh(regs)	;DD62 MOV XH,D (undocumented)
	jmp	(return)

movxhe	move.b	rege(regs),regxh(regs)	;DD63 MOV XH,E (undocumented)
	jmp	(return)

mvxhxh	jmp	(return)		;DD64 MOV XH,XH (undocumented)

mvxhxl	move.b	regxl(regs),regxh(regs) ;DD65 MOV XH,XL (undocumented)
	jmp	(return)

movhix	calcind x			;DD66 MOV H,(IX+d)
	move.b	0(targbase,d0.l),regh(regs)
	jmp	(return)

movxlb	move.b	regb(regs),regxl(regs)	;DD68 MOV XL,B (undocumented)
	jmp	(return)

movxlc	move.b	regc(regs),regxl(regs)	;DD69 MOV XL,C (undocumented)
	jmp	(return)

movxld	move.b	regd(regs),regxl(regs)	;DD6A MOV XL,D (undocumented)
	jmp	(return)

movxle	move.b	rege(regs),regxl(regs)	;DD6B MOV XL,E (undocumented)
	jmp	(return)

mvxlxh	move.b	regxh(regs),regxl(regs) ;DD6C MOV XL,XH (undocumented)
	jmp	(return)

mvxlxl	jmp	(return)		;DD6D MOV XL,XL (undocumented)

movlix	calcind x			;DD6E MOV L,(IX+d)
	move.b	0(targbase,d0.l),regl(regs)
	jmp	(return)

movixb	calcind x			;DD70 MOV (IX+d),B
	move.b	regb(regs),0(targbase,d0.l)
	jmp	(return)

movixc	calcind x			;DD71 MOV (IX+d),C
	move.b	regc(regs),0(targbase,d0.l)
	jmp	(return)

movixd	calcind x			;DD72 MOV (IX+d),D
	move.b	regd(regs),0(targbase,d0.l)
	jmp	(return)

movixe	calcind x			;DD73 MOV (IX+d),E
	move.b	rege(regs),0(targbase,d0.l)
	jmp	(return)

movixh	calcind x			;DD74 MOV (IX+d),H
	move.b	regh(regs),0(targbase,d0.l)
	jmp	(return)

movixl	calcind x			;DD75 MOV (IX+d),L
	move.b	regl(regs),0(targbase,d0.l)
	jmp	(return)

movixa	calcind x			;DD77 MOV (IX+d),A
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

movaxh	move.b	regxh(regs),rega	;DD7C MOV A,XH (undocumented)
	jmp	(return)

movaxl	move.b	regxl(regs),rega	;DD7D MOV A,XL (undocumented)
	jmp	(return)

movaix	calcind x			;DD7E MOV A,(IX+d)
	move.b	0(targbase,d0.l),rega
	jmp	(return)

addxh	move.b	regxh(regs),d0		;DD84 ADD XH (undocumented)
	addflag

addxl	move.b	regxl(regs),d0		;DD85 ADD XL (undocumented)
	addflag

addix	calcind x			;DD86 ADD (IX+d)
	move.b	0(targbase,d0.l),d0
	addflag

adcxh	move.b	regf,regop3(regs)	;DD8C ADC XH (undocumented)
	asr.b	#1,regf
	move.b	regxh(regs),d0
	adcflag

adcxl	move.b	regf,regop3(regs)	;DD8E ADC XL (undocumented)
	asr.b	#1,regf
	move.b	regxl(regs),d0
	adcflag

adcix	move.b	regf,regop3(regs)	;DD8E ADC (IX+d)
	calcind x
	asr.b	#1,regf
	move.b	0(targbase,d0.l),d0
	adcflag
	
subxh	sub.b	regxh(regs),rega	;DD94 SUB XH (undocumented)
	setflag

subxl	sub.b	regxl(regs),rega	;DD95 SUB XL (undocumented)
	setflag

subix	calcind x			;DD96 SUB (IX+d)
	sub.b	0(targbase,d0.l),rega
	setflag

sbbxh	asr.b	#1,regf 		;DD9C SBB XH (undocumented)
	move.b	regxh(regs),d0
	sbbflag

sbbxl	asr.b	#1,regf 		;DD9D SBB XL (undocumented)
	move.b	regxl(regs),d0
	sbbflag

sbbix	calcind x			;DD9E SBB (IX+d)
	asr.b	#1,regf
	move.b	0(targbase,d0.l),d0
	sbbflag

anaxh	and.b	regxh(regs),rega	;DDA4 ANA XH (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

anaxl	and.b	regxl(regs),rega	;DDA5 ANA XL (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

anaix	calcind x			;DDA6 ANA (IX+d)
	and.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xraxh	move.b	regxh(regs),d0		;DDAC XRA XH (undocumented)
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xraxl	move.b	regxl(regs),d0		;DDAD XRA XL (undocumented)
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xraix	calcind x			;DDAE XRA (IX+d)
	move.b	0(targbase,d0.l),d0
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oraxh	or.b	regxh(regs),rega	;DDB4 ORA XH (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oraxl	or.b	regxl(regs),rega	;DDB5 ORA XL (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oraix	calcind x			;DDB6 ORA (IX+d)
	or.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

cmpxh	cmp.b	regxh(regs),rega	;DDBC CMP XH (undocumented)
	setflag

cmpxl	cmp.b	regxl(regs),rega	;DDBD CMP XL (undocumented)
	setflag

rlcix	move.b	0(targbase,d0.l),d0	;DDCB06 RLC (IX+d)
	rol.b	#1,d0
	setflag SETONLY
	move.b	d0,0(targbase,d0.l)
	jmp	(return)

rrcix	move.b	0(targbase,d0.l),d1	;DDCB0E RRC (IX+d)
	ror.b	#1,d0
	setflag SETONLY
	move.b	d0,0(targbase,d0.l)
	jmp	(return)

rlix	move.b	0(targbase,d0.l),d1	;DDCB16 RL (IX+d)
	roxr.b	#1,regf
	roxl.b	#1,d1
	roxl.b	#1,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)

rrix	move.b	0(targbase,d0.l),d1	;DDCB1E RR (IX+d)
	roxr.b	#1,regf
	roxr.b	#1,d1
	roxl.b	#1,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)

slaix	move.b	0(targbase,d0.l),d1	;DDCB26 SLA (IX+d)
	asl.b	#1,d1
	bcs.s	slaixc
	bclr	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)
slaixc	bset	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)

sraix	move.b	0(targbase,d0.l),d1	;DDCB2E SRA (IX+d)
	asr.b	#1,d1
	bcs.s	sraixc
	bclr	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)
sraixc	bset	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)

srlix	move.b	0(targbase,d0.l),d1	;DDCB3E SRL (IX+d)
	lsr.b	#1,d1
	bcs.s	srlixc
	bclr	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)
srlixc	bset	#0,regf
	move.b	d1,0(targbase,d0.l)
	jmp	(return)

bit0ix	btst	#0,0(targbase,d0.l)	;DDCB46 BIT 0,(IX+d)
	dozf

bit1ix	btst	#1,0(targbase,d0.l)	;DDCB4E BIT 1,(IX+d)
	dozf

bit2ix	btst	#2,0(targbase,d0.l)	;DDCB56 BIT 2,(IX+d)
	dozf

bit3ix	btst	#3,0(targbase,d0.l)	;DDCB5E BIT 3,(IX+d)
	dozf

bit4ix	btst	#4,0(targbase,d0.l)	;DDCB66 BIT 4,(IX+d)
	dozf

bit5ix	btst	#5,0(targbase,d0.l)	;DDCB6E BIT 5,(IX+d)
	dozf

bit6ix	btst	#6,0(targbase,d0.l)	;DDCB76 BIT 6,(IX+d)
	dozf

bit7ix	btst	#7,0(targbase,d0.l)	;DDCB7E BIT 7,(IX+d)
	dozf

res0ix	bclr	#0,0(targbase,d0.l)	;DDCB86 RES 0,(IX+d)
	jmp	(return)

res1ix	bclr	#1,0(targbase,d0.l)	;DDCB8E RES 1,(IX+d)
	jmp	(return)

res2ix	bclr	#2,0(targbase,d0.l)	;DDCB96 RES 2,(IX+d)
	jmp	(return)

res3ix	bclr	#3,0(targbase,d0.l)	;DDCB9E RES 3,(IX+d)
	jmp	(return)

res4ix	bclr	#4,0(targbase,d0.l)	;DDCBA6 RES 4,(IX+d)
	jmp	(return)

res5ix	bclr	#5,0(targbase,d0.l)	;DDCBAE RES 5,(IX+d)
	jmp	(return)

res6ix	bclr	#6,0(targbase,d0.l)	;DDCBB6 RES 6,(IX+d)
	jmp	(return)

res7ix	bclr	#7,0(targbase,d0.l)	;DDCBBE RES 7,(IX+d)
	jmp	(return)

set0ix	bset	#0,0(targbase,d0.l)	;DDCBC6 SET 0,(IX+d)
	jmp	(return)

set1ix	bset	#1,0(targbase,d0.l)	;DDCBCE SET 1,(IX+d)
	jmp	(return)

set2ix	bset	#2,0(targbase,d0.l)	;DDCBD6 SET 2,(IX+d)
	jmp	(return)

set3ix	bset	#3,0(targbase,d0.l)	;DDCBDE SET 3,(IX+d)
	jmp	(return)

set4ix	bset	#4,0(targbase,d0.l)	;DDCBE6 SET 4,(IX+d)
	jmp	(return)

set5ix	bset	#5,0(targbase,d0.l)	;DDCBEE SET 5,(IX+d)
	jmp	(return)

set6ix	bset	#6,0(targbase,d0.l)	;DDCBF6 SET 6,(IX+d)
	jmp	(return)

set7ix	bset	#7,0(targbase,d0.l)	;DDCBFE SET 7,(IX+d)
	jmp	(return)

cmpix	calcind x			;DDBE CMP (IX+d)
	cmp.b	0(targbase,d0.l),rega
	setflag

popix	move.b	(pseudosp)+,regxl(regs) ;DDE1 POP IX
	move.b	(pseudosp)+,regxh(regs)
	jmp	(return)

xtix	move.b	regxl(regs),d0		;DDE3 XTIX
	move.b	(pseudosp),regxl(regs)
	move.b	d0,(pseudosp)
	move.b	regxh(regs),d0
	move.b	1(pseudosp),regxh(regs)
	move.b	d0,1(pseudosp)
	jmp	(return)

pushix	move.b	regxh(regs),-(pseudosp) ;DDE5 PUSH IX
	move.b	regxl(regs),-(pseudosp)
	jmp	(return)

pcix	move.w	regix(regs),d0		;DDE9 PCIX
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)

spix	move.w	regix(regs),d0		;DDF9 SPIX
	lea	0(targbase,d0.l),pseudosp
	jmp	(return)

inbc	move.b	regc(regs),d0		;ED40 IN B,(C)
	lea	regb(regs),a0
	jsr	inp
	jmp	(return)

outcb	move.b	regc(regs),d0		;ED41 OUT (C),B
	lea	regb(regs),a0
	jsr	outp
	jmp	(return)

dsbbb	ashl	sub,b			;ED42 DSBB B

sbcd	lsxxd				;ED43 SBCD addr
	move.b	regc(regs),(a0)+
	move.b	regb(regs),(a0)
	jmp	(return)

nega	neg.b	rega			;ED44 NEG
	setflag

retn	equ	ret			;ED45 RETN (treated as RET)

im0	jmp	(return)		;ED46 IM0 (set 8080A interrupt mode)

moveia	move.b	rega,regi(regs) 	;ED47 MOV I,A
	jmp	(return)

incc	move.b	regc(regs),d0		;ED48 IN C,(C)
	lea	regc(regs),a0
	jsr	inp
	jmp	(return)

outcc	move.b	regc(regs),d0		;ED49 OUT (C),C
	lea	regc(regs),a0
	jsr	outp
	jmp	(return)

dadcb	ashl	add,b			;ED4A DADC B

lbcd	lsxxd				;ED4B LBCD addr
	move.b	(a0)+,regc(regs)
	move.b	(a0),regb(regs)
	jmp	(return)

reti	equ	ret			;ED4D RETI (treated as RET)

movera	jmp	(return)		;ED4F MOV R,A (ignored)

indc	move.b	regc(regs),d0		;ED50 IN D,(C)
	lea	regd(regs),a0
	jsr	inp
	jmp	(return)

outcd	move.b	regc(regs),d0		;ED51 OUT (C),D
	lea	regd(regs),a0
	jsr	outp
	jmp	(return)

dsbbd	ashl	sub,d			;ED52 DSBB D

sded	lsxxd				;ED53 SDED addr
	move.b	rege(regs),(a0)+
	move.b	regd(regs),(a0)
	jmp	(return)

im1	jmp	(return)		;ED56 IM1 (ignored)

moveai	move.b	regi(regs),rega 	;ED57 MOV A,I
	jmp	(return)

inec	move.b	regc(regs),d0		;ED58 IN E,(C)
	lea	rege(regs),a0
	jsr	inp
	jmp	(return)

outce	move.b	regc(regs),d0		;ED59 OUT (C),E
	lea	rege(regs),a0
	jsr	outp
	jmp	(return)

dadcd	ashl	add,d			;ED5A DADC D

lded	lsxxd				;ED5B LDED addr
	move.b	(a0)+,rege(regs)
	move.b	(a0),regd(regs)
	jmp	(return)

im2	jmp	(return)		;ED5E IM2 (ignored)

*					;ED5F MOV A,R (handled in simcpm1)

inhc	move.b	regc(regs),d0		;ED60 IN H,(C)
	lea	regh(regs),a0
	jsr	inp
	jmp	(return)

outch	move.b	regc(regs),d0		;ED61 OUT (C),H
	lea	regh(regs),a0
	jsr	outp
	jmp	(return)

dsbbh	ashl	sub,h			;ED62 DSBB H

*					;ED63 SHLD addr (uses 8080 routine 22)

rrd	move.w	regh(regs),d0		;ED67 RRD
	lea	0(targbase,d0.l),a0	;Address of memory operand
	move.b	rega,d1 	;Original contents of the accumulator
	move.b	(a0),d0
	and.b	regcon0f,d0	;This part shifts out of the memory operand.
	and.b	#$F0,rega
	or.b	d0,rega 	;Adjust the accumulator.
	move.b	(a0),d0
	lsr.b	#4,d0		;Shift the memory operand 4 bits right.
	and.b	regcon0f,d1	;Low-order nybble of original accumulator
	lsl.b	#4,d1		;It moves to the high-order nybble.
	or.b	d1,d0		;Put it into the memory operand.
	move.b	d0,(a0) 	;Restore the new memory operand.
	jmp	(return)

inlc	move.b	regc(regs),d0		;ED68 IN L,(C)
	lea	regl(regs),a0
	jsr	inp
	jmp	(return)

outcl	move.b	regc(regs),d0		;ED69 OUT (C),L
	lea	regl(regs),a0
	jsr	outp
	jmp	(return)

dadch	ashl	add,h			;ED6A DADC H

*					;ED6B LHLD addr (uses 8080 routine 2A)

rld	move.w	regh(regs),d0		;ED6F RLD
	lea	0(targbase,d0.l),a0	;Address of memory operand
	move.b	rega,d1 	;Original contents of the accumulator
	move.b	(a0),d0
	lsr.b	#4,d0		;This part shifts out of the memory operand.
	and.b	#$F0,rega
	or.b	d0,rega 	;Adjust the accumulator.
	move.b	(a0),d0
	lsl.b	#4,d0		;Shift the memory operand 4 bits left.
	and.b	regcon0f,d1	;Low-order nybble of original accumulator
	or.b	d1,d0		;Put it into the memory operand.
	move.b	d0,(a0) 	;Restore the new memory operand.
	jmp	(return)

inmc	move.b	regc(regs),d0		;ED70 IN M,(C)
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	inp
	jmp	(return)

outcm	move.b	regc(regs),d0		;ED71 OUT (C),M
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	outp
	jmp	(return)

dsbbs	move.w	pseudosp,d0		;ED72 DSBB SP
	move.w	regh(regs),d1
	asr.b	#1,regf 	;Put simulated carry flag in 68000's X flag.
	ori	#4,ccr		;Set the 68000's Z flag for ADDX/SUBX.
	subx.w	d0,d1
	ifeq	x680x0		;68000 only
	move	sr,d0
	endc
	ifne	x680x0		;68010 and higher
	dc.w	$42C0		;MOVE CCR,D0
	endc
	and.w	regcon0f,d0
	move.b	0(flagptr,d0.w),regf
	move.w	d1,regh(regs)
	jmp	(return)

sspd	lsxxd				;ED73 SSPD addr
	move.l	pseudosp,d0
	sub.l	targbase,d0
	move.b	d0,(a0)+
	lsr.w	#8,d0
	move.b	d0,(a0)
	jmp	(return)

inac	move.b	regc(regs),d0		;ED78 IN A,(C)
	lea	tmprega,a0
	jsr	inp
	move.b	tmprega,rega
	jmp	(return)

outca	move.b	regc(regs),d0		;ED79 OUT (C),A
	lea	tmprega,a0
	move.b	rega,(a0)
	jsr	outp
	jmp	(return)

dadcs	move.w	pseudosp,d0		;ED7A DADC SP
	move.w	regh(regs),d1
	asr.b	#1,regf 	;Put simulated carry flag in 68000's X flag.
	ori	#4,ccr		;Set the 68000's Z flag for ADDX/SUBX.
	addx.w	d0,d1
	ifeq	x680x0		;68000 only
	move	sr,d0
	endc
	ifne	x680x0		;68010 and higher
	dc.w	$42C0		;MOVE CCR,D0
	endc
	and.w	regcon0f,d0
	move.b	0(flagptr,d0.w),regf
	move.w	d1,regh(regs)
	jmp	(return)

lspd	lsxxd				;ED7B LSPD addr
	moveq	#0,d0
	move.b	1(a0),d0
	lsl.w	#8,d0
	move.b	(a0),d0
	movea.l	d0,pseudosp
	adda.l	targbase,pseudosp
	jmp	(return)

ldi	moveq	#0,d1			;EDA0 LDI
	move.w	regh(regs),d0
	move.w	regd(regs),d1
	move.b	0(targbase,d0.l),0(targbase,d1.l)
	addq.w	#1,regh(regs)
	addq.w	#1,regd(regs)
	subq.w	#1,regb(regs)
	beq.s	1$
	ori.b	#4,regf
	andi.b	#$ED,regf
	jmp	(return)
1$	andi.b	#$E9,regf
	jmp	(return)

cmpi	move.w	regh(regs),d0		;EDA1 CMPI (Z-80 mnemonic is CPI)
	cmp.b	0(targbase,d0.l),rega
	setflag	SETONLY
	addq.w	#1,regh(regs)
	subq.w	#1,regb(regs)
	beq.s	1$
	ori.b	#6,regf
	jmp	(return)
1$	ori.b	#2,regf
	jmp	(return)

ini	move.b	regc(regs),d0		;EDA2 INI
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	inp
	addq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	jmp	(return)

outi	move.b	regc(regs),d0		;EDA3 OUTI
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	outp
	addq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	jmp	(return)

ldd	moveq	#0,d1			;EDA8 LDD
	move.w	regh(regs),d0
	move.w	regd(regs),d1
	move.b	0(targbase,d0.l),0(targbase,d1.l)
	subq.w	#1,regh(regs)
	subq.w	#1,regd(regs)
	subq.w	#1,regb(regs)
	beq.s	1$
	ori.b	#4,regf
	andi.b	#$ED,regf
	jmp	(return)
1$	andi.b	#$E9,regf
	jmp	(return)

cpd	move.w	regh(regs),d0		;EDA9 CPD
	cmp.b	0(targbase,d0.l),rega
	setflag	SETONLY
	subq.w	#1,regh(regs)
	subq.w	#1,regb(regs)
	beq.s	1$
	ori.b	#6,regf
	jmp	(return)
1$	ori.b	#2,regf
	jmp	(return)

ind	move.b	regc(regs),d0		;EDAA IND
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	inp
	subq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	jmp	(return)

outd	move.b	regc(regs),d0		;EDAB OUTD
	move.w	regh(regs),d1
	lea	0(targbase,d1.l),a0
	jsr	outp
	subq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	jmp	(return)

ldir	move.l	a5,-(sp)		;EDB0 LDIR
	move.w	regb(regs),d1		;Grab count.
	move.w	regh(regs),d0		;Source
	lea	0(targbase,d0.l),a0
	move.w	regd(regs),d0		;Destination
	lea	0(targbase,d0.l),a5
	add.w	d1,regh(regs)		;Adjust Z-80 registers now.
	add.w	d1,regd(regs)
	clr.w	regb(regs)
	andi.b	#$C1,regf
	subq.w	#1,d1			;Adjust count for DBRA.
ldirlop move.b	(a0)+,(a5)+
	dbra	d1,ldirlop
	move.l	(sp)+,a5
	jmp	(return)

cpir	move.w	regb(regs),d1		;EDB1 CPIR
	subq.w	#1,d1			;Grab count, adjust for DBRA.
	move.w	regh(regs),d0		;Source
	lea	0(targbase,d0.l),a0
cpirlop addq.w	#1,d0
	cmp.b	(a0)+,rega
	dbeq	d1,cpirlop
	bne.s	cpirnom
	move.w	d0,regh(regs)		;Restore Z-80 registers.
	move.w	d1,regb(regs)
	beq.s	1$
	moveq	#$46,regf		;Found, in the string.
	jmp	(return)
1$	moveq	#$42,regf		;Found, but at last place.
	jmp	(return)
cpirnom	move.w	d0,regh(regs)		;Restore Z-80 registers.
	addq.w	#1,d1
	move.w	d1,regb(regs)
	beq.s	2$
	moveq	#6,regf
	jmp	(return)
2$	moveq	#2,regf
	jmp	(return)

inir	move.b	regc(regs),d1		;EDB2 INIR
	move.w	regh(regs),d0
	lea	0(targbase,d0.l),a0
	jsr	inp
	addq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	bne.s	inir
	jmp	(return)

otir	move.b	regc(regs),d1		;EDB3 OTIR
	move.w	regh(regs),d0
	lea	0(targbase,d0.l),a0
	jsr	outp
	addq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	bne.s	otir
	jmp	(return)

lddr	move.l	a5,-(sp)		;EDB8 LDDR
	move.w	regb(regs),d1		;Grab count.
	move.w	regh(regs),d0		;Source
	lea	1(targbase,d0.l),a0
	move.w	regd(regs),d0		;Destination
	lea	1(targbase,d0.l),a5
	sub.w	d1,regh(regs)		;Adjust Z-80 registers now.
	sub.w	d1,regd(regs)
	clr.w	regb(regs)
	andi.b	#$C1,regf
	subq.w	#1,d1			;Adjust count for DBRA.
lddrlop move.b	-(a0),-(a5)
	dbra	d1,lddrlop
	move.l	(sp)+,a5
	jmp	(return)

cpdr	move.w	regb(regs),d1		;EDB9 CPDR
	subq.w	#1,d1			;Grab count, adjust for DBRA.
	move.w	regh(regs),d0		;Source
	lea	1(targbase,d0.l),a0
cpdrlop subq.w	#1,d0
	cmp.b	-(a0),rega
	dbeq	d1,cpdrlop
	bne.s	cpdrnom
	move.w	d0,regh(regs)		;Restore Z-80 registers.
	move.w	d1,regb(regs)
	beq.s	1$
	moveq	#$46,regf		;Found, in the string.
	jmp	(return)
1$	moveq	#$42,regf		;Found, but at last place.
	jmp	(return)
cpdrnom move.w	d0,regh(regs)
	addq.w	#1,d1
	move.w	d1,regb(regs)
	beq.s	2$
	moveq	#6,regf
	jmp	(return)
2$	moveq	#2,regf
	jmp	(return)

indr	move.b	regc(regs),d1		;EDBA INDR
	move.w	regh(regs),d0
	lea	0(targbase,d0.l),a0
	jsr	inp
	subq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	bne.s	indr
	jmp	(return)

otdr	move.b	regc(regs),d1		;EDBB OTDR
	move.w	regh(regs),d0
	lea	0(targbase,d0.l),a0
	jsr	outp
	subq.w	#1,regh(regs)
	subq.b	#1,regb(regs)
	bne.s	otdr
	jmp	(return)

dadiyb	move.w	regb(regs),d0		;FD09 DAD IY,B
	add.w	d0,regiy(regs)
	docyf

dadiyd	move.w	regd(regs),d0		;FD19 DAD IY,D
	add.w	d0,regiy(regs)
	docyf

lxiiy	move.b	(pseudopc)+,regyl(regs) ;FD21 LXI IY,nnnn
	move.b	(pseudopc)+,regyh(regs)
	jmp	(return)

siyd	lsxxd				;FD22 SIYD addr
	move.b	regyl(regs),(a0)+
	move.b	regyh(regs),(a0)
	jmp	(return)

inxiy	addq.w	#1,regiy(regs)		;FD23 INX IY
	jmp	(return)

inryh	addq.b	#1,regyh(regs)		;FD24 INR YH (undocumented)
	inrflag

dcryh	subq.b	#1,regyh(regs)		;FD25 DCR YH (undocumented)
	inrflag

mviyh	move.b	(pseudopc)+,regyh(regs) ;FD26 MVI YH,nn (undocumented)
	jmp	(return)

dadiyy	asl.w	regiy(regs)		;FD29 DAD IY,IY (multiply by 2)
	docyf

liyd	lsxxd				;FD2A LIYD addr
	move.b	(a0)+,regyl(regs)
	move.b	(a0),regyh(regs)
	jmp	(return)

dcxiy	subq.w	#1,regiy(regs)		;FD2B DCX IY
	jmp	(return)

inryl	addq.b	#1,regyl(regs)		;FD2C INR YL (undocumented)
	inrflag

dcryl	subq.b	#1,regyl(regs)		;FD2D DCR YL (undocumented)
	inrflag

mviyl	move.b	(pseudopc)+,regyl(regs) ;FD2E MVI YL,nn (undocumented)
	jmp	(return)

inriy	calcind y			;FD34 INR (IY+d)
	addq.b	#1,0(targbase,d0.l)
	inrflag

dcriy	calcind y			;FD35 DCR (IY+d)
	subq.b	#1,0(targbase,d0.l)
	inrflag

mviiy	calcind y			;FD36 MVI (IY+d),nn
	move.b	(pseudopc)+,0(targbase,d0.l)
	jmp	(return)

dadiys	move.l	pseudosp,d0		;FD39 DAD IY,SP
	sub.l	targbase,d0
	add.w	d0,regiy(regs)
	docyf

movbyh	move.b	regyh(regs),regb(regs)	;FD44 MOV B,YH (undocumented)
	jmp	(return)

movbyl	move.b	regyl(regs),regb(regs)	;FD45 MOV B,YL (undocumented)
	jmp	(return)

movbiy	calcind y			;FD46 MOV B,(IY+d)
	move.b	0(targbase,d0.l),regb(regs)
	jmp	(return)

movcyh	move.b	regyh(regs),regc(regs)	;FD4C MOV C,YH (undocumented)
	jmp	(return)

movcyl	move.b	regyl(regs),regc(regs)	;FD4D MOV C,YL (undocumented)
	jmp	(return)

movciy	calcind y			;FD4E MOV C,(IY+d)
	move.b	0(targbase,d0.l),regc(regs)
	jmp	(return)

movdyh	move.b	regyh(regs),regd(regs)	;FD54 MOV D,YH (undocumented)
	jmp	(return)

movdyl	move.b	regyl(regs),regd(regs)	;FD55 MOV D,YL (undocumented)
	jmp	(return)

movdiy	calcind y			;FD56 MOV D,(IY+d)
	move.b	0(targbase,d0.l),regd(regs)
	jmp	(return)

moveyh	move.b	regyh(regs),rege(regs)	;FD5C MOV E,YH (undocumented)
	jmp	(return)

moveyl	move.b	regyl(regs),rege(regs)	;FD5D MOV E,YL (undocumented)
	jmp	(return)

moveiy	calcind y			;FD5E MOV E,(IY+d)
	move.b	0(targbase,d0.l),rege(regs)
	jmp	(return)

movyhb	move.b	regb(regs),regyh(regs)	;FD60 MOV YH,B (undocumented)
	jmp	(return)

movyhc	move.b	regc(regs),regyh(regs)	;FD61 MOV YH,C (undocumented)
	jmp	(return)

movyhd	move.b	regd(regs),regyh(regs)	;FD62 MOV YH,D (undocumented)
	jmp	(return)

movyhe	move.b	rege(regs),regyh(regs)	;FD63 MOV YH,E (undocumented)
	jmp	(return)

mvyhyh	jmp	(return)		;FD64 MOV YH,YH (undocumented)

mvyhyl	move.b	regyl(regs),regyh(regs) ;FD65 MOV YH,YL (undocumented)
	jmp	(return)

movhiy	calcind y			;FD66 MOV H,(IY+d)
	move.b	0(targbase,d0.l),regh(regs)
	jmp	(return)

movylb	move.b	regb(regs),regyl(regs)	;FD68 MOV YL,B (undocumented)
	jmp	(return)

movylc	move.b	regc(regs),regyl(regs)	;FD69 MOV YL,C (undocumented)
	jmp	(return)

movyld	move.b	regd(regs),regyl(regs)	;FD6A MOV YL,D (undocumented)
	jmp	(return)

movyle	move.b	rege(regs),regyl(regs)	;FD6B MOV YL,E (undocumented)
	jmp	(return)

mvylyh	move.b	regyh(regs),regyl(regs) ;FD6C MOV YL,YH (undocumented)
	jmp	(return)

mvylyl	jmp	(return)		;FD6D MOV YL,YL (undocumented)

movliy	calcind y			;FD6E MOV L,(IY+d)
	move.b	0(targbase,d0.l),regl(regs)
	jmp	(return)

moviyb	calcind y			;FD70 MOV (IY+d),B
	move.b	regb(regs),0(targbase,d0.l)
	jmp	(return)

moviyc	calcind y			;FD71 MOV (IY+d),C
	move.b	regc(regs),0(targbase,d0.l)
	jmp	(return)

moviyd	calcind y			;FD72 MOV (IY+d),D
	move.b	regd(regs),0(targbase,d0.l)
	jmp	(return)

moviye	calcind y			;FD73 MOV (IY+d),E
	move.b	rege(regs),0(targbase,d0.l)
	jmp	(return)

moviyh	calcind y			;FD74 MOV (IY+d),H
	move.b	regh(regs),0(targbase,d0.l)
	jmp	(return)

moviyl	calcind y			;FD75 MOV (IY+d),L
	move.b	regl(regs),0(targbase,d0.l)
	jmp	(return)

moviya	calcind y			;FD77 MOV (IY+d),A
	move.b	rega,0(targbase,d0.l)
	jmp	(return)

movayh	move.b	regyh(regs),rega	;FD7C MOV A,YH (undocumented)
	jmp	(return)

movayl	move.b	regyl(regs),rega	;FD7D MOV A,YL (undocumented)
	jmp	(return)

movaiy	calcind y			;FD7E MOV A,(IY+d)
	move.b	0(targbase,d0.l),rega
	jmp	(return)

addyh	move.b	regyh(regs),d0		;FD84 ADD YH (undocumented)
	addflag

addyl	move.b	regyl(regs),d0		;FD85 ADD YL (undocumented)
	addflag

addiy	calcind y			;FD86 ADD (IY+d)
	move.b	0(targbase,d0.l),d0
	addflag

adcyh	move.b	regf,regop3(regs)	;FD8C ADC YH (undocumented)
	asr.b	#1,regf
	move.b	regyh(regs),d0
	adcflag

adcyl	move.b	regf,regop3(regs)	;FD8E ADC YL (undocumented)
	asr.b	#1,regf
	move.b	regyl(regs),d0
	adcflag

adciy	move.b	regf,regop3(regs)	;FD8E ADC (IY+d)
	calcind y
	asr.b	#1,regf
	move.b	0(targbase,d0.l),d0
	adcflag
	
subyh	sub.b	regyh(regs),rega	;FD94 SUB YH (undocumented)
	setflag

subyl	sub.b	regyl(regs),rega	;FD95 SUB YL (undocumented)
	setflag

subiy	calcind y			;FD96 SUB (IY+d)
	sub.b	0(targbase,d0.l),rega
	setflag

sbbyh	asr.b	#1,regf 		;FD9C SBB YH (undocumented)
	move.b	regyh(regs),d0
	sbbflag

sbbyl	asr.b	#1,regf 		;FD9D SBB YL (undocumented)
	move.b	regyl(regs),d0
	sbbflag

sbbiy	calcind y			;FD9E SBB (IY+d)
	asr.b	#1,regf
	move.b	0(targbase,d0.l),d0
	sbbflag

anayh	and.b	regyh(regs),rega	;FDA4 ANA YH (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

anayl	and.b	regyl(regs),rega	;FDA5 ANA YL (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

anaiy	calcind y			;FDA6 ANA (IY+d)
	and.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrayh	move.b	regyh(regs),d0		;FDAC XRA YH (undocumented)
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xrayl	move.b	regyl(regs),d0		;FDAD XRA YL (undocumented)
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

xraiy	calcind y			;FDAE XRA (IY+d)
	move.b	0(targbase,d0.l),d0
	eor.b	d0,rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orayh	or.b	regyh(regs),rega	;FDB4 ORA YH (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

orayl	or.b	regyl(regs),rega	;FDB5 ORA YL (undocumented)
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

oraiy	calcind	y			;FDB6 ORA (IY+d)
	or.b	0(targbase,d0.l),rega
	move.b	16(flagptr,rega.w),regf
	jmp	(return)

cmpyh	cmp.b	regyh(regs),rega	;FDBC CMP YH (undocumented)
	setflag

cmpyl	cmp.b	regyl(regs),rega	;FDBD CMP YL (undocumented)
	setflag

cmpiy	calcind	y			;FDBE CMP (IY+d)
	cmp.b	0(targbase,d0.l),rega
	setflag

* FDCB-prefix instructions can use the DDCB-prefix routines,
*  since the operand address is already in D0.

popiy	move.b	(pseudosp)+,regyl(regs) ;FDE1 POP IY
	move.b	(pseudosp)+,regyh(regs)
	jmp	(return)

xtiy	move.b	regyl(regs),d0		;FDE3 XTIY
	move.b	(pseudosp),regyl(regs)
	move.b	d0,(pseudosp)
	move.b	regyh(regs),d0
	move.b	1(pseudosp),regyh(regs)
	move.b	d0,1(pseudosp)
	jmp	(return)

pushiy	move.b	regyh(regs),-(pseudosp) ;FDE5 PUSH IY
	move.b	regyl(regs),-(pseudosp)
	jmp	(return)

pciy	move.w	regiy(regs),d0		;FDE9 PCIY
	lea	0(targbase,d0.l),pseudopc
	jmp	(return)

spiy	move.w	regiy(regs),d0		;FDF9 SPIY
	lea	0(targbase,d0.l),pseudosp
	jmp	(return)
	page
*************************************************************************
*									*
*	Opcode dispatch table						*
*									*
*************************************************************************
	data	data

	even
optabl	dc.l	nop00,lxib,staxb,inxb,inrb,dcrb,mvib,rlc
	dc.l	exaf,dadb,ldaxb,dcxb,inrc,dcrc,mvic,rrc
	dc.l	djnz,lxid,staxd,inxd,inrd,dcrd,mvid,ral
	dc.l	jr,dadd,ldaxd,dcxd,inre,dcre,mvie,rar
	dc.l	jrnz,lxih,shld,inxh,inrh,dcrh,mvih,daa
	dc.l	jrz,dadh,lhld,dcxh,inrl,dcrl,mvil,cma
	dc.l	jrnc,lxis,sta,inxs,inrm,dcrm,mvim,stc
	dc.l	jrc,dads,lda,dcxs,inra,dcra,mvia,cmc
	dc.l	movebb,movebc,movebd,movebe,movebh,movebl,movebm,moveba
	dc.l	movecb,movecc,movecd,movece,movech,movecl,movecm,moveca
	dc.l	movedb,movedc,movedd,movede,movedh,movedl,movedm,moveda
	dc.l	moveeb,moveec,moveed,moveee,moveeh,moveel,moveem,moveea
	dc.l	movehb,movehc,movehd,movehe,movehh,movehl,movehm,moveha
	dc.l	movelb,movelc,moveld,movele,movelh,movell,movelm,movela
	dc.l	movemb,movemc,movemd,moveme,movemh,moveml,halt,movema
	dc.l	moveab,moveac,movead,moveae,moveah,moveal,moveam,moveaa
	dc.l	addb,addc,addd,adde,addh,addl,addm,adda
	dc.l	adcb,adcc,adcd,adce,adch,adcl,adcm,adca
	dc.l	subb,subc,subd,sube,subh,subl,subm,suba
	dc.l	sbbb,sbbc,sbbd,sbbe,sbbh,sbbl,sbbm,sbba
	dc.l	andb,andc,andd,ande,andh,andl,andm,anda
	dc.l	xrab,xrac,xrad,xrae,xrah,xral,xram,xraa
	dc.l	orab,orac,orad,orae,orah,oral,oram,oraa
	dc.l	cmpb,cmpc,cmpd,cmpe,cmph,cmpl,cmpam,cmpaa
	dc.l	rnz,popb,jnz,jmpa,cnz,pushb,adi,rst0
	dc.l	rz,ret,jz,preCB,cz,call,aci,rst1
	dc.l	rnc,popd,jnc,out,cnc,pushd,sui,rst2
	dc.l	rc,exx,jc,in,cc,preDD,sbi,rst3
	dc.l	rpo,poph,jpo,xthl,cpo,pushh,ani,rst4
	dc.l	rpe,pchl,jpe,xchg,cpe,preED,xri,rst5
	dc.l	rp,popp,jp,di,cp,pushp,oria,rst6
	dc.l	rm,sphl,jm,ei,cm,preFD,cpi,rst7
	page
*************************************************************************
*									*
*	Flag register lookup tables					*
*									*
*************************************************************************
flags	dc.b	$00,$01,$04,$05,$40,$41,$44,$45,$80,$81,$84,$85,$C0,$C1,$C4,$C5
	dc.b	$44,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
	dc.b	$00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
	dc.b	$00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
	dc.b	$04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
	dc.b	$00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
	dc.b	$04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
	dc.b	$04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
	dc.b	$00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
	dc.b	$80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
	dc.b	$84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
	dc.b	$84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
	dc.b	$80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
	dc.b	$84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
	dc.b	$80,$84,$84,$80,$84,$80,$84,$80,$84,$80,$80,$84,$80,$84,$84,$80
	dc.b	$80,$84,$84,$80,$84,$80,$84,$80,$84,$80,$80,$84,$80,$84,$84,$80
	dc.b	$84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
	page
*************************************************************************
*									*
*	Z-80 opcode dispatch table.  One longword entry per opcode	*
*	of the target (Z-80) processor, including illegals.		*
*									*
*************************************************************************

CBoptab:
	dc.l	rlcb,rlcc,rlcd,rlce,rlch,rlcl,rlcm,rlca 		;CB00
	dc.l	rrcb,rrcc,rrcd,rrce,rrch,rrcl,rrcm,rrca 		;CB08
	dc.l	rlrb,rlrc,rlrd,rlre,rlrh,rlrl,rlrm,rlra 		;CB10
	dc.l	rrrb,rrrc,rrrd,rrre,rrrh,rrrl,rrrm,rrra 		;CB18
	dc.l	slab,slac,slad,slae,slah,slal,slam,slaa 		;CB20
	dc.l	srab,srac,srad,srae,srah,sral,sram,sraa 		;CB28
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;CB30
	dc.l	srlb,srlc,srld,srle,srlh,srll,srlm,srla 		;CB38
	dc.l	bit0b,bit0c,bit0d,bit0e,bit0h,bit0l,bit0m,bit0a 	;CB40
	dc.l	bit1b,bit1c,bit1d,bit1e,bit1h,bit1l,bit1m,bit1a 	;CB48
	dc.l	bit2b,bit2c,bit2d,bit2e,bit2h,bit2l,bit2m,bit2a 	;CB50
	dc.l	bit3b,bit3c,bit3d,bit3e,bit3h,bit3l,bit3m,bit3a 	;CB58
	dc.l	bit4b,bit4c,bit4d,bit4e,bit4h,bit4l,bit4m,bit4a 	;CB60
	dc.l	bit5b,bit5c,bit5d,bit5e,bit5h,bit5l,bit5m,bit5a 	;CB68
	dc.l	bit6b,bit6c,bit6d,bit6e,bit6h,bit6l,bit6m,bit6a 	;CB70
	dc.l	bit7b,bit7c,bit7d,bit7e,bit7h,bit7l,bit7m,bit7a 	;CB78
	dc.l	res0b,res0c,res0d,res0e,res0h,res0l,res0m,res0a 	;CB80
	dc.l	res1b,res1c,res1d,res1e,res1h,res1l,res1m,res1a 	;CB88
	dc.l	res2b,res2c,res2d,res2e,res2h,res2l,res2m,res2a 	;CB90
	dc.l	res3b,res3c,res3d,res3e,res3h,res3l,res3m,res3a 	;CB98
	dc.l	res4b,res4c,res4d,res4e,res4h,res4l,res4m,res4a 	;CBA0
	dc.l	res5b,res5c,res5d,res5e,res5h,res5l,res5m,res5a 	;CBA8
	dc.l	res6b,res6c,res6d,res6e,res6h,res6l,res6m,res6a 	;CBB0
	dc.l	res7b,res7c,res7d,res7e,res7h,res7l,res7m,res7a 	;CBB8
	dc.l	set0b,set0c,set0d,set0e,set0h,set0l,set0m,set0a 	;CBC0
	dc.l	set1b,set1c,set1d,set1e,set1h,set1l,set1m,set1a 	;CBC8
	dc.l	set2b,set2c,set2d,set2e,set2h,set2l,set2m,set2a 	;CBD0
	dc.l	set3b,set3c,set3d,set3e,set3h,set3l,set3m,set3a 	;CBD8
	dc.l	set4b,set4c,set4d,set4e,set4h,set4l,set4m,set4a 	;CBE0
	dc.l	set5b,set5c,set5d,set5e,set5h,set5l,set5m,set5a 	;CBE8
	dc.l	set6b,set6c,set6d,set6e,set6h,set6l,set6m,set6a 	;CBF0
	dc.l	set7b,set7c,set7d,set7e,set7h,set7l,set7m,set7a 	;CBF8

DDoptab:
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DD00
	dc.l	illgED,dadixb,illgED,illgED,illgED,illgED,illgED,illgED ;DD08
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DD10
	dc.l	illgED,dadixd,illgED,illgED,illgED,illgED,illgED,illgED ;DD18
	dc.l	illgED,lxiix,sixd,inxix,inrxh,dcrxh,mvixh,illgED	;DD20
	dc.l	illgED,dadixx,lixd,dcxix,inrxl,dcrxl,mvixl,illgED	;DD28
	dc.l	illgED,illgED,illgED,illgED,inrix,dcrix,mviix,illgED	;DD30
	dc.l	illgED,dadixs,illgED,illgED,illgED,illgED,illgED,illgED ;DD38
	dc.l	illgED,illgED,illgED,illgED,movbxh,movbxl,movbix,illgED ;DD40
	dc.l	illgED,illgED,illgED,illgED,movcxh,movcxl,movcix,illgED ;DD48
	dc.l	illgED,illgED,illgED,illgED,movdxh,movdxl,movdix,illgED ;DD50
	dc.l	illgED,illgED,illgED,illgED,movexh,movexl,moveix,illgED ;DD58
	dc.l	movxhb,movxhc,movxhd,movxhe,mvxhxh,mvxhxl,movhix,illgED ;DD60
	dc.l	movxlb,movxlc,movxld,movxle,mvxlxh,mvxlxl,movlix,illgED ;DD60
	dc.l	movixb,movixc,movixd,movixe,movixh,movixl,illgED,movixa ;DD70
	dc.l	illgED,illgED,illgED,illgED,movaxh,movaxl,movaix,illgED ;DD78
	dc.l	illgED,illgED,illgED,illgED,addxh,addxl,addix,illgED	;DD80
	dc.l	illgED,illgED,illgED,illgED,adcxh,adcxl,adcix,illgED	;DD88
	dc.l	illgED,illgED,illgED,illgED,subxh,subxl,subix,illgED	;DD90
	dc.l	illgED,illgED,illgED,illgED,sbbxh,sbbxl,sbbix,illgED	;DD98
	dc.l	illgED,illgED,illgED,illgED,anaxh,anaxl,anaix,illgED	;DDA0
	dc.l	illgED,illgED,illgED,illgED,xraxh,xraxl,xraix,illgED	;DDA8
	dc.l	illgED,illgED,illgED,illgED,oraxh,oraxl,oraix,illgED	;DDB0
	dc.l	illgED,illgED,illgED,illgED,cmpxh,cmpxl,cmpix,illgED	;DDB8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDC0
	dc.l	illgED,illgED,illgED,prDDCB,illgED,illgED,illgED,illgED ;DDC8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDD0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDD8
	dc.l	illgED,popix,illgED,xtix,illgED,pushix,illgED,illgED	;DDE0
	dc.l	illgED,pcix,illgED,illgED,illgED,illgED,illgED,illgED	;DDE8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDF0
	dc.l	illgED,spix,illgED,illgED,illgED,illgED,illgED,illgED	;DDF8

DDCBopt:
	dc.l	rlcix,rrcix,rlix,rrix,slaix,sraix,ilgDDCB,srlix 	;DDCB06
	dc.l	bit0ix,bit1ix,bit2ix,bit3ix,bit4ix,bit5ix,bit6ix,bit7ix ;DDCB46
	dc.l	res0ix,res1ix,res2ix,res3ix,res4ix,res5ix,res6ix,res7ix ;DDCB86
	dc.l	set0ix,set1ix,set2ix,set3ix,set4ix,set5ix,set6ix,set7ix ;DDCBC6

EDoptab:
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED00
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED08
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED10
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED18
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED20
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED28
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED30
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED38
	dc.l	inbc,outcb,dsbbb,sbcd,nega,retn,im0,moveia		;ED40
	dc.l	incc,outcc,dadcb,lbcd,illgED,reti,illgED,movera 	;ED48
	dc.l	indc,outcd,dsbbd,sded,illgED,illgED,im1,moveai		;ED50
	dc.l	inec,outce,dadcd,lded,illgED,illgED,im2,movear		;ED58
	dc.l	inhc,outch,dsbbh,shld,illgED,illgED,illgED,rrd		;ED60
	dc.l	inlc,outcl,dadch,lhld,illgED,illgED,illgED,rld		;ED68
	dc.l	inmc,outcm,dsbbs,sspd,illgED,illgED,illgED,illgED	;ED70
	dc.l	inac,outca,dadcs,lspd,illgED,illgED,illgED,illgED	;ED78
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED80
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED88
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED90
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED98
	dc.l	ldi,cmpi,ini,outi,illgED,illgED,illgED,illgED		;EDA0
	dc.l	ldd,cpd,ind,outd,illgED,illgED,illgED,illgED		;EDA8
	dc.l	ldir,cpir,inir,otir,illgED,illgED,illgED,illgED 	;EDB0
	dc.l	lddr,cpdr,indr,otdr,illgED,illgED,illgED,illgED 	;EDB8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDC0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDC8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDD0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDD8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDE0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDE8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDF0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDF8

FDoptab:
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FD00
	dc.l	illgED,dadiyb,illgED,illgED,illgED,illgED,illgED,illgED ;FD08
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FD10
	dc.l	illgED,dadiyd,illgED,illgED,illgED,illgED,illgED,illgED ;FD18
	dc.l	illgED,lxiiy,siyd,inxiy,inryh,dcryh,mviyh,illgED	;FD20
	dc.l	illgED,dadiyy,liyd,dcxiy,inryl,dcryl,mviyl,illgED	;FD28
	dc.l	illgED,illgED,illgED,illgED,inriy,dcriy,mviiy,illgED	;FD30
	dc.l	illgED,dadiys,illgED,illgED,illgED,illgED,illgED,illgED ;FD38
	dc.l	illgED,illgED,illgED,illgED,movbyh,movbyl,movbiy,illgED ;FD40
	dc.l	illgED,illgED,illgED,illgED,movcyh,movcyl,movciy,illgED ;FD48
	dc.l	illgED,illgED,illgED,illgED,movdyh,movdyl,movdiy,illgED ;FD50
	dc.l	illgED,illgED,illgED,illgED,moveyh,moveyl,moveiy,illgED ;FD58
	dc.l	movyhb,movyhc,movyhd,movyhe,mvyhyh,mvyhyl,movhiy,illgED ;FD60
	dc.l	movylb,movylc,movyld,movyle,mvylyh,mvylyl,movliy,illgED ;FD60
	dc.l	moviyb,moviyc,moviyd,moviye,moviyh,moviyl,illgED,moviya ;FD70
	dc.l	illgED,illgED,illgED,illgED,movayh,movayl,movaiy,illgED ;FD78
	dc.l	illgED,illgED,illgED,illgED,addyh,addyl,addiy,illgED	;FD80
	dc.l	illgED,illgED,illgED,illgED,adcyh,adcyl,adciy,illgED	;FD88
	dc.l	illgED,illgED,illgED,illgED,subyh,subyl,subiy,illgED	;FD90
	dc.l	illgED,illgED,illgED,illgED,sbbyh,sbbyl,sbbiy,illgED	;FD98
	dc.l	illgED,illgED,illgED,illgED,anayh,anayl,anaiy,illgED	;FDA0
	dc.l	illgED,illgED,illgED,illgED,xrayh,xrayl,xraiy,illgED	;FDA8
	dc.l	illgED,illgED,illgED,illgED,orayh,orayl,oraiy,illgED	;FDB0
	dc.l	illgED,illgED,illgED,illgED,cmpyh,cmpyl,cmpiy,illgED	;FDB8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDC0
	dc.l	illgED,illgED,illgED,prFDCB,illgED,illgED,illgED,illgED ;FDC8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDD0
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDD8
	dc.l	illgED,popiy,illgED,xtiy,illgED,pushiy,illgED,illgED	;FDE0
	dc.l	illgED,pciy,illgED,illgED,illgED,illgED,illgED,illgED	;FDE8
	dc.l	illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDF0
	dc.l	illgED,spiy,illgED,illgED,illgED,illgED,illgED,illgED	;FDF8

* FDCBopt is not necessary - DDCBopt can be used,
*  since the operand address is in D0 for both routines.


	bss	bss

	even

tracesad ds.l	1		;Start address for trace
traceead ds.l	1		;End address for trace
traceflg ds.b	1		;Trace activity flag
tmprega  ds.b	1		;Work area
	end
