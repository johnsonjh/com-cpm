*************************************************************************
*                                                                       *
*       This file contains the target processor (8080/Z80)              *
*       simulation routines.                                            *
*                                                                       *
*************************************************************************
* Last revised June 23, 1988.

        xdef    optabl,flags,mloop,traceit,tracesad,traceead,traceflg
        xref    illegl,service,dump

        ifd     tracehd
trace   equ     1               ;Include trace routines.
        endc
        ifnd    tracehd
trace   equ     0               ;Don't include trace routines.
        endc

        INCLUDE   "ecpmdefs.i"
        page
*************************************************************************
*                                                                       *
*       Macros used by opcode simulation routines                       *
*                                                                       *
*       These generate a lot of repetitive code sequences,              *
*       but it's faster than doing subroutine calls.                    *
*                                                                       *
*************************************************************************

setflag macro                   ;Set 8080 flags from 68000 flags.
        move    sr,d0
        and.w   regcon0f,d0
        move.b  0(flagptr,d0.w),regf
        jmp     (return)
        endm

inrflag macro                   ;Set flags after increment or decrement.
        move    sr,d0
        and.w   regcon0e,d0
        and.w   regcon01,regf
        or.b    0(flagptr,d0.w),regf
        jmp     (return)
        endm

addflag macro                   ;Add and set flags.
        move.b  d0,regop1(regs)
        move.b  rega,regop2(regs)
        move.b  regcon0e,regop3(regs)
        add.b   d0,rega
        setflag
        endm

adcflag macro                   ;Add with carry and set flags.
        move.b  d0,regop1(regs)
        move.b  rega,regop2(regs)
        moveq   #0,d1           ;Set Zero flag for ADDX.
        addx.b  d0,rega
        setflag
        endm

ashl    macro
        asr.b   #1,regf
        move.w  reg\2(regs),d0
        move.w  regh(regs),d1
        ori     #4,ccr
        \1x.w  d0,d1
        move    sr,d0
        and.w   regcon0f,d0
        move.b  0(flagptr,d0.w),regf
        move.w  d1,regh(regs)
        jmp     (return)
        endm

sbbflag macro                   ;Subtract with borrow and set flags.
        moveq   #0,d1           ;Set Zero flag for SUBX.
        subx.b  d0,rega
        setflag
        endm

ret80   macro                   ;Return from subroutine.
        move.b  1(pseudosp),d0
        rol.w   #8,d0
        move.b  (pseudosp),d0
        addq.l  #2,pseudosp
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)
        endm

lsxxd   macro
        move.b  1(pseudopc),d0
        rol.w   #8,d0
        move.b  (pseudopc),d0
        addq.l  #2,pseudopc
        move.l  d0,a0
        adda.l  targbase,a0
        endm

jmpaddr macro                   ;Get address for possible jump or call.
        move.b  1(pseudopc),d0
        rol.w   #8,d0
        move.b  (pseudopc),d0
        addq.l  #2,pseudopc
        endm

call80  macro                   ;Call a subroutine
        move.l  pseudopc,d1
        sub.l   targbase,d1
        move.b  d1,-2(pseudosp)
        rol.w   #8,d1
        move.b  d1,-1(pseudosp)
        subq.l  #2,pseudosp
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)
        endm

rst80   macro                   ;8080 RST instruction
        move.l  pseudopc,d1
        sub.l   targbase,d1
        move.b  d1,-2(pseudosp)
        rol.w   #8,d1
        move.b  d1,-1(pseudosp)
        subq.l  #2,pseudosp
        lea     \1(targbase),pseudopc
        jmp     (return)
        endm

docyf   macro                   ;Copy 68000's carry flag to 8080
        bcs.s   1$
        bclr    #0,regf
        jmp     (return)
1$      bset    #0,regf
        jmp     (return)
        endm

jraddr  macro                   ;Set up relative address
        move.b  (pseudopc)+,d0
        ext.w   d0
        ext.l   d0
        endm

gopc    macro
        add.l   d0,pseudopc
        jmp     (return)
        endm

calcind macro
        move.b  (pseudopc)+,d1
        ext.w   d1
        move.w  regi\1(regs),d0
        add.w   d1,d0
        endm

rotflag macro
        move    sr,d1
        and.w   regcon0f,d1
        move.b  0(flagptr,d1.w),regf
        endm
        page
*************************************************************************
*                                                                       *
*       Opcode dispatcher                                               *
*                                                                       *
*************************************************************************
        code
mloop:
        ifne    trace           ;Optional trace
        tst.b   traceflg
        bne.s   dotrace
        cmpa.l  tracesad,pseudopc
        bne.s   notrace
        move.b  #1,traceflg
dotrace jsr     dump
        cmpa.l  traceead,pseudopc
        bne.s   notrace
        clr.b   traceflg
notrace equ     *
        endc

        moveq   #0,d0           ;Execute appropriate simulation subroutine.
        move.b  (pseudopc)+,d0  ;Grab the next opcode.
        asl     #2,d0           ;(D0 high word is still zero!)
        move.l  0(opptr,d0.w),a0
        jmp     (a0)            ;Do the subroutine.
        page
*************************************************************************
*                                                                       *
*       Opcode simulation routines                                      *
*                                                                       *
*       Note:  I/O instructions are based as 68000 address $FF0000      *
*               as is appropriate for the Compupro CPU-68K card.        *
*                                                                       *
*       Also, all routines assume that the high word of D0 is zero!     *
*                                                                       *
*************************************************************************

        even

nop00   jmp     (return)                ;00 NOP

lxib    move.b  (pseudopc)+,regc(regs)  ;01 LXI B,nnnn
        move.b  (pseudopc)+,regb(regs)
        jmp     (return)

staxb   move.w  regb(regs),d0           ;02 STAX B
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

inxb    addq.w  #1,regb(regs)           ;03 INX B
        jmp     (return)

inrb    addq.b  #1,regb(regs)           ;04 INR B
        inrflag

dcrb    subq.b  #1,regb(regs)           ;05 DCR B
        inrflag

mvib    move.b  (pseudopc)+,regb(regs)  ;06 MVI B,nn
        jmp     (return)

rlca    rol.b   #1,rega                 ;07 RLC
        docyf

dadb    move.w  regb(regs),d0           ;09 DAD B
        add.w   d0,regh(regs)
        docyf

ldaxb   move.w  regb(regs),d0           ;0A LDAX B
        move.b  0(targbase,d0.l),rega
        jmp     (return)

dcxb    subq.w  #1,regb(regs)           ;0B DCX B
        jmp     (return)

inrc    addq.b  #1,regc(regs)           ;0C INR C
        inrflag

dcrc    subq.b  #1,regc(regs)           ;0D DCR C
        inrflag

mvic    move.b  (pseudopc)+,regc(regs)  ;0E MVI C
        jmp     (return)

rrca    ror.b   #1,rega                 ;0F RRC
        docyf

lxid    move.b  (pseudopc)+,rege(regs)  ;11 LXI D,nnnn
        move.b  (pseudopc)+,regd(regs)
        jmp     (return)

staxd   move.w  regd(regs),d0           ;12 STAX D
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

inxd    addq.w  #1,regd(regs)           ;13 INX D
        jmp     (return)

inrd    addq.b  #1,regd(regs)           ;14 INR D
        inrflag

dcrd    subq.b  #1,regd(regs)           ;15 DCR D
        inrflag

mvid    move.b  (pseudopc)+,regd(regs)  ;16 MVI D,nn
        jmp     (return)

ral     roxr.b  #1,regf                 ;17 RAL
        roxl.b  #1,rega
        roxl.b  #1,regf
        jmp     (return)

;18 JR (Z80 opcode)

dadd    move.w  regd(regs),d0           ;19 DAD D
        add.w   d0,regh(regs)
        docyf

ldaxd   move.w  regd(regs),d0           ;1A LDAX D
        move.b  0(targbase,d0.l),rega
        jmp     (return)

dcxd    subq.w  #1,regd(regs)           ;1B DCX D
        jmp     (return)

inre    addq.b  #1,rege(regs)           ;1C INR E
        inrflag

dcre    subq.b  #1,rege(regs)           ;1D DCR E
        inrflag

mvie    move.b  (pseudopc)+,rege(regs)  ;1E MVI E
        jmp     (return)

rar     roxr.b  #1,regf                 ;1F RAR
        roxr.b  #1,rega
        roxl.b  #1,regf
        jmp     (return)

;20 JRNZ (Z80 opcode)

lxih    move.b  (pseudopc)+,regl(regs)  ;21 LXI H,nnnn
        move.b  (pseudopc)+,regh(regs)
        jmp     (return)

shld    lsxxd                           ;22 SHLD addr
        move.b  regl(regs),(a0)+
        move.b  regh(regs),(a0)+
        jmp     (return)

inxh    addq.w  #1,regh(regs)           ;23 INX H
        jmp     (return)

inrh    addq.b  #1,regh(regs)           ;24 INR H
        inrflag

dcrh    subq.b  #1,regh(regs)           ;25 DCR H
        inrflag

mvih    move.b  (pseudopc)+,regh(regs)  ;26 MVI H,nn
        jmp     (return)

daa     move.b  regop3(regs),d0         ;27 DAA
        roxr.b  #1,d0
        move.b  regop2(regs),d0
        move.b  regop1(regs),d1
        swap    regcon0e
        move.b  rega,regcon0e
        and.b   regcon0f,regcon0e
        cmp.b   #9,regcon0e
        bhi.s   halfcy
        and.b   regcon0f,d0
        and.b   regcon0f,d1
        ori.b   #$F0,d1
        addx.b  d0,d1
        bcc.s   nohalf
halfcy  add.b   #6,rega
        bcs.s   fullcy
nohalf  btst    #0,regf
        bne.s   fullcy
        move.b  rega,regcon0e
        and.b   #$F0,regcon0e
        cmp.b   #$90,regcon0e
        bls.s   nofull
fullcy  add.b   #$60,rega
        ori     #1,ccr
enddaa  move    sr,regf
        swap    regcon0e
        and.w   regcon0f,regf
        move.b  0(flagptr,regf.w),regf
        jmp     (return)
nofull  tst.b   rega
        bra.s   enddaa

;28 JRZ (Z80 opcode)

dadh    asl.w   regh(regs)              ;29 DAD H (multiply by 2)
        docyf

lhld    lsxxd                           ;2A LHLD addr
        move.b  (a0)+,regl(regs)
        move.b  (a0),regh(regs)
        jmp     (return)

dcxh    subq.w  #1,regh(regs)           ;2B DCX H
        jmp     (return)

inrl    addq.b  #1,regl(regs)           ;2C INR L
        inrflag

dcrl    subq.b  #1,regl(regs)           ;2D DCR L
        inrflag

mvil    move.b  (pseudopc)+,regl(regs)  ;2E MVI L,nn
        jmp     (return)

cma     not.b   rega                    ;2F CMA
        jmp     (return)

;30 JRNC (Z80 opcode)

lxis    move.b  1(pseudopc),d0          ;31 LXI SP,nnnn
        rol.w   #8,d0
        move.b  (pseudopc),d0
        addq.l  #2,pseudopc
        move.l  d0,pseudosp
        adda.l  targbase,pseudosp
        jmp     (return)

sta     move.b  1(pseudopc),d0          ;32 STA addr
        rol.w   #8,d0
        move.b  (pseudopc),d0
        addq.l  #2,pseudopc
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

inxs    addq.l  #1,pseudosp             ;33 INX SP
        jmp     (return)

inrm    move.w  regh(regs),d0           ;34 INR M
        addq.b  #1,0(targbase,d0.l)
        inrflag

dcrm    move.w  regh(regs),d0           ;35 DCR M
        subq.b  #1,0(targbase,d0.l)
        inrflag

mvim    move.w  regh(regs),d0           ;36 MVI M,nn
        move.b  (pseudopc)+,0(targbase,d0.l)
        jmp     (return)

stc     bset    #0,regf                 ;37 STC
        jmp     (return)

;38 JRC (Z80 opcode)

dads    move.l  pseudosp,d0             ;39 DAD SP
        sub.l   targbase,d0
        add.w   d0,regh(regs)
        docyf

lda     move.b  1(pseudopc),d0          ;3A LDA addr
        rol.w   #8,d0
        move.b  (pseudopc),d0
        addq.l  #2,pseudopc
        move.b  0(targbase,d0.l),rega
        jmp     (return)

dcxs    subq.l  #1,pseudosp             ;3B DCX SP
        jmp     (return)

inra    move.b  rega,regop1(regs)       ;3C INR A
        move.b  regcon01,regop2(regs)
        move.b  regcon0e,regop3(regs)
        addq.b  #1,rega
        inrflag

dcra    subq.b  #1,rega                 ;3D DCR A
        inrflag

mvia    move.b  (pseudopc)+,rega        ;3E MVI A
        jmp     (return)

cmc     bchg    #0,regf                 ;3F CMC
        jmp     (return)

movebb  move.b  regb(regs),regb(regs)   ;40 MOV B,B
        jmp     (return)

movebc  move.b  regc(regs),regb(regs)   ;41 MOV B,C
        jmp     (return)

movebd  move.b  regd(regs),regb(regs)   ;42 MOV B,D
        jmp     (return)

movebe  move.b  rege(regs),regb(regs)   ;43 MOV B,E
        jmp     (return)

movebh  move.b  regh(regs),regb(regs)   ;44 MOV B,H
        jmp     (return)

movebl  move.b  regl(regs),regb(regs)   ;45 MOV B,L
        jmp     (return)

movebm  move.w  regh(regs),d0           ;46 MOV B,M
        move.b  0(targbase,d0.l),regb(regs)
        jmp     (return)

moveba  move.b  rega,regb(regs)         ;47 MOV B,A
        jmp     (return)

movecb  move.b  regb(regs),regc(regs)   ;48 MOV C,B
        jmp     (return)

movecc  move.b  regc(regs),regc(regs)   ;49 MOV C,C
        jmp     (return)

movecd  move.b  regd(regs),regc(regs)   ;4A MOV C,D
        jmp     (return)

movece  move.b  rege(regs),regc(regs)   ;4B MOV C,E
        jmp     (return)

movech  move.b  regh(regs),regc(regs)   ;4C MOV C,H
        jmp     (return)

movecl  move.b  regl(regs),regc(regs)   ;4D MOV C,L
        jmp     (return)

movecm  move.w  regh(regs),d0           ;4E MOV C,M
        move.b  0(targbase,d0.l),regc(regs)
        jmp     (return)

moveca  move.b  rega,regc(regs)         ;4F MOV C,A
        jmp     (return)

movedb  move.b  regb(regs),regd(regs)   ;50 MOV D,B
        jmp     (return)

movedc  move.b  regc(regs),regd(regs)   ;51 MOV D,C
        jmp     (return)

movedd  move.b  regd(regs),regd(regs)   ;52 MOV D,D
        jmp     (return)

movede  move.b  rege(regs),regd(regs)   ;53 MOV D,E
        jmp     (return)

movedh  move.b  regh(regs),regd(regs)   ;54 MOV D,H
        jmp     (return)

movedl  move.b  regl(regs),regd(regs)   ;55 MOV D,L
        jmp     (return)

movedm  move.w  regh(regs),d0           ;56 MOV D,M
        move.b  0(targbase,d0.l),regd(regs)
        jmp     (return)

moveda  move.b  rega,regd(regs)         ;57 MOV D,A
        jmp     (return)

moveeb  move.b  regb(regs),rege(regs)   ;58 MOV E,B
        jmp     (return)

moveec  move.b  regc(regs),rege(regs)   ;59 MOV E,C
        jmp     (return)

moveed  move.b  regd(regs),rege(regs)   ;5A MOV E,D
        jmp     (return)

moveee  move.b  rege(regs),rege(regs)   ;5B MOV E,E
        jmp     (return)

moveeh  move.b  regh(regs),rege(regs)   ;5C MOV E,H
        jmp     (return)

moveel  move.b  regl(regs),rege(regs)   ;5D MOV E,L
        jmp     (return)

moveem  move.w  regh(regs),d0           ;5E MOV E,M
        move.b  0(targbase,d0.l),rege(regs)
        jmp     (return)

moveea  move.b  rega,rege(regs)         ;5F MOV E,A
        jmp     (return)

movehb  move.b  regb(regs),regh(regs)   ;60 MOV H,B
        jmp     (return)

movehc  move.b  regc(regs),regh(regs)   ;61 MOV H,C
        jmp     (return)

movehd  move.b  regd(regs),regh(regs)   ;62 MOV H,D
        jmp     (return)

movehe  move.b  rege(regs),regh(regs)   ;63 MOV H,E
        jmp     (return)

movehh  move.b  regh(regs),regh(regs)   ;64 MOV H,H
        jmp     (return)

movehl  move.b  regl(regs),regh(regs)   ;65 MOV H,L
        jmp     (return)

movehm  move.w  regh(regs),d0           ;66 MOV H,M
        move.b  0(targbase,d0.l),regh(regs)
        jmp     (return)

moveha  move.b  rega,regh(regs)         ;67 MOV H,A
        jmp     (return)

movelb  move.b  regb(regs),regl(regs)   ;68 MOV L,B
        jmp     (return)

movelc  move.b  regc(regs),regl(regs)   ;69 MOV L,C
        jmp     (return)

moveld  move.b  regd(regs),regl(regs)   ;6A MOV L,D
        jmp     (return)

movele  move.b  rege(regs),regl(regs)   ;6B MOV L,E
        jmp     (return)

movelh  move.b  regh(regs),regl(regs)   ;6C MOV L,H
        jmp     (return)

movell  move.b  regl(regs),regl(regs)   ;6D MOV L,L
        jmp     (return)

movelm  move.w  regh(regs),d0           ;6E MOV L,M
        move.b  0(targbase,d0.l),regl(regs)
        jmp     (return)

movela  move.b  rega,regl(regs)         ;6F MOV L,A
        jmp     (return)

movemb  move.w  regh(regs),d0           ;70 MOV M,B
        move.b  regb(regs),0(targbase,d0.l)
        jmp     (return)

movemc  move.w  regh(regs),d0           ;71 MOV M,C
        move.b  regc(regs),0(targbase,d0.l)
        jmp     (return)

movemd  move.w  regh(regs),d0           ;72 MOV M,D
        move.b  regd(regs),0(targbase,d0.l)
        jmp     (return)

moveme  move.w  regh(regs),d0           ;73 MOV M,E
        move.b  rege(regs),0(targbase,d0.l)
        jmp     (return)

movemh  move.w  regh(regs),d0           ;74 MOV M,H
        move.b  regh(regs),0(targbase,d0.l)
        jmp     (return)

moveml  move.w  regh(regs),d0           ;75 MOV M,L
        move.b  regl(regs),0(targbase,d0.l)
        jmp     (return)

halt    jsr     service                 ;76 HLT
        jmp     (return)

movema  move.w  regh(regs),d0           ;77 MOV M,A
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

moveab  move.b  regb(regs),rega         ;78 MOV A,B
        jmp     (return)

moveac  move.b  regc(regs),rega         ;79 MOV A,C
        jmp     (return)

movead  move.b  regd(regs),rega         ;7A MOV A,D
        jmp     (return)

moveae  move.b  rege(regs),rega         ;7B MOV A,E
        jmp     (return)

moveah  move.b  regh(regs),rega         ;7C MOV A,H
        jmp     (return)

moveal  move.b  regl(regs),rega         ;7D MOV A,L
        jmp     (return)

moveam  move.w  regh(regs),d0           ;7E MOV A,M
        move.b  0(targbase,d0.l),rega
        jmp     (return)

moveaa  jmp     (return)                ;7F MOV A,A

addb    move.b  regb(regs),d0           ;80 ADD B
        addflag

addc    move.b  regc(regs),d0           ;81 ADD C
        addflag

addd    move.b  regd(regs),d0           ;82 ADD D
        addflag

adde    move.b  rege(regs),d0           ;83 ADD E
        addflag

addh    move.b  regh(regs),d0           ;84 ADD H
        addflag

addl    move.b  regl(regs),d0           ;85 ADD L
        addflag

addm    move.w  regh(regs),d0           ;86 ADD M
        move.b  0(targbase,d0.l),d0
        addflag

adda    move.b  rega,regop1(regs)       ;87 ADD A
        move.b  rega,regop2(regs)
        move.b  regcon0e,regop3(regs)
        add.b   rega,rega
        setflag

adcb    move.b  regf,regop3(regs)       ;88 ADC B
        asr.b   #1,regf
        move.b  regb(regs),d0
        adcflag

adcc    move.b  regf,regop3(regs)       ;89 ADC C
        asr.b   #1,regf
        move.b  regc(regs),d0
        adcflag

adcd    move.b  regf,regop3(regs)       ;8A ADC D
        asr.b   #1,regf
        move.b  regd(regs),d0
        adcflag

adce    move.b  regf,regop3(regs)       ;8B ADC E
        asr.b   #1,regf
        move.b  rege(regs),d0
        adcflag

adch    move.b  regf,regop3(regs)       ;8C ADC H
        asr.b   #1,regf
        move.b  regh(regs),d0
        adcflag

adcl    move.b  regf,regop3(regs)       ;8D ADC L
        asr.b   #1,regf
        move.b  regl(regs),d0
        adcflag

adcm    move.b  regf,regop3(regs)       ;8E ADC M
        move.w  regh(regs),d0
        move.l  d0,a0
        adda.l  targbase,a0
        asr.b   #1,regf
        move.b  (a0),d0
        adcflag

adca    move.b  regf,regop3(regs)       ;8F ADC A
        asr.b   #1,regf
        move.b  rega,d0
        adcflag

subb    sub.b   regb(regs),rega         ;90 SUB B
        setflag

subc    sub.b   regc(regs),rega         ;91 SUB C
        setflag

subd    sub.b   regd(regs),rega         ;92 SUB D
        setflag

sube    sub.b   rege(regs),rega         ;93 SUB E
        setflag

subh    sub.b   regh(regs),rega         ;94 SUB H
        setflag

subl    sub.b   regl(regs),rega         ;95 SUB L
        setflag

subm    move.w  regh(regs),d0           ;96 SUB M
        sub.b   0(targbase,d0.l),rega
        setflag

suba    sub.b   rega,rega               ;97 SUB A
        setflag

sbbb    asr.b   #1,regf                 ;98 SBB B
        move.b  regb(regs),d0
        sbbflag

sbbc    asr.b   #1,regf                 ;99 SBB C
        move.b  regc(regs),d0
        sbbflag

sbbd    asr.b   #1,regf                 ;9A SBB D
        move.b  regd(regs),d0
        sbbflag

sbbe    asr.b   #1,regf                 ;9B SBB E
        move.b  rege(regs),d0
        sbbflag

sbbh    asr.b   #1,regf                 ;9C SBB H
        move.b  regh(regs),d0
        sbbflag

sbbl    asr.b   #1,regf                 ;9D SBB L
        move.b  regl(regs),d0
        sbbflag

sbbm    move.w  regh(regs),d0           ;9E SBB M
        move.l  d0,a0
        adda.l  targbase,a0
        asr.b   #1,regf
        move.b  (a0),d0
        sbbflag

sbba    asr.b   #1,regf                 ;9F SBB A
        move.b  rega,d0
        sbbflag

andb    and.b   regb(regs),rega         ;A0 ANA B
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

andc    and.b   regc(regs),rega         ;A1 ANA C
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

andd    and.b   regd(regs),rega         ;A2 ANA D
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

ande    and.b   rege(regs),rega         ;A3 ANA E
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

andh    and.b   regh(regs),rega         ;A4 ANA H
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

andl    and.b   regl(regs),rega         ;A5 ANA L
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

andm    move.w  regh(regs),d0           ;A6 ANA M
        and.b   0(targbase,d0.l),rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

anda    move.b  16(flagptr,rega.w),regf ;A7 ANA A
        jmp     (return)

xrab    move.b  regb(regs),d0           ;A8 XRA B
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrac    move.b  regc(regs),d0           ;A9 XRA C
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrad    move.b  regd(regs),d0           ;AA XRA D
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrae    move.b  rege(regs),d0           ;AB XRA E
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrah    move.b  regh(regs),d0           ;AC XRA H
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xral    move.b  regl(regs),d0           ;AD XRA L
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xram    move.w  regh(regs),d0           ;AE XRA M
        move.b  0(targbase,d0.l),d0
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xraa    moveq   #0,rega                 ;AF XRA A (clears accumulator)
        move.b  16(flagptr),regf
        jmp     (return)

orab    or.b    regb(regs),rega         ;B0 ORA B
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orac    or.b    regc(regs),rega         ;B1 ORA C
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orad    or.b    regd(regs),rega         ;B2 ORA D
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orae    or.b    rege(regs),rega         ;B3 ORA E
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orah    or.b    regh(regs),rega         ;B4 ORA H
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

oral    or.b    regl(regs),rega         ;B5 ORA L
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

oram    move.w  regh(regs),d0           ;B6 ORA M
        or.b    0(targbase,d0.l),rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

oraa    move.b  16(flagptr,rega.w),regf ;B7 ORA A
        jmp     (return)

cmpb    cmp.b   regb(regs),rega         ;B8 CMP B
        setflag

cmpc    cmp.b   regc(regs),rega         ;B9 CMP C
        setflag

cmpd    cmp.b   regd(regs),rega         ;BA CMP D
        setflag

cmpe    cmp.b   rege(regs),rega         ;BB CMP E
        setflag

cmph    cmp.b   regh(regs),rega         ;BC CMP H
        setflag

cmpl    cmp.b   regl(regs),rega         ;BD CMP L
        setflag

cmpam   move.w  regh(regs),d0           ;BE CMP M
        cmp.b   0(targbase,d0.l),rega
        setflag

cmpaa   cmp.b   rega,rega               ;BF CMP A
        setflag

rnz     btst    #6,regf                 ;C0 RNZ
        bne     mloop
        ret80

popb    move.b  (pseudosp)+,regc(regs)  ;C1 POP B
        move.b  (pseudosp)+,regb(regs)
        jmp     (return)

jnz:                                    ;C2 JNZ addr
        jmpaddr
        btst    #6,regf
        bne     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

jmpa:                                   ;C3 JMP addr
        jmpaddr
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

cnz:                                    ;C4 CNZ addr
        jmpaddr
        btst    #6,regf
        bne     mloop
        call80

pushb   move.b  regb(regs),-(pseudosp)  ;C5 PUSH B
        move.b  regc(regs),-(pseudosp)
        jmp     (return)

adi     move.b  (pseudopc)+,d0          ;C6 ADI nn
        addflag

rst0:                                   ;C7 RST 0
        rst80   $0

rz      btst    #6,regf                 ;C8 RZ
        beq     mloop
ret:                                    ;C9 RET
        ret80

jz:                                     ;CA JZ addr
        jmpaddr
        btst    #6,regf
        beq     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

cz:                                     ;CC CZ addr
        jmpaddr
        btst    #6,regf
        beq     mloop
        call80

call:                                   ;CD CALL addr
        jmpaddr
        call80

aci:    move.b  regf,regop3(regs)       ;CE ACI nn
        asr.b   #1,regf
        move.b  (pseudopc)+,d0
        adcflag

rst1:                                   ;CF RST 1
        rst80   $8

rnc     btst    #0,regf                 ;D0 RNC
        bne     mloop
        ret80

popd    move.b  (pseudosp)+,rege(regs)  ;D1 POP D
        move.b  (pseudosp)+,regd(regs)
        jmp     (return)

jnc:                                    ;D2 JNC addr
        jmpaddr
        btst    #0,regf
        bne     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

out     moveq   #0,d0                   ;D3 OUT nn
*       move.b  (pseudopc)+,d0
*       move.l  #$ff0000,a0
*       move.b  rega,0(a0,d0.l)
        jmp     (return)

cnc                                     ;D4 CNC addr
        jmpaddr
        btst    #0,regf
        bne     mloop
        call80

pushd   move.b  regd(regs),-(pseudosp)  ;D5 PUSH D
        move.b  rege(regs),-(pseudosp)
        jmp     (return)

sui     move.b  (pseudopc)+,d0          ;D6 SUI nn
        sub.b   d0,rega
        setflag

rst2:                                   ;D7 RST 2
        rst80   $10

rc      btst    #0,regf                 ;D8 RC
        beq     mloop
        ret80

jc:                                     ;DA JC addr
        jmpaddr
        btst    #0,regf
        beq     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

in      moveq   #0,d0                   ;DB IN nn
*       move.b  (pseudopc)+,d0
*       move.l  #$FF0000,a0
*       move.b  0(a0,d0.l),rega
        jmp     (return)

cc:                                     ;DC CC addr
        jmpaddr
        btst    #0,regf
        beq     mloop
        call80

sbi     asr.b   #1,regf                 ;DE SBI nn
        move.b  (pseudopc)+,d0
        sbbflag

rst3:                                   ;DF RST 3
        rst80   $18

rpo     btst    #2,regf                 ;E0 RPO
        bne     mloop
        ret80

poph    move.b  (pseudosp)+,regl(regs)  ;E1 POP H
        move.b  (pseudosp)+,regh(regs)
        jmp     (return)

jpo:                                    ;E2 JPO addr
        jmpaddr
        btst    #2,regf
        bne     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

xthl    move.b  regl(regs),d0           ;E3 XTHL
        move.b  (pseudosp),regl(regs)
        move.b  d0,(pseudosp)
        move.b  regh(regs),d0
        move.b  1(pseudosp),regh(regs)
        move.b  d0,1(pseudosp)
        jmp     (return)

cpo     jmpaddr                         ;E4 CPO addr
        btst    #2,regf
        bne     mloop
        call80

pushh   move.b  regh(regs),-(pseudosp)  ;E5 PUSH H
        move.b  regl(regs),-(pseudosp)
        jmp     (return)

ani     and.b   (pseudopc)+,rega        ;E6 ANI nn
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

rst4:                                   ;E7 RST 4
        rst80   $20

rpe     btst    #2,regf                 ;E8 RPE
        beq     mloop
        ret80

pchl    move.w  regh(regs),d0           ;E9 PCHL
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

jpe:                                    ;EA JPE addr
        jmpaddr
        btst    #2,regf
        beq     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

xchg    move.w  regd(regs),d0           ;EB XCHG
        move.w  regh(regs),regd(regs)
        move.w  d0,regh(regs)
        jmp     (return)

cpe:                                    ;EC CPE addr
        jmpaddr
        btst    #2,regf
        beq     mloop
        call80

* ED-prefix Z80 instructions are simulated in the next module.

xri     move.b  (pseudopc)+,d0          ;EE XRI nn
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

rst5:                                   ;EF RST 5
        rst80   $28

rp      btst    #7,regf                 ;F0 RP
        bne     mloop
        ret80

popp    move.b  (pseudosp)+,regf        ;F1 POP P
        move.b  (pseudosp)+,rega
        jmp     (return)

jp:                                     ;F2 JP addr
        jmpaddr
        btst    #7,regf
        bne     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

di      jmp     (return)                ;F3 DI (treated as no-op)

cp:                                     ;F4 CP addr
        jmpaddr
        btst    #7,regf
        bne     mloop
        call80

pushp   move.b  rega,-(pseudosp)        ;F5 PUSH PSW
        move.b  regf,-(pseudosp)
        jmp     (return)

oria    or.b    (pseudopc)+,rega        ;F6 ORI nn
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

rst6:                                   ;F7 RST 6
        rst80   $30

rm      btst    #7,regf                 ;F8 RM
        beq     mloop
        ret80

sphl    move.w  regh(regs),d0           ;F9 SPHL
        lea     0(targbase,d0.l),pseudosp
        jmp     (return)

jm:                                     ;FA JM addr
        jmpaddr
        btst    #7,regf
        beq     mloop
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

ei      jmp     (return)                ;FB EI (treated as a no-op)

cm:                                     ;FC CM addr
        jmpaddr
        btst    #7,regf
        beq     mloop
        call80

cpi     cmp.b   (pseudopc)+,rega        ;FE CPI nn
        setflag

rst7:                                   ;FF RST 7
        rst80   $38
        page
*************************************************************************
*                                                                       *
*       Z-80 opcode simulation routines                                 *
*                                                                       *
*************************************************************************

exaf    move.b   regaaf(regs),d0           ;08
        move.b   regaaf+1(regs),d1
        move.b   rega,regaaf(regs)
        move.b   regf,regaaf+1(regs)
        move.b   d0,rega
        move.b   d1,regf
        jmp      (return)

djnz    jraddr                             ;10 DJNZ extension
        subq.b   #1,regb(regs)
        beq      mloop
        gopc

jr      jraddr                             ;18 JR extension
        gopc

jrnz    jraddr                             ;20 JR NZ,extension
        btst     #6,regf
        bne      mloop
        gopc

jrz     jraddr                             ;28 JR Z,extension
        btst     #6,regf
        beq      mloop
        gopc

jrnc    jraddr                             ;30 JR NC,extension
        btst     #0,regf
        bne      mloop
        gopc

jrc     jraddr                             ;38 JR C,extension
        btst     #0,regf
        beq      mloop
        gopc

preCB   clr.l   d0
        move.b  (pseudopc)+,d0
        move.l  d0,d1
        andi.b  #%11000000,d1
        bne     1$
        lea     CBtable1,a0
        asl     #2,d0
        move.l  0(a0,d0.w),-(sp)
        beq     illg2b
        rts
1$      lsr     #4,d1
        lea     CBtable2,a0
        move.l  0(a0,d1.w),d1
        movea.l d1,a0
        move.l  d0,d1
        andi.b  #%00000111,d1
        asl     #2,d1
        move.l  0(a0,d1.w),-(sp)
        beq     illg2b
        andi.b  #%00111000,d0
        lsr     #3,d0
        rts

ddcb    calcind x
        bra     xDCB
fdcb    calcind y
xDCB    clr.l   d1
        move.b  (pseudopc),d1
        andi.b  #%11000000,d1
        lsr     #4,d1
        lea     xDCBtbl,a0
        move.l  0(a0,d1.w),-(sp)
        move.b  (pseudopc)+,d1
        andi.b  #%00111000,d1
        lsr     #3,d1
        rts

cb10    roxr.b  #1,regf                    ;CB10 RL B
        move.b  regb(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,regb(regs)
        jmp     (return)

cb11    roxr.b  #1,regf                    ;CB11 RL C
        move.b  regc(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,regc(regs)
        jmp     (return)

cb12    roxr.b  #1,regf                    ;CB12 RL D
        move.b  regd(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,regd(regs)
        jmp     (return)

cb13    roxr.b  #1,regf                    ;CB13 RL E
        move.b  rege(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,rege(regs)
        jmp     (return)

cb14    roxr.b  #1,regf                    ;CB14 RL H
        move.b  regh(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,regh(regs)
        jmp     (return)

cb15    roxr.b  #1,regf                    ;CB15 RL L
        move.b  regl(regs),d0
        roxl.b  #1,d0
        rotflag
        move.b  d0,regl(regs)
        jmp     (return)

cb18    roxr.b  #1,regf                    ;CB18 RR B
        move.b  regb(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,regb(regs)
        jmp     (return)

cb19    roxr.b  #1,regf                    ;CB19 RR C
        move.b  regc(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,regc(regs)
        jmp     (return)

cb1a    roxr.b  #1,regf                    ;CB1A RR D
        move.b  regd(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,regd(regs)
        jmp     (return)

cb1b    roxr.b  #1,regf                    ;CB1B RR E
        move.b  rege(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,rege(regs)
        jmp     (return)

cb1c    roxr.b  #1,regf                    ;CB1C RR H
        move.b  regh(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,regh(regs)
        jmp     (return)

cb1d    roxr.b  #1,regf                    ;CB1D RR L
        move.b  regl(regs),d0
        roxr.b  #1,d0
        rotflag
        move.b  d0,regl(regs)
        jmp     (return)

cb25    move.b  regl(regs),d0              ;CB25 SLA L
        asl.b   #1,d0
        rotflag
        move.b  d0,regl(regs)
        jmp     (return)

cb2f    asr.b   #1,rega                    ;CB2F SRA A
        setflag

cb39    move.b  regc(regs),d0              ;CB39 SRL C
        lsr.b   #1,d0
        rotflag
        move.b  d0,regc(regs)
        jmp     (return)

cb3c    move.b  regh(regs),d0              ;CB3C SRL H
        lsr.b   #1,d0
        rotflag
        move.b  d0,regh(regs)
        jmp     (return)

cb3f    lsr.b   #1,rega                    ;CB3F SRL A
        setflag

bitb    btst    d0,regb(regs)              ;CB40, 48, 50, 58, 60, 68, 70, 78
        beq     setit
        bra     clrit

bitc    btst    d0,regc(regs)              ;CB41, 49, 51, 59, 61, 69, 71, 79
        beq     setit
        bra     clrit

bitd    btst    d0,regd(regs)              ;CB42, 4A, 52, 5A, 62, 6A, 72, 7A
        beq     setit
        bra     clrit

bite    btst    d0,rege(regs)              ;CB43, 4B, 53, 5B, 63, 6B, 73, 7B
        beq     setit
        bra     clrit

bith    btst    d0,regh(regs)              ;CB44
        beq     setit
        bra     clrit

bitl    btst    d0,regl(regs)              ;CB45
        beq     setit
        bra     clrit

bitm    clr.l   d1
        move.w  regh(regs),d1
        btst    d0,0(targbase,d1.l)
        beq     setit
        bra     clrit

bita    btst    d0,rega                    ;CB47, 4F, 57, 5F, 67, 6F, 77, 7F
        beq     setit
        bra     clrit

bitx    btst    d1,0(targbase,d0.l)
        beq     setit
*       bra     clrit
clrit   andi.b  #%10111111,regf
        jmp     (return)
setit   ori.b   #%01000000,regf
        jmp     (return)

resb    bclr    d0,regb(regs)
        jmp     (return)

resc    bclr    d0,regc(regs)
        jmp     (return)

resd    bclr    d0,regd(regs)
        jmp     (return)

rese    bclr    d0,rege(regs)
        jmp     (return)

resh    bclr    d0,regh(regs)
        jmp     (return)

resl    bclr    d0,regl(regs)
        jmp     (return)

resm    clr.l   d1
        move.w  regh(regs),d1
        bclr    d0,0(targbase,d1.l)
        jmp     (return)

resa    bclr    d0,rega
        jmp     (return)

resx    bclr    d1,0(targbase,d0.l)
        jmp     (return)

setb    bset    d0,regb(regs)
        jmp     (return)

setc    bset    d0,regc(regs)
        jmp     (return)

setd    bset    d0,regd(regs)
        jmp     (return)

sete    bset    d0,rege(regs)
        jmp     (return)

seth    bset    d0,regh(regs)
        jmp     (return)

setl    bset    d0,regl(regs)
        jmp     (return)

setm    clr.l   d1
        move.w  regh(regs),d1
        bset    d0,0(targbase,d1.l)
        jmp     (return)

seta    bset    d0,rega
        jmp     (return)

setx    bset    d1,0(targbase,d0.l)
        jmp     (return)

exx     move.w  regabc(regs),d0            ;D9 EXX
        move.w  regb(regs),regabc(regs)
        move.w  d0,regb(regs)
        move.w  regade(regs),d0
        move.w  regd(regs),regade(regs)
        move.w  d0,regd(regs)
        move.w  regahl(regs),d0
        move.w  regh(regs),regahl(regs)
        move.w  d0,regh(regs)
        jmp     (return)

preDD   lea     DDoptab,a0
        bra     twobyte
preED   lea     EDoptab,a0
        bra     twobyte
preFD   lea     FDoptab,a0
twobyte clr.l   d1                         ;Zero-fill high bits.
        move.b  (pseudopc)+,d1             ;Grab next opcode.
        asl     #2,d1
        move.l  0(a0,d1.w),-(sp)           ;Do the operation.
        beq     illg2b
        rts

illg2b  move.l  (sp)+,d1                   ;Trash the address.
ng2b    subq.l  #1,pseudopc                ;Fix PPC for ILLEGAL.
        jmp     illegl

dd09    move.w  regb(regs),d0              ;DD09 ADD IX,BC
        add.w   d0,regix(regs)
        docyf

dd19    move.w  regd(regs),d0              ;DD19 ADD IX,DE
        add.w   d0,regix(regs)
        docyf

dd21    move.b  (pseudopc)+,regix+1(regs)  ;DD21 LXI IX,nnnn
        move.b  (pseudopc)+,regix(regs)
        jmp     (return)

sixd    lsxxd                              ;DD22 SIXD addr
        move.b  regix+1(regs),(a0)+
        move.b  regix(regs),(a0)
        jmp     (return)

dd23    addq.w  #1,regix(regs)             ;DD23 INC IX
        jmp     (return)

dd29    asl.w   regix(regs)                ;DD29 ADD IX,IX
        docyf

lixd    lsxxd                              ;DD2A LIXD addr
        move.b  (a0)+,regix+1(regs)
        move.b  (a0),regix(regs)
        jmp     (return)

dd2b    subq.w  #1,regix(regs)             ;DD2B
        jmp     (return)

dd34    calcind x                          ;DD34
        addq.b  #1,0(targbase,d0.l)
        inrflag

dd35    calcind x                          ;DD35
        subq.b  #1,0(targbase,d0.l)
        inrflag

dd36    calcind x                          ;DD36
        move.b  (pseudopc)+,0(targbase,d0.l)
        jmp     (return)

dd39    move.l  pseudosp,d0                ;DD39 ADD IX,SP
        sub.l   targbase,d0
        add.w   d0,regix(regs)
        docyf

dd46    calcind x                          ;DD46
        move.b  0(targbase,d0.l),regb(regs)
        jmp     (return)

dd4e    calcind x                          ;DD4E
        move.b  0(targbase,d0.l),regc(regs)
        jmp     (return)

dd56    calcind x                          ;DD56
        move.b  0(targbase,d0.l),regd(regs)
        jmp     (return)

dd5e    calcind x                          ;DD5E
        move.b  0(targbase,d0.l),rege(regs)
        jmp     (return)

dd66    calcind x                          ;DD66
        move.b  0(targbase,d0.l),regh(regs)
        jmp     (return)

dd6e    calcind x                          ;DD6E
        move.b  0(targbase,d0.l),regl(regs)
        jmp     (return)

dd70    calcind x                          ;DD70
        move.b  regb(regs),0(targbase,d0.l)
        jmp     (return)

dd71    calcind x                          ;DD71
        move.b  regc(regs),0(targbase,d0.l)
        jmp     (return)

dd72    calcind x                          ;DD72
        move.b  regd(regs),0(targbase,d0.l)
        jmp     (return)

dd73    calcind x                          ;DD73
        move.b  rege(regs),0(targbase,d0.l)
        jmp     (return)

dd74    calcind x                          ;DD74
        move.b  regh(regs),0(targbase,d0.l)
        jmp     (return)

dd75    calcind x                          ;DD75
        move.b  regl(regs),0(targbase,d0.l)
        jmp     (return)

dd77    calcind x                          ;DD77
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

dd7e    calcind x                          ;DD7E
        move.b  0(targbase,d0.l),rega
        jmp     (return)

dd86    calcind x                          ;DD86
        move.b  0(targbase,d0.l),d0
        addflag

dd96    calcind x                          ;DD96
        sub.b   0(targbase,d0.l),rega
        jmp     (return)

ddbe    calcind x                          ;DDBE
        cmp.b   0(targbase,d0.l),rega
        setflag

popix   move.b  (pseudosp)+,regix+1(regs)  ;DDE1
        move.b  (pseudosp)+,regix(regs)
        jmp     (return)

dde3    move.b  regix+1(regs),d0           ;DDE3
        move.b  (pseudosp),regix+1(regs)
        move.b  d0,(pseudosp)
        move.b  regix(regs),d0
        move.b  1(pseudosp),regix(regs)
        move.b  d0,1(pseudosp)
        jmp     (return)

pushix  move.b  regix(regs),-(pseudosp)    ;DDE5
        move.b  regix+1(regs),-(pseudosp)
        jmp     (return)

dde9    move.w  regix(regs),d0             ;DDE9
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

ddf9    move.w  regix(regs),d0             ;DDF9
        lea     0(targbase,d0.l),pseudosp
        jmp     (return)

shlbc   ashl    sub,b                      ;ED42

sbcd    lsxxd                              ;ED43 SBCD addr
        move.b  regc(regs),(a0)+
        move.b  regb(regs),(a0)+
        jmp     (return)

ed44    neg.b   rega                       ;ED44 NEG
        setflag

ahlbc   ashl    add,b                      ;ED4A

lbcd    lsxxd                              ;ED4B LD BC,M
        move.b  (a0)+,regc(regs)
        move.b  (a0),regb(regs)
        jmp     (return)

shlde   ashl    sub,d                      ;ED52

sded    lsxxd                              ;ED53 SDED addr
        move.b  rege(regs),(a0)+
        move.b  regd(regs),(a0)+
        jmp     (return)

ahlde   ashl    add,d                      ;ED5A

lded    lsxxd                              ;ED5B LDED addr
        move.b  (a0)+,rege(regs)
        move.b  (a0),regd(regs)
        jmp     (return)

shlhl   ashl    sub,h                      ;ED62

ahlhl   ashl    add,h                      ;ED6A

ed6f    move.w  regh(regs),d0              ;ED6F RLD
        clr.w   d1
        move.b  0(targbase,d0.w),d1
        lsl.w   #4,d1
        move.b  rega,d0
        andi.b  #%00001111,d0
        or.b    d0,d1
        move.w  regh(regs),d0
        move.b  d1,0(targbase,d0.w)
        lsr.w   #8,d1
        andi.b  #%11110000,rega
        or.b    d1,rega
        jmp     (return)

sspd    lsxxd                              ;ED73 SSPD addr
        move.l  pseudosp,d0
        sub.l   targbase,d0
        move.b  d0,(a0)+
        lsr.w   #8,d0
        move.b  d0,(a0)
        jmp     (return)

lspd    lsxxd                              ;ED7B LSPD addr
        clr.l   d0
        move.b  1(a0),d0
        lsl.w   #8,d0
        move.b  (a0),d0
        movea.l d0,pseudosp
        adda.l  targbase,pseudosp
        jmp     (return)

ldi     clr.l   d1                         ;EDA0
        move.w  regh(regs),d1
        move.b  0(targbase,d1.l),d0
        move.w  regd(regs),d1
        move.b  d0,0(targbase,d1.l)
        addq.w  #1,regd(regs)
        addq.w  #1,regh(regs)
        subq.w  #1,regb(regs)
        beq     1$
        ori.b   #%00000100,regf
        jmp     (return)
1$      andi.b  #%11111011,regf
        jmp     (return)

ldir    movem.l d2/a5,-(sp)                ;EDB0
        move.w  regb(regs),d0              ;Grab count,
        subq.w  #1,d0                      ; adjust for DBRA.
        clr.l   d1
        move.w  regh(regs),d1              ;Source
        lea     0(targbase,d1.l),a0
        clr.l   d2
        move.w  regd(regs),d2              ;Destination
        lea     0(targbase,d2.l),a5
ldirlop move.b  (a0)+,(a5)+
        addq.w  #1,d1
        addq.w  #1,d2
        dbra    d0,ldirlop
        move.w  d1,regh(regs)              ;Update 8080 registers.
        move.w  d2,regd(regs)
        move.w  #0,regb(regs)
        moveq   #0,regf
        movem.l (sp)+,d2/a5
        jmp     (return)

lddr    movem.l d2/a5,-(sp)                ;EDC0
        move.w  regb(regs),d0              ;Grab count,
        subq.w  #1,d0                      ; adjust for DBRA.
        clr.l   d1
        move.w  regh(regs),d1              ;Source
        lea     1(targbase,d1.l),a0
        clr.l   d2
        move.w  regd(regs),d2              ;Destination
        lea     1(targbase,d2.l),a5
lddrlop move.b  -(a0),-(a5)
        subq.w  #1,d1
        subq.w  #1,d2
        dbra    d0,lddrlop
        move.w  d1,regh(regs)              ;Update 8080 registers.
        move.w  d2,regd(regs)
        move.w  #0,regb(regs)
        moveq   #0,regf
        movem.l (sp)+,d2/a5
        jmp     (return)
        page
cpir    move.w  regb(regs),d0              ;EDB1
        subq.w  #1,d0
        moveq   #0,d1
        move.w  regh(regs),d1              ;Source
        lea     0(targbase,d1.l),a0
cpirlop addq.w  #1,d1
        cmp.b   (a0)+,rega
        dbeq    d0,cpirlop
        bne     nomatch
        move.w  d1,regh(regs)              ;Restore 8080 registers.
        move.w  d0,regb(regs)
        tst.w   d0
        beq     1$
        moveq   #%01000100,regf            ;Found, in the string.
        jmp     (return)
1$      moveq   #%01000000,regf            ;Found, but at last place.
        jmp     (return)
nomatch move.w  d1,regh(regs)              ;Restore 8080 registers.
        addq.w  #1,d0
        move.w  d0,regb(regs)
        tst.w   d0
        beq     2$
        moveq   #%00000100,regf
        jmp     (return)
2$      moveq   #%00000000,regf
        jmp     (return)

fd09    move.w  regb(regs),d0              ;FD09 ADD IY,BC
        add.w   d0,regiy(regs)
        docyf

fd19    move.w  regd(regs),d0              ;FD19 ADD IY,DE
        add.w   d0,regiy(regs)
        docyf

fd21    move.b  (pseudopc)+,regiy+1(regs)  ;FD21 LXI IY,nnnn
        move.b  (pseudopc)+,regiy(regs)
        jmp     (return)

siyd    lsxxd                              ;FD22 SIYD addr
        move.b  regiy+1(regs),(a0)+
        move.b  regiy(regs),(a0)
        jmp     (return)

fd23    addq.w  #1,regiy(regs)             ;FD23
        jmp     (return)

fd29    asl.w   regiy(regs)                ;FD29 ADD IY,IY
        docyf

liyd    lsxxd                              ;FD2A LIYD addr
        move.b  (a0)+,regiy+1(regs)
        move.b  (a0),regiy(regs)
        jmp     (return)

fd2b    subq.w  #1,regiy(regs)             ;FD2B
        jmp     (return)

fd34    calcind y                          ;FD34
        addq.b  #1,0(targbase,d0.l)
        inrflag

fd35    calcind y                          ;FD35
        subq.b  #1,0(targbase,d0.l)
        inrflag

fd36    calcind y                          ;FD36
        move.b  (pseudopc)+,0(targbase,d0.l)
        jmp     (return)

fd39    move.l  pseudosp,d0                ;FD39 ADD IY,SP
        sub.l   targbase,d0
        add.w   d0,regiy(regs)
        docyf

fd46    calcind y                          ;FD46
        move.b  0(targbase,d0.l),regb(regs)
        jmp     (return)

fd4e    calcind y                          ;FD4E
        move.b  0(targbase,d0.l),regc(regs)
        jmp     (return)

fd56    calcind y                          ;FD56
        move.b  0(targbase,d0.l),regd(regs)
        jmp     (return)

fd5e    calcind y                          ;FD5E
        move.b  0(targbase,d0.l),rege(regs)
        jmp     (return)

fd66    calcind y                          ;FD66
        move.b  0(targbase,d0.l),regh(regs)
        jmp     (return)

fd6e    calcind y                          ;FD6E
        move.b  0(targbase,d0.l),regl(regs)
        jmp     (return)

fd70    calcind y                          ;FD70
        move.b  regb(regs),0(targbase,d0.l)
        jmp     (return)

fd71    calcind y                          ;FD71
        move.b  regc(regs),0(targbase,d0.l)
        jmp     (return)

fd72    calcind y                          ;FD72
        move.b  regd(regs),0(targbase,d0.l)
        jmp     (return)

fd73    calcind y                          ;FD73
        move.b  rege(regs),0(targbase,d0.l)
        jmp     (return)

fd74    calcind y                          ;FD74
        move.b  regh(regs),0(targbase,d0.l)
        jmp     (return)

fd75    calcind y                          ;FD75
        move.b  regl(regs),0(targbase,d0.l)
        jmp     (return)

fd77    calcind y                          ;FD77
        move.b  rega,0(targbase,d0.l)
        jmp     (return)

fd7e    calcind y                          ;FD7E
        move.b  0(targbase,d0.l),rega
        jmp     (return)

fd86    calcind y                          ;FD86
        move.b  0(targbase,d0.l),d0
        addflag

fd96    calcind y                          ;FD96
        sub.b   0(targbase,d0.l),rega
        jmp     (return)

fdbe    calcind y                          ;FDBE
        cmp.b   0(targbase,d0.l),rega
        setflag

popiy   move.b  (pseudosp)+,regiy+1(regs)  ;FDE1
        move.b  (pseudosp)+,regiy(regs)
        jmp     (return)

fde3    move.b  regiy+1(regs),d0           ;FDE3
        move.b  (pseudosp),regiy+1(regs)
        move.b  d0,(pseudosp)
        move.b  regiy(regs),d0
        move.b  1(pseudosp),regiy(regs)
        move.b  d0,1(pseudosp)
        jmp     (return)

pushiy  move.b  regiy(regs),-(pseudosp)    ;FDE5
        move.b  regiy+1(regs),-(pseudosp)
        jmp     (return)

fde9    move.w  regiy(regs),d0             ;FDE9
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

fdf9    move.w  regiy(regs),d0             ;FDF9
        lea     0(targbase,d0.l),pseudosp
        jmp     (return)
        page
*************************************************************************
*                                                                       *
*       Opcode dispatch table                                           *
*                                                                       *
*************************************************************************
        data    data

        even
optabl  dc.l    nop00,lxib,staxb,inxb,inrb,dcrb,mvib,rlca
        dc.l    exaf,dadb,ldaxb,dcxb,inrc,dcrc,mvic,rrca
        dc.l    djnz,lxid,staxd,inxd,inrd,dcrd,mvid,ral
        dc.l    jr,dadd,ldaxd,dcxd,inre,dcre,mvie,rar
        dc.l    jrnz,lxih,shld,inxh,inrh,dcrh,mvih,daa
        dc.l    jrz,dadh,lhld,dcxh,inrl,dcrl,mvil,cma
        dc.l    jrnc,lxis,sta,inxs,inrm,dcrm,mvim,stc
        dc.l    jrc,dads,lda,dcxs,inra,dcra,mvia,cmc
        dc.l    movebb,movebc,movebd,movebe,movebh,movebl,movebm,moveba
        dc.l    movecb,movecc,movecd,movece,movech,movecl,movecm,moveca
        dc.l    movedb,movedc,movedd,movede,movedh,movedl,movedm,moveda
        dc.l    moveeb,moveec,moveed,moveee,moveeh,moveel,moveem,moveea
        dc.l    movehb,movehc,movehd,movehe,movehh,movehl,movehm,moveha
        dc.l    movelb,movelc,moveld,movele,movelh,movell,movelm,movela
        dc.l    movemb,movemc,movemd,moveme,movemh,moveml,halt,movema
        dc.l    moveab,moveac,movead,moveae,moveah,moveal,moveam,moveaa
        dc.l    addb,addc,addd,adde,addh,addl,addm,adda
        dc.l    adcb,adcc,adcd,adce,adch,adcl,adcm,adca
        dc.l    subb,subc,subd,sube,subh,subl,subm,suba
        dc.l    sbbb,sbbc,sbbd,sbbe,sbbh,sbbl,sbbm,sbba
        dc.l    andb,andc,andd,ande,andh,andl,andm,anda
        dc.l    xrab,xrac,xrad,xrae,xrah,xral,xram,xraa
        dc.l    orab,orac,orad,orae,orah,oral,oram,oraa
        dc.l    cmpb,cmpc,cmpd,cmpe,cmph,cmpl,cmpam,cmpaa
        dc.l    rnz,popb,jnz,jmpa,cnz,pushb,adi,rst0
        dc.l    rz,ret,jz,preCB,cz,call,aci,rst1
        dc.l    rnc,popd,jnc,out,cnc,pushd,sui,rst2
        dc.l    rc,exx,jc,in,cc,preDD,sbi,rst3
        dc.l    rpo,poph,jpo,xthl,cpo,pushh,ani,rst4
        dc.l    rpe,pchl,jpe,xchg,cpe,preED,xri,rst5
        dc.l    rp,popp,jp,di,cp,pushp,oria,rst6
        dc.l    rm,sphl,jm,ei,cm,preFD,cpi,rst7
        page
*************************************************************************
*                                                                       *
*       Flag register lookup tables                                     *
*                                                                       *
*************************************************************************
flags   dc.b    $00,$01,$04,$05,$40,$41,$44,$45
        dc.b    $80,$81,$84,$85,$C0,$C1,$C4,$C5
        dc.b    $44,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $80,$84,$84,$80,$84,$80,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84
        page
*************************************************************************
*                                                                       *
*       Z80 opcode dispatch tables.  One longword entry per opcode      *
*       of the target (Z-80) processor, including illegals.             *
*                                                                       *
*************************************************************************
*                                                                       *
*       Only a few of the most popular instructions are simulated.      *
*       Support for the Z-80 Block Move instructions is provided        *
*       as the flags for this simulation resemble those of the Z-80     *
*       rather than the 8080.  Certain packages (notably BDS C) check   *
*       the flags and mistakenly assume a Z-80, then use LDIR/LDDR.     *
*       Therefore, minimal Z-80 support is provided for these           *
*       instructions.  By no means is this a complete simulation        *
*       of the Z-80.                                                    *
*                                                                       *
*************************************************************************

CBtable1:
        dc.l    0,0,0,0,0,0,0,0                    ;CB00
        dc.l    0,0,0,0,0,0,0,0                    ;CB08
        dc.l    cb10,cb11,cb12,cb13,cb14,cb15,0,0  ;CB10
        dc.l    cb18,cb19,cb1a,cb1b,cb1c,cb1d,0,0  ;CB18
        dc.l    0,0,0,0,0,cb25,0,0                 ;CB20
        dc.l    0,0,0,0,0,0,0,cb2f                 ;CB28
        dc.l    0,0,0,0,0,0,0,0                    ;CB30
        dc.l    0,cb39,0,0,cb3c,0,0,cb3f           ;CB38
CBtable2:
        dc.l    0,bittbl,restbl,settbl
bittbl  dc.l    bitb,bitc,bitd,bite,bith,bitl,bitm,bita
restbl  dc.l    resb,resc,resd,rese,resh,resl,resm,resa
settbl  dc.l    setb,setc,setd,sete,seth,setl,setm,seta
*xDCBtbl dc.l    rotsx,bitx,resx,setx
xDCBtbl dc.l    ng2b,bitx,resx,setx

DDoptab:
        dc.l    0,0,0,0,0,0,0,0                       ;DD00
        dc.l    0,dd09,0,0,0,0,0,0                    ;DD08
        dc.l    0,0,0,0,0,0,0,0                       ;DD10
        dc.l    0,dd19,0,0,0,0,0,0                    ;DD18
        dc.l    0,dd21,sixd,dd23,0,0,0,0              ;DD20
        dc.l    0,dd29,lixd,dd2b,0,0,0,0              ;DD28
        dc.l    0,0,0,0,dd34,dd35,dd36,0              ;DD30
        dc.l    0,dd39,0,0,0,0,0,0                    ;DD38
        dc.l    0,0,0,0,0,0,dd46,0                    ;DD40
        dc.l    0,0,0,0,0,0,dd4e,0                    ;DD48
        dc.l    0,0,0,0,0,0,dd56,0                    ;DD50
        dc.l    0,0,0,0,0,0,dd5e,0                    ;DD58
        dc.l    0,0,0,0,0,0,dd66,0                    ;DD60
        dc.l    0,0,0,0,0,0,dd6e,0                    ;DD68
        dc.l    dd70,dd71,dd72,dd73,dd74,dd75,0,dd77  ;DD70
        dc.l    0,0,0,0,0,0,dd7e,0                    ;DD78
        dc.l    0,0,0,0,0,0,dd86,0                    ;DD80
        dc.l    0,0,0,0,0,0,0,0                       ;DD88
        dc.l    0,0,0,0,0,0,dd96,0                    ;DD90
        dc.l    0,0,0,0,0,0,0,0                       ;DD98
        dc.l    0,0,0,0,0,0,0,0                       ;DDA0
        dc.l    0,0,0,0,0,0,0,0                       ;DDA8
        dc.l    0,0,0,0,0,0,0,0                       ;DDB0
        dc.l    0,0,0,0,0,0,ddbe,0                    ;DDB8
        dc.l    0,0,0,0,0,0,0,0                       ;DDC0
        dc.l    0,0,0,ddcb,0,0,0,0                    ;DDC8
        dc.l    0,0,0,0,0,0,0,0                       ;DDD0
        dc.l    0,0,0,0,0,0,0,0                       ;DDD8
        dc.l    0,popix,0,dde3,0,pushix,0,0           ;DDE0
        dc.l    0,dde9,0,0,0,0,0,0                    ;DDE8
        dc.l    0,0,0,0,0,0,0,0                       ;DDF0
        dc.l    0,ddf9,0,0,0,0,0,0                    ;DDF8

EDoptab:                                               
        dc.l    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ;ED00
        dc.l    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ;ED10
        dc.l    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ;ED20
        dc.l    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ;ED30
        dc.l    0,0,shlbc,sbcd,ed44,0,0,0             ;ED40
        dc.l    0,0,ahlbc,lbcd,0,0,0,0                ;ED48
        dc.l    0,0,shlde,sded,0,0,0,0                ;ED50
        dc.l    0,0,ahlde,lded,0,0,0,0                ;ED58
        dc.l    0,0,shlhl,0,0,0,0,0                   ;ED60
        dc.l    0,0,ahlhl,0,0,0,0,ed6f                ;ED68
        dc.l    0,0,0,sspd,0,0,0,0                    ;ED70
        dc.l    0,0,0,lspd,0,0,0,0                    ;ED78
        dc.l    0,0,0,0,0,0,0,0                       ;ED80
        dc.l    0,0,0,0,0,0,0,0                       ;ED88
        dc.l    0,0,0,0,0,0,0,0                       ;ED90
        dc.l    0,0,0,0,0,0,0,0                       ;ED98
        dc.l    ldi,0,0,0,0,0,0,0                     ;EDA0
        dc.l    0,0,0,0,0,0,0,0                       ;EDA8
        dc.l    ldir,cpir,0,0,0,0,0,0                 ;EDB0
        dc.l    lddr,0,0,0,0,0,0,0                    ;EDB8
        dc.l    0,0,0,0,0,0,0,0                       ;EDC0
        dc.l    0,0,0,0,0,0,0,0                       ;EDC8
        dc.l    0,0,0,0,0,0,0,0                       ;EDD0
        dc.l    0,0,0,0,0,0,0,0                       ;EDD8
        dc.l    0,0,0,0,0,0,0,0                       ;EDE0
        dc.l    0,0,0,0,0,0,0,0                       ;EDE8
        dc.l    0,0,0,0,0,0,0,0                       ;EDF0
        dc.l    0,0,0,0,0,0,0,0                       ;EDF8

FDoptab:
        dc.l    0,0,0,0,0,0,0,0                       ;FD00
        dc.l    0,fd09,0,0,0,0,0,0                    ;FD08
        dc.l    0,0,0,0,0,0,0,0                       ;FD10
        dc.l    0,fd19,0,0,0,0,0,0                    ;FD18
        dc.l    0,fd21,siyd,fd23,0,0,0,0              ;FD20
        dc.l    0,fd29,liyd,fd2b,0,0,0,0              ;FD28
        dc.l    0,0,0,0,fd34,fd35,fd36,0              ;FD30
        dc.l    0,fd39,0,0,0,0,0,0                    ;FD38
        dc.l    0,0,0,0,0,0,fd46,0                    ;FD40
        dc.l    0,0,0,0,0,0,fd4e,0                    ;FD48
        dc.l    0,0,0,0,0,0,fd56,0                    ;FD50
        dc.l    0,0,0,0,0,0,fd5e,0                    ;FD58
        dc.l    0,0,0,0,0,0,fd66,0                    ;FD60
        dc.l    0,0,0,0,0,0,fd6e,0                    ;FD68
        dc.l    fd70,fd71,fd72,fd73,fd74,fd75,0,fd77  ;FD70
        dc.l    0,0,0,0,0,0,fd7e,0                    ;FD78
        dc.l    0,0,0,0,0,0,fd86,0                    ;FD80
        dc.l    0,0,0,0,0,0,0,0                       ;FD88
        dc.l    0,0,0,0,0,0,fd96,0                    ;FD90
        dc.l    0,0,0,0,0,0,0,0                       ;FD98
        dc.l    0,0,0,0,0,0,0,0                       ;FDA0
        dc.l    0,0,0,0,0,0,0,0                       ;FDA8
        dc.l    0,0,0,0,0,0,0,0                       ;FDB0
        dc.l    0,0,0,0,0,0,fdbe,0                    ;FDB8
        dc.l    0,0,0,0,0,0,0,0                       ;FDC0
        dc.l    0,0,0,fdcb,0,0,0,0                    ;FDC8
        dc.l    0,0,0,0,0,0,0,0                       ;FDD0
        dc.l    0,0,0,0,0,0,0,0                       ;FDD8
        dc.l    0,popiy,fde3,0,0,pushiy,0,0           ;FDE0
        dc.l    0,fde9,0,0,0,0,0,0                    ;FDE8
        dc.l    0,0,0,0,0,0,0,0                       ;FDF0
        dc.l    0,fdf9,0,0,0,0,0,0                    ;FDF8


traceit dc.b    trace           ;Trace option is available - used by SIMCPM1.


        bss     bss

        even

tracesad ds.l   1               ;Start address for trace
traceead ds.l   1               ;End address for trace
traceflg ds.b   1               ;Trace activity flag

        end

