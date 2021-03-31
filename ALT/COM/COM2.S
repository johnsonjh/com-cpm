*************************************************************************
*                                                                       *
*       This file contains the target processor (8080) simulation       *
*       routines.                                                       *
*                                                                       *
*************************************************************************

*************************************************************************
*                                                                       *
*       Opcode dispatch table.  One longword entry per opcode of the    *
*       target (8080) processor, including illegals.                    *
*                                                                       *
*************************************************************************


        globl optabl,flags,nop00
        xdef mloop,illegl,service,preED,outspec

diskio   equ 0          ; Non-zero for special HDC/DMA support.

return   equ @16,r      ; JMP (return) is fast return to MLOOP.
pseudopc equ @15,r      ; 8080's PC is register A5.
opptr    equ @14,r      ; Pointer to opcode dispatch table.
pseudosp equ @13,r      ; 8080's SP is register A3.
flagptr  equ @12,r      ; Pointer to 8080's flag lookup table is A2.
targbase equ @11,r      ; Pointer to 8080's address space is A1.
regs     equ @11,r      ; Base pointer to 8080's registers is A1.

regcon0e equ 7,r        ; Register based constant #$E (for speed).
regcon01 equ 6,r        ; Register based constant #$1.
regcon0f equ 5,r        ; Register based constant #$F.
regconff equ 4,r        ; Register based constant #$FF.
*                       ;  Often used constants #0 & #8 are predominantly
*                       ;  used by instructions that have 'quick' modes
*                       ;  which encompass these values -- no register
*                       ;  needed (or available, either).
regf     equ 3,r        ; 8080's Flags
rega     equ 2,r        ; 8080's Accumulator

regop3  equ -9          ; Operand 3 for DAA storage.
regb    equ -8          ; Offsets from register base pointer for
regc    equ -7          ; 8080's pseudo-registers.
regd    equ -6          ; A & F are in Data Registers.
rege    equ -5          ; Pseudo-PC is kept in an Address Register.
regh    equ -4
regl    equ -3
regop1  equ -2          ; Operand 1 for DAA storage.
regop2  equ -1          ;    "    2  "   "     "

        data
        page
        even
optabl  dc.l nop00,lxib,staxb,inxb,inrb,dcrb,mvib,rlca
        dc.l nop08,dadb,ldaxb,dcxb,inrc,dcrc,mvic,rrca
        dc.l nop10,lxid,staxd,inxd,inrd,dcrd,mvid,ral
        dc.l nop18,dadd,ldaxd,dcxd,inre,dcre,mvie,rar
        dc.l nop20,lxih,shld,inxh,inrh,dcrh,mvih,daa
        dc.l nop28,dadh,lhld,dcxh,inrl,dcrl,mvil,cma
        dc.l nop30,lxis,sta,inxs,inrm,dcrm,mvim,stc
        dc.l nop38,dads,lda,dcxs,inra,dcra,mvia,cmc
        dc.l movebb,movebc,movebd,movebe,movebh,movebl,movebm,moveba
        dc.l movecb,movecc,movecd,movece,movech,movecl,movecm,moveca
        dc.l movedb,movedc,movedd,movede,movedh,movedl,movedm,moveda
        dc.l moveeb,moveec,moveed,moveee,moveeh,moveel,moveem,moveea
        dc.l movehb,movehc,movehd,movehe,movehh,movehl,movehm,moveha
        dc.l movelb,movelc,moveld,movele,movelh,movell,movelm,movela
        dc.l movemb,movemc,movemd,moveme,movemh,moveml,halt,movema
        dc.l moveab,moveac,movead,moveae,moveah,moveal,moveam,moveaa
        dc.l addb,addc,addd,adde,addh,addl,addm,addaa
        dc.l adcb,adcc,adcd,adce,adch,adcl,adcm,adca
        dc.l subb,subc,subd,sube,subh,subl,subm,subaa
        dc.l sbbb,sbbc,sbbd,sbbe,sbbh,sbbl,sbbm,sbba
        dc.l andb,andc,andd,ande,andh,andl,andm,anda
        dc.l xrab,xrac,xrad,xrae,xrah,xral,xram,xraa
        dc.l orab,orac,orad,orae,orah,oral,oram,oraa
        dc.l cmpb,cmpc,cmpd,cmpe,cmph,cmpl,cmpam,cmpaa
        dc.l rnz,popb,jnz,jmpa,cnz,pushb,adi,rst0
        dc.l rz,ret,jz,nopCB,cz,call,aci,rst8
        dc.l rnc,popd,jnc,out,cnc,pushd,sui,rst10
        dc.l rc,nopD9,jc,in,cc,nopDD,sbi,rst18
        dc.l rpo,poph,jpo,xthl,cpo,pushh,ani,rst20
        dc.l rpe,pchl,jpe,xchg,cpe,preED,xri,rst28
        dc.l rp,popp,jp,di,cp,pushp,oria,rst30
        dc.l rm,sphl,jm,ei,cm,nopFD,cpi,rst38

        page
*************************************************************************
*                                                                       *
*       Flag register lookup tables.                                    *
*                                                                       *
*************************************************************************

flags   dc.b $00,$01,$04,$05,$40,$41,$44,$45,$80,$81,$84,$85,$C0,$C1,$C4,$C5

        dc.b $44,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84

        page
*************************************************************************
*                                                                       *
*       Opcode simulation subroutines.                                  *
*                                                                       *
*       Note:   I/O instructions are based at 68000 address $FF0000     *
*               as is appropriate for the CompuPro CPU-68K card.        *
*                                                                       *
*       Also, all routines assume that the high word of D0 = 0!         *
*                                                                       *
*************************************************************************

        text
        even

nop00   jmp (return)                            ; 00 Nop

lxib    move.b (pseudopc)+,regc(regs)           ; 01 Lxi BC,nnnn
        move.b (pseudopc)+,regb(regs)
        jmp (return)

staxb   move.w regb(regs),d0                    ; 02 Stax B
        move.b rega,0(targbase,d0.l)
        jmp (return)

inxb    inc.w regb(regs)                        ; 03 Inx B
        jmp (return)

inrb    inc.b regb(regs)                        ; 04 Inr B
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrb    dec.b regb(regs)                        ; 05 Dcr B
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvib    move.b (pseudopc)+,regb(regs)           ; 06 Mvi b,nn
        jmp (return)

rlca    rol.b #1,rega                           ; 07 Rlc
docyf   bcs rlc1
        bclr #0,regf
        jmp (return)
rlc1    bset #0,regf
        jmp (return)

nop08   bra illegl                              ; 08 Illegal for 8080

dadb    move.w regb(regs),d0                    ; 09 Dad B
        add.w d0,regh(regs)
        bra docyf

ldaxb   move.w regb(regs),d0                    ; 0A Ldax B
        move.b 0(targbase,d0.l),rega
        jmp (return)

dcxb    dec.w regb(regs)                        ; 0B Dcx B
        jmp (return)

inrc    inc.b regc(regs)                        ; 0C Inr C
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrc    dec.b regc(regs)                        ; 0D Dcr C
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvic    move.b (pseudopc)+,regc(regs)           ; 0E Mvi C
        jmp (return)

rrca    ror.b #1,rega                           ; 0F Rrc
        bra docyf

nop10   bra illegl                              ; 10 Illegal for 8080


lxid    move.b (pseudopc)+,rege(regs)           ; 11 Lxi DE,nnnn
        move.b (pseudopc)+,regd(regs)
        jmp (return)


staxd   move.w regd(regs),d0                    ; 12 Stax D
        move.b rega,0(targbase,d0.l)
        jmp (return)


inxd    inc.w regd(regs)                        ; 13 Inx D
        jmp (return)

inrd    inc.b regd(regs)                        ; 14 Inr D
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrd    dec.b regd(regs)                        ; 15 Dcr D
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvid    move.b (pseudopc)+,regd(regs)           ; 16 Mvi D,nn
        jmp (return)

ral     roxr.b #1,regf                          ; 17 Ral
        roxl.b #1,rega
        roxl.b #1,regf
        jmp (return)

nop18   bra illegl                              ; 18 Illegal for 8080

dadd    move.w regd(regs),d0                    ; 19 Dad D
        add.w d0,regh(regs)
        bra docyf

ldaxd   move.w regd(regs),d0                    ; 1A Ldax D
        move.b 0(targbase,d0.l),rega
        jmp (return)

dcxd    dec.w regd(regs)                        ; 1B Dcx D
        jmp (return)

inre    inc.b rege(regs)                        ; 1C Inr E
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcre    dec.b rege(regs)                        ; 1D Dcr E
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvie    move.b (pseudopc)+,rege(regs)           ; 1E Mvi E,nn
        jmp (return)

rar     roxr.b #1,regf                          ; 1F Rar
        roxr.b #1,rega
        roxl.b #1,regf
        jmp (return)

nop20   bra illegl                              ; 20 Illegal for 8080

lxih    move.b (pseudopc)+,regl(regs)           ; 21 Lxi H,nnnn
        move.b (pseudopc)+,regh(regs)
        jmp (return)

shld    move.b 1(pseudopc),d0                   ; 22 Shld addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.l d0,a0
        adda.l targbase,a0
        move.b regl(regs),(a0)+
        move.b regh(regs),(a0)
        jmp (return)

inxh    inc.w regh(regs)                        ; 23 Inx H
        jmp (return)

inrh    inc.b regh(regs)                        ; 24 Inr H
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrh    dec.b regh(regs)                        ; 25 Dcr H
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvih    move.b (pseudopc)+,regh(regs)           ; 26 Mvi H,nn
        jmp (return)

daa     move.b regop3(regs),d0                  ; 27 Daa
        roxr.b d0
        move.b regop2(regs),d0
        move.b regop1(regs),d1
        swap regcon0e
        move.b rega,regcon0e
        and.b regcon0f,regcon0e
        cmp.b #9,regcon0e
        bhi halfcy
        and.b regcon0f,d0
        and.b regcon0f,d1
        ori.b #$f0,d1
        addx.b d0,d1
        bcc nohalf
halfcy  add.b #6,rega
        bcs fullcy
nohalf  btst #0,regf
        bnz fullcy
        move.b rega,regcon0e
        and.b #$f0,regcon0e
        cmp.b #$90,regcon0e
        bls nofull
fullcy  add.b #$60,rega
        ori #1,ccr
enddaa  move sr,regf
        swap regcon0e
        and.w regcon0f,regf
        move.b 0(flagptr,regf.w),regf
        jmp (return)
nofull  tst.b rega
        bra enddaa

nop28   bra illegl                              ; 28 Illegal for 8080


dadh    move.w regh(regs),d0                    ; 29 Dad H
        add.w d0,regh(regs)
        bra docyf

lhld    move.b 1(pseudopc),d0                   ; 2A Lhld addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.l d0,a0
        adda.l targbase,a0
        move.b (a0)+,regl(regs)
        move.b (a0),regh(regs)
        jmp (return)

dcxh    dec.w regh(regs)                        ; 2B Dcx H
        jmp (return)

inrl    inc.b regl(regs)                        ; 2C Inr L
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrl    dec.b regl(regs)                        ; 2D Dcr L
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvil    move.b (pseudopc)+,regl(regs)           ; 2E Mvi L,nn
        jmp (return)

cma     not.b rega                              ; 2F Cma
        jmp (return)

nop30   bra illegl                              ; 30 Illegal for 8080

lxis    move.b 1(pseudopc),d0                   ; 31 Lxi S,nnnn
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.l d0,pseudosp
        adda.l targbase,pseudosp
        jmp (return)

sta     move.b 1(pseudopc),d0                   ; 32 Sta addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.b rega,0(targbase,d0.l)
        jmp (return)

inxs    addq.l #1,pseudosp                      ; 33 Inx S
        jmp (return)

inrm    move.w regh(regs),d0                    ; 34 Inr M
        inc.b 0(targbase,d0.l)
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcrm    move.w regh(regs),d0                    ; 35 Dcr M
        dec.b 0(targbase,d0.l)
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvim    move.w regh(regs),d0                    ; 36 Mvi M,nn
        move.b (pseudopc)+,0(targbase,d0.l)
        jmp (return)

stc     bset #0,regf                            ; 37 Stc
        jmp (return)

nop38   bra illegl                              ; 38 Illegal for 8080

dads    move.l pseudosp,d0                      ; 39 Dad S
        sub.l targbase,d0
        add.w d0,regh(regs)
        bra docyf

lda     move.b 1(pseudopc),d0                   ; 3A Lda addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.b 0(targbase,d0.l),rega
        jmp (return)

dcxs    subq.l #1,pseudosp                      ; 3B Dcx S
        jmp (return)

inra    move.b rega,regop1(regs)                ; 3C Inr A
        move.b regcon01,regop2(regs)
        move.b regcon0e,regop3(regs)
        inc.b rega
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

dcra    dec.b rega                              ; 3D Dcr A
        move sr,d0
        and.w regcon0e,d0
        and.w regcon01,regf
        or.b 0(flagptr,d0.w),regf
        jmp (return)

mvia    move.b (pseudopc)+,rega                 ; 3E Mvi A,nn
        jmp (return)

cmc     bchg #0,regf                            ; 3F Cmc
        jmp (return)

movebb  move.b regb(regs),regb(regs)            ; 40 Mov B,B
        jmp (return)

movebc  move.b regc(regs),regb(regs)            ; 41 Mov B,C
        jmp (return)

movebd  move.b regd(regs),regb(regs)            ; 42 Mov B,D
        jmp (return)

movebe  move.b rege(regs),regb(regs)            ; 43 Mov B,E
        jmp (return)

movebh  move.b regh(regs),regb(regs)            ; 44 Mov B,H
        jmp (return)

movebl  move.b regl(regs),regb(regs)            ; 45 Mov B,L
        jmp (return)

movebm  move.w regh(regs),d0                    ; 46 Mov B,M
        move.b 0(targbase,d0.l),regb(regs)
        jmp (return)

moveba  move.b rega,regb(regs)                  ; 47 Mov B,A
        jmp (return)

movecb  move.b regb(regs),regc(regs)            ; 48 Mov C,B
        jmp (return)

movecc  move.b regc(regs),regc(regs)            ; 49 Mov C,C
        jmp (return)

movecd  move.b regd(regs),regc(regs)            ; 4A Mov C,D
        jmp (return)

movece  move.b rege(regs),regc(regs)            ; 4B Mov C,E
        jmp (return)

movech  move.b regh(regs),regc(regs)            ; 4C Mov C,H
        jmp (return)

movecl  move.b regl(regs),regc(regs)            ; 4D Mov C,L
        jmp (return)

movecm  move.w regh(regs),d0                    ; 4E Mov C,M
        move.b 0(targbase,d0.l),regc(regs)
        jmp (return)

moveca  move.b rega,regc(regs)                  ; 4F Mov C,A
        jmp (return)

movedb  move.b regb(regs),regd(regs)            ; 50 Mov D,B
        jmp (return)

movedc  move.b regc(regs),regd(regs)            ; 51 Mov D,C
        jmp (return)

movedd  move.b regd(regs),regd(regs)            ; 52 Mov D,D
        jmp (return)

movede  move.b rege(regs),regd(regs)            ; 53 Mov D,E
        jmp (return)

movedh  move.b regh(regs),regd(regs)            ; 54 Mov D,H
        jmp (return)

movedl  move.b regl(regs),regd(regs)            ; 55 Mov D,L
        jmp (return)

movedm  move.w regh(regs),d0                    ; 56 Mov D,M
        move.b 0(targbase,d0.l),regd(regs)
        jmp (return)

moveda  move.b rega,regd(regs)                  ; 57 Mov D,A
        jmp (return)

moveeb  move.b regb(regs),rege(regs)            ; 58 Mov E,B
        jmp (return)

moveec  move.b regc(regs),rege(regs)            ; 59 Mov E,C
        jmp (return)

moveed  move.b regd(regs),rege(regs)            ; 5A Mov E,D
        jmp (return)

moveee  move.b rege(regs),rege(regs)            ; 5B Mov E,E
        jmp (return)

moveeh  move.b regh(regs),rege(regs)            ; 5C Mov E,H
        jmp (return)

moveel  move.b regl(regs),rege(regs)            ; 5D Mov E,L
        jmp (return)

moveem  move.w regh(regs),d0                    ; 5E Mov E,M
        move.b 0(targbase,d0.l),rege(regs)
        jmp (return)

moveea  move.b rega,rege(regs)                  ; 5F Mov E,A
        jmp (return)

movehb  move.b regb(regs),regh(regs)            ; 60 Mov H,B
        jmp (return)

movehc  move.b regc(regs),regh(regs)            ; 61 Mov H,C
        jmp (return)

movehd  move.b regd(regs),regh(regs)            ; 62 Mov H,D
        jmp (return)

movehe  move.b rege(regs),regh(regs)            ; 63 Mov H,E
        jmp (return)

movehh  move.b regh(regs),regh(regs)            ; 64 Mov H,H
        jmp (return)

movehl  move.b regl(regs),regh(regs)            ; 65 Mov H,L
        jmp (return)

movehm  move.w regh(regs),d0                    ; 66 Mov H,M
        move.b 0(targbase,d0.l),regh(regs)
        jmp (return)

moveha  move.b rega,regh(regs)                  ; 67 Mov H,A
        jmp (return)

movelb  move.b regb(regs),regl(regs)            ; 68 Mov L,B
        jmp (return)

movelc  move.b regc(regs),regl(regs)            ; 69 Mov L,C
        jmp (return)

moveld  move.b regd(regs),regl(regs)            ; 6A Mov L,D
        jmp (return)

movele  move.b rege(regs),regl(regs)            ; 6B Mov L,E
        jmp (return)

movelh  move.b regh(regs),regl(regs)            ; 6C Mov L,H
        jmp (return)

movell  move.b regl(regs),regl(regs)            ; 6D Mov L,L
        jmp (return)

movelm  move.w regh(regs),d0                    ; 6E Mov L,M
        move.b 0(targbase,d0.l),regl(regs)
        jmp (return)

movela  move.b rega,regl(regs)                  ; 6F Mov L,A
        jmp (return)

movemb  move.w regh(regs),d0                    ; 70 Mov M,B
        move.b regb(regs),0(targbase,d0.l)
        jmp (return)

movemc  move.w regh(regs),d0                    ; 71 Mov M,C
        move.b regc(regs),0(targbase,d0.l)
        jmp (return)

movemd  move.w regh(regs),d0                    ; 72 Mov M,D
        move.b regd(regs),0(targbase,d0.l)
        jmp (return)

moveme  move.w regh(regs),d0                    ; 73 Mov M,E
        move.b rege(regs),0(targbase,d0.l)
        jmp (return)

movemh  move.w regh(regs),d0                    ; 74 Mov M,H
        move.b regh(regs),0(targbase,d0.l)
        jmp (return)

moveml  move.w regh(regs),d0                    ; 75 Mov M,L
        move.b regl(regs),0(targbase,d0.l)
        jmp (return)

halt    bsr service                             ; 76 Hlt
        jmp (return)

movema  move.w regh(regs),d0                    ; 77 Mov M,A
        move.b rega,0(targbase,d0.l)
        jmp (return)

moveab  move.b regb(regs),rega                  ; 78 Mov A,B
        jmp (return)

moveac  move.b regc(regs),rega                  ; 79 Mov A,C
        jmp (return)

movead  move.b regd(regs),rega                  ; 7A Mov A,D
        jmp (return)

moveae  move.b rege(regs),rega                  ; 7B Mov A,E
        jmp (return)

moveah  move.b regh(regs),rega                  ; 7C Mov A,H
        jmp (return)

moveal  move.b regl(regs),rega                  ; 7D Mov A,L
        jmp (return)

moveam  move.w regh(regs),d0                    ; 7E Mov A,M
        move.b 0(targbase,d0.l),rega
        jmp (return)

moveaa  jmp (return)                            ; 7F Mov A,A

addb    move.b regb(regs),d0                    ; 80 Add B
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addc    move.b regc(regs),d0                    ; 81 Add C
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addd    move.b regd(regs),d0                    ; 82 Add D
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adde    move.b rege(regs),d0                    ; 83 Add E
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addh    move.b regh(regs),d0                    ; 84 Add H
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addl    move.b regl(regs),d0                    ; 85 Add L
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addm    move.w regh(regs),d0                    ; 86 Add M
        move.b 0(targbase,d0.l),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

addaa   move.b rega,regop1(regs)                ; 87 Add A
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b rega,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adcb    move.b regf,regop3(regs)                ; 88 Adc B
        asr.b #1,regf
        move.b regb(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adcc    move.b regf,regop3(regs)                ; 89 Adc C
        asr.b #1,regf
        move.b regc(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adcd    move.b regf,regop3(regs)                ; 8A Adc D
        asr.b #1,regf
        move.b regd(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adce    move.b regf,regop3(regs)                ; 8B Adc E
        asr.b #1,regf
        move.b rege(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adch    move.b regf,regop3(regs)                ; 8C Adc H
        asr.b #1,regf
        move.b regh(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adcl    move.b regf,regop3(regs)                ; 8D Adc L
        asr.b #1,regf
        move.b regl(regs),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adcm    move.b regf,regop3(regs)                ; 8E Adc M
        move.w regh(regs),d0
        move.l d0,a0
        adda.l targbase,a0
        asr.b #1,regf
        move.b (a0),d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

adca    move.b regf,regop3(regs)                ; 8F Adc A
        asr.b #1,regf
        move.b rega,d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subb    move.b regb(regs),d0                    ; 90 Sub B
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subc    move.b regc(regs),d0                    ; 91 Sub C
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subd    move.b regd(regs),d0                    ; 92 Sub D
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sube    move.b rege(regs),d0                    ; 93 Sub E
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subh    move.b regh(regs),d0                    ; 94 Sub H
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subl    move.b regl(regs),d0                    ; 95 Sub L
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subm    move.w regh(regs),d0                    ; 96 Sub M
        move.b 0(targbase,d0.l),d0
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

subaa   move.b rega,d0                          ; 97 Sub A
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbb    asr.b #1,regf                           ; 98 Sbb B
        move.b regb(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbc    asr.b #1,regf                           ; 99 Sbb C
        move.b regc(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbd    asr.b #1,regf                           ; 9A Sbb D
        move.b regd(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbe    asr.b #1,regf                           ; 9B Sbb E
        move.b rege(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbh    asr.b #1,regf                           ; 9C Sbb H
        move.b regh(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbl    asr.b #1,regf                           ; 9D Sbb L
        move.b regl(regs),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbbm    move.w regh(regs),d0                    ; 9E Sbb M
        move.l d0,a0
        adda.l targbase,a0
        asr.b #1,regf
        move.b (a0),d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

sbba    asr.b #1,regf                           ; 9F Sbb A
        move.b rega,d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

andb    move.b regb(regs),d0                    ; A0 Ana B
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

andc    move.b regc(regs),d0                    ; A1 Ana C
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

andd    move.b regd(regs),d0                    ; A2 Ana D
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

ande    move.b rege(regs),d0                    ; A3 Ana E
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

andh    move.b regh(regs),d0                    ; A4 Ana H
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

andl    move.b regl(regs),d0                    ; A5 Ana L
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

andm    move.w regh(regs),d0                    ; A6 Ana M
        move.b 0(targbase,d0.l),d0
        and.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

anda    move.b rega,d0                          ; A7 Ana A
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xrab    move.b regb(regs),d0                    ; A8 Xra B
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xrac    move.b regc(regs),d0                    ; AA Xra C
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xrad    move.b regd(regs),d0                    ; AA Xra D
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xrae    move.b rege(regs),d0                    ; AB Xra E
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xrah    move.b regh(regs),d0                    ; AC Xra H
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xral    move.b regl(regs),d0                    ; AD Xra L
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xram    move.w regh(regs),d0                    ; AE Xra M
        move.b 0(targbase,d0.l),d0
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

xraa    moveq #0,rega                           ; AF Xra A
        move.b 16(flagptr),regf
        jmp (return)

orab    move.b regb(regs),d0                    ; B0 Ora B
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

orac    move.b regc(regs),d0                    ; B1 Ora C
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

orad    move.b regd(regs),d0                    ; B2 Ora D
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

orae    move.b rege(regs),d0                    ; B3 Ora E
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

orah    move.b regh(regs),d0                    ; B4 Ora H
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

oral    move.b regl(regs),d0                    ; B5 Ora L
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

oram    move.w regh(regs),d0                    ; B6 Ora M
        move.b 0(targbase,d0.l),d0
        or.b rega,d0
        move.b d0,rega
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

oraa    move.b rega,d0                          ; B7 Ora A
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)


cmpb    cmp.b regb(regs),rega                   ; B8 Cmp B
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpc    cmp.b regc(regs),rega                   ; BB Cmp C
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpd    cmp.b regd(regs),rega                   ; BA Cmp D
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpe    cmp.b rege(regs),rega                   ; BB Cmp E
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmph    cmp.b regh(regs),rega                   ; BC Cmp H
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpl    cmp.b regl(regs),rega                   ; BD Cmp L
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpam   move.w regh(regs),d0                    ; BE Cmp M
        move.l d0,a0
        adda.l targbase,a0
        cmp.b (a0),rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

cmpaa   cmp.b rega,rega                         ; BF Cmp A
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rnz     btst #6,regf                            ; C0 Rnz
        bne mloop
ret     move.b 1(pseudosp),d0                   ; C9 Ret
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)


popb    move.b (pseudosp)+,regc(regs)           ; C1 Pop B
        move.b (pseudosp)+,regb(regs)
        jmp (return)

jnz     move.b 1(pseudopc),d0                   ; C2 Jnz addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #6,regf
        bne mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

jmpa    move.b 1(pseudopc),d0                   ; C3 Jmp addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

cnz     move.b 1(pseudopc),d0                   ; C4 Cnz addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #6,regf
        bne mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

pushb   move.b regb(regs),-(pseudosp)           ; C5 Push B
        move.b regc(regs),-(pseudosp)
        jmp (return)

adi     move.b (pseudopc)+,d0                   ; C6 Adi nn
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        move.b regcon0e,regop3(regs)
        add.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rst0    move.l pseudopc,d1                      ; C7 Rst 0
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        move.l targbase,pseudopc
        jmp (return)

rz      btst #6,regf                            ; C8 Rz
        beq mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

jz      move.b 1(pseudopc),d0                   ; CA Jz addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #6,regf
        beq mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

nopCB   bra illegl                              ; CB Illegal for 8080


cz      move.b 1(pseudopc),d0                   ; CC Cz addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #6,regf
        beq mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

call    move.b 1(pseudopc),d0                   ; CD Call addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

aci     move.b regf,regop3(regs)                ; CE Aci nn
        asr.b #1,regf
        move.b (pseudopc)+,d0
        move.b d0,regop1(regs)
        move.b rega,regop2(regs)
        moveq #0,d1
        addx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rst8    move.l pseudopc,d1                      ; CF Rst 8
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $8(targbase),pseudopc
        jmp (return)

rnc     btst #0,regf                            ; D0 Rnc
        bne mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

popd    move.b (pseudosp)+,rege(regs)           ; D1 Pop D
        move.b (pseudosp)+,regd(regs)
        jmp (return)

jnc     move.b 1(pseudopc),d0                   ; D2 Jnc addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #0,regf
        bne mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

out     moveq #0,d0                             ; D3 Out nn
        move.b (pseudopc)+,d0

        ifne diskio
        cmp.b #$54,d0
        beq outspec
        cmp.b #$55,d0
        beq outspec
        endc

        move.l #$ff0000,a0
        move.b rega,0(a0,d0.l)
        jmp (return)

cnc     move.b 1(pseudopc),d0                   ; D4 Cnc addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #0,regf
        bne mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

pushd   move.b regd(regs),-(pseudosp)           ; D5 Push D
        move.b rege(regs),-(pseudosp)
        jmp (return)

sui     move.b (pseudopc)+,d0                   ; D6 Sui nn
        sub.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rst10   move.l pseudopc,d1                      ; D7 Rst 10
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $10(targbase),pseudopc
        jmp (return)

rc      btst #0,regf                            ; D8 Rc
        beq mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

nopD9   bra illegl                              ; D9 Illegal for 8080

jc      move.b 1(pseudopc),d0                   ; DA Jc addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #0,regf
        beq mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

in      moveq #0,d0                             ; DB In nn
        move.b (pseudopc)+,d0
        move.l #$ff0000,a0
        move.b 0(a0,d0.l),rega
        jmp (return)

cc      move.b 1(pseudopc),d0                   ; DC Cc addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #0,regf
        beq mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

nopDD   bra illegl                              ; DD Illegal for 8080

sbi     asr.b #1,regf                           ; DE Sbi nn
        move.b (pseudopc)+,d0
        moveq #0,d1
        subx.b d0,rega
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rst18   move.l pseudopc,d1                      ; DF Rst 18
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $18(targbase),pseudopc
        jmp (return)

rpo     btst #2,regf                            ; E0 Rpo
        bne mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

poph    move.b (pseudosp)+,regl(regs)           ; E1 Pop H
        move.b (pseudosp)+,regh(regs)
        jmp (return)

jpo     move.b 1(pseudopc),d0                   ; E2 Jpo addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #2,regf
        bne mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

xthl    move.b regl(regs),d0                    ; E3 Xthl
        move.b (pseudosp),regl(regs)
        move.b d0,(pseudosp)
        move.b regh(regs),d0
        move.b 1(pseudosp),regh(regs)
        move.b d0,1(pseudosp)
        jmp (return)

cpo     move.b 1(pseudopc),d0                   ; E4 Cpo addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #2,regf
        bne mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)


pushh   move.b regh(regs),-(pseudosp)           ; E5 Push H
        move.b regl(regs),-(pseudosp)
        jmp (return)

ani     and.b (pseudopc)+,rega                  ; E6 Ani nn
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

rst20   move.l pseudopc,d1                      ; E7 Rst 20
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $20(targbase),pseudopc
        jmp (return)

rpe     btst #2,regf                            ; E8 Rpe
        beq mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

pchl    move.w regh(regs),d0                    ; E9 Pchl
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

jpe     move.b 1(pseudopc),d0                   ; EA Jpe addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #2,regf
        beq mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

xchg    move.w regd(regs),d0                    ; EB Xchg
        move.w regh(regs),regd(regs)
        move.w d0,regh(regs)
        jmp (return)

cpe     move.b 1(pseudopc),d0                   ; EC Cpe addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #2,regf
        beq mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

*preED  bra illegl                              ; ED Illegal for 8080

*       ED is a prefix for the popular Z-80 instructions.  Some support
*       for them is provided by the minimal Z-80 simulation routines in
*       the next file.

xri     move.b (pseudopc)+,d0                   ; EE Xri nn
        eor.b d0,rega
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

rst28   move.l pseudopc,d1                      ; EF Rst 28
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $28(targbase),pseudopc
        jmp (return)

rp      btst #7,regf                            ; F0 Rp
        bne mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

popp    move.b (pseudosp)+,regf                 ; F1 Pop P
        move.b (pseudosp)+,rega
        jmp (return)

jp      move.b 1(pseudopc),d0                   ; F2 Jp addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #7,regf
        bne mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

di      jmp (return)                            ; F3 Di

cp      move.b 1(pseudopc),d0                   ; F4 Cp addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #7,regf
        bne mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

pushp   move.b rega,-(pseudosp)                 ; F5 Push P
        move.b regf,-(pseudosp)
        jmp (return)

oria    or.b (pseudopc)+,rega                   ; F6 Ori nn
        move.b rega,d0
        and.w regconff,d0
        move.b 16(flagptr,d0.w),regf
        jmp (return)

rst30   move.l pseudopc,d1                      ; F7 Rst 30
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $30(targbase),pseudopc
        jmp (return)

rm      btst #7,regf                            ; F8 Rm
        beq mloop
        move.b 1(pseudosp),d0
        rol.w #8,d0
        move.b (pseudosp),d0
        addq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

sphl    move.w regh(regs),d0                    ; F9 Sphl
        lea.l 0(targbase,d0.l),pseudosp
        jmp (return)

jm      move.b 1(pseudopc),d0                   ; FA Jm addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #7,regf
        beq mloop
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

ei      jmp (return)                            ; FB Ei

cm      move.b 1(pseudopc),d0                   ; FC Cm addr
        rol.w #8,d0
        move.b (pseudopc),d0
        addq.l #2,pseudopc
        btst #7,regf
        beq mloop
        move.l pseudopc,d1
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l 0(targbase,d0.l),pseudopc
        jmp (return)

nopFD   bra illegl                              ; FD Illegal for 8080

cpi     cmp.b (pseudopc)+,rega                  ; FE Cpi nn
        move sr,d0
        and.w regcon0f,d0
        move.b 0(flagptr,d0.w),regf
        jmp (return)

rst38   move.l pseudopc,d1                      ; FF Rst 38
        sub.l targbase,d1
        move.b d1,-2(pseudosp)
        rol.w #8,d1
        move.b d1,-1(pseudosp)
        subq.l #2,pseudosp
        lea.l $38(targbase),pseudopc
        jmp (return)


        .end
