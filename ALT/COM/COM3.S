*************************************************************************
*                                                                       *
*       This file contains the special Z-80 simulation routines and     *
*       the Morrow HDC/DMA support routines.                            *
*                                                                       *
*************************************************************************

        globl preED,outspec
        xdef mloop,illegl

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
regcondd equ 4,r        ; Register based constant #$FF.
regf     equ 3,r        ; 8080's Flags
rega     equ 2,r        ; 8080's Accumulator

regb    equ -8          ; Offsets from register base pointer for
regc    equ -7          ; 8080's pseudo-registers.
regd    equ -6          ; A & F are in Data Registers.
rege    equ -5          ; Pseudo-PC is kept in an Address Register.
regh    equ -4
regl    equ -3
regop1  equ -2          ; Operand 1 for DAA storage
regop2  equ -1          ;    "    2  "   "     "


        data
        page
*************************************************************************
*                                                                       *
*       Opcode dispatch table.  One longword entry per opcode of the    *
*       target (Z-80) processor, including illegals.                    *
*                                                                       *
*************************************************************************
*                                                                       *
*       Only a few of the most popular instructions are simulated.      *
*       Support for the Z-80 Block move instructions is provided        *
*       as the flags for this simulation resemble those of the Z-80     *
*       rather than the 8080.  Certain packages (notably BDS-C) check   *
*       the flags and mistakenly assume a Z-80, then use LDIR/LDDR.     *
*       Therefore, minimal Z-80 support is provided for these           *
*       instructions.  By no means is this a complete simulation        *
*       of the Z-80.                                                    *
*                                                                       *
*************************************************************************

        even
EDoptab dc.l 0,0,0,0,0,0,0,0            ; ED00
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED10
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED20
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED30
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED40
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED50
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED60
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED70
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED80
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; ED90
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; EDA0
        dc.l 0,0,0,0,0,0,0,0
        dc.l ldir,cpir,0,0,0,0,0,0      ; EDB0
        dc.l lddr,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; EDC0
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; EDD0
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; EDE0
        dc.l 0,0,0,0,0,0,0,0
        dc.l 0,0,0,0,0,0,0,0            ; EDF0
        dc.l 0,0,0,0,0,0,0,0

        page
        text
preED   moveq #0,d1                     ; Zero-fill high bits.
        move.b (pseudopc)+,d1           ; Grab next opcode.
        asl #2,d1
        lea.l EDoptab,a0
        move.l 0(a0,d1.w),-(sp)         ; Do the operation.
        beq illgED
        rts

illgED  move.l (sp)+,d1                 ; Trash the address,
        dec.l pseudopc                  ; fix PPC for ILLEGAL.
        bra illegl

        page
*************************************************************************
*                                                                       *
*       Z-80 opcode simulation routines.                                *
*                                                                       *
*************************************************************************


ldir    move.l d2,-(sp)
        move.w regb(regs),d0            ; Grab count,
        dec.w d0                        ; adjust for DBRA later.
        moveq #0,d1
        moveq #0,d2
        move.w regh(regs),d1            ; Grab source.
        move.w regd(regs),d2            ; Grab dest.
        move.l a5,-(sp)                 ; Need an address reg.
        lea.l 0(targbase,d2.l),a5
        lea.l 0(targbase,d1.l),a0
ldirlop move.b (a0)+,(a5)+
        inc.w d1
        inc.w d2
        dbra d0,ldirlop
        move.l (sp)+,a5
        move.w d1,regh(regs)            ; Restore result registers.
        move.w d2,regd(regs)
        move.w #0,regb(regs)
        moveq #0,regf
        move.l (sp)+,d2
        jmp (return)

lddr    move.l d2,-(sp)
        move.w regb(regs),d0            ; Grab count,
        dec.w d0                        ; adjust for DBRA later.
        moveq #0,d1
        moveq #0,d2
        move.w regh(regs),d1            ; Grab source.
        move.w regd(regs),d2            ; Grab dest.
        move.l a5,-(sp)                 ; Need an address reg.
        lea.l 1(targbase,d2.l),a5
        lea.l 1(targbase,d1.l),a0
lddrlop move.b -(a0),-(a5)
        dec.w d1
        dec.w d2
        dbra d0,lddrlop
        move.l (sp)+,a5
        move.w d1,regh(regs)            ; Restore result registers.
        move.w d2,regd(regs)
        move.w #0,regb(regs)
        moveq #0,regf
        move.l (sp)+,d2
        jmp (return)

        page
cpir    move.w regb(regs),d0            ; Grab count,
        dec.w d0                        ; adjust for DBRA later.
        moveq #0,d1
        move.w regh(regs),d1            ; Grab source.
        lea.l 0(targbase,d1.l),a0
cpirlop inc.w d1
        cmp.b (a0)+,rega
        dbeq d0,cpirlop
        seq regf
        move.w d1,regh(regs)            ; Restore result registers.
        inc.w d0
        move.w d0,regb(regs)
        tst.b regf
        bne cpir1
        moveq #0,regf                   ; Not found.
        jmp (return)
cpir1   tst d0
        beq cpir2
        moveq #$44,regf                 ; Found, in the string.
        jmp (return)
cpir2   moveq #$40,regf                 ; Found, but at last place.
        jmp (return)

        page
*************************************************************************
*                                                                       *
*       Output instruction simulator for Morrow HDDMA                   *
*                                                                       *
*************************************************************************


outspec move.l d3,-(sp)
        cmp.b #$55,d0
        beq hdstart                     ; Start command?  Do it,
        move.l #hdbuf,d1                ; else build first link to host buffer
        move.l #$50,a0                  ; if it's a HDRESET command.
        move.b d1,(a0)+
        ror.l #8,d1
        move.b d1,(a0)+
        ror.l #8,d1
        move.b d1,(a0)+

        move.l #$ff0000,a0              ; Do the output to HDDMA.
        adda.l d0,a0
        move.b rega,(a0)
        move.l (sp)+,d3
        jmp (return)

hdstart move.l dmalink,a0               ; Move target buffer to host buffer, do
        adda.l targbase,a0              ; all appropriate patching of addresses.
        moveq #0,d1
        move.b (a0)+,d1                 ; Get link address.
        ror.l #8,d1
        move.b (a0)+,d1
        ror.l #8,d1
        move.b (a0)+,d1
        rol.l #8,d1
        rol.l #8,d1
        move.l d1,a0
        adda.l targbase,a0
        move.l a6,-(sp)
        lea.l hdbuf,a6
        move.w #3,d1
hdloop  move.b (a0)+,(a6)+
        dbra d1,hdloop
        moveq #0,d3                     ; Fix DMA address to -> target area.
        move.b (a0)+,d3
        ror.l #8,d3
        move.b (a0)+,d3
        ror.l #8,d3
        move.b (a0)+,d3
        rol.l #8,d3
        rol.l #8,d3
        add.l targbase,d3
        move.b d3,(a6)+
        ror.l #8,d3
        move.b d3,(a6)+
        ror.l #8,d3
        move.b d3,(a6)+
        move.w #5,d1                    ; Move rest of command buffer.
hdloop2 move.b (a0)+,(a6)+
        dbra d1,hdloop2

        move.l #hdbuf,d1
        move.b d1,(a6)+                 ; Point host buffer to self.
        ror.l #8,d1
        move.b d1,(a6)+
        ror.l #8,d1
        move.b d1,(a6)+
        move.l (sp)+,a6
        move.l a0,-(sp)                 ; Save STATUS address for return val.
        suba.l targbase,a0
        move.l a0,dmalink               ; Stash new target link address.
        
        move.l #$ff0000,a0              ; Do the output to HDDMA.
        adda.l d0,a0
        move.b rega,(a0)

        move.l #hdbuf+12,a0             ; Wait for completion
hdloop3 tst.b (a0)
        beq hdloop3                     ; Fragile, but what do you want for $1.00?
        move.b (a0),d1
        move.l (sp)+,a0                 ; Grab the STATUS address in target space.
        move.b d1,-(a0)                 ; And stash status for it.
        move.l (sp)+,d3
        jmp (return)                    ; Return to simulation.

        data
        even

dmalink dc.l $50                ; Storage for current HDDMA command buffer.
hdbuf   ds.b 16

        end
