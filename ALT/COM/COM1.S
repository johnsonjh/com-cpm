*************************************************************************
*                                                                       *
*                                                                       *
*       8080 Simulator for MC68000                                      *
*                                                                       *
*       With CP/M 2.2 call support, optional tracing and                *
*       Morrow HDDMA DMA buffer translating.                            *
*                                                                       *
*                                                                       *
*       Version 1.2 1/21/85 JEC                                         *
*               Fixed Extent bug in OPEN logic.                         *
*               Sped up code, sample MAC from 2:13 to 1:40.             *
*               Now runs at a 1.4 MHz equivalent based on MAC sample.   *
*                                                                       *
*       Version 1.1 8/29/84 JEC                                         *
*               Fixed BDOS call #6 bug.                                 *
*                                                                       *
*       Version 1.0 05/25/84 by Jim Cathey                              *
*                                                                       *
*       This program has been written for speed wherever possible,      *
*       as such tends to be large because of the separate subroutine    *
*       for each and every opcode of the target processor.              *
*                                                                       *
*       On an 8MHz 68000 (Compupro) system the simulation speed is      *
*       a little better than a 1MHz Z-80 when running MAC.  The time    *
*       for a sample assembly was 2:13 for the simulation vs 0:35       *
*       on a 4MHz Z-80, both systems used identical hard disk systems.  *
*                                                                       *
*       It is not a complete simulation, as some flag handling          *
*       isn't quite right, but it is enough to run the programs         *
*       I wrote it for (DDT, LU, MAC, and Morrow's FORMATMW).           *
*                                                                       *
*************************************************************************
        text
        page
*************************************************************************
*                                                                       *
*       This file contains the startup routines, the simulator core,    *
*       tracing code, and the CP/M 2.2 simulation.                      *
*                                                                       *
*************************************************************************

        xdef optabl,flags,mnops
        globl mloop,illegl,service

*
*       Conditional assembly flags.
*
trace   equ 0           ; Non-zero for trace routine inclusion.
trcdsk  equ 0           ; Non-zero for FCB trace routine inclusion.
dmpdsk  equ 0           ; Non-zero for register dump in FCB trace.
*  !! diskio is in file COM2.S !!
*diskio equ 0           ; Non-zero for special HDDMA support.


*
*       Register definitions for the simulation.
*
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
regf     equ 3,r        ; 8080's Flags
rega     equ 2,r        ; 8080's Accumulator

*
*       Note, only leaves D0-D1/A0 for free use by entire
*       program without saving registers for temporary use.
*

bdos    .opd 0,$4e42    ; BDOS 'macro'.
bios    .opd 0,$4e43    ; BIOS 'macro'.

        page
*************************************************************************
*                                                                       *
*       Initialization and Main Opcode dispatcher.                      *
*                                                                       *
*************************************************************************

start   lea.l target,targbase   ; Start of target memory.
        ifne trace              ; Optional trace code.
        bsr entrads             ; Enter trace delimiting addresses
*                               ; if the code is desired.
        endc
        bsr lodfdos             ; Load up the fake FDOS in target mem.
        bsr lodregs             ; Load the remaining simulation registers.
        bsr loadcom             ; Load the .COM program,
        tst d0                  ; quit if unsuccessful.
        bne optprnt
        rts

optprnt equ *
        ifne trcdsk             ; If FCB tracing, print header.
        lea.l fcbmsg,a0
        bsr lpstr
        endc


mloop:  *                       ; Execute simulation
~~mloop:
        ifne trace              ; Optional trace.
        tst traceflg
        bne dotrace
        cmpa.l tracesad,pseudopc
        bne notrace
        move.b #1,traceflg
dotrace bsr dump
        cmpa.l traceead,pseudopc
        bne notrace
        move.b #0,traceflg
notrace equ *
        endc

        moveq #0,d0             ; Execute appropriate simulation subroutine.
        move.b (pseudopc)+,d0   ; Grab next opcode.
        asl #2,d0               ; (D0 high word is still 0!)
        move.l 0(opptr,d0.w),a0
        jmp (a0)                ; To the subroutine.

        page
*************************************************************************
*                                                                       *
*       Illegal instructions and Dumping.                               *
*                                                                       *
*************************************************************************

illegl  move.l #illgmsg,d1      ; Illegal opcode, say what & where,
        move.w #9,d0
        bdos
        lea.l -1(pseudopc),a0
        move.b (a0),d1
        suba.l targbase,a0
        bsr pbyte
        move.l #ilgmsg2,d1
        move.w #9,d0
        bdos
        move.l a0,d1
        bsr pword
        move.l #ilgmsg3,d1
        move.w #9,d0
        bdos
        move.l #dumpmsg,d1
        move.w #9,d0
        bdos
        bsr dump                ; and spill guts.
        rts                     ; Quit simulation.

        page
dump    movem.l d0-d1/a0,-(sp)
        move.l #dmpmsg2,d1      ; Dump all registers,
        move.w #9,d0            ; used for illegals and tracing.
        bdos
        move.b rega,d1
        bsr pbyte
        move.b regf,d1
        bsr pbyte
        bsr pspace
        move.w regb(regs),d1
        bsr pword
        bsr pspace
        move.w regd(regs),d1
        bsr pword
        bsr pspace
        move.w regh(regs),d1
        bsr pword
        bsr pspace
        move.l pseudosp,d1
        sub.l targbase,d1
        bsr pword
        bsr pspace
        move.l pseudosp,a0
        swap d2                 ; Save REGA
        move.w #3,d2
tosloop move.b 1(a0),d1
        ror.w #8,d1
        move.b 0(a0),d1
        bsr pword
        bsr pspace
        addq.l #2,a0
        dbra d2,tosloop
        swap d2
        move.l pseudopc,d1
        sub.l targbase,d1
        bsr pword
        bsr pspace
        bsr pspace
        move.b (pseudopc),d1
        bsr pbyte
        bsr pspace              ; Now show mnemonic
        bsr pspace
        moveq #0,d0
        move.b (pseudopc),d0
        asl.w #2,d0
        lea.l mnops,a0
        move.l (a0,d0.l),d1
        move.l d1,-(sp)
        inc.l d1
        move #9,d0
        bdos
        move.l (sp)+,a0
        cmp.b #" ",(a0)
        beq nooprnd
        cmp.b #"C",(a0)
        bne notcons
        move.b 1(pseudopc),d1
        bsr pbyte
        bra nooprnd
notcons cmp.b #"A",(a0)
        bne nooprnd
        move.b 2(pseudopc),d1
        bsr pbyte
        move.b 1(pseudopc),d1
        bsr pbyte
nooprnd bsr pspace              ; In case of conout calls during trace,
        bsr pspace              ; they will be visible at end of line.
        bsr pspace
        movem.l (sp)+,d0-d1/a0
        rts

        page
*************************************************************************
*                                                                       *
*       Initialization subroutines.                                     *
*                                                                       *
*************************************************************************

lodfdos lea.l fdos,a6           ; Load up the fake FDOS.
        move.l targbase,pseudosp
        adda.l #$10000,pseudosp
        lea.l -256(pseudosp),a0
        move.w #fdoslen,d0
lodloop move.b (a6)+,(a0)+
        dbra d0,lodloop
        lea.l -256(pseudosp),a0
        move.l a0,d0
        sub.l targbase,d0
        move.b #$c3,0(targbase) ; Build BIOS & BDOS jumps.
        move.b #$c3,5(targbase)
        move.b d0,6(targbase)
        rol.w #8,d0
        move.b d0,7(targbase)
        rol.w #8,d0
        add.w #3,d0
        move.b d0,1(targbase)
        rol.w #8,d0
        move.b d0,2(targbase)
        move.w #0,-(pseudosp)   ; Set up a return stack to exit simulation.
        rts


lodregs lea.l optabl,opptr      ; Point base register to opcode dispatch table.
        lea.l mloop,return
        lea.l flags,flagptr
        move.l targbase,pseudopc
        adda.l #$100,pseudopc   ; Start execution at 0100H in target space.
        moveq #$e,regcon0e      ; Set up quick constants.
        moveq #$1,regcon01
        moveq #$f,regcon0f
        move.l #$ff,regconff
        moveq #0,rega
        moveq #0,regf
        rts

        page
entrads move.l #tracemsg,d1     ; Enter trace address if necessary.
        move.w #9,d0
        bdos
        bsr atol                ; Get trace start address.
        and.l #$ffff,d1
        move.l d1,a0
        adda.l targbase,a0
        move.l a0,tracesad
        move.l #tracemg2,d1
        move.w #9,d0
        bdos
        bsr atol                ; Get trace end address.
        and.l #$ffff,d1
        move.l d1,a0
        adda.l targbase,a0
        move.l a0,traceead
        move.w #10,d1           ; CRLF to end line.
        move.w #2,d0
        bdos
        move.w #13,d1
        move.w #2,d0
        bdos
        rts

*
*       OPEN file to be loaded, and load it into target
*       space if successful.
*
        .globl  loadcom
loadcom link a6,#0              ; Mark stack frame.
        movem.l d2-d4/a2-a5,-(sp)
        move.l 12(a6),a0        ; Get the address of the base page.
        lea.l $5c(a0),a2        ; Get FCB address.
        move.b #'C',9(a2)       ; mash filename to .COM
        move.b #'O',10(a2)
        move.b #'M',11(a2)
        move.l a2,d1
        move.w #15,d0
        bdos                    ; OPEN file.
        cmpi.w #255,d0          ; ERROR?
        beq openerr

        move.l pseudopc,d2      ; Start loading at $0100 in target.
filelod move.l d2,d1            ; Set DMA address.
        move.w #26,d0
        bdos
        move.l a2,d1
        move.w #20,d0           ; Read file until EOF.
        bdos
        tst d0
        bne basepg
        add.l #128,d2
        bra filelod

basepg  lea.l $80(targbase),a2  ; Set up the target's base page.
        move.l a2,d1            ; Start with default DMA address.
        move.l a2,dmaaddr
        move.w #26,d0
        bdos
        lea.l $38(a0),a2
        lea.l $5c(targbase),a3  ; Copy host's 2nd FCB to target's 1st FCB.
        move.w #15,d0
fcbloop move.b (a2)+,(a3)+
        dbra d0,fcbloop
        clr.b   (A3)+           ; zero out fcb #2 drive
        move.w  #10,D4          ; fill filename & type with blanks
fcbxlp  move.b  #' ',(A3)+
        dbra    D4,fcbxlp
        clr.b   (A3)+           ; rest of "fcb2"
        clr.b   (A3)+
        clr.b   (A3)+
        clr.b   (A3)+
        clr.b   (A3)+           ; rec# field
        clr.b   (A3)+
        clr.b   (A3)+
        clr.b   (A3)+
        lea.l $80(a0),a2
        lea.l $80(targbase),a4
        lea.l $81(targbase),a3
        clr d0
        move.b d0,(a4)
        move.b (a2)+,d0         ; Grab command tail from host's buffer.
tail1   cmp.b #$20,(a2)+        ; Hack off ?.COM filename.
        dbeq d0,tail1
        bne loaded              ; If there's any tail left, then
tail2   cmp.b #$20,(a2)+        ; remove leading whitespace.
        dbne d0,tail2
        beq loaded
        dec.l a2
        subq #2,d0
        move.w  D0,D4
        move.l  A2,A5           ; save ptr to head of command tail
tail3   move.b (a2)+,(a3)+      ; Move the rest of the tail.
        inc.b (a4)
        dbra d0,tail3
        move.b #0,(a3)
        move.l  A5,A2           ; restore ptr to head of command tail
        move.w  D4,D0           ; character count
        addq    #1,D0           ; fixed
        lea     $6c(targbase),A3 ; fcb2 base    
tail4   cmp.b #$20,(a2)+        ; Hack off first filename
        dbeq d0,tail4
        bne loaded              ; If there's any tail left, then
tail5   cmp.b #$20,(a2)+        ; remove leading whitespace.
        dbne d0,tail5
        beq loaded
        dec.l a2
        subq #1,d0
* now parse second file name
        clr.b   0(A3)           ; assume default drive
        cmpi.b  #':',1(A2)      ; is there a colon?
        bne     fcb2drv         ; skip if no colon
        move.b  (a2)+,D4        ; get drive letter
        tst.b   (a2)+           ; skip colon
        subq    #2,D0           ; and count 'em
        subi.b  #'@',D4         ; convert to drive letter
        cmpi.b  #0,D4           ; lower bound
        ble     loaded          ; skip if bogus drive
        cmpi.b  #16,D4          ; upper bound
        bgt     loaded          ; skip if bogus drive
        move.b  D4,0(A3)        ; stash drive #
fcb2drv lea     1(A3),A5        ; file name base
        moveq   #7,D2           ; #bytes in file name
fcb2fn  cmpi.b  #'.',(A2)       ; is dot?
        beq     fcb2t1          ; br if so
        cmpi.b  #'*',(A2)       ; wild card?
        beq     fcb2fq          ; yes, fill with '?'
        cmpi.b  #' ',(A2)       ; is blank?
        beq     loaded
        move.b  (A2)+,(A5)+     ; else move byte
        subq    #1,D0           ; count in byte
        dbeq    D2,fcb2fn       ; count and loop
        bne     loaded          ; no more characters
fcb2sl  cmpi.b  #'.',(A2)       ; is dot?       
        beq     fcb2t1          ; br if so
        cmpi.b  #' ',(A2)       ; is space?
        beq     loaded          ; if so, we are done
        tst.b   (A2)+           ; skip over char
        subq    #1,D0           ; and count it
        bne     fcb2sl          ; loop
        bra     loaded
fcb2fq  move.b  #'?',(A5)+      ; stash ?
        dbra    D2,fcb2fq       ; loop
        tst.b   (A2)+           ; skip *
        subq    #1,D0           ; and count it
        bra     fcb2sl          ; and go look for end or dot    
fcb2t1  tst.b   (A2)+           ; skip dot
        subq    #1,D0           ; and count it
        lea     09(A3),A5       ; type base
        moveq   #2,D2           ; count
fcb2ty  cmpi.b  #'*',(A2)       ; wild card?
        beq     fcb2tq          ; go handle
        cmpi.b  #' ',(A2)       ; blank?
        beq     loaded
        move.b  (A2)+,(A5)+     ; else move byte
        subq    #1,D0
        dbeq    D2,fcb2ty       ; count and loop
        bra     loaded          ; done.
fcb2tq  move.b  #'?',(A5)+      ; fill with '?'
        dbra    D2,fcb2tq       ; loop
        bra loaded

openerr move.l #opnmsg,d1       ; Can't open file.
        move.w #9,d0
        bdos
        clr d0

loaded  movem.l (sp)+,d2-d4/a2-a5
        unlk    a6              ; Trantor.
        rts


        page
*************************************************************************
*                                                                       *
*       BIOS and BDOS service request handler.                          *
*                                                                       *
*************************************************************************

service moveq #0,d0             ; Handle BIOS/BDOS service request
        move.b (pseudopc)+,d0   ; of form HLT DB opcode.
        bne biosfn              ; BDOS or BIOS?
bdosfn  moveq #0,d1
        move.b regc(regs),d0    ; Get BDOS function number.
        move.w regd(regs),d1    ; Get argument.
        cmp #31,d0              ; Can't do Disk Parm Hdr function
        beq badbdos
        cmp #27,d0              ; or ALLOC vector fn.
        bne okbdos
badbdos move.l #ilgbdos,d1
        move.w #9,d0
        bdos
        bsr dump
        bra quitprg

okbdos  cmp #9,d0               ; Translate target address to real address.
        blt noconv
        cmp #14,d0
        beq noconv
        cmp #32,d0
        beq noconv
        cmp #37,d0
        beq noconv
        add.l targbase,d1
noconv  cmp #26,d0              ; Save last known DMA address
        bne notdma              ; (in case of OPEN processing).
        move.l d1,dmaaddr
notdma  move.b #0,fcbflag       ; Separate FCB type requests
        cmp #15,d0              ; from the rest of the swine.
        blt notfcb              ; (Assume not, at first).
        cmp #24,d0
        blt fcb
        cmp #30,d0
        beq fcb
        cmp #33,d0
        blt notfcb
        cmp #37,d0
        blt fcb
        cmp #40,d0
        beq fcb
        bra notfcb

        page
fcb     swap d2
        move.w #35,d2           ; Move the FCB to host working buf,
        move.l d1,a0
        move.l a1,-(sp)
        lea.l fcbstor,a1
fcb1    move.b (a0)+,(a1)+
        dbra d2,fcb1
        move.l (sp)+,a1
        lea.l fcbstor,a0        ; and swap the random record bytes
        move.b 33(a0),d2        ; to make them match the 68000's.
        move.b 35(a0),33(a0)
        move.b d2,35(a0)
        swap d2
        move.b #1,fcbflag       ; Set flag for proper recovery.
        move.l d1,-(sp)         ; (Gotta put the pig back in pen!)
        move.l a0,d1
        ifne trcdsk
        ifne dmpdsk             ; Optional^2 Register dump.
        bsr dump
        endc
        endc
        cmp.w #15,d0            ; OPEN has a problem in that CP/M-68K
        bne notopen             ; can only open the base extent, unlike
        tst.b 12(a0)            ; CP/M-80.  So we have to check and do
        beq notopen             ; an OPEN then SEEK (RREAD) if required.
        bsr openproc
        bra results

notopen:
~~notopen:
        ifne trcdsk             ; Optional FCB trace.
        move.l d2,-(sp)
        move.b #' ',d2
        bsr fcbtrc1
        move.l (sp)+,d2
        endc


notfcb  cmp #6,d0               ; Not an FCB request.  Is it
        bne notdcon             ; a direct console I/O function?
        cmp.b #$ff,d1           ; Yes, make host's look like target's.
        bne notdcon
        move.w #$fe,d1
        bdos
        tst d0
        beq results
        move.w #6,d0
        move.w #$FF,d1

notdcon bdos                    ; FINALLY!  Do the translated function.
results move.w d0,regh(regs)
        move.b d0,rega
        move.b regh(regs),regb(regs)
        tst.b fcbflag           ; Do we need to restore a FCB?
        beq done
        ifne trcdsk
        bsr fcbtrc2
        endc
        lea.l fcbstor,a0        ; Restore the FCB to target, in proper order.
        swap d2
        move.b 33(a0),d2
        move.b 35(a0),33(a0)
        move.b d2,35(a0)
        move.l (sp)+,a0
        move.l a1,-(sp)
        lea.l fcbstor,a1
        move.w #35,d2
fcb2    move.b (a1)+,(a0)+
        dbra d2,fcb2
        swap d2
        move.l (sp)+,a1
done    move.b rega,d0
        and.w regconff,d0
        move.b 0(flagptr,d0.w),regf
        rts


openproc:
~~openproc:
        ifne trcdsk             ; Optional FCB trace.
        swap d2
        move.b #' ',d2
        bsr fcbtrc1
        swap d2
        bsr fcbtrc2a
        endc

        move.b 33(a0),-(sp)     ; Save away RR fields!
        move.b 34(a0),-(sp)
        move.b 35(a0),-(sp)
        movem.l d0-d2,-(sp)
        moveq #0,d2
        move.b 12(a0),d2        ; Save desired extent.
        clr.b 12(a0)
        bsr fcbbdos             ; Do BDOS (with opt. tracing).
        tst.b d0
        bmi badopen             ; No seek if not good OPEN.
        asl.l #7,d2             ; Make EXTENT # into record offset.
        moveq #0,d0
        move.b 32(a0),d0
        bclr #7,d0
        add.l d2,d0             ; Add onto CR to make abs record #.
        move.w d0,34(a0)        ; Put into FCB.
        swap d0
        move.b d0,33(a0)
        move.l #junkbuf,d1      ; Set DMA addr elsewhere for Rand Seek.
        move.w #26,d0
        bdos
        movem.l (sp)+,d0-d2
        move.w #33,d0           ; Random READ (SEEK) desired extent.
        bsr fcbbdos             ; Do BDOS (with opt. tracing).
        clr d0                  ; (OPEN) must always be successful because
*                               ; of the way CP/M-80 & CP/M-68K differ
*                               ; on OPENing non-zero extents.
        movem.l d0-d1,-(sp)     ; Restore the proper DMA address.
        move.w #26,d0
        move.l dmaaddr,d1
        bdos
        movem.l (sp)+,d0-d1
restore move.b (sp)+,35(a0)     ; Restore RR fields.
        move.b (sp)+,34(a0)
        move.b (sp)+,33(a0)
        rts

badopen movem.l (sp)+,d0-d2
        bra restore


fcbbdos:
~~fcbbdos:
        ifne trcdsk             ; BDOS call with optional FCB trace.
        move.l d2,-(sp)
        move.b #'+',d2
        bsr fcbtrc1
        move.l (sp)+,d2
        endc
        bdos
        ifne trcdsk
        bsr fcbtrc2
        endc
        rts


biosfn  cmp #1,d0               ; Handle Bios calls.
        beq quitprg
        cmp #$f,d0              ; List Status is ok.
        beq gudbios
        cmp #7,d0
        bge badbios             ; Don't allow disk functions!
gudbios clr.w d1
        move.b regc(regs),d1
        movem.l d2-d7/a0-a6,-(sp)
        bios
        movem.l (sp)+,d2-d7/a0-a6
        move.b d0,rega
        rts

badbios move.b d0,-(sp)         ; Flag illegal BIOS call
        move.l #biosmsg,d1      ; and spill guts.
        move.w #9,d0
        bdos
        move.b (sp)+,d1
        bsr pbyte
        move.l #biosmg2,d1
        move.w #9,d0
        bdos
        bsr dump

quitprg move.l (sp)+,d0         ; Trash return address and
        rts                     ; quit simulation.

        page
*************************************************************************
*                                                                       *
*       FCB Tracing support routines.                                   *
*                                                                       *
*************************************************************************

        ifne trcdsk
fcbtrc1 movem.l d0-d2/a0,-(sp)  ; Dump to printer each FCB usage
        move.b #9,d1            ; in format FN #, Disk, Name (ASCII)
        bsr lpchar              ; and the rest, all in hex but the
        move.w d0,d1            ; name field.  Print the returned
        bsr lpbyte              ; value after the FCB.
        move.b d2,d1            ; Char in D2 is printed after FN #.
        bsr lpchar
        bsr lpspace
        bsr lpspace
        move.b (a0)+,d1
        bsr lpbyte
        bsr lpspace
        move.w #10,d2
fcbtr1  move.b (a0)+,d1         ; Print Name field...
        bsr lpchar
        dbra d2,fcbtr1
        bsr lpspace
        move.w #3,d2
fcbtr2  move.b (a0)+,d1         ; Ex .. Rc
        bsr lpbyte
        bsr lpspace
        dbra d2,fcbtr2
        bsr lpspace
        bsr lpspace
        lea.l 16(a0),a0         ; Skip d0..dn field.
        move.w #3,d2
fcbtr3  move.b (a0)+,d1         ; CR .. R2
        bsr lpbyte
        bsr lpspace
        dbra d2,fcbtr3
        bsr lpspace
        bsr lpspace
        move.l dmaaddr,d1
        sub.l targbase,d1
        bsr lpword
        bsr lpspace
        movem.l (sp)+,d0-d2/a0
        rts

        page
fcbtrc2 movem.l d0-d1,-(sp)     ; Line termination of FCB trace.
        bsr lpspace
        bsr lpspace
        move.b d0,d1
        bsr lpbyte
fcbtr21 move.b #10,d1
        bsr lpchar
        move.b #13,d1
        bsr lpchar
        movem.l (sp)+,d0-d1
        rts

fcbtrc2a:
        movem.l d0-d1,-(sp)     ; Line termination if no result
        bra fcbtr21             ; is to be presented.
        endc

        page
*************************************************************************
*                                                                       *
*       Misc. service routines.                                         *
*       (Inelegant, but rarely used so they stand as is).               *
*                                                                       *
*************************************************************************


pbyte   move.l #$20018,d0       ; 2 nybbles, 24 bit shift first.
        bra pdigits
pword   move.l #$40010,d0       ; 4 nybbles, 16 bit shift first.
        bra pdigits
paddr   move.l #$60008,d0       ; 6 nybbles, 8 bit shift first.
        bra pdigits
plong   move.l #$80000,d0       ; 8 nybbles, no shift first.
pdigits rol.l d0,d1             ; Do shift.
        bra pdigent
pdiglop swap d0                 ; Save nybble count.
        rol.l #4,d1             ; Print variable in d1.
        bsr ntoa
pdigent swap d0                 ; Get nybble count.
        dbra d0,pdiglop
        rts

ntoa    movem.l d0-d1,-(sp)     ; Nybble in d1 to ASCII, then output.
        and #$f,d1
        cmp #$a,d1
        blt ntoa2
        add.b #'A'-'9'-1,d1
ntoa2   add.b #'0',d1
        move.w #2,d0
        bdos 
        movem.l (sp)+,d0-d1
        rts

pspace  move.w #32,d1           ; Print a space.
        move.w #2,d0
        bdos
        rts

        page
*
*       Line Printer versions of above
*

lpbyte  move.l #$20018,d0       ; 2 nybbles, 24 bit shift first.
        bra lpdigts
lpword  move.l #$40010,d0       ; 4 nybbles, 16 bit shift first.
        bra lpdigts
lpaddr  move.l #$60008,d0       ; 6 nybbles, 8 bit shift first.
        bra lpdigts
lplong  move.l #$80000,d0       ; 8 nybbles, no shift first.
lpdigts rol.l d0,d1             ; Do shift.
        bra lpdgent
lpdiglp swap d0                 ; Save nybble count.
        rol.l #4,d1             ; Print variable in d1.
        bsr lntoa
lpdgent swap d0                 ; Get nybble count.
        dbra d0,lpdiglp
        rts

lntoa   movem.l d0-d1,-(sp)     ; Nybble in d1 to ASCII, then output.
        and #$f,d1
        cmp #$a,d1
        blt lntoa2
        add.b #'A'-'9'-1,d1
lntoa2  add.b #'0',d1
lntoa3  move.w #5,d0
        bdos 
        movem.l (sp)+,d0-d1
        rts

lpchar  movem.l d0-d1,-(sp)     ; Print a character.
        bra lntoa3

lpspace movem.l d0-d1,-(sp)     ; Print space.
        move.w #32,d1
        bra lntoa3

        page
*
*       Remaining misc. service routines.
*

lpstr   movem.l d0-d1,-(sp)     ; Print a null-terminated string.
lpstr1  move.b (a0)+,d1
        beq lpstr2
        bsr lpchar
        bra lpstr1
lpstr2  movem.l (sp)+,d0-d1
        rts


konin   move.w #1,d0    ; Console input for 'atol'.
        bdos
        rts


atol    moveq #0,d1     ; ASCII to long, stops on invalid hex char.
        clr d2          ; Returns long in d1, terminator char in d0,
atol1   bsr konin       ; d2=1 if any chars entered before terminator.
        cmp.b #$40,d0
        blo atol2
        and #$5F,d0     ; Mask to upper case.
atol2   cmpi.b #'0',d0  ; Check range (0..9,A..F).
        blo atolend
        cmpi.b #'F',d0
        bhi atolend
        cmpi.b #'9',d0
        bls atol3
        cmpi.b #'A',d0
        bhs atol3
        bra atolend
atol3   moveq #1,d2     ; Valid characters entered, flag it.
        sub.b #'0',d0
        cmp.b #$9,d0
        bls atol4
        sub.b #'A'-'9'-1,d0
atol4   ext d0          ; To long.
        ext.l d0
        asl.l #4,d1     ; Tack it onto D1.
        add.l d0,d1
        bra atol1
atolend rts

        page
        data
*************************************************************************
*                                                                       *
*       Target processor's data registers.                              *
*       Fake FDOS.                                                      *
*                                                                       *
*************************************************************************

        even
regop3  equ -9          ; Operand 1 for DAA storage.
regb    equ -8          ; Offsets from register base pointer for
regc    equ -7          ; 8080's pseudo-registers.
regd    equ -6          ; A & F are in Data Registers.
rege    equ -5          ; Pseudo-PC is kept in an Address Register.
regh    equ -4
regl    equ -3
regop1  equ -2          ; Operand 1 for DAA storage.
regop2  equ -1          ;    "    2  "   "     "

fcbstor ds.b 36         ; Host works FCB's out of here.
fcbflag ds.b 1          ; Flag says we used the FCB buffer.

        even
tracesad ds.l 1         ; Trace start address.
traceead ds.l 1         ; Trace end address.
traceflg ds.w 1         ; Tracing enabled flag.

dmaaddr ds.l 1          ; DMA address storage.

        page
fdos    dc.b $76,0,$C9  ; Fake BDOS for target system.
*                       ; BIOS Jump Table
        dc.b $C3,$33,$FF        ; Wboot
        dc.b $C3,$36,$FF        ; Const
        dc.b $C3,$39,$FF        ; Conin
        dc.b $C3,$3C,$FF        ; Conout
        dc.b $C3,$3F,$FF        ; List
        dc.b $C3,$42,$FF        ; Punch
        dc.b $C3,$45,$FF        ; Reader
        dc.b $C3,$48,$FF        ; Home
        dc.b $C3,$4B,$FF        ; Seldsk
        dc.b $C3,$4E,$FF        ; Settrk
        dc.b $C3,$51,$FF        ; Setsec
        dc.b $C3,$54,$FF        ; Setdma
        dc.b $C3,$57,$FF        ; Read
        dc.b $C3,$5A,$FF        ; Write
        dc.b $C3,$5D,$FF        ; Listst
        dc.b $C3,$60,$FF        ; Sectran

        dc.b $76,1,$C9  ; Fake BIOS for target system
        dc.b $76,2,$C9          ; Const
        dc.b $76,3,$C9          ; Conin
        dc.b $76,4,$C9          ; Conout
        dc.b $76,5,$C9          ; List
        dc.b $76,6,$C9          ; Punch
        dc.b $76,7,$C9          ; Reader
        dc.b $76,8,$C9          ; Home *
        dc.b $76,9,$C9          ; Seldsk *
        dc.b $76,10,$C9         ; Settrk *
        dc.b $76,11,$C9         ; Setsec *
        dc.b $76,12,$C9         ; Setdma *
        dc.b $76,13,$C9         ; Read *
        dc.b $76,14,$C9         ; Write *
        dc.b $76,15,$C9         ; Listst
        dc.b $76,16,$C9         ; Sectran *

fdoslen equ *-fdos

        page
*************************************************************************
*                                                                       *
*       Messages.                                                       *
*                                                                       *
*************************************************************************

illgmsg dc.b $d,$a,'Illegal instruction $'
ilgmsg2 dc.b ' at $'
ilgmsg3 dc.b '.$'
dumpmsg dc.b $d,$a,'Register contents:$'
dmpmsg2 dc.b $d,$a
        dc.b '-AF- -BC- -DE- -HL- -SP- -S0- -S1- -S2- -S3- -PC- -op-',$d,$a,'$'
biosmsg dc.b $d,$a,'Illegal BIOS call $'
biosmg2 dc.b '.$'
tracemsg dc.b 13,10,'Start trace at >$'
tracemg2 dc.b 13,10,'End trace at >$'
opnmsg  dc.b 'Cannot open .COM file.$'
ilgbdos dc.b 'Unsupported BDOS call.$'
fcbmsg  dc.b 9,'Fn#  Dr    NAME     EX S1 S2 RC   CR R0 R1 R2   Addr  Rslt',10,13
        dc.b 9,'----------------------------------------------------------',10,13,0
        page
        bss
*************************************************************************
*                                                                       *
*       Target processor's address space.                               *
*                                                                       *
*************************************************************************

        even
registers ds.b 10               ; Actual storage for 8080's other registers.
target  ds.b $10000             ; 8080's universe.
junkbuf ds.b $80                ; For BDOS' OPEN faking (RREAD buffer).
        .end
                                                                 