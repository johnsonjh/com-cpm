vermaj   equ    $02     ;Major version number
vermin   equ    $01     ;Minor version number
revyear  equ    $88     ;Year last assembled
revmonth equ    $06     ;Month last assembled
revday   equ    $29     ;Day last assembled
*************************************************************************
*                                                                       *
*                                                                       *
*       8080/Z80 Simulator for MC68000                                  *
*                                                                       *
*       With CP/M 2.2 call support, and optional tracing                *
*                                                                       *
*                                                                       *
*       Converted to AmigaDOS September 1987 by Charlie Gibbs           *
*       (after painstakingly typing it all in from Dr. Dobbs            *
*       Journal, January through March 1986).  Improvements             *
*       described by Jim Cathey in his letter in the June 1986          *
*       DDJ have been included.  Repetitive code is generated           *
*       by macros whenever it would save my fingers.                    *
*                                                                       *
*                                                                       *
*       Version 2.1 6/29/88 by Willi Kusche                             *
*               Fixed a number of bugs                                  *
*                 in existing Z80 op code emulation.                    *
*               Added enough new Z80 op codes and BDOS calls            *
*                 to allow Turbo Pascal V3.01A to be run.               *
*                                                                       *
*       Version 1.2 1/21/85 JEC                                         *
*               Fixed Extent bug in OPEN logic.                         *
*               Sped up code, sample MAC from 2:13 to 1:40              *
*               Now runs at a 1.4 MHz equivalent based on MAC sample.   *
*                                                                       *
*       Version 1.1 8/29/84 JEC                                         *
*               Fixed BIOS call #6 bug.                                 *
*                                                                       *
*       Version 1.0 05/25/84 by Jim Cathey                              *
*                                                                       *
*       This program has been written for speed whenever possible,      *
*       as such tends to be large because of the separate subroutine    *
*       for each and every opcode of the target processor.              *
*                                                                       *
*       On an 8MHz 68000 (Compupro) system the simulation speed is      *
*       a little better than a 1MHz Z-80 when running MAC.  The time    *
*       for a sample assembly was 2:13 for the simulation vs. 0:35      *
*       on a 4MHz Z-80, both systems used identical hard disk systems.  *
*                                                                       *
*       It is not a complete simulation, as some flag handling          *
*       isn't quite right, but it is enough to run the program          *
*       I wrote for it (DDT, LU, MAC, and Morrow's FORMATW).            *
*                                                                       *
*************************************************************************
        code
        page
*************************************************************************
*                                                                       *
*       This file contains the startup routines, the simulator core,    *
*       tracing code, and the CP/M 2.2 simulation.                      *
*                                                                       *
*************************************************************************

        xref    optabl,flags,mloop,traceit,tracesad,traceead,traceflg
        xdef    illegl,service,dump

* Changing this 'equ' to 0 will cause assembly errors
h19     equ     1               ;Non-zero for H19 emulation

*
* ASCII character values
*
bel     equ     $07             ;Bell (or beep or whatever)
bs      equ     $08             ;Backspace
ht      equ     $09             ;Horizontal tab
lf      equ     $0A             ;Line feed
ff      equ     $0C             ;Form feed
cr      equ     $0D             ;Carriage return
so      equ     $0E             ;Shift out
si      equ     $0F             ;Shift in
esc     equ     $1B             ;Escape

        INCLUDE   "ecpmdefs.i"
        page
*--------------------------------
*
* Some commonly used macros
*
*--------------------------------

sys     MACRO                   ;Call a system routine.
        jsr     _LVO\1(a6)
        ENDM

*----------------------------
* Target system Mnemonics
*----------------------------
tHLT    EQU    $76
tJMP    EQU    $C3
tRET    EQU    $C9

*----------------------
* Equates
*----------------------

Absbase              EQU    4
MODE_OLDFILE         EQU    1005
MODE_NEWFILE         EQU    1006
SHARED_LOCK          EQU    -2
OFFSET_CURRENT       EQU    0
;
;  File Information Block Sructure
;
fib_DiskKey          EQU    0
fib_DirEntryType     EQU    4
fib_FileName         EQU    8
fib_Protection       EQU    116
fib_EntryType        EQU    120
fib_Size             EQU    124
fib_NumBlocks        EQU    128
fib_DateStamp        EQU    132
fib_Comment          EQU    144
fib_SIZEOF           EQU    260

*----------------------
* External references
*----------------------

        XREF    _LVOOpenLibrary
        XREF    _LVOClose
        XREF    _LVOCloseLibrary
        XREF    _LVOCurrentDir
        XREF    _LVODeleteFile
        XREF    _LVODupLock
        XREF    _LVOExamine
        XREF    _LVOExNext
        XREF    _LVOInput
        XREF    _LVOIoErr
        XREF    _LVOLock
        XREF    _LVOOpen
        XREF    _LVOOutput
        XREF    _LVORead
        XREF    _LVORename
        XREF    _LVOSeek
        XREF    _LVOUnLock
        XREF    _LVOWaitForChar
        XREF    _LVOWrite
        page
*************************************************************************
*                                                                       *
*       Initialization                                                  *
*                                                                       *
*************************************************************************

start:  move.l  sp,savesp       ;Save the stack pointer.
        move.b  #1,testdol      ;"pstring" should test for leading $.
        clr.w   esclen          ;No partial escape sequence is saved.
        clr.b   insflag         ;We're not in insert mode.
        clr.b   btrcflg         ;Turn off BIOS/BDOS call tracing.
        clr.b   quitflg         ;Clear the quit flag.
        clr.b   bufflag         ;Disable output buffering.
        clr.b   listopn         ;The list device is closed.
        move.l  #strbuf,strptr  ;Initialize output buffer pointer.

        lea     handles,a1
        moveq   #(handlen-handles)/4-1,d1
clrhand clr.l   (a1)+           ;Clear file handles.
        dbra    d1,clrhand
        clr.l   rawhand         ;Clear RAW: handle too.

*
* Copy the command line to "cmdline", stripping out leading switches if any.
*
        lea     cmdline,a1
        subq    #1,d0
* Skip over leading blanks, if any.
leadblk cmpi.b  #' ',(a0)+      ;Leading blank?
        bne.s   bufparm         ;No.
        dbra    d0,leadblk      ;Skip over leading blank.
*  If the first parameter is "-b", skip over it but activate output buffering.
bufparm subq.l  #1,a0           ;Back onto the first non-blank.
        cmpi.b  #'-',(a0)       ;Possible buffer switch?
        bne.s   savecmd         ;No - start saving the command line.
        cmpi.b  #'B',1(a0)      ;Activate output buffering?
        beq.s   setbuff         ;Yes.
        cmpi.b  #'b',1(a0)
        bne.s   skipsw          ;No.
setbuff move.b  #1,bufflag      ;Set buffered-output flag.
skipsw  cmpi.b  #' ',(a0)+      ;Skip over the switch.
        beq.s   skipswx         ; and look for start of command line.
        dbra    d0,skipsw
        addq.l  #1,a1           ;Adjust A1.
        bra.s   gotcmd          ;There is no command line left.
skipswx subq.l  #1,a0           ;Back onto the first blank.
        bra.s   leadblk         ;Look for the next non-blank.
* Save the command line (except for leading switches).
savecmd move.b  (a0)+,(a1)+     ;Save the command line, if any.
        dbra    d0,savecmd
gotcmd  move.b  #0,-1(a1)       ;Replace the newline with a null.
        move.b  cmdline,cmdflag ;Save command-line flag.

*
* Open libraries and set up a RAW: window.
*
        move.b  #1,quitflg      ;Quit immediately if failure below.
        move.l  Absbase,a6      ;Find library
        lea     dosname,a1      ;'dos.library'
        moveq   #0,d0           ;Any version
        sys     OpenLibrary     ;Open dos.library.
        move.l  d0,a6           ;Point to doslib for next operation.
        move.l  d0,dosbase      ;Save it for future reference.
        sys     Input           ;Get file handle for keyboard.
        move.l  d0,stdin        ;Save it here.
        beq     quitprg         ;Couldn't get keyboard handle.
        sys     Output          ;Get file handle for screen.
        move.l  d0,stdout       ;Save it here.
        beq     quitprg         ;Couldn't get screen handle.
        move.l  #rawspec,d1
        move.l  #MODE_NEWFILE,d2
        sys     Open            ;Open a RAW: window.
        move.l  d0,rawhand      ;Save the file handle here.
        bne.s   opened          ;We succeeded.
        move.l  #rawerr,d1
        bsr     pstring         ;Display error message...
        sys     IoErr
        move.l  d0,d1
        bsr     plong           ; and error code.
        bsr     pcrlf
        bra     quitprg
opened  move.b  cmdflag,quitflg ;If we have a command, execute it and quit.
        move.l  #setwin,d1
        bsr     pstring         ;Set the window to 24 by 80.

*
* Come back here to load another program.
*
nextprg lea     target,targbase ;Start of target memory
        clr.b   insflag         ;Reset insert mode.
        move.l  #simsg,d1
        bsr     pstring         ;In case last program sent SHIFT OUT
        clr.b   dumpcnt         ;Reset dump pause counter.
        bsr     entrads         ;Enter trace delimiting addresses.
        bsr     lodfdos         ;Load up the fake FDOS in target memory.
        bsr     lodregs         ;Load the remaining simulation registers.
        bsr     loadcom         ;Load the .COM program.

        jmp     mloop           ;Execute simulation.
        page
*************************************************************************
*                                                                       *
*       Illegal instructions and dumping                                *
*                                                                       *
*************************************************************************

illegl  move.l  #illgmsg,d1     ;Illegal opcode, say what & where,
        bsr     pstring
        subq.l  #1,pseudopc
        move.b  (pseudopc),d1
        lsl.w   #8,d1
        move.b  1(pseudopc),d1
        bsr     pword
        move.l  #ilgmsg2,d1
        bsr     pstring
        move.l  pseudopc,d1
        sub.l   targbase,d1
        bsr     pword
        move.l  #ilgmsg3,d1
        bsr     pstring
        move.l  #dumpmsg,d1
        bsr     pstring
        clr.b   dumpcnt
        bsr     dump            ; and spill guts.
        bra     quitprg         ;Quit simulation.

dump    movem.l d0-d3,-(sp)
        bsr     pcrlf
        move.b  regf,d0
        bsr     setflags
        move.b  #'A',(a0)+
        move.b  #'=',(a0)+
        move.b  rega,d1         ;Accumulator
        bsr     ubyte
        move.b  #' ',(a0)+
        move.b  #'B',(a0)+
        move.b  #'=',(a0)+
        move.w  regb(regs),d1   ;BC
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'D',(a0)+
        move.b  #'=',(a0)+
        move.w  regd(regs),d1   ;DE
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'H',(a0)+
        move.b  #'=',(a0)+
        move.w  regh(regs),d1   ;HL
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'S',(a0)+
        move.b  #'=',(a0)+
        move.l  pseudosp,d1     ;SP
        sub.l   targbase,d1
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'P',(a0)+
        move.b  #'=',(a0)+
        move.l  pseudopc,d1     ;PC
        sub.l   targbase,d1
        bsr     uword
        move.b  #' ',(a0)+
        move.l  a1,-(sp)
        move.l  pseudosp,a1
        move.w  #3,d2
        move.b  #'0',d3
tosloop move.b  #'S',(a0)+      ;Display the top 4 stack entries.
        move.b  d3,(a0)+
        addq.b  #1,d3
        move.b  #'=',(a0)+
        move.b  1(a1),d1 
        ror.w   #8,d1
        move.b  0(a1),d1
        bsr     uword
        move.b  #' ',(a0)+
        addq.l  #2,a1
        dbra    d2,tosloop
        move.b  #'$',(a0)+
        move.l  (sp)+,a1
        move.l  #workbuf,d1
        bsr     pstring         ;Displaying as a single string is much faster.
        bsr     pcrlf
        move.b  regaaf+1(regs),d0
        bsr     setflags
        move.b  #'A',(a0)+
        move.b  #$27,(a0)+
        move.b  regaaf(regs),d1 ;Alternate accumulator
        bsr     ubyte
        move.b  #' ',(a0)+
        move.b  #'B',(a0)+
        move.b  #$27,(a0)+
        move.w  regabc(regs),d1 ;Alternate BC
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'D',(a0)+
        move.b  #$27,(a0)+
        move.w  regade(regs),d1 ;Alternate DE
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'H',(a0)+
        move.b  #$27,(a0)+
        move.w  regahl(regs),d1 ;Alternate HL
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'X',(a0)+
        move.b  #'=',(a0)+
        move.w  regix(regs),d1  ;IX
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #'Y',(a0)+
        move.b  #'=',(a0)+
        move.w  regiy(regs),d1  ;IY
        bsr     uword
        move.b  #' ',(a0)+
        move.b  #' ',(a0)+
        move.b  (pseudopc),d1   ;Current opcode byte
        bsr     ubyte
        move.b  #' ',(a0)+
        move.b  #' ',(a0)+
        move.b  #'$',(a0)+
        move.l  #workbuf,d1
        bsr     pstring         ;Displaying as a single string is much faster.
        moveq   #0,d0
        move.b  (pseudopc),d0   ;Opcode
        mulu    #9,d0           ;Offset into opcode table
        lea     mnops,a0
        move.l  a0,d1
        add.l   d0,d1           ;D1 -> opcode name
        move.l  d1,-(sp)
        addq.l  #1,d1
        bsr     pstring         ;Display opcode name.
        move.l  (sp)+,a0
        cmpi.b  #' ',(a0)
        beq.s   nooprnd         ;There are no operands.
        cmpi.b  #'C',(a0)
        bne.s   notcons
        move.b  1(pseudopc),d1  ;Display single-byte operand.
        bsr     pbyte
        bra.s   nooprnd
notcons cmpi.b  #'A',(a0)
        bne.s   nooprnd
        move.b  2(pseudopc),d1  ;Display two-byte operand.
        bsr     pbyte
        move.b  1(pseudopc),d1
        bsr     pbyte
nooprnd bsr     pcrlf
        addq.b  #1,dumpcnt      ;Count the number of times dumped.
        cmpi.b  #8,dumpcnt      ;Is the screen full of dumps?
        bmi     dumpx           ;No - exit.
        move.l  #dmpmsg3,d1
        bsr     pstring         ;Ask for operator action.
        bsr     dmpstr          ;Make sure the prompt gets out!
        movem.l d3/a0-a1/a6,-(sp)
        move.l  rawhand,d1      ;Console input
        move.l  #dumpcnt,d2
        move.l  dosbase,a6
        moveq   #1,d3
        sys     Read            ;Get the operator's reply.
        movem.l (sp)+,d3/a0-a1/a6
        bsr     pcrlf
        cmpi.b  #'Q',dumpcnt    ;Does he want to quit?
        beq     quitprg         ;Yes.
        cmpi.b  #'q',dumpcnt
        beq     quitprg
        cmpi.b  #'S',dumpcnt    ;Stop tracing?
        beq.s   dumpnt          ;Yes.
        cmpi.b  #'s',dumpcnt
        beq.s   dumpnt
        cmpi.b  #'C',dumpcnt    ;Change trace address?
        beq.s   gettr           ;Yes.
        cmpi.b  #'c',dumpcnt
        bne.s   dmpcont
gettr   bsr     gtrange
dumpnt  clr.b   traceflg        ;Disable tracing and continue.
dmpcont clr.b   dumpcnt         ;Reset the counter.
dumpx   movem.l (sp)+,d0-d3
        rts

setflags:
        lea     workbuf,a0
        move.b  #'-',d1
        btst    #0,d0
        beq     1$
        move.b  #'C',d1
1$      move.b  d1,(a0)+
        move.b  #'-',d1
        btst    #6,d0
        beq     2$
        move.b  #'Z',d1
2$      move.b  d1,(a0)+
        move.b  #'-',d1
        btst    #7,d0
        beq     3$
        move.b  #'M',d1
3$      move.b  d1,(a0)+
        move.b  #'-',d1
        btst    #2,d0
        beq     4$
        move.b  #'E',d1
4$      move.b  d1,(a0)+
        move.b  #'-',d1
        btst    #4,d0
        beq     5$
        move.b  #'I',d1
5$      move.b  d1,(a0)+
        move.b  #' ',(a0)+
        rts
        page
*************************************************************************
*                                                                       *
*       Initialization subroutines                                      *
*                                                                       *
*************************************************************************

*
* Load up the fake FDOS.
*
lodfdos move.l  a6,-(sp)
        lea     fdos,a6
        move.l  targbase,pseudosp
        adda.l  #$10000,pseudosp
        lea     -256(pseudosp),a0
        move.w  #fdoslen,d0
lodloop move.b  (a6)+,(a0)+
        dbra    d0,lodloop
        lea     -256(pseudosp),a0
        move.l  a0,d0
        sub.l   targbase,d0
        move.b  #tJMP,0(targbase)        ;Build BIOS and BDOS jumps.
        move.b  #tJMP,5(targbase)
        move.b  d0,6(targbase)
        rol.w   #8,d0
        move.b  d0,7(targbase)
        rol.w   #8,d0
        addq.w  #3,d0
        move.b  d0,1(targbase)
        rol.w   #8,d0
        move.b  d0,2(targbase)
        clr.w   -(pseudosp)     ;Set up a return stack to exit simulation.
        move.l  (sp)+,a6
        rts

*
* Set up working registers.
*
lodregs lea     optabl,opptr    ;Point base reg. to opcode dispatch table.
        lea     mloop,return
        lea     flags,flagptr
        move.l  targbase,pseudopc
        adda.l  #$100,pseudopc  ;Start execution at 0100H in target space.
        moveq   #$E,regcon0e    ;Set up quick constants.
        moveq   #$1,regcon01
        moveq   #$F,regcon0f
        move.l  #$FF,regconff
        moveq   #0,rega
        moveq   #0,regf
        rts
        page
*
* Get start and end addresses for tracing.
*
entrads tst.b   traceit         ;Is tracing required?
        beq     entradx         ;No.
        bsr     gtrange         ;get trace range
* Find out whether BIOS/BDOS calls are to be traced.
        move.l  #btrcmsg,d1
        bsr     pstring
        lea     workbuf,a0
        move.b  #10,(a0)
        bsr     getline
        move.b  #1,btrcflg
        cmpi.b  #'Y',workbuf+2
        beq.s   entradx
        cmpi.b  #'y',workbuf+2
        beq.s   entradx
        clr.b   btrcflg
entradx clr.b   traceflg        ;Start with tracing turned off.
        rts

gtrange:
        move.l  #tracemsg,d1    ;Enter trace address if necessary.
        bsr     pstring
        lea     workbuf,a0
        move.b  #workbufn-workbuf-2,(a0)
        bsr     getline         ;Get the string.
        moveq   #0,d0
        move.b  1(a0),d0        ;Number of bytes read
        addq.l  #2,a0           ;Skip over length information.
        clr.b   0(a0,d0)        ;Insert string terminator.
        bsr     atol            ;Get trace start address.
        and.l   #$FFFF,d1
        add.l   targbase,d1
        move.l  d1,tracesad
* Now get the ending address.
        move.l  #tracemg2,d1
        bsr     pstring
        lea     workbuf,a0
        move.b  #workbufn-workbuf-2,(a0)
        bsr     getline
        moveq   #0,d0
        move.b  1(a0),d0
        addq.l  #2,a0
        clr.b   0(a0,d0)
        bsr     atol
        and.l   #$FFFF,d1
        add.l   targbase,d1
        move.l  d1,traceead
        bsr     pcrlf
        rts

*
* Open the file to be loaded, and load it into
*  target space if successful.
*
loadcom movem.l d1-d3/a1-a2/a6,-(sp)    ;Save registers.
        lea     cmdline,a0
        tst.b   cmdflag         ;Do we have a command already?
        bne.s   scancmd         ;Yes - process it.
prompt  move.l  #aprompt,d1
        bsr     pstring         ;Display the command prompt.
        lea     cmdline,a0
        move.b  #cmdlinen-cmdline-2,(a0)
        bsr     getline         ;Get a command line.
        moveq   #0,d0
        move.b  1(a0),d0        ;Length of command line
        beq.s   prompt          ;Zero - try again.
        addq.l  #2,a0           ;Skip over length information.
        clr.b   0(a0,d0)        ;Insert command line terminator.
        cmpi.b  #3,(a0)         ;Control-C?
        bne.s   scancmd         ;No.
        move.b  #1,quitflg      ;Set quit flag.
        bra     quitprg         ;Exit the simulator.
scancmd lea     comname,a2
        moveq   #comnamen-comname-6,d1 ;Adjust length for DBcc.
loadnam move.b  (a0)+,d0        ;Convert file name to upper case.
        bsr     ucase
        move.b  d0,(a2)+
        cmpi.b  #' ',(a0)       ;End of name?
        beq     gotname         ;Yes.
        tst.b   (a0)            ;End of command string?
        dbeq    d1,loadnam      ;No - keep on going.
gotname move.l  a0,comend       ;Save position in command line.
        move.b  #'.',(a2)+      ;Mash file name to .COM.
        move.b  #'C',(a2)+
        move.b  #'O',(a2)+
        move.b  #'M',(a2)+
        clr.b   (a2)
        clr.b   cmdline         ;Ask for a new command next time.
        move.l  #comname,d1
        move.l  #MODE_OLDFILE,d2
        move.l  dosbase,a6
        sys     Open            ;Open the file.
        tst.l   d0              ;Did the open fail?
        bne.s   comopen         ;No.
        lea     comname,a0
openerr cmpi.b  #'.',(a0)+      ;Find end of file name.
        bne.s   openerr
        move.b  #'?',-1(a0)
        move.b  #cr,(a0)+
        move.b  #lf,(a0)+
        move.b  #'$',(a0)
        move.l  #comname,d1
        bsr     pstring         ;Echo "name?"
        bra     prompt          ;Try again.

comopen move.l  d0,-(sp)        ;Save the file handle.
        move.l  d0,d1
        move.l  pseudopc,d2     ;Start loading at $0100 in target.
        move.l  #65536-512,d3   ;Maximum number of bytes to load
        move.l  dosbase,a6
        sys     Read            ;Load the .COM file.
        move.l  (sp)+,d1
        move.l  dosbase,a6
        sys     Close           ;Close the .COM file.

* The program has now been loaded.
        movem.l (sp)+,d1-d3/a1-a2/a6    ;Refresh registers.
        movem.l d1-d3/a1-a2/a6,-(sp)
        lea     $80(targbase),a0        ;Set up target's base page.
        move.l  a0,dmaaddr

* Set up FCBs and command line tail.
        lea     $5C(targbase),a0
        lea     $6C(targbase),a2
        clr.b   (a0)+
        clr.b   (a2)+
        moveq   #10,d0
clrfcb  move.b  #' ',(a0)+      ;Clear FCBs.
        move.b  #' ',(a2)+
        dbra    d0,clrfcb
        clr.b   $80(targbase)   ;Clear the command line tail.

        move.l  comend,a0       ;Restore position in command line.
fcb1    tst.b   (a0)            ;End of command line?
        beq     loaded          ;Yes.
        cmpi.b  #' ',(a0)+      ;Skip over to first file name
        beq.s   fcb1
        subq.l  #1,a0           ;Back onto start of file name.
        move.l  a0,-(sp)        ;Save position on command line.
        lea     $81(targbase),a2;A2 loads the command line tail.
gettail move.b  (a0)+,d0        ;Copy the command tail.
        bsr     ucase
        move.b  d0,(a2)+
        tst.b   d0
        bne.s   gettail
        move.l  a2,d0
        lea     $82(targbase),a0;Don't count null terminator!
        sub.l   a0,d0
        move.b  d0,$80(targbase);Length of command line tail
        move.l  (sp)+,a0        ;Go back to the first file name.

        lea     $5C(targbase),a2;Address of current FCB
getfcb  move.l  a2,fcbptr       ;Save pointer to current FCB.
        cmpi.b  #':',1(a0)      ;Is a drive specified?
        bne.s   getfcbf         ;No.
        move.b  (a0),d0         ;Get drive letter.
        bsr     ucase
        move.b  d0,(a2)
        subi.b  #'A'-1,(a2)     ;Convert to drive code.
        addq.l  #2,a0           ;Skip over drive designator.
        tst.b   (a0)            ;End of command line?
        beq.s   loaded          ;Yes - we're done.
        cmpi.b  #' ',(a0)       ;End of file name?
        beq.s   getfcbx         ;Yes.
getfcbf addq.l  #1,a2           ;Start of file name in FCB
getfcbl move.b  (a0)+,d0        ;Copy file name to FCB.
        bsr     ucase
        move.b  d0,(a2)+
getfcbt tst.b   (a0)            ;End of command?
        beq.s   loaded          ;Yes.
        cmpi.b  #' ',(a0)       ;End of file name?
        beq.s   getfcbx         ;Yes.
        cmpi.b  #'.',(a0)       ;Start of file name extension?
        bne.s   getfcbl         ;No - continue loading file name.
        move.l  fcbptr,a2       ;Copy original pointer
        adda.l  #9,a2           ;Skip over to extension field.
        addq.l  #1,a0           ;Skip over the period.
        bra.s   getfcbt
getfcbx tst.b   (a0)            ;End of command line?
        beq.s   loaded          ;Yes.
        cmpi.b  #' ',(a0)+      ;Look for another file name.
        beq.s   getfcbx
        subq.l  #1,a0           ;Back onto start of file name.
        move.l  fcbptr,d0
        lea     $5C(targbase),a2
        cmp.l   d0,a2           ;Was this the first FCB?
        bne.s   loaded          ;No - stop after two FCBs.
        adda.l  #16,a2          ;Skip over to the next FCB.
        bra.s   getfcb          ;Load the next FCB.

loaded  movem.l (sp)+,d1-d3/a1-a2/a6    ;Restore registers.
        rts                     ;Exit.

*
* Subroutine to convert a character to upper case
*
ucase   cmpi.b  #'a',d0
        blt.s   ucasex
        cmpi.b  #'z',d0
        bgt.s   ucasex
        subi.b  #'a'-'A',d0
ucasex  rts
        page
*************************************************************************
*                                                                       *
*       BDOS / BIOS service routines                                    *
*                                                                       *
*************************************************************************
service movem.l a1/a6,-(sp)
        move.b  rega,newrega    ;Save 8080 accumulator (D2)
        move.l  dosbase,a6      ;Get dos.library pointer
* Decode the byte following the HLT instruction (BIOS call type).
        moveq   #0,d0           ;Handle BIOS/BDOS service request
        move.b  (pseudopc)+,d0  ; of form HLT DB opcode.
        cmp     #(biostabn-biostab)/4,d0
        blt.s   dobios          ;Function number is within range.
badbios move.b  d0,-(sp)        ;Flag illegal BIOS call
        move.l  #ilgbios,d1     ; and spill guts.
        bsr     pstring
        move.b  (sp)+,d1
        bsr     pbyte
        bsr     pcrlf
        bsr     dump
        bra     quitprg
dobios  move.l  d0,-(sp)        ;Save BIOS function number.
        beq.s   biostrx         ;Zero - it's a BDOS call.
        tst.b   btrcflg         ;Trace BIOS calls?
        beq.s   biostrx         ;No.
        move.l  #biosmsg,d1
        bsr     pstring
        move.l  (sp),d1
        bsr     pbyte
        move.l  #atmsg,d1
        bsr     pstring
        move.b  1(pseudosp),d1  ;Address where called (top stack entry)
        ror.w   #8,d1
        move.b  0(pseudosp),d1
        bsr     pword
        bsr     pcrlf
        move.l  (sp),d0
biostrx asl     #2,d0           ;Multiply function number by 4.
        addi.l  #biostab,d0     ;Point at address table entry.
        movea.l d0,a0
        movea.l (a0),a0         ;Point to appropriate service routine.
        move.l  (sp)+,d0        ;Restore BIOS function number.
        jmp     (a0)            ;Jump to the routine.
* If the BIOS code is zero, it's a BDOS call.
*  Decode register C using a similar routine to the BIOS decoding above.
bdosfn  moveq   #0,d0
        move.b  regc(regs),d0   ;Get BDOS function number.
        cmp     #(bdostabn-bdostab)/4,d0
        blt.s   dobdos          ;Function number is within range.
badbdos move.b  d0,-(sp)
        move.l  #ilgbdos,d1     ;Illegal or unsupported BDOS call
        bsr     pstring
        move.b  (sp)+,d1
        bsr     pbyte
        bsr     pcrlf
        bsr     dump
        bra     quitprg
dobdos  move.l  d0,-(sp)        ;Save BDOS function number.
        tst.b   btrcflg         ;Trace BDOS calls?
        beq.s   bdostrx         ;No.
        move.l  #bdosmsg,d1
        bsr     pstring
        move.l  (sp),d1
        bsr     pbyte
        move.l  #atmsg,d1
        bsr     pstring
        move.b  1(pseudosp),d1
        ror.w   #8,d1
        move.b  0(pseudosp),d1
        bsr     pword
        bsr     pcrlf
        move.l  (sp),d0
bdostrx cmpi.b  #10,d0          ;BDOS function 10 or higher?
        blt.s   bdosjmp         ;No.
        bsr     dmpstr          ;Dump any outstanding console output.
        move.l  (sp),d0         ;Restore BDOS function number.
bdosjmp asl     #2,d0           ;Multiply function number by 4.
        addi.l  #bdostab,d0     ;Point at address table entry.
        movea.l d0,a0
        movea.l (a0),a0         ;Point to appropriate service routine.
        move.l  (sp)+,d0        ;Restore BDOS function number.
        moveq   #0,d1
        move.w  regd(regs),d1   ;Get argument.
        jmp     (a0)            ;Jump to the routine.
* Return here after performing the BDOS function.
results movem.l (sp)+,a1/a6
        moveq   #0,rega
        move.b  newrega,rega    ;Get new accumulator value.
* We have finished processing the BDOS function.
        move.b  rega,d0         ;Set flags.
        and.w   regconff,d0
        move.b  0(flagptr,d0.w),regf
        rts
*
* Individual BDOS service routines
*
bdos00  bra     quitprg          ;Exit program.

bdos01  bsr     dmpstr           ;Console input
        move.l  rawhand,d1
        move.l  #newrega,d2
        moveq   #1,d3
        sys     Read
        bra     results

bdos02  move.b  rege(regs),d1    ;Console output
        clr.b   testdol          ;Allow dollar signs
        bsr     pchar
        bra     results

bdos03  equ     badbdos          ;Reader input

bdos04  equ     badbdos          ;Punch output

bdos05  pea     rege(regs)       ;List output byte
bdos05t tst.b   listopn          ;Is the printer already open?
        bne.s   bdos05w          ;Yes.
        move.l  #prtname,d1
        move.l  #MODE_NEWFILE,d2
        sys     Open             ;Open the printer.
        move.l  d0,prthand       ;Save the file handle.
        bne.s   bdos05o          ;The open was successful.
        move.l  #badprt,d1
        bsr     pstring          ;Indicate an unsuccessful open.
        bsr     dump             ;Spill guts...
        bra     quitprg          ; and exit.
bdos05o move.b  #1,listopn       ;Indicate that the list device is open.
bdos05w move.l  prthand,d1
        move.l  (sp)+,d2         ;Character to send to the list device
        moveq   #1,d3            ;Just send one byte.
        sys     Write            ;Send the byte to the list device.
        bra     results
        
bdos06  cmpi.b  #$FF,rege(regs)  ;Direct console I/O
        bne.s   bdos02           ;Send the byte.
        bsr     dmpstr           ;Dump any outstanding output.
        move.l  rawhand,d1
        moveq   #1,d2            ;Wait for one microsecond.
        sys     WaitForChar      ;Check whether a character is ready.
        tst.l   d0               ;Is a character ready?
        bne     bdos01           ;Yes - get it.
        clr.b   newrega          ;Indicate that nothing is ready.
        bra     results

bdos07  move.b  3(targbase),newrega     ;Get IOBYTE
        bra     results

bdos08  move.b  rege(regs),3(targbase)  ;Set IOBYTE
        bra     results
                             
bdos09  add.l   targbase,d1      ;Console output string
        bsr     pstring
        bra     results

bdos10  add.l   targbase,d1      ;Console input line
        movea.l d1,a0            ;The buffer is here.
        bsr     getline          ;Get a line.
        cmpi.b  #3,2(a0)         ;Was it a control-C?
        bne     results          ;No - continue processing.
        bra     quitprg          ;Terminate the program.

bdos11  move.l  rawhand,d1       ;Console status check
        moveq   #1,d2            ;Wait for one microsecond.
        sys     WaitForChar      ;Check whether a character is ready.
        move.b  d0,newrega       ;Result is compatible with CP/M.
        bra     results

bdos12  clr.b   regh(regs)       ;Get system identification
        move.b  #$22,regl(regs)  ;Pretend we're CP/M 2.2.
        move.b  #$22,newrega     ;Some programs use undocumented return reg
        bra     results

bdos13  equ     results          ;Reset all drives (ignored)

bdos14  move.b  rege(regs),4(targbase)  ;Select drive
        bra     results

bdos15  move.l  #MODE_OLDFILE,d2 ;Open existing file
bdos15o add.l   targbase,d1
        movea.l d1,a0            ;The FCB is here.
        move.l  d1,-(sp)
        lea     opnname,a1       ;Build AmigaDOS file name here.
        move.l  a1,d1            ;We'll need it here.
        bsr     convfn           ;Make a file name.
        sys     Open             ;Open the file.
        move.l  (sp)+,a1         ;The FCB is here.
        lea     handles,a0
        moveq   #(handlen-handles)/4-1,d1
        clr.b   newrega          ;Assume the open succeeded.
        tst.l   d0               ;Did it fail?
        bne     bdos15s          ;No.
        move.b  #$FF,newrega     ;Set failure code.
        bra     results
bdos15s tst.l   (a0)+            ;Find an available handle slot.
        dbeq    d1,bdos15s
        tst     d1               ;Did we find a slot?
        bmi     bdos15e          ;No - error!
        move.l  d0,-4(a0)        ;Save file handle address.
        moveq   #(handlen-handles)/4-1,d0
        sub.l   d1,d0
        move.b  d0,13(a1)        ;Save handle number in FCB.
        bra     results
bdos15e move.l  #fullmsg,d1      ;File handle table overflow!
        bsr     pstring          ;Display an error message
        bra     quitprg          ; and forget the whole thing.

bdos16  move.b  #$FF,newrega     ;Close file
        bsr     gethand          ;Get the file handle.
        beq     results          ;The file is not open.
        clr.l   0(a1,d0.w)       ;Clear the handle table entry.
        sys     Close            ;Close the file.
        clr.b   newrega          ;Indicate success.
        bra     results
        page
bdos17  movem.l a1-a2/a6/d2,-(sp)   ;Search for first file
        add.l   targbase,d1
        lea     scanname,a1
        bsr     setname
        clr.l   d1
        sys     CurrentDir
* looks as this next line can be deleted
        move.l  d0,CurDir
        move.l  d0,d1
        sys     DupLock
        move.l  d0,lockaddr
        bsr     ffirst
        beq     badbdos
        bra     NameLoop

bdos18  movem.l a1-a2/a6/d2,-(sp)   ;Search for next file
;
;       Find next entry in directory
;
NameLoop:
        move.l  lockaddr,d1
        move.l  #FileInfo,d2
        movea.l dosbase,a6
        sys     ExNext
;
;       Branch if no more entries
;
        tst.l   d0
        beq     EndDir
;
;       Bypass sub-directory entry
;
        tst.l   FileInfo+fib_DirEntryType
        bpl     NameLoop
;
;       Clear destination area
;
        movea.l #DirFName,a0
        movea.l #DirFExt,a1
        move.w  #7,d0
        move.b  #' ',d1
1$      move.b  d1,(a0)+
        move.b  d1,(a1)+
        dbra    d0,1$
;
;       Move file name while collecting data
;
        movea.l #FileInfo+fib_FileName,a0
        movea.l #DirFName,a2
        clr.l   d0           ;char count
        clr.l   d1           ;period count
2$      move.b  (a0)+,d2
        beq     4$
        cmpi.b  #'.',d2
        beq     3$
        move.b  d2,(a2)+
        addq.w  #1,d0
        bra     2$
3$      movea.l #DirFExt,a2
        movea.l a0,a1
        addq.w  #1,d1
        bra     2$
;
;       Bypass entry if name not in CP/M format
;
4$      cmpi.w  #1,d1
        bgt     NameLoop
        beq     5$
        moveq   #8,d2
        bra     6$
5$      cmpa.l  #FileInfo+fib_FileName+9,a1
        bgt     NameLoop
        moveq   #11,d2
6$      cmp.w   d2,d0
        bgt     NameLoop
;
;       Bypass entry if file names do not match
;
        movea.l fcbptr,a1
        addq    #1,a1
        movea.l #DirFName,a0
        moveq   #7,d2
        bsr     matchpart
        bne     NameLoop
;
;       Bypass entry if file name extensions do not match
;
        movea.l fcbptr,a1
        adda.l  #9,a1
        movea.l #DirFExt,a0
        moveq   #2,d2
        bsr     matchpart
        bne     NameLoop
;
;       Move file name
;
        movea.l #DirFName,a0
        move.l  dmaaddr,a1
        addq.l  #1,a1
        moveq   #7,d0
7$      move.b  (a0)+,(a1)+
        dbra    d0,7$
;
;       Move file name extension
;
        movea.l #DirFExt,a0
        move.l  dmaaddr,a1
        adda.l  #9,a1
        moveq   #7,d0
8$      move.b  (a0)+,(a1)+
        dbra    d0,8$
;
;
;
        clr.b   newrega
        movem.l (sp)+,a1-a2/a6/d2
        bra     results
;
;
;
EndDir:
        bsr     unlock
        move.b  regconff,newrega
        movem.l (sp)+,a1-a2/a6/d2
        bra     results
;
;       Subroutine to match part of file name
;
matchpart:
        move.b  (a0),d0
        bsr     ucase
        move.b  (a1),d1
        cmp.b   d0,d1
        beq     CharMatch
        cmpi.b  #'?',d1
        beq     CharMatch
        cmpi.b  #'*',d1
        bne     nomatch
        beq     match
CharMatch:
        addq.l  #1,a1
        addq.l  #1,a0
        dbra    d2,matchpart
match:
        clr.l   d0   ;insure zero flag is set
        rts
nomatch:
        moveq   #1,d0  ;insure zero flag reset
        rts
        page
bdos19  add.l   targbase,d1      ;Delete file
        movea.l d1,a0            ;The FCB is here.
        lea     opnname,a1       ;Build AmigaDOS file name here.
        move.l  a1,d1            ;We'll need it here.
        bsr     convfn           ;Make a file name.
        sys     DeleteFile       ;Delete the file.
        bra     results          

bdos20  clr.b   newrega          ;Sequential read
        bsr     gethand
        move.l  dmaaddr,d2
        move.l  #128,d3
        sys     Read
        tst.l   d0               ;Were we successful?
        bgt     results          ;Yes.
        move.b  #$FF,newrega     ;Set failure (EOF) flag.
        bra     results

bdos21  clr.b   newrega          ;Sequential write
        bsr     gethand
        move.l  dmaaddr,d2
        move.l  #128,d3
        sys     Write
        tst.l   d0               ;Were we successful?
        bgt     results          ;Yes.
        move.b  #$FF,newrega     ;Set failure flag.
        bra     results

bdos22  move.l  #MODE_NEWFILE,d2        ;Make new file
        bra     bdos15o                 ;Use BDOS 15 open routine

bdos23  add.l   targbase,d1             ;Rename file
        movea.l d1,a0
        lea     opnname,a1
        bsr     convfn                  ;Convert old file name.
        movea.l d1,a0
        adda.l  #16,a0
        lea     renname,a1
        bsr     convfn                  ;Convert new file name.
        move.l  #opnname,d1
        move.l  #renname,d2
        sys     Rename                  ;Rename the file.
        bra     results

bdos24  equ     badbdos                 ;Get active drive map

bdos25  move.b  4(targbase),newrega     ;Get default drive number
        bra     results

bdos26  add.l   targbase,d1             ;Set file buffer address
        move.l  d1,dmaaddr
        bra     results

bdos27  move.w  #$FF6B,regh(regs)       ;Get allocation vector
        bra     results

bdos28  equ     badbdos                 ;Protect drive

bdos29  equ     badbdos                 ;Get read-only map

bdos30  equ     badbdos                 ;Set file attributes

bdos31  move.w  #$FF63,regh(regs)       ;Get disk parameter block
        bra     results

bdos32  equ     badbdos                 ;Get or set user code

bdos33  pea     _LVORead(a6)            ;Direct access read
        bra.s   bdos34c                 ;Use common read/write routine.

bdos34  pea     _LVOWrite(a6)           ;Direct access write
bdos34c clr.b   newrega                 ;Common read/write routine
        bsr     gethand
        move.l  d1,-(sp)                ;Save file handle.
        moveq   #0,d2
        move.b  35(a0),d2               ;Get seek address.
        rol.l   #8,d2
        move.b  34(a0),d2
        rol.l   #8,d2
        move.b  33(a0),d2
        rol.l   #7,d2                   ;Convert record number to byte displacement.
        move.b  33(a0),32(a0)           ;Set up current record number in extent.
        andi.b  #$7F,32(a0)
        moveq   #14,d0
        ror.l   d0,d2
        move.b  d2,12(a0)               ;Current extent number
        rol.l   d0,d2
        moveq   #-1,d3
        sys     Seek                    ;Seek to desired position.
        move.l  (sp)+,d1                ;Get the file handle again.
        move.l  (sp)+,a0                ;Address of read or write routine
        tst.l   d0                      ;Were we successful?
        bmi     bdos34e                 ;No.
        move.l  dmaaddr,d2
        move.l  #128,d3
        jsr     (a0)                    ;Read or write the desired record.
        tst.l   d0                      ;Were we successful?
        bgt     results                 ;Yes.
bdos34e move.b  #6,newrega              ;Set failure (invalid address) flag.
        bra     results

bdos35  movem.l a1/a6/d2,-(sp)          ;Get file end address
        add.l   targbase,d1
        lea     opnname,a1
        bsr     setname
        moveq   #SHARED_LOCK,d2
        move.l  dosbase,a6
        sys     Lock
        move.l  d0,lockaddr
        tst.l   d0
        beq     quitprg
        bsr     ffirst
        beq     quitprg
        lea     FileInfo,a0
        move.l  fib_Size(a0),d0
        move.l  d0,-(sp)
        lsr.l   #7,d0
        and.l   #$7F,(sp)+
        beq     1$
        addq.l  #1,d0
1$      bsr     storn
        bsr     unlock
        movem.l (sp)+,a1/a6/d2
        bra     results

bdos36  movem.l a1/a6,-(sp)      ;Get direct address
        bsr     gethand
        move.l  a0,fcbptr
        clr.l   d2
        move.l  #OFFSET_CURRENT,d3
        move.l  dosbase,a6
        sys     Seek
        lsr.l   #7,d0
        bsr     storn
        movem.l (sp)+,a1/a6
        bra     results

setname movea.l d1,a0
        move.l  d1,fcbptr
        move.l  a1,d1
*       bsr     convfn
*       rts
        jmp     convfn

ffirst  move.l  d0,d1
        move.l  #FileInfo,d2
        sys     Examine
        tst.l   d0
        rts

unlock  move.l  lockaddr,d1
        sys     UnLock
        rts

storn   move.l  fcbptr,a0
        move.b  d0,33(a0)
        lsr.l   #8,d0
        move.b  d0,34(a0)
        lsr.l   #8,d0
        move.b  d0,35(a0)
        rts

*
* Individual BIOS service routines
*
bios01  bra     quitprg         ;Warm boot

bios02  equ     bdos11          ;Console status check

bios03  equ     bdos01          ;Console input byte

bios04  move.b  regc(regs),d1   ;Console output byte
        clr.b   testdol         ;Allow dollar signs
        bsr     pchar
        bra     results

bios05  pea     regc(regs)      ;List output byte
        bra     bdos05t

bios06  equ     badbios         ;Punch output byte

bios07  equ     badbios         ;Reader input byte

bios08  equ     badbios         ;Home disk

bios09  equ     badbios         ;Select disk

bios10  equ     badbios         ;Set track

bios11  equ     badbios         ;Set sector

bios12  equ     badbios         ;Set DMA address

bios13  equ     badbios         ;Read disk

bios14  equ     badbios         ;Write disk

bios15  move.b  #$FF,newrega    ;List status
        bra     results


*
* End of program, one way or another
*
quitprg move.l  savesp,sp       ;Restore stack pointer.
        bsr     dmpstr          ;Dump any outstanding console output.
* If the list device was used, close it.
        tst.b   listopn         ;Is the printer open?
        beq.s   closprt         ;No.
        move.l  prthand,d1
        sys     Close           ;Close the printer.
closprt clr.b   listopn         ;Reset the "printer-open" flag.
* If any files were left open by the last program, close them.
        lea     handles,a0
        moveq   #(handlen-handles)/4-1,d0
closall move.l  (a0)+,d1
        beq.s   closnxt         ;This file isn't open.
        clr.l   -4(a0)          ;Make sure it isn't closed again
        movem.l a0/d0,-(sp)
        move.l  dosbase,a6
        sys     Close           ;Close this file.
        movem.l (sp)+,a0/d0
closnxt dbra    d0,closall
* Check whether we should quit the simulation.
        tst.b   quitflg         ;Exit the simulator?
        bne.s   exitsim         ;Yes.
        tst.b   cmdflag         ;Was .COM file loaded from command line?
        beq     nextprg         ;No - re-display the command prompt.
* Terminate execution of the simulator.
exitsim move.l  rawhand,d1      ;Is RAW: open?
        beq.s   closlib         ;No.
        move.l  dosbase,a6
        sys     Close           ;Close RAW:
closlib move.l  Absbase,a6
        move.l  dosbase,a1
        sys     CloseLibrary    ;Close dos.library.
        moveq   #0,d0           ;Return with no error.
        rts                     ;All done
        page
*************************************************************************
*                                                                       *
*       AmigaDOS interface routines                                     *
*                                                                       *
*************************************************************************

*
* Get a line from the console.  CP/M BDOS 10 conventions are used.
*  A0 is assumed to point to the start of the buffer.
*  If the first character encountered is a control-C, this routine
*  exits, leaving just the control-C in the buffer.
*
getline movem.l d2-d3/a0-a1/a6,-(sp)
        bsr     dmpstr          ;Flush the screen buffer first.
        move.l  dosbase,a6
        movea.l a0,a1
        addq.l  #2,a1           ;The current character loads here.
        clr.b   1(a0)           ;Clear character count.
getlinl move.l  rawhand,d1      ;Read from RAW:
        move.l  a1,d2           ; into current position
        moveq   #1,d3           ;  for a length of one byte.
        movem.l d1-d3/a0-a1,-(sp)
        sys     Read            ;Get a character.
        movem.l (sp)+,d1-d3/a0-a1
        cmpi.b  #cr,(a1)        ;Did we get a carriage return?
        beq.s   getlinc         ;Yes - stop here.
        cmpi.b  #lf,(a1)        ;Stop on a line feed too.
        beq.s   getlinc
        cmpi.b  #bs,(a1)        ;Backspace?
        bne.s   getlinp         ;No.
        tst.b   1(a0)           ;Do we have anything yet?
        beq.s   getlinl         ;No - ignore the backspace.
        subq.l  #1,a1           ;Back over the previous character.
        subq.b  #1,1(a0)        ;Decrement character count.
        movem.l a0-a1,-(sp)
        move.l  #bsmsg,d1
        jsr     pstring         ;Erase the previous character on the screen.
        movem.l (sp)+,a0-a1
        bra.s   getlinl
getlinp movem.l a0-a1,-(sp)
        sys     Write           ;Echo the current character.
        movem.l (sp)+,a0-a1
getlinn addq.b  #1,1(a0)        ;Bump character count.
        move.b  1(a0),d0        ;Number of bytes read so far.
        cmpi.b  #3,(a1)+        ;Did we get a control-C?
        bne.s   getlinf         ;No.
        cmpi.b  #1,d0           ;Is is the first character?
        beq.s   getlinx         ;Yes - exit now.
getlinf cmp.b   (a0),d0         ;Is the buffer full?
        bne.s   getlinl         ;No - try for another character.
        bra.s   getlinx
getlinc bsr     pcrlf           ;Carriage return or line feed
getlinx movem.l (sp)+,d2-d3/a0-a1/a6
        rts                     ;Exit.

*
* Display the message pointed to by D1.
*  The message must be terminated by a dollar sign.
*
pstring movem.l d2-d3/a1-a2,-(sp)       ;Save work registers.
        move.l  d1,a0           ;A0 scans the message.
        bset    #0,testdol      ;Suppress $ test?
        beq.s   pstrs           ;Yes (used by BDOS/BIOS character output)
        cmpi.b  #'$',(a0)       ;Null string?
        beq     pstrx           ;Yes - do nothing.
pstrs   move.l  strptr,a1       ;A1 loads the output buffer.
        move.l  #strbufn,d3
        sub.l   a1,d3           ;Number of bytes left in buffer
        ifne    h19
        moveq   #0,d0
        move.w  esclen,d0       ;Is a partial escape sequence saved?
        beq.s   pstrl           ;No.
        lea     escbuf,a2
        adda.l  d0,a2           ;Continue loading it here.
        clr.w   esclen          ;Reset "saved length" counter.
        cmpi.w  #2,d0           ;Did we just save one byte?
        blt.s   pstresc         ;Yes - get the remainder.
        bhi     pstreY2         ;Get the last cursor positioning byte.
        subq.l  #1,a2           ;Back over dummy byte.
        bra     pstreY1         ;Get both cursor positioning bytes.
        endc
pstrl:  cmpi.b  #lf,(a0)        ;Line feed?
        bne.s   notlf           ;No.
        lea     escbuf,a2       ;Translate it to a cursor-down sequence.
        move.b  #$9B,(a2)+
        move.b  #'B',(a2)+
        addq.l  #1,a0
        bra     pstrsub
notlf:
        ifne    h19
* Optional H19 escape sequence translation
        cmpi.b  #esc,(a0)       ;Escape character?
        bne     pstrm           ;No - treat it normally.
        lea     escbuf,a2       ;Build translated escape sequence here.
        move.b  #$9B,(a2)+      ;Start with an AmigaDOS escape character.
        addq.l  #1,a0           ;Check the next character.
        cmpi.b  #'$',(a0)       ;End of string?
        bne.s   pstresc         ;No - analyze the sequence.
        move.w  #1,esclen       ;We've saved one byte for next time.
        bra     pstrw           ;Write everything up to the ESC character.
pstresc move.b  (a0)+,d0
        cmpi.b  #'[',d0         ;ANSI escape sequence?
        beq     pstrsub         ;Yes - pass the sequence with $9B header.
        cmpi.b  #'Y',d0
        beq.s   pstreY          ;Set cursor position.
        cmpi.b  #'@',d0
        beq.s   pstrein         ;Set insert mode.
        cmpi.b  #'A',d0
        blt.s   pstreun         ;Unknown code - copy it as is.
        cmpi.b  #'O',d0
        beq.s   pstreO          ;Reset insert mode.
        bhi.s   pstreun         ;Unknown code
        move.l  a0,-(sp)
        lea     esctran,a0      ;Translation table with offset
        move.b  -'A'(a0,d0.w),d2;Get the translated code.
        move.l  (sp)+,a0
        btst    #6,d2           ;Does the translated code stand alone?
        bne.s   pstresb         ;No.
        subq.l  #1,a2           ;Back over stored CSI character.
pstresb move.b  d2,(a2)+        ;Get the translated code.
        bra.s   pstrsub
pstrein move.b  #1,insflag      ;Set insert mode.
        bra.s   pstrsbx
pstreO  clr.b   insflag         ;Reset insert mode.
        bra.s   pstrsbx
pstreY  cmpi.b  #'Y',d0         ;Set cursor position
        bne.s   pstreun
        cmpi.b  #'$',(a0)       ;End of string?
        bne.s   pstreY1         ;No.
        move.w  #2,esclen       ;Indicate we need both position bytes.
        bra     pstrw           ;Finish the sequence next time.
pstreY1 moveq   #0,d0
        move.b  (a0)+,d0        ;Get the first position byte.
        bsr     pstrcvd         ;Convert to decimal in save area.
        move.b  #';',(a2)+      ;Add the separator character.
        cmpi.b  #'$',(a0)       ;End of string?
        bne.s   pstreY2         ;No.
        sub.l   #escbuf,a2      ;Number of bytes saved
        move.w  a2,esclen
        bra     pstrw           ;Get the last byte next time.
pstreY2 moveq   #0,d0
        move.b  (a0)+,d0        ;Get the last position byte.
        bsr     pstrcvd         ;Convert to decimal in save area.
        move.b  #'H',(a2)+      ;Terminate the sequence.
        bra.s   pstrsub
pstreun move.b  #esc,escbuf     ;Unidentified escape sequence -
        move.b  d0,(a2)+        ; pass it through as is.
* The translated escape sequence is now in "escbuf" -
*  copy it to the output buffer.
pstrsub move.l  a2,d0
        lea     escbuf,a2       ;A2 scans translated escape sequence.
        sub.l   a2,d0           ;Length of translated escape sequence
        subq.l  #1,d0
pstrsbl move.b  (a2)+,(a1)+     ;Copy substitution to output string.
        subq.w  #1,d3           ;Count down remaining length.
        dbra    d0,pstrsbl
pstrsbx cmpi.b  #'$',(a0)       ;End of string?
        beq     pstrw           ;Yes - write it out.
        tst.w   d3              ;Is the buffer full?
        bmi     pstrw           ;Yes - write out what we have.
        cmpi.b  #lf,-1(a0)      ;Line feed?
        bne     pstrl           ;No.
        tst.b   bufflag         ;Is console buffering in effect?
        beq     pstrl           ;No.
        move.l  a1,strptr
        bsr     dmpstr          ;Dump the buffer.
        move.l  strptr,a1
        bra     pstrl           ;Check for another escape sequence.
* Subroutine to convert the byte in D0 to a character string at (A2)+
pstrcvd subi.b  #' '-1,d0       ;Convert to binary row or column number.
        divu    #10,d0          ;Convert to tens and units.
        tst.w   d0              ;Is the number 10 or greater?
        beq.s   pstrcv1         ;No - just create a one-digit number.
        addi.b  #'0',d0         ;Convert the tens digit to ASCII.
        move.b  d0,(a2)+        ;Move it to the result field.
pstrcv1 swap    d0              ;Get the units digit.
        addi.b  #'0',d0         ;Convert it to ASCII.
        move.b  d0,(a2)+
        rts
        endc
* Normal character processing
pstrm   tst.b   insflag         ;Are we in insert mode?
        beq.s   pstrmv          ;No.
        lea     escbuf,a2
        move.b  #$9B,(a2)+      ;Build an insert-character sequence.
        move.b  #'@',(a2)+
        move.b  (a0)+,(a2)+     ;Here's the character to insert.
        bra.s   pstrsub         ;Use the substitution routine.
pstrmv  move.b  (a0)+,(a1)+     ;Move one character.
        tst.b   bufflag         ;Is console buffering in effect?
        beq.s   pstreos         ;No.
        cmpi.b  #cr,-1(a0)      ;Carriage return?
        beq.s   pstrseg         ;Yes - dump the current segment.
        cmpi.b  #bel,-1(a0)     ;Bell?
        bne.s   pstreos         ;No - continue buffering.
pstrseg move.l  a1,strptr
        bsr     dmpstr          ;Dump the buffer.
        move.l  strptr,a1
pstreos cmpi.b  #'$',(a0)       ;Test for end of string.
        dbeq    d3,pstrl        ;Loop until we get there or buffer is full.
pstrw   move.l  a1,strptr
        tst     d3              ;Is the buffer full?
        bmi.s   pstrdmp         ;Yes - dump it regardless.
        tst.b   bufflag         ;Is console buffering in effect?
        bne.s   pstrnxt         ;Yes - don't write anything yet.
pstrdmp bsr     dmpstr          ;Dump the buffer.
        move.l  strptr,a1
pstrnxt tst     d3              ;Did the output buffer overflow?
        bmi     pstrs           ;Yes - get another section of the message.
pstrx   movem.l (sp)+,d2-d3/a1-a2       ;Restore registers
        rts
*
* Write the contents of "strbuf" to RAW: if possible, or stdout if not.
*  The number of bytes to be written is calculated from "strptr".
*
dmpstr  movem.l d2-d3/a0-a1/a6,-(sp)
        move.l  strptr,d3
        move.l  #strbuf,d2      ;Address of buffer
        move.l  d2,strptr       ;Reset the buffer pointer.
        sub.l   d2,d3           ;Length of output string
        beq.s   dmpstrx         ;Zero - don't write anything.
        move.l  rawhand,d1      ;Assume we're writing to RAW:
        bne.s   dmpstrp
        move.l  stdout,d1       ;We don't have RAW: - use stdout.
dmpstrp move.l  dosbase,a6
        sys     Write           ;Display the line.
dmpstrx movem.l (sp)+,d2-d3/a0-a1/a6
        rts

*
* Convert the file name in the FCB pointed to by A0
*  to an AmigaDOS-format file name in the field pointed to by A1.
*  D0 is the only other register used by this routine.
*
convfn  move.l  a1,-(sp)
        move.l  a0,-(sp)        ;Save start address of FCB.
        tst.b   (a0)+           ;Skip over drive code for now.
        moveq   #7,d0           ;Maximum of 8 characters for file name
convfn1 cmpi.b  #' ',(a0)       ;End of file name?
        beq.s   convfnz         ;Yes
        move.b  (a0)+,(a1)+     ;Move one character of file name.
        dbra    d0,convfn1      ;Try for more.
convfnz movea.l (sp)+,a0        ;Back to start of FCB.
        adda.l  #9,a0           ;Go to start of file name extension.
        cmpi.b  #' ',(a0)       ;Do we have an extension?
        beq.s   convfnx         ;No.
        move.b  #'.',(a1)+      ;Insert extension separator.
        moveq   #2,d0           ;Maximum of 3 characters for extension.
convfn2 cmpi.b  #' ',(a0)       ;End of extension?
        beq.s   convfnx         ;Yes.
        move.b  (a0)+,(a1)+     ;Move one character of extension.
        dbra    d0,convfn2      ;Try for more.
convfnx clr.b   (a1)            ;Terminate file name string.
        move.l  (sp)+,a1
        rts

*
* Get the file handle indicated by the number inserted into the
*  CP/M FCB by the open routine.  It is copied from the file handle
*  table entry (whose address is set up as 0(A1,D0.W)) to D1.
*  The Z flag will be set if the handle is zero (i.e. file not open).
*  A0 points to the FCB.
*
gethand add.l   targbase,d1     ;The FCB is here.
        movea.l d1,a0
        lea     handles,a1
        moveq   #0,d0
        move.b  13(a0),d0       ;Get handle number from FCB.
        asl.w   #2,d0           ;Convert to table offset.
        move.l  0(a1,d0.w),d1   ;Get the file handle.
        rts
        page
*************************************************************************
*                                                                       *
*       Miscellaneous service routines                                  *
*       (Inelegant, but rarely used so they stand as is.)               *
*                                                                       *
*************************************************************************

*
* Display the contents of D1 in hex.
*
pbyte   move.l  #$20018,d0      ;2 nybbles, 24-bit shift first
        bra.s   phex
pword   move.l  #$40010,d0      ;4 nybbles, 16-bit shift first
        bra.s   phex
paddr   move.l  #$60008,d0      ;6 nybbles, 8-bit shift first
        bra.s   phex
plong   move.l  #$80000,d0      ;8 nybbles, no shift first
phex    lea     workbuf,a0
        move.l  a0,-(sp)
        bsr     pdigits
        move.b  #'$',(a0)+
        move.l  (sp)+,d1
        bsr     pstring
        rts
*
* Convert the contents of D1 to hex at (A0).
*  On exit, A0 points to the next available byte.
*
ubyte   move.l  #$20018,d0      ;2 nybbles, 24-bit shift first
        bra.s   pdigits
uword   move.l  #$40010,d0      ;4 nybbles, 16-bit shift first
        bra.s   pdigits
uaddr   move.l  #$60008,d0      ;6 nybbles, 8-bit shift first
        bra.s   pdigits
ulong   move.l  #$80000,d0      ;8 nybbles, no shift first
pdigits rol.l   d0,d1           ;Do shift.
        bra.s   pdigent
pdiglop swap    d0              ;Save nybble count.
        rol.l   #4,d1           ;Print variable in d1.
        move.l  d1,-(sp)
        and     #$F,d1          ;Isolate the current nybble.
        cmp     #$A,d1
        blt.s   ntoa2
        add.b   #'A'-'9'-1,d1   ;Adjust for digits A through F.
ntoa2   add.b   #'0',d1         ;Convert to ASCII.
        move.b  d1,(a0)+        ;Add to the result string.
        move.l  (sp)+,d1
pdigent swap    d0              ;Get nybble count.
        dbra    d0,pdiglop
        rts

pchar   move.b  d1,workbuf      ;Print the character in D1.
        move.b  #'$',workbuf+1
        move.l  #workbuf,d1
        bsr     pstring
        rts

pspace  move.l  #spacemsg,d1    ;Print a space.
        bsr     pstring
        rts

pcrlf   move.l  #crlfmsg,d1     ;Print a carriage return and line feed.
        bsr     pstring
        rts

*
* Convert the hex string pointed to by A0 to long in d1.
*  Stops on the first invalid hex digit, which is returned in d0.
*  A0 is left pointing to this first invalid digit.
*  d2 = 1 if any valid digits were found, 0 otherwise.
*
atol    moveq   #0,d1
        moveq   #0,d2
atol1   move.b  (a0)+,d0        ;Get the current byte.
        cmpi.b  #$40,d0
        blt.s   atol2
        and     #$5F,d0         ;Mask to upper case.
atol2   cmpi.b  #'0',d0         ;Check range (0..9,A..F).
        blt.s   atolend
        cmpi.b  #'F',d0
        bhi.s   atolend
        cmpi.b  #'9',d0
        ble.s   atol3
        cmpi.b  #'A'-1,d0
        bhi.s   atol3
        bra.s   atolend
atol3   moveq   #1,d2           ;Valid characters entered, set flag.
        sub.b   #'0',d0         ;Convert to binary
        cmp.b   #$9,d0          ;Digit in range 0..9?
        ble.s   atol4           ;Yes - conversion is complete
        sub.b   #'A'-'9'-1,d0   ;Adjust digits A..F.
atol4   ext     d0              ;Convert to long.
        ext.l   d0
        asl.l   #4,d1           ;Tack it onto d1.
        add.l   d0,d1
        bra.s   atol1           ;Try for another digit.
atolend subq.l  #1,a0           ;Back onto the first invalid digit.
        rts
        page
*************************************************************************
*                                                                       *
*       This table contains the mnemonic strings for the 8080           *
*       opcodes.  These are used in tracing.  The first character       *
*       flags operands.  Blank is nothing, A is an address (i.e.        *
*       a 16-bit value), and C is a constant (i.e. an 8-bit value).     *
*                                                                       *
*************************************************************************

        data    data

mnops:
        dc.b    ' NOP$    ALXI B,$  STAX B$  INX B$  '  ;00-03
        dc.b    ' INR B$   DCR B$  CMVI B,$  RLC$    '  ;04-07
        dc.b    ' ILLEGAL$ DAD B$   LDAX B$  DCX B$  '  ;08-0B
        dc.b    ' INR C$   DCR C$  CMVI C,$  RRC$    '  ;0C-0F
        dc.b    ' ILLEGAL$ALXI D,$  STAX D$  INX D$  '  ;10-13
        dc.b    ' INR D$   DCR D$  CMVI D,$  RAL$    '  ;14-17
        dc.b    ' JR$      DAD D$   LDAX D$  DCX D$  '  ;18-1B
        dc.b    ' INR E$   DCR E$  CMVI E,$  RAR$    '  ;1C-1F
        dc.b    ' JRNZ$   ALXI H,$ ASHLD $   INX H$  '  ;20-23
        dc.b    ' INR H$   DCR H$  CMVI H,$  DAA$    '  ;24-27
        dc.b    ' JRZ$     DAD H$  ALHLD $   DCX H$  '  ;28-2B
        dc.b    ' INR L$   DCR L$  CMVI L,$  CMA$    '  ;2C-2F
        dc.b    ' JRNC$   ALXI SP,$ASTA $    INX SP$ '  ;30-33
        dc.b    ' INR M$   DCR M$  CMVI M,$  STC$    '  ;34-37
        dc.b    ' JRC$     DAD SP$ ALDA $    DCX SP$ '  ;38-3B
        dc.b    ' INR A$   DCR A$  CMVI A,$  CMC$    '  ;3C-3F
        dc.b    ' MOV B,B$ MOV B,C$ MOV B,D$ MOV B,E$'  ;40-43
        dc.b    ' MOV B,H$ MOV B,L$ MOV B,M$ MOV B,A$'  ;44-47
        dc.b    ' MOV C,B$ MOV C,C$ MOV C,D$ MOV C,E$'  ;48-4B
        dc.b    ' MOV C,H$ MOV C,L$ MOV C,M$ MOV C,A$'  ;4C-4F
        dc.b    ' MOV D,B$ MOV D,C$ MOV D,D$ MOV D,E$'  ;50-53
        dc.b    ' MOV D,H$ MOV D,L$ MOV D,M$ MOV D,A$'  ;54-57
        dc.b    ' MOV E,B$ MOV E,C$ MOV E,D$ MOV E,E$'  ;58-5B
        dc.b    ' MOV E,H$ MOV E,L$ MOV E,M$ MOV E,A$'  ;5C-5F
        dc.b    ' MOV H,B$ MOV H,C$ MOV H,D$ MOV H,E$'  ;60-63
        dc.b    ' MOV H,H$ MOV H,L$ MOV H,M$ MOV H,A$'  ;64-67
        dc.b    ' MOV L,B$ MOV L,C$ MOV L,D$ MOV L,E$'  ;68-6B
        dc.b    ' MOV L,H$ MOV L,L$ MOV L,M$ MOV L,A$'  ;6C-6F
        dc.b    ' MOV M,B$ MOV M,C$ MOV M,D$ MOV M,E$'  ;70-73
        dc.b    ' MOV M,H$ MOV M,L$ HLT$     MOV M,A$'  ;74-77
        dc.b    ' MOV A,B$ MOV A,C$ MOV A,D$ MOV A,E$'  ;78-7B
        dc.b    ' MOV A,H$ MOV A,L$ MOV A,M$ MOV A,A$'  ;7C-7F
        dc.b    ' ADD B$   ADD C$   ADD D$   ADD E$  '  ;80-83
        dc.b    ' ADD H$   ADD L$   ADD M$   ADD A$  '  ;84-87
        dc.b    ' ADC B$   ADC C$   ADC D$   ADC E$  '  ;88-8B
        dc.b    ' ADC H$   ADC L$   ADC M$   ADC A$  '  ;8C-8F
        dc.b    ' SUB B$   SUB C$   SUB D$   SUB E$  '  ;90-93
        dc.b    ' SUB H$   SUB L$   SUB M$   SUB A$  '  ;94-97
        dc.b    ' SBB B$   SBB C$   SBB D$   SBB E$  '  ;98-9B
        dc.b    ' SBB H$   SBB L$   SBB M$   SBB A$  '  ;9C-9F
        dc.b    ' ANA B$   ANA C$   ANA D$   ANA E$  '  ;A0-A3
        dc.b    ' ANA H$   ANA L$   ANA M$   ANA A$  '  ;A4-A7
        dc.b    ' XRA B$   XRA C$   XRA D$   XRA E$  '  ;A8-AB
        dc.b    ' XRA H$   XRA L$   XRA M$   XRA A$  '  ;AC-AF
        dc.b    ' ORA B$   ORA C$   ORA D$   ORA E$  '  ;B0-B3
        dc.b    ' ORA H$   ORA L$   ORA M$   ORA A$  '  ;B4-B7
        dc.b    ' CMP B$   CMP C$   CMP D$   CMP E$  '  ;B8-BB
        dc.b    ' CMP H$   CMP L$   CMP M$   CMP A$  '  ;BC-BF
        dc.b    ' RNZ$     POP B$  AJNZ $   AJMP $   '  ;C0-C3
        dc.b    'ACNZ $    PUSH B$ CADI $    RST 0$  '  ;C4-C7
        dc.b    ' RZ$      RET$    AJZ $     ILLEGAL$'  ;C8-CB
        dc.b    'ACZ $    ACALL $  CACI $    RST 1$  '  ;CC-CF
        dc.b    ' RNC$     POP D$  AJNC $   COUT $   '  ;D0-D3
        dc.b    'ACNC $    PUSH D$ CSUI $    RST 2$  '  ;D4-D7
        dc.b    ' RC$      EXX$    AJC $    CIN $    '  ;D8-DB
        dc.b    'ACC $     ILLEGAL$CSBI $    RST 3$  '  ;DC-DF
        dc.b    ' RPO$     POP H$  AJPO $    XTHL$   '  ;E0-E3
        dc.b    'ACPO $    PUSH H$ CANI $    RST 4$  '  ;E4-E7
        dc.b    ' RPE$     PCHL$   AJPE $    XCHG$   '  ;E8-EB
        dc.b    'ACPE $    ILLEGAL$CXRI $    RST 5$  '  ;EC-EF
        dc.b    ' RP$      POP PSW$AJP $     DI$     '  ;F0-F3
        dc.b    'ACP $     PUSH P$ CORI $    RST 6$  '  ;F4-F7
        dc.b    ' RM$      SPHL$   AJM $     EI$     '  ;F8-FB
        dc.b    'ACM $     ILLEGAL$CCPI $    RST 7$  '  ;FC-FF
        page
*************************************************************************
*                                                                       *
*       Fake FDOS                                                       *
*                                                                       *
*************************************************************************

*
* Fake BDOS for target system
*
fdos    dc.b    tHLT,0,tRET       ;BIOS jump table
        dc.b    tJMP,$33,$FF     ;Warm boot
        dc.b    tJMP,$36,$FF     ;Console status
        dc.b    tJMP,$39,$FF     ;Console input
        dc.b    tJMP,$3C,$FF     ;Console output
        dc.b    tJMP,$3F,$FF     ;List output
        dc.b    tJMP,$42,$FF     ;Punch output
        dc.b    tJMP,$45,$FF     ;Reader input
        dc.b    tJMP,$48,$FF     ;Home disk
        dc.b    tJMP,$4B,$FF     ;Select disk
        dc.b    tJMP,$4E,$FF     ;Set track
        dc.b    tJMP,$51,$FF     ;Set sector
        dc.b    tJMP,$54,$FF     ;Set DMA address
        dc.b    tJMP,$57,$FF     ;Read
        dc.b    tJMP,$5A,$FF     ;Write
        dc.b    tJMP,$5D,$FF     ;Get list device status
        dc.b    tJMP,$60,$FF     ;Sector translation
*
* Fake BIOS for target system
*
        dc.b    tHLT,1,tRET       ;Warm boot
        dc.b    tHLT,2,tRET       ;Console status
        dc.b    tHLT,3,tRET       ;Console input
        dc.b    tHLT,4,tRET       ;Console output
        dc.b    tHLT,5,tRET       ;List output
        dc.b    tHLT,6,tRET       ;Punch output
        dc.b    tHLT,7,tRET       ;Reader input
        dc.b    tHLT,8,tRET       ;Home disk *
        dc.b    tHLT,9,tRET       ;Select disk *
        dc.b    tHLT,10,tRET      ;Set track *
        dc.b    tHLT,11,tRET      ;Set sector *
        dc.b    tHLT,12,tRET      ;Set DMA address *
        dc.b    tHLT,13,tRET      ;Read *
        dc.b    tHLT,14,tRET      ;Write *
        dc.b    tHLT,15,tRET      ;Get list device status *
        dc.b    tHLT,16,tRET      ;Sector translation *
*
* Fake Disk Parameter Block
*
        dc.b    26,0,3,$07,0,242,0,$3f
*
* Fake Disk Block Allocation Table
*
        dcb.b   21,$FF
        dc.b    %11111100
        dcb.b   10,0

fdoslen equ     *-fdos

*
* BDOS function vector table
*
        cnop    0,4
bdostab dc.l    bdos00,bdos01,bdos02,bdos03,bdos04,bdos05,bdos06,bdos07
        dc.l    bdos08,bdos09,bdos10,bdos11,bdos12,bdos13,bdos14,bdos15
        dc.l    bdos16,bdos17,bdos18,bdos19,bdos20,bdos21,bdos22,bdos23
        dc.l    bdos24,bdos25,bdos26,bdos27,bdos28,bdos29,bdos30,bdos31
        dc.l    bdos32,bdos33,bdos34,bdos35,bdos36
bdostabn:

*
* BIOS function vector table
*
        cnop    0,4
biostab dc.l    bdosfn,bios01,bios02,bios03,bios04,bios05,bios06,bios07
        dc.l    bios08,bios09,bios10,bios11,bios12,bios13,bios14,bios15
biostabn:
        page
*************************************************************************
*                                                                       *
*       Messages                                                        *
*                                                                       *
*************************************************************************

dosname dc.b    'dos.library',0
aprompt dc.b    'A>$'
crlfmsg dc.b    cr,lf,'$'
spacemsg dc.b   ' $'
bsmsg   dc.b    bs,' ',bs,'$'   ;Erases the previous character
simsg   dc.b    si,'$'          ;Resets MSB of each output character
rawspec dc.b    'RAW:0/0/640/200/CP/M Emulator'
        dc.b    ' by J. Cathey, C. Gibbs & W. Kusche - V'
        dc.b    vermaj&15+'0','.'
        dc.b    vermin/16+'0',vermin&15+'0',' ('
        dc.b    revmonth/16+'0',revmonth&15+'0','/'
        dc.b    revday/16+'0',revday&15+'0','/'
        dc.b    revyear/16+'0',revyear&15+'0',')',0
rawerr  dc.b    'Unable to open RAW: - code $'
setwin  dc.b    $9B,'0x',$9B,'8y',$9B,'24t',$9B,'80u',$9B,'H',$9B,'J$'
fullmsg dc.b    'Too many files are open!',cr,lf,'$'
illgmsg dc.b    cr,lf,'Illegal instruction $'
ilgmsg2 dc.b    ' at $'
ilgmsg3 dc.b    '.$'
dumpmsg dc.b    cr,lf,'Register contents:$'
dmpmsg3 dc.b    '(Q)uit, (C)hange trace address, (S)top tracing, '
        dc.b    'any other key to continue: $'
ilgbios dc.b    cr,lf,'Illegal BIOS call $'
ilgbdos dc.b    cr,lf,'Illegal BDOS call $'
tracemsg dc.b   cr,lf,'Start trace at >$'
tracemg2 dc.b   '  End trace at >$'
btrcmsg dc.b    'Trace BIOS/BDOS calls? >$'
biosmsg dc.b    'BIOS call $'
bdosmsg dc.b    'BDOS call $'
atmsg   dc.b    ' at $'
esctran dc.b    'ABCD',ff,so,si,'H',$8D,'JKLMP' ;Escape sequence translation
prtname dc.b    'PRT:RAW',0
badprt  dc.b    'Unable to open the list device!$'
        page
*************************************************************************
*                                                                       *
*       Variable storage                                                *
*                                                                       *
*************************************************************************

        bss     bss

savesp  ds.l    1               ;Stack pointer save area
dosbase ds.l    1               ;Pointer to dos.library
stdin   ds.l    1               ;Keyboard handle (stdin)
stdout  ds.l    1               ;Screen handle (stdout)
rawhand ds.l    1               ;RAW: file handle
prthand ds.l    1               ;PRT:RAW file handle
lockaddr:
        ds.l    1               ;Lock address - for retrieving file size
CurDir  ds.l    1               ;Old lock - current directory
handles ds.l    8               ;File handles for opened files (or zero)
handlen:                        ;End of file handle table
dmaaddr ds.l    1               ;Current DMA address
comend  ds.l    1               ;End of .COM file name on command line
FileInfo:
        ds.l    32              ;FileInfoBlock
cmdline ds.b    128             ;Command line
cmdlinen:                       ;End of command line
comname ds.b    13              ;Name of file to load
comnamen:                       ;End of file name
opnname ds.b    17              ;File name for OPEN or RENAME
renname ds.b    17              ;New file name for RENAME
scanname:
        ds.b    17              ;File name for directory scan
newrega ds.b    1               ;BIOS/BDOS accumulator work area
workbuf ds.b    80              ;Work buffer for "pstring" (including $)
workbufn:                       ;End of work buffer
DirFName:
        ds.b    80              ;Last 72 bytes leave room for non-CP/M names
DirFExt ds.b    80              ;Last 77 bytes leave room for non-CP/M names
strbuf  ds.b    2048            ;String output buffer
strbufn ds.b    8               ;"strbuf" overflow area - must follow "strbuf"!
strptr  ds.l    1               ;Current position in "strbuf"
escbuf  ds.b    8               ;Translated escape sequence
esclen  ds.w    1               ;Number of bytes saved in "escbuf"
cmdflag ds.b    1               ;Take program name from command line.
quitflg ds.b    1               ;"quitprg" exit flag
testdol ds.b    1               ;"pstring" should test for leading $
insflag ds.b    1               ;We're in insert mode.
dumpcnt ds.b    1               ;"dump" counter for pausing
btrcflg ds.b    1               ;Trace BIOS/BDOS calls.
bufflag ds.b    1               ;Console output is buffered.
fcbptr  ds.l    1               ;Pointer to current FCB
listopn ds.b    1               ;The list device is open.
*************************************************************************
*                                                                       *
*       Target processor's address space                                *
*                                                                       *
*************************************************************************

        even

registers ds.b  22              ;Actual storage for Z80's other registers
target  ds.b    $10000          ;Z80's universe

        end

