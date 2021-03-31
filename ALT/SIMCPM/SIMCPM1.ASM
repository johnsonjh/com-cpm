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
        bsr     pstring         ;Erase the previous character on the screen.
        movem.l (sp)+,a0-a1
        bra.s   getlinl
bsmsg   dc.b    bs,' ',bs,'$'   ;Erases the previous character
getlinp movem.l a0-a1,-(sp)
        sys     Write           ;Echo the current character.
        movem.l (sp)+,a0-a1
        addq.b  #1,1(a0)        ;Bump character count.
        move.b  1(a0),d0        ;Number of bytes read so far.
        cmpi.b  #3,(a1)+        ;Did we get a control-C?
        bne.s   1$              ;No.
        cmpi.b  #1,d0           ;Is is the first character?
        beq.s   getlinx         ;Yes - exit now.
1$      cmp.b   (a0),d0         ;Is the buffer full?
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
        bcs.s   pstresc         ;Yes - get the remainder.
        bhi     pstreY2         ;Get the last cursor positioning byte.
        subq.l  #1,a2           ;Back over dummy byte.
        bra     pstreY1         ;Get both cursor positioning bytes.
        endc
pstrl   cmpi.b  #lf,(a0)        ;Line feed?
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
        bcs     pstreun         ;Unknown code - copy it as is.
        cmpi.b  #'O',d0
        beq.s   pstreO          ;Reset insert mode.
        bhi     pstreun         ;Unknown code
        move.l  a0,-(sp)
        lea     esctran(pc),a0  ;Translation table with offset
        move.b  -'A'(a0,d0.w),d2;Get the translated code.
        move.l  (sp)+,a0
        btst    #6,d2           ;Does the translated code stand alone?
        bne.s   1$              ;No.
        subq.l  #1,a2           ;Back over stored CSI character.
1$      move.b  d2,(a2)+        ;Get the translated code.
        bra.s   pstrsub
esctran dc.b    'ABCD',ff,so,si,'H',$8D,'JKLMP' ;Escape sequence translation
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
        endc
* The translated escape sequence is now in "escbuf" -
*  copy it to the output buffer.
pstrsub move.l  a2,d0
        lea     escbuf,a2       ;A2 scans translated escape sequence.
        sub.l   a2,d0           ;Length of translated escape sequence
        subq.l  #1,d0
1$      move.b  (a2)+,(a1)+     ;Copy substitution to output string.
        subq.w  #1,d3           ;Count down remaining length.
        dbra    d0,1$
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
        beq.s   1$              ;No - just create a one-digit number.
        addi.b  #'0',d0         ;Convert the tens digit to ASCII.
        move.b  d0,(a2)+        ;Move it to the result field.
1$      swap    d0              ;Get the units digit.
        addi.b  #'0',d0         ;Convert it to ASCII.
        move.b  d0,(a2)+
        rts
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
        beq.s   2$              ;No.
        cmpi.b  #cr,-1(a0)      ;Carriage return?
        beq.s   1$              ;Yes - dump the current segment.
        cmpi.b  #bel,-1(a0)     ;Bell?
        bne.s   2$              ;No - continue buffering.
1$      move.l  a1,strptr
        bsr     dmpstr          ;Dump the buffer.
        move.l  strptr,a1
2$      cmpi.b  #'$',(a0)       ;Test for end of string.
        dbeq    d3,pstrl        ;Loop until we get there or buffer is full.
pstrw   move.l  a1,strptr
        tst     d3              ;Is the buffer full?
        bmi.s   1$              ;Yes - dump it regardless.
        tst.b   bufflag         ;Is console buffering in effect?
        bne.s   2$              ;Yes - don't write anything yet.
1$      bsr     dmpstr          ;Dump the buffer.
        move.l  strptr,a1
2$      tst     d3              ;Did the output buffer overflow?
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
        beq.s   2$              ;Zero - don't write anything.
        move.l  rawhand,d1      ;Assume we're writing to RAW:
        bne.s   1$
        move.l  stdout,d1       ;We don't have RAW: - use stdout.
1$      move.l  _DOSBase,a6
        sys     Write           ;Display the line.
2$      movem.l (sp)+,d2-d3/a0-a1/a6
        rts

*
* Convert the file name in the FCB pointed to by A0
*  to an AmigaDOS-format file name in the field pointed to by A1.
*  D0 is the only other register used by this routine.
*
convfn  move.l  a1,-(sp)
        move.l  a0,-(sp)        ;Save start address of FCB.
        move.b  (a0)+,d0        ;Get the drive code.
        bne.s   1$              ;We have a drive code.
        move.b  target+4,d0     ;Use the default drive code.
        andi.b  #$0F,d0
        addq.b  #1,d0           ;Start at 1 for drive A:.
1$      cmpi.b  #1,d0           ;Is it drive A:?
        beq.s   4$              ;Yes - don't add anything special.
        move.b  #'C',(a1)+
        move.b  #'P',(a1)+      ;Set up the prefix CPMx:
        move.b  #'M',(a1)+      ; where x is the drive letter.
        add.b   #'A'-1,d0
        move.b  d0,(a1)+
        move.b  target+4,d0     ;Get user number.
        lsr.b   #4,d0           ;Move it to low-order 4 bits.
        beq.s   3$              ;Don't insert user number if it's zero.
        move.b  #'0',(a1)+      ;Assume user number is less than 10.
        cmpi.b  #10,d0          ;Is user number 10 or greater?
        bcs.s   2$              ;No.
        move.b  #'1',-1(a1)     ;Change the first digit to 1.
        subi.b  #10,d0
2$      addi.b  #'0',d0         ;Convert user number to ASCII.
        move.b  d0,(a1)+        ;Insert user number into file spec.
3$      move.b  #':',(a1)+
4$      moveq   #7,d0           ;Maximum of 8 characters for file name
convfn1 cmpi.b  #' ',(a0)       ;End of file name?
        beq.s   3$              ;Yes
        cmpi.b  #'*',(a0)       ;Wild card?
        bne.s   1$              ;No.
        move.b  #'#',(a1)+      ;Convert to AmigaDOS format.
        move.b  #'?',(a1)+
        addq.l  #1,a0           ;Skip over the asterisk.
        bra.s   2$
1$      move.b  (a0)+,(a1)+     ;Move one character of file name.
2$      dbra    d0,convfn1      ;Try for more.
3$      movea.l (sp)+,a0        ;Back to start of FCB.
        lea     9(a0),a0        ;Go to start of file name extension.
        cmpi.b  #' ',(a0)       ;Do we have an extension?
        beq.s   convfnx         ;No.
        move.b  #'.',(a1)+      ;Insert extension separator.
        moveq   #2,d0           ;Maximum of 3 characters for extension.
convfn2 cmpi.b  #' ',(a0)       ;End of extension?
        beq.s   convfnx         ;Yes.
        cmpi.b  #'*',(a0)       ;Wild card?
        bne.s   1$              ;No.
        move.b  #'#',(a1)+      ;Convert to AmigaDOS format.
        move.b  #'?',(a1)+
        addq.l  #1,a0           ;Skip over the asterisk.
        bra.s   2$
1$      move.b  (a0)+,(a1)+     ;Move one character of extension.
2$      dbra    d0,convfn2      ;Try for more.
convfnx clr.b   (a1)            ;Terminate file name string.
        move.l  (sp)+,a1
        rts

*
* Get the file handle indicated by the first 12 bytes of the CP/M FCB
*  whose CP/M address is in D1.  The file handle (if found) is copied
*  to D1 from the file handle table entry (which is pointed to by A1).
*  If the file handle cannot be found, D1 will be set to zero.
*  In any event, A0 will be set to point to the FCB.
*
gethand lea     0(targbase,d1.l),a0     ;The FCB is here.
        lea     handles,a1      ;A1 scans the file handle table.
        moveq   #(handlen-handles)/16-1,d0
1$      movem.l d0/a0-a1,-(sp)
        tst.l   (a1)            ;Is this entry empty?
        beq.s   3$              ;Yes - ignore it.
        addq.l  #4,a1           ;Skip over to file name in table.
        moveq   #11,d1
2$      cmp.b   (a0)+,(a1)+     ;Compare first 12 bytes of FCB with table.
        bne.s   3$              ;No match.
        dbra    d1,2$
        movem.l (sp)+,d0/a0-a1
        move.l  (a1),d1         ;Here's the file handle.
        rts
3$      movem.l (sp)+,d0/a0-a1
        lea     16(a1),a1       ;Try the next table entry.
        dbra    d0,1$
        moveq   #0,d1           ;Couldn't find the handle!
        rts
        page
*************************************************************************
*                                                                       *
*       Serial port routines                                            *
*                                                                       *
*************************************************************************

*
* Read a byte from the port whose number is in D0.
*  A0 points to where to put it.
*
inp     movem.l a1/a6/d2-d3,-(sp)
        cmp.b   #$14,d0         ;Port 14?
        bne.s   inp15           ;No.
        move.l  a0,-(sp)
        tst.l   rpport          ;Is serial.device open?
        bne.s   1$              ;Yes.
        bsr     initser         ;Set up serial.device.
1$      bsr     checkio
        tst.l   d0              ;Is a character ready?
        beq.s   2$              ;No - give the previous one again.
        bsr     serread         ;Read the new character.
2$      move.l  (sp)+,a0
        move.b  charin,(a0)     ;Move it to user's area.
        bra.s   inpx
inp15   cmp.b   #$15,d0         ;Port 15?
        bne.s   inpx            ;No - ignore it.
        move.b  #7,(a0)         ;Assume a character is ready.
        move.l  a0,-(sp)
        bsr     checkio         ;Check whether a character is ready.
        move.l  (sp)+,a0
        tst.l   d0              ;Is a character ready?
        bne.s   inpx            ;Yes.
        move.b  #5,(a0)         ;Don't set "receiver ready" bit.
inpx    movem.l (sp)+,a1/a6/d2-d3
        rts

*
* Write the byte pointed to by A0 to the port whose number is in D0.
*
outp    movem.l a1/a6/d2-d3,-(sp)
        cmp.l   #$14,d0         ;Port 14?
        bne.s   outpx           ;No - ignore it.
        move.b  (a0),charout    ;Character to write
        tst.l   rpport          ;Is serial.device open?
        bne.s   1$              ;Yes.
        bsr     initser         ;Set up serial.device.
1$      bsr     serwrit         ;Write the character.
outpx   movem.l (sp)+,a1/a6/d2-d3
        rts

*
* Initialize the serial port.
*
initser:

* Open a reply port for the serial input device.
        lea     readreq,a0
        move.w  #rsize-1,d0
1$      clr.b   (a0)+           ;Clear the I/O request block.
        dbra    d0,1$
        clr.l   -(sp)
        move.l  #2$,-(sp)
        jsr     _CreatePort     ;rpport = CreatePort ("Read_RS", NULL);
        addq.l  #8,sp
        move.l  d0,rpport
        move.l  d0,r_ReplyPort
        bne.s   openin
        move.l  #3$,d1
        bsr     pstring
        bra     quitprg
2$      dc.b    'Read_RS',0
3$      dc.b    'Can''t create input port for serial.device!',cr,lf,'$'

* Open the serial input device.
openin  move.w  #rsize,r_MLength
        move.b  #SERFLAGS,r_SerFlags
        move.l  #CTLCHAR,r_CtlChar
        lea     1$,a0
        moveq   #0,d0
        lea     readreq,a1
        moveq   #0,d1
        move.l  _SysBase,a6
        sys     OpenDevice
        tst.l   d0              ;Were we successful?
        beq.s   setin           ;Yes.
        move.l  #2$,d1
        bsr     pstring
        bra     quitprg
1$      dc.b    'serial.device',0
2$      dc.b    'Can''t open serial.device!',cr,lf,'$'

setin   move.l  baud,r_Baud
        move.b  bits,r_ReadLen  ;Number of bits per character
        move.b  bits,r_WriteLen
        move.w  #SDCMD_SETPARAMS,r_Command
        lea     readreq,a1
        move.l  _SysBase,a6
        sys     DoIO            ;Set new input port parameters.

* Set up the serial output port.
        lea     readreq,a0
        lea     writreq,a1
        move.w  #wsize-1,d0
copw    move.b  (a0)+,(a1)+     ;Clone the read request block.
        dbra    d0,copw
        clr.l   -(sp)
        move.l  #1$,-(sp)
        jsr     _CreatePort     ;wpport = CreatePort ("Write_RS", NULL);
        addq.l  #8,sp
        move.l  d0,wpport
        move.l  d0,w_ReplyPort
        bne.s   startup
        move.l  #2$,d1
        bsr     pstring
        bra     quitprg
1$      dc.b    'Write_RS',0
2$      dc.b    'Can''t create output port for serial.device!',cr,lf,'$'

startup bsr     setbaud         ;Set parameters and start reading.
        rts

*
* Return TRUE (D0 <> 0) if the serial port has a character.
*
checkio lea     readreq,a1
        move.l  _SysBase,a6
        sys     CheckIO
        rts

*
* Set the serial port's baud rate, number of data bits, etc.
*
setbaud tas     frstset         ;Is this the first call?
        beq.s   1$              ;Yes - input port is set up.
        lea     readreq,a1
        move.l  _SysBase,a6
        sys     AbortIO         ;Abort the outstanding read.
        move.l  baud,r_Baud     ;Baud rate
        move.b  bits,r_ReadLen  ;Number of bits per character
        move.b  bits,r_WriteLen
        move.w  #SDCMD_SETPARAMS,r_Command
        lea     readreq,a1
        move.l  _SysBase,a6
        sys     DoIO            ;Set new input port parameters.
1$      move.w  #CMD_READ,r_Command
        move.l  #1,r_Length
        move.l  #charinb,r_Data
        lea     readreq,a1
        move.l  _SysBase,a6
        sys     SendIO          ;Start reading again.
* Now set up the output port - this one is more straightforward.
        move.l  baud,w_Baud
        move.b  bits,w_ReadLen
        move.b  bits,w_WriteLen
        move.w  #SDCMD_SETPARAMS,w_Command
        lea     writreq,a1
        move.l  _SysBase,a6
        sys     DoIO
        rts

*
* Write the byte in "charout" to the serial port.
*
serwrit move.w  #CMD_WRITE,w_Command
        move.l  #1,w_Length
        move.l  #charout,w_Data
        lea     writreq,a1
        move.l  _SysBase,a6
        sys     DoIO
        rts

*
* Read a byte from the serial port into "charin".
*  If a byte isn't ready, this routine will wait until one is.
*
serread lea     readreq,a1
        move.l  _SysBase,a6
        sys     WaitIO          ;Wait until a character is ready.
        move.b  charinb,charin  ;Get the character from the buffer.
        move.w  #CMD_READ,r_Command
        move.l  #1,r_Length
        move.l  #charinb,r_Data
        lea     readreq,a1
        move.l  _SysBase,a6
        sys     SendIO          ;Get ready for the next character.
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
        bra.s   3$
1$      swap    d0              ;Save nybble count.
        rol.l   #4,d1           ;Print variable in d1.
        move.l  d1,-(sp)
        and     #$F,d1          ;Isolate the current nybble.
        cmp     #$A,d1
        bcs.s   2$
        add.b   #'A'-'9'-1,d1   ;Adjust for digits A through F.
2$      add.b   #'0',d1         ;Convert to ASCII.
        move.b  d1,(a0)+        ;Add to the result string.
        move.l  (sp)+,d1
3$      swap    d0              ;Get nybble count.
        dbra    d0,1$
        rts

pchar   move.b  d1,workbuf      ;Print the character in D1.
        move.b  #'$',workbuf+1
        move.l  #workbuf,d1
        bsr     pstring
        rts

pspace  move.l  #1$,d1          ;Print a space.
        bsr     pstring
        rts
1$      dc.b    ' $'

pcrlf   move.l  #1$,d1          ;Print a carriage return and line feed.
        bsr     pstring
        rts
1$      dc.b    cr,lf,'$'

*
* Convert the hex string pointed to by A0 to long in d1.
*  Stops on the first invalid hex digit, which is returned in d0.
*  A0 is left pointing to this first invalid digit.
*  d2 = 1 if any valid digits were found, 0 otherwise.
*
atol    moveq   #0,d1
        moveq   #0,d2
1$      move.b  (a0)+,d0        ;Get the current byte.
        cmpi.b  #$60,d0
        bcs.s   2$
        andi.b  #$5F,d0         ;Mask to upper case.
2$      cmpi.b  #'0',d0         ;Check range (0..9,A..F).
        bcs.s   atolend
        cmpi.b  #'F',d0
        bhi.s   atolend
        cmpi.b  #'9',d0
        bls.s   3$
        cmpi.b  #'A',d0
        bcs.s   atolend
3$      moveq   #1,d2           ;Valid characters entered, set flag.
        sub.b   #'0',d0         ;Convert to binary
        cmpi.b  #$9,d0          ;Digit in range 0..9?
        bls.s   4$              ;Yes - conversion is complete
        sub.b   #'A'-'9'-1,d0   ;Adjust digits A..F.
4$      ext     d0              ;Convert to long.
        ext.l   d0
        asl.l   #4,d1           ;Tack it onto d1.
        add.l   d0,d1
        bra.s   1$              ;Try for another digit.
atolend subq.l  #1,a0           ;Back onto the first invalid digit.
        rts
        page
*************************************************************************
*                                                                       *
*       Instruction mnemonic table (used for tracing)                   *
*                                                                       *
*************************************************************************

        data    data

* This table contains the mnemonic strings for the 8080/Z-80 opcodes.
*
* "q" denotes a register number in bits 3 through 5 of the opcode.
*       Values are interpreted as follows:
*               Normal 8080     Normal Z-80     DD prefix       FD prefix
*       000             B               B               B               B
*       001             C               C               C               C
*       010             D               D               D               D
*       011             E               E               E               E
*       100             H               H               XH              YH
*       101             L               L               XL              YL
*       110             M            (HL)           (IX+n)          (IY+n)
*       111             A               A               A               A
*
* "r" denotes a register number in bits 0 through 2 of the opcode.
*       Values are interpreted the same as for "q" above.
*
* "p" denotes a 2-bit register pair number in bits 4 and 5 of the opcode.
*       Values are interpreted as follows:
*               8080    Z-80
*       00      B       BC
*       01      D       DE
*       10      H       HL      (no DD or FD prefix)
*       10      IX      IX      (with DD prefix)
*       10      IX      IX      (with FD prefix)
*       11      SP      SP      (if opcode is below F0)
*       11      PSW     AF      (if opcode is F0 or greater)
*
* "h" is replaced by IX or IY if the opcode prefix is DD or FD respectively.
*  If the instruction is not prefixed, "h" is replaced by HL.
*
* "n" denotes an 8-bit number following the opcode.
*
* "a" denotes a 16-bit address following the opcode.

* Mnemonics for 8080 opcodes 00 through 3F
mnop008:
        dc.b    'NOP$        LXI  p,a$   STAX p$     INX  p$     ' ;00-03
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   RLC$        ' ;04-07
        dc.b    'EXAF$       DAD  p$     LDAX p$     DCX  p$     ' ;08-0B
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   RRC$        ' ;0C-0F
        dc.b    'DJNZ n$     LXI  p,a$   STAX p$     INX  p$     ' ;10-13
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   RAL$        ' ;14-17
        dc.b    'JR   n$     DAD  p$     LDAX p$     DCX  p$     ' ;18-1B
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   RAR$        ' ;1C-1F
        dc.b    'JRNZ n$     LXI  p,a$   ShD a$      INX  p$     ' ;20-23
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   DAA$        ' ;24-27
        dc.b    'JRZ  n$     DAD  p$     LhD a$      DCX  p$     ' ;28-2B
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   CMA$        ' ;2C-2F
        dc.b    'JRNC n$     LXI  p,a$   STA  a$     INX  p$     ' ;30-33
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   STC$        ' ;34-37
        dc.b    'JRC  n$     DAD  p$     LDA  a$     DCX  p$     ' ;38-3B
        dc.b    'INR  q$     DCR  q$     MVI  q,n$   CMC$        ' ;3C-3F

* Mnemonics for Z-80 opcodes 00 through 3F
mnop00z:
        dc.b    'NOP$        LD   p,a$   LD   (p),A$ INC  p$     ' ;00-03
        dc.b    'INC  q$     DEC  q$     LD   q,n$   RLCA$       ' ;04-07
        dc.b    'EX   AF,AF$ ADD  HL,p$  LD   A,(p)$ DEC  p$     ' ;08-0B
        dc.b    'INC  q$     DEC  q$     LD   q,n$   RRCA$       ' ;0C-0F
        dc.b    'DJNZ n$     LD   p,a$   LD   (p),A$ INC  p$     ' ;10-13
        dc.b    'INC  q$     DEC  q$     LD   q,n$   RLA$        ' ;14-17
        dc.b    'JR   n$     ADD  HL,p$  LD   A,(p)$ DEC  p$     ' ;18-1B
        dc.b    'INC  q$     DEC  q$     LD   q,n$   RRA$        ' ;1C-1F
        dc.b    'JR   NZ,n$  LD   p,a$   LD   (a),HL$INC  p$     ' ;20-23
        dc.b    'INC  q$     DEC  q$     LD   q,n$   DAA$        ' ;24-27
        dc.b    'JR   Z,n$   ADD  HL,p$  LD   HL,(a)$DEC  p$     ' ;28-2B
        dc.b    'INC  q$     DEC  q$     LD   q,n$   CPL$        ' ;2C-2F
        dc.b    'JR   NC,n$  LD   p,a$   LD   (a),A$ INC  p$     ' ;30-33
        dc.b    'INC  q$     DEC  q$     LD   q,n$   SCF$        ' ;34-37
        dc.b    'JR   C,n$   ADD  HL,p$  LD   A,(a)$ DEC  p$     ' ;38-3B
        dc.b    'INC  q$     DEC  q$     LD   q,n$   CCF$        ' ;3C-3F

* Mnemonics for opcodes 40 through 7f are easy - 76 is HLT
*  (HALT for Z-80), and all others are MOV (LD for Z-80).
mnop408 dc.b    'MOV  q,r$'
mnop40z dc.b    'LD   q,r$'
mnop768 dc.b    'HLT$'
mnop76z dc.b    'HALT$'

* Mnemonics for 8080 opcodes 80 through BF
mnop808:
        dc.b    'ADD  r$  ADC  r$  SUB  r$  SBB  r$  '  ;80-9F
        dc.b    'ANA  r$  XRA  r$  ORA  r$  CMP  r$  '  ;A0-BF

* Mnemonics for Z-80 opcodes 80 through BF
mnop80z:
        dc.b    'ADD  A,r$ADC  A,r$SUB  r$  SBC  A,r$'  ;80-9F
        dc.b    'AND  r$  XOR  r$  OR   r$  CP   r$  '  ;A0-BF

* Mnemonics for 8080 opcodes C0 through FF
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopC08:
        dc.b    'RNZ$        POP  p$     JNZ  a$     JMP  a$     ' ;C0-C3
        dc.b    'CNZ  a$     PUSH p$     ADI  n$     RST  0$     ' ;C4-C7
        dc.b    'RZ$         RET$        JZ   a$     ILLEGAL$    ' ;C8-CB
        dc.b    'CZ   a$     CALL a$     ACI  n$     RST  1$     ' ;CC-CF
        dc.b    'RNC$        POP  p$     JNC  a$     OUT  n$     ' ;D0-D3
        dc.b    'CNC  a$     PUSH p$     SUI  n$     RST  2$     ' ;D4-D7
        dc.b    'RC$         EXX$        JC   a$     IN   n$     ' ;D8-DB
        dc.b    'CC   a$     ILLEGAL$    SBI  n$     RST  3$     ' ;DC-DF
        dc.b    'RPO$        POP  p$     JPO  a$     XTh$        ' ;E0-E3
        dc.b    'CPO  a$     PUSH p$     ANI  n$     RST  4$     ' ;E4-E7
        dc.b    'RPE$        PCh$        JPE  a$     XCHG$       ' ;E8-EB
        dc.b    'CPE  a$     ILLEGAL$    XRI  n$     RST  5$     ' ;EC-FF
        dc.b    'RP$         POP  p$     JP   a$     DI$         ' ;F0-F3
        dc.b    'CP   a$     PUSH p$     ORI  n$     RST  6$     ' ;F4-F7
        dc.b    'RM$         SPh$        JM   a$     EI$         ' ;F8-FB
        dc.b    'CM   a$     ILLEGAL$    CPI  n$     RST  7$     ' ;FC-FF

* Mnemonics for Z-80 opcodes C0 through FF
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopC0z:
        dc.b    'RET  NZ$    LD   p,(SP)$JP   NZ,a$  JP   a$     ' ;C0-C3
        dc.b    'CALL NZ,a$  LD   (SP),p$ADD  A,n$   RST  0$     ' ;C4-C7
        dc.b    'RET  Z$     RET$        JP   Z,a$   ILLEGAL$    ' ;C8-CB
        dc.b    'CALL Z,a$   CALL a$     ADC  A,n$   RST  8$     ' ;CC-CF
        dc.b    'RET  NC$    LD   p,(SP)$JP   NC,a$  OUT  n,A$   ' ;D0-D3
        dc.b    'CALL NC,a$  LD   (SP),p$SUB  A,n$   RST  10$    ' ;D4-D7
        dc.b    'RET  C$     EXX$        JP   C,a$   IN   A,n$   ' ;D8-DB
        dc.b    'CALL C,a$   ILLEGAL$    SBC  A,n$   RST  18$    ' ;DC-DF
        dc.b    'RET  PO$    LD   p,(SP)$JP   PO,a$  EX   (SP),p$' ;E0-E3
        dc.b    'CALL PO,a$  LD   (SP),p$AND  A,n$   RST  20$    ' ;E4-E7
        dc.b    'RET  PE$    JP   (p)$   JP   PE,a$  EX   DE,p$  ' ;E8-EB
        dc.b    'CALL PE,a$  ILLEGAL$    XOR  A,n$   RST  28$    ' ;EC-FF
        dc.b    'RET  P$     LD   p,(SP)$JP   P,a$   DI$         ' ;F0-F3
        dc.b    'CALL P,a$   LD   (SP),p$OR   A,n$   RST  30$    ' ;F4-F7
        dc.b    'RET  M$     LD   SP,h$  JP   M,a$   EI$         ' ;F8-FB
        dc.b    'CALL M,a$   ILLEGAL$    CP   A,n$   RST  38$    ' ;FC-FF

* Mnemonics for opcodes CB00 through CB3F -
*  these are the same for both the 8080 and the Z-80.
mnopCB08:
mnopCB0z:
        dc.b    'RLC  r$ RRC  r$ RL   r$ RR   r$ '      ;CB00-CB1F
        dc.b    'SLA  r$ SRA  r$ ILLEGAL$SRL  r$ '      ;CB20-CB3F

* Mnemonics for opcodes CB40 through CBFF -
*  these are the same for both the 8080 and the Z-80.
mnopCB48:
mnopCB4z:
        dc.b    'BIT  $  RES  $  SET  $  '

* Mnemonics for 8080 opcodes ED40 through ED7F
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopE48:
        dc.b    'IN   q,(C)$ OUT  (C),q$ DSBB p$     SBCD a$     ' ;ED40-ED43
        dc.b    'NEG$        RETN$       IM0$        MOV  I,A$   ' ;ED44-ED47
        dc.b    'IN   q,(C)$ OUT  (C),q$ DADC p$     LBCD a$     ' ;ED48-ED4B
        dc.b    'ILLEGAL$    RETI$       ILLEGAL$    MOV  R,A$   ' ;ED4C-ED4F
        dc.b    'IN   q,(C)$ OUT  (C),q$ DSBB p$     SDED a$     ' ;ED50-ED53
        dc.b    'ILLEGAL$    ILLEGAL$    IM1$        MOV  A,I$   ' ;ED54-ED57
        dc.b    'IN   q,(C)$ OUT  (C),q$ DADC p$     LDED a$     ' ;ED58-ED5B
        dc.b    'ILLEGAL$    ILLEGAL$    IM2$        MOV  A,R$   ' ;ED5C-ED5F
        dc.b    'IN   q,(C)$ OUT  (C),q$ DSBB p$     SHLD a$     ' ;ED60-ED63
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    RRD$        ' ;ED64-ED67
        dc.b    'IN   q,(C)$ OUT  (C),q$ DADC p$     LHLD a$     ' ;ED68-ED6B
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    RLD$        ' ;ED6C-ED6F
        dc.b    'IN   q,(C)$ OUT  (C),q$ DSBB p$     SSPD a$     ' ;ED70-ED73
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED74-ED77
        dc.b    'IN   q,(C)$ OUT  (C),q$ DADC p$     LSPD a$     ' ;ED78-ED7B
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED7C-ED7F

* Mnemonics for Z-80 opcodes ED40 through ED7F
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopE4z:
        dc.b    'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED40-ED43
        dc.b    'NEG$        RETN$       IM   0$     LD   I,A$   ' ;ED44-ED47
        dc.b    'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED48-ED4B
        dc.b    'ILLEGAL$    RETI$       ILLEGAL$    LD   R,A$   ' ;ED4C-ED4F
        dc.b    'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED50-ED53
        dc.b    'ILLEGAL$    ILLEGAL$    IM   1$     LD   A,I$   ' ;ED54-ED57
        dc.b    'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED58-ED5B
        dc.b    'ILLEGAL$    ILLEGAL$    IM   2$     LD   A,R$   ' ;ED5C-ED5F
        dc.b    'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED60-ED63
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    RRD$        ' ;ED64-ED67
        dc.b    'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED68-ED6B
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    RLD$        ' ;ED6C-ED6F
        dc.b    'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED70-ED73
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED74-ED77
        dc.b    'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED78-ED7B
        dc.b    'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED7C-ED7F

* Mnemonics for miscellaneous ED-prefix instructions -
*  these are the same for both the 8080 and the Z-80.
mnopEA8:
mnopEAz:
        dc.b    'LDI$ CPI$ INI$ OUTI$'  ;EDA0-EDA3
        dc.b    '                    '  ;EDA4-EDA7 (illegal)
        dc.b    'LDD$ CPD$ IND$ OUTD$'  ;EDA8-EDAB
        dc.b    '                    '  ;EDAC-EDAF (illegal)
        dc.b    'LDIR$CPIR$INIR$OTIR$'  ;EDB0-EDB3
        dc.b    '                    '  ;EDB4-EDB7 (illegal)
        dc.b    'LDDR$CPDR$INDR$OTDR$'  ;EDB8-EDBB

* Mnemonic for illegal instructions
mnopilg dc.b    'ILLEGAL$'
        page
*************************************************************************
*                                                                       *
*       Fake FDOS                                                       *
*                                                                       *
*************************************************************************

*
* Fake BDOS for target system
*
fdos    dc.b    tHLT,0,tRET     ;BIOS jump table
        dc.b    tJMP,$33,$FF    ;Warm boot
        dc.b    tJMP,$36,$FF    ;Console status
        dc.b    tJMP,$39,$FF    ;Console input
        dc.b    tJMP,$3C,$FF    ;Console output
        dc.b    tJMP,$3F,$FF    ;List output
        dc.b    tJMP,$42,$FF    ;Punch output
        dc.b    tJMP,$45,$FF    ;Reader input
        dc.b    tJMP,$48,$FF    ;Home disk
        dc.b    tJMP,$4B,$FF    ;Select disk
        dc.b    tJMP,$4E,$FF    ;Set track
        dc.b    tJMP,$51,$FF    ;Set sector
        dc.b    tJMP,$54,$FF    ;Set DMA address
        dc.b    tJMP,$57,$FF    ;Read
        dc.b    tJMP,$5A,$FF    ;Write
        dc.b    tJMP,$5D,$FF    ;Get list device status
        dc.b    tJMP,$60,$FF    ;Sector translation

*
* Fake BIOS for target system
*
        dc.b    tHLT,1,tRET     ;Warm boot
        dc.b    tHLT,2,tRET     ;Console status
        dc.b    tHLT,3,tRET     ;Console input
        dc.b    tHLT,4,tRET     ;Console output
        dc.b    tHLT,5,tRET     ;List output
        dc.b    tHLT,6,tRET     ;Punch output
        dc.b    tHLT,7,tRET     ;Reader input
        dc.b    tHLT,8,tRET     ;Home disk *
        dc.b    tHLT,9,tRET     ;Select disk *
        dc.b    tHLT,10,tRET    ;Set track *
        dc.b    tHLT,11,tRET    ;Set sector *
        dc.b    tHLT,12,tRET    ;Set DMA address *
        dc.b    tHLT,13,tRET    ;Read *
        dc.b    tHLT,14,tRET    ;Write *
        dc.b    tHLT,15,tRET    ;Get list device status *
        dc.b    tHLT,16,tRET    ;Sector translation *

*
* Fake Disk Parameter Block
*
fakedpb dc.b    11,0            ;SPT (sectors per track)
        dc.b    4               ;BSH (block shift to record number)
        dc.b    15              ;BLM (block number mask to record no.)
        dc.b    0               ;EXM (logical->physical extent shift)
        dc.b    439&255,439/256 ;DSM (highest allocation block number)
        dc.b    255,0           ;DRM (highest directory entry number)
        dc.b    $F0,0           ;AL0, AL1 (initial allocation vector)
        dc.b    64,0            ;CKS (size of directory check area)
        dc.b    0,0             ;OFF (offset, number of reserved tracks)

*
* Fake Disk Block Allocation Table
*
fakealv dcb.b   21,$FF
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

null    dc.b    0               ;Null string
        page
*************************************************************************
*                                                                       *
*       Variable storage                                                *
*                                                                       *
*************************************************************************

        bss     bss

* File information block - must be on a 4-byte boundary!
fib:
fibkey  ds.l    1
fibtype ds.l    1       ;Type (file if negative, directory if positive)
fibname ds.b    108     ;File name (null-terminated)
fibprot ds.l    1       ;Protection mask
fibent  ds.l    1
fibsize ds.l    1       ;Number of bytes in file
fibblks ds.l    1       ;Number of blocks in file
fibdays ds.l    1       ;Date stamp - number of days since Jan. 1, 1978
fibmins ds.l    1       ;Date stamp - number of minutes past midnight
fibtick ds.l    1       ;Date stamp - number of ticks past minute
fibcmt  ds.b    116     ;Comments (null-terminated)

* InfoData structure
InfoData:
id_NumSoftErrors ds.l   1       ;number of soft errors on disk
id_UnitNumber    ds.l   1       ;Which unit disk is (was) mounted on
id_DiskState     ds.l   1       ;See defines below
id_NumBlocks     ds.l   1       ;Number of blocks on disk
id_NumBlocksUsed ds.l   1       ;Number of block in use
id_BytesPerBlock ds.l   1
id_DiskType      ds.l   1       ;Disk Type code
id_VolumeNode    ds.l   1       ;BCPL pointer to volume node
id_InUse         ds.l   1       ;Flag, zero if not in use
id_SIZEOF        equ    36

* Miscellaneous storage areas
savesp  ds.l    1               ;Stack pointer save area
_SysBase ds.l   1               ;Copy of _AbsExecBase
_DOSBase ds.l   1               ;Pointer to dos.library
stdin   ds.l    1               ;Keyboard handle (stdin)
stdout  ds.l    1               ;Screen handle (stdout)
rawhand ds.l    1               ;RAW: file handle
prthand ds.l    1               ;PRT:RAW file handle
handles ds.l    8*4             ;File handle (or zero) plus 12 bytes of FCB
handlen:                        ;End of file handle table
dmaaddr ds.l    1               ;Current DMA address
comend  ds.l    1               ;End of .COM file name on command line
dtstamp ds.l    3               ;Date and time stamp
rpport  ds.l    1               ;Serial input message port
wpport  ds.l    1               ;Serial output message port
baud    ds.l    1               ;New baud rate for "setbaud"
bits    ds.b    1               ;Number of data bits
charin  ds.b    1               ;Current input character
charinb ds.b    1               ;Serial input buffer
charout ds.b    1               ;Current output character
frstset ds.b    1               ;$80 after first call to "setbaud"
cmdline ds.b    128             ;Command line
cmdlinen:                       ;End of command line
comname ds.b    20              ;Name of file to load
comnamen:                       ;End of file name
opnname ds.b    24              ;File name for OPEN or RENAME
renname ds.b    24              ;New file name for RENAME
srchnam ds.b    11              ;CP/M file name for search first/next
ext17   ds.w    1               ;Extent counter for search first/next
newrega ds.b    1               ;BIOS/BDOS accumulator work area
workbuf ds.b    80              ;Work buffer for "pstring" (including $)
workbufn:                       ;End of work buffer
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
traceit ds.b    1               ;-t (trace) flag was set on command line
btrcflg ds.b    1               ;Trace BIOS/BDOS calls.
bufflag ds.b    1               ;Console output is buffered.
z80flag ds.b    1               ;Display Z-80 mnemonics in instruction trace.
opcode  ds.b    1               ;Current opcode (used for tracing)
prefix  ds.b    1               ;Instruction prefix (DD or FD)
fcbptr  ds.l    1               ;Pointer to current FCB
listopn ds.b    1               ;The list device is open.
builtin ds.b    1               ;1 = USER command, 2 = SAVE command
acmap   ds.w    1               ;Active drive map
romap   ds.w    1               ;Read-only map
newdmap ds.w    1               ;Map bit from "mapdrv"

*
* Serial port read request
*
        ds.l    0
readreq         ;struct IOExtSer
                ;struct IOStdReq
                ;struct Message
        ds.b    14      ;struct Node
r_ReplyPort     ds.l 1  ;Pointer to MsgPort (message reply port)
r_MLength ds.w  1       ;Message length in bytes
                ; End of struct Message
        ds.l    1       ;Pointer to device node
        ds.l    1       ;Pointer to Unit (driver private)
r_Command ds.w  1       ;Device command
        ds.b    1       ;io_Flags
        ds.b    1       ;Error or warning number
                ; End of struct IOReq - IOStdReq continues...
r_Actual ds.l   1       ;Actual number of bytes transferred
r_Length ds.l   1       ;Requested number of bytes transferred
r_Data  ds.l    1       ;Points to data area.
r_Offset ds.l   1       ;Offset for block-structured devices
                ; End of struct IOStdReq
r_CtlChar ds.l  1       ;control char's (order = xON,xOFF,INQ,ACK)
r_RBufLen ds.l  1       ;length in bytes of serial port's read buffer
r_ExtFlags ds.l 1       ;additional serial flags (see bitdefs below)
r_Baud  ds.l    1       ;baud rate requested (true baud)
        ds.l    1       ;duration of break signal in MICROseconds
                ;struct IOTArray termination character array
r_ReadLen ds.b  1       ;bits per read character (# of bits)
r_WriteLen ds.b 1       ;bits per write character (# of bits)
r_StopBits ds.b 1       ;stopbits for read (# of bits)
r_SerFlags ds.b 1       ;see SerFlags bit definitions below
r_Status ds.w   1       ;status of serial port
rsize   equ     *-readreq

*
* Serial port write request
*
        ds.l    0
writreq         ;struct IOExtSer
                ;struct IOStdReq
                ;struct Message
        ds.b    14      ;struct Node
w_ReplyPort     ds.l 1  ;Pointer to MsgPort (message reply port)
w_MLength ds.w  1       ;Message length in bytes
                ; End of struct Message
        ds.l    1       ;Pointer to device node
        ds.l    1       ;Pointer to Unit (driver private)
w_Command ds.w  1       ;Device command
        ds.b    1       ;io_Flags
        ds.b    1       ;Error or warning number
                ; End of struct IOReq - IOStdReq continues...
w_Actual ds.l   1       ;Actual number of bytes transferred
w_Length ds.l   1       ;Requested number of bytes transferred
w_Data  ds.l    1       ;Points to data area.
w_Offset ds.l   1       ;Offset for block-structured devices
                ; End of struct IOStdReq
w_CtlChar ds.l  1       ;control char's (order = xON,xOFF,INQ,ACK)
w_RBufLen ds.l  1       ;length in bytes of serial port's read buffer
w_ExtFlags ds.l 1       ;additional serial flags (see bitdefs below)
w_Baud  ds.l    1       ;baud rate requested (true baud)
        ds.l    1       ;duration of break signal in MICROseconds
                ;struct IOTArray termination character array
w_ReadLen ds.b  1       ;bits per read character (# of bits)
w_WriteLen ds.b 1       ;bits per write character (# of bits)
w_StopBits ds.b 1       ;stopbits for read (# of bits)
w_SerFlags ds.b 1       ;see SerFlags bit definitions below
w_Status ds.w   1       ;status of serial port, as follows:
wsize   equ     *-writreq
*                  BIT  ACTIVE  FUNCTION
*                   0    low    busy
*                   1    low    paper out
*                   2    low    select
*                   3    low    Data Set Ready
*                   4    low    Clear To Send
*                   5    low    Carrier Detect
*                   6    low    Ready To Send
*                   7    low    Data Terminal Ready
*                   8    high   read overrun
*                   9    high   break sent
*                  10    high   break received
*                  11    high   transmit x-OFFed       
*                  12    high   receive x-OFFed       
*               13-15           reserved

*************************************************************************
*                                                                       *
*       Target processor's address space                                *
*                                                                       *
*************************************************************************

        even

registers ds.b  22              ;Actual storage for Z-80's other registers
target  ds.b    $10000          ;Z-80's universe

        end
