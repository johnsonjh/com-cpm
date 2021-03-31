vermaj	 equ	$02	;Major version number
vermin	 equ	$30	;Minor version number
revyear  equ	$89	;Year last assembled
revmonth equ	$01	;Month last assembled
revday	 equ	$09	;Day last assembled
*************************************************************************
*									*
*									*
*	Z-80 Simulator for MC68000					*
*									*
*	With CP/M 2.2 call support, and optional tracing		*
*									*
*									*
* Amiga version 2.3 (December 1988, Charlie Gibbs):			*
*									*
*	Incorporated modifications by Willi Kusche			*
*	(his version 2.1, June 29, 1988):				*
*		Register definitions split into "ecpmdefs.i"		*
*		Improved macro usage					*
*		Correction of errors in Z-80 flag setting routines	*
*		Incorporated modified trace dump routines		*
*		Allow changing of trace addresses during execution	*
*		Added the following BDOS calls:				*
*			27 - get allocation vector			*
*				(using a fake allocation vector)	*
*			31 - get disk parameter block			*
*				(using a fake disk parameter block)	*
*									*
*	Modifications inspired by Ulf Nordquist's CP/M emulator:	*
*	      - BDOS calls 17 and 18 (search for first file and		*
*		search for next file) now put file size information	*
*		into the FCB.						*
*	      - BDOS call 27 (get allocation vector) now builds a fake	*
*		allocation vector based on actual volume information.	*
*									*
*	The file handle table now also contains the first 12 bytes	*
*	of the FCB to which the handle corresponds.  The FCB is		*
*	no longer modified with a sequence number.			*
*									*
*	BDOS call 20 (sequential read) now returns 01h in the		*
*	accumulator when end of file is reached, rather than 0FFh	*
*	as is suggested by CP/M manuals.  Some utilities (such as	*
*	ASM.COM) assume 0FFh indicates an I/O error.			*
*									*
*	A new INCLUDE file, options.i, has been added.  This file	*
*	contains EQUate statements which allow you to set options	*
*	that cannot be easily set at execution time.  Current options	*
*	allow inclusion of Heath/Zenith 19 escape code translation,	*
*	and support of 68010 and higher processors by replacing all	*
*	MOVE SR instructions by MOVE CCR.				*
*									*
*									*
* Amiga version 2.2 (June 1988, Charlie Gibbs): 			*
*									*
*	BDOS call 23 (rename) was not working properly. 		*
*									*
*	BIOS and BDOS call messages (for tracing or errors)		*
*	now indicate that the call number is in hexadecimal.		*
*									*
*	The following BDOS calls have now been implemented:		*
*		35 - get file end address				*
*		36 - get direct address 				*
*									*
*									*
* Amiga version 2.1 (May 1988, Charlie Gibbs):				*
*									*
*	If a reference is made to any drive other than A:, SimCPM	*
*	will insert the string CPMx: ahead of any file name it uses.	*
*	For instance, if a program tries to open MYFILE.DAT on drive	*
*	B:, SimCPM will try to open CPMB:MYFILE.DAT.  You can ASSIGN	*
*	these simulated drives anywhere you want.  Drive A: will	*
*	always go to the current directory, unless the user number	*
*	is not zero.  If you specify any user number other than zero,	*
*	it will be included, e.g. if the above example were opened	*
*	under user 1, SimCPM will look for CPMB01:MYFILE.DAT.		*
*	If the file were on drive A under user 1, SimCPM will look	*
*	for CPMA01:MYFILE.DAT.						*
*									*
*	If the file handle table overflowed, the file which caused	*
*	the overflow has already been opened.  It was not being 	*
*	closed again.  This file is now closed before the emulated	*
*	program is aborted.						*
*									*
*	If an emulated program failed to close all the files it opened, *
*	the emulator would close these files.  However, it did not	*
*	clear the open flag for any such files.  If another program	*
*	was then run, when it terminated the emulator would try to	*
*	close these same files again.  This bug has been corrected.	*
*									*
*	Z-80 instructions not supported by version 2.0 have been	*
*	implemented as follows: 					*
*		LD I,A moves to a dummy interrupt register.		*
*		LD A,I moves from a dummy interrupt register.		*
*		LD R,A is ignored.					*
*		LD A,R moves a random value from the clock into 	*
*			the low-order 7 bits of the accumulator.	*
*		IM1 and IM2 are ignored.				*
*									*
*	The -z command-line switch has been implemented.		*
*	It sets instruction tracing, just like the -t switch,		*
*	but causes Z-80 instruction mnemonics to be used.		*
*	If this switch is not set, 8080 mnemonics are used.		*
*									*
*	BDOS call 32 (get or set user code) is now fully implemented.	*
*									*
*	The following BDOS calls have now been implemented:		*
*		13 - reset all drives					*
*		24 - get active drive map				*
*		28 - protect drive					*
*		29 - get read-only drive map				*
*									*
*	The USER and SAVE built-in commands have been implemented.	*
*	These are the only build-in commands provided, since commands	*
*	such as DIR or TYPE can be handled through a CLI window.	*
*									*
*									*
* Amiga version 2.0 (January 1988, Charlie Gibbs):			*
*									*
*	Added all Z-80 instructions except the following:		*
*		LD A,I							*
*		LD I,A							*
*		LD A,R							*
*		LD R,A							*
*		IM1							*
*		IM2							*
*									*
*	Added -t flag for tracing without needing			*
*		a separate version to maintain speed.			*
*									*
*	Added the following BDOS calls: 				*
*		17 - search for first file				*
*		18 - search for next file				*
*		24 - get active drive map				*
*			(assumes only drive A: is active)		*
*		28 - protect drive (ignored)				*
*		29 - get read-only map					*
*			(assumes no drives are protected)		*
*		32 - get or set user code				*
*			(always gets user code zero, any attempt	*
*			to set to a user code other than zero		*
*			results in a fatal error)			*
*									*
*	Added serial port support (simulated 8251 on ports 14 and 15)	*
*									*
*									*
*	Converted to AmigaDOS September 1987 by Charlie Gibbs		*
*	(after painstakingly typing it all in from Dr. Dobbs		*
*	Journal, January through March 1986).  Improvements		*
*	described by Jim Cathey in his letter in the June 1986		*
*	DDJ have been included.  Repetitive code is generated		*
*	by macros whenever it would save my fingers.			*
*									*
*									*
*	Version 1.2 1/21/85 JEC 					*
*		Fixed Extent bug in OPEN logic. 			*
*		Sped up code, sample MAC from 2:13 to 1:40		*
*		Now runs at a 1.4 MHz equivalent based on MAC sample.	*
*									*
*	Version 1.1 8/29/84 JEC 					*
*		Fixed BIOS call #6 bug. 				*
*									*
*	Version 1.0 05/25/84 by Jim Cathey				*
*									*
*	This program has been written for speed whenever possible,	*
*	as such tends to be large because of the separate subroutine	*
*	for each and every opcode of the target processor.		*
*									*
*	On an 8MHz 68000 (Compupro) system the simulation speed is	*
*	a little better than a 1MHz Z-80 when running MAC.  The time	*
*	for a sample assembly was 2:13 for the simulation vs. 0:35	*
*	on a 4MHz Z-80, both systems used identical hard disk systems.	*
*									*
*	It is not a complete simulation, as some flag handling		*
*	isn't quite right, but it is enough to run the program		*
*	I wrote for it (DDT, LU, MAC, and Morrow's FORMATW).		*
*									*
*************************************************************************
	code
	page
*************************************************************************
*									*
*	This file contains the startup routines, the simulator core,	*
*	tracing code, and the CP/M 2.2 simulation.			*
*									*
*************************************************************************

	xref	optabl,flags,mloop,mloopt,tracesad,traceead,traceflg,start2
	xdef	illegl,service,dump,inp,outp,movear

	xref	_AbsExecBase
	xref	_CreatePort
	xref	_DeletePort
	xdef	_SysBase
	xdef	_DOSBase

	include	"options.i"
	include	"ecpmdefs.i"

*
* ASCII character values
*
bel	equ	$07		;Bell (or beep or whatever)
bs	equ	$08		;Backspace
ht	equ	$09		;Horizontal tab
lf	equ	$0A		;Line feed
ff	equ	$0C		;Form feed
cr	equ	$0D		;Carriage return
so	equ	$0E		;Shift out
si	equ	$0F		;Shift in
esc	equ	$1B		;Escape
	page
*--------------------------------
*
* Some commonly used macros
*
*--------------------------------

sys	macro			;Call a system routine.
	ifnd	_LVO\1
	xref	_LVO\1
	endc
	jsr	_LVO\1(a6)
	endm

*----------------------------
* Target system Mnemonics
*----------------------------
tHLT	EQU    $76
tJMP	EQU    $C3
tRET	EQU    $C9

*----------------------
* Equates
*----------------------

MODE_OLDFILE equ 1005
MODE_NEWFILE equ 1006
ACCESS_READ  equ -2
ACCESS_WRITE equ -1
STARTBAUD equ	1200		;Starting baud rate
NUMBITS equ	8		;Number of data bits
CTLCHAR equ	$11130501	;XON, XOFF, ENQ, ACK
SERFLAGS equ	$A0		;No XON/XOFF, shared
PARITYON equ	1		;Parity enabled
PARITYODD equ	2		;Odd parity
* I/O command codes
CMD_INVALID equ 0
CMD_RESET   equ 1
CMD_READ    equ 2
CMD_WRITE   equ 3
CMD_UPDATE  equ 4
CMD_CLEAR   equ 5
CMD_STOP    equ 6
CMD_START   equ 7
CMD_FLUSH   equ 8
CMD_NONSTD  equ 9
SDCMD_QUERY equ CMD_NONSTD
SDCMD_BREAK equ CMD_NONSTD+1
SDCMD_SETPARAMS equ CMD_NONSTD+2 ;Set serial port parameters
	page
*************************************************************************
*									*
*	Initialization							*
*									*
*************************************************************************

start:	move.l	sp,savesp	;Save the stack pointer.
	move.l	_AbsExecBase,a6
	move.l	a6,_SysBase	;Working copy of _AbsExecBase
	move.b	#1,testdol	;"pstring" should test for leading $.
	clr.w	esclen		;No partial escape sequence is saved.
	clr.b	insflag 	;We're not in insert mode.
	clr.b	traceit 	;Clear trace request flag.
	clr.b	z80flag 	;Clear Z-80 flag.
	clr.b	btrcflg 	;Turn off BIOS/BDOS call tracing.
	clr.b	quitflg 	;Clear the quit flag.
	clr.b	bufflag 	;Disable output buffering.
	clr.b	listopn 	;The list device is closed.
	clr.b	frstset 	;First call to "setbaud"
	clr.b	target+4	;Set default drive and user to A0:.
	move.w	#1,acmap	;Set active drive map to A: only.
	clr.w	romap		;No drives are set read-only.
	move.l	#STARTBAUD,baud ;Initial serial port parameters
	move.b	#NUMBITS,bits
	move.l	#strbuf,strptr	;Initialize output buffer pointer.

	lea	handles,a1
	move.w	#(handlen-handles)/4-1,d1
1$	clr.l	(a1)+		;Clear file handles.
	dbra	d1,1$
	clr.l	rawhand 	;Clear RAW: handle too.

*
* Copy the command line to "cmdline", stripping out leading switches if any.
*
	lea	cmdline,a1
	subq	#1,d0
* Skip over leading blanks, if any.
leadblk cmpi.b	#' ',(a0)+	;Leading blank?
	bne.s	setbuf		;No.
	dbra	d0,leadblk	;Skip over leading blank.
*  If the -b switch is given, activate output buffering.
setbuf	subq.l	#1,a0		;Back onto the first non-blank.
	cmpi.b	#'-',(a0)	;Possible buffer switch?
	bne.s	savecmd 	;No - start saving the command line.
	cmpi.b	#'B',1(a0)	;Activate output buffering?
	beq.s	1$		;Yes.
	cmpi.b	#'b',1(a0)
	bne.s	settrc		;No.
1$	move.b	#1,bufflag	;Set buffered-output flag.
	bra.s	skipsw
* If the -t switch is given, set "traceit".
settrc	cmpi.b	#'T',1(a0)
	beq.s	1$
	cmpi.b	#'t',1(a0)
	bne.s	setz80
1$	move.b	#1,traceit	;Set trace flag.
* If the -z switch is given, set both "z80flag" and "traceit".
setz80	cmpi.b	#'Z',1(a0)
	beq.s	1$
	cmpi.b	#'z',1(a0)
	bne.s	skipsw
1$	move.b	#1,z80flag	;Set Z-80 flag.
	move.b	#1,traceit	;Set trace flag also.
* Skip over the switch and check for more.
skipsw	cmpi.b	#' ',(a0)+	;End of switch?
	beq.s	1$		;Yes.
	dbra	d0,skipsw
	addq.l	#1,a1		;Adjust A1.
	bra.s	gotcmd		;There is no command line left.
1$	subq.l	#1,a0		;Back onto the first blank.
	bra.s	leadblk 	;Look for the next non-blank.
* Save the command line (except for leading switches).
savecmd move.b	(a0)+,(a1)+	;Save the command line, if any.
	dbra	d0,savecmd
gotcmd	move.b	#0,-1(a1)	;Replace the newline with a null.
	move.b	cmdline,cmdflag ;Save command-line flag.

*
* Open libraries and set up a RAW: window.
*
	move.b	#1,quitflg	;Quit immediately if failure below.
	move.l	_SysBase,a6	;Find library
	lea	dosname,a1	;'dos.library'
	moveq	#0,d0		;Any version
	sys	OpenLibrary	;Open dos.library.
	move.l	d0,a6		;Point to doslib for next operation.
	move.l	d0,_DOSBase	;Save it for future reference.
	sys	Input		;Get file handle for keyboard.
	move.l	d0,stdin	;Save it here.
	beq	quitprg 	;Couldn't get keyboard handle.
	sys	Output		;Get file handle for screen.
	move.l	d0,stdout	;Save it here.
	beq	quitprg 	;Couldn't get screen handle.
	move.l	#loadmsg,d1
	bsr	pstring 	;Display where we loaded.
	move.l	#start,d1
	bsr	plong
	bsr	pspace
	move.b	#'/',d1
	bsr	pchar
	bsr	pspace
	move.l	#start2,d1
	bsr	plong
	bsr	pcrlf
	move.l	#rawspec,d1
	move.l	#MODE_NEWFILE,d2
	sys	Open		;Open a RAW: window.
	move.l	d0,rawhand	;Save the file handle here.
	bne	opened		;We succeeded.
	move.l	#rawerr,d1
	bsr	pstring 	;Display error message...
	sys	IoErr
	move.l	d0,d1
	bsr	plong		; and error code.
	bsr	pcrlf
	bra	quitprg

dosname dc.b	'dos.library',0
loadmsg dc.b	'SimCPM version '
	dc.b	vermaj&15+'0','.',vermin/16+'0',vermin&15+'0'
	dc.b	' has loaded at $'
rawspec dc.b	'RAW:0/0/640/200/CP/M Emulator'
	dc.b	' by Jim Cathey and Charlie Gibbs - version '
	dc.b	vermaj&15+'0','.'
	dc.b	vermin/16+'0',vermin&15+'0',' ('
	dc.b	revyear/16+'0',revyear&15+'0','/'
	dc.b	revmonth/16+'0',revmonth&15+'0','/'
	dc.b	revday/16+'0',revday&15+'0',')',0
rawerr	dc.b	'Unable to open RAW: - code $'
setwin	dc.b	$9B,'0x',$9B,'8y',$9B,'24t',$9B,'80u',$9B,'H',$9B,'J$'

opened	move.b	cmdflag,quitflg ;If we have a command, execute it and quit.
	move.l	#setwin,d1
	bsr	pstring 	;Set the window to 24 by 80.

*
* Come back here to load another program.
*
nextprg lea	target,targbase ;Start of target memory
	clr.b	insflag 	;Reset insert mode.
	move.l	#simsg,d1
	bsr	pstring 	;In case last program sent SHIFT OUT
	clr.b	dumpcnt 	;Reset dump pause counter.
	clr.l	rpport		;serial.device message port pointers
	clr.l	wpport
	bsr	entrads 	;Enter trace delimiting addresses.
	bsr	lodfdos 	;Load up the fake FDOS in target memory.
	bsr	lodregs 	;Load the remaining simulation registers.
	bsr	loadcom 	;Load the .COM program.
	jmp	(return)	;Execute the simulation.

simsg	dc.b	si,'$'		;Resets MSB of each output character
	page
*************************************************************************
*									*
*	Illegal instructions and dumping				*
*									*
*************************************************************************

illegl	move.l	#illgmsg,d1	;Illegal opcode, say what & where,
	bsr	pstring
	lea	-1(pseudopc),a0
	move.b	(a0),d1
	bsr	pbyte
	cmpi.b	#$CB,(a0)	;Display sub-op-code for prefixes CB...
	beq.s	1$
	cmpi.b	#$DD,(a0)	;DD...
	beq.s	1$
	cmpi.b	#$ED,(a0)	;ED...
	beq.s	1$
	cmpi.b	#$FD,(a0)	;and FD.
	bne.s	2$
1$	move.b	1(a0),d1
	bsr	pbyte
2$	move.l	#ilgmsg2,d1
	bsr	pstring
	suba.l	targbase,a0
	move.l	a0,d1
	bsr	pword
	move.l	#ilgmsg3,d1
	bsr	pstring
	move.l	#dumpmsg,d1
	bsr	pstring
	clr.b	dumpcnt
	bsr	dump		; and spill guts.
	bra	quitprg 	;Quit simulation.

illgmsg dc.b	cr,lf,'Illegal instruction $'
ilgmsg2 dc.b	' at $'
ilgmsg3 dc.b	'.$'
dumpmsg dc.b	cr,lf,'Register contents:$'

*
* Dump all registers and decode the current instruction.
*
dump	movem.l	d0-d3/a1,-(sp)
	bsr	pcrlf
	move.b	regf,d0
	bsr	dspflag
	move.b	#'A',(a0)+
	move.b	#'=',(a0)+
	move.b	rega,d1		;Accumulator
	bsr	ubyte
	move.b	#' ',(a0)+
	move.b	#'B',(a0)+
	move.b	#'=',(a0)+
	move.w	regb(regs),d1	;BC
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'D',(a0)+
	move.b	#'=',(a0)+
	move.w	regd(regs),d1	;DE
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'H',(a0)+
	move.b	#'=',(a0)+
	move.w	regh(regs),d1	;HL
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'S',(a0)+
	move.b	#'=',(a0)+
	move.l	pseudosp,d1	;SP
	sub.l	targbase,d1
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'P',(a0)+
	move.b	#'=',(a0)+
	move.l	pseudopc,d1	;PC
	sub.l	targbase,d1
	bsr	uword
	move.b	#' ',(a0)+
	move.l	a1,-(sp)
	move.l	pseudosp,a1
	moveq	#3,d2
	moveq	#'0',d3
tosloop	move.b	#'S',(a0)+	;Display the top 4 stack entries.
	move.b	d3,(a0)+
	addq.b	#1,d3
	move.b	#'=',(a0)+
	move.b	1(a1),d1 
	ror.w	#8,d1
	move.b	0(a1),d1
	bsr	uword
	move.b	#' ',(a0)+
	addq.l	#2,a1
	dbra	d2,tosloop
	move.b	#'$',(a0)+
	move.l	(sp)+,a1
	move.l	#workbuf,d1
	bsr	pstring 	;Displaying as a single string is much faster.
	bsr	pcrlf
	move.b	regf2(regs),d0	;Alternate flags
	bsr	dspflag
	move.b	#'A',(a0)+
	move.b	#'''',(a0)+
	move.b	rega2(regs),d1	;Alternate accumulator
	bsr	ubyte
	move.b	#' ',(a0)+
	move.b	#'B',(a0)+
	move.b	#'''',(a0)+
	move.w	regb2(regs),d1	;Alternate BC
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'D',(a0)+
	move.b	#'''',(a0)+
	move.w	regd2(regs),d1	;Alternate DE
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'H',(a0)+
	move.b	#'''',(a0)+
	move.w	regh2(regs),d1	;Alternate HL
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'X',(a0)+
	move.b	#'=',(a0)+
	move.w	regix(regs),d1	;IX
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#'Y',(a0)+
	move.b	#'=',(a0)+
	move.w	regiy(regs),d1	;IY
	bsr	uword
	move.b	#' ',(a0)+
	move.b	#' ',(a0)+
	move.b	(pseudopc),d1	;Current opcode byte
	cmpi.b	#$CB,d1 	;Prefix?
	beq.s	oppfx		;Yes.
	cmpi.b	#$DD,d1
	beq.s	oppfx
	cmpi.b	#$ED,d1
	beq.s	oppfx
	cmpi.b	#$FD,d1
	beq.s	oppfx
	move.b	#' ',(a0)+
	bsr	ubyte		;Unprefixed opcode
	move.b	#' ',(a0)+
	bra.s	opx
oppfx	bsr	ubyte		;Prefix
	move.b	1(pseudopc),d1
	bsr	ubyte		;Prefixed opcode
opx	move.b	#' ',(a0)+
*
* Decode the instruction.
*
	moveq	#0,d0
	move.b	(pseudopc),d0	;Opcode
	clr.b	prefix		;Assume it's not a prefix.
	cmpi.b	#$DD,d0 	;Is it?
	beq.s	dopfx		;Yes.
	cmpi.b	#$FD,d0
	bne.s	saveop		;No.
dopfx	move.b	d0,prefix	;It's a prefix.
	move.b	1(pseudopc),d0	;The opcode is really here.
saveop	move.b	d0,opcode	;Save the opcode.
* Look up the opcode.
	cmpi.b	#$40,d0 	;Is opcode in range 00-3F?
	bcc.s	lookupC 	;No.
* The opcode is in the range 00-3F.
	lea	mnop008,a1	;Assume we're using 8080 mnemonics.
	tst.b	z80flag 	;Should we use Z-80 mnemonics?
	beq.s	lokup0c 	;No
	lea	mnop00z,a1
lokup0c mulu	#12,d0		;Offset into opcode table
	add.l	d0,a1		;A1 points to decoded instruction
	bra	decode		;Decode remainder of instruction.
* The opcode is in the range C0-FF.
lookupC cmpi.b	#$C0,d0 	;Is opcode in range C0-FF?
	bcs.s	lookup8 	;No.
	cmpi.b	#$ED,d0 	;ED-prefix instructions?
	beq	lookupE 	;Yes.
	cmpi.b	#$CB,d0 	;CB-prefix instructions?
	beq	lookupB 	;Yes.
	lea	mnopC08,a1	;Assume we're using 8080 mnemonics.
	tst.b	z80flag 	;Should we use Z-80 mnemonics?
	beq.s	lokupCc 	;No.
	lea	mnopC0z,a1
lokupCc subi.w	#$C0,d0
	mulu	#12,d0		;Offset into opcode table
	add.l	d0,a1		;A1 points to decoded instruction
	bra	decode		;Decode remainder of instruction.
* The opcode is in the range 80-BF.
lookup8 cmpi.b	#$80,d0 	;Is opcode in range 80-BF?
	bcs.s	lookup4 	;No - it's in range 40-7F.
	lea	mnop808,a1	;Assume we're using 8080 mnemonics.
	tst.b	z80flag 	;Are we?
	beq.s	lokup8c 	;Yes.
	lea	mnop80z,a1	;Use the Z-80 mnemonic table.
lokup8c lsr.b	#3,d0		;Divide opcode by 8.
	andi.w	#7,d0		;Instruction type
	mulu	#9,d0		;Offset into opcode table
	add.l	d0,a1		;A1 points to decoded instruction
	bra	decode		;Decode remainder of instruction.
* The opcode is in the range 40-7F.
lookup4 cmpi.b	#$76,d0 	;Is this a HLT (Z-80 HALT) instruction?
	bne.s	lokup4m 	;No.
	lea	mnop768,a1
	tst.b	z80flag 	;Do the Z-80 mnemonic?
	beq	decode		;No.
	lea	mnop76z,a1
	bra	decode
lokup4m lea	mnop408,a1	;Assume 8080 MOV instruction.
	tst.b	z80flag 	;Are we doing Z-80 mnemonics?
	beq	decode		;No.
	lea	mnop40z,a1	;Make it a Z-80 LD instruction.
	bra	decode
* CB-prefix instruction decoding
lookupB move.b	1(pseudopc),d0	;Sub-opcode
	tst.b	prefix		;IX or IY with displacement?
	beq.s	lokupB0 	;No.
	move.b	2(pseudopc),d0	;Skip over the displacement.
lokupB0 move.b	d0,opcode
	cmpi.b	#$40,d0 	;Is opcode in range CB00-CB3F?
	bcc.s	lokupB4 	;No.
	lea	mnopCB08,a1
	lsr.b	#3,d0
	mulu	#8,d0
	add.l	d0,a1
	bra	decode
lokupB4 move.b	d0,d1		;Save the sub-opcode.
	andi.w	#$C0,d0 	;Opcode is in range CB40-CBFF.
	lsr.b	#3,d0		;Displacement into 8-byte entries
	lea	mnopCB48,a1
	add.l	d0,a1
lokupBm move.b	(a1)+,(a0)+	;Move mnemonic to decoded area.
	cmpi.b	#'$',(a1)
	bne.s	lokupBm
	move.b	d1,d0		;Get the sub-opcode again.
	andi.w	#$38,d0 	;Isolate the bit number.
	lsr.b	#3,d0
	addi.b	#'0',d0 	;Convert the bit number to ASCII.
	move.b	d0,(a0)+	;Move bit number to decoded instruction.
	move.b	#',',(a0)+
	move.b	d1,d0
	andi.b	#7,d0		;Isolate the register specification.
	bsr	dspreg		;Set up the register number.
	bra	dispop
* ED-prefix instruction decoding
lookupE move.b	1(pseudopc),d0	;Sub-opcode
	move.b	d0,opcode
	cmpi.b	#$40,d0 	;Is it below ED40?
	bcs	lookupI 	;Yes - it's illegal.
	cmpi.b	#$80,d0 	;Is it in range ED40-ED7F?
	bcc	lokupE8 	;No.
	lea	mnopE48,a1	;Assume we're using 8080 mnemonics.
	tst.b	z80flag 	;Should we use Z-80 mnemonics?
	beq.s	lokupEc 	;No
	lea	mnopE4z,a1
lokupEc andi.w	#$3F,d0
	mulu	#12,d0		;Offset into opcode table
	add.l	d0,a1		;A1 points to decoded instruction
	bra	decode		;Decode remainder of instruction.
lokupE8 cmpi.b	#$A0,d0 	;Is opcode in range ED80-ED9F?
	bcs	lookupI 	;Yes - it's illegal.
	cmpi.b	#$BC,d0 	;It must be in range EDA0-EDBB.
	bcc	lookupI
	lea	mnopEA8,a1
	andi.w	#$1F,d0
	mulu	#5,d0
	add.l	d0,a1
	cmpi.b	#' ',(a1)	;Is opcode blank?
	bne.s	decode		;No - decode the instruction.
* The instruction is illegal.
lookupI lea	mnopilg,a1	;Point to "ILLEGAL" message.
*
* Decode the instruction according to the string pointed to by A1.
*  The decoded instruction is built where A0 points.
*  See the mnemonic tables for an explanation of operand codes.
*
decode:
* Code "r" - register in bits 0-2 of opcode
	cmpi.b	#'r',(a1)
	bne.s	decodeq
	move.b	opcode,d0
	bsr	dspreg
	bra	decodex
* Code "q" - register in bits 3-5 of opcode
decodeq cmpi.b	#'q',(a1)
	bne.s	decodep
	move.b	opcode,d0
	lsr.b	#3,d0
	bsr	dspreg
	bra	decodex
* Code "p" - register pair in bits 4-5 of opcode
decodep cmpi.b	#'p',(a1)
	bne	decodeh
	move.b	opcode,d0
	andi.b	#$30,d0 	;Isolate the register bits.
	bne.s	decodpd 	;They aren't 00.
	move.b	#'B',(a0)+	;00 - register B
	tst.b	z80flag 	;Are we using Z-80 mnemonics?
	beq	decodex 	;No.
	move.b	#'C',(a0)+	;Call it BC for Z-80.
	bra	decodex
decodpd cmpi.b	#$20,d0
	beq.s	decodph
	bhi.s	decodps
	move.b	#'D',(a0)+	;01 - register D (DE for Z-80)
	tst.b	z80flag
	beq	decodex
	move.b	#'E',(a0)+
	bra	decodex
decodph cmpi.b	#$DD,prefix	;10 - check for index prefix.
	beq.s	decodpx 	;IX
	bhi.s	decodpy 	;IY
	move.b	#'H',(a0)+	;Register H (HL for Z-80)
	tst.b	z80flag
	beq	decodex
	move.b	#'L',(a0)+
	bra	decodex
decodpx move.b	#'I',(a0)+	;IX
	move.b	#'X',(a0)+
	bra	decodex
decodpy move.b	#'I',(a0)+	;IY
	move.b	#'Y',(a0)+
	bra	decodex
decodps cmpi.b	#$F0,opcode	;11 - depends on opcode
	bcc.s	decodpp
	move.b	#'S',(a0)+	;11 is SP if opcode is less than F0.
	move.b	#'P',(a0)+
	bra	decodex
decodpp tst.b	z80flag 	;Otherwise, it's PSW (AF for Z-80).
	bne.s	decodpz
	move.b	#'P',(a0)+
	move.b	#'S',(a0)+
	move.b	#'W',(a0)+
	bra.s	decodex
decodpz move.b	#'A',(a0)+
	move.b	#'F',(a0)+
	bra.s	decodex
* Code "h" - HL, IX, or IY depending on prefix
decodeh cmpi.b	#'h',(a1)
	bne.s	decoden
	cmpi.b	#$DD,prefix
	beq.s	decodpx 	;IX
	bhi.s	decodpy 	;IY
	move.b	#'H',(a0)+	;HL
	move.b	#'L',(a0)+
	bra	decodex
* Code "n" - 8-bit value following opcode
decoden cmpi.b	#'n',(a1)
	bne.s	decodea
	move.b	1(pseudopc),d1
	tst.b	prefix		;DD or FD prefix?
	beq.s	1$		;No - we have the value.
	cmpi.b	#$36,opcode	;Is opcode below 36?
	bcs.s	1$		;Yes - it's a move to index register half
	move.b	3(pseudopc),d1	;The value is actually here.
1$	bsr	ubyte		;Convert the value to a hex string.
	bra.s	decodex
* Code "a" - 16-bit value following opcode
decodea cmpi.b	#'a',(a1)
	bne.s	decodem
	move.b	2(pseudopc),d1
	lsl.w	#8,d1
	move.b	1(pseudopc),d1
	cmpi.b	#$ED,(pseudopc) ;Is this an ED-prefix instruction?
	beq.s	1$		;Yes - the value is shifted one byte.
	tst.b	prefix		;DD or FD prefix?
	beq.s	2$		;No - we have the value.
1$	move.b	3(pseudopc),d1	;The value is actually here.
	lsl.w	#8,d1
	move.b	2(pseudopc),d1
2$	bsr	uword		;Convert the value to a hex string.
	bra.s	decodex
* Not a special code - just move the character as is.
decodem move.b	(a1),(a0)+
* Try for another character to move (and possibly decode).
decodex addq.l	#1,a1
	cmpi.b	#'$',(a1)
	bne	decode
*
* Display the decoded instruction.
*
dispop	move.b	#cr,(a0)+
	move.b	#lf,(a0)+
	move.b	#'$',(a0)+
	move.l	#workbuf,d1
	bsr	pstring 	;Displaying as a single string is much faster.
* Pause after every screenful to give the operator time to read it.
	addq.b	#1,dumpcnt	;Count the number of times dumped.
	cmpi.b	#8,dumpcnt	;Is the screen full of dumps?
	bcs	dumpx		;No - exit.
	move.l	#dmpmsg3,d1
	bsr	pstring 	;Ask for operator action.
	bsr	dmpstr		;Make sure the prompt gets out!
	movem.l a0-a1/a6,-(sp)
	move.l	rawhand,d1	;Console input
	move.l	#dumpcnt,d2
	move.l	_DOSBase,a6
	moveq	#1,d3
	sys	Read		;Get the operator's reply.
	movem.l (sp)+,a0-a1/a6
	bsr	pcrlf
	cmpi.b	#'Q',dumpcnt	;Does he want to quit?
	beq	quitprg 	;Yes.
	cmpi.b	#'q',dumpcnt
	beq	quitprg
	cmpi.b	#'S',dumpcnt	;Stop tracing?
	beq.s	2$		;Yes.
	cmpi.b	#'s',dumpcnt
	beq.s	2$
	cmpi.b	#'C',dumpcnt	;Change trace address?
	beq.s	1$		;Yes.
	cmpi.b	#'c',dumpcnt
	bne.s	dmpcont
1$	bsr	gtrange		;Get new trace range.
2$	clr.b	traceflg	;Disable tracing and continue.
dmpcont	clr.b	dumpcnt 	;Reset the dump counter.
dumpx	movem.l (sp)+,d0-d3/a1
	rts

dmpmsg3 dc.b	'Q to quit, S to stop tracing, C to change trace,'
	dc.b	' any other key to continue: $'

*
* Display the register number indicated by the low-order 3 bits of D0.
*  H and L will be displayed as XH and XL if necessary.
*  M ((HL) for Z-80) will be converted to (IX+d) or (IY+d) if necessary.  
*
dspreg	andi.b	#7,d0		;Isolate register number.
	cmpi.b	#6,d0		;Which register is it?
	beq.s	dspregm 	;M - might need a special routine.
	bhi.s	dsprega 	;A - this one is easy.
	addi.b	#'B',d0 	;Registers B, C, D, E, H, and L
	cmpi.b	#'F',d0 	;Is it H or L?
	bcs.s	dspregb 	;No - we're ready.
	bne.s	dspregl 	;It's L.
	moveq	#'H',d0 	;It's H.
	bra.s	dspregp
dspregl moveq	#'L',d0 	;It's L.
dspregp cmpi.b	#$DD,prefix	;Are we manipulating an index register?
	bcs.s	dspregb 	;No.
	bne.s	dspregy 	;Register YH or YL
	move.b	#'X',(a0)+	;Register XH or XL
	bra.s	dspregb
dspregy move.b	#'Y',(a0)+
dspregb move.b	d0,(a0)+
	rts
dsprega move.b	#'A',(a0)+	;It's the accumulator.
	rts
dspregm cmpi.b	#$DD,prefix	;Index register?
	bcc.s	dspregi 	;Yes.
	tst.b	z80flag 	;Are we using 8080 mnemonics?
	bne.s	dspregz 	;No.
	move.b	#'M',(a0)+	;It's M for 8080.
	rts
dspregz move.b	#'(',(a0)+	;It's (HL) for Z-80.
	move.b	#'H',(a0)+
	move.b	#'L',(a0)+
	move.b	#')',(a0)+
	rts
dspregi move.b	#'(',(a0)+	;It's an indexed operand.
	move.b	#'I',(a0)+
	move.b	#'X',(a0)+	;Assume it's IX.
	cmpi.b	#$DD,prefix	;Is it?
	beq.s	dspregd 	;Yes.
	move.b	#'Y',-1(a0)	;It's IY.
dspregd move.b	#'+',(a0)+
	move.b	2(pseudopc),d1	;Displacement
	bsr	ubyte
	move.b	#')',(a0)+
	rts

*
* Display the contents of the flag register in D0.
*
dspflag	lea	workbuf,a0
	move.b	#'-',d1		;Carry (assume not set)
	btst	#0,d0
	beq	1$
	move.b	#'C',d1		;Carry flag is set.
1$	move.b	d1,(a0)+
	move.b	#'-',d1		;Zero
	btst	#6,d0
	beq	2$
	move.b	#'Z',d1
2$	move.b	d1,(a0)+
	move.b	#'-',d1		;Minus
	btst	#7,d0
	beq	3$
	move.b	#'M',d1
3$	move.b	d1,(a0)+
	move.b	#'-',d1		;Even parity
	btst	#2,d0
	beq	4$
	move.b	#'E',d1
4$	move.b	d1,(a0)+
	move.b	#'-',d1		;Intermediate carry
	btst	#4,d0
	beq	5$
	move.b	#'I',d1
5$	move.b	d1,(a0)+
	move.b	#' ',(a0)+
	rts
	page
*************************************************************************
*									*
*	Initialization subroutines					*
*									*
*************************************************************************

*
* Load up the fake FDOS.
*
lodfdos move.l	a6,-(sp)
	lea	fdos,a6
	move.l	targbase,pseudosp
	adda.l	#$10000,pseudosp
	lea	-256(pseudosp),a0
	move.w	#fdoslen,d0
1$	move.b	(a6)+,(a0)+
	dbra	d0,1$
	lea	-256(pseudosp),a0
	move.l	a0,d0
	sub.l	targbase,d0
	move.b	#tJMP,0(targbase)	;Build BIOS and BDOS jumps.
	move.b	#tJMP,5(targbase)
	move.b	d0,6(targbase)
	rol.w	#8,d0
	move.b	d0,7(targbase)
	rol.w	#8,d0
	addq.w	#3,d0
	move.b	d0,1(targbase)
	rol.w	#8,d0
	move.b	d0,2(targbase)
	clr.w	-(pseudosp)	;Set up a return stack to exit simulation.
	move.l	(sp)+,a6
	rts

*
* Set up working registers.
*
lodregs lea	optabl,opptr	;Point base reg. to opcode dispatch table.
	lea	mloop,return
	tst.b	traceit 	;Is tracing required?
	beq.s	1$		;No.
	lea	mloopt,return	;Point return to trace test.
1$	lea	flags,flagptr
	lea	$100(targbase),pseudopc ;Start execution at 0100H.
	moveq	#$E,regcon0e		;Set up quick constants.
	moveq	#$1,regcon01
	moveq	#$F,regcon0f
	move.l	#$FF,regconff
	moveq	#0,rega
	moveq	#0,regf
	clr.b	regi(regs)
	rts
	page
*
* Get start and end addresses for tracing.
*
entrads tst.b	traceit 	;Is tracing required?
	beq	entradx 	;No.
	bsr	gtrange 	;Get trace range.
* Find out whether BIOS/BDOS calls are to be traced.
	move.l	#btrcmsg,d1
	bsr	pstring
	lea	workbuf,a0
	move.b	#10,(a0)
	bsr	getline
	move.b	#1,btrcflg
	cmpi.b	#'Y',workbuf+2
	beq.s	entradx
	cmpi.b	#'y',workbuf+2
	beq.s	entradx
	clr.b	btrcflg
entradx clr.b	traceflg	;Start with tracing turned off.
	rts

gtrange move.l	#tracemsg,d1	;Enter trace address if necessary.
	bsr	pstring
	lea	workbuf,a0
	move.b	#workbufn-workbuf-2,(a0)
	bsr	getline 	;Get the string.
	moveq	#0,d0
	move.b	1(a0),d0	;Number of bytes read
	addq.l	#2,a0		;Skip over length information.
	clr.b	0(a0,d0)	;Insert string terminator.
	bsr	atol		;Get trace start address.
	andi.l	#$FFFF,d1
	add.l	#target,d1
	move.l	d1,tracesad
* Now get the ending address.
	move.l	#tracemg2,d1
	bsr	pstring
	lea	workbuf,a0
	move.b	#workbufn-workbuf-2,(a0)
	bsr	getline
	moveq	#0,d0
	move.b	1(a0),d0
	addq.l	#2,a0
	clr.b	0(a0,d0)
	bsr	atol
	andi.l	#$FFFF,d1
	add.l	#target,d1
	move.l	d1,traceead
	bsr	pcrlf
	rts

tracemsg dc.b	cr,lf,'Start trace at >$'
tracemg2 dc.b	'  End trace at >$'
btrcmsg dc.b	'Trace BIOS/BDOS calls? >$'

*
* Open the file to be loaded, and load it into target space if successful.
*
loadcom movem.l d1-d3/a1-a2/a6,-(sp)	;Save registers.
	lea	cmdline,a0
	tst.b	cmdflag 	;Do we have a command already?
	bne.s	scancmd 	;Yes - process it.
* Display the command prompt.
prompt	lea	target,targbase ;Just in case "targbase" gets clobbered
	move.b	4(targbase),d1	;Get the default drive code.
	andi.b	#$0F,d1
	addi.b	#'A',d1 	;Convert drive code to ASCII letter.
	bsr	pchar		;Display the current drive.
	move.b	4(targbase),d1	;Get user number.
	lsr.b	#4,d1		;Move it to low-order 4 bits.
	beq.s	promptp 	;Don't insert user number if it's zero.
	cmpi.b	#10,d1		;Is user number 10 or greater?
	bcs.s	promptu 	;No.
	move.b	d1,-(sp)
	move.b	#'1',d1
	bsr	pchar		;Display tens digit of user number.
	move.b	(sp)+,d1
	subi.b	#10,d1
promptu addi.b	#'0',d1 	;Convert user number to ASCII.
	bsr	pchar		;Display user number.
promptp move.b	#'>',d1
	bsr	pchar
	lea	cmdline,a0
	move.b	#cmdlinen-cmdline-2,(a0)
	bsr	getline 	;Get a command line.
	moveq	#0,d0
	move.b	1(a0),d0	;Length of command line
	beq.s	prompt		;Zero - try again.
	addq.l	#2,a0		;Skip over length information.
	clr.b	0(a0,d0)	;Insert command line terminator.
	cmpi.b	#3,(a0) 	;Control-C?
	bne.s	scancmd 	;No.
	move.b	#1,quitflg	;Set quit flag.
	bra	quitprg 	;Exit the simulator.
scancmd clr.b	cmdflag 	;Ask for a new command next time.
	clr.b	builtin 	;Clear "built-in command" flag.
* Check for a change of drive number.
	tst.b	2(a0)		;Is the command two characters long?
	bne.s	convcmd 	;No - treat as a normal command.
	cmpi.b	#':',1(a0)	;Drive specifier?
	bne.s	convcmd 	;No.
	bsr	ucase		;Get the drive code.
	cmpi.b	#'A',d0 	;Is it valid?
	bcs.s	opensel 	;No.
	cmpi.b	#'P',d0
	bhi.s	opensel
	subq.b	#1,d0
	andi.b	#$0F,d0 	;New drive code
	andi.b	#$F0,4(targbase)
	or.b	d0,4(targbase)	;Insert into current drive slot.
	bra	prompt
opensel move.l	#1$,d1
	bsr	pstring 	;'BDOS Error on '
	bsr	ucase
	move.b	d0,d1
	bsr	pchar		;Drive letter
	move.l	#2$,d1
	bsr	pstring 	;': Select'
	bra	prompt		;Try again.

1$	dc.b	'BDOS Error on $'
2$	dc.b	': Select',cr,lf,'$'

* Convert the program file name to an AmigaDOS file name in "comname".
convcmd lea	comname,a2
	cmpi.b	#':',1(a0)	;Is there a drive specifier?
	bne.s	1$		;No.
	bsr	ucase		;Get the drive letter.
	cmpi.b	#'A',d0 	;Is it valid?
	bcs.s	opensel 	;No.
	cmpi.b	#'P',d0
	bhi.s	opensel
	addq.l	#1,a0		;Skip over the colon.
	bra.s	2$
1$	move.b	4(targbase),d0	;Current drive and user number
	beq.s	loadnam 	;Drive A:, user 0
	andi.b	#$0F,d0 	;Isolate current drive bits
	addi.b	#'A',d0 	;Convert to drive code.
2$	move.b	4(targbase),d1	;Current drive and user number
	lsr.b	#4,d1		;Isolate the user number.
	bne.s	3$		;Not zero - make a directory name.
	cmpi.b	#'A',d0 	;Are we loading from drive A:?
	beq.s	loadnam 	;Yes - use the current directory.
3$	move.b	#'C',(a2)+	;Convert to AmigaDOS device.
	move.b	#'P',(a2)+
	move.b	#'M',(a2)+
	move.b	d0,(a2)+	;Insert the drive letter.
	tst.b	d1		;User zero?
	beq.s	5$		;Yes - don't bother inserting it.
	move.b	#'0',(a2)+	;Assume user number is less than 10.
	cmpi.b	#10,d1		;Is user number 10 or greater?
	bcs.s	4$		;No.
	move.b	#'1',-1(a2)	;Change the first digit to 1.
	subi.b	#10,d1
4$	addi.b	#'0',d1 	;Convert user number to ASCII.
	move.b	d1,(a2)+	;Insert user number into file spec.
5$	move.b	#':',(a2)+	;Insert a colon.
loadnam move.l	#comnamen,d1	;End of name, allowing for .COM suffix
	subq.l	#6,d1		;Allow for .COM suffix.
	sub.l	a2,d1		;Adjust for directory prefix, if any.
1$	bsr	ucase		;Convert file name to upper case.
	move.b	d0,(a2)+
	cmpi.b	#' ',(a0)	;End of name?
	beq.s	gotname 	;Yes.
	tst.b	(a0)		;End of command string?
	dbeq	d1,1$		;No - keep on going.
gotname move.l	a0,comend	;Save position in command line.
	move.b	#'.',(a2)+	;Mash file name to .COM.
	move.b	#'C',(a2)+
	move.b	#'O',(a2)+
	move.b	#'M',(a2)+
	clr.b	(a2)
	clr.b	cmdline 	;Ask for a new command next time.
* If this is a USER or SAVE command, don't look for a .COM file -
*  we'll process these commands ourselves.
	lea	comname,a0	;Scan for possible device name.
	move.l	a0,a1		;Use A1 as a scan pointer - preserve A0.
1$	tst.b	(a1)		;End of command name?
	beq.s	2$		;Yes - there is no device name.
	cmpi.b	#':',(a1)+	;End of device name?
	bne.s	1$		;No - continue scanning.
	move.l	a1,a0		;Point A0 past the device name.
2$	cmpi.b	#'.',4(a0)	;Is the command name four characters long?
	bne	opencom 	;No.
	cmpi.b	#'U',(a0)	;Check for a USER command.
	bne.s	tstsave
	cmpi.b	#'S',1(a0)
	bne.s	tstsave
	cmpi.b	#'E',2(a0)
	bne.s	tstsave
	cmpi.b	#'R',3(a0)
	bne.s	tstsave
	move.b	#1,builtin	;Indicate we have a USER command.
	bra	loaded
tstsave cmpi.b	#'S',(a0)	;Check for a SAVE command.
	bne.s	opencom
	cmpi.b	#'A',1(a0)
	bne.s	opencom
	cmpi.b	#'V',2(a0)
	bne.s	opencom
	cmpi.b	#'E',3(a0)
	bne.s	opencom
	move.b	#2,builtin	;Indicate we have a SAVE command.
	bra	loaded
* Open the command file and load it if it exists.
opencom move.l	#comname,d1
	move.l	#MODE_OLDFILE,d2
	move.l	_DOSBase,a6
	sys	Open		;Open the file.
	tst.l	d0		;Did the open fail?
	bne.s	comopen 	;No.
	lea	comname,a0
openerr cmpi.b	#'.',(a0)+	;Find end of file name.
	bne.s	openerr
	move.b	#'?',-1(a0)
	move.b	#cr,(a0)+
	move.b	#lf,(a0)+
	move.b	#'$',(a0)
	move.l	#comname,d1
	bsr	pstring 	;Echo "name?"
	bra	prompt		;Try again.

comopen move.l	d0,-(sp)	;Save the file handle.
	move.l	d0,d1
	move.l	pseudopc,d2	;Start loading at $0100 in target.
	move.l	#65536-512,d3	;Maximum number of bytes to load
	move.l	_DOSBase,a6
	sys	Read		;Load the .COM file.
	move.l	(sp)+,d1
	move.l	_DOSBase,a6
	sys	Close		;Close the .COM file.

* The program has now been loaded (unless it's USER or SAVE).
loaded	movem.l (sp)+,d1-d3/a1-a2/a6	;Refresh registers.
	movem.l d1-d3/a1-a2/a6,-(sp)
	lea	$80(targbase),a0	;Set up target's base page.
	move.l	a0,dmaaddr

* Set up FCBs and command line tail.
	lea	$5C(targbase),a0
	lea	$6C(targbase),a2
	clr.b	(a0)+
	clr.b	(a2)+
	moveq	#10,d0
clrfcb	move.b	#' ',(a0)+	;Clear FCBs.
	move.b	#' ',(a2)+
	dbra	d0,clrfcb
	clr.b	$80(targbase)	;Clear the command line tail.

	move.l	comend,a0	;Restore position in command line.
fcb1	tst.b	(a0)		;End of command line?
	beq	loadusr 	;Yes.
	cmpi.b	#' ',(a0)+	;Skip over to first file name
	beq.s	fcb1
	subq.l	#1,a0		;Back onto start of file name.
	move.l	a0,-(sp)	;Save position on command line.
	lea	$81(targbase),a2;A2 loads the command line tail.
gettail move.b	(a0)+,(a2)+	;Copy the command tail.
	bne.s	gettail
	move.l	a2,d0
	lea	$82(targbase),a0;Don't count null terminator!
	sub.l	a0,d0
	move.b	d0,$80(targbase);Length of command line tail
	move.l	(sp)+,a0	;Go back to the first file name.

	lea	$5C(targbase),a2;Address of current FCB
getfcb	move.l	a2,fcbptr	;Save pointer to current FCB.
	cmpi.b	#':',1(a0)	;Is a drive specified?
	bne.s	1$		;No.
	move.b	(a0),(a2)	;Get drive letter.
	subi.b	#'A'-1,(a2)	;Convert to drive code.
	addq.l	#2,a0		;Skip over drive designator.
	tst.b	(a0)		;End of command line?
	beq.s	loadusr 	;Yes - we're done.
	cmpi.b	#' ',(a0)	;End of file name?
	beq.s	getfcbx 	;Yes.
1$	addq.l	#1,a2		;Start of file name in FCB
2$	move.b	(a0)+,(a2)+	;Copy file name to FCB.
3$	tst.b	(a0)		;End of command?
	beq.s	loadusr 	;Yes.
	cmpi.b	#' ',(a0)	;End of file name?
	beq.s	getfcbx 	;Yes.
	cmpi.b	#'.',(a0)	;Start of file name extension?
	bne.s	2$		;No - continue loading file name.
	move.l	fcbptr,a2	;Copy original pointer
	lea	9(a2),a2	;Skip over to extension field.
	addq.l	#1,a0		;Skip over the period.
	bra.s	3$
getfcbx tst.b	(a0)		;End of command line?
	beq.s	loadusr 	;Yes.
	cmpi.b	#' ',(a0)+	;Look for another file name.
	beq.s	getfcbx
	subq.l	#1,a0		;Back onto start of file name.
	move.l	fcbptr,d0
	lea	$5C(targbase),a2
	cmp.l	d0,a2		;Was this the first FCB?
	bne.s	loadusr 	;No - stop after two FCBs.
	lea	16(a2),a2	;Skip over to the next FCB.
	bra.s	getfcb		;Load the next FCB.

* If this is a USER or SAVE command, process it here.
*  These are the only built-in commands that SimCPM supports,
*  since the rest are just as easily done through the CLI.
*  The first operand is a USER number or the number of pages to SAVE.
loadusr tst.b	builtin 	;Is this a built-in command?
	beq	loadcmx 	;No - we're done.
	lea	$5C(targbase),a0 ;Scan the first FCB.
	moveq	#0,d0		;Build the number here.
	tst.b	(a0)+		;Is drive code omitted?
	bne.s	badunum 	;No - we don't have a valid number.
	cmpi.b	#' ',(a0)	;Is the number missing?
	beq.s	badunum 	;Yes - error.
	cmpi.b	#' ',3(a0)	;Is the number too long?
	bne.s	badunum 	;Yes - it's likely too big.
getuser cmpi.b	#'0',(a0)	;Is the current digit valid?
	bcs.s	badunum 	;No.
	cmpi.b	#'9',(a0)
	bhi.s	badunum
	mulu	#10,d0		;Shift previous digits, if any.
	move.b	(a0)+,d1	;Get the current digit.
	andi.w	#$0F,d1 	;Convert current digit to binary.
	add.w	d1,d0		;Accumulate total.
	cmpi.b	#' ',(a0)	;End of number?
	bne.s	getuser 	;No - try for another digit.
	bra.s	chkuser
badunum moveq	#-1,d0		;Invalid number
chkuser cmpi.b	#1,builtin	;Is this a USER command?
	bne.s	chksave 	;No.
	cmpi.w	#15,d0		;Is user number over 15?
	bls.s	setuser 	;No - it's valid.
	move.l	#badumsg,d1
	bsr	pstring 	;Display an error message and try again.
	bra	prompt
setuser andi.b	#$0F,4(targbase);Clear original user number.
	lsl.b	#4,d0		;Move new user bits into position.
	or.b	d0,4(targbase)	;Insert new user number.
	bra	prompt

badumsg dc.b	'Invalid user number',cr,lf,'$'

chksave tst.w	d0		;Attempt to save zero blocks?
	beq.s	badblks 	;Yes - error.
	cmpi.w	#255,d0 	;Is number of blocks to SAVE valid?
	bls.s	savefn		;Yes.
badblks move.l	#1$,d1
	bsr	pstring
	bra	prompt
1$	dc.b	'Number of pages must be from 1 to 255.',cr,lf,'$'
savefn	lea	$6C(targbase),a0;FCB for SAVE file name
	cmpi.b	#' ',1(a0)	;Is file name missing?
	bne.s	opensav 	;No.
	move.l	#1$,d1
	bsr	pstring
	bra	prompt
1$	dc.b	'File name is missing.',cr,lf,'$'
opensav lea	opnname,a1	;Build AmigaDOS file name here.
	move.l	a1,d1		;We'll need it here.
	move.l	d0,-(sp)	;Save number of blocks to save.
	bsr	convfn		;Make a file name.
	move.l	#MODE_NEWFILE,d2
	move.l	_DOSBase,a6
	sys	Open		;Open the file.
	tst.l	d0		;Did the open fail?
	bne.s	saveit		;No.
	move.l	(sp)+,d0	;Clean up the stack.
	move.l	#1$,d1
	bsr	pstring
	bra	prompt
1$	dc.b	'Unable to open file for SAVE.',cr,lf,'$'
saveit	move.l	d0,d1		;Handle for SAVE file
	lea	target+$100,a0	;"targbase" isn't intact right now.
	move.l	a0,d2		;Start of data to save
	move.l	(sp)+,d3	;Number of 256-byte pages to save
	asl.l	#8,d3		;Convert to number of bytes.
	move.l	d1,-(sp)	;Save the file handle.
	sys	Write		;Write the file.
	move.l	(sp)+,d1
	sys	Close		;Close the file.
	bra	prompt		;Successful completion

* We have successfully loaded a .COM file.
loadcmx movem.l (sp)+,d1-d3/a1-a2/a6	;Restore registers.
	rts			;Exit.

*
* Subroutine to get a character and convert it to upper case
*
ucase	move.b	(a0)+,d0
	cmpi.b	#'a',d0
	bcs.s	ucasex
	cmpi.b	#'z',d0
	bhi.s	ucasex
	subi.b	#'a'-'A',d0
ucasex	rts
	page
*************************************************************************
*									*
*	BDOS / BIOS service routines					*
*									*
*************************************************************************
service movem.l a1/a6,-(sp)
	move.b	rega,newrega	;Save Z-80 accumulator (D2)
	move.l	_DOSBase,a6	;Get dos.library pointer
* Decode the byte following the HLT instruction (BIOS call type).
	moveq	#0,d0		;Handle BIOS/BDOS service request
	move.b	(pseudopc)+,d0	; of form HLT DB opcode.
	cmp	#(biostabn-biostab)/4,d0
	blt.s	dobios		;Function number is within range.
badbios move.b	d0,-(sp)	;Flag illegal BIOS call
	move.l	#ilgbios,d1	; and spill guts.
	bsr	pstring
	move.b	(sp)+,d1
	bsr	pbyte
	move.l	#atmsg,d1
	bsr	pstring
	move.b	1(pseudosp),d1	;Address where called (top stack entry)
	ror.w	#8,d1
	move.b	0(pseudosp),d1
	bsr	pword
	bsr	pcrlf
	bsr	dump
	bra	quitprg

ilgbios dc.b	cr,lf,'Illegal BIOS call $'
biosmsg dc.b	'BIOS call $'
atmsg	dc.b	' (hex) at $'

dobios	move.l	d0,-(sp)	;Save BIOS function number.
	beq.s	biostrx 	;Zero - it's a BDOS call.
	tst.b	btrcflg 	;Trace BIOS calls?
	beq.s	biostrx 	;No.
	move.l	#biosmsg,d1
	bsr	pstring
	move.l	(sp),d1
	bsr	pbyte
	move.l	#atmsg,d1
	bsr	pstring
	move.b	1(pseudosp),d1	;Address where called (top stack entry)
	ror.w	#8,d1
	move.b	0(pseudosp),d1
	bsr	pword
	bsr	pcrlf
	move.l	(sp),d0
biostrx asl	#2,d0		;Multiply function number by 4.
	addi.l	#biostab,d0	;Point at address table entry.
	movea.l d0,a0
	movea.l (a0),a0 	;Point to appropriate service routine.
	move.l	(sp)+,d0	;Restore BIOS function number.
	jmp	(a0)		;Jump to the routine.
* If the BIOS code is zero, it's a BDOS call.
*  Decode register C using a similar routine to the BIOS decoding above.
bdosfn	moveq	#0,d0
	move.b	regc(regs),d0	;Get BDOS function number.
	cmp	#(bdostabn-bdostab)/4,d0
	blt.s	dobdos		;Function number is within range.
badbdos move.b	d0,-(sp)
	move.l	#ilgbdos,d1	;Illegal or unsupported BDOS call
	bsr	pstring
	move.b	(sp)+,d1
	bsr	pbyte
	move.l	#atmsg,d1
	bsr	pstring
	move.b	1(pseudosp),d1	;Address where called (top stack entry)
	ror.w	#8,d1
	move.b	0(pseudosp),d1
	bsr	pword
	bsr	pcrlf
	bsr	dump
	bra	quitprg

ilgbdos dc.b	cr,lf,'Illegal BDOS call $'
bdosmsg dc.b	'BDOS call $'

dobdos	move.l	d0,-(sp)	;Save BDOS function number.
	tst.b	btrcflg 	;Trace BDOS calls?
	beq.s	bdostrx 	;No.
	move.l	#bdosmsg,d1
	bsr	pstring
	move.l	(sp),d1
	bsr	pbyte
	move.l	#atmsg,d1
	bsr	pstring
	move.b	1(pseudosp),d1
	ror.w	#8,d1
	move.b	0(pseudosp),d1
	bsr	pword
	bsr	pcrlf
	move.l	(sp),d0
bdostrx cmpi.b	#10,d0		;BDOS function 10 or higher?
	bcs.s	bdosjmp 	;No.
	bsr	dmpstr		;Dump any outstanding console output.
	move.l	(sp),d0 	;Restore BDOS function number.
bdosjmp asl	#2,d0		;Multiply function number by 4.
	addi.l	#bdostab,d0	;Point at address table entry.
	movea.l d0,a0
	movea.l (a0),a0 	;Point to appropriate service routine.
	move.l	(sp)+,d0	;Restore BDOS function number.
	moveq	#0,d1
	move.w	regd(regs),d1	;Get argument.
	jmp	(a0)		;Jump to the routine.
* Return here after performing the BDOS function.
results movem.l (sp)+,a1/a6
	moveq	#0,rega
	move.b	newrega,rega	;Get new accumulator value.
* We have finished processing the BDOS function.
	move.b	rega,d0 	;Set flags.
	and.w	regconff,d0
	move.b	0(flagptr,d0.w),regf
	rts
*
* Individual BDOS service routines
*
bdos00	bra	quitprg 	;Exit program.

bdos01	bsr	dmpstr		;Console input
	move.l	rawhand,d1
	move.l	#newrega,d2
	moveq	#1,d3
	sys	Read
	bra	results

bdos02	move.b	rege(regs),d1	;Console output
	clr.b	testdol 	;Allow dollar signs
	bsr	pchar
	bra	results

bdos03	equ	badbdos 	;Reader input

bdos04	equ	badbdos 	;Punch output

bdos05	pea	rege(regs)	;List output byte
bdos05t tst.b	listopn 	;Is the printer already open?
	bne.s	bdos05w 	;Yes.
	move.l	#prtname,d1
	move.l	#MODE_NEWFILE,d2
	sys	Open		;Open the printer.
	move.l	d0,prthand	;Save the file handle.
	bne.s	1$		;The open was successful.
	move.l	#badprt,d1
	bsr	pstring 	;Indicate an unsuccessful open.
	bsr	dump		;Spill guts...
	bra	quitprg 	; and exit.
1$	move.b	#1,listopn	;Indicate that the list device is open.
bdos05w move.l	prthand,d1
	move.l	(sp)+,d2	;Character to send to the list device
	moveq	#1,d3		;Just send one byte.
	sys	Write		;Send the byte to the list device.
	bra	results
	
prtname dc.b	'PRT:RAW',0
badprt	dc.b	'Unable to open the list device!$'

bdos06	cmpi.b	#$FF,rege(regs) ;Direct console I/O
	bne	bdos02		;Send the byte.
	bsr	dmpstr		;Dump any outstanding output.
	move.l	rawhand,d1
	moveq	#1,d2		;Wait for one microsecond.
	sys	WaitForChar	;Check whether a character is ready.
	tst.l	d0		;Is a character ready?
	bne	bdos01		;Yes - get it.
	clr.b	newrega 	;Indicate that nothing is ready.
	bra	results

bdos07	move.b	3(targbase),newrega	;Get IOBYTE
	bra	results

bdos08	move.b	rege(regs),3(targbase)	;Set IOBYTE
	bra	results

bdos09	add.l	targbase,d1	;Console output string
	bsr	pstring
	bra	results

bdos10	add.l	targbase,d1	;Console input line
	movea.l d1,a0		;The buffer is here.
	bsr	getline 	;Get a line.
	cmpi.b	#3,2(a0)	;Was it a control-C?
	bne	results 	;No - continue processing.
	bra	quitprg 	;Terminate the program.

bdos11	move.l	rawhand,d1	;Console status check
	moveq	#1,d2		;Wait for one microsecond.
	sys	WaitForChar	;Check whether a character is ready.
	move.b	d0,newrega	;Result is compatible with CP/M.
	bra	results

bdos12	clr.b	regh(regs)	;Get system identification
	move.b	#$22,regl(regs) ;Pretend we're CP/M 2.2.
	move.b	#$22,newrega	;Some programs use undocumented return reg.
	clr.b	regb(regs)
	bra	results

bdos13	move.b	4(targbase),d0	;Reset all drives
	andi.b	#$0F,d0 	;Current drive
	moveq	#1,d1
	lsl.w	d0,d1		;Set up drive bit.
	move.w	d1,acmap	;Set active drive map to current drive only.
	clr.w	romap		;Reset read-only map.
	bra	results

bdos14	move.b	rege(regs),d0	;Select drive
	andi.b	#$0F,d0 	;Isolate drive bits.
	andi.b	#$F0,4(targbase)
	or.b	d0,4(targbase)	;Insert new bits.
	moveq	#1,d1
	lsl.w	d0,d1		;Set up drive bit.
	or.w	d0,acmap	;Add new drive to active drive map.
	bra	results

bdos15	move.l	d1,-(sp)	;Open existing file
	bsr	gethand
	tst.l	d1		;Is the file already open?
	beq.s	bdos15g		;No - go ahead.
	clr.l	(a1)		;Clear file handle table entry.
	sys	Close		;Close the file.
bdos15g	move.l	(sp)+,d1
	add.l	#target,d1
	move.l	#MODE_OLDFILE,d2
	movea.l d1,a0		;The FCB is here.
	bsr	mapdrv		;Get drive map bit.
bdos15o move.l	a0,-(sp)
	lea	opnname,a1	;Build AmigaDOS file name here.
	move.l	a1,d1		;We'll need it here.
	bsr	convfn		;Make a file name.
	sys	Open		;Open the file.
	move.l	(sp)+,a1	;The FCB is here.
	lea	handles,a0
	moveq	#(handlen-handles)/16-1,d1
	move.b	#$FF,newrega	;Assume the open failed.
	tst.l	d0		;Did it fail?
	beq	results		;Yes.
	clr.b	newrega 	;Set flag to indicate success.
1$	tst.l	(a0)		;Available handle entry?
	beq.s	2$		;Yes.
	lea	16(a0),a0	;Check the next entry.
	dbra	d1,1$
	move.l	d0,d1		;File handle table overflow!
	sys	Close		;Close the file.
	move.l	#fullmsg,d1
	bsr	pstring 	;Display an error message
	bra	quitprg 	; and forget the whole thing.
2$	move.l	d0,(a0)+	;Save the file handle.
	moveq	#11,d0
3$	move.b	(a1)+,(a0)+	;Move first 12 bytes of FCB to table.
	dbra	d0,3$
	move.w	newdmap,d0
	or.w	d0,acmap	;Add drive to active drive map.
	bra	results

fullmsg dc.b	'Too many files are open!',cr,lf,'$'

bdos16	move.b	#$FF,newrega	;Close file
	bsr	gethand 	;Get the file handle.
	tst.l	d1		;Did we find it?
	beq	results 	;No - return failure code.
	clr.l	(a1)		;Clear the handle table entry.
	sys	Close		;Close the file.
	clr.b	newrega 	;Indicate success.
	bra	results

bdos18	lea	renname,a0	;Search for next file
	bra.s	bdos17i
bdos17	lea	srchnam,a0	;Search for first file
bdos17i	move.b	#$FF,newrega	;Assume we'll fail.
	add.l	targbase,d1
	movea.l d1,a1		;The FCB is here.
	addq.l	#1,a1		;The file name is here.
	moveq	#10,d0
1$	cmp.b	#'*',(a1)	;Is this an asterisk?
	beq.s	2$		;Yes - replace with question marks.
	move.b	(a1)+,(a0)+	;Move the current character.
	dbra	d0,1$
	bra.s	bdos17c
2$	move.b	#'?',(a0)+	;Convert to question marks.
	addq.l	#1,a1
	subq	#1,d0
	bmi.s	bdos17c		;The asterisk was in the extension.
	cmpi.w	#2,d0		;End of file name?
	bne.s	2$		;No - insert another question mark.
	bra.s	1$		;Start the extension field.
bdos17c	lea	target,targbase
	cmpi.b	#18,regc(regs)	;Search for next file?
	bne.s	bdos17k		;No.
	lea	srchnam,a0
	lea	renname,a1
	moveq	#10,d0
1$	cmp.b	(a0)+,(a1)+	;Search for the same name as last time?
	bne	results		;No - exit with failed status.
	dbra	d0,1$
	tst.l	fibsize		;Is the last file completely done?
	beq.s	bdos17k		;Yes - try for another one.
	pea	0		;Housekeeping for the stack
	bra.s	bdos17b		;Create an entry for the next extent.
bdos17k	move.l	#null,d1
	move.l	#ACCESS_READ,d2
	sys	Lock		;Get a file lock.
	tst.l	d0		;Did we fail?
	beq	results 	;Yes - exit.
	move.l	d0,-(sp)	;Save the lock.
	move.l	d0,d1		;Put it here for Examine.
	move.l	#fib,d2
	lea	target,targbase
	move.w	newdmap,d0
	or.w	d0,acmap	;Add drive to active drive map.
	cmpi.b	#18,regc(regs)	;Search for next file?
	beq.s	bdos17n 	;Yes.
	sys	Examine 	;Set up to find the first file.
	tst.l	d0		;Did we succeed?
	beq	bdos17x 	;No.
bdos17n move.l	(sp),d1
	move.l	#fib,d2
	sys	ExNext		;Look for the next file.
	tst.l	d0		;Did we succeed?
	beq	bdos17x 	;No.
	tst.l	fibtype 	;Is this a file entry?
	bpl	bdos17n 	;No - ignore subdirectory entries.
	clr.w	ext17		;Clear extent number.

bdos17b	move.l	dmaaddr,a1	;Build a fake FCB here.
	clr.b	(a1)+		;Clear drive code.
	move.l	a1,-(sp)	;The file name starts here.
	moveq	#10,d0
1$	move.b	#' ',(a1)+	;Clear file name and extension.
	dbra	d0,1$
	moveq	#19,d0
2$	clr.b	(a1)+		;Clear remainder of FCB to zeros.
	dbra	d0,2$
	lea	fibname,a0	;A0 scans AmigaDOS file name.
	move.l	(sp)+,a1	;A1 builds CP/M file name.
bdos17f tst.b	(a0)		;End of file name?
	beq.s	2$		;Yes.
	move.b	(a0)+,d0	;Get the current character.
	cmpi.b	#'a',d0		;Is it lower case?
	bcs.s	1$		;No - leave it alone.
	cmpi.b	#'z'+1,d0
	bcc.s	1$
	subi.b	#'a'-'A',d0	;Convert to upper case.
1$	move.b	d0,(a1)+	;Move the (possibly-converted) character.
	cmp.b	#'.',(a0)	;Start of file name extension?
	bne.s	bdos17f 	;No.
	move.l	dmaaddr,d0
	add.w	#9,d0		;Point to start of extension field.
	cmpa.l	d0,a1		;Is file name too long?
	bhi	bdos17n 	;Yes - not a valid CP/M file name - ignore it.
	move.l	d0,a1
	addq.l	#1,a0		;Skip over period.
	bra.s	bdos17f 	;Get the file name extension.
2$	move.l	dmaaddr,d0
	add.w	#13,d0
	cmpa.l	d0,a1		;Is the file name too long?
	bhi	bdos17n 	;Yes - ignore it.
	move.l	dmaaddr,a0
	addq.l	#1,a0
	lea	srchnam,a1
	moveq	#10,d0
3$	cmpi.b	#'?',(a1)	;Wild card character?
	bne.s	4$		;No - compare it.
	addq.l	#1,a0		;Skip over wild card character.
	addq.l	#1,a1
	dbra	d0,3$
	bra.s	bdos17w		;We found a wild card match.
4$	cmp.b	(a0)+,(a1)+	;Does the file name match the search name?
	bne	bdos17n 	;No - try for another one.
	dbra	d0,3$		;Check the next character.
bdos17w	lea	target,targbase
	moveq	#0,d1
	move.w	regd(regs),d1
	moveq	#0,d0
	move.b	12(targbase,d1.l),d0	;Extent flag
	beq.s	bdos17h		;Extent zero - take it.
	cmpi.b	#'?',d0		;Search for all extents?
	beq.s	bdos17h		;Yes - take this one.
	move.w	d0,ext17	;Save extent number.
	moveq	#14,d1
	lsl.l	d1,d0		;Multiply by 16384 to get byte displacement.
	sub.l	d0,fibsize	;Beyond end of file?  (Adjust byte count!)
	bcs	bdos17n		;Yes - forget this entry.

bdos17h	clr.b	newrega 	;Set success flag.
	move.l	dmaaddr,a0	;The FCB is here.
	move.w	ext17,d0	;Current extent number (counts up from zero)
	move.b	d0,12(a0)	;Insert it into the FCB.
	andi.b	#$1F,12(a0)	;Only the low-order 5 bits go here.
	lsr.l	#5,d0
	move.b	d0,14(a0)	;High-order portion of extent number
	move.l	fibsize,d0	;Number of bytes remaining
	move.b	#$80,15(a0)	;Assume the current extent is full.
	cmpi.l	#16384,d0	;Is it?
	bcc.s	1$		;Yes.
	and.l	#$3FFF,d0	;Number of bytes in the last extent
	add.w	#$7F,d0		;(for rounding)
	lsr.l	#7,d0		;Number of 128-byte records in extent
	move.b	d0,15(a0)	;Store record count in FCB.
1$	lea	target,targbase
	moveq	#0,d1
	move.w	regd(regs),d1
	cmpi.b	#'?',12(targbase,d1.l)	;Search for all extents?
	bne.s	2$			;No - we're done with this file.
	addq.w	#1,ext17	;Bump extent counter.
	move.l	#16384,d0
	sub.l	d0,fibsize	;Decrement remaining byte count
	bhi.s	bdos17x		;There's more to do.
2$	clr.l	fibsize		;Set remaining count to zero.
bdos17x move.l	(sp)+,d1	;Get the lock.
	beq	results		;Ignore dummy entry for multi-extent file.
	sys	UnLock		;Release the lock.
	bra	results

bdos19	add.l	targbase,d1	;Delete file
	movea.l d1,a0		;The FCB is here.
	bsr	mapdrv		;Get drive map bit.
	and.w	romap,d1	;Is this drive read-only?
	bne	roerr		;Yes - abort the deletion.
	lea	opnname,a1	;Build AmigaDOS file name here.
	move.l	a1,d1		;We'll need it here.
	bsr	convfn		;Make a file name.
	sys	DeleteFile	;Delete the file.
	move.w	newdmap,d0
	or.w	d0,acmap	;Add drive to active drive map.
	bra	results

bdos20	clr.b	newrega 	;Sequential read
	bsr	gethand
	tst.l	d1
	beq.s	1$
	move.l	dmaaddr,d2
	move.l	#128,d3
	sys	Read
	tst.l	d0		;Were we successful?
	bgt	results 	;Yes.
1$	move.b	#$1,newrega	;Set end-of-file flag.
	bra	results

bdos21	clr.b	newrega 	;Sequential write
	bsr	gethand
	tst.l	d1
	beq.s	1$
	move.l	d1,-(sp)
	bsr	mapdrv		;Get drive map bit.
	move.w	d1,d0
	move.l	(sp)+,d1
	and.w	romap,d0	;Is this drive read-only?
	bne.s	roerr		;Yes - error
	move.l	dmaaddr,d2
	move.l	#128,d3
	sys	Write
	tst.l	d0		;Were we successful?
	bgt	results 	;Yes.
1$	move.b	#$FF,newrega	;Set failure flag.
	bra	results

bdos22	move.l	d1,-(sp)	;Make new file
	bsr	gethand
	tst.l	d1		;Is the file already open?
	beq.s	bdos22g		;No - go ahead.
	clr.l	(a1)		;Clear file handle table entry.
	sys	Close		;Close the file.
bdos22g	move.l	(sp)+,d1
	add.l	#target,d1
	move.l	#MODE_NEWFILE,d2
	movea.l d1,a0		;The FCB is here.
	bsr	mapdrv		;Get the drive map bit.
	and.w	romap,d1	;Is the drive read-only?
	beq	bdos15o 	;No - continue with BDOS 15 open routine
roerr	move.l	#1$,d1
	bsr	pstring 	;'BDOS Error on '
	bsr	ucase
	move.b	d0,d1
	add.b	#'A',d1
	bsr	pchar		;Drive letter
	move.l	#2$,d1
	bsr	pstring 	;': R/O'
	bra	quitprg 	;Abort the program.

1$	dc.b	'BDOS Error on $'
2$	dc.b	': R/O',cr,lf,'$'

bdos23	add.l	targbase,d1	;Rename file
	movea.l d1,a0
	move.l	a0,-(sp)
	bsr	mapdrv		;Get drive map bit.
	and.w	romap,d1	;Is this drive read-only?
	bne	roerr		;Yes - error
	lea	opnname,a1
	bsr	convfn		;Convert old file name.
	move.l	(sp)+,a0
	lea	16(a0),a0
	lea	renname,a1
	bsr	convfn		;Convert new file name.
	move.l	#opnname,d1
	move.l	#renname,d2
	sys	Rename		;Rename the file.
	move.w	newdmap,d1
	or.w	d1,acmap	;Add drive to active drive map.
	clr.b	newrega 	;Assume we succeeded.
	tst.l	d0		;Did we fail?
	bne	results 	;No.
	move.b	#$FF,newrega
	bra	results

bdos24	move.w	acmap,regh(regs);Get active drive map
	bra	results

bdos25	move.b	4(targbase),newrega	;Get default drive number
	andi.b	#$0F,newrega	;Isolate drive bits.
	bra	results

bdos26	add.l	targbase,d1	;Set file buffer address
	move.l	d1,dmaaddr
	bra	results

bdos27	move.w	#fakealv-fdos+$FF00,regh(regs)	;Get allocation vector
	move.b	regl(regs),newrega	;Make undocumented copy.
	move.b	regh(regs),regb(regs)
	move.l	#null,d1
	move.l	#ACCESS_READ,d2
	sys	Lock		;Get a file lock.
	move.l	d0,d1		;Did we fail?
	beq	results 	;Yes - exit.
	move.l	d0,-(sp)	;Save the lock.
	move.l	#InfoData,d2
	sys	Info		;Get disk information.
	tst.l	d0		;Did we fail?
	beq	bdos27x		;Yes - exit.
	lea	fakealv-fdos+$FF00,a0
	adda.l	targbase,a0	;A0 loads allocation vector.
	move.b	#$F0,(a0)+	;Initial allocation for directory
	clr.b	(a0)+
	move.l	a0,-(sp)
	move.l	id_NumBlocks,d1	;Number of 512-byte blocks on disk
	addq.l	#3,d1
	lsr.l	#2,d1		;Convert to 2048-byte blocks for CP/M.
	move.l	d1,d0
	subq.l	#1,d0
	move.b	d0,fakedpb+5	;Insert high block number in DPB.
	lsr.l	#8,d0
	move.b	d0,fakedpb+6
1$	clr.b	(a0)+		;Clear remainder of allocation vector.
	dbra	d1,1$
	move.l	(sp)+,a0
	move.l	id_NumBlocksUsed,d0	;Number of 512-byte blocks used
	moveq	#11,d1
	lsr.w	#2,d0		;CP/M (2048-byte) blocks used
	beq.s	bdos27x		;Nothing is allocated.
	move.w	d0,d1
	and.w	#7,d1		;Number of bits to set in partial ALV byte
	beq.s	2$ 		;No partial byte
	subq.w	#1,d1		;Number of bits less one
	move.b	#$80,d2		;Here's one bit.
	asr.b	d1,d2		;Make some more.
	move.b	d2,(a0)+	;Store partial allocation vector byte.
2$	lsr.w	#3,d0		;Number of ALV bytes to set all bits in
	beq	bdos27x		;There are none - we're done.
	subq.w	#1,d0
3$	move.b	#$FF,(a0)+	;Set all bits in these ALV bytes.
	dbra	d0,3$
bdos27x	move.l	(sp)+,d1
	sys	UnLock		;Release the file lock.
	bra	results

bdos28	move.b	4(targbase),d0	;Protect drive
	andi.b	#$0F,d0 	;Current drive
	moveq	#1,d1
	lsl.w	d0,d1		;Set up drive bit.
	or.w	d1,romap	;Add it to read-only map.
	bra	results

bdos29	move.w	romap,regh(regs);Get read-only map
	bra	results

bdos30	equ	badbdos 	;Set file attributes

bdos31	move.w	#fakedpb-fdos+$FF00,regh(regs)	;Get disk parameter block
	move.b	regl(regs),newrega	;Make undocumented copy.
	move.b	regh(regs),regb(regs)
	bra	results

bdos32	cmp.b	#$FF,rege(regs) ;Get or set user code
	bne.s	1$		;Set it.
	move.b	4(targbase),d0	;Current drive and user code
	lsr.b	#4,d0		;Set up user code.
	move.b	d0,newrega
	move.b	newrega,regl(regs)
	move.b	regh(regs),regb(regs)
	bra	results
1$	andi.b	#$0F,4(targbase);Clear old user code.
	move.b	rege(regs),d0	;Get new user code.
	lsl.b	#4,d0		;Shift bits into position.
	or.b	d0,4(targbase)	;Insert new user code.
	bra	results

bdos33	pea	_LVORead(a6)	;Direct access read
	clr.b	newrega
	bsr	gethand
	bra.s	bdos34c 	;Use common read/write routine.

bdos34	pea	_LVOWrite(a6)	;Direct access write
	clr.b	newrega
	bsr	gethand
	move.l	d1,-(sp)
	bsr	mapdrv		;Get drive map bit.
	move.w	d1,d0
	move.l	(sp)+,d1
	and.w	romap,d0	;Is this drive read-only?
	bne	roerr		;Yes - error
bdos34c move.l	d1,-(sp)	;Save file handle (common read/write routine)
	moveq	#0,d2
	move.b	35(a0),d2	;Get seek address.
	rol.l	#8,d2
	move.b	34(a0),d2
	rol.l	#8,d2
	move.b	33(a0),d2
	rol.l	#7,d2		;Convert record number to byte displacement.
	move.b	33(a0),32(a0)	;Set up current record number in extent.
	andi.b	#$7F,32(a0)
	moveq	#14,d0
	ror.l	d0,d2
	move.b	d2,12(a0)	;Current extent number
	rol.l	d0,d2
	moveq	#-1,d3
	sys	Seek		;Seek to desired position.
	move.l	(sp)+,d1	;Get the file handle again.
	move.l	(sp)+,a0	;Address of read or write routine
	tst.l	d0		;Were we successful?
	bmi	1$		;No.
	move.l	dmaaddr,d2
	move.l	#128,d3
	jsr	(a0)		;Read or write the desired record.
	tst.l	d0		;Were we successful?
	bgt	results 	;Yes.
1$	move.b	#6,newrega	;Set failure (invalid address) flag.
	bra	results

bdos35	bsr	gethand 	;Get file end address
	move.l	a0,-(sp)	;Pointer to FCB
	move.l	d1,-(sp)
	moveq	#0,d2
	moveq	#1,d3
	sys	Seek		;Jump to end of file.
	move.l	(sp)+,d1
	move.l	d0,d2		;Old position
	moveq	#-1,d3
	sys	Seek		;Go back to the old position.
	move.l	(sp)+,a0	;Restore FCB pointer.
	add.l	#$7F,d0		;Adjust for partial final block, if any.
	lsr.l	#7,d0		;File size in 128-byte records
	move.b	d0,33(a0)	;Insert file size into FCB.
	lsr.l	#8,d0
	move.b	d0,34(a0)
	lsr.l	#8,d0
	move.b	d0,35(a0)
	bra	results

bdos36	bsr	gethand 	;Get direct address
	move.l	a0,-(sp)	;Save pointer to FCB.
	moveq	#0,d2
	moveq	#0,d3
	sys	Seek		;Seek to current position.
	move.l	(sp)+,a0	;Restore FCB pointer.
	lsr.l	#7,d0		;Convert to 128-byte record number.
	beq.s	bdos36m 	;Beginning of file
	subq.l	#1,d0		;Back up to record just read.
bdos36m move.b	d0,33(a0)	;Insert position into FCB.
	lsr.l	#8,d0
	move.b	d0,34(a0)
	lsr.l	#8,d0
	move.b	d0,35(a0)
	bra	results

*
* Individual BIOS service routines
*
bios01	bra	quitprg 	;Warm boot

bios02	equ	bdos11		;Console status check

bios03	equ	bdos01		;Console input byte

bios04	move.b	regc(regs),d1	;Console output byte
	clr.b	testdol 	;Allow dollar signs
	bsr	pchar
	bra	results

bios05	pea	regc(regs)	;List output byte
	bra	bdos05t

bios06	equ	badbios 	;Punch output byte

bios07	equ	badbios 	;Reader input byte

bios08	equ	badbios 	;Home disk

bios09	equ	badbios 	;Select disk

bios10	equ	badbios 	;Set track

bios11	equ	badbios 	;Set sector

bios12	equ	badbios 	;Set DMA address

bios13	equ	badbios 	;Read disk

bios14	equ	badbios 	;Write disk

bios15	move.b	#$FF,newrega	;List status
	bra	results


*
* Set "newdmap" with a bit corresponding to the drive code in the
*  FCB pointed to by A0 (or the default drive if necessary).
*  The contents of "newdmap" will also be in the low-order word of D1.
*  Register D0 will be set to 0 for drive A:, 1 for drive B:, 2 for C:, etc.
*  The contents of register A0 will be preserved.
*
mapdrv	move.b	(a0),d0 	;Drive code
	bne.s	1$		;The drive is specified.
	move.b	4(targbase),d0	;Get the default drive.
	andi.b	#$0F,d0
	addq	#1,d0
1$	subq.b	#1,d0		;Adjust drive code so that 0 is A:, etc.
	moveq	#1,d1
	lsl.w	d0,d1		;Set up drive map bit.
	move.w	d1,newdmap	;Save it.
	rts


*
* Simulation of the Z-80 LD A,R instruction -
*  load a random 7-bit value into the accumulator.
*
movear	move.l	#dtstamp,d1
	movem.l a1/a6,-(sp)
	move.l	_DOSBase,a6
	sys	DateStamp
	movem.l (sp)+,a1/a6
	move.b	dtstamp+11,rega ;Low-order 7 bits of timer ticks
	andi.b	#$7F,rega
	jmp	(return)


*
* End of program, one way or another
*
quitprg move.l	savesp,sp	;Restore stack pointer.
	bsr	dmpstr		;Dump any outstanding console output.
	clr.w	romap		;Clear the read-only map.

* Close the serial.device if it was used.
	tst.l	rpport
	beq.s	closlis 	;Nothing was opened.
	tst.l	wpport
	beq.s	q3		;The output port wasn't opened.

	lea	writreq,a1
	move.l	_SysBase,a6
	sys	CloseDevice	;Close the serial output device.

	move.l	wpport,-(sp)
	jsr	_DeletePort	;Delete the serial output port.
	addq.l	#4,sp
	clr.l	wpport

q3	lea	readreq,a1
	move.l	_SysBase,a6
	sys	CloseDevice	;Close the serial input device.

	move.l	rpport,-(sp)
	jsr	_DeletePort	;Delete the serial input port.
	addq.l	#4,sp
	clr.l	rpport

* If the list device was used, close it.
closlis tst.b	listopn 	;Is the printer open?
	beq.s	closprt 	;No.
	move.l	prthand,d1
	sys	Close		;Close the printer.
closprt clr.b	listopn 	;Reset the "printer-open" flag.

* If any files were left open by the last program, close them.
	lea	handles,a0
	moveq	#(handlen-handles)/16-1,d0
closall move.l	(a0),d1
	beq.s	closnxt 	;This file isn't open.
	movem.l a0/d0,-(sp)
	move.l	_DOSBase,a6
	sys	Close		;Close this file.
	movem.l (sp)+,a0/d0
	clr.l	(a0)		;Clear the file handle.
closnxt	lea	16(a0),a0
	dbra	d0,closall
* Check whether we should quit the simulation.
	tst.b	quitflg 	;Exit the simulator?
	bne.s	exitsim 	;Yes.
	tst.b	cmdflag 	;Was .COM file loaded from command line?
	beq	nextprg 	;No - re-display the command prompt.
* Terminate execution of the simulator.
exitsim move.l	rawhand,d1	;Is RAW: open?
	beq.s	closlib 	;No.
	move.l	_DOSBase,a6
	sys	Close		;Close RAW:
closlib move.l	_SysBase,a6
	move.l	_DOSBase,a1
	sys	CloseLibrary	;Close dos.library.
	moveq	#0,d0		;Return with no error.
	rts			;All done
	page
*************************************************************************
*									*
*	AmigaDOS interface routines					*
*									*
*************************************************************************

*
* Get a line from the console.	CP/M BDOS 10 conventions are used.
*  A0 is assumed to point to the start of the buffer.
*  If the first character encountered is a control-C, this routine
*  exits, leaving just the control-C in the buffer.
*
getline movem.l d2-d3/a0-a1/a6,-(sp)
	bsr	dmpstr		;Flush the screen buffer first.
	move.l	_DOSBase,a6
	lea	2(a0),a1	;The current character loads here.
	clr.b	1(a0)		;Clear character count.
getlinl move.l	rawhand,d1	;Read from RAW:
	move.l	a1,d2		; into current position
	moveq	#1,d3		;  for a length of one byte.
	movem.l d1-d3/a0-a1,-(sp)
	sys	Read		;Get a character.
	movem.l (sp)+,d1-d3/a0-a1
	cmpi.b	#cr,(a1)	;Did we get a carriage return?
	beq.s	getlinc 	;Yes - stop here.
	cmpi.b	#lf,(a1)	;Stop on a line feed too.
	beq.s	getlinc
	cmpi.b	#bs,(a1)	;Backspace?
	bne.s	getlinp 	;No.
	tst.b	1(a0)		;Do we have anything yet?
	beq.s	getlinl 	;No - ignore the backspace.
	subq.l	#1,a1		;Back over the previous character.
	subq.b	#1,1(a0)	;Decrement character count.
	movem.l a0-a1,-(sp)
	move.l	#bsmsg,d1
	bsr	pstring 	;Erase the previous character on the screen.
	movem.l (sp)+,a0-a1
	bra.s	getlinl
bsmsg	dc.b	bs,' ',bs,'$'	;Erases the previous character
getlinp movem.l a0-a1,-(sp)
	sys	Write		;Echo the current character.
	movem.l (sp)+,a0-a1
	addq.b	#1,1(a0)	;Bump character count.
	move.b	1(a0),d0	;Number of bytes read so far.
	cmpi.b	#3,(a1)+	;Did we get a control-C?
	bne.s	1$		;No.
	cmpi.b	#1,d0		;Is is the first character?
	beq.s	getlinx 	;Yes - exit now.
1$	cmp.b	(a0),d0 	;Is the buffer full?
	bne.s	getlinl 	;No - try for another character.
	bra.s	getlinx
getlinc bsr	pcrlf		;Carriage return or line feed
getlinx movem.l (sp)+,d2-d3/a0-a1/a6
	rts			;Exit.

*
* Display the message pointed to by D1.
*  The message must be terminated by a dollar sign.
*
pstring movem.l d2-d3/a1-a2,-(sp)	;Save work registers.
	move.l	d1,a0		;A0 scans the message.
	bset	#0,testdol	;Suppress $ test?
	beq.s	pstrs		;Yes (used by BDOS/BIOS character output)
	cmpi.b	#'$',(a0)	;Null string?
	beq	pstrx		;Yes - do nothing.
pstrs	move.l	strptr,a1	;A1 loads the output buffer.
	move.l	#strbufn,d3
	sub.l	a1,d3		;Number of bytes left in buffer
	ifne	h19
	moveq	#0,d0
	move.w	esclen,d0	;Is a partial escape sequence saved?
	beq.s	pstrl		;No.
	lea	escbuf,a2
	adda.l	d0,a2		;Continue loading it here.
	clr.w	esclen		;Reset "saved length" counter.
	cmpi.w	#2,d0		;Did we just save one byte?
	bcs.s	pstresc 	;Yes - get the remainder.
	bhi	pstreY2 	;Get the last cursor positioning byte.
	subq.l	#1,a2		;Back over dummy byte.
	bra	pstreY1 	;Get both cursor positioning bytes.
	endc
pstrl	cmpi.b	#lf,(a0)	;Line feed?
	bne.s	notlf		;No.
	lea	escbuf,a2	;Translate it to a cursor-down sequence.
	move.b	#$9B,(a2)+
	move.b	#'B',(a2)+
	addq.l	#1,a0
	bra	pstrsub
notlf:
	ifne	h19
* Optional H19 escape sequence translation
	cmpi.b	#esc,(a0)	;Escape character?
	bne	pstrm		;No - treat it normally.
	lea	escbuf,a2	;Build translated escape sequence here.
	move.b	#$9B,(a2)+	;Start with an AmigaDOS escape character.
	addq.l	#1,a0		;Check the next character.
	cmpi.b	#'$',(a0)	;End of string?
	bne.s	pstresc 	;No - analyze the sequence.
	move.w	#1,esclen	;We've saved one byte for next time.
	bra	pstrw		;Write everything up to the ESC character.
pstresc move.b	(a0)+,d0
	cmpi.b	#'[',d0 	;ANSI escape sequence?
	beq	pstrsub 	;Yes - pass the sequence with $9B header.
	cmpi.b	#'Y',d0
	beq.s	pstreY		;Set cursor position.
	cmpi.b	#'@',d0
	beq.s	pstrein 	;Set insert mode.
	cmpi.b	#'A',d0
	bcs	pstreun 	;Unknown code - copy it as is.
	cmpi.b	#'O',d0
	beq.s	pstreO		;Reset insert mode.
	bhi	pstreun 	;Unknown code
	move.l	a0,-(sp)
	lea	esctran(pc),a0	;Translation table with offset
	move.b	-'A'(a0,d0.w),d2;Get the translated code.
	move.l	(sp)+,a0
	btst	#6,d2		;Does the translated code stand alone?
	bne.s	1$		;No.
	subq.l	#1,a2		;Back over stored CSI character.
1$	move.b	d2,(a2)+	;Get the translated code.
	bra.s	pstrsub
esctran dc.b	'ABCD',ff,so,si,'H',$8D,'JKLMP' ;Escape sequence translation
pstrein move.b	#1,insflag	;Set insert mode.
	bra.s	pstrsbx
pstreO	clr.b	insflag 	;Reset insert mode.
	bra.s	pstrsbx
pstreY	cmpi.b	#'Y',d0 	;Set cursor position
	bne.s	pstreun
	cmpi.b	#'$',(a0)	;End of string?
	bne.s	pstreY1 	;No.
	move.w	#2,esclen	;Indicate we need both position bytes.
	bra	pstrw		;Finish the sequence next time.
pstreY1 moveq	#0,d0
	move.b	(a0)+,d0	;Get the first position byte.
	bsr	pstrcvd 	;Convert to decimal in save area.
	move.b	#';',(a2)+	;Add the separator character.
	cmpi.b	#'$',(a0)	;End of string?
	bne.s	pstreY2 	;No.
	sub.l	#escbuf,a2	;Number of bytes saved
	move.w	a2,esclen
	bra	pstrw		;Get the last byte next time.
pstreY2 moveq	#0,d0
	move.b	(a0)+,d0	;Get the last position byte.
	bsr	pstrcvd 	;Convert to decimal in save area.
	move.b	#'H',(a2)+	;Terminate the sequence.
	bra.s	pstrsub
pstreun move.b	#esc,escbuf	;Unidentified escape sequence -
	move.b	d0,(a2)+	; pass it through as is.
	endc
* The translated escape sequence is now in "escbuf" -
*  copy it to the output buffer.
pstrsub move.l	a2,d0
	lea	escbuf,a2	;A2 scans translated escape sequence.
	sub.l	a2,d0		;Length of translated escape sequence
	subq.l	#1,d0
1$	move.b	(a2)+,(a1)+	;Copy substitution to output string.
	subq.w	#1,d3		;Count down remaining length.
	dbra	d0,1$
pstrsbx cmpi.b	#'$',(a0)	;End of string?
	beq	pstrw		;Yes - write it out.
	tst.w	d3		;Is the buffer full?
	bmi	pstrw		;Yes - write out what we have.
	cmpi.b	#lf,-1(a0)	;Line feed?
	bne	pstrl		;No.
	tst.b	bufflag 	;Is console buffering in effect?
	beq	pstrl		;No.
	move.l	a1,strptr
	bsr	dmpstr		;Dump the buffer.
	move.l	strptr,a1
	bra	pstrl		;Check for another escape sequence.
* Subroutine to convert the byte in D0 to a character string at (A2)+
pstrcvd subi.b	#' '-1,d0	;Convert to binary row or column number.
	divu	#10,d0		;Convert to tens and units.
	tst.w	d0		;Is the number 10 or greater?
	beq.s	1$		;No - just create a one-digit number.
	addi.b	#'0',d0 	;Convert the tens digit to ASCII.
	move.b	d0,(a2)+	;Move it to the result field.
1$	swap	d0		;Get the units digit.
	addi.b	#'0',d0 	;Convert it to ASCII.
	move.b	d0,(a2)+
	rts
* Normal character processing
pstrm	tst.b	insflag 	;Are we in insert mode?
	beq.s	pstrmv		;No.
	lea	escbuf,a2
	move.b	#$9B,(a2)+	;Build an insert-character sequence.
	move.b	#'@',(a2)+
	move.b	(a0)+,(a2)+	;Here's the character to insert.
	bra.s	pstrsub 	;Use the substitution routine.
pstrmv	move.b	(a0)+,(a1)+	;Move one character.
	tst.b	bufflag 	;Is console buffering in effect?
	beq.s	2$		;No.
	cmpi.b	#cr,-1(a0)	;Carriage return?
	beq.s	1$		;Yes - dump the current segment.
	cmpi.b	#bel,-1(a0)	;Bell?
	bne.s	2$		;No - continue buffering.
1$	move.l	a1,strptr
	bsr	dmpstr		;Dump the buffer.
	move.l	strptr,a1
2$	cmpi.b	#'$',(a0)	;Test for end of string.
	dbeq	d3,pstrl	;Loop until we get there or buffer is full.
pstrw	move.l	a1,strptr
	tst	d3		;Is the buffer full?
	bmi.s	1$		;Yes - dump it regardless.
	tst.b	bufflag 	;Is console buffering in effect?
	bne.s	2$		;Yes - don't write anything yet.
1$	bsr	dmpstr		;Dump the buffer.
	move.l	strptr,a1
2$	tst	d3		;Did the output buffer overflow?
	bmi	pstrs		;Yes - get another section of the message.
pstrx	movem.l (sp)+,d2-d3/a1-a2	;Restore registers
	rts
*
* Write the contents of "strbuf" to RAW: if possible, or stdout if not.
*  The number of bytes to be written is calculated from "strptr".
*
dmpstr	movem.l d2-d3/a0-a1/a6,-(sp)
	move.l	strptr,d3
	move.l	#strbuf,d2	;Address of buffer
	move.l	d2,strptr	;Reset the buffer pointer.
	sub.l	d2,d3		;Length of output string
	beq.s	2$		;Zero - don't write anything.
	move.l	rawhand,d1	;Assume we're writing to RAW:
	bne.s	1$
	move.l	stdout,d1	;We don't have RAW: - use stdout.
1$	move.l	_DOSBase,a6
	sys	Write		;Display the line.
2$	movem.l (sp)+,d2-d3/a0-a1/a6
	rts

*
* Convert the file name in the FCB pointed to by A0
*  to an AmigaDOS-format file name in the field pointed to by A1.
*  D0 is the only other register used by this routine.
*
convfn	move.l	a1,-(sp)
	move.l	a0,-(sp)	;Save start address of FCB.
	move.b	(a0)+,d0	;Get the drive code.
	bne.s	1$		;We have a drive code.
	move.b	target+4,d0	;Use the default drive code.
	andi.b	#$0F,d0
	addq.b	#1,d0		;Start at 1 for drive A:.
1$	cmpi.b	#1,d0		;Is it drive A:?
	beq.s	4$		;Yes - don't add anything special.
	move.b	#'C',(a1)+
	move.b	#'P',(a1)+	;Set up the prefix CPMx:
	move.b	#'M',(a1)+	; where x is the drive letter.
	add.b	#'A'-1,d0
	move.b	d0,(a1)+
	move.b	target+4,d0	;Get user number.
	lsr.b	#4,d0		;Move it to low-order 4 bits.
	beq.s	3$		;Don't insert user number if it's zero.
	move.b	#'0',(a1)+	;Assume user number is less than 10.
	cmpi.b	#10,d0		;Is user number 10 or greater?
	bcs.s	2$		;No.
	move.b	#'1',-1(a1)	;Change the first digit to 1.
	subi.b	#10,d0
2$	addi.b	#'0',d0 	;Convert user number to ASCII.
	move.b	d0,(a1)+	;Insert user number into file spec.
3$	move.b	#':',(a1)+
4$	moveq	#7,d0		;Maximum of 8 characters for file name
convfn1 cmpi.b	#' ',(a0)	;End of file name?
	beq.s	3$		;Yes
	cmpi.b	#'*',(a0)	;Wild card?
	bne.s	1$		;No.
	move.b	#'#',(a1)+	;Convert to AmigaDOS format.
	move.b	#'?',(a1)+
	addq.l	#1,a0		;Skip over the asterisk.
	bra.s	2$
1$	move.b	(a0)+,(a1)+	;Move one character of file name.
2$	dbra	d0,convfn1	;Try for more.
3$	movea.l (sp)+,a0	;Back to start of FCB.
	lea	9(a0),a0	;Go to start of file name extension.
	cmpi.b	#' ',(a0)	;Do we have an extension?
	beq.s	convfnx 	;No.
	move.b	#'.',(a1)+	;Insert extension separator.
	moveq	#2,d0		;Maximum of 3 characters for extension.
convfn2 cmpi.b	#' ',(a0)	;End of extension?
	beq.s	convfnx 	;Yes.
	cmpi.b	#'*',(a0)	;Wild card?
	bne.s	1$		;No.
	move.b	#'#',(a1)+	;Convert to AmigaDOS format.
	move.b	#'?',(a1)+
	addq.l	#1,a0		;Skip over the asterisk.
	bra.s	2$
1$	move.b	(a0)+,(a1)+	;Move one character of extension.
2$	dbra	d0,convfn2	;Try for more.
convfnx clr.b	(a1)		;Terminate file name string.
	move.l	(sp)+,a1
	rts

*
* Get the file handle indicated by the first 12 bytes of the CP/M FCB
*  whose CP/M address is in D1.  The file handle (if found) is copied
*  to D1 from the file handle table entry (which is pointed to by A1).
*  If the file handle cannot be found, D1 will be set to zero.
*  In any event, A0 will be set to point to the FCB.
*
gethand	lea	0(targbase,d1.l),a0	;The FCB is here.
	lea	handles,a1	;A1 scans the file handle table.
	moveq	#(handlen-handles)/16-1,d0
1$	movem.l	d0/a0-a1,-(sp)
	tst.l	(a1)		;Is this entry empty?
	beq.s	3$		;Yes - ignore it.
	addq.l	#4,a1		;Skip over to file name in table.
	moveq	#11,d1
2$	cmp.b	(a0)+,(a1)+	;Compare first 12 bytes of FCB with table.
	bne.s	3$		;No match.
	dbra	d1,2$
	movem.l	(sp)+,d0/a0-a1
	move.l	(a1),d1		;Here's the file handle.
	rts
3$	movem.l	(sp)+,d0/a0-a1
	lea	16(a1),a1	;Try the next table entry.
	dbra	d0,1$
	moveq	#0,d1		;Couldn't find the handle!
	rts
	page
*************************************************************************
*									*
*	Serial port routines						*
*									*
*************************************************************************

*
* Read a byte from the port whose number is in D0.
*  A0 points to where to put it.
*
inp	movem.l a1/a6/d2-d3,-(sp)
	cmp.b	#$14,d0 	;Port 14?
	bne.s	inp15		;No.
	move.l	a0,-(sp)
	tst.l	rpport		;Is serial.device open?
	bne.s	1$		;Yes.
	bsr	initser 	;Set up serial.device.
1$	bsr	checkio
	tst.l	d0		;Is a character ready?
	beq.s	2$		;No - give the previous one again.
	bsr	serread 	;Read the new character.
2$	move.l	(sp)+,a0
	move.b	charin,(a0)	;Move it to user's area.
	bra.s	inpx
inp15	cmp.b	#$15,d0 	;Port 15?
	bne.s	inpx		;No - ignore it.
	move.b	#7,(a0) 	;Assume a character is ready.
	move.l	a0,-(sp)
	bsr	checkio 	;Check whether a character is ready.
	move.l	(sp)+,a0
	tst.l	d0		;Is a character ready?
	bne.s	inpx		;Yes.
	move.b	#5,(a0) 	;Don't set "receiver ready" bit.
inpx	movem.l (sp)+,a1/a6/d2-d3
	rts

*
* Write the byte pointed to by A0 to the port whose number is in D0.
*
outp	movem.l a1/a6/d2-d3,-(sp)
	cmp.l	#$14,d0 	;Port 14?
	bne.s	outpx		;No - ignore it.
	move.b	(a0),charout	;Character to write
	tst.l	rpport		;Is serial.device open?
	bne.s	1$		;Yes.
	bsr	initser 	;Set up serial.device.
1$	bsr	serwrit 	;Write the character.
outpx	movem.l (sp)+,a1/a6/d2-d3
	rts

*
* Initialize the serial port.
*
initser:

* Open a reply port for the serial input device.
	lea	readreq,a0
	move.w	#rsize-1,d0
1$	clr.b	(a0)+		;Clear the I/O request block.
	dbra	d0,1$
	clr.l	-(sp)
	move.l	#2$,-(sp)
	jsr	_CreatePort	;rpport = CreatePort ("Read_RS", NULL);
	addq.l	#8,sp
	move.l	d0,rpport
	move.l	d0,r_ReplyPort
	bne.s	openin
	move.l	#3$,d1
	bsr	pstring
	bra	quitprg
2$	dc.b	'Read_RS',0
3$	dc.b	'Can''t create input port for serial.device!',cr,lf,'$'

* Open the serial input device.
openin	move.w	#rsize,r_MLength
	move.b	#SERFLAGS,r_SerFlags
	move.l	#CTLCHAR,r_CtlChar
	lea	1$,a0
	moveq	#0,d0
	lea	readreq,a1
	moveq	#0,d1
	move.l	_SysBase,a6
	sys	OpenDevice
	tst.l	d0		;Were we successful?
	beq.s	setin		;Yes.
	move.l	#2$,d1
	bsr	pstring
	bra	quitprg
1$	dc.b	'serial.device',0
2$	dc.b	'Can''t open serial.device!',cr,lf,'$'

setin	move.l	baud,r_Baud
	move.b	bits,r_ReadLen	;Number of bits per character
	move.b	bits,r_WriteLen
	move.w	#SDCMD_SETPARAMS,r_Command
	lea	readreq,a1
	move.l	_SysBase,a6
	sys	DoIO		;Set new input port parameters.

* Set up the serial output port.
	lea	readreq,a0
	lea	writreq,a1
	move.w	#wsize-1,d0
copw	move.b	(a0)+,(a1)+	;Clone the read request block.
	dbra	d0,copw
	clr.l	-(sp)
	move.l	#1$,-(sp)
	jsr	_CreatePort	;wpport = CreatePort ("Write_RS", NULL);
	addq.l	#8,sp
	move.l	d0,wpport
	move.l	d0,w_ReplyPort
	bne.s	startup
	move.l	#2$,d1
	bsr	pstring
	bra	quitprg
1$	dc.b	'Write_RS',0
2$	dc.b	'Can''t create output port for serial.device!',cr,lf,'$'

startup bsr	setbaud 	;Set parameters and start reading.
	rts

*
* Return TRUE (D0 <> 0) if the serial port has a character.
*
checkio lea	readreq,a1
	move.l	_SysBase,a6
	sys	CheckIO
	rts

*
* Set the serial port's baud rate, number of data bits, etc.
*
setbaud tas	frstset 	;Is this the first call?
	beq.s	1$		;Yes - input port is set up.
	lea	readreq,a1
	move.l	_SysBase,a6
	sys	AbortIO 	;Abort the outstanding read.
	move.l	baud,r_Baud	;Baud rate
	move.b	bits,r_ReadLen	;Number of bits per character
	move.b	bits,r_WriteLen
	move.w	#SDCMD_SETPARAMS,r_Command
	lea	readreq,a1
	move.l	_SysBase,a6
	sys	DoIO		;Set new input port parameters.
1$	move.w	#CMD_READ,r_Command
	move.l	#1,r_Length
	move.l	#charinb,r_Data
	lea	readreq,a1
	move.l	_SysBase,a6
	sys	SendIO		;Start reading again.
* Now set up the output port - this one is more straightforward.
	move.l	baud,w_Baud
	move.b	bits,w_ReadLen
	move.b	bits,w_WriteLen
	move.w	#SDCMD_SETPARAMS,w_Command
	lea	writreq,a1
	move.l	_SysBase,a6
	sys	DoIO
	rts

*
* Write the byte in "charout" to the serial port.
*
serwrit move.w	#CMD_WRITE,w_Command
	move.l	#1,w_Length
	move.l	#charout,w_Data
	lea	writreq,a1
	move.l	_SysBase,a6
	sys	DoIO
	rts

*
* Read a byte from the serial port into "charin".
*  If a byte isn't ready, this routine will wait until one is.
*
serread lea	readreq,a1
	move.l	_SysBase,a6
	sys	WaitIO		;Wait until a character is ready.
	move.b	charinb,charin	;Get the character from the buffer.
	move.w	#CMD_READ,r_Command
	move.l	#1,r_Length
	move.l	#charinb,r_Data
	lea	readreq,a1
	move.l	_SysBase,a6
	sys	SendIO		;Get ready for the next character.
	rts
	page
*************************************************************************
*									*
*	Miscellaneous service routines					*
*	(Inelegant, but rarely used so they stand as is.)		*
*									*
*************************************************************************

*
* Display the contents of D1 in hex.
*
pbyte	move.l	#$20018,d0	;2 nybbles, 24-bit shift first
	bra.s	phex
pword	move.l	#$40010,d0	;4 nybbles, 16-bit shift first
	bra.s	phex
paddr	move.l	#$60008,d0	;6 nybbles, 8-bit shift first
	bra.s	phex
plong	move.l	#$80000,d0	;8 nybbles, no shift first
phex	lea	workbuf,a0
	move.l	a0,-(sp)
	bsr	pdigits
	move.b	#'$',(a0)+
	move.l	(sp)+,d1
	bsr	pstring
	rts
*
* Convert the contents of D1 to hex at (A0).
*  On exit, A0 points to the next available byte.
*
ubyte	move.l	#$20018,d0	;2 nybbles, 24-bit shift first
	bra.s	pdigits
uword	move.l	#$40010,d0	;4 nybbles, 16-bit shift first
	bra.s	pdigits
uaddr	move.l	#$60008,d0	;6 nybbles, 8-bit shift first
	bra.s	pdigits
ulong	move.l	#$80000,d0	;8 nybbles, no shift first
pdigits rol.l	d0,d1		;Do shift.
	bra.s	3$
1$	swap	d0		;Save nybble count.
	rol.l	#4,d1		;Print variable in d1.
	move.l	d1,-(sp)
	and	#$F,d1		;Isolate the current nybble.
	cmp	#$A,d1
	bcs.s	2$
	add.b	#'A'-'9'-1,d1	;Adjust for digits A through F.
2$	add.b	#'0',d1 	;Convert to ASCII.
	move.b	d1,(a0)+	;Add to the result string.
	move.l	(sp)+,d1
3$	swap	d0		;Get nybble count.
	dbra	d0,1$
	rts

pchar	move.b	d1,workbuf	;Print the character in D1.
	move.b	#'$',workbuf+1
	move.l	#workbuf,d1
	bsr	pstring
	rts

pspace	move.l	#1$,d1		;Print a space.
	bsr	pstring
	rts
1$	dc.b	' $'

pcrlf	move.l	#1$,d1		;Print a carriage return and line feed.
	bsr	pstring
	rts
1$	dc.b	cr,lf,'$'

*
* Convert the hex string pointed to by A0 to long in d1.
*  Stops on the first invalid hex digit, which is returned in d0.
*  A0 is left pointing to this first invalid digit.
*  d2 = 1 if any valid digits were found, 0 otherwise.
*
atol	moveq	#0,d1
	moveq	#0,d2
1$	move.b	(a0)+,d0	;Get the current byte.
	cmpi.b	#$60,d0
	bcs.s	2$
	andi.b	#$5F,d0 	;Mask to upper case.
2$	cmpi.b	#'0',d0 	;Check range (0..9,A..F).
	bcs.s	atolend
	cmpi.b	#'F',d0
	bhi.s	atolend
	cmpi.b	#'9',d0
	bls.s	3$
	cmpi.b	#'A',d0
	bcs.s	atolend
3$	moveq	#1,d2		;Valid characters entered, set flag.
	sub.b	#'0',d0 	;Convert to binary
	cmpi.b	#$9,d0		;Digit in range 0..9?
	bls.s	4$		;Yes - conversion is complete
	sub.b	#'A'-'9'-1,d0	;Adjust digits A..F.
4$	ext	d0		;Convert to long.
	ext.l	d0
	asl.l	#4,d1		;Tack it onto d1.
	add.l	d0,d1
	bra.s	1$		;Try for another digit.
atolend subq.l	#1,a0		;Back onto the first invalid digit.
	rts
	page
*************************************************************************
*									*
*	Instruction mnemonic table (used for tracing)			*
*									*
*************************************************************************

	data	data

* This table contains the mnemonic strings for the 8080/Z-80 opcodes.
*
* "q" denotes a register number in bits 3 through 5 of the opcode.
*	Values are interpreted as follows:
*		Normal 8080	Normal Z-80	DD prefix	FD prefix
*	000		B		B		B		B
*	001		C		C		C		C
*	010		D		D		D		D
*	011		E		E		E		E
*	100		H		H		XH		YH
*	101		L		L		XL		YL
*	110		M	     (HL)	    (IX+n)	    (IY+n)
*	111		A		A		A		A
*
* "r" denotes a register number in bits 0 through 2 of the opcode.
*	Values are interpreted the same as for "q" above.
*
* "p" denotes a 2-bit register pair number in bits 4 and 5 of the opcode.
*	Values are interpreted as follows:
*		8080	Z-80
*	00	B	BC
*	01	D	DE
*	10	H	HL	(no DD or FD prefix)
*	10	IX	IX	(with DD prefix)
*	10	IX	IX	(with FD prefix)
*	11	SP	SP	(if opcode is below F0)
*	11	PSW	AF	(if opcode is F0 or greater)
*
* "h" is replaced by IX or IY if the opcode prefix is DD or FD respectively.
*  If the instruction is not prefixed, "h" is replaced by HL.
*
* "n" denotes an 8-bit number following the opcode.
*
* "a" denotes a 16-bit address following the opcode.

* Mnemonics for 8080 opcodes 00 through 3F
mnop008:
	dc.b	'NOP$        LXI  p,a$   STAX p$     INX  p$     ' ;00-03
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   RLC$        ' ;04-07
	dc.b	'EXAF$       DAD  p$     LDAX p$     DCX  p$     ' ;08-0B
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   RRC$        ' ;0C-0F
	dc.b	'DJNZ n$     LXI  p,a$   STAX p$     INX  p$     ' ;10-13
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   RAL$        ' ;14-17
	dc.b	'JR   n$     DAD  p$     LDAX p$     DCX  p$     ' ;18-1B
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   RAR$        ' ;1C-1F
	dc.b	'JRNZ n$     LXI  p,a$   ShD a$      INX  p$     ' ;20-23
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   DAA$        ' ;24-27
	dc.b	'JRZ  n$     DAD  p$     LhD a$      DCX  p$     ' ;28-2B
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   CMA$        ' ;2C-2F
	dc.b	'JRNC n$     LXI  p,a$   STA  a$     INX  p$     ' ;30-33
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   STC$        ' ;34-37
	dc.b	'JRC  n$     DAD  p$     LDA  a$     DCX  p$     ' ;38-3B
	dc.b	'INR  q$     DCR  q$     MVI  q,n$   CMC$        ' ;3C-3F

* Mnemonics for Z-80 opcodes 00 through 3F
mnop00z:
	dc.b	'NOP$        LD   p,a$   LD   (p),A$ INC  p$     ' ;00-03
	dc.b	'INC  q$     DEC  q$     LD   q,n$   RLCA$       ' ;04-07
	dc.b	'EX   AF,AF$ ADD  HL,p$  LD   A,(p)$ DEC  p$     ' ;08-0B
	dc.b	'INC  q$     DEC  q$     LD   q,n$   RRCA$       ' ;0C-0F
	dc.b	'DJNZ n$     LD   p,a$   LD   (p),A$ INC  p$     ' ;10-13
	dc.b	'INC  q$     DEC  q$     LD   q,n$   RLA$        ' ;14-17
	dc.b	'JR   n$     ADD  HL,p$  LD   A,(p)$ DEC  p$     ' ;18-1B
	dc.b	'INC  q$     DEC  q$     LD   q,n$   RRA$        ' ;1C-1F
	dc.b	'JR   NZ,n$  LD   p,a$   LD   (a),HL$INC  p$     ' ;20-23
	dc.b	'INC  q$     DEC  q$     LD   q,n$   DAA$        ' ;24-27
	dc.b	'JR   Z,n$   ADD  HL,p$  LD   HL,(a)$DEC  p$     ' ;28-2B
	dc.b	'INC  q$     DEC  q$     LD   q,n$   CPL$        ' ;2C-2F
	dc.b	'JR   NC,n$  LD   p,a$   LD   (a),A$ INC  p$     ' ;30-33
	dc.b	'INC  q$     DEC  q$     LD   q,n$   SCF$        ' ;34-37
	dc.b	'JR   C,n$   ADD  HL,p$  LD   A,(a)$ DEC  p$     ' ;38-3B
	dc.b	'INC  q$     DEC  q$     LD   q,n$   CCF$        ' ;3C-3F

* Mnemonics for opcodes 40 through 7f are easy - 76 is HLT
*  (HALT for Z-80), and all others are MOV (LD for Z-80).
mnop408 dc.b	'MOV  q,r$'
mnop40z dc.b	'LD   q,r$'
mnop768 dc.b	'HLT$'
mnop76z dc.b	'HALT$'

* Mnemonics for 8080 opcodes 80 through BF
mnop808:
	dc.b	'ADD  r$  ADC  r$  SUB  r$  SBB  r$  '	;80-9F
	dc.b	'ANA  r$  XRA  r$  ORA  r$  CMP  r$  '	;A0-BF

* Mnemonics for Z-80 opcodes 80 through BF
mnop80z:
	dc.b	'ADD  A,r$ADC  A,r$SUB  r$  SBC  A,r$'	;80-9F
	dc.b	'AND  r$  XOR  r$  OR   r$  CP   r$  '	;A0-BF

* Mnemonics for 8080 opcodes C0 through FF
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopC08:
	dc.b	'RNZ$        POP  p$     JNZ  a$     JMP  a$     ' ;C0-C3
	dc.b	'CNZ  a$     PUSH p$     ADI  n$     RST  0$     ' ;C4-C7
	dc.b	'RZ$         RET$        JZ   a$     ILLEGAL$    ' ;C8-CB
	dc.b	'CZ   a$     CALL a$     ACI  n$     RST  1$     ' ;CC-CF
	dc.b	'RNC$        POP  p$     JNC  a$     OUT  n$     ' ;D0-D3
	dc.b	'CNC  a$     PUSH p$     SUI  n$     RST  2$     ' ;D4-D7
	dc.b	'RC$         EXX$        JC   a$     IN   n$     ' ;D8-DB
	dc.b	'CC   a$     ILLEGAL$    SBI  n$     RST  3$     ' ;DC-DF
	dc.b	'RPO$        POP  p$     JPO  a$     XTh$        ' ;E0-E3
	dc.b	'CPO  a$     PUSH p$     ANI  n$     RST  4$     ' ;E4-E7
	dc.b	'RPE$        PCh$        JPE  a$     XCHG$       ' ;E8-EB
	dc.b	'CPE  a$     ILLEGAL$    XRI  n$     RST  5$     ' ;EC-FF
	dc.b	'RP$         POP  p$     JP   a$     DI$         ' ;F0-F3
	dc.b	'CP   a$     PUSH p$     ORI  n$     RST  6$     ' ;F4-F7
	dc.b	'RM$         SPh$        JM   a$     EI$         ' ;F8-FB
	dc.b	'CM   a$     ILLEGAL$    CPI  n$     RST  7$     ' ;FC-FF

* Mnemonics for Z-80 opcodes C0 through FF
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopC0z:
	dc.b	'RET  NZ$    LD   p,(SP)$JP   NZ,a$  JP   a$     ' ;C0-C3
	dc.b	'CALL NZ,a$  LD   (SP),p$ADD  A,n$   RST  0$     ' ;C4-C7
	dc.b	'RET  Z$     RET$        JP   Z,a$   ILLEGAL$    ' ;C8-CB
	dc.b	'CALL Z,a$   CALL a$     ADC  A,n$   RST  8$     ' ;CC-CF
	dc.b	'RET  NC$    LD   p,(SP)$JP   NC,a$  OUT  n,A$   ' ;D0-D3
	dc.b	'CALL NC,a$  LD   (SP),p$SUB  A,n$   RST  10$    ' ;D4-D7
	dc.b	'RET  C$     EXX$        JP   C,a$   IN   A,n$   ' ;D8-DB
	dc.b	'CALL C,a$   ILLEGAL$    SBC  A,n$   RST  18$    ' ;DC-DF
	dc.b	'RET  PO$    LD   p,(SP)$JP   PO,a$  EX   (SP),p$' ;E0-E3
	dc.b	'CALL PO,a$  LD   (SP),p$AND  A,n$   RST  20$    ' ;E4-E7
	dc.b	'RET  PE$    JP   (p)$   JP   PE,a$  EX   DE,p$  ' ;E8-EB
	dc.b	'CALL PE,a$  ILLEGAL$    XOR  A,n$   RST  28$    ' ;EC-FF
	dc.b	'RET  P$     LD   p,(SP)$JP   P,a$   DI$         ' ;F0-F3
	dc.b	'CALL P,a$   LD   (SP),p$OR   A,n$   RST  30$    ' ;F4-F7
	dc.b	'RET  M$     LD   SP,h$  JP   M,a$   EI$         ' ;F8-FB
	dc.b	'CALL M,a$   ILLEGAL$    CP   A,n$   RST  38$    ' ;FC-FF

* Mnemonics for opcodes CB00 through CB3F -
*  these are the same for both the 8080 and the Z-80.
mnopCB08:
mnopCB0z:
	dc.b	'RLC  r$ RRC  r$ RL   r$ RR   r$ '	;CB00-CB1F
	dc.b	'SLA  r$ SRA  r$ ILLEGAL$SRL  r$ '	;CB20-CB3F

* Mnemonics for opcodes CB40 through CBFF -
*  these are the same for both the 8080 and the Z-80.
mnopCB48:
mnopCB4z:
	dc.b	'BIT  $  RES  $  SET  $  '

* Mnemonics for 8080 opcodes ED40 through ED7F
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopE48:
	dc.b	'IN   q,(C)$ OUT  (C),q$ DSBB p$     SBCD a$     ' ;ED40-ED43
	dc.b	'NEG$        RETN$       IM0$        MOV  I,A$   ' ;ED44-ED47
	dc.b	'IN   q,(C)$ OUT  (C),q$ DADC p$     LBCD a$     ' ;ED48-ED4B
	dc.b	'ILLEGAL$    RETI$       ILLEGAL$    MOV  R,A$   ' ;ED4C-ED4F
	dc.b	'IN   q,(C)$ OUT  (C),q$ DSBB p$     SDED a$     ' ;ED50-ED53
	dc.b	'ILLEGAL$    ILLEGAL$    IM1$        MOV  A,I$   ' ;ED54-ED57
	dc.b	'IN   q,(C)$ OUT  (C),q$ DADC p$     LDED a$     ' ;ED58-ED5B
	dc.b	'ILLEGAL$    ILLEGAL$    IM2$        MOV  A,R$   ' ;ED5C-ED5F
	dc.b	'IN   q,(C)$ OUT  (C),q$ DSBB p$     SHLD a$     ' ;ED60-ED63
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    RRD$        ' ;ED64-ED67
	dc.b	'IN   q,(C)$ OUT  (C),q$ DADC p$     LHLD a$     ' ;ED68-ED6B
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    RLD$        ' ;ED6C-ED6F
	dc.b	'IN   q,(C)$ OUT  (C),q$ DSBB p$     SSPD a$     ' ;ED70-ED73
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED74-ED77
	dc.b	'IN   q,(C)$ OUT  (C),q$ DADC p$     LSPD a$     ' ;ED78-ED7B
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED7C-ED7F

* Mnemonics for Z-80 opcodes ED40 through ED7F
*  These are interpreted by the same routine as for opcodes 00 through 3F.
mnopE4z:
	dc.b	'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED40-ED43
	dc.b	'NEG$        RETN$       IM   0$     LD   I,A$   ' ;ED44-ED47
	dc.b	'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED48-ED4B
	dc.b	'ILLEGAL$    RETI$       ILLEGAL$    LD   R,A$   ' ;ED4C-ED4F
	dc.b	'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED50-ED53
	dc.b	'ILLEGAL$    ILLEGAL$    IM   1$     LD   A,I$   ' ;ED54-ED57
	dc.b	'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED58-ED5B
	dc.b	'ILLEGAL$    ILLEGAL$    IM   2$     LD   A,R$   ' ;ED5C-ED5F
	dc.b	'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED60-ED63
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    RRD$        ' ;ED64-ED67
	dc.b	'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED68-ED6B
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    RLD$        ' ;ED6C-ED6F
	dc.b	'IN   q,(C)$ OUT  (C),q$ SBC  HL,p$  LD   (a),p$ ' ;ED70-ED73
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED74-ED77
	dc.b	'IN   q,(C)$ OUT  (C),q$ ADC  HL,p$  LD   p,(a)$ ' ;ED78-ED7B
	dc.b	'ILLEGAL$    ILLEGAL$    ILLEGAL$    ILLEGAL$    ' ;ED7C-ED7F

* Mnemonics for miscellaneous ED-prefix instructions -
*  these are the same for both the 8080 and the Z-80.
mnopEA8:
mnopEAz:
	dc.b	'LDI$ CPI$ INI$ OUTI$'	;EDA0-EDA3
	dc.b	'                    '	;EDA4-EDA7 (illegal)
	dc.b	'LDD$ CPD$ IND$ OUTD$'	;EDA8-EDAB
	dc.b	'                    '	;EDAC-EDAF (illegal)
	dc.b	'LDIR$CPIR$INIR$OTIR$'	;EDB0-EDB3
	dc.b	'                    '	;EDB4-EDB7 (illegal)
	dc.b	'LDDR$CPDR$INDR$OTDR$'	;EDB8-EDBB

* Mnemonic for illegal instructions
mnopilg dc.b	'ILLEGAL$'
	page
*************************************************************************
*									*
*	Fake FDOS							*
*									*
*************************************************************************

*
* Fake BDOS for target system
*
fdos	dc.b	tHLT,0,tRET	;BIOS jump table
	dc.b	tJMP,$33,$FF	;Warm boot
	dc.b	tJMP,$36,$FF	;Console status
	dc.b	tJMP,$39,$FF	;Console input
	dc.b	tJMP,$3C,$FF	;Console output
	dc.b	tJMP,$3F,$FF	;List output
	dc.b	tJMP,$42,$FF	;Punch output
	dc.b	tJMP,$45,$FF	;Reader input
	dc.b	tJMP,$48,$FF	;Home disk
	dc.b	tJMP,$4B,$FF	;Select disk
	dc.b	tJMP,$4E,$FF	;Set track
	dc.b	tJMP,$51,$FF	;Set sector
	dc.b	tJMP,$54,$FF	;Set DMA address
	dc.b	tJMP,$57,$FF	;Read
	dc.b	tJMP,$5A,$FF	;Write
	dc.b	tJMP,$5D,$FF	;Get list device status
	dc.b	tJMP,$60,$FF	;Sector translation

*
* Fake BIOS for target system
*
	dc.b	tHLT,1,tRET	;Warm boot
	dc.b	tHLT,2,tRET	;Console status
	dc.b	tHLT,3,tRET	;Console input
	dc.b	tHLT,4,tRET	;Console output
	dc.b	tHLT,5,tRET	;List output
	dc.b	tHLT,6,tRET	;Punch output
	dc.b	tHLT,7,tRET	;Reader input
	dc.b	tHLT,8,tRET	;Home disk *
	dc.b	tHLT,9,tRET	;Select disk *
	dc.b	tHLT,10,tRET	;Set track *
	dc.b	tHLT,11,tRET	;Set sector *
	dc.b	tHLT,12,tRET	;Set DMA address *
	dc.b	tHLT,13,tRET	;Read *
	dc.b	tHLT,14,tRET	;Write *
	dc.b	tHLT,15,tRET	;Get list device status *
	dc.b	tHLT,16,tRET	;Sector translation *

*
* Fake Disk Parameter Block
*
fakedpb	dc.b	11,0		;SPT (sectors per track)
	dc.b	4		;BSH (block shift to record number)
	dc.b	15		;BLM (block number mask to record no.)
	dc.b	0		;EXM (logical->physical extent shift)
	dc.b	439&255,439/256	;DSM (highest allocation block number)
	dc.b	255,0		;DRM (highest directory entry number)
	dc.b	$F0,0		;AL0, AL1 (initial allocation vector)
	dc.b	64,0		;CKS (size of directory check area)
	dc.b	0,0		;OFF (offset, number of reserved tracks)

*
* Fake Disk Block Allocation Table
*
fakealv	dcb.b	21,$FF
	dc.b	%11111100
	dcb.b	10,0

fdoslen equ	*-fdos

*
* BDOS function vector table
*
	cnop	0,4
bdostab dc.l	bdos00,bdos01,bdos02,bdos03,bdos04,bdos05,bdos06,bdos07
	dc.l	bdos08,bdos09,bdos10,bdos11,bdos12,bdos13,bdos14,bdos15
	dc.l	bdos16,bdos17,bdos18,bdos19,bdos20,bdos21,bdos22,bdos23
	dc.l	bdos24,bdos25,bdos26,bdos27,bdos28,bdos29,bdos30,bdos31
	dc.l	bdos32,bdos33,bdos34,bdos35,bdos36
bdostabn:

*
* BIOS function vector table
*
	cnop	0,4
biostab dc.l	bdosfn,bios01,bios02,bios03,bios04,bios05,bios06,bios07
	dc.l	bios08,bios09,bios10,bios11,bios12,bios13,bios14,bios15
biostabn:

null	dc.b	0		;Null string
	page
*************************************************************************
*									*
*	Variable storage						*
*									*
*************************************************************************

	bss	bss

* File information block - must be on a 4-byte boundary!
fib:
fibkey	ds.l	1
fibtype ds.l	1	;Type (file if negative, directory if positive)
fibname ds.b	108	;File name (null-terminated)
fibprot ds.l	1	;Protection mask
fibent	ds.l	1
fibsize ds.l	1	;Number of bytes in file
fibblks ds.l	1	;Number of blocks in file
fibdays ds.l	1	;Date stamp - number of days since Jan. 1, 1978
fibmins ds.l	1	;Date stamp - number of minutes past midnight
fibtick ds.l	1	;Date stamp - number of ticks past minute
fibcmt	ds.b	116	;Comments (null-terminated)

* InfoData structure
InfoData:
id_NumSoftErrors ds.l	1	;number of soft errors on disk
id_UnitNumber	 ds.l	1	;Which unit disk is (was) mounted on
id_DiskState	 ds.l	1	;See defines below
id_NumBlocks	 ds.l	1	;Number of blocks on disk
id_NumBlocksUsed ds.l	1	;Number of block in use
id_BytesPerBlock ds.l	1
id_DiskType	 ds.l	1	;Disk Type code
id_VolumeNode	 ds.l	1	;BCPL pointer to volume node
id_InUse	 ds.l	1	;Flag, zero if not in use
id_SIZEOF	 equ	36

* Miscellaneous storage areas
savesp	ds.l	1		;Stack pointer save area
_SysBase ds.l	1		;Copy of _AbsExecBase
_DOSBase ds.l	1		;Pointer to dos.library
stdin	ds.l	1		;Keyboard handle (stdin)
stdout	ds.l	1		;Screen handle (stdout)
rawhand ds.l	1		;RAW: file handle
prthand ds.l	1		;PRT:RAW file handle
handles ds.l	8*4		;File handle (or zero) plus 12 bytes of FCB
handlen:			;End of file handle table
dmaaddr ds.l	1		;Current DMA address
comend	ds.l	1		;End of .COM file name on command line
dtstamp ds.l	3		;Date and time stamp
rpport	ds.l	1		;Serial input message port
wpport	ds.l	1		;Serial output message port
baud	ds.l	1		;New baud rate for "setbaud"
bits	ds.b	1		;Number of data bits
charin	ds.b	1		;Current input character
charinb ds.b	1		;Serial input buffer
charout ds.b	1		;Current output character
frstset ds.b	1		;$80 after first call to "setbaud"
cmdline ds.b	128		;Command line
cmdlinen:			;End of command line
comname ds.b	20		;Name of file to load
comnamen:			;End of file name
opnname ds.b	24		;File name for OPEN or RENAME
renname ds.b	24		;New file name for RENAME
srchnam ds.b	11		;CP/M file name for search first/next
ext17	ds.w	1		;Extent counter for search first/next
newrega ds.b	1		;BIOS/BDOS accumulator work area
workbuf ds.b	80		;Work buffer for "pstring" (including $)
workbufn:			;End of work buffer
strbuf	ds.b	2048		;String output buffer
strbufn ds.b	8		;"strbuf" overflow area - must follow "strbuf"!
strptr	ds.l	1		;Current position in "strbuf"
escbuf	ds.b	8		;Translated escape sequence
esclen	ds.w	1		;Number of bytes saved in "escbuf"
cmdflag ds.b	1		;Take program name from command line.
quitflg ds.b	1		;"quitprg" exit flag
testdol ds.b	1		;"pstring" should test for leading $
insflag ds.b	1		;We're in insert mode.
dumpcnt ds.b	1		;"dump" counter for pausing
traceit ds.b	1		;-t (trace) flag was set on command line
btrcflg ds.b	1		;Trace BIOS/BDOS calls.
bufflag ds.b	1		;Console output is buffered.
z80flag ds.b	1		;Display Z-80 mnemonics in instruction trace.
opcode	ds.b	1		;Current opcode (used for tracing)
prefix	ds.b	1		;Instruction prefix (DD or FD)
fcbptr	ds.l	1		;Pointer to current FCB
listopn ds.b	1		;The list device is open.
builtin ds.b	1		;1 = USER command, 2 = SAVE command
acmap	ds.w	1		;Active drive map
romap	ds.w	1		;Read-only map
newdmap ds.w	1		;Map bit from "mapdrv"

*
* Serial port read request
*
	ds.l	0
readreq 	;struct IOExtSer
		;struct IOStdReq
		;struct Message
	ds.b	14	;struct Node
r_ReplyPort	ds.l 1	;Pointer to MsgPort (message reply port)
r_MLength ds.w	1	;Message length in bytes
		; End of struct Message
	ds.l	1	;Pointer to device node
	ds.l	1	;Pointer to Unit (driver private)
r_Command ds.w	1	;Device command
	ds.b	1	;io_Flags
	ds.b	1	;Error or warning number
		; End of struct IOReq - IOStdReq continues...
r_Actual ds.l	1	;Actual number of bytes transferred
r_Length ds.l	1	;Requested number of bytes transferred
r_Data	ds.l	1	;Points to data area.
r_Offset ds.l	1	;Offset for block-structured devices
		; End of struct IOStdReq
r_CtlChar ds.l	1	;control char's (order = xON,xOFF,INQ,ACK)
r_RBufLen ds.l	1	;length in bytes of serial port's read buffer
r_ExtFlags ds.l 1	;additional serial flags (see bitdefs below)
r_Baud	ds.l	1	;baud rate requested (true baud)
	ds.l	1	;duration of break signal in MICROseconds
		;struct IOTArray termination character array
r_ReadLen ds.b	1	;bits per read character (# of bits)
r_WriteLen ds.b 1	;bits per write character (# of bits)
r_StopBits ds.b 1	;stopbits for read (# of bits)
r_SerFlags ds.b 1	;see SerFlags bit definitions below
r_Status ds.w	1	;status of serial port
rsize	equ	*-readreq

*
* Serial port write request
*
	ds.l	0
writreq 	;struct IOExtSer
		;struct IOStdReq
		;struct Message
	ds.b	14	;struct Node
w_ReplyPort	ds.l 1	;Pointer to MsgPort (message reply port)
w_MLength ds.w	1	;Message length in bytes
		; End of struct Message
	ds.l	1	;Pointer to device node
	ds.l	1	;Pointer to Unit (driver private)
w_Command ds.w	1	;Device command
	ds.b	1	;io_Flags
	ds.b	1	;Error or warning number
		; End of struct IOReq - IOStdReq continues...
w_Actual ds.l	1	;Actual number of bytes transferred
w_Length ds.l	1	;Requested number of bytes transferred
w_Data	ds.l	1	;Points to data area.
w_Offset ds.l	1	;Offset for block-structured devices
		; End of struct IOStdReq
w_CtlChar ds.l	1	;control char's (order = xON,xOFF,INQ,ACK)
w_RBufLen ds.l	1	;length in bytes of serial port's read buffer
w_ExtFlags ds.l 1	;additional serial flags (see bitdefs below)
w_Baud	ds.l	1	;baud rate requested (true baud)
	ds.l	1	;duration of break signal in MICROseconds
		;struct IOTArray termination character array
w_ReadLen ds.b	1	;bits per read character (# of bits)
w_WriteLen ds.b 1	;bits per write character (# of bits)
w_StopBits ds.b 1	;stopbits for read (# of bits)
w_SerFlags ds.b 1	;see SerFlags bit definitions below
w_Status ds.w	1	;status of serial port, as follows:
wsize	equ	*-writreq
*		   BIT	ACTIVE	FUNCTION
*		    0	 low	busy
*		    1	 low	paper out
*		    2	 low	select
*		    3	 low	Data Set Ready
*		    4	 low	Clear To Send
*		    5	 low	Carrier Detect
*		    6	 low	Ready To Send
*		    7	 low	Data Terminal Ready
*		    8	 high	read overrun
*		    9	 high	break sent
*		   10	 high	break received
*		   11	 high	transmit x-OFFed       
*		   12	 high	receive x-OFFed       
*		13-15		reserved

*************************************************************************
*									*
*	Target processor's address space				*
*									*
*************************************************************************

	even

registers ds.b	22		;Actual storage for Z-80's other registers
target	ds.b	$10000		;Z-80's universe

	end
