
;   Keyboard Controller Version 2.0
;
;   Author:  Jim Cathey 	3/30/84
; 
;   This is the firmware for an 8085 based serial keyboard.
;   The hardware is 1 8085, 1 2716, 2 8155's, and 1 8251.  It 
;   features autorepeat, a 240 character buffer, a keyboard macro
;   facility, and 256 characters of macro storage.  It features
;   polled operation (with typeahead) or standard operation
;   (without typeahead).  Macro lookup may be disabled, and a
;   META key may be enabled (8th bit set) under key control.
;   The keyboard is a modified Keytronic 65-1655 word processing
;   keyboard.


;  Port Equates ...

SERCMD	EQU	09H	; 8251 command port
SERDAT	EQU	08H	; 8251 data port
P1CMD	EQU	10H	; 8155 #1 command port
P1ADAT  EQU	11H	;  "   "  A data port
P1BDAT	EQU	12H	;  "   "  B data port
P1CDAT	EQU	13H	;  "   "  C data port
P1TLO	EQU	14H	;  "   "  Timer, low
P1THI	EQU	15H	;  "   "  Timer, high
P2CMD	EQU	18H	; 8155 #2 command port
P2ADAT	EQU	19H	;  "   "  A data port
P2BDAT	EQU	1AH	;  "   "  B data port
P2TLO	EQU	1CH	;  "   "  Timer, low
P2THI	EQU	1DH	;  "   "  Timer, high
;
; Miscellaneous Equates
;
RAM1	EQU	1000H	; Start of RAM area #1
RAM2	EQU	1800H	;   "   "   "   "   #2
RAM1TP	EQU	1100H	; End of RAM #1 +1
RAM2TP	EQU	1900H	; End of RAM #2 +1
STACK	EQU	RAM2TP	; Stack at end of RAM 2
BUFST	EQU	RAM2	; Start of typeahead buffer.
BUFEND	EQU	RAM2TP-RAM2-32	; End of buffer.
MACBUF	EQU	RAM1	; Keyboard macro buffer.
BPTIME	EQU	40	; 1/5 Second beep time.
RTIM1	EQU	100	; 1/2 Sec delay to repeat start.
RTIM2 	EQU	20	; 1/10 Sec initial repeat time.
RTIM3	EQU	10	; 1/20 Sec final repeat time.
T100MS	EQU	20	; 100 msec power on beep.

BON	EQU	8	; Beeper on -- C port data.
BOFF	EQU	0	; Beeper off
ACK	EQU	0FEH	; Acknowledge		; Communication constants and
NAK	EQU	0FFH	; No-Acknowledge	; buffer flags.  Don't change!

BAUDIV	EQU	15 	; Divisor for 9600 baud.
;	  		= 4.608MHz/16/2/9600

; Port contents -- keys

KMSG	EQU	1	; MSG key   = bit 0 P1A  Do META key lock for EDIT.
KCONV	EQU	2	; CONV key  = bit 1  "	 Do macro lookups.
KPAGE	EQU	4	; PAGE key  = bit 2  "   Lock number pad in control mode.
KLOCAL	EQU	8	; LOCAL key = bit 3  "   Do typeahead buffering.

KSHIFT	EQU	1	; SHIFT keys = bit 1 P2A
KCTRL	EQU	2	; CTRL keys  = bit 2  "
KEDIT	EQU	4	; EDIT keys  = bit 3  "
KYELLO	EQU	8	; YELLOW key = bit 4  "
KCAPSL	EQU	16	; CAPS lock  = bit 5  "
KREPT	EQU	32	; REPT key   = bit 6  "
KAKD	EQU	64	; any key    = bit 7  "


  	ORG	RAM2TP-32	; RAM storage.

TIMER:	DS	2	; Repeat timer.
BTIMR:	DS	2 	; Beeper timer.
TIMLOD:	DS	2	; Timer reload storage.
RTCNT:	DS	1	; Repeat timer count for increment.
QHEAD:	DS	2	; Queue head pointer.
QTAIL:	DS	2 	; Queue tail pointer.
LASTCH:	DS	1	; Last typed char (for repeat).

	ORG	0
RESET:	MVI	A,0CEH	; Init USART --
	OUT	SERCMD	; 2 stop, no parity, x16 clock async,
	MVI	A,15H	; Enable Rx, Tx, clear errors.
	OUT	SERCMD
	MVI 	A,0EDH	; 5msec pulses on timer #1, main clock
	OUT	P1THI	; interrupt for beep & repeat times.
	MVI	A,0	; Don't start yet.  (15360 decimal)
	OUT	P1TLO
	MVI	A,0D4H	; Start timer, 'A' interrupts.
	OUT	P1CMD	; Alt mode 3, A=B=IN on #1
	MVI	A,BAUDIV/256+40H	; x16 Square wave.
	OUT	P2THI	; ON TIMER #2. (Baud clock)
	MVI	A,BAUDIV AND 0FFH
	OUT	P2TLO
	MVI	A,0C0H	; Start timer, no interrupts.
	OUT	P2CMD	; ALT mode 1, A=B=input
	JMP	INIT2	; Continue elsewhere, past RST locs.


; Interrupt Service Locations.

	ORG	2CH	; RST 5.5   	Timer interrupt.
	JMP	TIMINT

	ORG	34H	; RST 6.5 	Serial input char interrupt.
	JMP	SERINT

	ORG	38H	; RST 7  	( Block RST 7 instructions )
	DI		;		( which fill all unused ROM )
	JMP	ISOOPS

	ORG	3CH	; RST 7.5	Keyboard interrupt
	JMP	KEYINT	;		on char typed.

	ORG	100H	; More code here.
INIT2:	LXI	H,0	; Clear repeat timer
	SHLD	TIMER
	LXI	H,T100MS	; 100msec beep on power-up.
	SHLD	BTIMR

	LXI	SP,STACK
	CALL	CLRQ	; Clear the typeahead buffer.
	CALL	KILMACS	; Clear the macro buffer.
	MVI	A,58H	; Enable interrupts.
	DB	30H	; SIM instruction.
	EI
	JMP	START	; and GO!

;  Main loop checks for no keys pressed and stops repeat
;  timer if so.  All other tasks are interrupt driven.

START:	IN	P2ADAT	; Check for falling edge.
START2:	ANI  	KAKD	; of keydown flag.
	JZ 	START	; Found leading edge?
	IN	P2ADAT
	ANI	KAKD
	JNZ	START2	; Found falling edge?
	LXI	H,0
	SHLD	TIMER	; Yes, clear repeat timer.
	JMP	START	; Go back	

;
;   Clear the typeahead queue.  Called by SCOTCH
;   command and by the initialization routine.
; 
CLRQ:	LXI	H,BUFST
	SHLD	QHEAD	; Clear by setting head & tail
	SHLD	QTAIL	; to same place.
	RET


;
;   Enqueue the 'A' character if possible.  Return a
;   NAK and beep bell if queue is full, discarding
;   the character.  Called by SHIPIT.
;
ENQ:	PUSH	H
	PUSH	PSW	; Save state
	LHLD	QTAIL
	XCHG
	LHLD	QHEAD	; Look for end of buffer.
	INX	H	; Bump pointer.
	MVI	A,BUFEND
	CMP	L
	JNZ	ENQ1	; No
	LXI	H,BUFST	; Yes, wrap around to beginning.
ENQ1:	MOV	A,L
	CMP	E	; Check for equal pointers.
	JZ	QFULL
	XCHG		; BUFFERS<=256 CHARS ONLY!!!!!!!!!
	LHLD	QHEAD	; Get old head pointer
	POP	PSW	; and char to enqueue.
	MOV	M,A	; Do so.
	XCHG		; Retrieve new calculated pointer
	SHLD	QHEAD	; and store it.
	POP	H
	RET

QFULL:	POP	PSW	; Clean stack.
	LXI	H,BPTIME
	SHLD	BTIMR	; Ring bell
	MVI	A,NAK	; if queue is full,
	POP	H	; and return NAK.
	RET

;
;   Get a character from the queue if possible.
;   Return a NAK if queue is empty.  Called by
;   SERINT subcommand NXTCHR.
;
DEQ:	LHLD	QHEAD
	XCHG
	LHLD	QTAIL
	MOV	A,L
	CMP	E	; Are pointers equal?
	MVI	A,NAK	; Get NAK now.
	RZ		; Return NAK if empty,
	MOV	D,M	; else get char.
	INX	H
	MVI	A,BUFEND	; Check for end of bufr
	CMP	L
	JNZ	DEQ1	; No
	LXI	H,BUFST	; Yes, wrap around.
DEQ1:	SHLD	QTAIL
	MOV	A,D	; Get char
	RET		; and return.


;
;   Search learn buffer for 'A' character.  Leave HL
;   pointing at it if found, or at $FF terminator.
;   Called by SHIPIT and SERINT subcommand LEARN.
;
SEARCH:	MOV	B,A	; Tuck away char.
	LXI	H,MACBUF
	CMP	M	; Norman?
	RZ
SRLOP1:	CALL	SRLOOP	; Search for next terminator.
	CPI	NAK	; At end of table?
	RZ		; Yes, quit.
	INR	L	; Else is a $FE separator.
	MOV	A,M	; Bump to next and check it.
	CMP	B	; Is it our baby?
	RZ
	JMP	SRLOP1	; No, try next one.

SRLOOP:	MOV	A,M
	CPI	ACK	; Is char a terminator?
	RNC		; No.
	INR	L
	JNZ	SRLOOP	; Didn't fall off end, keep looking.
	DCR	L
	RET		; Make sure pointer is in table.


;
; 5 msec interrupt timer handler.  Takes care of
; the repeat and beeper timers, and of emptying the
; typeahead buffer if the 'LOCAL' key is not down.
; Called by RST 5.5.
;
TIMINT:	PUSH	PSW	; Save state.
	PUSH	D
	PUSH	H
	IN	P1ADAT	; Reset timer interrupt
	LHLD	BTIMR	; every 5 msec.
	MOV	A,H	; Decrement timer.
	ORA	L	; If not zero only
	JZ	NOBEEP
	DCX	H	; Do the decrement.
	SHLD	BTIMR
	MVI	A,BON	; Turn on beeper.
BP1:	OUT	P1CDAT
	JMP	RPT

NOBEEP:	MVI	A,BOFF	; Shut off beeper.
	JMP	BP1

RPT:	LHLD	TIMER	; Handle repeat timer.
	MOV	A,H
	ORA	L	; Decrement if not zero.
	CNZ	NOTOFF
	IN	P1ADAT	; Check to see if we must empty
	ANI	KLOCAL	; typeahead buffer steadily.
	JZ	TIMEND	; 'LOCAL' key?  Don't empty buffer.
	IN	SERCMD	; Check for ready, don't if not.
	ANI	1
	JZ	TIMEND
	CALL	DEQ	; Is there a char to send?
	CPI	NAK
	JZ	TIMEND	; No, skip sending NAK,
	OUT	SERDAT	; else ship it.

TIMEND:	POP	H	; Return from all timer ops.
	POP	D
	POP	PSW
	EI
	RET

NOTOFF:	DCX	H	; Decrement timer.
	MOV	A,H
	ORA	L	; Detect edge of zero state.
	JZ	LODTMR
	SHLD	TIMER	; No
	RET

LODTMR:	LHLD	TIMLOD	; Reload timer.
	SHLD	TIMER
	LDA	RTCNT	; Get rate count.
	ORA	A
	JZ	FASTRP	; If zero then we're already at
	DCR	A	; fast rate, else decrement rate.
	STA	RTCNT
	DCX	H	; Decrement reload value
	SHLD	TIMLOD	; so succeeding repeats faster.
FASTRP:	LDA	LASTCH	; Finally do the repeat itself
	CALL	SHIPIT	; and put in output buffer.
	RET


;
;   Take translated character and look up any
;   aliases in the macro buffer and enqueue
;   all the chars in the definition, if any.
;   called by KEYINT and TIMINT sections.
;
SHIPIT:	MOV	B,A
	IN	P1ADAT	; In lookup mode?
	ANI	KCONV	; ( CONV key )
	MOV	A,B
	JNZ	ENQ	; No, directly enqueue it,
	CALL	SEARCH	; else look for it.
	CPI	NAK	; Found?
	MOV	A,B
	JZ	ENQ	; Yes, enqueue definition.
SHLOOP:	INX	H
	MOV	A,M
	CPI	ACK	; Look for terminator at end.
	RNC		; $FE OR $FF WILL DO.
	CALL	ENQ
	JMP	SHLOOP


;   Take raw char from keyboard and translate to
;   ASCII, set repeat timer, and handle the
;   'BREAK' character.  Enqueues the resultant
;   character, if any.  Called by RST 7.5.
;
KEYINT:	PUSH	PSW
	PUSH	H
	IN	P2BDAT	; Get raw key.
	CMA		; Invert to proper sense.
	ANI	7FH	; Mask to 7 bits.
	CPI	10H	; Is it a shifting key?
	JC	SHFTOP
	CPI	2FH	; No, 'BREAK'?
	JNZ	NOBRK
	LXI	H,0	; Yes
	SHLD	TIMER	; Stop autorepeat if running.
	MVI	A,ACK
	CALL	SENDCH	; Send $FE, bypass buffering!
	JMP	SHFTOP	; and quit.

NOBRK:	CALL	LOOKUP	; Normal char, lookup keycode.
	STA	LASTCH	; Save for autorepeat.
	CALL	SHIPIT	; Ship char (or macro)
	LXI	H,RTIM1	; Start autorepeat timer.
	SHLD	TIMER
	LXI	H,RTIM2	; Set slowest reload rate.
	SHLD	TIMLOD
	MVI	A,RTIM2-RTIM3
	STA	RTCNT	; Set repeat accel counter.
	IN	P2ADAT	; Is REPEAT key down?
	ANI	KREPT
	JZ	SHFTOP	; No,
	LXI	H,RTIM3	; else reload at max repeat rate.
	SHLD	TIMLOD
	SHLD	TIMER	; From the start.
	MVI	A,0
	STA	RTCNT

SHFTOP:	POP	H
	POP	PSW
	EI
	RET


;   Handle request from host system for service.
;   Ignore errors on serial input line.
;   Also ignore command # larger than maximum.
;   Called by RST 6.5.
;
SERINT:	PUSH	PSW
	PUSH	H
	LXI	H,SHFTOP	; Return address.
	PUSH	H
	IN	SERCMD
	ANI	38H	; Errors?
	JZ	SEROK
	IN	SERDAT	; Yes, clear register.
	MVI	A,15H	; Reset errors.
	OUT	SERCMD
	RET		; Return to SHFTOP return point.

SEROK:	IN	SERDAT	; Get command.
	LXI	H,CMDTAB
	CPI	MAXCMD	; Too big?
	RNC		; Yes, quit.
	ADD	A	; Double it.
	MOV	E,A
	MVI	D,0
	DAD	D	; Index into table.
	MOV	E,M	; Find routine address --
	INX	H
	MOV	D,M
	XCHG
	PCHL		; and execute it.

;
;   Defined commands:
;
;   0	Enquire for next character.
;   1	Sound bell.
;   2	Clear typeahead buffer.
;   3   Empty the Macro definition buffer.
;   4	Learn following definition.
;
CMDTAB:	DW	NXTCHR
	DW	DOBEEP
	DW	SCOTCH
	DW	KILMACS
	DW	LEARN
;
MAXCMD	EQU	5


NXTCHR:	CALL	DEQ	; Get char if any.
	CALL	SENDCH	; Send it.
	RET

DOBEEP:	LXI	H,BPTIME
	SHLD	BTIMR	; Start beeper.
	RET

SCOTCH:	CALL	CLRQ	; Empty buffer.
	RET

KILMACS: MVI	A,NAK	; Dump MACRO buffer
	STA	MACBUF	; by putting term. at end.
	RET
;
;   Learn the definition of the following character.
;   The definition is the characters following the
;   redefined character until a terminating $FF or
;   until the learn buffer is full.  Keyboard
;   processing is suspended during the learn
;   sequence.  If the definition is of null length
;   the def. is removed, likewise a redefinition
;   removes the old definition first.
;
LEARN:	CALL	GETCH	; Get char to learn def for.
	PUSH	PSW	; Save it.
	CALL	SEARCH	; Find any old def, or the
	CPI	NAK	; end of the buffer.
	JZ	NODUP
	DCX	H	; Point to preceding delimiter.
	MOV	E,L
	MOV	D,H
DELOOP:	INX	D	; Find end of this old def.
	LDAX	D
	CPI	ACK	; At delimeter.
	JC	DELOOP	; No, keep looking.
	CPI	NAK	; If last one then no delete needed.
	JZ	NODUP
	MVI	C,0
	MOV	B,E
DELOP1:	LDAX	D	; Now how long is rest of table?
	INX	D
	INR	C	; ( in C )
	CPI	NAK	; Look for terminator.
	JNZ	DELOP1
	DCX	D	; Leave DE at delim after def.
	MOV	E,B
DELOP2:	LDAX	D
	MOV	M,A	; Move rest of table down --
	INX	D
	INX	H
	DCR	C
	JNZ	DELOP2	; until all moved.
	DCX	H	; HL at delimiter at end of table.
NODUP:	MOV	A,L	; Room for any new def?
	CPI	ACK-1
	JNC	NOROOM
	MVI	M,ACK	; New separator at end of table.
	INX	H
	POP	PSW	; Get char 'NAME.'
	MOV	M,A	; Save in table.
	INX	H
	CALL	GETCH	; Now see if a null def, get char.
	CPI	NAK	; If end then erase name.
	JNZ	INSLP1
	DCX	H	; Point to separator.
	DCX	H
	MVI	M,NAK	; New terminator.
	RET

INSLOP:	CALL	GETCH	; Get char in definition.
INSLP1:	MOV	M,A	; Put into table.
	CPI	ACK	; Don't insert separators!
	JZ	INSLOP
	INX	H	; Else OK.
	CPI	NAK	; A terminator?
	RZ		; Yes, all done!
	MVI	A,NAK
	CMP	L	; End of table space?
	JNZ	INSLOP	; No, go for more.
	MOV	M,A	; Make sure a terminator is there
	JMP	NORLOP	; in the table, then swallow
NOROOM:	POP	PSW	; rest of definition!
NORLOP:	CALL	GETCH
	CPI	NAK
	JNZ	NORLOP	; Until terminating NAK
	RET

;
;   Get a character from the serial line
;   in a polled manner.  Called by LEARN
;   for getting the definition.
;
GETCH:	IN	SERCMD
	ANI	2	; Errors?
	JZ	GETCH	; Yes, ignore.
	IN	SERDAT	; Get data.
	RET

;
;   Send the 'A' character out the serial line.
;   Wait for ready first.  Called by SERINT
;   subcommand NXTCHR, the 'BREAK' handler, and
;   the TIMER routine if not buffering.
;
SENDCH:	MOV	B,A
SENDLP:	IN	SERCMD	; Poll for ready.
	ANI	1
	JZ	SENDLP
	MOV	A,B	; Ship it.
	OUT	SERDAT
	RET

;
;   Error routine, called upon RST 7
;   halts the processor with the beeper
;   on to flag an error.  We should
;   never get here!
;
ISOOPS:	MVI	A,BON
	OUT	P1CDAT
	DI
	HLT
	JMP	ISOOPS	; Just in case.
;
;   Do the actual translation from raw keyboard
;   character to ASCII.  Called by KEYINT.
;
LOOKUP:	SUI	10H	; No typing chars are < $10
	MVI	D,0	; Table index.
	MOV	E,A
	IN	P2ADAT	; Do some 'SHIFT' handling first.
	ANI	KYELLO	; CRT (YELLOW) key?
	JNZ	SCRKEY	; Yes, always a SCREEN control key.
	IN	P2ADAT
	ANI	KSHIFT	; SHIFT key?
	JNZ	SCRKEY	; Yes, always a SCREEN key.
	IN	P1ADAT
	ANI	KPAGE	; 'SCREEN LOCK' key?
	JNZ	NOCRT	; Yes, always a SCREEN key.
SCRKEY:	LXI	H,SCRTAB	; SCREEN key table.
	DAD	D
	MOV	A,M	; Try to look up SCREEN key.
	ORA	A
	RNZ		; If not zero then valid SCREEN key,
NOCRT:	IN	P2ADAT	; else do upr/lwr ASCII translate.
	ANI	KSHIFT	; SHIFT key.
	LXI	H,LOWTBL	; Lowercase table.
	JZ	LOWER	; Lower case?
	LXI	H,UPRTBL
LOWER:	DAD	D
	IN	P2ADAT
	ANI	KCAPSL	; CAPS lock?
	MOV	A,M
	JZ	LDONE	; No CAPS, then lookup done,
	CPI	'a'	; otherwise
	JC	LDONE	; check range for CAPS lock.
	CPI	'z'+1
	JNC	LDONE
	SUI	20H	; Lower case ==> upper case.
LDONE:	MOV	E,A	; Apply CTRL & EDIT functions.
	IN	P2ADAT
	ANI	KCTRL+KEDIT	; CTRL or EDIT down?
	MOV	A,E
	RZ		; No, all done.
	IN	P1ADAT	; META lock?
	ANI	KMSG
	JNZ	NOMETA	; No, process normally.
	IN	P2ADAT	; Is an EDIT or CTRL, META lock.
	ANI	KCTRL	; Is CTRL,
	JNZ	NOMETA	; process normally.
	MOV	A,E
	JMP	ISEDIT	; Is EDIT, & META lock -- skip case folding.
NOMETA:	MOV	A,E	; Is an EDIT or CTRL, non-META lock.
	CPI	'@'
	RC		; Check range, must be >$3F
	CPI	60H	; Lowercase?
	JC	CTRLED	; No, straight,
	SUI	20H	; else do LWR => UPR first,
CTRLED:	SUI	'@'	; then do CTRL folding.
	MOV	E,A
	IN	P2ADAT
	ANI	KEDIT	; EDIT key?
	MOV	A,E
	RZ		; No, done.
ISEDIT:	ADI	80H	; Yes, set high bit.
	CPI	ACK	; Make sure no ACK or NAK chars get out.
	RC
	ANI	7FH	; Clip them to normal ASCII just in case.
	RET

;
;   Lowercase lookup table.
;
LOWTBL:	DB	0,0,0,0,'3',8,'.,'		; Codes 10-17
	DB	'6',0,0,0,'28',13,'9'		; Codes 18-1f
	DB	'5',0,0,'1*-47'			; Codes 20-27
	DB	'+',0,'0',3,7BH,60H,7FH,0	; Codes 28-2f
	DB	'\',0,'/',13,27H,'=',10,8	; Codes 30-37
	DB	5BH,'./;l0p-'			; Codes 38-3f
	DB	'om,kj8i9'			; Codes 40-47
	DB	'u nhg6y7'			; Codes 48-4f
	DB	'tbvfd4r5'			; Codes 50-57
	DB	'ecxsa2w3'			; Codes 58-5f
	DB	'qz',0,0,1BH,13H,9,'1'		; Codes 60-67
	DB	11H,0,0,0,0,0,0,0		; Codes 68-6f

;
;   Uppercase lookup table.
;
UPRTBL:	DB	0,0,0,0,'3','8','.,'		; Codes 10-17
	DB	'6',0,0,0,'28',13,'9'		; Codes 18-1f
	DB	'5',0,0,'1*-47'			; Codes 20-27
	DB	'+',0,'0',3,7DH,7EH,7FH,0	; Codes 28-2f
	DB	7CH,0,'/',13,'"+',10,8		; Codes 30-37
	DB	5DH,'>?:L)P',5FH		; Codes 38-3f
	DB	'OM<KJ*I('			; Codes 40-47
	DB	'U NHG^Y&'			; Codes 48-4f
	DB	'TBVFD$R%'			; Codes 50-57
	DB	'ECXSA@W#'			; Codes 58-5f
	DB	'QZ',0,0,1BH,13H,9,'!'		; Codes 60-67
	DB	11H,0,0,0,0,0,0,0		; Codes 68-6f

;
; Screen control lookup table.
;
SCRTAB:	DB	0,0,0,0,0ADH,0AFH,0AEH,0D0H		; Codes 10-17
	DB	0ACH,0,0,0,0A9H,0A7H,0D1H,0ABH		; Codes 18-1f
	DB	0A8H,0,0,0A6H,0A2H,0A0H,0A5H,0A4H	; Codes 20-27
	DB	0A1H,0,0AAH,0,0,0,0,0		; Codes 28-2f
	DB	0,0,0A3H,0,0,0,0,0		; Codes 30-37
	DB	0,0,0,0,0,0,0,0			; Codes 38-3f
	DB	0,0,0,0,0,0,0,0			; Codes 40-47
	DB	0,0,0,0,0,0,0,0			; Codes 48-4f
	DB	0,0,0,0,0,0,0,0			; Codes 50-57
	DB	0,0,0,0,0,0,0,0			; Codes 58-5f
	DB	0,0,0,0,0,0,0,0			; Codes 60-67
	DB	0,0,0,0,0,0,0,0			; Codes 68-6f

	END
                                          