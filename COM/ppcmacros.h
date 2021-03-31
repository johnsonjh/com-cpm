/*
** This file is the Macro header file for creating inline NEXT's
** for the simulation, and for globally defining constants and equates.
*/

/* This message is buried in the .S file as a warning against editing it. */
|*************************************************************************
|*************************************************************************
|**									**
|**	This file created from a file xxxx.MAC using CPP as a macro	**
|**	preprocessor.  WARNING -- Changes to this file are not saved!	**
|**									**
|*************************************************************************
|*************************************************************************

/* TUNEABLE #defines and other flags. */
/* Which mode is the simulator to run in?  Pick ONE of the two below, */
/* which should be asserted in the Makefile. */

/*#define NORMAL	/* Normal (no tracing, inline NEXT. */
/*#define TRACE	/* Trace code is desired (centralized NEXT). */

|
|	Register definitions for the simulation.
|
#define anchorreg r31
#define opptr r30
#define pseudopc r29
#define pseudosp r28
#define lflagptr r27
#define aflagptr r26
#define sflagptr r25
#define sumptr r24
#define targbase r23
#define regs r23
#define regconm1 r22
#define regcon01 r21
#define regf r20
#define rega r19
#define ret1 r18
#define ret2 r17

|
|	Note, only leaves R0/R3-R18 for free use by entire
|	program without saving registers for temporary use.  R3
|       is generally the 'accumulator' (as was D0 in the 68K code)
|       because there are too many restrictions on R0.  R0 is used
|	as the secondary accumulator/scratch register, R4 as tertiary.
|	R17-R18 are used to save LR during internal subroutine calls.
|	(They could go on the stack, but PPC stack frames are tricksy.
|	 The PPC calling convention preserves R13+ inside functions.)
|

|
|	Offsets from register base pointer for 8080/Z-80 pseudo-registers.
|
cycles = -100
opcntrs = -96
opcntrs1 = -92
opcntrs2 = -88
opcntrs3 = -84
opcntrs4 = -80
opcntrs5 = -76
opcntrs6 = -72
trcflag = -66
trcbegin = -64
trcend = -60

pppctmp = -52
pppc = -48
ppc = -44		| These are used only when calling C functions,
psp = -40		| normally these 'registers' are in PPC registers.
badflag = -36
running = -35
spchi = -34		| (The 's' is for 'shadow'.)
spclo = -33
ssphi = -32
ssplo = -31
srega = -30
sregf = -29
regr = -28
regiff = -27
seed = -24

regb = -22
regc = -21
regd = -20		| A, F and Pseudo-PC and -SP are in PPC Registers.
rege = -19
regh = -18
regl = -17
regix = -16		| IX
regxh = -16
regxl = -15
regiy = -14		| IY
regyh = -14
regyl = -13
regb2 = -12		| Alternate register set for Z-80
regc2 = -11
regd2 = -10
rege2 = -9
regh2 = -8
regl2 = -7
rega2 = -6
regf2 = -5
regi = -4		| Interrupt page address register (I)
regop1 = -3		| Operand 1 for DAA storage.
regop2 = -2		|    "    2  "   "     "
regop3 = -1		|    "    3  "   "     "

/*
** Define the NEXT macro and jump table entry size appropriately for
** the environment and what we wish to do.  This is messy, but results
** in the fastest opcode dispatching for the given environment.
*/

/* First define the inline next code. */
#define entry .long
#define inext lbzux r3,pseudopc,regcon01 | inline-nxt-instr;\
	slwi r3,r3,2;\
	lwzx r0,r3,opptr;\
	mtctr r0;\
	bctr

#ifdef Z80
#  define inextr3 lbzux r4,pseudopc,regcon01;\
	slwi r4,r4,2;\
	lwzx r0,r4,r3;\
	mtctr r0;\
	bctr
#endif /* Z80 */

#ifdef TRACE
#  define next	b mloop
#  ifdef Z80
#    define nextr3 inextr3
#    define znexta0 zinexta0
#  endif /* Z80 */
#else /* not TRACE */
#  define next inext
#  ifdef Z80
#    define nextr3 inextr3
#    define znexta0 zinexta0
#  endif /* Z80 */
#endif /* FAST */

/* Byte-swapping Get address and Put address. */
#define getaddr(addrarg)\
	lhbrx r3,addrarg,regcon01
#define getaddr2(addrarg)\
	lhbrx r3,addrarg,regcon01;\
	addi addrarg,addrarg,2
#define putaddr	sthbrx r0,pseudosp,regconm1;\
	subi pseudosp,pseudosp,2

#define inrflag(r) rlwinm r0,r,8,16,23;\
	or r,r0,regcon01;\
	lbzx r0,r,aflagptr;\
	rlwimi regf,r0,0,24,30;\
	lbzx r,r,sumptr;\

#define dcrflag(r) rlwinm r0,r,8,16,23;\
	or r,r0,regcon01;\
	lbzx r0,r,sflagptr;\
	xori r,r,0xFF;\
	oris r,r,1;\
	rlwimi regf,r0,0,24,30;\
	lbzx r,r,sumptr

#define RLCM(r) rlwimi regf,r,25,31,31;\
	rlwinm r,r,1,24,31;\
	rlwimi r,regf,0,31,31

#define RRCM(r) rlwimi regf,r,0,31,31;\
	rlwimi r,regf,8,23,23;\
	srawi r,r,1

#define RALM(r) add r,r,r;\
	rlwimi r,regf,0,31,31;\
	rlwimi regf,r,24,31,31
	
#define RARM(r) insrwi r,regf,1,23;\
	insrwi regf,r,1,31;\
	srawi r,r,1

#define SLAM(r) rlwimi regf,r,25,31,31;\
	rlwinm r,r,1,24,31

#define SRAM(r) rlwimi regf,r,0,31,31;\
	rlwimi r,r,31,25,31

#define SRLM(r) rlwimi regf,r,0,31,31;\
	rlwinm r,r,31,24,31

#define addflags rlwimi r3,rega,8,16,23;\
	lbzx regf,r3,aflagptr;\
	lbzx rega,r3,sumptr

#define cmpflags rlwimi r3,rega,8,16,23;\
	lbzx regf,r3,sflagptr;

#define subflags cmpflags;\
	xori r3,r3,0xFF;\
	oris r3,r3,1;\
	lbzx rega,r3,sumptr

#define adcflags rlwimi r3,rega,8,16,23;\
	rlwimi r3,regf,16,15,15;\
	lbzx regf,r3,aflagptr;\
	lbzx rega,r3,sumptr

#define sbbflags rlwimi r3,rega,8,16,23;\
	rlwimi r3,regf,16,15,15;\
	lbzx regf,r3,sflagptr;\
	xori r3,r3,0xFF;\
	xoris r3,r3,1;\
	lbzx rega,r3,sumptr

#ifdef Z80			/* Jump Relative, etc.  (Z-80) */
#  define calcind(x)	lbzux r0,pseudopc,regcon01;\
	extsb r0,r0;\
	lhz r3,x(regs);\
	add r0,r0,r3

#  define setapflags(r)	lbzx r0,lflagptr,r;\
	rlwimi regf,r0,0,24,30

#  define dozf(x)	beq zset##x;\
	andi. regf,regf,0xBF

#endif /* Z80 */
