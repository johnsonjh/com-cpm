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
/* Which mode is the simulator to run in?  Pick ONE of the three below, */
/* which should be asserted in the Makefile. */

/*#define FAST	/* Fastest (no tracing, inline NEXT, short addressing). */
/*#define NORMAL	/* Normal (no tracing, long addr, centralized NEXT. */
/*#define TRACE	/* Trace code is desired (long addr, centralized NEXT). */

#ifdef FAST
/* MAGICNO is only used if FAST is #defined above, and on 68010's. */
/* This is a bias for a jump table of word offsets from address 0. */
/* Usage: opcode: .word subr-magicno; ...; movw opcode,a0; jmp magicno(a0) */
#define MAGICNO 0
magicno	= MAGICNO	| Magic offset to give us short addressing on 68010.
|			| Should be 0 for best speed if the target system can
|			| load the opcode subrs entirely under the 32K mark.
|			| Magicno MUST BE <$8000 for the code to work right.
|			| For a program at $8000, magicno = $4000 works well.
|			| For 68020+, there is no speed advantage to using
|			| short addressing, so MAGICNO should be 0.
#endif /* FAST */
|
|	Register definitions for the simulation.
|
#if GAS || UAS
#define return a6
#define pseudopc a5
#define opptr a4
#define pseudosp a3
#define flagptr a2
#define targbase a1
#define regs a1
#define regcon0e d7
#define regcon01 d6
#define regcon0f d5
#define regf d4
#define rega d3
#else
return:	.reg a6		| JMP (return) is fast return to MLOOP.
pseudopc: .reg a5	| 8080/Z-80 PC is register A5.
opptr:	.reg a4		| Pointer to opcode dispatch table.
pseudosp: .reg a3	| 8080/Z-80 SP is register A3.
flagptr: .reg a2	| Pointer to 8080/Z-80 flag lookup table is A2.
targbase: .reg a1	| Pointer to 8080/Z-80 address space is A1.
regs:	.reg a1		| Base pointer to 8080/Z-80 registers is A1.

regcon0e: .reg d7	| Register based constant #$E (for speed).
regcon01: .reg d6	| Register based constant #$1.
regcon0f: .reg d5	| Register based constant #$F.
|			|  Often used constants #0 & #8 are predominantly
|			|  used by instructions that have 'quick' modes
|			|  which encompass these values -- no registers
|			|  needed (or available, either).
regf:	.reg d4		| 8080/Z-80 Flags
rega:	.reg d3		| 8080/Z-80 Accumulator
#endif

|
|	Note, only leaves D0-D2/A0 for free use by entire
|	program without saving registers for temporary use.
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
psp = -40		| normally these 'registers' are in 68K registers.
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
regd = -20		| A & F are in Data Registers.
rege = -19		| Pseudo-PC is kept in an Address Register.
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

/* First define the inline next and the if-already-zero inline next code. */
#if MAGICNO
#  define entry .word
#  define zinext movb (pseudopc)+,d0;\
	addw d0,d0;\
	movw 0(opptr,d0:w),a0;\
	jmp magicno(a0)
#else /* no MAGICNO */
#  ifdef M68020
#    define entry .long
#    define zinext movb (pseudopc)+,d0;\
	jmp ([opptr,d0*4])
#  else /* not M68020 */
#    define entry .word
#    define zinext movb (pseudopc)+,d0;\
	addw d0,d0;\
	movw 0(opptr,d0:w),a0;\
	jmp (a0)
#  endif /* M68020 */
#endif /* MAGICNO */
#define inext moveq NO0,d0; zinext

#ifdef Z80
#  ifdef M68020
#    define zinexta0 movb (pseudopc)+,d0;\
	jmp ([a0,d0*4])
#  else /* not M68020 */
#    define zinexta0 movb (pseudopc)+,d0;\
	aslw NO2,d0;\
	movl 0(a0,d0:w),a0;\
	jmp (a0)
#  endif /* M68020 */
#  define inexta0 moveq NO0,d0; zinexta0
#endif /* Z80 */

#ifdef FAST
#  ifdef M68020
#    define next inext
#    define znext zinext
#    ifdef Z80
#      define nexta0 inexta0
#      define znexta0 zinexta0
#    endif /* Z80 */
#  else /* not M68020 */
#    define next inext
#    define znext inext
#    ifdef Z80
#      define nexta0 inexta0
#      define znexta0 inexta0
#    endif /* Z80 */
#  endif
#else /* not FAST */
#  define next jmp (return)
#  define znext jmp (return)
#  ifdef Z80
#    define nexta0 inexta0
#    define znexta0 zinexta0
#  endif /* Z80 */
magicno = 0
#endif /* FAST */

/* Byte-swapping Get address and Put address. */
#ifdef M68020
#  define getaddr(addrarg)\
	movw (addrarg),d0;\
	rolw NO8,d0
#  define getaddr2(addrarg)\
	movw (addrarg)+,d0;\
	rolw NO8,d0
#  define putaddr	rolw NO8,d1;\
	movw d1,-(pseudosp)
#else /* not M68020 */
#  define getaddr(addrarg)\
	movb 1(addrarg),-(sp);\
	movw (sp)+,d0;\
	movb (addrarg),d0
#  define getaddr2(addrarg) getaddr(addrarg);\
	addql NO2,addrarg
#  define putaddr	movw d1,-(sp);\
	movb (sp)+,-(pseudosp);\
	movb d1,-(pseudosp)
#endif /* M68020 */

#define inrflag	movw ccr,d0;\
	andw regcon0e,d0;\
	andw regcon01,regf;\
	orb 0(flagptr,d0:w),regf

#define dcrflag	movw ccr,d0;\
	andw regcon0e,d0;\
	andw regcon01,regf;\
	orb -16(flagptr,d0:w),regf

#ifdef Z80			/* Jump Relative, etc.  (Z-80) */
#  define jrd1	extw d1;\
	addw d1,pseudopc;\
	znext
#  define calcind(x)	movb (pseudopc)+,d0;\
	extw d0;\
	addw x(regs),d0
#  define setaflags	movw ccr,d1;\
	andw regcon0f,d1;\
	movb 0(flagptr,d1:w),regf

#  define setanflags	movw ccr,d1;\
	andw regcon0f,d1;\
	movb -16(flagptr,d1:w),regf

#  define setapflags(r)	movw ccr,d1;		\
	andw regcon0f,d1;\
	movb -32(flagptr,d1:w),regf;\
	movb r,d1;\
	orb 272(flagptr,d1:w),regf

#  define dozf(x)	beqs zset##x;\
	bclr NO6,regf

#  define docyf(x)	bcss cset##x;\
	bclr NO0,regf;\
	znext

#  define addflags	movb d0,regop1(regs);\
	movb rega,regop2(regs);\
	movb regcon0e,regop3(regs);\
	addb d0,rega;\
	setaflags

#  define adcflags	movb d0,regop1(regs);\
	movb rega,regop2(regs);\
	moveq NO0,d1;\
	addxb d0,rega;\
	setaflags

#  define sbbflags	moveq NO0,d1;\
	subxb d0,rega;\
	setaflags

#endif /* Z80 */
