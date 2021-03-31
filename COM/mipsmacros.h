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
#define sp $29
#define opptr $15
#define pseudopc $23
#define pseudosp $22
#define lflagptr $14
#define aflagptr $13
#define sflagptr $12
#define sumptr $11
#define targbase $21
#define regs $21
#define regconm1 $25
#define regcon01 $24
#define regf $17
#define rega $16
#define ret1 $19
#define ret2 $18

|
|	Note, only leaves R0/R3-R18 for free use by entire
|	program without saving registers for temporary use.  $3
|       is generally the 'accumulator' (as was D0 in the 68K code)
|       because there are too many restrictions on R0.  $2 is used
|	as the secondary accumulator/scratch register, $4 as tertiary.
|	$18-$19 are used to save LR during internal subroutine calls.
|	(They could go on the stack, but MIPS stack frames are tricksy.
|	 The MIPS calling convention preserves $16-$23 inside functions.)
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
#define inext\
	lbu $3,0(pseudopc) | inline-nxt-instr;	\
	addu pseudopc,pseudopc,regcon01;\
	sll $3,$3,2;\
	addu $2,$3,opptr;\
	lw $3,0($3);\
	jr $3;\
	nop

#ifdef Z80
#  define inextr3\
	lbu $2,0(pseudopc);\
	addu pseudopc,pseudopc,regcon01;\
	sll $2,$2,2;\
	addu $2,$2,$3;\
	lw $2,0($2);\
	jr $2;\
	nop
#endif /* Z80 */

#ifdef TRACE
#  define next	j mloop;\
	nop
#  ifdef Z80
#    define nextr3 inextr3
#  endif /* Z80 */
#else /* not TRACE */
#  define next inext
#  ifdef Z80
#    define nextr3 inextr3
#  endif /* Z80 */
#endif /* FAST */

/* Byte-swapping Get address and Put address. */
#define getaddr(addrarg)\
	lbu $3,1(addrarg);\
	sll $3,$3,8;\
	lbu $1,0(addrarg);\
	or $3,$3,$1
#define getaddr2(addrarg)\
	getaddr(addrarg);\
	addiu addrarg,addrarg,2
#define putaddr\
	sb $2,-2(pseudosp);\
	srl $2,$2,8;\
	sb $2,-1(pseudosp);\
	addiu pseudosp,pseudosp,-2

#define inrflag(r)\
	sll $2,r,8;\
	or $2,$2,regcon01;\
	addu r,$2,aflagptr;\
	lbu r,0(r);\
	andi r,r,0xFE;\
	and regf,regf,regcon01;\
	or regf,regf,r;\
	addu r,$2,sumptr;\
	lbu r,0(r)

#define dcrflag(r)\
	sll $2,r,8;\
	or $2,$2,regcon01;\
	addu r,$2,sflagptr;\
	lbu r,0(r);\
	andi r,r,0xFE;\
	and regf,regf,regcon01;\
	or regf,regf,r;\
	xori $2,$2,0xFF;\
	sll r,regcon01,16;\
	or $2,r,r;\
	addu r,$2,sumptr;\
	lbu r,0(r)

#define RLCR(r)\
	srl $1,r,7;\
	andi regf,regf,0xFE;\
	or regf,regf,$1;\
	sll r,r,1;\
	or r,r,$1

#define RRCR(r)\
	and $1,r,regcon01;\
	andi regf,regf,0xFE;\
	or regf,regf,$1;\
	srl r,r,1;\
	sll $1,$1,7;\
	or r,r,$1

#define RALR(r)\
	sll r,r,1;\
	and $1,regf,regcon01;\
	or r,r,$1;\
	srl $1,r,8;\
	andi regf,regf,0xFE;\
	or regf,regf,$1
	
#define RARR(r)\
	and $1,regf,regcon01;\
	sll $1,$1,8;\
	or r,r,$1;\
	and $1,r,regcon01;\
	srl r,r,1;\
	andi regf,regf,0xFE;\
	or regf,regf,$1

#define SLAR(r)\
	srl $1,r,7;\
	andi regf,regf,0xFE;\
	or regf,regf,$1;\
	sll r,r,1

#define SRAR(r)\
	andi regf,regf,0xFE;\
	and $1,r,regcon01;\
	or regf,regf,$1;\
	andi $1,r,0x80;\
	srl r,r,1;\
	or r,r,$1

#define SRLR(r)\
	andi regf,regf,0xFE;\
	and $1,r,regcon01;\
	or regf,regf,$1;\
	srl r,r,1

#define addflags\
	sll $2,rega,8;\
	or $3,$3,$2;\
	addu $2,$3,aflagptr;\
	lbu regf,0($2);\
	addu $2,$3,sumptr;\
	lbu rega,0($2);\

#define cmpflags\
	sll $2,rega,8;\
	or $3,$3,$2;\
	addu $2,$3,sflagptr;\
	lbu regf,0($2)

#define subflags\
	cmpflags;\
	xori $3,$3,0xFF;\
	sll $2,regcon01,16;\
	or $3,$3,$2;\
	addu $2,$3,sumptr;\
	lbu rega,0($2)

#define adcflags\
	sll $2,rega,8;\
	or $3,$3,$2;\
	and $2,regf,regcon01;\
	sll $2,$2,16;\
	or $3,$3,$2;\
	addu $2,$3,aflagptr;\
	lbu regf,0($2);\
	addu $2,$3,sumptr;\
	lbu rega,0($2)

#define sbbflags\
	sll $2,rega,8;\
	or $3,$3,$2;\
	and $2,regf,regcon01;\
	sll $2,$2,16;\
	or $3,$3,$2;\
	addu $2,$3,sflagptr;\
	lbu regf,0($2);\
	xori $3,$3,0xFF;\
	sll $2,regcon01,16;\
	xor $3,$3,$2;\
	addu $2,$3,sumptr;\
	lbu rega,0($2)

#define carryf(r)\
	srl r,r,16;\
	andi regf,regf,0xFE;\
	andi r,r,1;\
	or regf,regf,r

#ifdef Z80			/* Jump Relative, etc.  (Z-80) */
#  define calcind(x)\
	lb $2,0(pseudopc);\
	addu pseudopc,pseudopc,regcon01;\
	lhu $3,x(regs);\
	addu $2,$2,$3

#  define setapflags(r)\
	addu $2,lflagptr,r;\
	lbu $2,0($2);\
	andi $2,$2,0xFE;\
	and regf,regf,regcon01;\
	or regf,regf,$2

#  define bit(r)\
	slt r,$0,r;\
	ori regf,regf,0x40;\
	sll r,r,6;\
	xor regf,regf,r

#endif /* Z80 */
