*
* Emulator register definitions
*  Note, only leaves D0-D1/A0 free for use by entire
*  program without saving registers for temporary use.
*
return	 equr	A6		;JMP (return) is fast return to MLOOP.
pseudopc equr	A5		;Emulated program counter
opptr	 equr	A4		;Address of opcode dispatch table
pseudosp equr	A3		;Emulated stack pointer
flagptr  equr	A2		;Address of flag lookup table
targbase equr	A1		;Address of emulated address space
regs	 equr	A1		;Base pointer to emulated registers
regcon0e equr	D7		;Register-based constant #$E (for speed)
regcon01 equr	D6		;Register-based constant #$1
regcon0f equr	D5		;Register-based constant #$F
regconff equr	D4		;Register-based constant #$FF
regf	 equr	D3		;Emulated flag register
rega	 equr	D2		;Emulated accumulator
*
* Target processor's data registers (offsets into storage area)
*  The emulated accumulator and flag register are kept in data registers.
*  The emulated program counter and stack pointer
*  are kept in address registers.
*
regb	equ	-22		;B
regc	equ	-21		;C
regd	equ	-20		;D
rege	equ	-19		;E
regh	equ	-18		;H
regl	equ	-17		;L
regix	equ	-16		;IX
regxh	equ	-16
regxl	equ	-15
regiy	equ	-14		;IY
regyh	equ	-14
regyl	equ	-13
regb2	equ	-12		;Alternate register set for Z-80
regc2	equ	-11
regd2	equ	-10
rege2	equ	-9
regh2	equ	-8
regl2	equ	-7
rega2	equ	-6
regf2	equ	-5
regi	equ	-4		;Interrupt page address register (I)
regop1	equ	-3		;Operand 1 for DAA storage
regop2	equ	-2		;   "	 2  "	"     "
regop3	equ	-1		;   "    3  "   "     "
