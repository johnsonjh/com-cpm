*
* Emulator register definitions
*  Note, leaves only D0-D1/A0 free for use by entire
*  program without saving registers for temporary use.
*
return   equr     A6             ;JMP (return) is fast return to MLOOP
pseudopc equr     A5             ;Pseudo program counter
opptr    equr     A4             ;Address of opcode dispatch table
pseudosp equr     A3             ;Pseudo stack pointer
flagptr  equr     A2             ;Address of flag lookup table
targbase equr     A1             ;Address of emulated address space
regs     equr     A1             ;Base pointer to emulated registers
regcon0e equr     D7             ;Register-based constant #$E (for speed)
regcon01 equr     D6             ;Register-based constant #$1
regcon0f equr     D5             ;Register-based constant #$F
regconff equr     D4             ;Register-based constant #$FF
regf     equr     D3             ;Emulated flag register
rega     equr     D2             ;Emulated accumulator
*
* Target processor's data registers (offsets into storage area)
*
regop3   equ      -21            ;Operand 3 for DAA storage
regb     equ      -20            ;Offsets from register base pointer for
regc     equ      -19            ; 8080's pseudo-registers.
regd     equ      -18            ; A and F are in 68000's data registers.
rege     equ      -17            ; Pseudo-PC is kept in an address register.
regh     equ      -16            ;
regl     equ      -15            ;
regix    equ      -14            ;
regiy    equ      -12            ;
regaaf   equ      -10            ;Alternate AF
regabc   equ      -8             ;Alternate BC
regade   equ      -6             ;Alternate DE
regahl   equ      -4             ;Alternate HL
regop1   equ      -2             ;Operand 1 for DAA storage
regop2   equ      -1             ;   "    2  "   "     "

