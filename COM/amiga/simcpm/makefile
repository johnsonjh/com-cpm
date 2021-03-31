simcpm : simcpm1.o simcpm2.o
	blink simcpm1.o simcpm2.o to simcpm lib dh0:lib/amiga.lib verbose

simcpm1.o : simcpm1.asm options.i ecpmdefs.i
	a68k simcpm1.asm

simcpm2.o : simcpm2.asm options.i ecpmdefs.i
	a68k simcpm2.asm
