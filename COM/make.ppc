#
# Make file for the CP/M 2.2 Simulator for PowerPC (Mac OSX) machines.
#

.SUFFIXES: .o .L .s .mac

CPP=/usr/bin/cpp

#
# Set the type of system here to one of DNIX, SYSV, CYGW, NEXT or BSD.
# DNIX is SVR2 with some Xenix extensions, like the rdchk function.
# CYGW (Cygwin) is SYSV with a broken VMIN/VTIME implementation.
# NEXT (NeXTStep) is BSD with a messy dirent implementation.
#
# SYSTYPE= One of: DNIX, SYSV, CYGW, NEXT, or BSD (also for linux, OSX)
SYSTYPE=BSD

#
# Configuration options.
# To get Z80 simulation, give it -DZ80 here
OPTIONS=-D$(SYSTYPE)=1 -DZ80 -DRUNOPTS -DUSEDIRENT

#
# Code-generation options.
CFLAGS=-O $(OPTIONS)

#
# Normal version.
#
com: cpu.o cpu2.o com.o console.o dir.o disass.o fakefs.o Makefile
	$(CC) cpu.o cpu2.o com.o console.o dir.o disass.o fakefs.o -o $@

#
# Tracing version (slower).
#
comt: cput.o cpu2t.o comt.o console.o dir.o disass.o fakefs.o Makefile
	$(CC) cput.o cpu2t.o comt.o console.o dir.o disass.o fakefs.o -o $@

#
# C version (not quite as fast).
#

ccom: ccpu.o com.o console.o dir.o disass.o fakefs.o Makefile
	$(CC) ccpu.o com.o console.o dir.o disass.o fakefs.o -o $@

ccomt: ccput.o comt.o console.o dir.o disass.o fakefs.o Makefile
	$(CC) ccput.o comt.o console.o dir.o disass.o fakefs.o -o $@

#
# Contest version (slower yet).  Pits both CPU implementations against
# each other.  The intent is to use them to diagnose each other, to
# bootstrap themselves up to a greater level of functionality.
#
comtest: ccputest.o cputest.o cpu2t.o comtest.o console.o dir.o disass.o fakefs.o Makefile
	cc ccputest.o cputest.o cpu2t.o comtest.o console.o dir.o disass.o fakefs.o -o $@

cpu.o: cpuppc.mac ppcmacros.h flags.s mactogas Makefile
	$(CPP) -DNORMAL $(OPTIONS) -P cpuppc.mac >cpu.s1
	tr ';' '\012' <cpu.s1 | tr '|' ';' | sed -f mactogas >cpu.s
	rm -f cpu.s1
	$(AS) cpu.s -o $@

cput.o: cpuppc.mac ppcmacros.h flags.s mactogas Makefile
	$(CPP) -DTRACE $(OPTIONS) -P cpuppc.mac >cput.s1
	tr ';' '\012' <cput.s1 | tr '|' ';' | sed -f mactogas >cput.s
	rm -f cput.s1
	$(AS) cput.s -o $@

cputest.o: cpuppc.mac ppcmacros.h flags.s mactogas Makefile
	$(CPP) -DTRACE -DCONTEST $(OPTIONS) -P cpuppc.mac >cputest.s1
	tr ';' '\012' <cputest.s1 | tr '|' ';' | sed -f mactogas >cputest.s
	rm -f cputest.s1
	$(AS) cputest.s -o $@

cpu2.o: cpu2ppc.mac ppcmacros.h mactogas Makefile
	$(CPP) -DNORMAL $(OPTIONS) -P cpu2ppc.mac >cpu2.s1
	tr ';' '\012' <cpu2.s1 | tr '|' ';' | sed -f mactogas >cpu2.s
	rm -f cpu2.s1
	$(AS) cpu2.s -o $@

cpu2t.o: cpu2ppc.mac ppcmacros.h mactogas Makefile
	$(CPP) -DTRACE $(OPTIONS) -P cpu2ppc.mac >cpu2t.s1
	tr ';' '\012' <cpu2t.s1 | tr '|' ';' | sed -f mactogas >cpu2t.s
	rm -f cpu2t.s1
	$(AS) cpu2t.s -o $@

#
# Build run-time flag lookup table.  (It's invariant, but this is easier.)
#
flags.s: makeflags
	 ./makeflags >$@

#
# This is always run on the host, so don't use the CC/CFLAGS macros.
#
makeflags: makeflags.c Makefile
	cc $(OPTIONS) makeflags.c -o $@

ccpu.o: ccpu.c com.h Makefile
	$(CC) $(CFLAGS) -c ccpu.c -o $@

ccput.o: ccpu.c com.h Makefile
	$(CC) $(CFLAGS) -DTRACE -c ccpu.c -o $@

ccputest.o: ccpu.c com.h Makefile
	$(CC) $(CFLAGS) -DTRACE -DCONTEST -c ccpu.c -o $@

com.o: com.c com.h Makefile
	$(CC) $(CFLAGS) -c com.c -o $@

comt.o: com.c com.h Makefile
	$(CC) $(CFLAGS) -c -DTRACE com.c -o $@

comtest.o: com.c com.h Makefile
	$(CC) $(CFLAGS) -c -DTRACE -DCONTEST com.c -o $@

console.o: com.h console.c Makefile

dir.o: com.h dir.c Makefile

ws_com.c: testprogs/ws.com
	./bintoc $? ws_com >$@

wsmsgs_ovr.c: testprogs/wsmsgs.ovr
	./bintoc $? wsmsgs_ovr >$@

wsovly1_ovr.c: testprogs/wsovly1.ovr
	./bintoc $? wsovly1_ovr >$@

mbasic_com.c: testprogs/mbasic.com
	./bintoc $? mbasic_com >$@

fakefsws.o: fakefs.c ws_com.o wsmsgs_ovr.o wsovly1_ovr.o
	$(CC) $(CFLAGS) -DWORDSTAR -c fakefs.c -o $@

embed.o: com.c com.h Makefile
	$(CC) $(CFLAGS) -DEMBED -c com.c -o $@

ws: cpu.o cpu2.o console.o dir.o disass.o fakefsws.o embed.o ws_com.o wsmsgs_ovr.o wsovly1_ovr.o Makefile
	$(CC) cpu.o cpu2.o console.o dir.o disass.o fakefsws.o embed.o ws_com.o wsmsgs_ovr.o wsovly1_ovr.o -o $@

fakefsmb.o: fakefs.c mbasic_com.o
	$(CC) $(CFLAGS) -DMBASIC -c fakefs.c -o $@

mbasic: cpu.o cpu2.o console.o dir.o disass.o fakefsmb.o embed.o mbasic_com.o Makefile
	$(CC) cpu.o cpu2.o console.o dir.o disass.o fakefsmb.o embed.o mbasic_com.o -o $@

zork1_com.c: testprogs/zork1.com
	./bintoc $? zork1_com >$@

zork1_dat.c: testprogs/zork1.dat
	./bintoc $? zork1_dat >$@

fakefsz1.o: fakefs.c zork1_com.o
	$(CC) $(CFLAGS) -DZORK1 -c fakefs.c -o $@

zork1: cpu.o cpu2.o console.o dir.o disass.o fakefsz1.o embed.o zork1_com.o zork1_dat.o Makefile
	$(CC) cpu.o cpu2.o console.o dir.o disass.o fakefsz1.o embed.o zork1_com.o zork1_dat.o -o $@

zork2_com.c: testprogs/zork2.com
	./bintoc $? zork2_com >$@

zork2_dat.c: testprogs/zork2.dat
	./bintoc $? zork2_dat >$@

fakefsz2.o: fakefs.c zork2_com.o
	$(CC) $(CFLAGS) -DZORK2 -c fakefs.c -o $@

zork2: cpu.o cpu2.o console.o dir.o disass.o fakefsz2.o embed.o zork2_com.o zork2_dat.o Makefile
	$(CC) cpu.o cpu2.o console.o dir.o disass.o fakefsz2.o embed.o zork2_com.o zork2_dat.o -o $@

zork3_com.c: testprogs/zork3.com
	./bintoc $? zork3_com >$@

zork3_dat.c: testprogs/zork3.dat
	./bintoc $? zork3_dat >$@

fakefsz3.o: fakefs.c zork3_com.o
	$(CC) $(CFLAGS) -DZORK3 -c fakefs.c -o $@

zork3: cpu.o cpu2.o console.o dir.o disass.o fakefsz3.o embed.o zork3_com.o zork3_dat.o Makefile
	$(CC) cpu.o cpu2.o console.o dir.o disass.o fakefsz3.o embed.o zork3_com.o zork3_dat.o -o $@

.PHONY: all clean

all: com comt comtest ccom ccomt ws mbasic zork1 zork2 zork3

benchmark: com comt comtest
	./benchtest $? >$@

clean:
	rm -f com comt comtest *.[soL] *.s1 a.out core *~ *.map benchmark \
		ws ws_com.c wsmsgs_ovr.c wsovly1_ovr.c mbasic mbasic_com.c \
		zork1 zork1_com.c zork1_dat.c zork2 zork2_com.c zork2_dat.c \
		zork3 zork3_com.c zork3_dat.c makeflags ccom ccomt
