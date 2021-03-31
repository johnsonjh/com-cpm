# COM: A CP/M-80 Simulator

---

A CP/M-80 simulator (in portable C and 68000 assembly flavors)

---

Sample \'_screen shot_\' from _**WordStar 3.00**_:

>                editing no file
>                      < < <  N O - F I L E   M E N U  > > >
>        ---Preliminary  Commands---   | --File  Commands-- | -System  Commands-
>      L  Changed logged disk drive    |                    |  R  Run a program
>      F  File directory    off (ON)   |  P  Print a file   |  X  EXIT to system
>      H  Set help level               |                    |
>       ---Commands to open a file---  |  E  RENAME a file  | -WordStar Options-
>         D  Open a  document  file    |  O  COPY   a file  |  M  Run MailMerge
>         N  Open a non-document file  |  Y  DELETE a file  |  S  Run SpellStar
>
>     DIRECTORY of disk A:
>      FFT.BAS      KEYCON.ASM   KEYCON.PRN   KEYCON.SYM   MORTGAGE.BAS PILOT.TXT
>      SPELSTAR.DCT SQRTSIM.BAS  THRASH.BAS   TUTORIAL.PIL ZORK1.DAT    ZORK2.DAT
>      ZORK3.DAT    KEYCON.HEX   CD.COM       FORTH.COM    INSTALL.COM  LOAD.COM
>      MAC.COM      MBASIC.COM   PILOT.COM    SARGON.COM   TESTPROG.COM WS.COM
>      WS.COM       ZORK1.COM    ZORK2.COM    ZORK3.COM    MAILMRGE.OVR MERGPRIN.OVR
>      SPELSTAR.OVR WIMSGS.OVR   WSMSGS.OVR   WSOVLY1.OVR

And then running **Zork** (from within **WordStar**):

>     r          editing no file
>
>      Enter name of program you wish to Run,
>      optionally followed by appropriate arguments.
>         Example (shows disk space):     STAT
>
>      ^S=delete character   ^Y=delete entry   ^F=File directory
>      ^D=restore character  ^R=Restore entry  ^U=cancel command
>
>         COMMAND? zork1
>
>     ZORK I: The Great Underground Empire
>     Copyright (c) 1981, 1982, 1983 Infocom, Inc. All rights
>     reserved.
>     ZORK is a registered trademark of Infocom, Inc.
>     Revision 88 / Serial number 840726
>
>     West of House
>     You are standing in an open field west of a white house, with
>     a boarded front door.
>     There is a small mailbox here.
>
>     >

---

## Update (2008)

Now that the _PowerPC_ platforms are being left in the dust, I decided to port
the assembly-language synthetic CPU to them. Why? No good reason, mostly just
curiosity to see how much better than the C compiler I could do by careful
hand-coding. (I also wished to use it as a vehicle for familiarizing myself with
the _PPC_ because we use it at work.)

The port is roughly a line-by-line translation of the original _680x0_ code.
This is probably sub-optimal with regards to what the _PPC_ could really do, but
I just wasn\'t up to the job of completely re-architecting it. The _PPC_\'s flag
registers are sufficiently difficult to access, and weird enough once you do get
there, that I abandoned the original memory-thrifty architecture of converting
the native arithmetic flags to the target\'s flags via small lookup tables in
favor of an approach where I look up the flags (and the sum/difference) directly
via large (128 KB) three-input tables. (I had considered this table-driven
approach in the very beginning, but the original host machine had all of 128 KB
of RAM in it, of which half was going to be dedicated to the target machine
image space, leaving the other half for the host OS _and_ the simulator, so that
was not possible.) This approach also enabled creation of a true half-carry
flag, rather than recording all arguments to add/subtract instructions for later
use in recreating a half-carry in any subsequent DAA instructions.

My unfamiliarity with the _PPC_ has also no doubt resulted in less than optimal
results, even given this architecture, especially as pipeline and cache effects
have been ignored. I must say that I find the _PPC_ difficult to write assembly
for, but the bulk of that can be blamed on poor documentation. (Very few people
do this, after all. It\'s a C compiler world now, so far as high-performance
code is concerned.)

I also began a port to the _MIPS_, because we use those at work too, and this
_is_ an excellent vehicle for learning a processor\'s native instruction set.
The move away from using the host processor\'s native condition-code flags
proved to be a good one, because the _MIPS_ doesn\'t even _have_ a condition
code register! However, though the resultant port compiles I was unable to get
it to link properly so that it would actually execute. I have left finishing
this \'til later, since my main objective (learning the _MIPS_) was accomplished
and I was getting tired of working on it. The _PPC_ timing results imply that
any improvement would hardly be worth the effort anyway.

Performance (_in seconds for the test run_):

       MHz| 4     8     10    20    25    25    233     800     3000   2330
       CPU| Z-80  68000 68010 68020 68030 68030 PPC G3  PPC G4  Pent-D iC2Duo
       OS | CP/M  CP/M  DNIX  DNIX  NeXT  SVR4  OSX 2.8 OSX 3.5 XP/Cyg OSX 4.8
       ===+ ====  ===== ===== ===== ===== ===== ======= ======= ====== =======
       com   35*               19    20   18.5* 0.50    0.19
       comt                    52    52         1.2     0.35
       com10      82*     52   23    25
       com10t            149   55    60
       ccom              130   48    58         0.71    0.24    0.14   0.08
       ccomt             260   91    92         1.3     0.38    0.17   0.13
       comtest          2060  840   681         7.6     2.40
       ccomtest                                 7.1     2.13    1.00   0.63

     Note:  * Old numbers, not reproducible.  (Platforms long gone.)

Of particular interest is that while for the _680x0_ platforms I could beat the
C code by a substantial margin, for the _PPC_ it wasn\'t worth the effort!
Modern C compilers seem to mate to modern RISC processors very well, imagine
that.

What\'s more, except for the large lookup table, the C version of the CPU is
several kilobytes _smaller_! (This can be blamed on the assembly version\'s
in-line next macro, which doesn\'t seem to make any speed difference when
replaced with a branch to a central next routine, which does make the assembly
version smaller.)

Most instructive, though the results were not exactly what I had expected.
(Which means this was a worthy exercise.)

Also note that the C-only version of the `comtest` runs _faster_ than the one
that includes assembly language, yet the simple assembler version runs faster
than the simple C version, though not by a huge margin. This tells me that the
processor\'s cache isn\'t big enough, and that `comtest` is starting to thrash
the cache a bit more than `ccomtest` did, as it is larger. Interesting.

(The `comtest` programs are for pitting the C and assembler versions of a
synthetic CPU against each other. Differences halt the simulation. _Both_ CPU
models have gained by this procedure. The C-only test version is for debugging
the test harness itself, as it compares two identical CPU models, which can be
tweaked to induce errors to ensure that they are caught by the harness.)

Two additional `Makefiles` have been added: `make.ppc` and `make.mips`. The
desired file should be linked to `Makefile`, as appropriate.

## Update (2006)

The simulator has recently been ported to an all-C version, which runs not only
on the **UNIX** systems that the assembly-language version runs on (albeit
slower), but also on much more modern machines. As one might imagine, hundreds
to thousands of megahertz host machines really perk the old girl up, even with
the innate _2-3\_\_×_ reduction in efficiency of C versus the hand-tuned
assembly language of the original.

The simulation environment has been enhanced to support **CP/M** drives `A:`
through `P:`, the full set, though not to the associated 16 _USER_ spaces per
drive. These translate to directories (one each) under **UNIX**, and which are
specified on the command line. Also provided is a way to \'bury\' selected files
within the simulator itself. This lets something like _WordStar_, which requires
overlay files to always be available, edit files anywhere in the system, just
like any native editor.

A `Makefile` for _Mac OS X_ has been added, named (obviously enough) `make.osx`,
it should be linked to `Makefile` if appropriate. It\'s the one to use for any
non-_680x0_ machine as it\'s the one that\'s C-only. Changing one `#define` at
the front specifies whether the target system is **DNIX**, **SVR4**, or **BSD**
in flavor. It has been compiled and run unchanged (except for the flavor
selector) on **OS X**, **Cygwin**, **FreeBSD**, and **Linux**.

As should be obvious, porting (to anything **UNIX**-ey) is now a nearly trivial
exercise. The most system-specific code is in the directory listing logic,
what\'s there now is a choice of using the dirent package or a really nasty exec
of find and sed. This could be converted to something else, the changes should
be localized. The next-most-system-specific code is the terminal handling code.
(That\'s the main reason for the flavor selector `#define` in the first place.)

The C simulation code was written from scratch, so it\'s unrestricted.

### Performance

Times (in seconds) largely collected with:

    time ../com mac keycon

A stopwatch was used for the host (_native_) **CP/M** platforms.

            4     8     10    20    25    25    233     800     3000   2330
            Z-80  68000 68010 68020 68030 68030 PPC G3  PPC G4  Pent-D iC2Duo
            CP/M  CP/M  DNIX  DNIX  NeXT  SVR4  OSX 2.8 OSX 3.5 XP/Cyg OSX 4.8
            ====  ===== ===== ===== ===== ===== ======= ======= ====== =======
       com   35*               19    20   18.5*
       comt                    52    52
       com10      82*     52   23    25
       com10t            149   55    60
       ccom              130   48    58         0.67    0.24    0.14   0.08
       ccomt             260   91    92         1.1     0.38    0.17   0.13
       comtest          2060  840   681         6.0     2.13    1.00   0.63

       Note: * Old numbers, not reproducible.  (Platforms long gone.)

This simulator is probably not the fastest one out there, nor is it particularly
complete, nor is it feature-laden, and it is _far_ from the only one out there!
What it _is_ is venerable, it has existed (in some form) since 1984, making it
one of the first, if not _the_ first, **CP/M** simulators designed for practical
use. (I won\'t count any potential simulations that may have existed at chip
manufacturers.)

I got it published in _Dr. Dobb\'s Journal_ in 1986, my one and only
publication.

## Usage

Here is a sample session in use. Like **WINE** (for _Microsoft Windows_
applications under **UNIX**), you don\'t ever actually run **CP/M** itself, only
its applications.

    $ ../com mbasic
    BASIC-80 Rev. 5.21
    [CP/M Version]
    Copyright 1977-1981 (C) by Microsoft
    Created: 28-Jul-81
    39730 Bytes free
    Ok
    files
    ZORK3   .DAT  ZORK3   .COM  ZORK2   .DAT  ZORK2   .COM  ZORK1   .DAT
    ZORK1   .COM  WSOVLY1 .OVR  WSMSGS  .OVR  WS      .COM  WS      .COM
    WIMSGS  .OVR  TUTORIAL.PIL  THRASH  .BAS  TESTPROG.COM  SQRTSIM .BAS
    SPELSTAR.OVR  SPELSTAR.DCT  SARGON  .COM  PILOT   .TXT  PILOT   .COM
    PARANOIA.BAS  MORTGAGE.BAS  MERGPRIN.OVR  MBASIC  .COM  MAILMRGE.OVR
    MAC     .COM  LOAD    .COM  KEYCON  .ASM  JUNK    .COM  INSTALL .COM
    FORTH   .COM  FFT     .BAS  CD      .COM
    Ok
    system
    $

## Introduction (1993)

This is a **CP/M** *2.2* Simulator that simulates an *8080* CPU and a
**CP/M** *2.2* OS (if you can call it that) environment. The heart of the
simulator is written in _680x0_ assembly language for speed. It has been tested
under **DNIX** (a **SVR2** compatible with many **SVR3**, **BSD**, **Xenix**,
and **Sun** extensions), on a _68030_ **NeXT**, and on a _68030_ **Amiga**
running **SVR4**.

One \'benchmark\' shows that on machines of the _68020_/_68030_ class the
simulator performs about as well as a 7 MHz _Z-80_ would. Other tests indicate
that this is somewhat optimistic.

## Why CP/M?

Why not? I updated this program (originally running under **CP/M-68K**, and
published in the _January 1986_ issue of _Dr._ _Dobb\'s_ _Journal_ _\[of_
_Computer_ _Calisthenics_ _and_ _Orthodontia\]_) out of curiosity to see how it
would run on something newer than the _68000_ it was originally running on in
1984, and out of a perverse desire to make my colleagues sick.

## What You Get

The simulator gives you a simple **CP/M** run-time environment with only one
drive (`A:`), which is the current directory when the simulator is invoked.

There is no _CCP_ (the command processor, analogous to the **UNIX** shell);
programs must be run individually under the simulator. The simulator can be
invoked so that the listing (printer) output goes to a file (otherwise it is
discarded).

It normally converts **H19** escape sequences (and a couple of other oddballs)
to **VT100** sequences, since much **CP/M** software doesn\'t know what a
**VT100** is, yet it is the current **UNIX** _\'standard\'_. If **CP/M** had any
_\'standard\'_ (it didn\'t, really) it was the **H19**. This feature can be
disabled, if needed.

Similarly, the simulation optionally maps `DEL` to `BS` on keyboard input, since
`BS` is the **CP/M** _\'standard\'_, whereas `DEL` is many **UNIX** systems\'
standard. The simulator maps all filenames to uppercase (for display by **CP/M**
programs), and converts them to lowercase when actually doing file I/O.

Several of the more esoteric _BDOS_ calls are not supported.

Please remember that **CP/M** text files use both carriage returns and line
feeds (one each) to delimit lines, and the end-of-file character is an embedded
_Control-Z_. The `EOF` character is needed because **CP/M** files are always
multiples of 128 bytes - there is no byte-level file length indication. **CP/M**
programs can get annoyed if files fed to them don\'t conform to these
expectations.

## Porting

The hardest part of porting this program is getting the synthetic CPU (assembly
language) module to assemble since in the infinite wisdom of the **UNIX** world
no vendor seems to use Motorola\'s standard mnemonics.

The module is written using the **DNIX** assembler syntax (supposedly MIT
mnemonics with Motorola standard addressing syntax), and is sed\'ed to whatever
is necessary. (It actually started life in 1984 under **CP/M-68K**, using
Motorola standard mnemonics, but I converted it to be most compatible with
**DNIX**, since that\'s what I use most of the time.) The C pre-processor is
used on the assembly module, so some sed-ing is always needed to get things like
the octothorpe (\#) character through it. If your cpp can\'t hack it, or is
nonexistent, more work will be required.

I have left as an exercise for the reader the writing of the synthetic CPU
module for non-_680x0_ systems. I considered writing one in C to serve as a
model, but it would function so slowly on the systems I use (the ones I _did_
get it working on) that I didn\'t want to waste my time - it\'s not like I
_need_ a **CP/M** simulator for anything. The energetic could write one fairly
easily (how hard can an _8080_ be after all?), but for real speed a new
assembly-language module should be written.

It would be particularly efficient on an _x86_ machine, since the slowest part
of the simulation is getting the flags right. The _x86_ and the _8080_ have the
same flags, so it\'s probable that the _8080_ flags could just be taken straight
out of the _x86_ flags register (i.e. no work at all). However, as I think that
most _x86_ machines are evil incarnate I refuse to take a stab at the project
(except maybe with a wooden stake).

There are three `Makefiles` provided: one for **DNIX**, one for **NeXT**, and
one for vanilla (_?_) **System V**. The appropriate file should be linked to the
name `Makefile`, then you should just be able to type \'`make`\'. If I were
better at multi-system configuration programs this could probably have been made
more elegant, but it works well enough for me.

## Terms

As was the case for its predecessor, this program is in the **Public**
**Domain**. All I ask is that my name be left on the code if it is used
elsewhere. (As if that\'s likely at _this_ late date!)

The _Z-80_ simulation routines were taken from an independent Amiga
port/enhancement of the original code done by _Charlie_ _Gibbs_ and _Willi_
_Kusche_. Their code is slightly more restricted. I have placed their disclaimer
at the front of `com.c`; their code is conditionally included in the simulator.
If it isn\'t included, you get a plain _8080_ CPU with a few _Z-80_ instructions
(as before, under **CP/M-68K**).

\*Enjoy**\*!**

## Background (1984)

This simulator was written in desperation in 1983 when I was working on an
_S100_ bus **CP/M-68K** system and trying to integrate a hard disk controller.
The disk formatter I wrote according to the (expensive!) controller\'s
documentation simply did not work, yet the card was one that worked very well in
regular **CP/M** systems, using a formatting program that came with the card.

I had two options: either borrow one of the _EM-180_ emulators from work, _and_
a functioning _S100_ **CP/M** system, and trap how the card was being talked to,
or write a simulator and do the same trapping in software. The latter seemed
more interesting, less bulky, and didn\'t inconvenience anyone else. It also
offered the possibility of running existing applications (like word processors
or spreadsheets) on the new hardware. (Native applications of this sort were
neither expected to be particularly available, nor inexpensive even if they
were.)

The choice was easy.

Because the host and target systems were both **CP/M**, potential problem areas
like terminal I/O or disk files didn\'t need any special treatment or
translation, requests could just be forwarded directly to the host OS. The
entire thing could easily be written in assembly language, which was a necessity
for the synthetic CPU to have the best performance anyway.

Once the simulator worked well enough to host the hard disk formatter it became
clear that the documentation for the controller card was wrong. One of the
registers required the 2\'s-complement of the desired value to be written to it,
a fact the documentation didn\'t see fit to mention. With this change my own
formatter program worked correctly, and in short order my system then had a
whopping _ST-506_ **5MB** hard disk on it!

Shortly after that the simulator worked well enough to run **MBASIC**, etc. The
simulator was _8080_-only, all that was necessary to run 90% of **CP/M**
programs. Adding a couple of _Z-80_ instructions, the ones the _BDS C_ compiler
wanted to use in its objects, brought that up to something like _**99%**_.

The final Version 1.0 simulator transformed this expensive system from
laboratory curiosity to practical computer, nearly overnight. (Although the
simulation ran at less than half the speed of the real thing. Native programs,
though, _were_ blazingly fast in comparison.)

## Author Information

- [Jim Cathey](http://formicapeak.com/~jimc)
- **Email**: [jim.cathey.pb@gmail.com](mailto:jim.cathey.pb@gmail.com)
- **Phone**: `+1 (509) 926-7801` (_I prefer e-mail, where possible_.)
