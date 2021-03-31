/*
** Make 8080/Z-80 arithmetic lookup tables.  These are big 128Kx8-bit
** tables that contain the target's AF register pair for the results
** of additions and subtractions of the two operands (plus carry/borrow)
** used to index them.  The sum table is shared with subtraction,
** the run-time code complements one of the operands and adds 1 to
** the carry-in bit.  That makes only three big tables, not four.
**
** Used for the PPC simulation, since its native condition code flags
** are just too difficult to gain quick access to, and too weird.
**
** The original 68K implementation was on a machine with only 128K of
** RAM itself, so the simulation architecture has hitherto avoided the
** use of large lookup tables.  For the PPC, and any other more modern
** machine, this is no longer much of an issue.
*/

#include <stdio.h>

#define CF 1
#ifdef Z80
#  define NF 2
#  define ZPF 4
#  define VF 4
/* Z80 add/subtract V flag logic taken from 68K flags register description. */
#  define AVF(s, d, r) (((( (s)&(d)&~(r))|(~(s)&~(d)&(r))) >> 5) & VF)
#  define SVF(s, d, r) ((((~(s)&(d)&~(r))|( (s)&~(d)&(r))) >> 5) & VF)
#else
#  define NF 0
#  define ZPF 0			/* Force 8080 Parity only. */
#  define VF 0
#  define AVF(s, d, r) 2	/* Bit 1 always set. */
#  define SVF(s, d, r) 2
#endif
#define PF 4
#define HF 0x10
#define ZF 0x40
#define SF 0x80

int
main(argc, argv)
int argc;
char *argv[];
{
    long ii, jj, kk, ll, hsum, sum, hdiff, diff, flags, sflags, addr;
    unsigned char atable[2*256*256], stable[2*256*256], table[2*256*256];

    for (ii=0; ii<2; ii++) {	       /* Carry/Borrow */
	for (jj=0; jj<256; jj++) {     /* Addend 1/Subtrahend */
	    for (kk=0; kk<256; kk++) { /* Addend 2/Minuend */
		sum = jj + kk + ii;
		hsum = (jj & 0x0F) + (kk & 0x0F) + ii;
		flags = 0;
		if (!(sum & 0xFF))
		    flags |= ZF;
		if (sum > 0xFF)
		    flags |= CF;
		if (hsum > 0xF)
		    flags |= HF;
		if (hsum > 0xF)
		    sflags |= HF;
		if (sum & 0x80)
		    flags |= SF;
		flags |= AVF(kk, jj, sum);

		diff = ((jj - kk) - ii) & 0x1FF;
		hdiff =(((jj & 0x0F) - (kk & 0x0F)) - ii) & 0x1FF;
		sflags = 0;
		if (diff & 0x100)
		    sflags |= CF;
		if (diff & 0x80)
		    sflags |= SF;
		if (!(diff & 0xFF))
		    sflags |= ZF;
		if (hdiff > 0xF)
		    sflags |= HF;
		sflags |= SVF(kk, jj, diff);
		sflags |= NF;

		addr = ii<<16 | jj<<8 | kk;
		table[addr] = sum;
		atable[addr] = flags;
		stable[addr] = sflags;
	    }
	}
    }

#if 1 // Normal output.
    printf("sums:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    sum = table[ii<<16 | jj<<8 | kk << 3 | ll];
		    printf("0x%02X%s", sum, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | Sum %d + %02X + %02X..%02X\n", ii, jj,
			       kk<<3, (kk<<3)+ll);
		}
	    }
	}
    }

    printf("\naflags:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    flags = atable[ii<<16 | jj<<8 | kk<<3 | ll];
		    printf("0x%02X%s", flags, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | Add %d + %02X + %02X..%02X\n", ii, jj,
			       kk<<3, (kk<<3)+ll);
		}
	    }
	}
    }

    printf("\nsflags:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    sflags = stable[ii<<16 | jj<<8 | kk<<3 | ll];
		    printf("0x%02X%s", sflags, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | Sub %02X - %02X..%02X - %d\n", jj,
			       kk<<3, (kk<<3)+ll, ii);
		}
	    }
	}
    }
#else // Test the tables.
    printf("\nsumerr:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    addr = ii<<16 | jj<<8 | kk<<3 | ll;
		    sum = (ii + jj + ((kk<<3) + ll)) & 0xFF;
		    sum ^= table[addr];
		    printf("0x%02X%s", sum, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | Sumerr %d + %02X + %02X..%02X\n", ii, jj,
			       kk<<3, (kk<<3)+ll);
		    if (sum) {
			fprintf(stderr, "Sum error: %d + %02X + %02X"
				", %02X (table[%05X]) is not %02X\n", ii, jj,
				(kk<<3)+ll, table[addr], addr,
				(ii + jj + ((kk<<3) + ll)) & 0xFF);
			return 1;
		    }
		}
	    }
	}
    }

    printf("\ndiferr:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    addr = ((ii^1)<<16) | jj << 8 | (((kk<<3) | ll) ^ 0xFF);
		    diff = ((jj - ((kk<<3) + ll)) - ii) & 0xFF;
		    diff ^= table[addr];
		    printf("0x%02X%s", diff, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | Diferr %02X - %02X..%02X - %d\n", jj,
			       kk<<3, (kk<<3)+ll, ii);
		    if (diff) {
			fprintf(stderr, "Diff error: %02X - %02X - %d"
				", %02X (table[%05X]) is not %02X\n", jj,
				(kk<<3)+ll, ii, table[addr], addr,
				((jj - ((kk<<3) + ll)) - ii) & 0xFF);
			return 2;
		    }
		}
	    }
	}
    }
    
    printf("\naferr:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    addr = ii<<16 | jj<<8 | kk<<3 | ll;
		    sum = (ii + jj + ((kk<<3) + ll)) & 0x1FF;
		    hsum = ii + (jj & 0x0F) + (((kk<<3) + ll) & 0x0F);
		    flags = 0;
		    if (sum & 0x100)
			flags |= CF;
		    if (sum & 0x80)
			flags |= SF;
		    if (!(sum & 0xFF))
			flags |= ZF;
		    if (hsum > 0xF)
			flags |= HF;
		    flags |= AVF(jj, ((kk<<3) + ll), sum);
		    diff = flags ^ atable[addr];
		    printf("0x%02X%s", diff, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | aferr %d + %02X + %02X..%02X\n", ii, jj,
			       kk<<3, (kk<<3)+ll);
		    if (diff) {
			fprintf(stderr, "AF error: %d + %02X + %02X"
				", %02X (atable[%05X]) is not %02X\n", ii, jj,
				(kk<<3)+ll, atable[addr], addr, flags);
			return 3;
		    }
		}
	    }
	}
    }

    printf("\nsferr:");
    for (ii=0; ii<2; ii++) {
	for (jj=0; jj<256; jj++) {
	    for (kk=0; kk<32; kk++) {
		printf("\t.byte ");
		for (ll=0; ll<8; ll++) {
		    addr = ii<<16 | jj<<8 | kk<<3 | ll;
		    sum = ((jj - ((kk<<3) + ll)) - ii) & 0x1FF;
		    hsum =(((jj & 0x0F) - (((kk<<3) + ll)&0x0F) - ii)) & 0x1FF;
		    flags = 0;
		    if (sum & 0x100)
			flags |= CF;
		    if (sum & 0x80)
			flags |= SF;
		    if (!(sum & 0xFF))
			flags |= ZF;
		    if (hsum > 0xF)
			flags |= HF;
		    flags |= SVF(((kk<<3) + ll), jj, sum);
		    flags |= NF;
		    diff = flags ^ stable[addr];
		    printf("0x%02X%s", diff, (ll == 7) ? "" : ",");
		    if (ll == 7)
			printf(" | sferr %02X - %02X..%02X - %d\n", jj,
			       kk<<3, (kk<<3)+ll, ii);
		    if (diff) {
			fprintf(stderr, "SF error: %02X - %02X - %d"
				", %02X (stable[%05X]) is not %02X\n", jj,
				(kk<<3)+ll,ii,stable[addr], addr,
				flags);
			return 4;
		    }
		}
	    }
	}
    }
    
#endif
    return 0;
}
