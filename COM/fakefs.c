/*
** Embedded simplistic file system so that a single Unix
** executable can be built of something like, say, WordStar.
** So something disgusting like this:
**
**	jimc$ ws /etc/passwd
**
** will work, and the executable will be a single copyable file
** rather than a shell script dependent on a directory structure
** somewhere.
**
** This isn't a real file system, it's just an archive of
** embedded read-only data for use by COM's file handling code.
*/

#include "com.h"

#ifdef WORDSTAR
extern char ws_com_name[], ws_com[];
extern int ws_com_size;
extern char wsmsgs_ovr_name[], wsmsgs_ovr[];
extern int wsmsgs_ovr_size;
extern char wsovly1_ovr_name[], wsovly1_ovr[];
extern int wsovly1_ovr_size;
#endif

#ifdef MBASIC
extern char mbasic_com_name[], mbasic_com[];
extern int mbasic_com_size;
#endif

#ifdef ZORK1
extern char zork1_com_name[], zork1_com[];
extern int zork1_com_size;
extern char zork1_dat_name[], zork1_dat[];
extern int zork1_dat_size;
#endif

#ifdef ZORK2
extern char zork2_com_name[], zork2_com[];
extern int zork2_com_size;
extern char zork2_dat_name[], zork2_dat[];
extern int zork2_dat_size;
#endif

#ifdef ZORK3
extern char zork3_com_name[], zork3_com[];
extern int zork3_com_size;
extern char zork3_dat_name[], zork3_dat[];
extern int zork3_dat_size;
#endif

/*
** The first file in this array is what is executed when the EMBED
** version is built.
*/

struct fakefile fakefs[] = {
#ifdef WORDSTAR
    ws_com_name,
    ws_com,
    &ws_com_size,

    wsmsgs_ovr_name,
    wsmsgs_ovr,
    &wsmsgs_ovr_size,

    wsovly1_ovr_name,
    wsovly1_ovr,
    &wsovly1_ovr_size,
#endif

#ifdef MBASIC
    mbasic_com_name,
    mbasic_com,
    &mbasic_com_size,
#endif

#ifdef ZORK1
    zork1_com_name,
    zork1_com,
    &zork1_com_size,
    zork1_dat_name,
    zork1_dat,
    &zork1_dat_size,
#endif
#ifdef ZORK2
    zork2_com_name,
    zork2_com,
    &zork2_com_size,
    zork2_dat_name,
    zork2_dat,
    &zork2_dat_size,
#endif
#ifdef ZORK3
    zork3_com_name,
    zork3_com,
    &zork3_com_size,
    zork3_dat_name,
    zork3_dat,
    &zork3_dat_size,
#endif
    0, 0, 0			/* Some C compilers require >=1 element. */
};

int nfakefiles = sizeof(fakefs)/sizeof(fakefs[0]) - 1;
