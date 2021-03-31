        sbbflag

sbbyl   asr.b   #1,regf                 ;FD9D SBB YL (undocumented)
        move.b  regyl(regs),d0
        sbbflag

sbbiy   calcind y                       ;FD9E SBB (IY+d)
        asr.b   #1,regf
        move.b  0(targbase,d0.l),d0
        sbbflag

anayh   and.b   regyh(regs),rega        ;FDA4 ANA YH (undocumented)
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

anayl   and.b   regyl(regs),rega        ;FDA5 ANA YL (undocumented)
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

anaiy   calcind y                       ;FDA6 ANA (IY+d)
        and.b   0(targbase,d0.l),rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrayh   move.b  regyh(regs),d0          ;FDAC XRA YH (undocumented)
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xrayl   move.b  regyl(regs),d0          ;FDAD XRA YL (undocumented)
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

xraiy   calcind y                       ;FDAE XRA (IY+d)
        move.b  0(targbase,d0.l),d0
        eor.b   d0,rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orayh   or.b    regyh(regs),rega        ;FDB4 ORA YH (undocumented)
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

orayl   or.b    regyl(regs),rega        ;FDB5 ORA YL (undocumented)
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

oraiy   calcind y                       ;FDB6 ORA (IY+d)
        or.b    0(targbase,d0.l),rega
        move.b  16(flagptr,rega.w),regf
        jmp     (return)

cmpyh   cmp.b   regyh(regs),rega        ;FDBC CMP YH (undocumented)
        setflag

cmpyl   cmp.b   regyl(regs),rega        ;FDBD CMP YL (undocumented)
        setflag

cmpiy   calcind y                       ;FDBE CMP (IY+d)
        cmp.b   0(targbase,d0.l),rega
        setflag

* FDCB-prefix instructions can use the DDCB-prefix routines,
*  since the operand address is already in D0.

popiy   move.b  (pseudosp)+,regyl(regs) ;FDE1 POP IY
        move.b  (pseudosp)+,regyh(regs)
        jmp     (return)

xtiy    move.b  regyl(regs),d0          ;FDE3 XTIY
        move.b  (pseudosp),regyl(regs)
        move.b  d0,(pseudosp)
        move.b  regyh(regs),d0
        move.b  1(pseudosp),regyh(regs)
        move.b  d0,1(pseudosp)
        jmp     (return)

pushiy  move.b  regyh(regs),-(pseudosp) ;FDE5 PUSH IY
        move.b  regyl(regs),-(pseudosp)
        jmp     (return)

pciy    move.w  regiy(regs),d0          ;FDE9 PCIY
        lea     0(targbase,d0.l),pseudopc
        jmp     (return)

spiy    move.w  regiy(regs),d0          ;FDF9 SPIY
        lea     0(targbase,d0.l),pseudosp
        jmp     (return)
        page
*************************************************************************
*                                                                       *
*       Opcode dispatch table                                           *
*                                                                       *
*************************************************************************
        data    data

        even
optabl  dc.l    nop00,lxib,staxb,inxb,inrb,dcrb,mvib,rlc
        dc.l    exaf,dadb,ldaxb,dcxb,inrc,dcrc,mvic,rrc
        dc.l    djnz,lxid,staxd,inxd,inrd,dcrd,mvid,ral
        dc.l    jr,dadd,ldaxd,dcxd,inre,dcre,mvie,rar
        dc.l    jrnz,lxih,shld,inxh,inrh,dcrh,mvih,daa
        dc.l    jrz,dadh,lhld,dcxh,inrl,dcrl,mvil,cma
        dc.l    jrnc,lxis,sta,inxs,inrm,dcrm,mvim,stc
        dc.l    jrc,dads,lda,dcxs,inra,dcra,mvia,cmc
        dc.l    movebb,movebc,movebd,movebe,movebh,movebl,movebm,moveba
        dc.l    movecb,movecc,movecd,movece,movech,movecl,movecm,moveca
        dc.l    movedb,movedc,movedd,movede,movedh,movedl,movedm,moveda
        dc.l    moveeb,moveec,moveed,moveee,moveeh,moveel,moveem,moveea
        dc.l    movehb,movehc,movehd,movehe,movehh,movehl,movehm,moveha
        dc.l    movelb,movelc,moveld,movele,movelh,movell,movelm,movela
        dc.l    movemb,movemc,movemd,moveme,movemh,moveml,halt,movema
        dc.l    moveab,moveac,movead,moveae,moveah,moveal,moveam,moveaa
        dc.l    addb,addc,addd,adde,addh,addl,addm,adda
        dc.l    adcb,adcc,adcd,adce,adch,adcl,adcm,adca
        dc.l    subb,subc,subd,sube,subh,subl,subm,suba
        dc.l    sbbb,sbbc,sbbd,sbbe,sbbh,sbbl,sbbm,sbba
        dc.l    andb,andc,andd,ande,andh,andl,andm,anda
        dc.l    xrab,xrac,xrad,xrae,xrah,xral,xram,xraa
        dc.l    orab,orac,orad,orae,orah,oral,oram,oraa
        dc.l    cmpb,cmpc,cmpd,cmpe,cmph,cmpl,cmpam,cmpaa
        dc.l    rnz,popb,jnz,jmpa,cnz,pushb,adi,rst0
        dc.l    rz,ret,jz,preCB,cz,call,aci,rst1
        dc.l    rnc,popd,jnc,out,cnc,pushd,sui,rst2
        dc.l    rc,exx,jc,in,cc,preDD,sbi,rst3
        dc.l    rpo,poph,jpo,xthl,cpo,pushh,ani,rst4
        dc.l    rpe,pchl,jpe,xchg,cpe,preED,xri,rst5
        dc.l    rp,popp,jp,di,cp,pushp,oria,rst6
        dc.l    rm,sphl,jm,ei,cm,preFD,cpi,rst7
        page
*************************************************************************
*                                                                       *
*       Flag register lookup tables                                     *
*                                                                       *
*************************************************************************
flags   dc.b    $00,$01,$04,$05,$40,$41,$44,$45,$80,$81,$84,$85,$C0,$C1,$C4,$C5
        dc.b    $44,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $04,$00,$00,$04,$00,$04,$04,$00,$00,$04,$04,$00,$04,$00,$00,$04
        dc.b    $00,$04,$04,$00,$04,$00,$00,$04,$04,$00,$00,$04,$00,$04,$04,$00
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $80,$84,$84,$80,$84,$80,$80,$84,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        dc.b    $80,$84,$84,$80,$84,$80,$84,$80,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $80,$84,$84,$80,$84,$80,$84,$80,$84,$80,$80,$84,$80,$84,$84,$80
        dc.b    $84,$80,$80,$84,$80,$84,$84,$80,$80,$84,$84,$80,$84,$80,$80,$84
        page
*************************************************************************
*                                                                       *
*       Z-80 opcode dispatch table.  One longword entry per opcode      *
*       of the target (Z-80) processor, including illegals.             *
*                                                                       *
*************************************************************************

CBoptab:
        dc.l    rlcb,rlcc,rlcd,rlce,rlch,rlcl,rlcm,rlca                 ;CB00
        dc.l    rrcb,rrcc,rrcd,rrce,rrch,rrcl,rrcm,rrca                 ;CB08
        dc.l    rlrb,rlrc,rlrd,rlre,rlrh,rlrl,rlrm,rlra                 ;CB10
        dc.l    rrrb,rrrc,rrrd,rrre,rrrh,rrrl,rrrm,rrra                 ;CB18
        dc.l    slab,slac,slad,slae,slah,slal,slam,slaa                 ;CB20
        dc.l    srab,srac,srad,srae,srah,sral,sram,sraa                 ;CB28
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;CB30
        dc.l    srlb,srlc,srld,srle,srlh,srll,srlm,srla                 ;CB38
        dc.l    bit0b,bit0c,bit0d,bit0e,bit0h,bit0l,bit0m,bit0a         ;CB40
        dc.l    bit1b,bit1c,bit1d,bit1e,bit1h,bit1l,bit1m,bit1a         ;CB48
        dc.l    bit2b,bit2c,bit2d,bit2e,bit2h,bit2l,bit2m,bit2a         ;CB50
        dc.l    bit3b,bit3c,bit3d,bit3e,bit3h,bit3l,bit3m,bit3a         ;CB58
        dc.l    bit4b,bit4c,bit4d,bit4e,bit4h,bit4l,bit4m,bit4a         ;CB60
        dc.l    bit5b,bit5c,bit5d,bit5e,bit5h,bit5l,bit5m,bit5a         ;CB68
        dc.l    bit6b,bit6c,bit6d,bit6e,bit6h,bit6l,bit6m,bit6a         ;CB70
        dc.l    bit7b,bit7c,bit7d,bit7e,bit7h,bit7l,bit7m,bit7a         ;CB78
        dc.l    res0b,res0c,res0d,res0e,res0h,res0l,res0m,res0a         ;CB80
        dc.l    res1b,res1c,res1d,res1e,res1h,res1l,res1m,res1a         ;CB88
        dc.l    res2b,res2c,res2d,res2e,res2h,res2l,res2m,res2a         ;CB90
        dc.l    res3b,res3c,res3d,res3e,res3h,res3l,res3m,res3a         ;CB98
        dc.l    res4b,res4c,res4d,res4e,res4h,res4l,res4m,res4a         ;CBA0
        dc.l    res5b,res5c,res5d,res5e,res5h,res5l,res5m,res5a         ;CBA8
        dc.l    res6b,res6c,res6d,res6e,res6h,res6l,res6m,res6a         ;CBB0
        dc.l    res7b,res7c,res7d,res7e,res7h,res7l,res7m,res7a         ;CBB8
        dc.l    set0b,set0c,set0d,set0e,set0h,set0l,set0m,set0a         ;CBC0
        dc.l    set1b,set1c,set1d,set1e,set1h,set1l,set1m,set1a         ;CBC8
        dc.l    set2b,set2c,set2d,set2e,set2h,set2l,set2m,set2a         ;CBD0
        dc.l    set3b,set3c,set3d,set3e,set3h,set3l,set3m,set3a         ;CBD8
        dc.l    set4b,set4c,set4d,set4e,set4h,set4l,set4m,set4a         ;CBE0
        dc.l    set5b,set5c,set5d,set5e,set5h,set5l,set5m,set5a         ;CBE8
        dc.l    set6b,set6c,set6d,set6e,set6h,set6l,set6m,set6a         ;CBF0
        dc.l    set7b,set7c,set7d,set7e,set7h,set7l,set7m,set7a         ;CBF8

DDoptab:
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DD00
        dc.l    illgED,dadixb,illgED,illgED,illgED,illgED,illgED,illgED ;DD08
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DD10
        dc.l    illgED,dadixd,illgED,illgED,illgED,illgED,illgED,illgED ;DD18
        dc.l    illgED,lxiix,sixd,inxix,inrxh,dcrxh,mvixh,illgED        ;DD20
        dc.l    illgED,dadixx,lixd,dcxix,inrxl,dcrxl,mvixl,illgED       ;DD28
        dc.l    illgED,illgED,illgED,illgED,inrix,dcrix,mviix,illgED    ;DD30
        dc.l    illgED,dadixs,illgED,illgED,illgED,illgED,illgED,illgED ;DD38
        dc.l    illgED,illgED,illgED,illgED,movbxh,movbxl,movbix,illgED ;DD40
        dc.l    illgED,illgED,illgED,illgED,movcxh,movcxl,movcix,illgED ;DD48
        dc.l    illgED,illgED,illgED,illgED,movdxh,movdxl,movdix,illgED ;DD50
        dc.l    illgED,illgED,illgED,illgED,movexh,movexl,moveix,illgED ;DD58
        dc.l    movxhb,movxhc,movxhd,movxhe,mvxhxh,mvxhxl,movhix,illgED ;DD60
        dc.l    movxlb,movxlc,movxld,movxle,mvxlxh,mvxlxl,movlix,illgED ;DD60
        dc.l    movixb,movixc,movixd,movixe,movixh,movixl,illgED,movixa ;DD70
        dc.l    illgED,illgED,illgED,illgED,movaxh,movaxl,movaix,illgED ;DD78
        dc.l    illgED,illgED,illgED,illgED,addxh,addxl,addix,illgED    ;DD80
        dc.l    illgED,illgED,illgED,illgED,adcxh,adcxl,adcix,illgED    ;DD88
        dc.l    illgED,illgED,illgED,illgED,subxh,subxl,subix,illgED    ;DD90
        dc.l    illgED,illgED,illgED,illgED,sbbxh,sbbxl,sbbix,illgED    ;DD98
        dc.l    illgED,illgED,illgED,illgED,anaxh,anaxl,anaix,illgED    ;DDA0
        dc.l    illgED,illgED,illgED,illgED,xraxh,xraxl,xraix,illgED    ;DDA8
        dc.l    illgED,illgED,illgED,illgED,oraxh,oraxl,oraix,illgED    ;DDB0
        dc.l    illgED,illgED,illgED,illgED,cmpxh,cmpxl,cmpix,illgED    ;DDB8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDC0
        dc.l    illgED,illgED,illgED,prDDCB,illgED,illgED,illgED,illgED ;DDC8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDD0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDD8
        dc.l    illgED,popix,illgED,xtix,illgED,pushix,illgED,illgED    ;DDE0
        dc.l    illgED,pcix,illgED,illgED,illgED,illgED,illgED,illgED   ;DDE8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;DDF0
        dc.l    illgED,spix,illgED,illgED,illgED,illgED,illgED,illgED   ;DDF8

DDCBopt:
        dc.l    rlcix,rrcix,rlix,rrix,slaix,sraix,ilgDDCB,srlix         ;DDCB06
        dc.l    bit0ix,bit1ix,bit2ix,bit3ix,bit4ix,bit5ix,bit6ix,bit7ix ;DDCB46
        dc.l    res0ix,res1ix,res2ix,res3ix,res4ix,res5ix,res6ix,res7ix ;DDCB86
        dc.l    set0ix,set1ix,set2ix,set3ix,set4ix,set5ix,set6ix,set7ix ;DDCBC6

EDoptab:
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED00
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED08
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED10
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED18
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED20
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED28
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED30
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED38
        dc.l    inbc,outcb,dsbbb,sbcd,nega,retn,im0,moveia              ;ED40
        dc.l    incc,outcc,dadcb,lbcd,illgED,reti,illgED,movera         ;ED48
        dc.l    indc,outcd,dsbbd,sded,illgED,illgED,im1,moveai          ;ED50
        dc.l    inec,outce,dadcd,lded,illgED,illgED,im2,movear          ;ED58
        dc.l    inhc,outch,dsbbh,shld,illgED,illgED,illgED,rrd          ;ED60
        dc.l    inlc,outcl,dadch,lhld,illgED,illgED,illgED,rld          ;ED68
        dc.l    inmc,outcm,dsbbs,sspd,illgED,illgED,illgED,illgED       ;ED70
        dc.l    inac,outca,dadcs,lspd,illgED,illgED,illgED,illgED       ;ED78
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED80
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED88
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED90
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;ED98
        dc.l    ldi,cmpi,ini,outi,illgED,illgED,illgED,illgED           ;EDA0
        dc.l    ldd,cpd,ind,outd,illgED,illgED,illgED,illgED            ;EDA8
        dc.l    ldir,cpir,inir,otir,illgED,illgED,illgED,illgED         ;EDB0
        dc.l    lddr,cpdr,indr,otdr,illgED,illgED,illgED,illgED         ;EDB8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDC0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDC8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDD0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDD8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDE0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDE8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDF0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;EDF8

FDoptab:
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FD00
        dc.l    illgED,dadiyb,illgED,illgED,illgED,illgED,illgED,illgED ;FD08
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FD10
        dc.l    illgED,dadiyd,illgED,illgED,illgED,illgED,illgED,illgED ;FD18
        dc.l    illgED,lxiiy,siyd,inxiy,inryh,dcryh,mviyh,illgED        ;FD20
        dc.l    illgED,dadiyy,liyd,dcxiy,inryl,dcryl,mviyl,illgED       ;FD28
        dc.l    illgED,illgED,illgED,illgED,inriy,dcriy,mviiy,illgED    ;FD30
        dc.l    illgED,dadiys,illgED,illgED,illgED,illgED,illgED,illgED ;FD38
        dc.l    illgED,illgED,illgED,illgED,movbyh,movbyl,movbiy,illgED ;FD40
        dc.l    illgED,illgED,illgED,illgED,movcyh,movcyl,movciy,illgED ;FD48
        dc.l    illgED,illgED,illgED,illgED,movdyh,movdyl,movdiy,illgED ;FD50
        dc.l    illgED,illgED,illgED,illgED,moveyh,moveyl,moveiy,illgED ;FD58
        dc.l    movyhb,movyhc,movyhd,movyhe,mvyhyh,mvyhyl,movhiy,illgED ;FD60
        dc.l    movylb,movylc,movyld,movyle,mvylyh,mvylyl,movliy,illgED ;FD60
        dc.l    moviyb,moviyc,moviyd,moviye,moviyh,moviyl,illgED,moviya ;FD70
        dc.l    illgED,illgED,illgED,illgED,movayh,movayl,movaiy,illgED ;FD78
        dc.l    illgED,illgED,illgED,illgED,addyh,addyl,addiy,illgED    ;FD80
        dc.l    illgED,illgED,illgED,illgED,adcyh,adcyl,adciy,illgED    ;FD88
        dc.l    illgED,illgED,illgED,illgED,subyh,subyl,subiy,illgED    ;FD90
        dc.l    illgED,illgED,illgED,illgED,sbbyh,sbbyl,sbbiy,illgED    ;FD98
        dc.l    illgED,illgED,illgED,illgED,anayh,anayl,anaiy,illgED    ;FDA0
        dc.l    illgED,illgED,illgED,illgED,xrayh,xrayl,xraiy,illgED    ;FDA8
        dc.l    illgED,illgED,illgED,illgED,orayh,orayl,oraiy,illgED    ;FDB0
        dc.l    illgED,illgED,illgED,illgED,cmpyh,cmpyl,cmpiy,illgED    ;FDB8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDC0
        dc.l    illgED,illgED,illgED,prFDCB,illgED,illgED,illgED,illgED ;FDC8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDD0
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDD8
        dc.l    illgED,popiy,illgED,xtiy,illgED,pushiy,illgED,illgED    ;FDE0
        dc.l    illgED,pciy,illgED,illgED,illgED,illgED,illgED,illgED   ;FDE8
        dc.l    illgED,illgED,illgED,illgED,illgED,illgED,illgED,illgED ;FDF0
        dc.l    illgED,spiy,illgED,illgED,illgED,illgED,illgED,illgED   ;FDF8

* FDCBopt is not necessary - DDCBopt can be used,
*  since the operand address is in D0 for both routines.


        bss     bss

        even

tracesad ds.l   1               ;Start address for trace
traceead ds.l   1               ;End address for trace
traceflg ds.b   1               ;Trace activity flag
tmprega  ds.b   1               ;Work area
        end
