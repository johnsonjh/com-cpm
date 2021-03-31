10 ?"A  PARANOID  PROGRAM  to  DIAGNOSE  FLOATING-POINT  ARITHMETIC"
20 ? "in  BASIC  v. A1.10  (May 1982) on the  IBM  Personal Computer."
30 '*****  Use only one of the following two statement lines !  *****
40   ? "          ... Single Precision version  ..."
50 ' DEFDBL A-H, O-Z : ? "          ... Double Precision version  ..."
60 '*****************************************************************
70 DEFINT I-N : I0=20 : ' ...  I0 = #( random trials of X*Y=Y*X , etc.)
80 GOTO 160
90 ' This program runs interactively putting all output to a screen.  The next
100 ' Subprogram pauses, allowing you to read the screen or copy it.
110 ? CHR$(7) : INPUT; "To continue diagnosis, press ENTER  [<-']  key:",X$
120 ? CHR$(12) : ' ... or  HOME,  or  CLS,  or   FOR I=1 TO 22 : ? : NEXT I
130 K=K+1 : ? "Diagnosis resumes after milestone  #L =";L;",            ... page";K
140 L=L+1 : ? : RETURN
150 ' -----------------------------------------------
160 K=0 : L=0 : ' ... K = #( page of diagnosis ),  L = Milestone in program
170 ? : ? "Lest this program stop prematurely, i.e. before displaying"
180 ? "                 `   End of Test   ' ,"
190 ? "try to persuade the computer NOT to terminate execution whenever an"
200 ? "`error'  like Over/Underflow or Division by Zero occurs, but rather"
210 ? "to persevere with a surrogate value after, perhaps, displaying some"
220 ? "warning.   If persuasion avails naught,  don't despair but run this"
230 ? "program anyway to see how many milestones (#L) it passes, and then"
240 ? "amend it to make further progress.  (See program lines #250 - 370.)"
250 '________ Only for  Compiled BASIC  on the  IBM PC  ________
260 V8=0 : Q8=0 : ON ERROR GOTO 270 : GOTO 380
270 IF NOT ( ERR=5 OR ERR=6 OR ERR=11) THEN ON ERROR GOTO 0
280 IF NOT (ERR=5) THEN 320
290 IF NOT (ERL=3030 AND I=1) THEN ON ERROR GOTO 0
300 Y=-O : J1=J1+1 : ? S$;"BUG:  If X=0 then  SQR(-X)  stops the machine !
310 RESUME NEXT
320 E8=O2^63 : E8=E8*O2*F9*E8 : IF (ERR=6) THEN 340
330 Q8=O1+Q8 : Q9=E8 : B8=Q8 : ? "DIVISION by ZERO ..." : GOTO 350
340 V8=O1+V8 : V9=E8 : B8=V8 : ? "OVERFLOW ..." : IF (ERL=5530 OR ERL=5520) THEN V9=-V9
350 IF (B8>O1) THEN RESUME NEXT
360 J2=J2+1 : ? "were it not simulated here, it would Stop the machine!  This is a DEFECT." : RESUME NEXT
370 '~~~~~~~~           end of error-handler            ~~~~~~~~~
380 ?
390 ? "Users are invited to help debug and augment this program so it will"
400 ? "cope with unanticipated and newly uncovered arithmetic pathologies."
410 ? "Please send suggestions and interesting results to the author:"
420 ? "*(C) 1983                     Prof. W. M. Kahan,   567 Evans Hall,"
430 ? "  Apr. 19                     Elect. Eng. & Computer Science Dept.,"
440 ? "  ~~~~~~~                     University of California,"
450 ? "                              Berkeley,   Calif. 94720"
460 ? "*  You may copy this program freely if you acknowledge its source."
470 GOSUB 110 : ' ---- PAUSE ----
480 ? "Running this program should reveal these characteristics:"
490 ? "  B = Radix ( 1, 2, 4, 8, 10, 16, 100, 256, or ... ) ."
500 ? "  P = Precision, the number of significant  B-digits carried."
510 ? "  U2 = B/B^P = One Ulp (Unit in the Last Place) of 1.000xxx... ."
520 ? "  U1 = 1/B^P = One Ulp of numbers a little less than 1.0 ."
530 ? "  G1, G2, G3  tell whether adequate Guard Digits are carried;"
540 ? "   1 = YES ,  0 = NO ;   G1 for MULT.,  G2 for DIV.,  G3 for SUBT."
550 ? "  R1, R2, R3, R4  tell whether arithmetic is rounded or chopped;"
560 ? "   0 = chopped,  1 = correctly rounded,  -1 = some other rounding;"
570 ? "   R1 for MULT.,   R2 for DIV.,   R3 for ADD/SUBT.,   R4 for SQRT."
580 ? "  S=1 when a sticky bit is used correctly in rounding; else S=0."
590 ? "  U0 = an underflow threshold."
600 ? "  E0 and Z0  tell whether underflow is abrupt, gradual or fuzzy."
610 ? "  V = an overflow threshold, roughly."
620 ? "  V0   tells, roughly, whether  Infinity  is represented."
630 ? "  Comparisons are checked for consistency with subtraction"
640 ? "                      and for contamination by pseudo-zeros."
650 ? "  SQRT is tested.  So is  Y^X  for (mostly) integers  X ."
660 ? "  Extra-precise subexpressions are revealed but NOT YET tested."
670 ? "  Decimal-Binary conversion is NOT YET tested for accuracy."
680 GOSUB 110 : ' ---- PAUSE ----
690 ? "The program attempts to discriminate among"
700 ? "  FLAWs, like lack of a sticky bit,                          (J3)"
710 ? "  Serious DEFECTs, like lack of a guard digit, and       (J1, J2)"
720 ? "  FAILUREs, like  2+2 = 5 .                                  (J0)"
730 ? "Failures may confound subsequent diagnoses."
740 ? "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
750 ?
760 ? "The diagnostic capabilities of this program go beyond an earlier"
770 ? "program called  `MACHAR' , which can be found at the end of the"
780 ? "book  `Software Manual for the Elementary Functions'  (1980)  by"
790 ? "W. J. Cody and W. Waite.  Although both programs try to discover"
800 ? "the radix (B), precision (P) and range (over/underflow thresholds)"
810 ? "of the arithmetic, this program tries to cope with a wider variety"
820 ? "of pathologies, and to say how well the arithmetic is implemented."
830 ?
840 ? "The program is based upon a conventional radix representation for"
850 ? "floating-point numbers, but also allows for logarithmic encoding"
860 ? "(B=1)  as used by certain early WANG machines."
870 ?
880 L=7 : GOSUB 110 : ' ---- PAUSE ---- =====================================
890 J0=0 : O=0 : O1=1 : ? "Program is now RUNNING tests on small integers:"
900 F$="FAILURE:" : V$=" Violation of " : M$="  Multiplication"
910 O2=O1+O1 : IF (O+O=O AND O1-O1=O AND O1>O AND O2=2) THEN 930
920 J0=1 : ? F$;V$;"  0+0=0  or 1-1=0 or 1 > 0  or  1+1 = 2 "
930 Z=-O : IF (Z=O) THEN 960
940 J0=J0+1 : ? F$;" Comparison alleges that Minus Zero, obtained by setting" : ? "         X = 0  and then  Z = -X ,  is nonzero!
950 U2=.001 : Z$="Z" : B=1 : GOSUB 4640
960 O3=O2+O1 : O4=O3+O1 : IF (O4+O2*(-O2) = O AND (O4-O3)-O1 = O) THEN 980
970 J0=J0+1 : ? F$;V$;"  3+1 = 2*2 "
980 F1=-O1 : IF (F1+O1=O AND O1+F1=O AND F1+ABS(F1)=O AND F1+F1*F1=O) THEN 1000
990 J0=J0+1 : ? F$;V$;"  -1 + 1 = 0 "
1000 F2=O1/O2 : IF (F2+F1+F2=O) THEN 1020
1010 J0=J0+1 : ? F$;V$;"  1/2 - 1 + 1/2 = 0"
1020 L=10 : 'Miscellaneous variables: ========================================
1030 J0=J0 : J1=0 : J2=0 : J3=0 : ' ... count FAILUREs, serious DEFECTS, FLAWS
1040 G1=O1 : G2=O1 : G3=O1 : '  ... to record guard digits
1050 A$="  Add/Subtract" : B$="correctly rounded." : C$="chopped"
1060 D$="  Division" : S$="SERIOUS " : L$=" lacks a Guard Digit, "
1070 E$=" appears to be " : H$=" is neither " : Q$="Square Root"
1080 I$="  gets too many last digits wrong."
1090 P$=" test is inconsistent;  PLEASE  NOTIFY  THE  AUTHOR !"
1100 O9=O3*O3 : T7=O9*O3 : O8=O4+O4 : T2=O4*O8 : IF (T2-T7-O4-O1=O) THEN 1120
1110 J0=J0+1 : ? F$;V$;"  32 - 27 - 4 - 1 = 0"
1120 O5=O4+O1 : S=O4*O5 : T=O3*O4 : T8=S*T : ' ... T8=240
1130 X=T8/O3 : Y=T8/O4 : Z=T8/O5 : X=X-O4*S : Y=Y-O5*T : Z=Z-O4*T : IF ( X=O AND Y=O AND Z=O ) THEN 1150
1140 J0=J0+1 : ? F$;V$;" 240/3 = 80  or  240/4 = 60  or  240/5 = 48."
1150 IF (J0=0) THEN ? " -1, 0, 1/2 , 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K." : ?
1160 ? "Searching for radix  B  and precision  P ; ";
1170 W=O1
1180 W=W+W : Y=W+O1 : Z=Y-W : Y=Z-O1 : IF (F1+ABS(Y)<O) THEN 1180
1190 '... now  W  is just big enough that  |((W+1)-W)-1| >= 1 ...
1200 P=O :  Y=O1
1210 B=W+Y : Y=Y+Y : B=B-W : IF (B=O) THEN 1210
1220 IF (B<O2) THEN B=O1
1230 ? " Radix  B = "; B : IF (B=O1) THEN 1270  :' ... else  B >= 2 ...
1240 W=O1
1250 P=P+O1 : W=W*B : Y=W+O1 : Z=Y-W : IF (Z=O1) THEN 1250
1260 ' ... now  W = B^P  is barely too big to satisfy  (W+1)-W = 1  ...
1270 U1=O1/W : U2=B*U1 : ? "Closest relative separation found is  U1 ="; U1
1280 ? : ? "Recalculating radix and precision ";
1290 E0=B : E1=U1 : E9=U2 : E3=P :'  ...  save old values
1300 X=O4/O3 : F3=X-O1 : F6=F2-F3 : X=F6+F6 : X=ABS(X-F3) : IF (X<U2) THEN X=U2
1310 '  ... now  x = (unknown no.) ulps of  1 + ...
1320 U2=X : Y=F2*U2+T2*U2*U2 : Y=O1+Y : X=Y-O1 : IF (U2>X AND X>O) THEN 1320
1330 '  ... now  u2 = 1 ulp of  1 + ...
1340 X=O2/O3 : F6=X-F2 : F3=F6+F6 : X=F3-F2 : X=ABS(X+F6) : IF (X<U1) THEN X=U1
1350 '  ... now  x = (unknown no.) ulps of  1 - ...
1360 U1=X : Y=F2*U1+T2*U1*U1 : Y=F2-Y : X=F2+Y : Y=F2-X : X=F2+Y : IF (U1>X AND X>O) THEN 1360
1370 '  ... now  u1 = 1 ulp of  1 - ...
1380 IF (U1=E1) THEN ? " confirms closest relative separation  U1 ."
1390 IF (U1><E1) THEN ? " gets better closest relative separation  U1 = "; U1
1400 W=O1/U1 : F9=(F2-U1)+F2 : ' ... = 1 - U1 = nextafter(1.0, 0)
1410 B=INT(.01 + U2/U1) : IF (B=E0) THEN ? "Radix  B  confirmed."
1420 IF (B><E0) THEN ? "MYSTERY: recalculated radix  B = "; B
1430 IF (B=O2 OR B-O9=O1 OR B=O1) THEN 1470
1440 IF (B>O8+O8) THEN 1460
1450 J3=J3+1 : ? "FLAW: Radix  B =";B;" is not so good as  2  or  10." : GOTO 1470
1460 J2=J2+1 : ? "DEFECT: Radix  B =";B;" is so big that roundoff propagates capriciously."
1470 L=20 : ' Test for fuzzy comparison ... ==================================
1480 IF (F9-F2<F2) THEN 1510
1490 J0=J0+1
1500 ? F$;"  (1-U1)-1/2 < 1/2  is FALSE, so this program may malfunction."
1510 X=F9 : I=1
1520 Y=X-F2 : Z=Y-F2 : IF (X><O1 OR Z=O) THEN 1540
1530 J0=J0+1 : ? F$;"  Comparison is fuzzy; it alleges  X = 1  although" : ? "          subtraction yields  (X - 1/2) - 1/2 = ";Z
1540 IF (I=0) THEN 1560
1550 X=O1+U2 : I=0 : GOTO 1520
1560 L=25 : ' End of test for fuzzy comparison.===============================
1570 B9=B-O1 : B9=(B9-U2)+O1 : IF (B=O1) THEN 1610 : ' ... B9 = nextafter(B, 0)
1580 X=-T8*LOG(U1)/LOG(B) : Y=INT(F2+X) : IF ( ABS(X-Y)*O4<O1 ) THEN X=Y
1590 P=X/T8 : Y=INT(F2+P) : IF ( ABS(P-Y)*T8<F2 ) THEN P=Y : ' Purify integers.
1600 IF (P=INT(P)) THEN 1640
1610 ? "Precision cannot be characterized by an integer number of sig. digits;" : IF (B>O1) THEN 1630
1620 ? "logarithmic encoding (B=1) has precision characterized solely by  U1 ." : GOTO 1650
1630 ? "but, by itself, this is a minor flaw."
1640 ? "The number of significant digits of radix  B  is  P = "; P
1650 IF (U2*O9*O9*T8<O1) THEN 1670
1660 J1=J1+1 : ? S$;"DEFECT: Precision worse than  5 sig. dec. is usually inadequate." : GOTO 1680
1670 ?
1680 L=30 : 'Test for extra-precise subexpressions:  ========================
1690 X=ABS( ((O4/O3-O1)-O1/O4)*O3 - O1/O4 )
1700 Z2=X : X=(O1+(F2*Z2+T2*Z2*Z2))-O1 : IF (Z2>X AND X>O) THEN 1700
1710 Y=ABS( (O3/O4-O2/O3)*O3 - O1/O4 ) : Z=Y : X=Y
1720 Z1=Z : Z=(O1/O2 - ((O1/O2-(F2*Z1+T2*Z1*Z1))+O1/O2)) + O1/O2 : IF (Z1>Z AND Z>O) THEN 1720
1730 Y1=Y : Y=(F2 - ((F2-(F2*Y1+T2*Y1*Y1))+F2)) + F2 : IF (Y1>Y AND Y>O) THEN 1730
1740 X1=X : X=((F2*X1+T2*X1*X1)-F9)+F9 : IF (X1>X AND X>O) THEN 1730
1750 IF (X1=Y1 AND X1=Z1) THEN 1780
1760 J1=J1+1 : ? S$;"DEFECT: Disagreements among the values  X1, Y1, Z1  resp." : ? X1;",     ";Y1;",     ";Z1 : ? "are symptoms of inconsistencies introduced by extra-precise evaluation of"
1770 ? "allegedly  `optimized'  arithmetic subexpressions.  Possibly some part of this" : ? P$ : IF (X1=U1 OR Y1=U1 OR Z1=U1) THEN 1850
1780 IF (Z1=U1 AND Z2=U2) THEN 1870
1790 IF (Z1<U1 AND Z2<U2) THEN 1810
1800 J0=J0+1 : ? F$;"Precision";P$ : ? "U1 =";U1;",  Z1-U1 =";Z1-U1 : ? "U2 =";U2;",  Z2-U2 =";Z2-U2 : GOTO 1860
1810 IF (Z1>O AND Z2>O) THEN 1830
1820 ? "Because of an unusual radix  B =";B;",  or exact rational arithmetic," : ? "a result  Z1 =";Z1;"  or  Z2 =";Z2;"  of an extra-precision" : ? P$ : IF (Z1=Z2) THEN 1850
1830 X=Z1/U1 : Y=Z2/U2 : IF (Y>X) THEN X=Y
1840 Q=-LOG(X) : ? "Some subexpressions appear to be calculated extra-precicely with" : ? "about   ";Q/LOG(B);" extra B-digits, i. e." : ? "roughly ";Q/LOG(10);" extra significant decimals."
1850 ? "  That feature is not tested further by this program."
1860 GOSUB 110 : ' ---- PAUSE ----
1870 L=35 : ' ================================================================
1880 IF (B<O2) THEN 1920
1890 X=W/(B*B) : Y=X+O1 : Z=Y-X : T=Z+U2 : X=T-Z : IF (X=U2) THEN 1910
1900 J0=J0+1 : ? F$;" Subtraction is not normalized, so  X=Y  does not imply  X+Z=Y+Z !" : GOTO 1920
1910 ? "Subtraction appears to be normalized, as it should."
1920 ?
1930 ? "Checking for guard digit in Multiply (G1), Divide (G2) and Subtract (G3) :"
1940 Y=F9*O1 : Z=O1*F9 : X=F9-F2 : Y=(Y-F2)-X : Z=(Z-F2)-X : X=O1+U2 : T=X*B
1950 R=B*X : X=T-B : X=X-B*U2 : T=R-B : T=T-B*U2 : X=X*(B-O1) : T=T*(B-O1) : IF (X=O AND Y=O AND Z=O AND T=O) THEN 1980
1960 J1=J1+1
1970 G1=O : ? S$;"DEFECT:";M$;L$;"violating  1*X = X ."
1980 Z=B*U2 : X=O1+Z : Y=ABS((X+Z)-X*X)-U2 : X=O1-U2 : Z=ABS((X-U2)-X*X)-U1 : IF (Y<=O AND Z<=O) THEN 2000
1990 J0=J0+1 : ? F$;M$;I$
2000 Y=O1-U2 : X=O1+U2 : Z=O1/Y : Y=Z-X : X=O1/O3 : Z=O3/O9 : X=X-Z : T=O9/T7
2010 Z=Z-T : IF (X=O AND Y=O AND Z=O) THEN 2040
2020 J2=J2+1
2030 G2=O : ? "DEFECT:";D$;L$;"so error can exceed 1 ulp" : ? "or  1/3  and  3/9  and  9/27  may disagree."
2040 Y=F9/O1 : X=F9-F2 : Y=(Y-F2)-X : X=O1+U2 : T=X/O1 : X=T-X : IF (X=O AND Y=O) THEN 2070
2050 J1=J1+1 : J2=J2-1+G2
2060 G2=O : ? S$;"DEFECT:";D$;L$;"violating  X/1 = X ."
2070 X=O1/(O1+U2) : Y=X-F2-F2 : IF (Y<O) THEN 2100
2080 J1=J1+1
2090 ? "VERY SERIOUS DEFECT:  Computed value of  1/1.00...001  is not less than  1 ."
2100 X=O1-U2 : Y=O1+B*U2 : Z=X*B : T=Y*B : R=Z/B : S=T/B : X=R-X : Y=S-Y : IF (X=O AND Y=O) THEN 2130
2110 J0=J0+1
2120 ? F$;M$;"  and/or";D$;I$
2130 Y=O1-U1 : X=O1-F9 : Y=O1-Y : T=B-U2 : Z=B-B9 : T=B-T
2140 IF (X=U1 AND Y=U1 AND Z=U2 AND T=U2) THEN 2230
2150 J1=J1+1
2160 G3=O : ? S$;"DEFECT: Subtraction";L$;"so cancellation is obscured."
2170 IF (F9=O1 OR F9-O1<O) THEN 2240
2180 J1=J1+1
2190 ? "VERY SERIOUS DEFECT: comparison alleges  (1-U1) < 1  although"
2200 ? "         subtraction yields  (1-U1) - 1 = 0  , thereby vitiating"
2210 ? "         such precautions against division by zero as"
2220 ? "         ...  if (X=1.0) then ..... else .../(X-1.0)..."
2230 IF(G1*G2*G3=O1) THEN ? "  These operations appear to have guard digits, as they should."
2240 L=40 : GOSUB 110 : ' ---- PAUSE ---- ====================================
2250 ? "Checking for rounding in Multiply (R1), Divide (R2) and Add/Subtract (R3):"
2260 R1=F1 : R2=F1 : R3=F1 : B2=B/O2 : T5=O1+F2 : ' ...  B2 = B/2 ,  T5 = 1.5
2270 A1=O2 : ' Is  B  a power of  2  or  10 ?
2280 A=B
2290 X=A : A=A/A1 : IF (INT(A)=A) THEN 2290
2300 IF (X=O1) THEN 2340 : ' ...  B  is a power of  A1 ;  if B=1 then  A1=2.
2310 IF (A1>O3) THEN 2330
2320 A1 = O9+O1 : GOTO 2280 : ' Is  B  a power of  10 ?
2330 A1 = B : ' ... unless  B  is a power of  A1  and  A1 = 2 or 10 .
2340 A=O1/A1 : X=A1 : Y=A
2350 Z=X*Y-F2 : IF (Z=F2) THEN 2370
2360 J0=J0+1 : ? F$;"  1/";X;" = ";Y;", and  ";X;"*(1/";X;") differs from  1."
2370 IF (X=B) THEN 2390
2380 X=B : Y=O1/X : GOTO 2350
2390 Y2=O1+U2 : Y1=O1-U2 : X=T5-U2 : Y=T5+U2 : Z=(X-U2)*Y2 : T=Y*Y1 : Z=Z-X : T=T-X : X=X*Y2 : Y=(Y+U2)*Y1 : X=X-T5 : Y=Y-T5 : IF NOT ( X=O AND Y=O AND Z=O AND T<=O ) THEN 2460
2400 X=(T5+U2)*Y2 : Y=T5-U2-U2 : Z=T5+U2+U2 : T=(T5-U2)*Y1 : X=X-(Z+U2) : S=Y*Y1 : S1=Z*Y2 : T=T-Y : Y=(U2-Y)+S : Z=S1-(Z+U2+U2) : S=(Y2+U2)*Y1 : Y1=Y2*Y1 : S=S-Y2 : Y1=Y1-F2
2410 IF ( X=O AND Y=O AND Z=O AND T=O AND S=O AND Y1=F2 ) THEN R1=O1
2420 IF ( X+U2=O AND Y<O AND Z+U2=O AND T<O AND S+U2=O AND Y1<F2 ) THEN R1=O
2430 IF (R1=O) THEN ? "  R1=0:";M$;E$;C$;"."
2440 IF (R1=O1) THEN ? "  R1=1:";M$;E$;B$
2450 IF (R1-G1=O1) THEN ? F$;M$;P$
2460 IF (R1=F1) THEN ? M$;H$;C$;" nor ";B$
2470 L=45 : ' ================================================================
2480 Y2=O1+U2 :Y1=O1-U2 : Z=T5+U2+U2 : X=Z/Y2 : T=T5-U2-U2 : Y=(T-U2)/Y1 : Z=(Z+U2)/Y2 : X=X-T5 : Y=Y-T : T=T/Y1 : Z=Z-(T5+U2) : T=(U2-T5)+T : IF ( X>O OR Y>O OR Z>O OR T>O ) THEN 2540
2490 X=T5/Y2 : Y=T5-U2 : Z=T5+U2 : X=X-Y : T=T5/Y1 : Y=Y/Y1 : T=T-(Z+U2) : Y=Y-Z : Z=Z/Y2 : Y1=(Y2+U2)/Y2 : Z=Z-T5 : Y2=Y1-Y2 : Y1=(F9-U1)/F9 : IF ( X=O AND Y=O AND Z=O AND T=O AND Y2=O AND Y1-F2=F9-F2 ) THEN R2=O1
2500 IF ( X<O AND Y<O AND Z<O AND T<O AND Y2<O AND Y1-F2<F9-F2 ) THEN R2=O
2510 IF (R2=O) THEN ? "  R2=0:";D$;E$;C$;"."
2520 IF (R2=O1) THEN ? "  R2=1:";D$;E$;B$
2530 IF (R2-G2=O1) THEN ? F$;D$;P$
2540 IF (R2=F1) THEN ? D$;H$;C$;" nor ";B$
2550 B1=O1/B : IF (B1*B-F2=F2) THEN 2580
2560 J0=J0+1
2570 ? F$;"  B*(1/B)  differs from  1."
2580 L=50 : '=================================================================
2590 IF ( (F9+U1)-F2 = F2  AND  (B9+U2)-O1 = B-O1 ) THEN 2610
2600 J0=J0+1 : ? F$;" Incomplete carry-propagation in Addition."
2610 X=O1-U1*U1 : Y=O1+U2*(O1-U2) : Z=F9-F2 : X=(X-F2)-Z : Y=Y-O1
2620 IF (X><O OR Y><O) THEN 2640
2630 R3=O : ? "  R3=0:";A$;E$;C$;"."
2640 IF (G3=O) THEN 2710
2650 X=(F2+U2)*U2 : Y=(F2-U2)*U2 : X=O1+X : Y=O1+Y : X=(O1+U2)-X : Y=O1-Y
2660 IF (X><O OR Y><O) THEN 2710
2670 X=(F2+U2)*U1 : Y=(F2-U2)*U1 : X=O1-X : Y=O1-Y : X=F9-X : Y=O1-Y
2680 IF (X><O OR Y><O) THEN 2710
2690 R3=F1-O2*R3 : ? "  R3=1:";A$;E$;B$
2700 IF (R3-G3=O1) THEN ? F$;A$;P$
2710 IF R3=F1 THEN ? A$;H$;C$;" nor ";B$
2720 S1=O1 : X=O1+F2*(O1+F2) : Y=(O1+U2)*F2 : Z=X-Y : T=Y-X : S=Z+T
2730 IF (S=0) THEN 2770
2740 S1=O : J3=J3+1
2750 ? "FLAW: Nonzero  (X-Y)+(Y-X) =";S;" when"
2760 ? "      X =";X;"  and  Y =";Y
2770 S=O : IF (G1*G2*G3<O1 OR R1<O1 OR R2<O1 OR R3<O1 OR INT(B2)><B2) THEN 2890
2780 ? : ? "Checking for sticky bit (S)."
2790 X=(F2+U1)*U2 : Y=F2*U2 : Z=O1+Y : T=O1+X : IF (Z-O1>O OR T-O1<U2)THEN 2890
2800 Z=T+Y : Y=Z-X : IF (Z-T<U2 OR Y-T><O) THEN 2890
2810 X=(F2+U1)*U1 : Y=F2*U1 : Z=O1-Y : T=O1-X : IF (Z-O1><O OR T-F9><O) THEN 2890
2820 Z=(F2-U1)*U1 : T=F9-Z : Q=F9-Y : IF (T-F9><O OR (F9-U1)-Q><O) THEN 2890
2830 Z=(O1+U2)*T5 : T=(T5+U2)-Z+U2 : X=O1+F2/B : Y=O1+B*U2 : Z=X*Y
2840 IF (T><O OR (X+B*U2)-Z><O) THEN 2890
2850 IF (B=O2) THEN 2870
2860 X=O2+U2 : Y=X/O2 : IF (Y-O1><O) THEN 2890
2870 S=S1
2880 IF (S=O1) THEN ? " S=1: Sticky bit";E$;"used correctly."
2890 IF (S=O) THEN ? " S=0: Sticky bit used incorrectly or not at all."
2900 IF (G1*G2*G3=O OR R1<O OR R2<O OR R3<O) THEN J3=J3+1
2910 L=60 : '=================================================================
2920 ? : ? "Does Multiplication commute?  Testing if  X*Y = Y*X  for";I0;"random pairs:"
2930 R9=SQR(3) : I=I0+1 : X9=O1/O3 : GOTO 2960
2940 ' ____  Random Number Generator  ____
2950 X=X9+R9 : Y=X*X : Y=Y*Y : X=X*Y : Y=X-INT(X) : X9=Y+X*.000005 : RETURN
2960 GOSUB 2950 : Y9=X9 : GOSUB 2950 : Z=X9*Y9 : Y=Y9*X9 : Z9=Z-Y : I=I-1 : IF (I>0 AND Z9=O) THEN 2960
2970 IF (I>0) THEN 3000
2980 X9=O1+F2/O3 : Y9=(U2+U1)+O1 : Z=X9*Y9 : Y=Y9*X9 : Z9=(O1+F2/O3)*((U2+U1)+O1)-((U2+U1)+O1)*(O1+F2/O3) : IF NOT (Z9=O) THEN 3000
2990 ? " **********  No failure found in ";I0;" randomly chosen pairs.  **********" : GOTO 3010
3000 J2=J2+1 : ? "DEFECT:   X*Y = Y*X  violated at  X = ";X9;", Y = ";Y9 : ? " X*Y =";Z;",  Y*X =";Y;",  X*Y-Y*X =";Z9 : ? "   ... pair no.";I0-I+1
3010 L=70 : '=================================================================
3020  ? : ? "Running tests of Square Root  SQR(X) :" : X=O : I=0
3030 Y=SQR(X) : IF (Y=X AND Y-F2=X-F2) THEN 3050
3040 J0=J0+1 : ? F$;V$;X;"= SQR(";X;"), miscalculated as";Y
3050 X=-X : I=I+1 : IF (I=1) THEN 3030
3060 X=O1 : I=I+1 : IF (I=3) THEN 3030
3070 E5=O : E7=O : GOTO 3150 : ' ... record min and max errors.
3080 ' ____ Subroutine to assess error  SQRT(X*X)-X  in Ulps. ____
3090 E6=((SQR(X*X)-X*B1)-(X-X*B1))/U : IF (E6=O) THEN RETURN
3100 IF (E6<E5) THEN E5=E6
3110 IF (E6>E7) THEN E7=E6
3120  ? : J=J+1 : ? Z$;"DEFECT :  SQR(";X*X;") - ";X;" = ";U*E6 : ? "         instead of correct value  0 ." : RETURN
3130 ' ---- End of Sqrt(X*X)-X subroutine. ----
3140 ' Test whether SQRT(X*X) = X ...
3150 J=0 : Z$=S$ : X=B : U=U2 : GOSUB 3090
3160 X=B1 : U=B1*U1 : GOSUB 3090
3170 X=W : U=O1 : GOSUB 3090
3180 X=U1 : U=U1*U1 : GOSUB 3090
3190 IF (J=0) THEN 3210
3200 J1=J1+J : GOSUB 110 : ' If SQRT has SERIOUS DEFECTS, then PAUSE ----
3210 ? "Testing if  SQR(X*X) = X  for  ";I0;" integers  X";
3220 J=0 : Z$="" : X=O2 : Y=B : IF (B=O1) THEN 3240
3230 X=Y : Y=B*X : IF (Y-X<I0) THEN 3230
3240 U=X*U2 : FOR I=1 TO I0
3250 X=X+O1 : GOSUB 3090 : IF (J>0) THEN 3280
3260 NEXT I
3270 ? "  found no discrepancies." : GOTO 3290
3280 J2=J2+J
3290  ' Test SQRT for monotonicity.
3300 I=-1 : X=B9 : Y=B : Z=B+B*U2
3310 I=I+1 : X=SQR(X) : Q=SQR(Y) : Z=SQR(Z) : IF NOT (X>Q OR Q>Z) THEN 3330
3320 J2=J2+1 : ? "DEFECT:  SQR(X) is non-monotonic for  X  near ";Y : GOTO 3390
3330 Q=INT(Q+F2) : IF NOT (I>0 OR Q*Q=B) THEN 3380
3340 IF (I>0) THEN 3360
3350 Y=Q : X=Y-U2 : Z=Y+U2 : GOTO 3310
3360 IF (I>1) THEN 3380
3370 Y=Y*B1 : X=Y-U1 : Z=Y+U1 : GOTO 3310
3380 ? "SQR has passed a test for Monotonicity."
3390 L=80 : ' Test SQRT for accuracy ...=====================================
3400 E5=E5+F2 : E7=E7-F2 : ' e5=min{error+1/2}, e7=max{error-1/2}
3410 Y=(SQR(O1+U2)-O1)/U2 : E6=(Y-O1)+U2/O8 : IF (E6>E7) THEN E7=E6
3420 E6=Y+U2/O8 : IF (E6<E5) THEN E5=E6
3430 Y=((SQR(F9)-U2)-(O1-U2))/U1 : E6=Y+U1/O8 : IF (E6>E7) THEN E7=E6
3440 E6=(Y+O1)+U1/O8 : IF (E6<E5) THEN E5=E6
3450 I=0 : U=U2 : X=U
3460 I=I+1 : Y=SQR((X+U1+X)+F9) : Y=((Y-U2)-((O1-U2)+X))/U : Z=((U1-X)+F9)*F2*X*X/U : E6=(Y+F2)+Z : IF (E6<E5) THEN E5=E6
3470  E6=(Y-F2)+Z : IF (E6>E7) THEN E7=E6
3480 ON I GOTO 3500, 3520, 3500, 3530 : ' Case statement
3490 ' Cases  I = 1 and 3
3500 X=U*SGN(X)*INT( O8/(O9*SQR(U)) ) : GOTO 3460
3510 ' Case  I = 2
3520 U=U1 : X=-U : GOTO 3460
3530 L=85 : ' Case  I = 4 exits  ..... =====================================
3540 IF (B=O1) THEN 3900
3550 ?"Testing whether  SQR  is rounded or chopped:"
3560 D=INT(F2+B^(O1+P-INT(P))) : ' ... = B^(1+fract)  if  P = integer + fract.
3570 X=D/B : Y=D/A1 : IF NOT (X=INT(X) AND Y=INT(Y)) THEN 3700
3580 X=O : Z2=X : Y=O1 : Y2=Y : Z1=B-O1 : D4=O4*D
3590 'Loop: for  Y = 1, 3, 5, ...  maximize  Y2 = Y*Y mod 4D .
3600 IF NOT (Y2>Z2) THEN 3650
3610 Q=B : Y1=Y : ' ... if new Y2 > old, check that  GCD(Y,B) = 1
3620 X1=ABS(Q+INT(F2-Q/Y1)*Y1) : Q=Y1 : Y1=X1 : IF (X1>O) THEN 3620
3630 IF (Q>O1) THEN 3650 : ' If GCD(Y,B) > 1 then skip over Y ;  else ...
3640 Z2=Y2 : Z=Y : ' ... and GCD(Z, B) =1
3650 Y=Y+O2 : X=X+O8 : Y2=Y2+X : IF NOT (Y2<D4) THEN Y2=Y2-D4 : '...= Y*Y mod 4D
3660 IF (Y<D) THEN 3600 : ' else  0 < Z < D  &  Z2 = Z^2 mod 4D  is maximal.
3670 X8=D4-Z2 : Q=(X8+Z*Z)/D4 : X8=X8/O8 : IF NOT (Q=INT(Q)) THEN 3700
3680 X=Z1*Z : X=X-INT(X/B)*B : IF (X=O1) THEN 3800 : ' with  1 = Z*Z1 mod B
3690 Z1=Z1-O1 : IF (Z1>O) THEN 3680 : ' else Failure!
3700 J0=J0+1 : ? F$;" Anomalous arithmetic with integers < B^P =";W : ? "         foils test whether  SQR  rounds or chops." : GOTO 3940
3710 ' This subroutine puts  NewD = B*D  and  NewZ^2 mod NewD  =  Z^2 mod D
3720 X=Z1*Q : X=INT(F2-X/B)*B+X : Q=(Q-X*Z)/B+X*X*(D/B) : Z=Z-O2*X*D : IF (Z>O) THEN 3740
3730 Z=-Z : Z1=-Z1
3740 D=B*D : RETURN : ' ___ end of NewD subroutine.___
3750 'This Subroutine tests if SQRT(D*X)=SQRT((Y-1/2)^2+X8/2) rounds to  Y .__
3760 IF (X-B<Z2-B OR X-Z2>W-Z2) THEN RETURN
3770 I=I+1 : X2=SQR(X*D) : Y2=(X2-Z2)-(Y-Z2) : X2=X8/(Y-F2) : X2=X2-F2*X2*X2 : E6=(Y2+F2)+(F2-X2) : IF (E6<E5) THEN E5=E6
3780 E6=Y2-X2 : IF (E6>E7) THEN E7=E6
3790 RETURN : ' ____ End of subroutine to test  SQRT(D*X) = Y . ____
3800 IF (Z1>B2) THEN Z1=Z1-B : '  -B/2 <= Z1 = 1/Z mod B <= B/2
3810 GOSUB 3720 : IF (U2*D<F9) THEN 3810 : ' ... until  D = B^(P-1) .
3820 IF NOT (D*B-D=W-D) THEN 3700
3830 Z2=D : I=0 : ' Count how many tests of SQRT(D*X)=Y yield results.
3840 Y=D+(O1+Z)*F2 : X=D+Z+Q : GOSUB 3750
3850 Y=D+(O1-Z)*F2+D : X=D-Z+D : X=X+Q+X : GOSUB 3750
3860 GOSUB 3720 : IF NOT (D-Z2=W-Z2) THEN 3700
3870 Y=(D-Z2)+(Z2+(O1-Z)*F2) : X=(D-Z2)+(Z2-Z+Q) : GOSUB 3750
3880 Y=(O1+Z)*F2 : X=Q : GOSUB 3750
3890 IF (I=0) THEN 3700
3900 IF (E5<0 OR E7>0) THEN 3920
3910 R4=O1 : ? Q$;E$;B$ : GOTO 3960
3920 IF (E7+U2>U2-F2 OR E5>F2 OR E5+B<F2) THEN 3940
3930 R4=O : ? Q$;E$;C$;"." : GOTO 3960
3940 ? Q$;H$;C$;" nor ";B$ : ? "Observed errors run from  ";E5-F2;"  to  ";F2+E7;" ulps." : IF (E7-E5<B*B) THEN 3960
3950 J1=J1+1 : ? S$;"DEFECT:  SQR";I$
3960 L=90 : GOSUB 110 : ' ---- PAUSE ---- ===================================
3970 ? "Testing powers  Z^i  for small integers  Z  and  i :" : GOTO 4120
3980 ' ___  Subroutine to compare  Z^I  with  X = Z*Z*...*Z  ( I times )  ___
3990 Y=Z^I : Q=I : GOSUB 4040 : ' ... test whether  Y=X
4000 I=I+1 : IF (I>M) THEN RETURN : ' ... with  X = Z^M
4010 X=Z*X : IF (X<W) THEN 3990
4020 RETURN : ' ___ End of comparison  subroutine ___
4030 Y=Z^Q : ' ___ Subroutine to test if  Y = X  ___
4040 IF (Y=X) THEN 4080
4050 IF (N>0) THEN 4070
4060 J2=J2+1 : ? "DEFECT: Computed  (";Z;")^(";Q;") =";Y : ? "   compares Unequal to correct ";X;" ; they differ by  ";Y-X
4070 N=N+1 : ' ... counts discrepancies.
4080 RETURN : ' ___ End of test subroutine. ___
4090 ' ___ Subroutine to print count  N  of discrepancies. ___
4100 IF (N>0) THEN ? "Similar discrepancies have occurred ";N;" times."
4110 RETURN : ' ___ End of sub. ___
4120 N=0 : I=0 : Z=-O : M=3 : ' ... test powers of zero
4130 X=O1 : GOSUB 3980 : IF (I>10) THEN 4150
4140 I=1023 : GOSUB 3980
4150 IF (Z=F1) THEN 4170 : ' ... If (-1)^n is invalid, replace  F1  by  O1  ...
4160 Z=F1 : I=-4 : GOTO 4130
4170 GOSUB 4090 : ' ... print  N  if  N>0.
4180 N1=N : N=0 : Z=A1 : M=INT( O2*LOG(W)/LOG(A1) )
4190 X=Z : I=1 : GOSUB 3980 : IF (Z=A) THEN 4210
4200 Z=A : GOTO 4190
4210 L=100 : ' Powers of radix  B  have been tested; next try a few primes: ==
4220 M=I0 : Z=O3
4230 X=Z : I=1 : GOSUB 3980
4240 Z=Z+O2 : IF (O3*INT(Z/O3) = Z) THEN 4240
4250 IF (Z<O8*O3) THEN 4230
4260 IF (N>0) THEN ? "Error like this may invalidate financial calculations involving interest rates."
4270 N=N+N1 : GOSUB 4280 : GOTO 4330
4280 GOSUB 4090 : ' ... print  N  if  N>0.
4290 IF (N=0) THEN ? "  ... no discrepancies found."
4300 ?
4310 IF (N>0) THEN  GOSUB 110 : ' ---- PAUSE ----
4320 RETURN : ' ___ End of printing sub. ___
4330 L=110 : ? "Seeking Underflow thresholds  U0  and  E0 :" : '==========
4340 D=U1 : IF(P=INT(P)) THEN 4370
4350 D=B1 : X=P
4360 D=D*B1 : X=X-O1 : IF (X>O) THEN 4360
4370 Y=O1 : Z=D : ' ... D = a power of  1/B < 1
4380 C=Y : Y=Z : Z=Y*Y : IF (Y>Z AND Z+Z>Z) THEN 4380
4390 Y=C : Z=Y*D
4400 C=Y : Y=Z : Z=Y*D : IF (Y>Z AND Z+Z>Z) THEN 4400
4410 H1=B : IF (H1<O2) THEN H1=O2
4420 H=O1/H1 : '  ... 1/H1 = H = min{ 1/B, 1/2 }
4430 C1=O1/C :E0=C : Z=E0*H : ' ... C = 1/B^(big integer) << 1 << C1 = 1/C
4440 Y=E0 : E0=Z : Z=E0*H : IF (E0>Z AND Z+Z>Z) THEN 4440
4450 U0=E0 : E1=O : Q=O : E9=U2 :S1=O1+E9 : D=C*S1 : IF (D>C) THEN 4490
4460 E9=B*U2 : S1=O1+E9 : D=C*S1 : IF (D>C) THEN 4490
4470 ? F$;M$;I$ : ' ... Multiplication is too crude
4480 J0=J0+1 : T0=E0 : Y1=O : Z0=Z : GOSUB 110 : GOTO 4570
4490 T0=D : Z0=T0*H : U0=O
4500 Y1=T0 : T0=Z0 : IF (E1+E1>E1) THEN 4520
4510 Y2=T0*H1 : E1=ABS(Y1-Y2) : Q=Y1 : IF (U0=O AND Y1><Y2) THEN U0=Y1
4520 Z0=T0*H : IF (T0>Z0 AND Z0+Z0>Z0) THEN 4500
4530 ' Now  1 >> C=1/B^(integer)  >=   Y    >   E0=Y*H   >~ Z:=E0*H >~ 0 ,
4540 ' and  1 >> D=(1+E9)*C >= U0 >= Q >= Y1 > T0:=Y1*H >~ Z0:=T0*H >~ 0 ,
4550 ' and  U0 = D/B^integer  is first to violate  (U0*H)/H=U0 , else  U0=0 ;
4560 ' and  Q:=U0/B^integer  is first with  E1 := |(Q*H)/H - Q| > 0, else Q=Y1.
4570 IF (Z0=O) THEN 4860
4580 ' ... Test  Z0  for "phoney-zero" violating  Z0<T0 or Z0<Z0+Z0  ...
4590 ? : Z=Z0 : Z$="Z0" : IF (Z0>O) THEN 4620
4600 J0=J0+1 : ? "FAILURE:  Positive expressions can underflow to an allegedly negative value" : ? "          Z0  that prints out as "; Z0 : X=-Z0 : IF (X>0) THEN 4630
4610 ? "          But  -Z0 , which should then be positive, isn't; it prints out as", X : GOTO 4630
4620 J3=J3+1 : ? "FLAW:  Underflow can stick at an allegedly positive value  Z0" : ? "       that prints out as "; Z0
4630 GOSUB 4640 : GOTO 4860 : ' ... end of test for "phoney-zero".
4640 '___ Subroutine to test  Z & Z$  for Partial Underflow ___
4650 N=0 : IF (Z=0) THEN RETURN
4660 ? "Since Comparison denies  ";Z$;" = 0 , evaluating  (";Z$;"+";Z$;")/";Z$;"  should be safe;" : ? "what the machine gets for  (";Z$;"+";Z$;")/";Z$;"  is   "; : Q9=(Z+Z)/Z : ? Q9 : IF (ABS(Q9-O2)<B*U2) THEN 4750
4670 IF (Q9<O1 OR Q9>O2) THEN 4740
4680 N=1 : J2=J2+1 : ? "This is a DEFECT." : ? : GOTO 4760
4690 ' ___ Subroutine to test  Z & Z$ for Partial Overflow ___
4700 N=0 : ? "Since Comparison alleges ";Z$;" =";Z;" is finite" : ? "and nonzero, the next two expressions should not over/underflow:" : V9=Z*F2
4710 ? "The machine computes  (0.5*";Z$;")/";Z$;" ="; : Q9=V9/Z : ? Q9
4720 ? "The machine computes  ";Z$;"/(0.5*";Z$;") ="; : V9=Z/V9 : ? V9
4730 IF (ABS(Q9-F2)<U2 AND ABS(V9-O2)<B*U2) THEN 4750
4740 N=1 : J1=J1+1 : ? "This is a VERY SERIOUS DEFECT." : ? : GOTO 4760
4750 ? "  This is O.K. provided Over/Underflow has NOT just been signaled."
4760 V9=Z*O1 : X9=V9 : V9=O1*Z : Y9=V9 : V9=Z/O1 : IF (Z=X9 AND Z=Y9 AND Z=V9) THEN 4840
4770 N=1 : J2=J2+1
4780  ? "DEFECT:  What prints as  ";Z$;" =";Z;" compares different from"
4790 IF NOT (Z=X9) THEN ? "           ";Z$;"*1 = ";X9
4800 IF NOT (Z=Y9 OR Y9=X9) THEN ? "           1*";Z$;" = ";Y9
4810 IF NOT (Z=V9) THEN ? "           ";Z$;"/1 = ";V9
4820 IF (Y9=X9) THEN 4840
4830 J2=J2+1 : ? "DEFECT:";M$;" does not commute; comparison alleges that" : ? "         1*";Z$;" =";Y9;"  differs from  ";Z$;"*1 =";X9
4840 IF (N>0) THEN GOSUB 110 : ' ---- PAUSE ----
4850 RETURN : ' end of test for Partial Over/Underflow
4860 L=120 : ' ==============================================================
4870 IF (C1*Y<=C1*Y1) THEN 4890 : ' ... as happens on most machines.
4880 S1=H*S1 : E0=T0 : ' = least positive no. on HP 3000
4890 IF (E1=0 OR E1=E0) THEN 4930
4900 IF (E1<E0) THEN 4920
4910 J2=J2+1 : ? "DEFECT: Differences underflow at a higher threshold than Products." : GOTO 4930
4920 J2=J2+1 : ? "DEFECT: Products underflow at a higher threshold than Differences." : IF (Z0=O) THEN E0=E1 : ' ... but not if pseudo-zeros exist.
4930 ? "Smallest strictly positive number found is  E0 ="; E0
4940 Z=E0 : Z$="E0" : GOSUB 4640 : T0=E0 : IF (N=1) THEN T0=Y : ' for CDC 7600
4950 I=4 : IF (E1=O) THEN I=3 : ' ...  I=1 if E1=0=U0  ,   I=2 if E1>0=U0  ,
4960 IF (U0=O) THEN I=I-2 : '     ...  I=3 if E1=0<U0  ,   I=4 if E1>0 & U0>0
4970 ON I GOTO 4980, 5090, 5010, 5130 : ' ... case statement
4980 U0=T0 : IF (C1*Q = (C1*Y)*S1) THEN 5010
4990 J0=J0+1 : U0=Y : ? F$;"Either accuracy deteriorates as numbers approach a threshold" : ? "U0 =";U0;" coming down from  ";C;"," : ? "or else ";M$;I$ : GOSUB 110
5000 ' ___ Test for  X-Z = 0  although  X >< Z ___
5010 ? : R=SQR(T0/U0) : IF (R>H) THEN 5030
5020 Z=R*U0 : X=Z*(O1+R*H*(O1+H)) : GOTO 5040
5030 Z=U0 : X=Z*(O1+H*H*(O1+H))
5040 IF (X=Z OR X-Z><O) THEN 5080
5050 J3=J3+1 : ? "FLAW:  X =";X;" is Unequal to  Z =";Z;" ," : ? "      yet  X-Z  yields "; : Z9=X-Z : ? Z9;".  Should this NOT signal Underflow,"
5060 ? "this is a SERIOUS DEFECT that causes confusion when innocent statements like" : ? "        if (X=Z) then  ...  else  ... ( f(X)-f(Z) )/(X-Z) ..."
5070 ? "encounter Division by Zero although actually  X/Z = 1 + "; (X/Z-F2)-F2 : ?
5080 GOTO 5160 : ' ... end of test for  X-Z = 0  &  X >< Z
5090 ' Case I=2 : U0 = 0 < E1  !
5100 J0=J0+1 : ? F$;" Underflow confuses Comparison, which alleges that  Q = Y " : ? "         while denying that  |Q-Y| = 0 ; these values print out as" : ? "Q =";Q;",  Y =";Y2;",  |Q-Y| =";ABS(Q-Y2);" ,"
5110 ? "and  Q/Y = 1 + "; (Q/Y2-F2)-F2
5120 U0=Q : GOTO 5010
5130 ' Case I=4 ;  U0 > 0  &  E1 > 0
5140 IF NOT (Q=U0 AND E1=E0 AND ABS(U0-E1/E9)<=E1) THEN 5010
5150 ? "Underflow is Gradual; it incurs  Absolute error = (roundoff in U0) < E0." : Y=E0*C1 : Y=Y*(T5+U2) : X=C1*(O1+U2) : Y=Y/X : I3=0 : IF (Y=E0) THEN I3=1 : ' ... I3=1 unless gradual underflows are doubly rounded.
5160 ? "The  Underflow threshold  U0 is ";U0;" , below which" : ? "calculation may suffer larger  Relative error  than merely roundoff."
5170 Y2=U1*U1 : Y=Y2*Y2 : Y2=Y*U1 : IF (Y2>U0) THEN 5220
5180 IF (Y>E0) THEN 5200
5190 J1=J1+1 : I=4 : ? S$; : GOTO 5210
5200 J2=J2+1 : I=5
5210 ? "DEFECT:  Range is too narrow;   U1^";I;"  underflows."
5220 L=130 : GOSUB 110 : ' ---- PAUSE ---- ==================================
5230 Y=-INT(F2-T8*LOG(U0)/LOG(H1))/T8 : Y2=Y+Y
5240 ? "Since Underflow occurs below the threshold  U0 = (";H1;")^(";Y;") ," : ? "only underflow should afflict the expression     (";H1;")^(";Y2;") ;" : ? "actually calculating it yields   ";
5250 V9=H1^(Y2) : ? V9 : IF (V9>=O AND V9<=(B+B*E9)*U0) THEN 5270
5260 J1=J1+1 : ? S$; : GOTO 5300
5270 IF (V9>U0*(O1+E9)) THEN 5290
5280 ? "  This computed value is  O.K." : GOTO 5310
5290 J2=J2+1
5300 ? " DEFECT: this is not between 0 and  U0 =";U0
5310 L=140 : ? : ' ======================================================
5320 'Calculate  E2 = exp(2) = 7.389056099...
5330 X=O : I=2 : Y=O2*O3 : Q=O : N=0
5340 Z=X : I=I+1 : Y=Y/(I+I) : R=Y+Q : X=Z+R : Q=(Z-X)+R : IF (X>Z) THEN 5340
5350 Z=(T5+O1/O8)+X/(T5*T2) : X=Z*Z : E2=X*X : X=F9 : Y=X-U1
5360 ? "Testing  X^((X+1)/(X-1))  vs.  exp(2) = ";E2;"  as  X -> 1."
5370 FOR I=1 TO I0
5380 Z=X-B1 : Z=(X+O1)/(Z-(O1-B1)) : Q=X^Z-E2 : IF (ABS(Q)>T8*U2) THEN 5420
5390 Z=(Y-X)*O2+Y : X=Y : Y=Z : IF (O1+(X-F9)*(X-F9)>O1) THEN NEXT I
5400 IF (X>O1) THEN 5440
5410 X=O1+U2 : Y=U2+U2+X : GOTO 5370
5420 N=1 : J2=J2+1 : ? "DEFECT:  Calculated  (1 + (";(X-B1)-(O1-B1);"))^(";Z;")" : ? "         differs from correct value by  ";Q
5430 ? "This much error may spoil financial calculations involving tiny interest rates." : GOTO 5450
5440 IF (N=0) THEN ? "Accuracy seems adequate."
5450 L=150 : ? : ' =======================================================
5460 ? "Testing powers  Z^Q  at four nearly extreme values:" : N=0 : Z=A1 : Q=INT(F2-LOG(C)/LOG(A1))
5470 X=C1 : GOSUB 4030 : Q=-Q : X=C : GOSUB 4030 : IF (Z<O1) THEN 5490
5480 Z=A : GOTO 5470
5490 GOSUB 4280 : ' ... print count of discrepancies.
5500 L=160 : GOSUB 110 : ' ---- PAUSE ---- ===================================
5510 ? "Searching for Overflow threshold:"
5520 Y=-C1 : V9=H1*Y
5530 V=Y : Y=V9 : V9=H1*Y : IF (V9<Y) THEN 5530
5540 Z=V9 : ? "Can ` Z = -Y ' overflow?  Trying it on  Y =";Y; : V9=-Y : V0=V9 : IF (V-Y=V+V0) THEN 5560
5550 J3=J3+1 : ? " finds a" : ? "FLAW:  -(-Y) differs from Y" : GOTO 5570
5560 ? " seems O.K."
5570 IF (Z=Y) THEN 5590
5580 J1=J1+1 : ? S$;"DEFECT: Overflow past ";Y;"  shrinks to ";Z
5590  Y=V*(H1*U2-H1) : Z=Y+((O1-H1)*U2)*V : IF (Z<V0) THEN Y=Z
5600 IF (Y<V0) THEN V=Y
5610 IF(V0-V<V0) THEN V=V0
5620 ? "Overflow threshold is   V = ";V
5630 ? "Overflow saturates at  V0 = ";V0
5640 ? "No overflow should be signaled for  V*1 = "; : V9=V*O1 : ? V9 : ? "                           nor for  V/1 = "; : V9=V/O1 : ? V9 : ? "Any overflow signal separating this  *  from one above or below is a DEFECT."
5650 L=170 : ' ===============================================================
5660 IF (-V<V AND -V0<V0 AND -U0<V AND U0<V) THEN 5680
5670 J0=J0+1 : ? F$;" Comparisons are confused by Overflow."
5680 L=175 : '================================================================
5690 ? : I=0 : Z=U0
5700 I=I+1 : IF (Z=O) THEN 5770
5710 V9=SQR(Z) : Y=V9*V9 : IF NOT (Y/(O1-B*E9)<Z OR Y>(O1+B*E9)*Z) THEN 5770
5720 IF (V9>U1) THEN 5750
5730 Z$="" : J2=1+J2 : GOTO 5760
5740 ? Z$;"DEFECT:  Comparison alleges that what prints as  Z = ";Z : ? "                 is too far from  SQR(Z)^2 = ";Y : RETURN
5750 Z$=S$ : J1=1+J1
5760 GOSUB 5740
5770 ON I GOTO 5780, 5790, 5800
5780 Z=E0 : GOTO 5700
5790 Z=Z0 : GOTO 5700
5800 I=0 : Z=V
5810 L=180 : '=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
5820 IF (B><2 OR P><56 OR Z0=0 OR -O=O) THEN 5850
5830 J0=1+J0 : ? F$;" Attempts to evaluate  SQR(Overflow threshold V)  in Double Precision" : ? "in  BASIC  on the  IBM PC  display the word  ` Overflow '  and then" : ? "disable the Keyboard !  This is disastrous." : GOTO 5920
5840 '************************************************************************
5850 V9=SQR(Z) : X=(O1-B*E9)*V9 : V9=V9*X : IF NOT (V9<(O1-O2*B*E9)*Z OR V9>Z) THEN 5900
5860 Y=V9 : IF (X<W) THEN 5880
5870 Z$="" : J2=1+J2 : GOTO 5890
5880 Z$=S$ : J1=1+J1
5890 I=1 : GOSUB 5740
5900 IF (I=1) THEN 5920
5910 I=1 : Z=V0 : GOTO 5850
5920 ? " *   *   *   *   *   *   *   *   *   *"
5930 L=190 : GOSUB 110 : '---- PAUSE ---- ====================================
5940 X=U0*V : Y=B*B : IF (X*Y>= O1 AND X<=Y) THEN 5990
5950 IF (X*Y>=U1 AND X<=Y/U1)THEN 5970
5960 J2=J2+1 : ? "DEFECT: Badly "; : GOTO 5980
5970 J3=J3+1 : ? "FLAW: ";
5980 ? "unbalanced range;  U0*V =";X;" is too far from  1."
5990 L=200 : ' Test  X/X  vs.  1. ============================================
6000 FOR I=1 TO 5
6010 X=F9 : IF (I=2) THEN X=O1+U2
6020 IF (I=3) THEN X=V
6030 IF (I=4) THEN X=U0
6040 IF (I=5) THEN X=B
6050 Y=X : V9=(Y/X-F2)-F2 : IF (V9=0) THEN 6100
6060 X$="   X/X  differs from  1  when  X =" : IF (V9=-U1 AND I<5) THEN 6080
6070 J0=J0+1 : ? F$;X$;X : GOTO 6090
6080 J1=J1+1 : ? "SERIOUS DEFECT:  ";X$;X
6090 ? "          Instead,  X/X - 1/2 - 1/2 = ";V9 : ?
6100 NEXT I
6110 L=210 : ' ===============================================================
6120 ? : ? "What messages and/or values does Division by Zero produce?" : ? "    Trying to compute  1/0  produces ...  "; : Q9=O1/O : ? Q9 : ? "    Trying to compute  0/0  produces ...  "; : Q9=O/O : ? Q9
6130 L=220 : GOSUB 110 : ' ---- PAUSE ---- ===================================
6140 N$="The number of  " : T$=" The arithmetic diagnosed " : ?
6150 IF (J0>0) THEN ? N$;"FAILURES  encountered =          ";J0
6160 IF (J1>0) THEN ? N$;S$;"DEFECTS  discovered =    ";J1
6170 IF (J2>0) THEN ? N$;"DEFECTS  discovered =            ";J2
6180 IF (J3>0) THEN ? N$;"FLAWS  discovered =              ";J3
6190 IF (J0+J1+J2+J3>0) THEN 6270
6200 ? "No failures, defects nor flaws have been discovered."
6210 IF (R1+R2+R3+R4<O4) THEN 6260
6220 IF (S<O1 OR (B-O2)*(B-O9-O1)><O) THEN 6250
6230 Z$="854" : IF (B=O2 AND (P-O4*O3*O2)*(P-T7-T7+O1)=O) THEN Z$="754"
6240 ? "Rounding appears to conform to the proposed IEEE standard  p";Z$ : IF (I3=0) THEN ? "except possibly for Double Rounding during Gradual Underflow."
6250 ? T$;"appears to be Excellent! : go to 4550
6260 ? T$;"seems  Satisfactory." : GOTO 6310
6270 IF (J0+J1+J2=0 AND J3>0) THEN ? T$;"seems Satisfactory though flawed."
6280 IF (J0+J1=0 AND J2>0) THEN ? T$;"may be Acceptable despite inconvenient Defects."
6290 IF (J0+J1>0) THEN ? T$;"has unacceptable  Serious  Defects."
6300 IF (J0>0) THEN ? "Potentially fatal FAILURE may have spoiled this program's subsequent diagnoses."
6310 ? : ? "End of Test."
6320 END
6330 ' Glossary of Variables
6340 ' ~~~~~~~~~~~~~~~~~~~~~
6350 ' A1=first of {2, 10, B} for which  B = A1^(integer) ,  A=1/A1
6360 ' B=radix, B1=1/B, B2=B/2, B8=Q8orV8, B9=B-U2
6370 ' C=1/B^(large integer), C1=1/C
6380 ' D=C*(1+E9) or 1/B^(integer) or B^(P-integer), D4=4D
6390 ' E0=min positive, E1=..., E2=exp(2), E3=..., E5-1/2=minSQRerror,
6400 ' E6=SQRerror, E7+1/2=maxSQRerror, E8=V a priori, E9=?*U2
6410 ' F1=-1, F2=1/2, F3.=1/3, F9=1-U1
6420 ' G1=?guard*, G2=?guard/, G3=?guard-
6430 ' H=min{ 1/B, 1/2 }, H1=1/H
6440 ' I=scratch integer, I0=number of random trials X*Y=Y*X, I3=IEEE
6450 ' J=scratch, J0=#FAILUREs, J1=#SERIOUS DEFECTs, J2=#DEFECTs, J3=#FLAWs
6460 ' K=page no. of diagnosis
6470 ' L=Milestone passed in program
6480 ' M=upper bound for  i  in tests of  Z^i
6490 ' N=0 unless Partial Over/Underflow is uncovered, N1=...
6500 ' O=0, O1=1, O2=2, O3=3, O4=4, O8=8, O9=9
6510 ' P=precision.=#Bdigits; if B=1 then P=0
6520 ' Q=scratch, Q8=#(Div. by 0), Q9=potential Divide by Zero
6530 ' R=scratch, R1=?round*, R2=?round/, R3=?round+-, R9=sqrt(3)
6540 ' S=?sticky bit, S1=...
6550 ' T=scratch, T0=underflow, T2=32, T5=1.5, T7=27, T8=240
6560 ' U=1 ulp, U0=underflow threshold, U1=1-Next(1, 0), U2=Next(1, 2)-1
6570 ' V.=overflow threshold, V0=1/0, V8=#(Overflow), V9=potential Overflow
6580 ' W=1/U1=B^P
6590 ' X=scratch, X1=..., X8=((-Z*Z)mod4D)/8, X9=random
6600 ' Y=scratch, Y1=..., Y2=..., Y9=random
6610 ' Z=scratch, Z0=pseudozero, Z1=..., Z2=Z*Zmod4D, Z9=scratch
6620 ' A$="  Add/Subtract", B$="correctly rounded.", C$="chopped"
6630 ' D$="  Division", F$="FAILURE:", I$="  gets too many last digits wrong.",
6640 ' E$=" appears to be ", H$=" is neither ", L$=" lacks a Guard Digit, "
6650 ' M$="  Multiplication", N$="The number of  ", V$=" Violation of "
6660 ' P$=" test is inconsistent;  PLEASE  NOTIFY  THE  AUTHOR !"
6670 ' Q$="Square Root"
6680 ' S$="SERIOUS ", T$=" The arithmetic diagnosed ", X$=scratch, Z$=scratch
6690 '------------------------------------------------------------------
