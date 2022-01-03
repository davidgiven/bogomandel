
Z%=&A8
M%=-1
cr=0
ci=0
vr=0
vi=0
scroll%=0
scale=4
maxiter%=32

PROChelp

REPEAT
step=scale/4
PROCrender
PROCcursor
UNTIL FALSE
END

DEFPROCcursor
REPEAT
*FX 19
REPEAT
PROCdrawcursor
*FX 19
PROCdrawcursor
G%=INKEY(0)
UNTIL G%<>-1
*FX 21,0
scroll%=0
IF G%=129 AND vr>-2 THEN vr=vr-step:scroll%=2: UNTIL TRUE: ENDPROC
IF G%=130 AND vr<+2 THEN vr=vr+step:scroll%=4: UNTIL TRUE: ENDPROC
IF G%=131 AND vi<+2 THEN vi=vi+step:scroll%=6: UNTIL TRUE: ENDPROC
IF G%=132 AND vi>-2 THEN vi=vi-step:scroll%=8: UNTIL TRUE: ENDPROC
IF G%=140 THEN cr=cr-step/5: PROCbanner: IF NOT M% UNTIL TRUE: ENDPROC
IF G%=141 THEN cr=cr+step/5: PROCbanner: IF NOT M% UNTIL TRUE: ENDPROC
IF G%=142 THEN ci=ci+step/5: PROCbanner: IF NOT M% UNTIL TRUE: ENDPROC
IF G%=143 THEN ci=ci-step/ 5: PROCbanner: IF NOT M% UNTIL TRUE: ENDPROC
IF G%=43 AND scale>0.125 THEN scale=scale/2: UNTIL TRUE: ENDPROC
IF G%=45 AND scale<4 THEN scale=scale*2: UNTIL TRUE: ENDPROC
IF G%=60 AND maxiter%>1 THEN maxiter%=(maxiter%+1)/2: UNTIL TRUE: ENDPROC
IF G%=62 AND maxiter%<255 THEN maxiter%=maxiter%*2: UNTIL TRUE: ENDPROC
IF G%=32 THEN M%=NOT M%: UNTIL TRUE: ENDPROC
IF G%=13 THEN UNTIL TRUE: ENDPROC
IF G%=63 THEN PROChelp: UNTIL TRUE: ENDPROC
UNTIL FALSE
ENDPROC

DEFPROCbanner
PRINT TAB(0,0);
IF M% THEN COLOUR 1:PRINT " MANDEL" ELSE COLOUR 2:PRINT " JULIA"

@%=&20205
COLOUR 6: PRINT ''"  View"
COLOUR 3: PRINT ;;"r=";: COLOUR 7: PRINT vr
COLOUR 3: PRINT "i=";: COLOUR 7: PRINT vi
COLOUR 3: PRINT "s=";: COLOUR 7: PRINT scale
COLOUR 6: PRINT ''" Cursor"
COLOUR 3: PRINT ;;"r=";: COLOUR 7: PRINT cr
COLOUR 3: PRINT "i=";: COLOUR 7: PRINT ci
@%=&0000A
COLOUR 3: PRINT '"max ";: COLOUR 7: PRINT ;maxiter%
COLOUR 3: PRINT "  iters"

COLOUR 6: PRINT TAB(1,30);"?";: COLOUR 3: PRINT " help"
ENDPROC

DEFPROCrender
VDU 28, 32, 31, 39, 0
CLS

IF vr<-2 THEN vr=-2: scroll%=0
IF vr>+2 THEN vr=+2: scroll%=0
IF vi<-2 THEN vi=-2: scroll%=0
IF vi>+2 THEN vi=+2: scroll%=0
IF maxiter%>255 THEN maxiter%=255

PROCbanner

TIME=0
Z%!0=FNfixed(vr)
Z%!2=FNfixed(vi)
Z%?4=scale*16
Z%?5=NOT M%
Z%!6=FNfixed(cr)
Z%!8=FNfixed(ci)
Z%?12=scroll%
Z%?13=maxiter%
CALL &2000
t=(Z%!10 AND &FFFF)/100
PRINT TAB(0,18);
@%=5
COLOUR 7: PRINT INT((128*256)/t): COLOUR 3: PRINT "   px/s"
@%=&20105
COLOUR 7: PRINT 't: COLOUR 3: PRINT "   secs"
ENDPROC

DEFFNfixed(r)
=(r*&1000) AND &FFFE EOR &8000

DEFPROCdrawcursor
IF NOT M% THEN ENDPROC
X%=512 + (cr-vr)*1024/scale
Y%=512 - (ci-vi)*1024/scale
GCOL 4, 7
MOVE X%-48, Y%:DRAW X%+48, Y%
MOVE X%, Y%-48:DRAW X%, Y%+48
ENDPROC

DEFPROChelp
VDU 26:CLS
COLOUR 6: PROCcenter("B O G O M A N D E L", 1)
COLOUR 4: PROCcenter("Mandelbrot and Julia set explorer", 3)
COLOUR 4: PROCcenter("(C) 2018 David Given and Andrew Jenner", 5)
COLOUR 6: PROCcenter("http://cowlark.com/2018-05-26-bogomandel", 6)
COLOUR 3
PRINTTAB(5, 9);"Controls:"
COLOUR 7
PRINT'TAB(7);"Cursor keys"
PRINT'TAB(7);"+ and -"
PRINT'TAB(7);"Shift + cursor keys"
PRINT'TAB(7);"Space"
PRINT'TAB(7);"< and >"
PRINT'TAB(7);"Return"
COLOUR 3
PRINTTAB(9, 12);"Pan image"
PRINT'TAB(9);"Zooms in and out"
PRINT'TAB(9);"Moves the cursor"
PRINT'TAB(9);"Flips Julia and Mandelbrot mode"
PRINTTAB(9);"Changes iteration limit"
PRINT'TAB(9);"Rerenders the current image"
COLOUR 1
PROCcenter("You don't have to wait for a render to", 24)
PROCcenter("complete before pressing a key!", 25)
COLOUR 5
PROCcenter("Try moving the cursor while in", 27)
PROCcenter("Julia mode!", 28)

COLOUR 3: PROCcenter("Press any key to continue", 30)
IFGET
ENDPROC

DEFPROCcenter(S$,Y%)
PRINTTAB(20-LEN(S$)/2,Y%);S$;
ENDPROC
