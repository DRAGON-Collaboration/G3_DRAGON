CDECK  ID>, GDOPT.  
*CMZ :  3.21/02 29/03/94  15.41.26  by  S.Giani
*-- Author :
      SUBROUTINE GDOPT(IOPTC,IVALC)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Set options for the drawing package                      *
C.    *                                                                *
C.    *       IOPTN = option (input)                                   *
C.    *       IVALN = value (input)                                    *
C.    *                                                                *
C.    *    ==>Called by : <USER>, <GXINT>                              *
C.    *       Author : P.Zanarini  J.Salt  S.Giani                     *
C.    *                                                                *
C.    ******************************************************************
C.
      COMMON/GCDRAW/NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB(16),
     + GRMAT0(10),GTRAN0(3),IDRNUM,GSIN(41),GCOS(41),SINPSI,COSPSI,
     + GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV,NGVIEW,
     + ICUTFL,ICUT,CTHETA,CPHI,DCUT,NSURF,ISURF,
     + GZUA,GZVA,GZUB,GZVB,GZUC,GZVC,PLTRNX,PLTRNY,
     + LINATT,LINATP,ITXATT,ITHRZ,IPRJ,DPERS,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN,
     + ZZFU,ZZFV,MYISEL,
     + DDUMMY(15)
C
      INTEGER NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB,IDRNUM,NGVIEW,ICUTFL,ICUT,NSURF,ISURF,LINATT,LINATP,
     + ITXATT,ITHRZ,IPRJ,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN
      REAL GRMAT0,GTRAN0,GSIN,GCOS,SINPSI,COSPSI,GTHETA,GPHI,GPSI,
     + GU0,GV0,GSCU,GSCV,CTHETA,CPHI,DCUT,GZUA,GZVA,GZUB,GZVB,GZUC,
     + GZVC,PLTRNX,PLTRNY,DPERS,DDUMMY
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
*
      REAL S1,S2,S3,SS1,SS2,SS3,SRAGMX,SRAGMN,
     +     RAINT1,RAINT2,RMIN1,RMIN2,RMAX1,RMAX2
      INTEGER ISCOP,NTIM,NTFLAG,IOLDCU,ITSTCU,ISUBLI,IPORLI
      INTEGER LPASS,JPORJJ,LEP,JSC
*
      COMMON/GCSPEE/S1,S2,S3,SS1,SS2,SS3,LEP,IPORLI,ISUBLI,
     +              SRAGMX,SRAGMN,RAINT1,RAINT2,RMIN1,RMIN2,
     +              RMAX1,RMAX2,JPORJJ,ITSTCU,IOLDCU,ISCOP,
     +              NTIM,NTFLAG,LPASS,JSC
*
      COMMON/GCRAYT/INTEN,IOMBRA,IXYFLA,NOFLAG,
     +XLPOS,YLPOS,ZLPOS,XLDIR,YLDIR,ZLDIR,APFLAH,
     +CCXX(4),CCYY(4),CCZZ(4),BOFLAG,APFLAG,
     +XCOSXS,YCOSYS,ZCOSZS,VDX,VDY,VDZ,SSLENG,
     +XPINTS,YPINTS,ZPINTS,FPINTX,FPINTY,FPINTZ,
     +AROTS(4,4),ZROTS(4,4),RRR(4)
C
      INTEGER INTEN,IOMBRA,IXYFLA,NOFLAG
      REAL XLPOS,YLPOS,ZLPOS,XLDIR,YLDIR,ZLDIR,APFLAH,
     +     CCXX,CCYY,CCZZ,BOFLAG,APFLAG,
     +     XCOSXS,YCOSYS,ZCOSZS,VDX,VDY,VDZ,SSLENG,
     +     XPINTS,YPINTS,ZPINTS,FPINTX,FPINTY,FPINTZ,
     +     AROTS,ZROTS,RRR
C
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
      COMMON/GCPIXE/LIMPRE,IFLAPE,ICOLOR,IXXX,IYYY,
     +ISSEEN,ISCOLO,ISLSTY,ISLWID,ISFILL,
     +IMAP,JON,NMAP,UUU,VVV,ZUV,ZNMAP1
C
      INTEGER LIMPRE,IFLAPE,ICOLOR,IXXX,IYYY,
     +        ISSEEN,ISCOLO,ISLSTY,ISLWID,ISFILL,
     +        IMAP,JON,NMAP
      REAL UUU,VVV,ZUV,ZNMAP1

C
      CHARACTER*4 IOPTC,IVALC
C.
C.    ------------------------------------------------------------------
C.
      CALL UCTOH(IVALC,IVAL,4,4)
      IF (IOPTC.EQ.'THRZ') THEN
         IF (IVALC.EQ.'ON  ') THEN
            ITHRZ=IVAL
         ELSE IF (IVALC.EQ.'OFF ') THEN
            ITHRZ=IVAL
         ELSE IF (IVALC.EQ.'180 ') THEN
            ITHRZ=IVAL
         ELSE IF (IVALC.EQ.'360 ') THEN
            ITHRZ=IVAL
         ELSE
            WRITE (CHMAIL,10200) IOPTC,ITHRZ
            CALL GMAIL(0,0)
         ENDIF
      ELSE IF (IOPTC.EQ.'PROJ') THEN
         IF (IVALC.EQ.'PARA') THEN
            IPRJ=IVAL
         ELSE IF (IVALC.EQ.'PERS') THEN
            IPRJ=IVAL
         ELSE
            WRITE (CHMAIL,10200) IOPTC,IPRJ
            CALL GMAIL(0,0)
         ENDIF
      ELSE IF (IOPTC.EQ.'TRAK') THEN
         IF (IVALC.EQ.'LINE') THEN
            ITRKOP=IVAL
         ELSE IF (IVALC.EQ.'POIN') THEN
            ITRKOP=IVAL
         ELSE
            WRITE (CHMAIL,10200) IOPTC,ITRKOP
            CALL GMAIL(0,0)
         ENDIF
*JS
      ELSE IF (IOPTC.EQ.'HIDE') THEN
         IF (IVALC.EQ.'ON  ') THEN
            IHIDEN=IVAL
         ELSE IF (IVALC.EQ.'OFF ') THEN
            IHIDEN=IVAL
         ELSE
            WRITE (CHMAIL,10200) IOPTC,IHIDEN
            CALL GMAIL(0,0)
         ENDIF
*JS
**SG
      ELSE IF (IOPTC.EQ.'SHAD') THEN
         IF (IVALC.EQ.'ON  ') THEN
            LEP=10
         ELSE IF (IVALC.EQ.'OFF ') THEN
            LEP=1
         ENDIF

      ELSE IF (IOPTC.EQ.'EDGE') THEN
         IF (IVALC.EQ.'OFF ') THEN
            LLEP=ABS(LEP)
            IF(LLEP.EQ.10)LEP=11
         ELSE IF (IVALC.EQ.'ON  ') THEN
            LLEP=ABS(LEP)
            IF(LLEP.EQ.11)LEP=10
            IF(LLEP.EQ.1)LEP=1
         ENDIF

      ELSE IF (IOPTC.EQ.'RAYT') THEN
         IF (IVALC.EQ.'ON  ') THEN
             RAYTRA=1.
             IOMBRA=0
             RAYTRA=0.
         ELSE IF (IVALC.EQ.'OFF ') THEN
             RAYTRA=0.
         ENDIF

      ELSE IF (IOPTC.EQ.'MAPP') THEN
         IF (IVALC.EQ.'0   ') THEN
             NMAP=0
         ELSE IF (IVALC.EQ.'1   ') THEN
             NMAP=1
         ELSE IF (IVALC.EQ.'2   ') THEN
             NMAP=2
         ELSE IF (IVALC.EQ.'3   ') THEN
             NMAP=3
         ELSE IF (IVALC.EQ.'4   ') THEN
             NMAP=4
         ENDIF

      ELSE IF (IOPTC.EQ.'USER') THEN
         IF (IVALC.EQ.'ON  ') THEN
             IMYSE=1
             CALL GSATT('*   ','SEEN',-10)
             CALL GSATT('*   ','COLO',-10)
             CALL GSATT('*   ','LSTY',-10)
         ELSE IF (IVALC.EQ.'OFF ') THEN
             IMYSE=0
             CALL GSATT('*   ','SEEN',1)
             CALL GSATT('*   ','COLO',1)
             CALL GSATT('*   ','LSTY',1)
         ENDIF
**SG
      ELSE
         WRITE (CHMAIL,10000) IOPTC
         CALL GMAIL(0,0)
         WRITE (CHMAIL,10100)
         CALL GMAIL(0,0)
      ENDIF
C
10000 FORMAT(' GDOPT: ',A4,' is not a valid option')
*JS
10100 FORMAT('    Valid options : THRZ , PROJ , TRAK , HIDE , SHAD')
*JS
10200 FORMAT(' GDOPT: Option ',A4,' is ',A4)
*SG
10300 FORMAT(' GDOPT: Option ',A4,' is ',A4)
10400 FORMAT(' Please, compute and set the right size of Zebra',
     +       ' store, needed for your drawing, with LHC option',
     +       ' OFF. If it is done, good luck !!!')
*SG
      END
CDECK  ID>, GMEDI2. 
*CMZ :  3.21/02 29/03/94  15.41.31  by  S.Giani
*-- Author :
      SUBROUTINE GMEDIA (X, NUMED)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Finds in which volume/medium the point X is, and updates the *
C.    *    common /GCVOLU/ and the structure JGPAR accordingly.        *
C.    *                                                                *
C.    *   NUMED returns the tracking medium number, or 0 if point is   *
C.    *         outside the experimental setup.                        *
C.    *                                                                *
C.    *   Called by :  GTREVE, GLTRAC, 'User'                          *
C.    *   Authors   : R.Brun, F.Bruyant, A.McPherson                   *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
C.
      DIMENSION  X(*)
      REAL       XC(3)
      LOGICAL    BTEST
C.
C.    ------------------------------------------------------------------
*
      IF (NLEVEL.EQ.0) CALL GMEDIN
*
* SECTION I: The /GCVOLU/ table contains the initial guess for a path
*            in the geometry tree on which X may be found.  Look along this
*            path until X is found inside.  This is the starting position.
*            If this is an ONLY volume with no daughters, we are done;
*            otherwise reset search record variables, proceed to section II.
*
*            The information contained in INFROM has to be invalidated
*            because it has no meaning for the subsequent tracking. INFR
*            is a local variable used to optimise the search in the
*            geometry tree.
*
      INFROM = 0
*
* *** Check if point is in current volume
*
      INFR   = 0
      JVIN   = 0
C*****  Code Expanded From Routine:  GTRNSF
C
  100 IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
         XC(1) = X(1) - GTRAN(1,NLEVEL)
         XC(2) = X(2) - GTRAN(2,NLEVEL)
         XC(3) = X(3) - GTRAN(3,NLEVEL)
*
      ELSE
         XL1 = X(1) - GTRAN(1,NLEVEL)
         XL2 = X(2) - GTRAN(2,NLEVEL)
         XL3 = X(3) - GTRAN(3,NLEVEL)
         XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) + XL3*GRMAT(3
     +      ,NLEVEL)
         XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) + XL3*GRMAT(6
     +      ,NLEVEL)
         XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) + XL3*GRMAT(9
     +      ,NLEVEL)

      ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
      JVO  = LQ(JVOLUM-LVOLUM(NLEVEL))
      JPAR = LQ(JGPAR-NLEVEL)
      CALL GINME (XC, Q(JVO+2), Q(JPAR+1), IYES)
      IF (IYES.EQ.0) THEN
*
*  **   Point not in current volume, go up the tree
*
         IF (NLEVEL.GT.1) THEN
            NLEVEL = NLEVEL -1
            JVO  = LQ(JVOLUM-LVOLUM(NLEVEL))
            NIN = Q(JVO+3)
            IF(NIN.GT.0) THEN
*
*       Do not set INFR whne going up the tree. GMEDIA can be called
*       by the user and it  should not assume  that the previous
*       position has something to do with the current search. INFR
*       is otherwise useful when searching in a 'MANY' volume
*       configuration. This statement is commented for the above reason.
*
*              INFR  =LINDEX(NLEVEL+1)
            ELSE
               INFR  =0
            ENDIF
            GO TO 100
         ELSE
*
*   *      Point is outside setup
*
            NUMED = 0
            GO TO 999
         ENDIF
      ENDIF
*
*  **   Point is in current volume
*
      IF(INFR  .GT.0) THEN
         JIN=LQ(JVO-INFR  )
         IQ(JIN) = IBSET(IQ(JIN),4)
         JVIN = JIN
      ENDIF
      NLMIN = NLEVEL
      NLMANY = 0
*
* SECTION II: X is found inside current node at NLEVEL in /GCVOLU/.
*             Search all contents recursively for any containing X.
*             Take the first one found, if any, and continue at that
*             level, incrementing NLEVEL and extending /GCVOLU/ tables.
*             This is continued until a level is reached where X is not
*             found in any of the contents, or there are no contents.
* Note: Since Section II is re-entered from Section III, a blocking word
* is used to mark those contents already checked.  Upon exit from Section
* II, these blocking words are cleared at NLEVEL, but may remain set in
* levels between NLEVEL-1 and NLMIN, if any.  They must be cleared at exit.
*
*  **  Check contents, if any
*
  200 JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
      NIN = Q(JVO+3)
*
*   *   Case with no contents
*
      IF (NIN.EQ.0) THEN
         GO TO 300
*
*   *   Case with contents defined by division
*
      ELSEIF (NIN.LT.0) THEN
         CALL GMEDIV (JVO, IN, XC, 1)
         IF (IN.GT.0) THEN
            INFR   = 0
            GO TO 200
         ENDIF
*
*   *  Case with contents positioned
*
      ELSE
         JCONT  = LQ(JVO-NIN-1)+1
         NCONT  = IQ(JCONT)
         ISEARC = Q(JVO+1)
         IF (ISEARC.LT.0) THEN
*
*       Prepare access to contents, when ordered by GSORD
*
            JSB = LQ(LQ(JVO-NIN-1))
            IAX = Q(JSB+1)
            NSB = Q(JSB+2)
            IF (IAX.LE.3) THEN
               IDIV = LOCATF (Q(JSB+3), NSB, XC(IAX))
            ELSE
               CALL GFCOOR (XC, IAX, CX)
               IDIV = LOCATF (Q(JSB+3), NSB, CX)
            ENDIF
            IF (IDIV.LT.0) IDIV = -IDIV
            IF (IDIV.EQ.0) THEN
               IF (IAX.NE.6) GO TO 260
               IDIV = NSB
            ELSEIF (IDIV.EQ.NSB) THEN
               IF (IAX.NE.6) GO TO 260
            ENDIF
            JSC0  = LQ(JVO-NIN-2)
            NCONT = IQ(JSC0+IDIV)
            JCONT = LQ(JSC0-IDIV)
         ELSE
*
*       otherwise, scan contents (possibly a user selection of them)
*
            JNEAR = LQ(JVO-NIN-1)
            IF (ISEARC.GT.0) THEN
               CALL GUNEAR (ISEARC, 1, XC, JNEAR)
            ELSEIF (INFR  .GT.0) THEN
               JNUP = LQ(LQ(JVO-INFR  )-1)
               IF (JNUP.GT.0) THEN
                  JNEAR = JNUP
               ENDIF
            ENDIF
            JCONT = JNEAR +1
            NCONT = IQ(JCONT)
         ENDIF
*
*     For each selected content in turn, check if point is inside
*
         DO 259 ICONT=1,NCONT
            IN = IQ(JCONT+ICONT)
            IF(IN.EQ.0) THEN
*
*     If the value IQ(JCONT+ICONT)=0 then we are back in the mother.
*     So jump to 260, the search is finished. Clean-up should be done
*     only up to ICONT-1, so we set:
*
               NCONT=ICONT-1
               GOTO 260
            ELSE
            JIN = LQ(JVO-IN)
            IF (.NOT.BTEST(IQ(JIN),4)) THEN
               CALL GMEPOS (JVO, IN, XC, 1)
               IF (IN.GT.0) THEN
                  IF (GONLY(NLEVEL).NE.0.) NLMANY = 0
                  INFR   = 0
                  GO TO 200
               ELSE
                  IQ(JIN) = IBSET(IQ(JIN),4)
               ENDIF
            ENDIF
            ENDIF
  259    CONTINUE
*
  260    IF(NCONT.EQ.NIN) THEN
         DO 268 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  268    CONTINUE
         ELSE
         DO 269 ICONT=1,NCONT
            IN  = IQ(JCONT+ICONT)
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  269    CONTINUE
         IF(INFR  .GT.0) THEN
            JIN = LQ(JVO-INFR  )
            IQ(JIN) = IBCLR(IQ(JIN),4)
         ENDIF
         ENDIF
*
      ENDIF
*
* SECTION III: X is found at current node (NLEVEL in /GCVOLU) but not in
*              any of its contents, if any.  If this is a MANY volume,
*              save it as a candidate best-choice, and continue the search
*              by backing up the tree one node and proceed to Section II.
*              If this is an ONLY volume, proceed to Section IV.
*
* *** Point is in current volume/medium, and not in any content
*
  300 IF (GONLY(NLEVEL).EQ.0.) THEN
*
*  **   Lowest level is 'NOT ONLY'
*
         IF (NLEVEL.GT.NLMANY) THEN
            CALL GSCVOL
            NLMANY = NLEVEL
         ENDIF
*
*   *   Go up the tree up to a volume with positioned contents
*
  310    INFR   = LINDEX(NLEVEL)
         NLEVEL = NLEVEL -1
         JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
         NIN    = Q(JVO+3)
         IF (NIN.LT.0) GO TO 310
*
C*****  Code Expanded From Routine:  GTRNSF
C
         IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
            XC(1) = X(1) - GTRAN(1,NLEVEL)
            XC(2) = X(2) - GTRAN(2,NLEVEL)
            XC(3) = X(3) - GTRAN(3,NLEVEL)
*
         ELSE
            XL1 = X(1) - GTRAN(1,NLEVEL)
            XL2 = X(2) - GTRAN(2,NLEVEL)
            XL3 = X(3) - GTRAN(3,NLEVEL)
            XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) + XL3*
     +      GRMAT(3,NLEVEL)
            XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) + XL3*
     +      GRMAT(6,NLEVEL)
            XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) + XL3*
     +      GRMAT(9,NLEVEL)

         ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
         JIN = LQ(JVO-INFR  )
         IQ(JIN) = IBSET(IQ(JIN),4)
         NLMIN = MIN(NLEVEL,NLMIN)
         GO TO 200
      ENDIF
*
* SECTION IV: This is the end of the search.  The current node (NLEVEL
*             in /GCVOLU/) is the lowest ONLY volume in which X is found.
*             If X was also found in any of its contents, they are MANY
*             volumes: the best-choice is the one among them at the greatest
*             level in the tree, and it is stored.  Otherwise the current
*             volume is the solution.  Before exit, all of the blocking
*             words leftover in the tree must be reset to zero.
* Note: A valid structure is assumed, in which no ONLY volumes overlap.
* If this rule is violated, or if a daughter is not entirely contained
* within the mother volume, the results are unpredictable.
*
      DO 419 NL=NLMIN,NLEVEL-1
         JVO = LQ(JVOLUM-LVOLUM(NL))
         NIN = Q(JVO+3)
         DO 418 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  418    CONTINUE
  419 CONTINUE
*
      IF (NLMANY.GT.0) CALL GFCVOL
      JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
      IF(JVIN.NE.0) IQ(JVIN) = IBCLR(IQ(JVIN),4)
      NUMED = Q(JVO+4)
*                                                             END GMEDIA
  999 IF(JGSTAT.NE.0) CALL GFSTAT(2)
      END
CDECK  ID>, GNEX2.  
*CMZ :  3.21/02 29/03/94  15.41.31  by  S.Giani
*-- Author :
      SUBROUTINE GNEXT (X, SNEXT, SAFETY)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   SUBR. GNEXT (X, SNEXT, SAFETY)                               *
C.    *                                                                *
C.    *   Computes SNEXT and SAFETY                                    *
C.    *     SNEXT  (output) : distance to closest boundary             *
C.    *                      from point X(1-3) along X(4-6)            *
C.    *     SAFETY (output) : shortest distance to any boundary        *
C.    *                                                                *
C.    *   Called by : User                                             *
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant                    *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      PARAMETER ( NSBOX=1,  NSTRD1=2, NSTRD2=3, NSTRAP=4, NSTUBE=5,
     +  NSTUBS=6, NSCONE=7, NSCONS=8, NSSPHE=9, NSPARA=10,NSPGON=11,
     +  NSPCON=12,NSELTU=13,NSHYPE=14,NSGTRA=28, NSCTUB=29 )
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      REAL    X(6), X0(3), XC(6), XT(6)
      INTEGER IDTYP(3,12)
      LOGICAL BTEST
      SAVE IDTYP
C.
      DATA  IDTYP / 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 2, 3, 1,
     +              2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 4, 3, 1, 1, 1,
     +              2, 3, 1, 2, 3, 1/
C.
C.    ------------------------------------------------------------------
*
* *** Transform current point and direction into local reference system
*
      IF (GRMAT(10,NLEVEL).EQ.0.) THEN
         DO 19 I = 1,3
            XC(I)   = X(I) -GTRAN(I,NLEVEL)
            XC(I+3) = X(I+3)
   19    CONTINUE
      ELSE
*       (later, code in line)
         CALL GTRNSF (X, GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), XC)
         CALL GROT (X(4), GRMAT(1,NLEVEL), XC(4))
      ENDIF
*
* *** Compute distance to boundaries
*
      SNEXT  = BIG
      SAFETY = BIG
      JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
      ISH    = Q(JVO+2)
      IF (Q(JVO+3).EQ.0.) GO TO 300
      NIN = Q(JVO+3)
      IF (NIN.LT.0) GO TO 200
*
* *** Case with contents positioned
*
      ISEARC = Q(JVO+1)
      IF (ISEARC.GE.-1) GO TO 120
*
*  ** Contents are ordered by (dynamic) GSORD, select neighbours
*
      JSB = LQ(LQ(JVO-NIN-1))
      IAX = Q(JSB+1)
      NSB = Q(JSB+2)
      IF (IAX.LE.3) THEN
         CX  = XC(IAX)
         INC = SIGN(1., XC(IAX+3))
      ELSE
         CALL GFCOOR (XC, IAX, CX)
         IF (IAX.LE.5) THEN
            DR = XC(1)*XC(4) +XC(2)*XC(5)
            IF (IAX.EQ.5) DR = DR +XC(3)*XC(6)
            INC = SIGN(1., DR)
         ELSE IF (IAX.EQ.6) THEN
            INC = SIGN(1., XC(1)*XC(5)-XC(2)*XC(4))
         ELSE
            INC = SIGN(1., XC(3)*(XC(1)*XC(4)+XC(2)*XC(5))
     +                    -XC(6)*(XC(1)*XC(1)+XC(2)*XC(2)))
         ENDIF
      ENDIF
      IDIV = LOCATF (Q(JSB+3), NSB, CX)
      IF (IDIV.LT.0) IDIV = -IDIV
      IF (IAX.NE.6) THEN
         IF (IDIV.EQ.0) THEN
            IF (INC.LT.0.AND.IAX.LE.3) THEN
               SAFETY = Q(JSB+3) -CX
               GO TO 300
            ENDIF
            IDIV = 1
         ELSE IF (IDIV.EQ.NSB) THEN
            IF (INC.GT.0.AND.IAX.NE.7) THEN
               SAFETY = CX -Q(JSB+2+NSB)
               GO TO 300
            ENDIF
            IDIV = NSB -1
         ELSE
            IF (IAX.NE.7) THEN
               IF (INC.GT.0) THEN
                  SAFETY = CX -Q(JSB+2+IDIV)
               ELSE
                  SAFETY = Q(JSB+3+IDIV) -CX
               ENDIF
            ELSE
               SAFETY = 0.
            ENDIF
         ENDIF
      ELSE IF (IAX.EQ.6) THEN
         IF (IDIV.EQ.0) IDIV = NSB
         SAFETY = 0.
      ENDIF
*
      IDIVL = 0
      IDIVB = 0
      JSC0  = LQ(JVO-NIN-2)
  110 NCONT = IQ(JSC0+IDIV)
*
*  ** Loop over (selected) contents
*
      IF (NCONT.EQ.0) THEN
         IF (IDIV.EQ.IDIVL) GO TO 400
         IDIV = IDIV +INC
*      (following statement for IAX=6, when division NSB is empty)
         IF (IDIV.GT.NSB) IDIV = 1
         GO TO 110
      ELSE
         ICONT = 1
         JSCV = LQ(JSC0-IDIV)
         GO TO 140
      ENDIF
*
  120 JNEAR = LQ(JVO-NIN-1)
      IF (ISEARC.GT.0) THEN
         CALL GUNEAR (ISEARC, 2, XC, JNEAR)
         IF (IQ(JNEAR+1).EQ.0) GO TO 300
      ENDIF
      JNEAR = JNEAR +1
      NNEAR = IQ(JNEAR)
      IF (IQ(JNEAR+1).NE.0) THEN
         INEAR = 1
      ELSE
         INEAR = 2
      ENDIF
*
  130 IN = IQ(JNEAR+INEAR)
      GO TO 150
*
  140 IN = IQ(JSCV+ICONT)
*
  150 IF(IN.LE.0)GO TO 300
      JIN   = LQ(JVO-IN)
      IVOT  = Q(JIN+2)
      JVOT  = LQ(JVOLUM-IVOT)
      IROTT = Q(JIN+4)
*
      IF (NLEVEL.GE.NLDEV(NLEVEL)) THEN
*       (case with JVOLUM structure locally developed)
         JPAR = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))
         DO 169 ILEV = NLDEV(NLEVEL), NLEVEL
            IF (IQ(JPAR+1).EQ.0) THEN
               IF (ILEV.EQ.NLEVEL) THEN
                  JPAR = LQ(JPAR-IN)
               ELSE
                  JPAR = LQ(JPAR-LINDEX(ILEV+1))
               ENDIF
               IF (JPAR.EQ.0) GO TO 170
            ELSE IF (IQ(JPAR-3).GT.1) THEN
               JPAR = LQ(JPAR-LINDEX(ILEV+1))
            ELSE
               JPAR = LQ(JPAR-1)
            ENDIF
  169    CONTINUE
         JPAR = JPAR + 5
         GO TO 180
      ENDIF
*     (normal case)
  170 NPAR = Q(JVOT+5)
      IF (NPAR.EQ.0) THEN
         JPAR = JIN +9
      ELSE
         JPAR = JVOT +6
      ENDIF
*
*   * Compute distance to boundary of current content
*
  180 IF (IROTT.EQ.0) THEN
         DO 189 I = 1,3
            XT(I)   = XC(I) -Q(JIN+4+I)
            XT(I+3) = XC(I+3)
  189    CONTINUE
      ELSE
*       (later, code in line)
         CALL GITRAN (XC, Q(JIN+5), IROTT, XT)
         CALL GRMTD (XC(4), IROTT, XT(4))
      ENDIF
*
      IACT = 2
      ISHT = Q(JVOT+2)
      IF (ISHT.LT.5) THEN
         IF (ISHT.EQ.1) THEN
            CALL GNOBOX (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.2) THEN
            CALL GNOTRA(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.3) THEN
            CALL GNOTRA(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE
            CALL GNOTRP (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ENDIF
      ELSE IF (ISHT.LE.10) THEN
         IF (ISHT.EQ.5) THEN
            CALL GNOTUB(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.6) THEN
            CALL GNOTUB(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.7) THEN
            CALL GNOCON(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.8) THEN
            CALL GNOCON(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.9) THEN
            CALL GNOSPH (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ELSE
            CALL GNOPAR (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ENDIF
      ELSE IF (ISHT.EQ.11) THEN
         CALL GNOPGO (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.12) THEN
         CALL GNOPCO (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.13) THEN
         CALL GNOELT (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.14) THEN
         CALL GNOHYP (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.28) THEN
         CALL GSNGTR (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE,0)
      ELSE IF (ISHT.EQ.NSCTUB) THEN
         CALL GNOCTU (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE
         PRINT *, ' GNEXT : No code for shape ', ISHT
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LT.SNEXT) THEN
         SNEXT = SNXT
         IF (ISEARC.EQ.-2) THEN
            IF (MOD(IQ(JSC0),2).NE.0) THEN
               IDIVB = IDIV
            ELSE
               DO 191 I = 1,3
                  X0(I) = XC(I) + SNXT*XC(I+3)
  191          CONTINUE
               IF (IAX.LE.3) THEN
                  IDIVB = LOCATF (Q(JSB+3), NSB, X0(IAX))
               ELSE
                  CALL GFCOOR (X0, IAX, CX)
                  IDIVB = LOCATF (Q(JSB+3), NSB, CX)
               ENDIF
               IF (IDIVB.LT.0) IDIVB = -IDIVB
               IF (IDIVB.EQ.0) THEN
                  IF (IAX.EQ.6) THEN
                     IDIVB = NSB
                  ELSE
                     IDIVB = 1
                  ENDIF
               ELSE IF (IDIVB.EQ.NSB) THEN
                  IF (IAX.NE.6) IDIVB = NSB - 1
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      IF (ISEARC.EQ.-2) THEN
         IF (ICONT.EQ.NCONT) THEN
            IF (IDIVL.EQ.0) THEN
               IF (IDIVB.NE.0) THEN
                  IF (IDIV.EQ.IDIVB) GO TO 300
                  IF (.NOT.BTEST(IQ(JVO),2)) THEN
                     IDIVL = IDIVB
                     GO TO 193
                  ENDIF
               ENDIF
*
*   *         Compute distance to boundary of current volume
*
               JPAR = LQ(JGPAR-NLEVEL)
               IACT = 2
               ISH  = Q(JVO+2)
               IF (ISH.LT.5) THEN
                  IF (ISH.EQ.1) THEN
                     CALL GNBOX (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
                  ELSE IF (ISH.EQ.2) THEN
                     CALL GNTRAP (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.3) THEN
                     CALL GNTRAP (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE
                     CALL GNTRP (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
                  ENDIF
               ELSE IF (ISH.LE.10) THEN
                  IF (ISH.EQ.5) THEN
                     CALL GNTUBE (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.6) THEN
                     CALL GNTUBE (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.7) THEN
                     CALL GNCONE (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.8) THEN
                     CALL GNCONE (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.9) THEN
                     CALL GNSPHR (XC, Q(JPAR+1),IACT, SNEXT, SNXT, SAFE)
                  ELSE
                     CALL GNPARA (XC, Q(JPAR+1),IACT, SNEXT, SNXT, SAFE)
                  ENDIF
               ELSE IF (ISH.EQ.12) THEN
                  CALL GNPCON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.11) THEN
                  CALL GNPGON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.13) THEN
                  CALL GNELTU (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.14) THEN
                  CALL GNHYPE (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.28) THEN
                  CALL GSNGTR (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE,1)
               ELSE IF (ISH.EQ.NSCTUB) THEN
                  CALL GNCTUB (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE
                  PRINT *, ' GNEXT : No code for shape ', ISH
                  STOP
               ENDIF
*
               IF (SAFE.LT.SAFETY) SAFETY = SAFE
               IF (SNXT.LT.SNEXT)  SNEXT  = SNXT
*
*   *         Check wether other pseudo-divisions have to be scanned
*
               DO 192 I = 1,3
                  X0(I) = XC(I) +SNXT*XC(I+3)
  192          CONTINUE
               IF (IAX.LE.3) THEN
                  IDIVL = LOCATF (Q(JSB+3), NSB, X0(IAX))
               ELSE
                  CALL GFCOOR (X0, IAX, CX)
                  IDIVL = LOCATF (Q(JSB+3), NSB, CX)
               ENDIF
               IF (IDIVL.LT.0) IDIVL = -IDIVL
               IF (IDIVL.EQ.0) THEN
                  IF(IAX.EQ.6)THEN
                     IDIVL=NSB
                  ELSE
                     IDIVL=1
                  ENDIF
               ELSEIF (IDIVL.EQ.NSB)THEN
                  IF(IAX.NE.6)IDIVL=NSB-1
               ENDIF
            ELSE
               IF (IDIV.EQ.IDIVB)   GO TO 400
            ENDIF
  193       IF ((IDIV-IDIVL)*INC.GE.0) GO TO 400
            IDIV = IDIV +INC
            GO TO 110
         ELSE
            ICONT = ICONT +1
            GO TO 140
         ENDIF
      ELSE
         IF (INEAR.EQ.NNEAR) GO TO 300
         INEAR = INEAR +1
         GO TO 130
      ENDIF
*
* ***    Case of volume incompletely divided
*
  200 JDIV  = LQ(JVO-1)
      IAXIS = Q(JDIV+1)
      IVOT  = Q(JDIV+2)
      JVOT  = LQ(JVOLUM-IVOT)
      ISHT  = Q(JVOT+2)
*
*  ** Get the division parameters
*
      IF (NLEVEL.LT.NLDEV(NLEVEL)) THEN
         JPARM = 0
      ELSE
*        (case with JVOLUM structure locally developed)
         JPARM = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))
         IF (NLEVEL.EQ.NLDEV(NLEVEL)) GO TO 215
         DO 210 ILEV = NLDEV(NLEVEL), NLEVEL-1
            IF (IQ(JPARM+1).EQ.0) THEN
               JPARM = LQ(JPARM-LINDEX(ILEV+1))
               IF (JPARM.EQ.0) GO TO 215
            ELSE IF (IQ(JPARM-3).GT.1) THEN
               JPARM = LQ(JPARM-LINDEX(ILEV+1))
            ELSE
               JPARM = LQ(JPARM-1)
            ENDIF
            IF (ILEV.EQ.NLEVEL-1) THEN
               NDIV = IQ(JPARM+1)
               ORIG =  Q(JPARM+2)
               SDIV =  Q(JPARM+3)
            ENDIF
  210    CONTINUE
         GO TO 220
      ENDIF
*     (normal case)
  215 NDIV = Q(JDIV+3)
      ORIG = Q(JDIV+4)
      SDIV = Q(JDIV+5)
*
*  ** Look at the first and the last divisions only
*
  220 IDT  = IDTYP(IAXIS, ISH)
      IF (IDT.EQ.1) THEN
         IN2 = 0
         IF (XC(IAXIS).LT.ORIG) THEN
            IN  = 1
         ELSE
            IN  = NDIV
         ENDIF
      ELSE IF (IDT.EQ.2) THEN
         R   = XC(1)**2 + XC(2)**2
         IF (ISH.EQ.9) R = R + XC(3)**2
         R   = SQRT(R)
         IN2 = 0
         IF (ISH.EQ.5.OR.ISH.EQ.6.OR.ISH.EQ.9) THEN
            IF (R.LT.ORIG) THEN
               IN  = 1
            ELSE
               IN  = NDIV
            ENDIF
         ELSE
            PRINT *, ' GNEXT : Partially divided ',ISH,IAXIS
            IN  = 1
            IF (NDIV.GT.1) IN2 = NDIV
         ENDIF
      ELSE IF (IDT.EQ.4) THEN
         IN2 = 0
         RXY = XC(1)**2 + XC(2)**2
         RXY = SQRT(RXY)
         IF (XC(3).NE.0.0) THEN
            THET = RADDEG * ATAN (RXY/XC(3))
            IF (THET.LT.0.0) THET = THET + 180.0
         ELSE
            THET = 90.
         ENDIF
         IF (THET.LE.ORIG) THEN
            IN  = 1
         ELSE
            IN  = NDIV
         ENDIF
      ELSE
         PRINT *, ' GNEXT : Partially divided ',ISH,IAXIS
         IN2 = 0
         IF (ISH.EQ.5.OR.ISH.EQ.7) THEN
            IN  = 1
            IF (NDIV.GT.1) IN2 = NDIV
         ELSE
            IF (XC(1).NE.0.0.OR.XC(2).NE.0.0) THEN
               PHI = RADDEG * ATAN2 (XC(2), XC(1))
            ELSE
               PHI = 0.0
            ENDIF
            IF (ISH.EQ.6.OR.ISH.EQ.8) THEN
               IF (PHI.LT.ORIG) THEN
                  IN  = 1
               ELSE
                  IN  = NDIV
               ENDIF
            ELSE
               IN  = 1
               IF (NDIV.GT.1) IN2 = NDIV
            ENDIF
         ENDIF
      ENDIF
*
  225 IF (IDT.EQ.1) THEN
         DO 231 I = 1, 3
            X0(I) = 0.0
  231    CONTINUE
         X0(IAXIS) = ORIG + (IN - 0.5) * SDIV
         IF (ISH.EQ.4.OR.(ISH.EQ.10.AND.IAXIS.NE.1)) THEN
            CALL GCENT (IAXIS, X0)
         ENDIF
         DO 232 I = 1, 3
            XT(I)   = XC(I) - X0(I)
            XT(I+3) = XC(I+3)
  232    CONTINUE
      ELSE IF (IDT.EQ.3) THEN
         PH0  = DEGRAD * (ORIG + (IN - 0.5) * SDIV)
         CPHR = COS(PH0)
         SPHR = SIN(PH0)
         DO 233 I = 1, 4, 3
            XT(I)   = XC(I)*CPHR + XC(I+1)*SPHR
            XT(I+1) = XC(I+1)*CPHR - XC(I)*SPHR
            XT(I+2) = XC(I+2)
  233    CONTINUE
      ELSE
         DO 234 I = 1, 6
            XT(I) = XC(I)
  234    CONTINUE
      ENDIF
*
      IF (JPARM.NE.0) THEN
         IF (IQ(JPARM-3).GT.1) THEN
            JPAR = LQ(JPARM-IN)
         ELSE
            JPAR = LQ(JPARM-1)
         ENDIF
         JPAR = JPAR + 5
      ELSE
         JPAR = JVOT + 6
      ENDIF
*
      IACT = 2
      IF (ISHT.LT.5) THEN
         IF (ISHT.EQ.1) THEN
            CALL GNOBOX (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.2) THEN
            CALL GNOTRA (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.3) THEN
            CALL GNOTRA (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNOTRP (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISHT.LE.10) THEN
         IF (ISHT.EQ.5) THEN
            CALL GNOTUB (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.6) THEN
            CALL GNOTUB (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.7) THEN
            CALL GNOCON (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.8) THEN
            CALL GNOCON (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.9) THEN
            CALL GNOSPH (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNOPAR (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISHT.EQ.11) THEN
         CALL GNOPGO (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.12) THEN
         CALL GNOPCO (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.13) THEN
         CALL GNOELT (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.14) THEN
         CALL GNOHYP (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.28) THEN
         CALL GSNGTR (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE,0)
      ELSE IF (ISHT.EQ.NSCTUB) THEN
         CALL GNOCTU (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE
         PRINT *, ' GNEXT : No code for shape ', ISHT
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LT.SNEXT)  SNEXT  = SNXT
*
      IF (IN2.NE.0) THEN
         IF (IN2.NE.IN) THEN
            IN  = IN2
            GO TO 225
         ENDIF
      ENDIF
*
* ***  Calculate SNEXT and SAFETY with respect to the Mother
* ***            SAFETY only for concave volumes if finite SNEXT
* ***            has been found with respect to one of its contents
*
  300 IACT = 2
      IF (SNEXT.LT.0.9*BIG) THEN
         IF (.NOT.BTEST(IQ(JVO),2)) IACT = 0
      ENDIF
      JPAR = LQ(JGPAR-NLEVEL)
      IF (ISH.LT.5) THEN
         IF (ISH.EQ.1) THEN
            CALL GNBOX (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE )
         ELSE IF (ISH.EQ.2) THEN
            CALL GNTRAP (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.3) THEN
            CALL GNTRAP (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNTRP (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISH.LE.10) THEN
         IF (ISH.EQ.5) THEN
            CALL GNTUBE (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.6) THEN
            CALL GNTUBE (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.7) THEN
            CALL GNCONE (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.8) THEN
            CALL GNCONE (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.9) THEN
            CALL GNSPHR (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNPARA (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISH.EQ.12) THEN
         CALL GNPCON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.11) THEN
         CALL GNPGON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.13) THEN
         CALL GNELTU (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.14) THEN
         CALL GNHYPE (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.28) THEN
         CALL GSNGTR (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE,1)
      ELSE IF (ISH.EQ.NSCTUB) THEN
         CALL GNCTUB (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE
         PRINT *, ' GNEXT : No code for shape ', ISH
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LT.SNEXT)  SNEXT  = SNXT
*
  400 IF (GONLY(NLEVEL).EQ.0.) THEN
*
* ***   Case of a 'NOT ONLY' volume -> step search
*
         SAFETY = 0.
         EPSI2  = 0.5*EPSIL
         ST     = SNEXT -EPSI2
         IF (ST.LE.0) GO TO 999
         EPSI3  = 10.*EPSIL
         IF (ST.LE.EPSI3) THEN
            NN = 1
         ELSE
            NN = ST/EPSI3 +1
            ST = ST/NN
         ENDIF
*
         NBIN = 0
         SN   = 0.
  420    SN   = SN +ST
         DO 429 I = 1,3
            XT(I) = X(I) +SN*X(I+3)
  429    CONTINUE
*
         CALL GINVOL (XT, ISAME)
         IF (ISAME.EQ.0) THEN
            IF (ST.LT.EPSI2) GO TO 490
            SN   = SN -ST
            ST   = 0.5*ST
            NBIN = 1
            GO TO 420
         ENDIF
*
         IF (NBIN.NE.0) THEN
            IF (ST.LT.EPSI2) THEN
               SN = SN +EPSI2
               GO TO 490
            ELSE
               ST = 0.5*ST
               GO TO 420
            ENDIF
         ENDIF
         NN = NN -1
         IF (NN.GT.0) GO TO 420
         GO TO 999
*
  490    IF (SN.LT.SNEXT) SNEXT = SN
      ENDIF
*                                                              END GNEXT
  999 END

CDECK  ID>, GGCLO2. 
*CMZ :  3.21/02 29/03/94  15.41.19  by  S.Giani
*-- Author :
      SUBROUTINE GGCLOS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    Closes off the geometry setting.                            *
C.    *    Initializes the search list for the contents of each        *
C.    *    volume following the order they have been positioned, and   *
C.    *    inserting the content '0' when a call to GSNEXT (-1) has    *
C.    *    been required by the user.                                  *
C.    *    Performs the development of the JVOLUM structure for all    *
C.    *    volumes with variable parameters, by calling GGDVLP.        *
C.    *    Interprets the user calls to GSORD, through GGORD.          *
C.    *    Computes and stores in a bank (next to JVOLUM mother bank)  *
C.    *    the number of levels in the geometrical tree and the        *
C.    *    maximum number of contents per level, by calling GGNLEV.    *
C.    *    Sets status bit for CONCAVE volumes, through GGCAVE.        *
C.    *    Completes the JSET structure with the list of volume names  *
C.    *    which identify uniquely a given physical detector, the      *
C.    *    list of bit numbers to pack the corresponding volume copy   *
C.    *    numbers, and the generic path(s) in the JVOLUM tree,        *
C.    *    through the routine GHCLOS.                                 *
C.    *                                                                *
C.    *    Called by : <USER>                                          *
C.    *    Authors   : R.Brun, F.Bruyant  *********                    *
C.    *                                                                *
C.    *    Modified by S.Egli at 15.9.90: automatic sorting of volumes *
C     *    done by calling GGORDQ for each volume                      *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCLIST/NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     +       ,NSTAT,LHSTA(20),LGET (20),LSAVE(20),LSETS(20),LPRIN(20)
     +             ,LGEOM(20),LVIEW(20),LPLOT(20),LSTAT(20)
C
      INTEGER       NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     + ,NSTAT,LHSTA,LGET ,LSAVE,LSETS,LPRIN,LGEOM,LVIEW,LPLOT,LSTAT
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCOPTI/ IOPTIM
C
      INTEGER IOPTIM
      CHARACTER*4 NAME
      LOGICAL BTEST
C.
C.    ------------------------------------------------------------------
C.
*
* *** Stop the run in case of serious anomaly during initialization
*
      IF (IEORUN.NE.0) THEN
         WRITE (CHMAIL, 1001)
         CALL GMAIL (0, 0)
         STOP
      ENDIF
*
      IF (NVOLUM.LE.0) THEN
         WRITE (CHMAIL, 1002) NVOLUM
         CALL GMAIL (0, 0)
         GO TO 999
      ENDIF
*
      NPUSH = NVOLUM -IQ(JVOLUM-2)
      CALL MZPUSH (IXCONS, JVOLUM, NPUSH, NPUSH,'I')
*
* *** Loop over volumes, create default JNear banks as relevant,
*      and release unused bank space
*
      IDO = 0
      DO 80 IVO = 1,NVOLUM
         JVO = LQ(JVOLUM-IVO)
*
* *** Check if Tracking medium has been defined
*
         NMED=Q(JVO+4)
         IF(NMED.LE.0.OR.NMED.GT.IQ(JTMED-2))THEN
            WRITE(CHMAIL,1003)IQ(JVOLUM+IVO)
            CALL GMAIL (0, 0)
         ELSE
            IF(LQ(JTMED-NMED).EQ.0)THEN
               WRITE(CHMAIL,1003)IQ(JVOLUM+IVO)
               CALL GMAIL (0, 0)
            ENDIF
         ENDIF
         IF (BTEST(IQ(JVO),0)) GO TO 80
         IDO = 1
         IQ(JVO) = IBSET(IQ(JVO),0)
         NINL  = IQ(JVO-2)
         NIN   = Q(JVO+3)
         NUSED = IABS(NIN)
         IF (NIN.GT.0) THEN
*           reserve enough additional space for sorted volumes
            IF(NIN.LE.1.OR.NIN.GT.500.OR.IOPTIM.LT.0)THEN
              NUSED=NUSED+1
            ELSE
              NUSED=NUSED+2
            ENDIF
         ENDIF
*
         NPUSH = NUSED -NINL
         DO 90 IN=NINL,NUSED+1,-1
            JIN = LQ(JVO-IN)
            IF(JIN.GT.0) THEN
               CALL MZDROP(IXCONS,JIN,'L')
            ENDIF
  90     CONTINUE
         CALL MZPUSH (IXCONS, JVO, NPUSH, 0, 'I')
         IF (NIN.LE.0) GO TO 80
*
         IF(BTEST(IQ(JVO),3)) THEN
            IZERO=1
         ELSE
            IZERO=0
         ENDIF
         NEL = NIN +IZERO
         JN = LQ(JVO-NIN-1)
         IF(JN.EQ.0) THEN
            CALL MZBOOK (IXCONS,JN,JVO,-NIN-1,'VONE',0,0,NEL+1,2,0)
         ENDIF
         IQ(JN-5) = IVO
         IQ(JN+1) = NEL
         JN = JN +1
         DO 29 I = 1,NIN
            IQ(JN+IZERO+I) = I
   29    CONTINUE
         IF (IZERO.NE.0) IQ(JN+1) = 0
*
   80 CONTINUE
*
      IF (IDO.NE.0) THEN
*
* ***    Perform development of JVOLUM structure where necessary
*
         CALL GGDVLP
*
* ***    Fill GSORD ordering banks if required
*
* Modified by S.Egli to allow GGORDQ to find the optimum sorting for
* all volumes
*
         IF(IOPTIM.GE.1)THEN
            WRITE(6,'(A)')' GGCLOS: Start automatic volume ordering:'
         ENDIF
         DO 91 IVO = 1,NVOLUM
            JVO = LQ(JVOLUM-IVO)
            NIN = Q(JVO+3)
            ISEARC=Q(JVO+1)
            IF(ISEARC.GT.0) GO TO 91
*           check if sorting not possible or not wanted
            IF(NIN.LE.1.OR.NIN.GT.500.OR.IOPTIM.LT.0)THEN
               Q(JVO+1)=0.
               IF(NIN.GT.500.AND.IOPTIM.GE.1)THEN
                 CALL UHTOC(IQ(JVOLUM+IVO),4,NAME,4)
                 WRITE (CHMAIL,1004) NAME,NIN
                 CALL  GMAIL (0, 0)
               ENDIF
            ELSEIF(IOPTIM.EQ.0)THEN
               IF(ISEARC.LT.0)CALL GGORD (IVO)
            ELSEIF(IOPTIM.EQ.1)THEN
               IF(ISEARC.EQ.0) THEN
                  CALL GGORDQ(IVO)
               ELSE
                  CALL GGORD (IVO)
               END IF
            ELSE
               CALL GGORDQ(IVO)
            ENDIF
   91    CONTINUE
*
* ***    Set status bit for concave volumes
*
         CALL GGCAVE
*
* ***    Compute maximum number of levels and of contents per level
*
         CALL GGNLEV
*
      ENDIF
*
* *** Scan the volume structure to retrieve the path through
*      the physical tree for all sensitive detectors
*
       CALL GHCLOS
*
* *** Books STAT banks if data card STAT is submitted
*
      IF (NSTAT.GT.0)  CALL GBSTAT
*
      CALL MZGARB (IXCONS, 0)
*
 1001 FORMAT (' Severe diagnostic in initialization phase. STOP')
 1002 FORMAT (' GGCLOS : NVOLUM =',I5,' *****')
 1003 FORMAT (' Illegal tracking medium number in volume : ',A4)
 1004 FORMAT (' GGORDQ : Volume ',A4,' has more than 500 (',
     +        I3,') daughters ; volume sorting not possible !')
*                                                             END GGCLOS
  999 END

CDECK  ID>, GTMED2. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTMEDI (X, NUMED)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Finds in which volume/medium the point X is, and updates the *
C.    *    common /GCVOLU/ and the structure JGPAR accordingly.        *
C.    *                                                                *
C.    *   NUMED returns the tracking medium number, or 0 if point is   *
C.    *         outside the experimental setup.                        *
C.    *                                                                *
C.    *   Note : For INWVOL = 2, INFROM set to a positive number is    *
C.    *      interpreted by GTMEDI as the number IN of the content     *
C.    *      just left by the current track within the mother volume   *
C.    *      where the point X is assumed to be.                       *
C.    *                                                                *
C.    *   Note : INFROM is set correctly by this routine but it is     *
C.    *      used on entrance only in the case GSNEXT has been called  *
C.    *      by the user. In other words the value of INFROM received  *
C.    *      on entrance is not considered necessarily valid. This     *
C.    *      assumption has been made for safety. A wrong value of     *
C.    *      INFROM can cause wrong tracking.                          *
C.    *                                                                *
C.    *   Called by : GTRACK                                           *
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant, A.McPherson       *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCCHAN/LSAMVL
      LOGICAL LSAMVL
C.
      DIMENSION  X(*)
      REAL       XC(3), XT(3)
      LOGICAL    BTEST
C.
C.    ------------------------------------------------------------------
*
* SECTION I: The /GCVOLU/ table contains the initial guess for a path
*            in the geometry tree on which X may be found.  Look along this
*            path until X is found inside.  This is the starting position.
*            If this is an ONLY volume with no daughters, we are done;
*            otherwise reset search record variables, proceed to section II.
*
* *** Check if point is in current volume
*
      INFR = 0
      INGT = 0
      JVIN = 0
*
* *** LSAMVL is a logical variable that indicates whether we are still
* *** in the current volume or not. It is used in GTRACK to detect
* *** precision problems.
      LSAMVL = .TRUE.
C*****  Code Expanded From Routine:  GTRNSF
C
  100 IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
         XC(1) = X(1) - GTRAN(1,NLEVEL)
         XC(2) = X(2) - GTRAN(2,NLEVEL)
         XC(3) = X(3) - GTRAN(3,NLEVEL)
*
      ELSE
         XL1 = X(1) - GTRAN(1,NLEVEL)
         XL2 = X(2) - GTRAN(2,NLEVEL)
         XL3 = X(3) - GTRAN(3,NLEVEL)
         XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) + XL3*
     +      GRMAT(3,NLEVEL)
         XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) + XL3*
     +      GRMAT(6,NLEVEL)
         XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) + XL3*
     +      GRMAT(9,NLEVEL)

      ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
      JVO  = LQ(JVOLUM-LVOLUM(NLEVEL))
*
* Note: At entry the variable INGOTO may contain the index of a volume
* contained within the current one at NLEVEL.  If so, begin by checking
* if X lies inside.  This improves the search speed over that of GMEDIA.
*
      NIN = Q(JVO+3)
      IF ((INGOTO.LE.0).OR.(INGOTO.GT.NIN)) THEN
         INGOTO = 0
      ELSE
*
* ***   Entrance in content INGOTO predicted by GTNEXT
*
         JIN  = LQ(JVO-INGOTO)
         IVOT = Q(JIN+2)
         JVOT = LQ(JVOLUM-IVOT)
         JPAR = LQ(JGPAR-NLEVEL-1)
*
         IROTT = Q(JIN+4)
C*****  Code Expanded From Routine:  GITRAN
C.
C.    ------------------------------------------------------------------
C.
         IF (IROTT .EQ. 0) THEN
            XT(1) = XC(1) - Q(5+JIN)
            XT(2) = XC(2) - Q(6+JIN)
            XT(3) = XC(3) - Q(7+JIN)
*
         ELSE
            XL1 = XC(1) - Q(5+JIN)
            XL2 = XC(2) - Q(6+JIN)
            XL3 = XC(3) - Q(7+JIN)
            JR = LQ(JROTM-IROTT)
            XT(1) = XL1*Q(JR+1) + XL2*Q(JR+2) + XL3*Q(JR+3)
            XT(2) = XL1*Q(JR+4) + XL2*Q(JR+5) + XL3*Q(JR+6)
            XT(3) = XL1*Q(JR+7) + XL2*Q(JR+8) + XL3*Q(JR+9)
*
         ENDIF
C*****  End of Code Expanded From Routine:  GITRAN
*
*   *   Check if point is in content
*
         CALL GINME (XT, Q(JVOT+2), Q(JPAR+1), IYES)
         IF (IYES.NE.0) THEN
*
*          If so, prepare information for volume retrieval, and return
*
            LSAMVL = .FALSE.
            NL1 = NLEVEL +1
            LVOLUM(NL1) = IVOT
            NAMES(NL1)  = IQ(JVOLUM+IVOT)
            NUMBER(NL1) = Q(JIN+3)
            LINDEX(NL1) = INGOTO
            LINMX(NL1)  = Q(JVO+3)
            GONLY(NL1)  = Q(JIN+8)
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN
               NLDEV(NL1) = NLDEV(NLEVEL)
            ELSE
               NLDEV(NL1) = NL1
            ENDIF
            CALL GTRMUL (GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), Q(JIN+5),
     +                   IROTT, GTRAN(1,NL1), GRMAT(1,NL1))
            NLEVEL = NL1
            XC(1) = XT(1)
            XC(2) = XT(2)
            XC(3) = XT(3)
            JVO = JVOT
            INFROM = 0
            GO TO 190
         ENDIF
      ENDIF
*
* End of INGOTO processing
*
      JPAR = LQ(JGPAR-NLEVEL)
      CALL GINME (XC, Q(JVO+2), Q(JPAR+1), IYES)
      IF (IYES.EQ.0) THEN
*
*  **   Point not in current volume, go up the tree
*
         LSAMVL = .FALSE.
         INGOTO = 0
         IF (NLEVEL.GT.1) THEN
            NLEVEL = NLEVEL -1
            JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
            NIN = Q(JVO+3)
            IF(NIN.GT.0) THEN
               INFROM=LINDEX(NLEVEL+1)
            ELSE
               INFROM=0
            ENDIF
            INFR = INFROM
            GO TO 100
         ELSE
*
*   *      Point is outside setup
*
            NUMED = 0
            GO TO 999
         ENDIF
      ELSE
*
*   *      Point in current volume but not in INGOTO. We block the
*   *      corresponding volume
*
         IF (INGOTO.GT.0) THEN
            INGT = INGOTO
            JIN = LQ(JVO-INGOTO)
            IQ(JIN) = IBSET(IQ(JIN),4)
         ENDIF
      ENDIF
*
*   *      Found a volume up the tree which contains our point. We block
*   *      the branch we came up from.
*
      IF(INFR.GT.0) THEN
         JIN=LQ(JVO-INFR)
         IQ(JIN) = IBSET(IQ(JIN),4)
         JVIN = JIN
      ENDIF
*
*  **   Point is in current volume
*
  190 INGOTO = 0
      NLMIN = NLEVEL
      IF (INWVOL.NE.2) INFROM = 0
      NLMANY = 0
*
* SECTION II: X is found inside current node at NLEVEL in /GCVOLU/.
*             Search all contents recursively for any containing X.
*             Take the first one found, if any, and continue at that
*             level, incrementing NLEVEL and extending /GCVOLU/ tables.
*             This is continued until a level is reached where X is not
*             found in any of the contents, or there are no contents.
* Note: Since Section II is re-entered from Section III, a blocking word
* is used to mark those contents already checked.  Upon exit from Section
* II, these blocking words are cleared at NLEVEL, but may remain set in
* levels between NLEVEL-1 and NLMIN, if any.  They must be cleared at exit.
*
*  **  Check contents, if any
*
  200 JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
      NIN = Q(JVO+3)
*
*   *   Case with no contents
*
      IF (NIN.EQ.0) THEN
         GO TO 300
*
*   *   Case with contents defined by division
*
      ELSEIF (NIN.LT.0) THEN
         CALL GMEDIV (JVO, IN, XC, 1)
         IF (IN.GT.0) THEN
            INFROM = 0
            INFR   = 0
            INGT   = 0
            LSAMVL = .FALSE.
            GO TO 200
         ENDIF
*
*   *  Case with contents positioned
*
      ELSE
         JCONT = LQ(JVO-NIN-1)+1
         NCONT = IQ(JCONT)
         ISEARC = Q(JVO+1)
         IF (ISEARC.LT.0) THEN
*
*       Prepare access to contents, when ordered by GSORD
*
            JSB = LQ(LQ(JVO-NIN-1))
            IAX = Q(JSB+1)
            NSB = Q(JSB+2)
            IF (IAX.LE.3) THEN
               CX   = XC(IAX)
            ELSE
               CALL GFCOOR (XC, IAX, CX)
            ENDIF
            IDIV = ABS(LOCATF (Q(JSB+3), NSB, CX))
            IF (IDIV.EQ.0) THEN
               IF (IAX.NE.6) GO TO 260
               IDIV = NSB
            ELSEIF (IDIV.EQ.NSB) THEN
               IF (IAX.NE.6) GO TO 260
            ENDIF
            JSC0  = LQ(JVO-NIN-2)
            NCONT = IQ(JSC0+IDIV)
            JCONT = LQ(JSC0-IDIV)
         ELSE
*
*       otherwise, scan contents (possibly a user selection of them)
*
            JNEAR = LQ(JVO-NIN-1)
            IF (ISEARC.GT.0) THEN
               CALL GUNEAR (ISEARC, 1, XC, JNEAR)
            ELSEIF (INFROM.GT.0) THEN
               JNUP = LQ(LQ(JVO-INFROM)-1)
               IF (JNUP.GT.0) THEN
                  JNEAR = JNUP
               ENDIF
            ENDIF
            JCONT = JNEAR +1
            NCONT = IQ(JCONT)
         ENDIF
*
*     For each selected content in turn, check if point is inside
*
         DO 259 ICONT=1,NCONT
            IN = IQ(JCONT+ICONT)
            IF(IN.EQ.0) THEN
*
*     If the value IQ(JCONT+ICONT)=0 then we are back in the mother.
*     So jump to 260, the search is finished. Clean-up should be done
*     only up to ICONT-1, so we set:
*
               NCONT=ICONT-1
               GOTO 260
            ELSE
            JIN = LQ(JVO-IN)
            IF (.NOT.BTEST(IQ(JIN),4)) THEN
               CALL GMEPOS (JVO, IN, XC, 1)
               IF (IN.GT.0) THEN
                  IF (GONLY(NLEVEL).NE.0.) NLMANY = 0
                  INFROM = 0
                  INGT   = 0
                  INFR   = 0
                  LSAMVL = .FALSE.
                  GO TO 200
               ELSE
                  IQ(JIN) = IBSET(IQ(JIN),4)
               ENDIF
            ENDIF
            ENDIF
  259    CONTINUE
*
  260    IF(NCONT.EQ.NIN) THEN
         DO 268 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  268    CONTINUE
         ELSE
         DO 269 ICONT=1,NCONT
            IN  = IQ(JCONT+ICONT)
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  269    CONTINUE
         IF(INFR.NE.0) THEN
            JIN = LQ(JVO-INFR)
            IQ(JIN) = IBCLR(IQ(JIN),4)
            INFR = 0
         ENDIF
         IF(INGT.NE.0) THEN
            JIN = LQ(JVO-INGT)
            IQ(JIN) = IBCLR(IQ(JIN),4)
            INGT = 0
         ENDIF
         ENDIF
*
      ENDIF
*
* SECTION III: X is found at current node (NLEVEL in /GCVOLU) but not in
*              any of its contents, if any.  If this is a MANY volume,
*              save it as a candidate best-choice, and continue the search
*              by backing up the tree one node and proceed to Section II.
*              If this is an ONLY volume, proceed to Section IV.
*
* *** Point is in current volume/medium, and not in any content
*
  300 IF (GONLY(NLEVEL).EQ.0.) THEN
*
*  **   Lowest level is 'NOT ONLY'
*
         IF (NLEVEL.GT.NLMANY) THEN
            CALL GSCVOL
            NLMANY = NLEVEL
         ENDIF
*
*   *   Go up the tree up to a volume with positioned contents
*
  310    INFROM = LINDEX(NLEVEL)
         NLEVEL = NLEVEL -1
         JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
         NIN    = Q(JVO+3)
         IF (NIN.LT.0) GO TO 310
*
C*****  Code Expanded From Routine:  GTRNSF
C
         IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
            XC(1) = X(1) - GTRAN(1,NLEVEL)
            XC(2) = X(2) - GTRAN(2,NLEVEL)
            XC(3) = X(3) - GTRAN(3,NLEVEL)
*
         ELSE
            XL1 = X(1) - GTRAN(1,NLEVEL)
            XL2 = X(2) - GTRAN(2,NLEVEL)
            XL3 = X(3) - GTRAN(3,NLEVEL)
            XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) +
     +      XL3* GRMAT(3,NLEVEL)
            XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) +
     +      XL3* GRMAT(6,NLEVEL)
            XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) +
     +      XL3* GRMAT(9,NLEVEL)
*
         ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
         INFR = INFROM
         JIN = LQ(JVO-INFROM)
         IQ(JIN) = IBSET(IQ(JIN),4)
         NLMIN = MIN(NLEVEL,NLMIN)
         GO TO 200
      ENDIF
*
* SECTION IV: This is the end of the search.  The current node (NLEVEL
*             in /GCVOLU/) is the lowest ONLY volume in which X is found.
*             If X was also found in any of its contents, they are MANY
*             volumes: the best-choice is the one among them at the greatest
*             level in the tree, and it is stored.  Otherwise the current
*             volume is the solution.  Before exit, all of the blocking
*             words leftover in the tree must be reset to zero.
* Note: A valid structure is assumed, in which no ONLY volumes overlap.
* If this rule is violated, or if a daughter is not entirely contained
* within the mother volume, the results are unpredictable.
*
      DO 419 NL=NLMIN,NLEVEL-1
         JVO = LQ(JVOLUM-LVOLUM(NL))
         NIN = Q(JVO+3)
         DO 418 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  418    CONTINUE
  419 CONTINUE
*
      IF (NLMANY.GT.0) CALL GFCVOL
      JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
      IF(JVIN.NE.0) IQ(JVIN) = IBCLR(IQ(JVIN),4)
      NUMED = Q(JVO+4)
*                                                             END GTMEDI
  999 IF(JGSTAT.NE.0) CALL GFSTAT(4)
      END
CDECK  ID>, GINVO2. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GINVOL (X, ISAME)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GINVOL (X, ISAME*)                                    *
C.    *                                                                *
C.    *   Checks if particle at point X has left current volume/medium *
C.    *   If so, returns ISAME = 0 and prepares information useful to  *
C.    *    identify the new volume entered.                            *
C.    *   Otherwise, returns ISAME = 1                                 *
C.    *                                                                *
C.    *   Note : INGOTO is set by GTNEXT, to transmit the information  *
C.    *       on the one volume which has limited the step SNEXT,      *
C.    *       >0 : INth content                                        *
C.    *       =0 : current volume                                      *
C.    *       <0 : -NLONLY, with NLONLY defined as the first 'ONLY'    *
C.    *           level up in the tree for the 'NOT-ONLY' volume       *
C.    *           where the point X is found to be.                    *
C.    *                                                                *
C.    *   Called by : GNEXT, GTELEC, GTHADR, GTMUON, GTNEXT            *
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant                    *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
C.
      DIMENSION  X(*)
      REAL       XC(3), XT(3)
      LOGICAL    BTEST
C.
C.    ------------------------------------------------------------------
*
* SECTION I: The /GCVOLU/ table contains the presumed location of X in the
*            geometry tree, at level NLEVEL.  The suggestion is that INGOTO
*            is the index of a content at NLEVEL which may also contain X.
*            If this is so, ISAME=0 and return.  INGOTO is left unchanged.
*            If this is not so, have we left the volume at NLEVEL altogether?
*            If so, ISAME=0 and INGOTO=0, return.  Otherwise, this is the
*            starting position for a search.  Reset search record variables
*            and proceed to section II.
*
* *** Check if point is in current volume
*
      INGT = 0
C*****  Code Expanded From Routine:  GTRNSF
C
  100 IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
         XC(1) = X(1) - GTRAN(1,NLEVEL)
         XC(2) = X(2) - GTRAN(2,NLEVEL)
         XC(3) = X(3) - GTRAN(3,NLEVEL)
*
      ELSE
         XL1 = X(1) - GTRAN(1,NLEVEL)
         XL2 = X(2) - GTRAN(2,NLEVEL)
         XL3 = X(3) - GTRAN(3,NLEVEL)
         XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) + XL3*
     +      GRMAT(3,NLEVEL)
         XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) + XL3*
     +      GRMAT(6,NLEVEL)
         XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) + XL3*
     +      GRMAT(9,NLEVEL)
*
      ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
      JVO  = LQ(JVOLUM-LVOLUM(NLEVEL))
*
* Note: At entry the variable INGOTO may contain the index of a volume
* contained within the current one at NLEVEL.  If so, begin by checking
* if X lies inside.  This improves the search speed over that of GMEDIA.
*
      NIN = Q(JVO+3)
      IF ((INGOTO.LE.0).OR.(INGOTO.GT.NIN)) THEN
         INGOTO = 0
      ELSE
*
* ***   Entrance in content INGOTO predicted by GTNEXT
*
         JIN  = LQ(JVO-INGOTO)
         IVOT = Q(JIN+2)
         JVOT = LQ(JVOLUM-IVOT)
         JPAR = LQ(JGPAR-NLEVEL-1)
*
         IROTT = Q(JIN+4)
C*****  Code Expanded From Routine:  GITRAN
C.
C.    ------------------------------------------------------------------
C.
         IF (IROTT .EQ. 0) THEN
            XT(1) = XC(1) - Q(5+JIN)
            XT(2) = XC(2) - Q(6+JIN)
            XT(3) = XC(3) - Q(7+JIN)
*
         ELSE
            XL1 = XC(1) - Q(5+JIN)
            XL2 = XC(2) - Q(6+JIN)
            XL3 = XC(3) - Q(7+JIN)
            JR = LQ(JROTM-IROTT)
            XT(1) = XL1*Q(JR+1) + XL2*Q(JR+2) + XL3*Q(JR+3)
            XT(2) = XL1*Q(JR+4) + XL2*Q(JR+5) + XL3*Q(JR+6)
            XT(3) = XL1*Q(JR+7) + XL2*Q(JR+8) + XL3*Q(JR+9)
*
         ENDIF
C*****  End of Code Expanded From Routine:  GITRAN
*
*   *   Check if point is in content
*
         CALL GINME (XT, Q(JVOT+2), Q(JPAR+1), IYES)
         IF (IYES.NE.0) THEN
*
*          If so, prepare information for volume retrieval, and return
*
            NLEVIN = NLEVEL +1
            LVOLUM(NLEVIN) = IVOT
            NAMES(NLEVIN)  = IQ(JVOLUM+IVOT)
            NUMBER(NLEVIN) = Q(JIN+3)
            LINDEX(NLEVIN) = INGOTO
            LINMX(NLEVIN)  = Q(JVO+3)
            GONLY(NLEVIN)  = Q(JIN+8)
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN
               NLDEV(NLEVIN) = NLDEV(NLEVEL)
            ELSE
               NLDEV(NLEVIN) = NLEVIN
            ENDIF
            CALL GTRMUL (GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), Q(JIN+5),
     +                   IROTT, GTRAN(1,NLEVIN), GRMAT(1,NLEVIN))
            ISAME = 0
            GO TO 999
         ENDIF
      ENDIF
*
* End of INGOTO processing
*
      JPAR = LQ(JGPAR-NLEVEL)
      CALL GINME (XC, Q(JVO+2), Q(JPAR+1), IYES)
      IF (IYES.EQ.0) THEN
         ISAME  = 0
         INGOTO = 0
         GO TO 999
      ENDIF
*
*  **   Point is in current volume
*
      NLEVIN = NLEVEL
      NLMIN = NLEVEL
      IF ((INFROM.LE.0).OR.(INFROM.GT.NIN)) THEN
         INFROM = 0
      ENDIF
      INFR = INFROM
      NLMANY = 0
      IF (INGOTO.GT.0) THEN
         INGT = INGOTO
         JIN = LQ(JVO-INGOTO)
         IQ(JIN) = IBSET(IQ(JIN),4)
      ENDIF
*
* SECTION II: X is found inside current node at NLEVEL in /GCVOLU/.
*             Search all contents for any containing X.  Take the
*             first one found, incrementing NLEVEL and extending the
*             /GCVOLU/ tables.  Otherwise if the list of contents is
*             exhausted without finding X inside, proceed to Section III.
* Note: Since Section II is re-entered from Section III, a blocking word
* is used to mark those contents already checked.  Upon exit from Section
* II, these blocking words are cleared at NLEVEL, but may remain set in
* levels between NLEVEL-1 and NLMIN, if any.  They must be cleared at exit.
*
*  **  Check contents, if any
*
  200 JVO = LQ(JVOLUM-LVOLUM(NLEVEL))
      NIN = Q(JVO+3)
*
*   *   Case with no contents
*
      IF (NIN.EQ.0) THEN
         GO TO 300
*
*   *   Case with contents defined by division
*
      ELSEIF (NIN.LT.0) THEN
         CALL GMEDIV (JVO, IN, XC, 1)
         IF (IN.GT.0) THEN
            IF ((GONLY(NLEVEL).EQ.0).AND.
     +          (NLEVEL.LE.NLEVIN)) THEN
                INFR = 0
                INGT = 0
                GO TO 200
             ELSE
                GO TO 450
             ENDIF
         ENDIF
*
*   *  Case with contents positioned
*
      ELSE
         JCONT = LQ(JVO-NIN-1)+1
         NCONT = IQ(JCONT)
         ISEARC = Q(JVO+1)
         IF (ISEARC.LT.0) THEN
*
*       Prepare access to contents, when ordered by GSORD
*
            JSB = LQ(LQ(JVO-NIN-1))
            IAX = Q(JSB+1)
            NSB = Q(JSB+2)
            IF (IAX.LE.3) THEN
               IDIV = LOCATF (Q(JSB+3), NSB, XC(IAX))
            ELSE
               CALL GFCOOR (XC, IAX, CX)
               IDIV = LOCATF (Q(JSB+3), NSB, CX)
            ENDIF
            IF (IDIV.LT.0) IDIV = -IDIV
            IF (IDIV.EQ.0) THEN
               IF (IAX.NE.6) GO TO 260
               IDIV = NSB
            ELSEIF (IDIV.EQ.NSB) THEN
               IF (IAX.NE.6) GO TO 260
            ENDIF
            JSC0  = LQ(JVO-NIN-2)
            NCONT = IQ(JSC0+IDIV)
            JCONT = LQ(JSC0-IDIV)
         ELSE
*
*       otherwise, scan contents (possibly a user selection of them)
*
            JNEAR = LQ(JVO-NIN-1)
            IF (ISEARC.GT.0) THEN
               CALL GUNEAR (ISEARC, 1, XC, JNEAR)
            ELSEIF (INFR.GT.0) THEN
               JNUP = LQ(LQ(JVO-INFR)-1)
               IF (JNUP.GT.0) THEN
                  JNEAR = JNUP
               ENDIF
            ENDIF
            JCONT = JNEAR +1
            NCONT = IQ(JCONT)
         ENDIF
*
*     For each selected content in turn, check if point is inside
*
         DO 259 ICONT=1,NCONT
            IN = IQ(JCONT+ICONT)
            IF(IN.EQ.0) THEN
*
*     If the value IQ(JCONT+ICONT)=0 then we are back in the mother.
*     So jump to 260, the search is finished. Clean-up should be done
*     only up to ICONT-1, so we set:
*
               NCONT=ICONT-1
               GOTO 260
            ELSE
            JIN = LQ(JVO-IN)
            IF (.NOT.BTEST(IQ(JIN),4)) THEN
               CALL GMEPOS (JVO, IN, XC, 1)
               IF (IN.GT.0) THEN
                  IF ((GONLY(NLEVEL).EQ.0).AND.
     +                (NLEVEL.LE.NLEVIN)) THEN
                     INFR = 0
                     INGT = 0
                     GO TO 200
                  ELSE
                     GO TO 450
                  ENDIF
               ELSE
                  IQ(JIN) = IBSET(IQ(JIN),4)
               ENDIF
            ENDIF
            ENDIF
  259    CONTINUE
*
  260    IF(NCONT.EQ.NIN) THEN
         DO 268 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  268    CONTINUE
         ELSE
         DO 269 ICONT=1,NCONT
            IN  = IQ(JCONT+ICONT)
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  269    CONTINUE
         IF(INFR.NE.0) THEN
            JIN = LQ(JVO-INFR)
            IQ(JIN) = IBCLR(IQ(JIN),4)
         ENDIF
         IF(INGT.NE.0) THEN
            JIN = LQ(JVO-INGT)
            IQ(JIN) = IBCLR(IQ(JIN),4)
         ENDIF
         ENDIF
*
      ENDIF
*
* SECTION III: X is found at current node (NLEVEL in /GCVOLU/) but not in
*              any of its contents, if any.  If this is a MANY volume,
*              save it as a candidate best-choice, and continue the search
*              by backing up the tree one node and proceed to Section II.
*              If this is an ONLY volume, proceed to Section IV.
*
* *** Point is in current volume/medium, and not in any content
*
  300 IF (GONLY(NLEVEL).EQ.0.) THEN
*
*  **   Lowest level is 'NOT ONLY'
*
         IF (NLMANY.EQ.0) THEN
            CALL GSCVOL
            NLMANY = NLEVEL
         ENDIF
*
*   *   Go up the tree up to a volume with positioned contents
*
  310    INFR   = LINDEX(NLEVEL)
         NLEVEL = NLEVEL -1
         JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
         NIN    = Q(JVO+3)
         IF (NIN.LT.0) GO TO 310
*
C*****  Code Expanded From Routine:  GTRNSF
C
         IF (GRMAT(10,NLEVEL) .EQ. 0.) THEN
            XC(1) = X(1) - GTRAN(1,NLEVEL)
            XC(2) = X(2) - GTRAN(2,NLEVEL)
            XC(3) = X(3) - GTRAN(3,NLEVEL)
*
         ELSE
            XL1 = X(1) - GTRAN(1,NLEVEL)
            XL2 = X(2) - GTRAN(2,NLEVEL)
            XL3 = X(3) - GTRAN(3,NLEVEL)
            XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) +
     +      XL3* GRMAT(3,NLEVEL)
            XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) +
     +      XL3* GRMAT(6,NLEVEL)
            XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) +
     +      XL3* GRMAT(9,NLEVEL)

         ENDIF
C*****  End of Code Expanded From Routine:  GTRNSF
*
         JIN = LQ(JVO-INFR)
         IQ(JIN) = IBSET(IQ(JIN),4)
         NLMIN = MIN(NLEVEL,NLMIN)
         GO TO 200
      ENDIF
*
* SECTION IV: This is the end of the search.
*             (1) Entry at 400:  ISAME = 1     The current node (NLEVEL
*             in /GCVOLU/) is an ONLY volume and there were no contents
*             in the tree below it which could claim X.
*             (2) Entry at 450:  ISAME = 0    Section II has just found
*             another volume which has more claim to X than the current
*             one: either another ONLY or a deeper MANY was found.
* Note: A valid structure is assumed, in which no ONLY volumes overlap.
* If this rule is violated, or if a daughter is not entirely contained
* within the mother volume, the results are unpredictable.
*
  400 ISAME = 1
      GOTO 480

  450 ISAME = 0

  480 DO 489 NL=NLMIN,NLEVEL-1
         JVO = LQ(JVOLUM-LVOLUM(NL))
         NIN = Q(JVO+3)
         DO 488 IN=1,NIN
            JIN = LQ(JVO-IN)
            IQ(JIN) = IBCLR(IQ(JIN),4)
  488    CONTINUE
  489 CONTINUE
*
      IF (NLMANY.GT.0) THEN
         CALL GFCVOL
         NLEVIN = NLEVEL
      ELSEIF (NLEVEL.GT.NLEVIN) THEN
         INGOTO = LINDEX(NLEVEL)
         NL = NLEVIN
         NLEVIN = NLEVEL
         NLEVEL = NL
      ENDIF
*                                                             END GINVOL
  999 IF(JGSTAT.NE.0) CALL GFSTAT(ISAME)
      END
CDECK  ID>, GTNEX2. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTNEXT
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   SUBR. GTNEXT                                                 *
C.    *                                                                *
C.    *   Computes SAFETY and, only when new SAFETY is smaller than    *
C.    *    STEP, computes SNEXT.                                       *
C.    *   STEP has to be preset to BIG or to physical step size        *
C.    *                                                                *
C.    *   Called by : GTELEC, GTGAMA, GTHADR, GTMUON, GTNEUT, GTNINO   *
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant                    *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      PARAMETER ( NSBOX=1,  NSTRD1=2, NSTRD2=3, NSTRAP=4, NSTUBE=5,
     +  NSTUBS=6, NSCONE=7, NSCONS=8, NSSPHE=9, NSPARA=10,NSPGON=11,
     +  NSPCON=12,NSELTU=13,NSHYPE=14,NSGTRA=28, NSCTUB=29 )
C.
      PARAMETER (BIG1=0.9*BIG)
C.
      REAL      X0(3), XC(6), XT(6)
      INTEGER   IDTYP(3,12)
      LOGICAL   BTEST
C.
      DATA  IDTYP / 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 2, 3, 1,
     +              2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 4, 3, 1, 1, 1,
     +              2, 3, 1, 2, 3, 1/
C.
C.    ------------------------------------------------------------------
*
* * *** Transform current point and direction into local reference system
*
      IF (GRMAT(10,NLEVEL).EQ.0.) THEN
         XC(1) = VECT(1) - GTRAN(1,NLEVEL)
         XC(2) = VECT(2) - GTRAN(2,NLEVEL)
         XC(3) = VECT(3) - GTRAN(3,NLEVEL)
         XC(4) = VECT(4)
         XC(5) = VECT(5)
         XC(6) = VECT(6)
      ELSE
C*****  Code Expanded From Routine:  GTRNSF
C
*
         XL1 = VECT(1) - GTRAN(1,NLEVEL)
         XL2 = VECT(2) - GTRAN(2,NLEVEL)
         XL3 = VECT(3) - GTRAN(3,NLEVEL)
         XC(1) = XL1*GRMAT(1,NLEVEL) + XL2*GRMAT(2,NLEVEL) + XL3*
     1      GRMAT(3,NLEVEL)
         XC(2) = XL1*GRMAT(4,NLEVEL) + XL2*GRMAT(5,NLEVEL) + XL3*
     1      GRMAT(6,NLEVEL)
         XC(3) = XL1*GRMAT(7,NLEVEL) + XL2*GRMAT(8,NLEVEL) + XL3*
     1      GRMAT(9,NLEVEL)
*
C*****  End of Code Expanded From Routine:  GTRNSF
C*****  Code Expanded From Routine:  GROT
C
         XC(4) = VECT(4)*GRMAT(1,NLEVEL) + VECT(5)*GRMAT(2,NLEVEL) +
     1      VECT(6)*GRMAT(3,NLEVEL)
         XC(5) = VECT(4)*GRMAT(4,NLEVEL) + VECT(5)*GRMAT(5,NLEVEL) +
     1      VECT(6)*GRMAT(6,NLEVEL)
         XC(6) = VECT(4)*GRMAT(7,NLEVEL) + VECT(5)*GRMAT(8,NLEVEL) +
     1      VECT(6)*GRMAT(9,NLEVEL)
*
C*****  End of Code Expanded From Routine:  GROT
      ENDIF
*
* *** Compute distance to boundaries
*
      SNEXT  = STEP
      SAFETY = BIG
      INGOTO = 0
      JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
      ISH    = Q(JVO+2)
      IF (Q(JVO+3).EQ.0.) GO TO 300
      NIN = Q(JVO+3)
      IF (NIN.LT.0) GO TO 200
*
* *** Case with contents positioned
*
      ISEARC = Q(JVO+1)
      IF (ISEARC.GE.-1) GO TO 120
*
*  ** Contents are ordered by (dynamic) GSORD, select neighbours
*
      JSB = LQ(LQ(JVO-NIN-1))
      IAX = Q(JSB+1)
      NSB = Q(JSB+2)
      IF (IAX.LE.3) THEN
         CX  = XC(IAX)
         INC = SIGN(1., XC(IAX+3))
      ELSE
         CALL GFCOOR (XC, IAX, CX)
         IF (IAX.LE.5) THEN
            DR = XC(1)*XC(4) +XC(2)*XC(5)
            IF (IAX.EQ.5) DR = DR +XC(3)*XC(6)
            INC = SIGN(1., DR)
         ELSE IF (IAX.EQ.6) THEN
            INC = SIGN(1., XC(1)*XC(5)-XC(2)*XC(4))
         ELSE
            INC = SIGN(1., XC(3)*(XC(1)*XC(4)+XC(2)*XC(5))
     +                    -XC(6)*(XC(1)*XC(1)+XC(2)*XC(2)))
         ENDIF
      ENDIF
      IDIV = LOCATF (Q(JSB+3), NSB, CX)
      IF (IDIV.LT.0) IDIV = -IDIV
      IF (IAX.NE.6) THEN
         IF (IDIV.EQ.0) THEN
            IF (INC.LT.0.AND.IAX.LE.3) THEN
               SAFETY = Q(JSB+3) -CX
               GO TO 300
            ENDIF
            IDIV = 1
         ELSE IF (IDIV.EQ.NSB) THEN
            IF (INC.GT.0.AND.IAX.NE.7) THEN
               SAFETY = CX -Q(JSB+2+NSB)
               GO TO 300
            ENDIF
            IDIV = NSB -1
         ELSE
            IF (IAX.NE.7) THEN
               IF (INC.GT.0) THEN
                  SAFETY = CX -Q(JSB+2+IDIV)
               ELSE
                  SAFETY = Q(JSB+3+IDIV) -CX
               ENDIF
            ELSE
               SAFETY = 0.
            ENDIF
         ENDIF
      ELSE IF (IAX.EQ.6) THEN
         IF (IDIV.EQ.0) IDIV = NSB
         SAFETY = 0.
      ENDIF
*
      IDIVL = 0
      IDIVB = 0
      JSC0  = LQ(JVO-NIN-2)
  110 NCONT = IQ(JSC0+IDIV)
*
*  ** Loop over (selected) contents
*
      IF (NCONT.EQ.0) THEN
         IF (IDIV.EQ.IDIVL) GO TO 400
         IDIV = IDIV +INC
         IF (IAX.NE.6) GOTO 110
*      (following statement for IAX=6, when division NSB is empty)
         IF (IDIV.GT.NSB) IDIV = 1
         IF (IDIV.EQ.0) IDIV = NSB
         GO TO 110
      ELSE
         ICONT = 1
         JSCV = LQ(JSC0-IDIV)
         GO TO 140
      ENDIF
*
  120 JNEAR = LQ(JVO-NIN-1)
      IF (ISEARC.GT.0) THEN
         CALL GUNEAR (ISEARC, 2, XC, JNEAR)
         IF (IQ(JNEAR+1).EQ.0) GO TO 300
      ELSE
         IF (INFROM.GT.0) THEN
            JIN = LQ(JVO-INFROM)
            IF (LQ(JIN-1).NE.0) THEN
               JNE = LQ(JIN-1)
               IF (IQ(JNE+1).GT.1.OR.IQ(JNE+2).NE.0) JNEAR = JNE
            ENDIF
         ENDIF
      ENDIF
      JNEAR = JNEAR +1
      NNEAR = IQ(JNEAR)
      IF (IQ(JNEAR+1).NE.0) THEN
         INEAR = 1
      ELSE
         INEAR = 2
      ENDIF
*
  130 IN = IQ(JNEAR+INEAR)
      GO TO 150
*
  140 IN = IQ(JSCV+ICONT)
*
  150 IF (IN.LT.0)  GO TO 300
      JIN   = LQ(JVO-IN)
      IVOT  = Q(JIN+2)
      JVOT  = LQ(JVOLUM-IVOT)
      IROTT = Q(JIN+4)
*
      IF (BTEST(IQ(JVOT),1)) THEN
*       (case with JVOLUM structure locally developed)
         JPAR = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))
         DO 169 ILEV = NLDEV(NLEVEL), NLEVEL
            IF (IQ(JPAR+1).EQ.0) THEN
               IF (ILEV.EQ.NLEVEL) THEN
                  JPAR = LQ(JPAR-IN)
               ELSE
                  JPAR = LQ(JPAR-LINDEX(ILEV+1))
               ENDIF
               IF (JPAR.EQ.0) GO TO 170
            ELSE IF (IQ(JPAR-3).GT.1) THEN
               JPAR = LQ(JPAR-LINDEX(ILEV+1))
            ELSE
               JPAR = LQ(JPAR-1)
            ENDIF
  169    CONTINUE
         JPAR = JPAR + 5
         NPAR = IQ(JPAR)
         GO TO 180
      ENDIF
*     (normal case)
  170 NPAR = Q(JVOT+5)
      IF (NPAR.EQ.0) THEN
         JPAR = JIN +9
         NPAR = Q(JPAR)
      ELSE
         JPAR = JVOT +6
      ENDIF
*
*   * Compute distance to boundary of current content
*
C*****  Code Expanded From Routine:  GITRAN
  180 IF (IROTT .EQ. 0) THEN
         XT(1) = XC(1) - Q(5+JIN)
         XT(2) = XC(2) - Q(6+JIN)
         XT(3) = XC(3) - Q(7+JIN)
*
         XT(4) = XC(4)
         XT(5) = XC(5)
         XT(6) = XC(6)
*
      ELSE
         XL1 = XC(1) - Q(5+JIN)
         XL2 = XC(2) - Q(6+JIN)
         XL3 = XC(3) - Q(7+JIN)
         JR = LQ(JROTM-IROTT)
         XT(1) = XL1*Q(JR+1) + XL2*Q(JR+2) + XL3*Q(JR+3)
         XT(2) = XL1*Q(JR+4) + XL2*Q(JR+5) + XL3*Q(JR+6)
         XT(3) = XL1*Q(JR+7) + XL2*Q(JR+8) + XL3*Q(JR+9)
*
C*****  End of Code Expanded From Routine:  GITRAN
C*****  Code Expanded From Routine:  GRMTD
         XT(4)=XC(4)*Q(JR+1)+XC(5)*Q(JR+2)+XC(6)*Q(JR+3)
         XT(5)=XC(4)*Q(JR+4)+XC(5)*Q(JR+5)+XC(6)*Q(JR+6)
         XT(6)=XC(4)*Q(JR+7)+XC(5)*Q(JR+8)+XC(6)*Q(JR+9)
*
C*****  End of Code Expanded From Routine:  GRMTD
      ENDIF
*
      IACT = 1
      ISHT = Q(JVOT+2)
      IF (ISHT.LT.5) THEN
         IF (ISHT.EQ.1) THEN
            CALL GNOBOX (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.2) THEN
            CALL GNOTRA(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.3) THEN
            CALL GNOTRA(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE
            CALL GNOTRP (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ENDIF
      ELSE IF (ISHT.LE.10) THEN
         IF (ISHT.EQ.5) THEN
            CALL GNOTUB(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.6) THEN
            CALL GNOTUB(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.7) THEN
            CALL GNOCON(XT,Q(JPAR+1),IACT,1,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.8) THEN
            CALL GNOCON(XT,Q(JPAR+1),IACT,2,SNEXT,SNXT,SAFE)
         ELSE IF (ISHT.EQ.9) THEN
            CALL GNOSPH (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ELSE
            CALL GNOPAR (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
         ENDIF
      ELSE IF (ISHT.EQ.11) THEN
         CALL GNOPGO (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.12) THEN
         CALL GNOPCO (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.13) THEN
         CALL GNOELT (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.14) THEN
         CALL GNOHYP (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE IF (ISHT.EQ.28) THEN
         CALL GSNGTR (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE,0)
      ELSE IF (ISHT.EQ.NSCTUB) THEN
         CALL GNOCTU (XT,Q(JPAR+1),IACT,SNEXT,SNXT,SAFE)
      ELSE
         PRINT *, ' GTNEXT : No code for shape ', ISHT
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LE.MIN(SNEXT,BIG1)) THEN
         INGOTO = IN
         SNEXT  = SNXT
         IGNEXT = 1
         LQ(JGPAR-NLEVEL-1) = JPAR
         IQ(JGPAR+NLEVEL+1) = NPAR
         IF (ISEARC.EQ.-2) THEN
            IF (MOD(IQ(JSC0),2).NE.0) THEN
               IDIVB = IDIV
            ELSE
               X0(1) = XC(1) + SNXT*XC(4)
               X0(2) = XC(2) + SNXT*XC(5)
               X0(3) = XC(3) + SNXT*XC(6)
               IF (IAX.LE.3) THEN
                  IDIVB = LOCATF (Q(JSB+3), NSB, X0(IAX))
               ELSE
                  CALL GFCOOR (X0, IAX, CX)
                  IDIVB = LOCATF (Q(JSB+3), NSB, CX)
               ENDIF
               IF (IDIVB.LT.0) IDIVB = -IDIVB
               IF (IDIVB.EQ.0) THEN
                  IF (IAX.EQ.6) THEN
                     IDIVB = NSB
                  ELSE
                     IDIVB = 1
                  ENDIF
               ELSE IF (IDIVB.EQ.NSB) THEN
                  IF (IAX.NE.6) IDIVB = NSB - 1
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      IF (ISEARC.EQ.-2) THEN
         IF (ICONT.EQ.NCONT) THEN
            IF (IDIVL.EQ.0) THEN
               IF (IDIVB.NE.0) THEN
                  IF (IDIV.EQ.IDIVB) GO TO 300
                  IF (.NOT.BTEST(IQ(JVO),2)) THEN
                     IDIVL = IDIVB
                     GO TO 193
                  ENDIF
               ENDIF
*
*   *         Compute distance to boundary of current volume
*
               JPAR = LQ(JGPAR-NLEVEL)
               IACT = 2
               ISH  = Q(JVO+2)
               IF (ISH.LT.5) THEN
                  IF (ISH.EQ.1) THEN
                     CALL GNBOX (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
                  ELSE IF (ISH.EQ.2) THEN
                     CALL GNTRAP (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.3) THEN
                     CALL GNTRAP (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE
                     CALL GNTRP (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
                  ENDIF
               ELSE IF (ISH.LE.10) THEN
                  IF (ISH.EQ.5) THEN
                     CALL GNTUBE (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.6) THEN
                     CALL GNTUBE (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.7) THEN
                     CALL GNCONE (XC, Q(JPAR+1),IACT,1, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.8) THEN
                     CALL GNCONE (XC, Q(JPAR+1),IACT,2, SNEXT,SNXT,SAFE)
                  ELSE IF (ISH.EQ.9) THEN
                     CALL GNSPHR (XC, Q(JPAR+1),IACT, SNEXT, SNXT, SAFE)
                  ELSE
                     CALL GNPARA (XC, Q(JPAR+1),IACT, SNEXT, SNXT, SAFE)
                  ENDIF
               ELSE IF (ISH.EQ.12) THEN
                  CALL GNPCON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.11) THEN
                  CALL GNPGON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.13) THEN
                  CALL GNELTU (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.14) THEN
                  CALL GNHYPE (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE IF (ISH.EQ.28) THEN
                  CALL GSNGTR (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE,1)
               ELSE IF (ISH.EQ.NSCTUB) THEN
                  CALL GNCTUB (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
               ELSE
                  PRINT *, ' GTNEXT : No code for shape ', ISH
                  STOP
               ENDIF
*
               IF (SAFE.LT.SAFETY) SAFETY = SAFE
               IF (SNXT.LE.MIN(SNEXT,BIG1)) THEN
                  SNEXT  = SNXT
                  IGNEXT = 1
                  INGOTO = 0
               ENDIF
*
*   *         Check wether other pseudo-divisions have to be scanned
*
               X0(1) = XC(1) + SNXT*XC(4)
               X0(2) = XC(2) + SNXT*XC(5)
               X0(3) = XC(3) + SNXT*XC(6)
               IF (IAX.LE.3) THEN
                  IDIVL = LOCATF (Q(JSB+3), NSB, X0(IAX))
               ELSE
                  CALL GFCOOR (X0, IAX, CX)
                  IDIVL = LOCATF (Q(JSB+3), NSB, CX)
               ENDIF
               IF (IDIVL.LT.0) IDIVL = -IDIVL
               IF (IDIVL.EQ.0) THEN
                  IF(IAX.EQ.6)THEN
                     IDIVL=NSB
                  ELSE
                     IDIVL=1
                  ENDIF
               ELSEIF (IDIVL.EQ.NSB)THEN
                  IF(IAX.NE.6)IDIVL=NSB-1
               ENDIF
            ELSE
               IF (IDIV.EQ.IDIVB)   GO TO 400
            ENDIF
  193       IF ((IDIV-IDIVL)*INC.GE.0) GO TO 400
            IDIV = IDIV +INC
            GO TO 110
         ELSE
            ICONT = ICONT +1
            GO TO 140
         ENDIF
      ELSE
         IF (INEAR.EQ.NNEAR) GO TO 300
         INEAR = INEAR +1
         GO TO 130
      ENDIF
*
* ***    Case of volume incompletely divided
*
  200 JDIV   = LQ(JVO-1)
      IAXIS  = Q(JDIV+1)
      IVOT   = Q(JDIV+2)
      JVOT   = LQ(JVOLUM-IVOT)
      ISHT   = Q(JVOT+2)
*
*  ** Get the division parameters
*
      IF (NLEVEL.LT.NLDEV(NLEVEL)) THEN
         JPARM = 0
      ELSE
*        (case with JVOLUM structure locally developed)
         JPARM = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))
         IF (NLEVEL.EQ.NLDEV(NLEVEL)) GO TO 215
         DO 210 ILEV = NLDEV(NLEVEL), NLEVEL-1
            IF (IQ(JPARM+1).EQ.0) THEN
               JPARM = LQ(JPARM-LINDEX(ILEV+1))
               IF (JPARM.EQ.0) GO TO 215
            ELSE IF (IQ(JPARM-3).GT.1) THEN
               JPARM = LQ(JPARM-LINDEX(ILEV+1))
            ELSE
               JPARM = LQ(JPARM-1)
            ENDIF
            IF (ILEV.EQ.NLEVEL-1) THEN
               NDIV = IQ(JPARM+1)
               ORIG =  Q(JPARM+2)
               SDIV =  Q(JPARM+3)
            ENDIF
  210    CONTINUE
         GO TO 220
      ENDIF
*     (normal case)
  215 NDIV = Q(JDIV+3)
      ORIG = Q(JDIV+4)
      SDIV = Q(JDIV+5)
*
*  ** Look at the first and the last divisions only
*
  220 IDT  = IDTYP(IAXIS, ISH)
      IF (IDT.EQ.1) THEN
         IN2 = 0
         IF (XC(IAXIS).LT.ORIG) THEN
            IN  = 1
         ELSE
            IN  = NDIV
         ENDIF
      ELSE IF (IDT.EQ.2) THEN
         R   = XC(1)**2 + XC(2)**2
         IF (ISH.EQ.9) R = R + XC(3)**2
         R   = SQRT(R)
         IN2 = 0
         IF (ISH.EQ.5.OR.ISH.EQ.6.OR.ISH.EQ.9) THEN
            IF (R.LT.ORIG) THEN
               IN  = 1
            ELSE
               IN  = NDIV
            ENDIF
         ELSE
**          PRINT *, ' GTNEXT : Partially divided ',ISH,IAXIS
            IN  = 1
            IF (NDIV.GT.1) IN2 = NDIV
         ENDIF
      ELSE IF (IDT.EQ.4) THEN
         IN2 = 0
         RXY = XC(1)**2 + XC(2)**2
         RXY = SQRT(RXY)
         IF (XC(3).NE.0.0) THEN
            THET = RADDEG * ATAN (RXY/XC(3))
            IF (THET.LT.0.0) THET = THET + 180.0
         ELSE
            THET = 90.
         ENDIF
         IF (THET.LE.ORIG) THEN
            IN  = 1
         ELSE
            IN  = NDIV
         ENDIF
      ELSE
         IN2 = 0
         IF (ISH.EQ.5.OR.ISH.EQ.7) THEN
            IN  = 1
            IF (NDIV.GT.1) IN2 = NDIV
         ELSE
            IF (XC(1).NE.0.0.OR.XC(2).NE.0.0) THEN
               PHI = RADDEG * ATAN2 (XC(2), XC(1))
            ELSE
               PHI = 0.0
            ENDIF
            IF (ISH.EQ.6.OR.ISH.EQ.8) THEN
               IF (PHI.LT.ORIG) THEN
                  IN  = 1
               ELSE
                  IN  = NDIV
               ENDIF
            ELSE
               IN  = 1
               IF (NDIV.GT.1) IN2 = NDIV
            ENDIF
         ENDIF
      ENDIF
*
  225 IF (IDT.EQ.1) THEN
         X0(1) = 0.0
         X0(2) = 0.0
         X0(3) = 0.0
         X0(IAXIS) = ORIG + (IN - 0.5) * SDIV
         IF (ISH.EQ.4.OR.(ISH.EQ.10.AND.IAXIS.NE.1)) THEN
            CALL GCENT (IAXIS, X0)
         ENDIF
         XT(1) = XC(1) - X0(1)
         XT(2) = XC(2) - X0(2)
         XT(3) = XC(3) - X0(3)
         XT(4) = XC(4)
         XT(5) = XC(5)
         XT(6) = XC(6)
      ELSE IF (IDT.EQ.3) THEN
         PH0  = DEGRAD * (ORIG + (IN - 0.5) * SDIV)
         CPHR = COS(PH0)
         SPHR = SIN(PH0)
         XT(1) = XC(1)*CPHR + XC(2)*SPHR
         XT(2) = XC(2)*CPHR - XC(1)*SPHR
         XT(3) = XC(3)
         XT(4) = XC(4)*CPHR + XC(5)*SPHR
         XT(5) = XC(5)*CPHR - XC(4)*SPHR
         XT(6) = XC(6)
      ELSE
         DO 234 I = 1, 6, 2
            XT(I) = XC(I)
            XT(I+1) = XC(I+1)
  234    CONTINUE
      ENDIF
*
      IF (JPARM.NE.0) THEN
         IF (IQ(JPARM-3).GT.1) THEN
            JPAR = LQ(JPARM-IN)
         ELSE
            JPAR = LQ(JPARM-1)
         ENDIF
         JPAR = JPAR + 5
      ELSE
         JPAR = JVOT + 6
      ENDIF
*
      IACT = 1
      IF (ISHT.LT.5) THEN
         IF (ISHT.EQ.1) THEN
            CALL GNOBOX (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.2) THEN
            CALL GNOTRA (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.3) THEN
            CALL GNOTRA (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNOTRP (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISHT.LE.10) THEN
         IF (ISHT.EQ.5) THEN
            CALL GNOTUB (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.6) THEN
            CALL GNOTUB (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.7) THEN
            CALL GNOCON (XT, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.8) THEN
            CALL GNOCON (XT, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISHT.EQ.9) THEN
            CALL GNOSPH (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNOPAR (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISHT.EQ.11) THEN
         CALL GNOPGO (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.12) THEN
         CALL GNOPCO (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.13) THEN
         CALL GNOELT (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISHT.EQ.28) THEN
         CALL GSNGTR (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE,0)
      ELSE IF (ISHT.EQ.NSCTUB) THEN
         CALL GNOCTU (XT, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE
         PRINT *, ' GTNEXT : No code for shape ', ISHT
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LE.MIN(SNEXT,BIG1)) THEN
         SNEXT  = SNXT
         IGNEXT = 1
      ENDIF
*
      IF (IN2.NE.0) THEN
         IF (IN2.NE.IN) THEN
            IN  = IN2
            GO TO 225
         ENDIF
      ENDIF
*       (later, this section only for concave volumes if INGOTO >0
  300 IACT = 1
      IF (IGNEXT.NE.0) THEN
         IF (.NOT.BTEST(IQ(JVO),2)) IACT = 0
      ENDIF
      JPAR = LQ(JGPAR-NLEVEL)
      IF (ISH.LT.5) THEN
         IF (ISH.EQ.1) THEN
            CALL GNBOX (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE )
         ELSE IF (ISH.EQ.2) THEN
            CALL GNTRAP (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.3) THEN
            CALL GNTRAP (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNTRP (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISH.LE.10) THEN
         IF (ISH.EQ.5) THEN
            CALL GNTUBE (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.6) THEN
            CALL GNTUBE (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.7) THEN
            CALL GNCONE (XC, Q(JPAR+1), IACT, 1, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.8) THEN
            CALL GNCONE (XC, Q(JPAR+1), IACT, 2, SNEXT, SNXT, SAFE)
         ELSE IF (ISH.EQ.9) THEN
            CALL GNSPHR (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ELSE
            CALL GNPARA (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
         ENDIF
      ELSE IF (ISH.EQ.12) THEN
         CALL GNPCON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.11) THEN
         CALL GNPGON (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.13) THEN
         CALL GNELTU (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.14) THEN
         CALL GNHYPE (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE IF (ISH.EQ.28) THEN
         CALL GSNGTR (XC,Q(JPAR+1),  IACT, SNEXT, SNXT, SAFE,1)
      ELSE IF (ISH.EQ.NSCTUB) THEN
         CALL GNCTUB (XC, Q(JPAR+1), IACT, SNEXT, SNXT, SAFE)
      ELSE
         PRINT *, ' GTNEXT : No code for shape ', ISH
         STOP
      ENDIF
*
      IF (SAFE.LT.SAFETY) SAFETY = SAFE
      IF (SNXT.LE.MIN(SNEXT,BIG1)) THEN
         SNEXT  = SNXT
         IGNEXT = 1
         INGOTO = 0
      ENDIF
*
  400 IF (GONLY(NLEVEL).EQ.0.) THEN
*
* ***   Case of a 'NOT ONLY' volume -> step search
*
         SAFETY = 0.
         EPSI2  = 0.5*EPSIL
         ST     = SNEXT -EPSI2
         IF (ST.LE.0) GO TO 900
         EPSI3  = 10.*EPSIL
         IF (ST.LE.EPSI3) THEN
            NN = 1
         ELSE
            NN = ST/EPSI3 +1
            ST = ST/NN
         ENDIF
*
         NBIN = 0
         SN   = 0.
  420    SN   = SN +ST
         XT(1) = VECT(1) + SN*VECT(4)
         XT(2) = VECT(2) + SN*VECT(5)
         XT(3) = VECT(3) + SN*VECT(6)
*
         INGOTO = 0
         CALL GINVOL (XT, ISAME)
         IF (ISAME.EQ.0) THEN
            IF (ST.LE.EPSI2) GO TO 490
            SN   = SN -ST
            ST   = 0.5*ST
            NBIN = 1
            GO TO 420
         ENDIF
*
         IF (NBIN.NE.0) THEN
            IF (ST.LT.EPSI2) THEN
               ST = EPSI2
            ELSE
               ST = 0.5*ST
            ENDIF
            GO TO 420
         ENDIF
         NN = NN -1
         IF (NN.GT.0) GO TO 420
         GO TO 495
*
  490    IF (SN.LT.SNEXT) THEN
            INGOTO = -1
            SNEXT  = SN
            IGNEXT = 1
            GO TO 900
         ENDIF
*
  495    NLEVIN = NLEVEL
      ENDIF
*
* *** Attempt to rescue negative SNXT due to rounding errors
*
  900 IF (SNEXT.LT.0.) THEN
CCC debug
         IF (ISWIT(9).EQ.123456789) THEN
            PRINT *,' GTNEXT : SNEXT,SAFETY,INGOTO=',SNEXT,SAFETY,INGOTO
            CALL GPCXYZ
         ENDIF
CCC
         SAFETY = 0.
         SNEXT  = 0.
         IGNEXT = 1
         INGOTO = 0
      ENDIF
*
      IF(JGSTAT.NE.0) CALL GFSTAT(3)
*                                                             END GTNEXT
      END
CDECK  ID>, GGDSPE. 
*CMZ :  3.21/02 29/03/94  15.41.28  by  S.Giani
*-- Author :
      SUBROUTINE GGDSPE (JVO, NPAR, PAR, NL, NDIV, ORIG, STEP)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GGDSPE (JVO,NPAR,PAR,NL*,NDIV*,ORIG*,STEP*)           *
C.    *                                                                *
C.    *   Computes the actual division parameters of the mother volume *
C.    *    at address JVO with actual parameters in NPAR, PAR          *
C.    *   Returns the division parameters NDIV, ORIG, STEP and the     *
C.    *    number of different division cells NL                       *
C.    *                                                                *
C.    *   Called by : GGDVLP                                           *
C.    *   Author    : S.Banerjee                                       *
C.    *               (Original algorithms of A.McPherson)             *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
C.
      PARAMETER (NPAMAX=50)
C.
      DIMENSION PAR(*)
C.
      REAL      PARM(NPAMAX)
      INTEGER   IDTYP(12)
      SAVE IDTYP
      DATA      IDTYP / 1, 1, 1, 1, 2, 2, 3, 3, 4, 1, 6, 6/
C.
C.    ------------------------------------------------------------------
*
* *** Get the division parameters from JVOLUM structure and check if
*         these have to be developed
*
      JIN   = LQ(JVO-1)
      IAXIS = Q(JIN+1)
      NDIV  = Q(JIN+3)
      ORIG  = Q(JIN+4)
      STEP  = Q(JIN+5)
*
      ISHM  = Q(JVO+2)
      IDTY  = IDTYP(ISHM)
      NPARM = Q(JVO+5)
      IF (NPARM.GT.0) THEN
         CALL UCOPY (Q(JVO+7), PARM, NPARM)
      ELSE
         NPARM = NPAR
         CALL VFILL (PARM, NPARM, -1.0)
      ENDIF
*
* *** Find the actual division parameters
*
      IF (IDTY.EQ.1) THEN
*
* BOX, TRD1, TRD2, TRAP, PARA
*
         IF (ISHM.EQ.4) THEN
            IPAR = 1
         ELSE IF (ISHM.EQ.10) THEN
            IPAR = IAXIS
         ELSE
            IPAR = IAXIS + ISHM - 1
         ENDIF
         IF (PARM(IPAR).LT.0.0) ORIG = -PAR(IPAR)
         IF (STEP.LE.0.0) THEN
            STEP = (PAR(IPAR) - ORIG) / NDIV
         ELSE IF (NDIV.LE.0) THEN
            NDIV = (PAR(IPAR) - ORIG + 0.001) / STEP
         ENDIF
         IF (PARM(IPAR).LT.0.0) ORIG = -0.5 * STEP * NDIV
         IF (ISHM.EQ.1.OR.ISHM.EQ.10.OR.(ISHM.EQ.2.AND.IAXIS.EQ.2)) THEN
            NL = 1
         ELSE
            NL = NDIV
         ENDIF
*
      ELSE IF (IDTY.EQ.4) THEN
*
* SPHE
*
         IF (IAXIS.EQ.1.OR.IAXIS.EQ.2) THEN
            IAX1 = 2*IAXIS - 1
            IAX2 = IAX1 + 1
            IF (PARM(IAX1).LT.0.0) ORIG = PAR(IAX1)
            IF (STEP.LE.0.0) THEN
               STEP = (PAR(IAX2) - ORIG) / NDIV
            ELSE IF (NDIV.LE.0) THEN
               NDIV = (PAR(IAX2) - ORIG + 0.001) / STEP
            ENDIF
            IF (PARM(IAX1).LT.0.0) ORIG = 0.5*(ORIG+PAR(IAX2)-STEP*NDIV)
            NL = NDIV
         ELSE
            IF (STEP.LE.0.0.OR.NDIV.LE.0) THEN
               ORIG    = PAR(5)
               DP = PAR(6) - PAR(5)
               IF (DP.LT.0.0) DP = DP + 360.0
               IF (ORIG.LT.PAR(5)) ORIG = ORIG + 360.0
               IF (ORIG-PAR(5).GT.DP) THEN
                  PMIN = PAR(5)
                  GO TO 910
               ENDIF
               DP = PAR(6) - ORIG
               IF (DP.LT.0.0) DP = DP + 360.0
               IF (NDIV.LE.0) THEN
                  NDIV = (DP + 0.001) /STEP
                  IF (NDIV.LE.0) GO TO 920
               ELSE
                  STEP = DP / NDIV
               ENDIF
            ENDIF
            NL = 1
         ENDIF
*
      ELSE IF (IDTY.EQ.2.OR.IDTY.EQ.3.OR.IDTY.EQ.6) THEN
*
* TUBE, TUBS, CONE, CONS, PGON, PCON
*
         IF (IAXIS.EQ.3) THEN
            IF (IDTY.NE.6) THEN
               IF (IDTY.EQ.2) THEN
                  IPAR = 3
               ELSE
                  IPAR = 1
               ENDIF
               IF (PARM(IPAR).LT.0.0) ORIG = -PAR(IPAR)
               IF (STEP.LE.0.0) THEN
                  STEP = (PAR(IPAR) - ORIG) / NDIV
               ELSE IF (NDIV.LE.0) THEN
                  NDIV = (PAR(IPAR) - ORIG + 0.001) / STEP
               ENDIF
               IF (PARM(IPAR).LT.0.0) ORIG = -0.5 * STEP * NDIV
               IF (IDTY.EQ.2) THEN
                  NL = 1
               ELSE
                  NL = NDIV
               ENDIF
            ELSE
               IF (ISHM.EQ.11) THEN
                  NZ = PAR(4)
               ELSE
                  NZ = PAR(3)
               ENDIF
               IF (NDIV.LE.0.OR.STEP.LE.0.0.OR.NZ.GT.0) GO TO 930
               NL = NDIV
            ENDIF
*
         ELSE IF (IAXIS.EQ.2) THEN
            IF (STEP.LE.0.0.OR.NDIV.LE.0) THEN
               IF (ISHM.EQ.5.OR.ISHM.EQ.7) THEN
                  PMIN = ORIG
                  PMAX = ORIG + 360.0
                  DP   = 360.0
               ELSE IF (ISHM.EQ.6.OR.ISHM.EQ.8) THEN
                  PMIN = PAR(NPAR-1)
                  PMAX = PAR(NPAR)
                  DP   = PMAX - PMIN
               ELSE
                  PMIN = PAR(1)
                  DP   = PAR(2)
                  PMAX = PMIN + DP
               ENDIF
               IF (DP.LT.0.0) DP = DP + 360.0
               IF (ORIG.LT.PMIN) ORIG = ORIG + 360.0
               IF (ORIG-PMIN.GT.DP) GO TO 910
               DP = PMAX - ORIG
               IF (DP.LT.0.0) DP = DP + 360.0
               IF (NDIV.LE.0) THEN
                  NDIV = (DP + 0.001) / STEP
                  IF (NDIV.LE.0) GO TO 920
               ELSE
                  STEP = DP / NDIV
               ENDIF
            ENDIF
            NL = 1
*
         ELSE
            IF (IDTY.NE.6) THEN
               IF (IDTY.EQ.2) THEN
                  IAX1 = 1
                  IAX2 = 2
               ELSE
                  IAX1 = 2
                  IAX2 = 3
               ENDIF
               IF (PARM(IAX1).LT.0.0) ORIG = PAR(IAX1)
               IF (STEP.LE.0.0) THEN
                  STEP = (PAR(IAX2) - ORIG) / NDIV
               ELSE IF (NDIV.LE.0) THEN
                  NDIV = (PAR(IAX2) - ORIG + 0.001) / STEP
               ENDIF
               IF (PARM(IAX1).LT.0.0)
     +            ORIG = ORIG + 0.5 * (PAR(IAX2)-ORIG-STEP*NDIV)
               NL = NDIV
            ELSE
               IF (STEP.LE.0.0.OR.NDIV.LE.0) THEN
                  IF (ISHM.EQ.11) THEN
                     NZ = PAR(4)
                  ELSE
                     NZ = PAR(3)
                  ENDIF
                  GO TO 930
               ENDIF
               NL = NDIV
            ENDIF
         ENDIF
*
      ELSE
         GO TO 900
      ENDIF
      GO TO 999
*
  900 WRITE (CHMAIL, 1001) ISHM, IAXIS
      GO TO 990
*
  910 WRITE (CHMAIL, 1002) ISHM, IAXIS, PMIN, DP, ORIG
      GO TO 990
*
  920 WRITE (CHMAIL, 1003) ISHM, IAXIS, NDIV, DP, STEP
      GO TO 990
*
  930 WRITE (CHMAIL, 1004) ISHM, IAXIS, NDIV, NZ, STEP
*
  990 CALL GMAIL (0, 0)
      IEORUN = 1
*
 1001 FORMAT (' GGDSPE : Invalid call ISHM,IAXIS=',2I5)
 1002 FORMAT (' GGDSPE : Error ISHM,IAXIS,PMIN,DP,ORIG=',2I5,3G12.4)
 1003 FORMAT (' GGDSPE : Error ISHM,IAXIS,NDIV,DP,STEP=',3I5,2G12.4)
 1004 FORMAT (' GGDSPE : Error ISHM,IAXIS,NDIV,NZ,STEP=',4I5,G12.4)
*                                                             END GGDSPE
  999 END
*
CDECK  ID>, GSDVN.  
*CMZ :  3.21/02 29/03/94  15.41.30  by  S.Giani
*-- Author :
      SUBROUTINE GSDVN(KNAME,MOTHER,NDIV,IAXIS)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine divides MOTHER into NDIV divisions called NAME   *
C.    *       along axis number IAXIS.                                 *
C.    *          JVO=Pointer to MOTHER volume                          *
C.    *          JDIV=LQ(JVO-1)                                        *
C.    *                                                                *
C.    *            Q(JDIV+1)=IAXIS                                     *
C.    *            Q(JDIV+2)=Volume number.                            *
C.    *            Q(JDIV+3)=NDIV                                      *
C.    *            Q(JDIV+4)=Lowest coord of slices.                   *
C.    *            Q(JDIV+5)=Step size in coordinates.                 *
C.    *                                                                *
C.    *    ==>Called by :  <USER>, GEDITV                              *
C.    *         Authors R.Brun,   A.McPherson  *********               *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCDRAW/NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB(16),
     + GRMAT0(10),GTRAN0(3),IDRNUM,GSIN(41),GCOS(41),SINPSI,COSPSI,
     + GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV,NGVIEW,
     + ICUTFL,ICUT,CTHETA,CPHI,DCUT,NSURF,ISURF,
     + GZUA,GZVA,GZUB,GZVB,GZUC,GZVC,PLTRNX,PLTRNY,
     + LINATT,LINATP,ITXATT,ITHRZ,IPRJ,DPERS,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN,
     + ZZFU,ZZFV,MYISEL,
     + DDUMMY(15)
C
      INTEGER NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB,IDRNUM,NGVIEW,ICUTFL,ICUT,NSURF,ISURF,LINATT,LINATP,
     + ITXATT,ITHRZ,IPRJ,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN
      REAL GRMAT0,GTRAN0,GSIN,GCOS,SINPSI,COSPSI,GTHETA,GPHI,GPSI,
     + GU0,GV0,GSCU,GSCV,CTHETA,CPHI,DCUT,GZUA,GZVA,GZUB,GZVB,GZUC,
     + GZVC,PLTRNX,PLTRNY,DPERS,DDUMMY
      PARAMETER ( NSBOX=1,  NSTRD1=2, NSTRD2=3, NSTRAP=4, NSTUBE=5,
     +  NSTUBS=6, NSCONE=7, NSCONS=8, NSSPHE=9, NSPARA=10,NSPGON=11,
     +  NSPCON=12,NSELTU=13,NSHYPE=14,NSGTRA=28, NSCTUB=29 )
      CHARACTER*4 KNAME,MOTHER
      DIMENSION PAR(50),PARM(50),ATT(20)
      SAVE ATT
      DATA ATT /1.,1.,1.,1.,1.,15*0./
C.
C.    ------------------------------------------------------------------
C.
C              Check if volume master bank exists.
C
      CALL UCTOH(KNAME,NAME,4,4)
      IF(JVOLUM.GT.0)GO TO 10
      WRITE(CHMAIL,1000)
      CALL GMAIL(0,0)
      GO TO 99
C
C              Check if MOTHER volume exists.
C
  10  CALL GLOOK(MOTHER,IQ(JVOLUM+1),NVOLUM,IVO)
      IF(IVO.GT.0)GO TO 20
      WRITE(CHMAIL,1100)MOTHER
      CALL GMAIL(0,0)
      GO TO 99
C
C              Check if NAME volume exists.
C
  20  CALL GLOOK(KNAME,IQ(JVOLUM+1),NVOLUM,IN)
      IF(IN.LE.0)GO TO 50
      WRITE(CHMAIL,2000)NAME
      CALL GMAIL(0,0)
      GO TO 99
C
C              Check if MOTHER is not divided.
C
  50  JVO=LQ(JVOLUM-IVO)
      NIN=Q(JVO+3)
      IF(NIN.EQ.0)GO TO 60
      WRITE(CHMAIL,4000)MOTHER
      CALL GMAIL(0,0)
      GO TO 99
C
C              Check validity of axis value.
C
  60  IF(IAXIS.GT.0.AND.IAXIS.LT.4)GO TO 70
      WRITE(CHMAIL,5000)IAXIS
      CALL GMAIL(0,0)
      GO TO 99
C
C              Check validity of NDIV
C
  70  IF(NDIV.GT.0)GO TO 80
      WRITE(CHMAIL,6000)NDIV
      CALL GMAIL(0,0)
      GO TO 99
C
C               Create bank to store division parameters.
C
  80  CALL MZBOOK(IXCONS,JDIV,JVO,-1,'VODI',0,0,6,3,0)
      IF(IEOTRI.NE.0)GO TO 95
      IQ(JDIV-5)=IVO
C
C               Now store parameters into bank area.
C
  90  Q(JDIV+1)=IAXIS
      Q(JDIV+2)=NVOLUM+1
      Q(JDIV+3)=NDIV
      Q(JVO+3)=-1
      IVOM= IVO
      NWM = IQ(JVO-1)
      NW  = NWM
      ISH = Q(JVO+2)
C
C               Bit to allow division of objects defined
C               by GSPOSP.
C
      C0=0.0
      STEP=0.0
      NPAR=Q(JVO+5)
      NATT=Q(JVO+6)
      CALL UCOPY(Q(JVO+NPAR+7),ATT,NATT)
      IF(NPAR.LE.0) GO TO 230
C
      CALL GFIPAR(JVO,0,0,NPAR,NATT,PAR,ATT)
      CALL UCOPY(PAR,PARM,NPAR)
C
C              Find and store start and step.
C
      IF(ISH.NE.1) GO TO 100
C
C               Box.
C
      STEP=-1.0
      PAR(IAXIS)=-1.0
      IF(PARM(IAXIS).LT.0.0) GO TO 230
      C0  =-PARM(IAXIS)
      STEP=PARM(IAXIS)*2.0/NDIV
      PAR(IAXIS)=STEP/2.
      GO TO 230
C
  100 CONTINUE
      IF(ISH.NE.2) GO TO 110
C
C              Trapezoid with only X thickness varying with Z.
C
      IF(IAXIS.EQ.1) GO TO 900
      PAR(1)=-1.
      PAR(2)=-1.
      STEP=-1.0
      PAR(IAXIS+1)=-1.0
      IF(PARM(IAXIS+1).LT.0.0) GO TO 230
      C0  =-PARM(IAXIS+1)
      STEP=PARM(IAXIS+1)*2.0/NDIV
      PAR(IAXIS+1)=STEP/2.
      GO TO 230
C
  110 CONTINUE
      IF(ISH.NE.3) GO TO 120
C
C              Trapezoid with both X and Y thicknesses varying with
C              Z
C
      IF(IAXIS.NE.3) GO TO 900
      PAR(1)=-1.
      PAR(2)=-1.
      PAR(3)=-1.
      PAR(4)=-1.
      STEP=-1.0
      PAR(5)=-1.0
      IF(PARM(5).LT.0.0) GO TO 230
      C0  =-PARM(5)
      STEP=PARM(5)*2.0/NDIV
      PAR(5)=STEP/2.
      GO TO 230
C
  120 CONTINUE
      IF(ISH.NE.4) GO TO 125
      IF(IAXIS.NE.3) GO TO 126
      PAR(1)=-1.
      PAR(4)=-1.
      PAR(5)=-1.
      PAR(6)=-1.
      PAR(8)=-1.
      PAR(9)=-1.
      PAR(10)=-1.
      STEP=-1.0
      IF(PARM(1).LT.0.0) GO TO 230
      C0=-PARM(1)
      STEP=PARM(1)*2.0/NDIV
      PAR(1)=STEP*0.5
C
      GO TO 230
C
  126 IF(IAXIS.NE.2) GO TO 900
      IF(MOD(PARM(3),180.).EQ.0.) GO TO 127
      WRITE(CHMAIL,10100)
10100 FORMAT(' Division of TRAP ',A4,
     +    ' along Y only possible when PHI=0,180')
      CALL GMAIL(0,0)
      GOTO 99
  127 IF(PARM(4).EQ.PARM(8))  GO TO 128
      WRITE(CHMAIL,10200)
10200 FORMAT(' Division of TRAP ',A4,
     +    ' along Y only possible when H1=H2')
      CALL GMAIL(0,0)
      GOTO 99
  128 CONTINUE
      STEP = -1.
      IF(PARM(4).LT.0.0) GO TO 230
      C0=-PARM(4)
      STEPH = PARM(4)/NDIV
      PAR(4) = STEPH
      PAR(5) = -1.
      PAR(6) = -1.
      PAR(8) = STEPH
      PAR(9) = -1.
      PAR(10) = -1.
      STEP = 2.*STEPH
C
      GO TO 230
C
  125 CONTINUE
      IF(ISH.NE.5.AND.ISH.NE.6.AND.ISH.NE.NSCTUB) GO TO 160
C
C              Tube, tube segment or cut tube.
C
      IF(IAXIS.NE.3) GO TO 130
      STEP=-1.0
      PAR(3)=-1.0
      IF(PARM(3).LT.0.0) GO TO 230
      C0  =-PARM(3)
      STEP=PARM(3)*2.0/NDIV
      PAR(3)=STEP/2.
      GO TO 230
C
  130 CONTINUE
      IF(IAXIS.NE.1) GO TO 140
      PAR(1)=-1.
      PAR(2)=-1.
      STEP=-1.0
      IF(PARM(1).LT.0.0) GO TO 230
      C0  =PARM(1)
      IF(PARM(2).LT.0.0) GO TO 230
      STEP=(PARM(2)-PARM(1))/NDIV
      GO TO 230
C
  140 CONTINUE
      IF(ISH.EQ.6) GO TO 150
      NW=NW+2
      ISH=6
      C0  =0.0
      STEP=360.0/NDIV
      NPAR=5
      PAR(4)=-STEP/2.
      PAR(5)=STEP/2.
      GO TO 230
C
  150 CONTINUE
      DP=PAR(5)-PAR(4)
      IF(DP.LT.0.0) DP=DP+360.0
      C0  =PAR(4)
      STEP=DP/NDIV
      PAR(4)=-STEP/2.
      PAR(5)=STEP/2.
      GO TO 230
C
  160 CONTINUE
C
      IF(ISH.NE.7.AND.ISH.NE.8) GO TO 190
      IF(IAXIS.EQ.1) GO TO 165
      IF(IAXIS.NE.3) GO TO 170
C
      STEP=-1.0
      PAR(1)=-1.0
      PAR(2)=-1.0
      PAR(3)=-1.0
      PAR(4)=-1.0
      PAR(5)=-1.0
      IF(PARM(1).LT.0.0) GO TO 230
      C0=-PARM(1)
      STEP=PARM(1)*2.0/NDIV
      PAR(1)=STEP*0.5
      GO TO 230
C
  165 CONTINUE
C
      STEP=-1.0
      PAR(2)=-1.0
      PAR(3)=-1.0
      PAR(4)=-1.0
      PAR(5)=-1.0
      IF(PARM(2).LT.0.0) GO TO 230
      C0=PARM(2)
      STEP=(PARM(3)-PARM(2))/NDIV
      GO TO 230
C
  170 CONTINUE
C
      IF(ISH.EQ.8) GO TO 180
      NW=NW+2
      ISH=8
      C0  =0.0
      STEP=360.0/NDIV
      NPAR=7
      PAR(6)=-STEP/2.
      PAR(7)=STEP/2.
      GO TO 230
C
  180 CONTINUE
      DP=PAR(7)-PAR(6)
      IF(DP.LT.0.0) DP=DP+360.0
      C0  =PAR(6)
      STEP=DP/NDIV
      PAR(6)=-STEP/2.
      PAR(7)=STEP/2.
      GO TO 230
C
  190 CONTINUE
      IF(ISH.NE.9) GO TO 200
      IF(IAXIS.NE.1) GO TO 195
      PAR(1)=-1.0
      PAR(2)=-1.0
      C0    = PARM(1)
      STEP = (PARM(2)-PARM(1))/NDIV
  195 CONTINUE
      IF(IAXIS.NE.2) GO TO 196
      WRITE(CHMAIL,8102)
      CALL GMAIL(0,0)
      GOTO 99
C
  196 CONTINUE
      IF(IAXIS.NE.3) GO TO 230
      C0=PAR(5)
      DP=PAR(6)-PAR(5)
      IF(DP.LE.0.0) DP=DP+360.0
      STEP=DP/NDIV
      PAR(3)=-1.
      PAR(4)=-1.
      PAR(5)=-0.5*STEP
      PAR(6)=0.5*STEP
      GO TO 230
C
  200 CONTINUE
C
      IF(ISH.NE.10) GO TO 210
C
C              Parallelipiped.
C
      C0  =-PAR(IAXIS)
      STEP=-2.0*C0/NDIV
      PAR(IAXIS)=STEP/2.
      GO TO 230
C
  210 CONTINUE
      IF(ISH.GT.12) GO TO 900
      IF(IAXIS.EQ.1) GO TO 230
      IF(IAXIS.EQ.2) GO TO 220
C
      IPNZ=4
      IF(ISH.EQ.12) IPNZ=3
      IF(PAR(IPNZ).NE.2) GO TO 910
C
      ZH=PAR(IPNZ+4)
      ZL=PAR(IPNZ+1)
      STEP=(ZH-ZL)/NDIV
      C0=ZL
      PAR(IPNZ+4)=STEP*0.5
      PAR(IPNZ+1)=-PAR(IPNZ+4)
      PAR(IPNZ+2)=-1.
      PAR(IPNZ+3)=-1.
      PAR(IPNZ+5)=-1.
      PAR(IPNZ+6)=-1.
C
      GO TO 230
  220 CONTINUE
C
      NDV=NDIV
      IF(ISH.EQ.11) NDV=PAR(3)
      Q(JDIV+3)=NDV
      C0=PAR(1)
      STEP=PAR(2)/NDV
      PAR(1)=-STEP*0.5
      PAR(2)=STEP
      IF(ISH.EQ.11)PAR(3)=1.
C
  230 CONTINUE
C
C                Now create the volume for division.
C
      Q(JDIV+4)=C0
      Q(JDIV+5)=STEP
      NVOLUM=NVOLUM+1
      NVOL  =IQ(JVOLUM-2)
      IF(NVOLUM.GT.NVOL)CALL MZPUSH(IXCONS,JVOLUM,50,50,'I')
      CALL MZBOOK(IXCONS,JVO,JVOLUM,-NVOLUM,'VOL1',50,50,NW,3,0)
      IF(IEOTRI.NE.0)GO TO 95
      IQ(JVOLUM+NVOLUM)=NAME
C
C              Copy parameters in data area.
C
      JVOM=LQ(JVOLUM-IVOM)
      CALL UCOPY(Q(JVOM+1),Q(JVO+1),NWM)
      IF(NPAR.GT.0) CALL GSIPAR(JVO,0,NPAR,NATT,PAR,ATT)
      Q(JVO+2)=ISH
      Q(JVO+3)=0.
      GO TO 99
C
  900 CONTINUE
C
C        Divide action not supported.
C
      WRITE(CHMAIL,8000)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,8001) ISH,IAXIS
      CALL GMAIL(0,0)
C
      GO TO 99
C
  910 CONTINUE
C
C          Trying to divide multi Z sector shape along Z.
C
      WRITE(CHMAIL,8100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,8101) ISH,IAXIS,IPNZ,INT(PAR(IPNZ))
      CALL GMAIL(0,0)
C
      GO TO 99
C
C              Not enough space.
C
  95  WRITE(CHMAIL,7000)NAME,MOTHER
      CALL GMAIL(0,0)
C
  99  CONTINUE
 1000 FORMAT(' ***** GSDVN CALLED AND NO VOLUMES DEFINED *****')
 1100 FORMAT(' ***** GSDVN MOTHER VOLUME ',A4,' DOES NOT EXIST *****')
 2000 FORMAT(' ***** GSDVN VOLUME ',A4,' ALREADY EXISTS *****')
 3000 FORMAT(' ***** GSDVN ROTATION MATRIX',I5,' DOES NOT EXISTS *****')
 4000 FORMAT(' ***** GSDVN MOTHER ',A4,' ALREADY DIVIDED *****')
 5000 FORMAT(' ***** GSDVN BAD AXIS VALUE ',I5,' *****')
 6000 FORMAT(' ***** GSDVN BAD NUMBER OF DIVISIONS ',I5,' *****')
 7000 FORMAT(' ***** GSDVN NOT ENOUGH SPACE TO STORE DIVISIONS ',
     +       ' IN ',A4,' *****')
 8000 FORMAT(' DIVIDE ACTION REQUESTED NOT SUPPORTED AT PRESENT.')
 8001 FORMAT(' ISH =',I5,' IAXIS =',I5)
 8100 FORMAT(' ATTEMPT TO DIVIDE MULTI Z SECTOR SHAPE ALONG Z.')
 8101 FORMAT(' ISH =',I5,' IAXIS =',I5,' NZ (THE',I3,'TH PAR) IS',I5)
 8102 FORMAT(' DIVISION OF A SPHERE ALONG AXIS 2 NOT SUPPORTED')
      END
CDECK  ID>, GPGHEI. 
*CMZ :  3.21/02 29/03/94  15.41.37  by  S.Giani
*-- Author :
      SUBROUTINE GPGHEI
C
C *** COMPUTE DISTANCE TO NEXT HADRONIC INTERACTION POINT ***
C *** THIS ROUTINE IS AN INTERFACE TO GHEISHA8 ***
C *** NVE 06-APR-1988 CERN GENEVA ***
C
C CALLED BY : GUPHAD (USER ROUTINE)
C ORIGIN : F.CARMINATI
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
C --- GHEISHA COMMONS ---
      COMMON/PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT,NPRT(10)
                    LOGICAL LPRT,NPRT
C
C
C --- INITIALIZE RELEVANT GHEISHA VARIABLES AT FIRST PASS ---
      IF (IFINIT(4) .EQ. 0) CALL GHEINI
C
      IF (Z .LT. 1.0) GO TO 1000
C
      KK=ABS(Q(JMA+11))
      IF (KK .GT. 1) GO TO 10
C
      IF(IPART.EQ.70)THEN
        SIG=GHESIG(VECT(7),GEKIN,A,A,Z,1.0,1,DENS,0.0,IPART-60)
      ELSE
        SIG=GHESIG(VECT(7),GEKIN,A,A,Z,1.0,1,DENS,0.0,IPART)
      ENDIF
      GO TO 20
C
 10   CONTINUE
      QCOR=0.0
      IF(JTM.GT.0) THEN
         LNVE=LQ(JTM)
         IF (LNVE .GT. 0) QCOR=Q(LNVE+26)
      ENDIF
      IF(IPART.EQ.70)THEN
        SIG=GHESIG(VECT(7),GEKIN,A,Q(JMIXT+1),Q(JMIXT+KK+1),
     $             Q(JMIXT+2*KK+1),KK,DENS,QCOR,IPART-60)
      ELSE
        SIG=GHESIG(VECT(7),GEKIN,A,Q(JMIXT+1),Q(JMIXT+KK+1),
     $             Q(JMIXT+2*KK+1),KK,DENS,QCOR,IPART)
      ENDIF
C
 20   CONTINUE
      IF (SIG .LE. 0.0) GO TO 1000
      SHADR=ZINTHA/SIG
      IF (NPRT(9)) PRINT 2000,KK,SIG,SHADR
 2000 FORMAT(' *GPGHEI* KK,SIG,SHADR = ',I3,1X,2(G12.5,1X))
      GO TO 9999
C
C --- ENSURE NO INTERACTION IN CURRENT MEDIUM ---
C
 1000 CONTINUE
      SHADR=BIG
      IF (NPRT(9)) PRINT 2001,KK,SIG,SHADR
 2001 FORMAT(' *GPGHEI* === NO INTERACTION IN CURRENT MEDIUM ==='/
     $ ' KK,SIG,SHADR = ',I3,1X,2(G12.5,1X))
C
 9999 CONTINUE
      END
CDECK  ID>, GHEISH. 
*CMZ :  3.21/02 29/03/94  15.41.38  by  S.Giani
*-- Author :
      SUBROUTINE GHEISH
C
C *** MAIN STEERING FOR HADRON SHOWER DEVELOPMENT ***
C *** NVE 15-JUN-1988 CERN GENEVA ***
C
C CALLED BY : GUHADR (USER ROUTINE)
C ORIGIN : F.CARMINATI, H.FESEFELDT
C                       ROUTINES : CALIM  16-SEP-1987
C                                  SETRES 19-AUG-1985
C                                  INTACT 06-OCT-1987
C
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      INTEGER K0FLAG
      REAL AIEL,AIIN,AIFI,AICA,ALAM
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
C --- GHEISHA COMMONS ---
      PARAMETER (MXGKGH=100)
      PARAMETER (MXGKPV=MXGKGH)
      COMMON /VECUTY/ PV(10,MXGKPV)
C
      COMMON/CONSTS/ PI,TWPI,PIBTW,MP,MPI,MMU,MEL,MKCH,MK0,SMP,SMPI,
     $               SMU,CT,CTKCH,CTK0,
     $               ML0,MSP,MS0,MSM,MX0,MXM,CTL0,CTSP,CTSM,CTX0,CTXM,
     $               RMASS(35),RCHARG(35)
C
                     REAL MP,MPI,MMU,MEL,MKCH,MK0,
     *                    ML0,MSP,MS0,MSM,MX0,MXM
C
      PARAMETER (MXEVEN=12*MXGKGH)
      COMMON/EVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)
C
      COMMON/PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT,NPRT(10)
                    LOGICAL LPRT,NPRT
C
C
C --- "NEVENT" CHANGED TO "KEVENT" IN COMMON /CURPAR/ DUE TO CLASH ---
C --- WITH VARIABLE "NEVENT" IN GEANT COMMON ---
C
      PARAMETER (MXGKCU=MXGKGH)
      COMMON /CURPAR /WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,KEVENT,SHFLAG,
     $                ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     $                RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     $                ATNO2,ZNO2
C
C --- "IPART" CHANGED TO "KPART" IN COMMON /RESULT/ DUE TO CLASH ---
C --- WITH VARIABLE "IPART" IN GEANT COMMON ---
C
      COMMON /RESULT/ XEND,YEND,ZEND,RCA,RCE,AMAS,NCH,TOF,PX,PY,PZ,
     $                USERW,INTCT,P,EN,EK,AMASQ,DELTN,ITK,NTK,KPART,IND,
     $                LCALO,ICEL,SINL,COSL,SINP,COSP,
     $                XOLD,YOLD,ZOLD,POLD,PXOLD,PYOLD,PZOLD,
     $                XSCAT,YSCAT,ZSCAT,PSCAT,PXSCAT,PYSCAT,PZSCAT
                      REAL NCH,INTCT
C
C --- "ABSL(21)" CHANGED TO "ABSLTH(21)" IN COMMON /MAT/ DUE TO CLASH ---
C --- WITH VARIABLE "ABSL" IN GEANT COMMON ---
C
      COMMON /MAT/ LMAT,
     $             DEN(21),RADLTH(21),ATNO(21),ZNO(21),ABSLTH(21),
     $             CDEN(21),MDEN(21),X0DEN(21),X1DEN(21),RION(21),
     $             MATID(21),MATID1(21,24),PARMAT(21,10),
     $             IFRAT,IFRAC(21),FRAC1(21,10),DEN1(21,10),
     $             ATNO1(21,10),ZNO1(21,10)
C
      DIMENSION IPELOS(35)
      SAVE IDEOL
C
C --- TRANSFER GEANT CUT-OFFS INTO GHEISHA VALUES ---
      DIMENSION CUTS(5)
      EQUIVALENCE (CUTS(1),CUTGAM)
      DIMENSION RNDM(1)
C
C --- DIMENSION STMTS. FOR GEANT/GHEISHA PARTICLE CODE CONVERSIONS ---
C --- KIPART(I)=GHEISHA CODE CORRESPONDING TO GEANT   CODE I ---
C --- IKPART(I)=GEANT   CODE CORRESPONDING TO GHEISHA CODE I ---
C
      DIMENSION KIPART(48),IKPART(35)
C
C --- DATA STMTS. FOR GEANT/GHEISHA PARTICLE CODE CONVERSIONS ---
C --- KIPART(I)=GHEISHA CODE CORRESPONDING TO GEANT   CODE I ---
C --- IKPART(I)=GEANT   CODE CORRESPONDING TO GHEISHA CODE I ---
C
      DATA KIPART/
     $               1,   3,   4,   2,   5,   6,   8,   7,
     $               9,  12,  10,  13,  16,  14,  15,  11,
     $              35,  18,  20,  21,  22,  26,  27,  33,
     $              17,  19,  23,  24,  25,  28,  29,  34,
     $              35,  35,  35,  35,  35,  35,  35,  35,
     $              35,  35,  35,  35,  30,  31,  32,  35/
C
      DATA IKPART/
     $               1,   4,   2,   3,   5,   6,   8,   7,
     $               9,  11,  16,  10,  12,  14,  15,  13,
     $              25,  18,  26,  19,  20,  21,  27,  28,
     $              29,  22,  23,  30,  31,  45,  46,  47,
     $              24,  32,  48/
C
C
C --- DENOTE STABLE PARTICLES ACCORDING TO GHEISHA CODE ---
C --- STABLE : GAMMA, NEUTRINO, ELECTRON, PROTON AND HEAVY FRAGMENTS ---
C --- WHEN STOPPING THESE PARTICLES ONLY LOOSE THEIR KINETIC ENERGY ---
      DATA IPELOS/
     $             1,   1,   0,   1,   0,   0,   0,   0,
     $             0,   0,   0,   0,   0,   1,   0,   0,
     $             0,   0,   0,   0,   0,   0,   0,   0,
     $             0,   0,   0,   0,   0,   1,   1,   1,
     $             0,   0,   1/
C
C --- LOWERBOUND OF KINETIC ENERGY BIN IN N CROSS-SECTION TABLES ---
      DATA TEKLOW /0.0001/
C
C --- KINETIC ENERGY TO SWITCH FROM "CASN" TO "GNSLWD" FOR N CASCADE ---
      DATA SWTEKN /0.05/
C
      DATA IDEOL/0/
C
C --- INITIALIZE RELEVANT GHEISHA VARIABLES IN CASE NOT DONE ALREADY ---
      IF (IFINIT(4) .EQ. 0) CALL GHEINI
C
C --- SET THE INTERACTION MECHANISM TO "HADR" ---
      KCASE=NAMEC(12)
C
C --- SET GHEISHA PRINTING FLAGS ACCORDING TO "DEBUG" STEERING CARD --
      IF (IDEOL .EQ. IDEBUG) GO TO 9000
C
      IF (IDEBUG .NE. 1) GO TO 9001
C
C --- SET SELECTED DEBUGGING FLAGS ---
      DO 9002 LL=1,10
      IF ((ISWIT(LL) .LE. 100) .OR. (ISWIT(LL) .GT. 110)) GO TO 9002
      JJ=ISWIT(LL)-100
      NPRT(JJ)=.TRUE.
 9002 CONTINUE
      GO TO 9000
C
C --- NO DEBUGGING SELECTED ---
 9001 CONTINUE
      DO 9003 LL=1,10
      NPRT(LL)=.FALSE.
 9003 CONTINUE
      IDEOL=IDEBUG
C
 9000 CONTINUE
C
C --- SET THE GHEISHA PARTICLE TYPE TO THE ONE OF GEANT ---
      IF(IPART.GT.48.AND.IPART.NE.70) THEN
         IF(ISTOP.EQ.0) GOTO 9999
         JPA = LQ(JPART-IPART)
         AMAS=Q(JPA+7)
         NCH =Q(JPA+8)
         KPART=-IPART
         GOTO 107
      ENDIF
      NETEST=IKPART(KPART)
      IF(NETEST.EQ.11.AND.IPART.EQ.70)NETEST=NETEST+60
      IF ((NETEST .EQ. IPART) .OR. (ISTOP .NE. 0)) GO TO 9004
C
      PRINT 8881,IPART,KPART,ISTOP
 8881 FORMAT(' *GHEISH* IPART,KPART = ',2(I3,1X),' ISTOP = ',I3/
     $ ' *GHEISH* ======> PARTICLE TYPES DO NOT MATCH <=======')
      STOP
C
 9004 CONTINUE
      IF(IPART.EQ.70)THEN
        KPART=KIPART(IPART-60)
      ELSE
        KPART=KIPART(IPART)
      ENDIF
      KKPART=KPART
      AMAS=RMASS(KPART)
      NCH=RCHARG(KPART)
C
C --- TRANSPORT THE TRACK NUMBER TO GHEISHA AND INITIALISE SOME NUMBERS
 107  NTK=ITRA
      INTCT=0.0
      NEXT=1
      NTOT=0
      TOF=0.0
C
C --- FILL RESULT COMMON FOR THIS TRACK WITH GEANT VALUES ---
C --- CALIM CODE ---
      XEND=VECT(1)
      YEND=VECT(2)
      ZEND=VECT(3)
      PX=VECT(4)
      PY=VECT(5)
      PZ=VECT(6)
      USERW=UPWGHT
C --- SETRES CODE ---
      P=VECT(7)
      AMASQ=AMAS*AMAS
      EN=SQRT(AMASQ+P*P)
      EK=ABS(EN-ABS(AMAS))
      ENOLD=EN
C
      SINL=0.0
      COSL=1.0
      SINP=0.0
      COSP=1.0
C
      IF (ABS(P) .LE. 1.0E-10) GO TO 1
      SINL=PZ
      COSL=SQRT(ABS(1.0-SINL**2))
C
 1    CONTINUE
      CALL GRNDM(RNDM,1)
      PHI=RNDM(1)*TWPI
      IF ((PX .EQ. 0.0) .AND. (PY .EQ. 0.0)) GOTO 3
      IF (ABS(PX) .LT. 1.E-10) GOTO 2
      PHI=ATAN2(PY,PX)
      GOTO 3
C
 2    CONTINUE
      IF (PY .GT. 0.0) PHI=PI/2.0
      IF (PY .LE. 0.0) PHI=3.0*PI/2.0
C
 3    CONTINUE
      SINP=SIN(PHI)
      COSP=COS(PHI)
C
C --- SET GHEISHA INDEX FOR THE CURRENT MEDIUM ALWAYS TO 1 ---
      IND=1
C
C --- TRANSFER GLOBAL MATERIAL CONSTANTS FOR CURRENT MEDIUM ---
C --- DETAILED DATA FOR COMPOUNDS IS OBTAINED VIA ROUTINE COMPO ---
      ATNO(IND+1)=A
      ZNO(IND+1)=Z
      DEN(IND+1)=DENS
      RADLTH(IND+1)=RADL
      ABSLTH(IND+1)=ABSL
C
C --- SETUP PARMAT FOR PHYSICS STEERING ---
      PARMAT(IND+1,5)=0.0
      PARMAT(IND+1,8)=IPFIS
      PARMAT(IND+1,9)=0.0
      PARMAT(IND+1,10)=0.0
      JTMN=LQ(JTM)
      IF (JTMN .LE. 0) GO TO 4
      PARMAT(IND+1,5)=Q(JTMN+26)
 4    CONTINUE
C
C --- CHECK WHETHER PARTICLE IS STOPPING OR NOT ---
      IF (ISTOP .EQ. 0) GO TO 5
C
      IF (NPRT(9)) PRINT 1000,KPART
 1000 FORMAT(' *GHEISH* STOPPING GHEISHA PARTICLE ',I3)
      CALL GHSTOP
C --- IN CASE OF DECAY OF PARTICLE OR USER PARTICLE ==> RETURN ---
      IF (LMEC(NMEC) .EQ. 5 .OR. KPART .LT. 0) GO TO 9999
C --- IN CASE OF HAD. INT. WITH GENERATION OF SEC. ==> GO TO 40 ---
      IF (IHADR .NE. 2) GO TO 40
C --- ALSO DEPOSIT REST MASS ENERGY FOR IN-STABLE PARTICLES ---
      IF (IPELOS(KPART) .EQ. 0) DESTEP=DESTEP+ABS(RMASS(KPART))
      GO TO 9999
  5   CONTINUE
C
C --- INDICATE LIGHT (<= PI) AND HEAVY PARTICLES (HISTORICALLY) ---
C --- CALIM CODE ---
      J=2
      TEST=RMASS(7)-0.001
      IF (ABS(AMAS) .LT. TEST) J=1
C
C *** DIVISION INTO VARIOUS INTERACTION CHANNELS DENOTED BY "INT" ***
C THE CONVENTION FOR "INT" IS THE FOLLOWING
C
C INT  = -1 REACTION CROSS SECTIONS NOT YET TABULATED/PROGRAMMED
C      =  0 NO INTERACTION
C      =  1 ELEASTIC SCATTERING
C      =  2 INELASTIC SCATTERING
C      =  3 NUCLEAR FISSION WITH INELEASTIC SCATTERING
C      =  4 NEUTRON CAPTURE
C
C --- INTACT CODE ---
      KK=ABS(Q(JMA+11))
      ALAM1=0.0
      CALL GRNDM(RNDM,1)
      RAT=RNDM(1)*ALAM
      NMEC=NMEC+1
      ATNO2=A
      ZNO2 =Z
C
      DO 6 K=1,KK
      IF (KK .LE. 0) GO TO 6
C
      IF (KK .EQ. 1) GO TO 7
      ATNO2=Q(JMIXT+K)
      ZNO2 =Q(JMIXT+K+KK)
C
 7    CONTINUE
C
C --- TRY FOR ELASTIC SCATTERING ---
      INT=1
      LMEC(NMEC)=13
      ALAM1=ALAM1+AIEL(K)
      IF (RAT .LT. ALAM1) GO TO 8
C
C --- TRY FOR INELASTIC SCATTERING ---
      INT=2
      LMEC(NMEC)=20
      ALAM1=ALAM1+AIIN(K)
      IF (RAT .LT. ALAM1) GO TO 8
C
C --- TRY FOR NUCLEAR FISSION WITH INELASTIC SCATTERING ---
      INT=3
      LMEC(NMEC)=15
      ALAM1=ALAM1+AIFI(K)
      IF (RAT .LT. ALAM1) GO TO 8
C
C --- TRY FOR NEUTRON CAPTURE ---
      INT=4
      LMEC(NMEC)=18
      ALAM1=ALAM1+AICA(K)
      IF (RAT .LT. ALAM1) GO TO 8
C
 6    CONTINUE
C --- NO REACTION SELECTED ==> ELASTIC SCATTERING ---
      INT=1
      LMEC(NMEC)=13
C
C *** TAKE ACTION ACCORDING TO SELECTED REACTION CHANNEL ***
C --- FOLLOWING CODE IS A TRANSLATION OF "CALIM" INTO GEANT JARGON ---
C
 8    CONTINUE
      IF (NPRT(9)) PRINT 1001,INT
 1001 FORMAT(' *GHEISH* INTERACTION TYPE CHOSEN INT = ',I3)
C
C --- IN CASE OF NO INTERACTION OR UNKNOWN CROSS SECTIONS ==> DONE ---
      IF (INT .LE. 0) GO TO 40
C
C --- IN CASE OF NON-ELASTIC SCATTERING AND NO GENERATION OF SEC. ---
C --- PARTICLES DEPOSIT TOTAL PARTICLE ENERGY AND RETURN ---
      IF ((INT .EQ. 1) .OR. (IHADR .NE. 2)) GO TO 9
      ISTOP=2
      DESTEP=DESTEP+EN
      NGKINE=0
      GO TO 9999
C
 9    CONTINUE
      IF (INT .NE. 4) GO TO 10
C
C --- NEUTRON CAPTURE ---
      IF (NPRT(9)) PRINT 2000
 2000 FORMAT(' *GHEISH* ROUTINE CAPTUR WILL BE CALLED')
      ISTOP=1
      CALL CAPTUR(NOPT)
      GO TO 40
C
 10   CONTINUE
      IF (INT .NE. 3) GO TO 11
C --- NUCLEAR FISSION ---
      IF (NPRT(9)) PRINT 2001
 2001 FORMAT(' *GHEISH* ROUTINE FISSIO WILL BE CALLED')
      ISTOP=1
      TKIN=FISSIO(EK)
      GO TO 40
C
 11   CONTINUE
C
C --- ELASTIC AND INELASTIC SCATTERING ---
      PV( 1,MXGKPV)=P*PX
      PV( 2,MXGKPV)=P*PY
      PV( 3,MXGKPV)=P*PZ
      PV( 4,MXGKPV)=EN
      PV( 5,MXGKPV)=AMAS
      PV( 6,MXGKPV)=NCH
      PV( 7,MXGKPV)=TOF
      PV( 8,MXGKPV)=KPART
      PV( 9,MXGKPV)=0.
      PV(10,MXGKPV)=USERW
C
C --- ADDITIONAL PARAMETERS TO SIMULATE FERMI MOTION AND EVAPORATION ---
      DO 111 JENP=1,10
         ENP(JENP)=0.
 111  CONTINUE
      ENP(5)=EK
      ENP(6)=EN
      ENP(7)=P
C
      IF (INT .NE. 1) GO TO 12
C
C *** ELASTIC SCATTERING PROCESSES ***
C
C --- ONLY NUCLEAR INTERACTIONS FOR HEAVY FRAGMENTS ---
      IF ((KPART .GE. 30) .AND. (KPART .LE. 32)) GO TO 35
C
C --- NORMAL ELASTIC SCATTERING FOR LIGHT MEDIA ---
      IF (ATNO2 .LT. 1.5) GO TO 35
C
C --- COHERENT ELASTIC SCATTERING FOR HEAVY MEDIA ---
      IF (NPRT(9)) PRINT 2002
 2002 FORMAT(' *GHEISH* ROUTINE COSCAT WILL BE CALLED')
      CALL COSCAT
      GO TO 40
C
C *** NON-ELASTIC SCATTERING PROCESSES ***
 12   CONTINUE
C
C --- ONLY NUCLEAR INTERACTIONS FOR HEAVY FRAGMENTS ---
      IF ((KPART .GE. 30) .AND. (KPART .LE. 32)) GO TO 35
C
C *** USE SOMETIMES NUCLEAR REACTION ROUTINE "NUCREC" FOR LOW ENERGY ***
C *** PROTON AND NEUTRON SCATTERING ***
      CALL GRNDM(RNDM,1)
      TEST1=RNDM(1)
      TEST2=4.5*(EK-0.01)
      IF ((KPART .EQ. 14) .AND. (TEST1 .GT. TEST2)) GO TO 85
      IF ((KPART .EQ. 16) .AND. (TEST1 .GT. TEST2)) GO TO 86
C
C *** FERMI MOTION AND EVAPORATION ***
      TKIN=CINEMA(EK)
      PV( 9,MXGKPV)=TKIN
      ENP(5)=EK+TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6)=ENP(5)+ABS(AMAS)
      ENP(7)=(ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7)=SQRT(ABS(ENP(7)))
      TKIN=FERMI(ENP(5))
      ENP(5)=ENP(5)+TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6)=ENP(5)+ABS(AMAS)
      ENP(7)=(ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7)=SQRT(ABS(ENP(7)))
      TKIN=EXNU(ENP(5))
      ENP(5)=ENP(5)-TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6)=ENP(5)+ABS(AMAS)
      ENP(7)=(ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7)=SQRT(ABS(ENP(7)))
C
C *** IN CASE OF ENERGY ABOVE CUT-OFF LET THE PARTICLE CASCADE ***
      TEST=ABS(CHARGE)
      IF ((TEST .GT. 1.0E-10) .AND. (ENP(5) .GT. CUTHAD)) GO TO 35
      IF ((TEST .LE. 1.0E-10) .AND. (ENP(5) .GT. CUTNEU)) GO TO 35
C
C --- SECOND CHANCE FOR ANTI-BARYONS DUE TO POSSIBLE ANNIHILATION ---
      IF ((AMAS .GE. 0.0) .OR. (KPART .LE. 14)) GO TO 13
      ANNI=1.3*P
      IF (ANNI .GT. 0.4) ANNI=0.4
      CALL GRNDM(RNDM,1)
      TEST=RNDM(1)
      IF (TEST .GT. ANNI) GO TO 35
C
C *** PARTICLE WITH ENERGY BELOW CUT-OFF ***
C --- ==> ONLY NUCLEAR EVAPORATION AND QUASI-ELASTIC SCATTERING ---
 13   CONTINUE
C
      ISTOP=3
C
      IF (NPRT(9)) PRINT 1002,KPART,EK,EN,P,ENP(5),ENP(6),ENP(7)
 1002 FORMAT(' *GHEISH* ENERGY BELOW CUT-OFF FOR GHEISHA PARTICLE ',I3/
     $ ' EK,EN,P,ENP(5),ENP(6),ENP(7) = ',6(G12.5,1X))
C
      IF ((KPART .NE. 14) .AND. (KPART .NE. 16)) GO TO 14
      IF (KPART .EQ. 16) GO TO 86
C
C --- SLOW PROTON ---
 85   CONTINUE
      IF (NPRT(9)) PRINT 2003,EK,KPART
 2003 FORMAT(' *GHEISH* ROUTINE NUCREC WILL BE CALLED',
     $ ' EK = ',G12.5,' GEV  KPART = ',I3)
      CALL NUCREC(NOPT,2)
C
      IF (NOPT .NE. 0) GO TO 50
C
      IF (NPRT(9)) PRINT 2004,EK,KPART
 2004 FORMAT(' *GHEISH* ROUTINE COSCAT WILL BE CALLED',
     $ ' EK = ',G12.5,' GEV  KPART = ',I3)
      CALL COSCAT
      GO TO 40
C
C --- SLOW NEUTRON ---
 86   CONTINUE
      IF (NPRT(9)) PRINT 2015
      NUCFLG=0
      CALL GNSLWD(NUCFLG,INT,NFL,TEKLOW)
      IF (NUCFLG .NE. 0) GO TO 50
      GO TO 40
C
C --- OTHER SLOW PARTICLES ---
 14   CONTINUE
      IPA(1)=KPART
C --- DECIDE FOR PROTON OR NEUTRON TARGET ---
      IPA(2)=16
      CALL GRNDM(RNDM,1)
      TEST1=RNDM(1)
      TEST2=ZNO2/ATNO2
      IF (TEST1 .LT. TEST2) IPA(2)=14
      AVERN=0.0
      NFL=1
      IF (IPA(2) .EQ. 16) NFL=2
      IPPP=KPART
      IF (NPRT(9)) PRINT 2005
 2005 FORMAT(' *GHEISH* ROUTINE TWOB WILL BE CALLED')
      CALL TWOB(IPPP,NFL,AVERN)
      GOTO 40
C
C --- INITIALISATION OF CASCADE QUANTITIES ---
 35   CONTINUE
C
C *** CASCADE GENERATION ***
C --- CALCULATE FINAL STATE MULTIPLICITY AND LONGITUDINAL AND ---
C --- TRANSVERSE MOMENTUM DISTRIBUTIONS ---
C
C --- FIXED PARTICLE TYPE TO STEER THE CASCADE ---
      KKPART=KPART
C
C --- NO CASCADE FOR LEPTONS ---
      IF (KKPART .LE. 6) GO TO 9999
C
C *** WHAT TO DO WITH "NEW PARTICLES" FOR GHEISHA ?????? ***
C --- RETURN FOR THE TIME BEING ---
      IF (KKPART .GE. 35) GO TO 9999
C
C --- CASCADE OF HEAVY FRAGMENTS
      IF ((KKPART .GE. 30) .AND. (KKPART .LE. 32)) GO TO 390
C
C --- INITIALIZE THE IPA ARRAY ---
      CALL VZERO(IPA(1),MXGKCU)
C
C --- CASCADE OF OMEGA - AND OMEGA - BAR ---
      IF (KKPART .EQ. 33) GO TO 330
      IF (KKPART .EQ. 34) GO TO 331
C
      NVEPAR=KKPART-17
      IF (NVEPAR .LE. 0) GO TO 15
      GO TO (318,319,320,321,322,323,324,325,326,327,328,329),NVEPAR
C
 15   CONTINUE
      NVEPAR=KKPART-6
      GO TO (307,308,309,310,311,312,313,314,315,316,317,318),NVEPAR
C
C --- PI+ CASCADE ---
 307  CONTINUE
      IF (NPRT(9)) PRINT 2006
 2006 FORMAT(' *GHEISH* ROUTINE CASPIP WILL BE CALLED')
      CALL CASPIP(J,INT,NFL)
      GO TO 40
C
C --- PI0 ==> NO CASCADE ---
 308  CONTINUE
      GO TO 40
C
C --- PI- CASCADE ---
 309  CONTINUE
      IF (NPRT(9)) PRINT 2007
 2007 FORMAT(' *GHEISH* ROUTINE CASPIM WILL BE CALLED')
      CALL CASPIM(J,INT,NFL)
      GO TO 40
C
C --- K+ CASCADE ---
 310  CONTINUE
      IF (NPRT(9)) PRINT 2008
 2008 FORMAT(' *GHEISH* ROUTINE CASKP WILL BE CALLED')
      CALL CASKP(J,INT,NFL)
      GO TO 40
C
C --- K0 CASCADE ---
 311  CONTINUE
      IF (NPRT(9)) PRINT 2009
 2009 FORMAT(' *GHEISH* ROUTINE CASK0 WILL BE CALLED')
      CALL CASK0(J,INT,NFL)
      GO TO 40
C
C --- K0 BAR CASCADE ---
 312  CONTINUE
      IF (NPRT(9)) PRINT 2010
 2010 FORMAT(' *GHEISH* ROUTINE CASK0B WILL BE CALLED')
      CALL CASK0B(J,INT,NFL)
      GO TO 40
C
C --- K- CASCADE ---
 313  CONTINUE
      IF (NPRT(9)) PRINT 2011
 2011 FORMAT(' *GHEISH* ROUTINE CASKM WILL BE CALLED')
      CALL CASKM(J,INT,NFL)
      GO TO 40
C
C --- PROTON CASCADE ---
 314  CONTINUE
      IF (NPRT(9)) PRINT 2012
 2012 FORMAT(' *GHEISH* ROUTINE CASP WILL BE CALLED')
      CALL CASP(J,INT,NFL)
      GO TO 40
C
C --- PROTON BAR CASCADE ---
 315  CONTINUE
      IF (NPRT(9)) PRINT 2013
 2013 FORMAT(' *GHEISH* ROUTINE CASPB WILL BE CALLED')
      CALL CASPB(J,INT,NFL)
      GO TO 40
C
C --- NEUTRON CASCADE ---
 316  CONTINUE
      NUCFLG=0
      IF (EK .GT. SWTEKN) THEN
         CALL CASN(J,INT,NFL)
         IF (NPRT(9)) PRINT 2014
 2014 FORMAT(' *GHEISH* ROUTINE CASN WILL BE CALLED')
      ELSE
         CALL GNSLWD(NUCFLG,INT,NFL,TEKLOW)
         IF (NPRT(9)) PRINT 2015
 2015 FORMAT(' *GHEISH* ROUTINE GNSLWD WILL BE CALLED')
      ENDIF
      IF (NUCFLG .NE. 0) GO TO 50
      GO TO 40
C
C --- NEUTRON BAR CASCADE ---
 317  CONTINUE
      IF (NPRT(9)) PRINT 2016
 2016 FORMAT(' *GHEISH* ROUTINE CASNB WILL BE CALLED')
      CALL CASNB(J,INT,NFL)
      GO TO 40
C
C --- LAMBDA CASCADE ---
 318  CONTINUE
      IF (NPRT(9)) PRINT 2017
 2017 FORMAT(' *GHEISH* ROUTINE CASL0 WILL BE CALLED')
      CALL CASL0(J,INT,NFL)
      GO TO 40
C
C --- LAMBDA BAR CASCADE ---
 319  CONTINUE
      IF (NPRT(9)) PRINT 2018
 2018 FORMAT(' *GHEISH* ROUTINE CASAL0 WILL BE CALLED')
      CALL CASAL0(J,INT,NFL)
      GO TO 40
C
C --- SIGMA + CASCADE ---
 320  CONTINUE
      IF (NPRT(9)) PRINT 2019
 2019 FORMAT(' *GHEISH* ROUTINE CASSP WILL BE CALLED')
      CALL CASSP(J,INT,NFL)
      GO TO 40
C
C --- SIGMA 0 ==> NO CASCADE ---
 321  CONTINUE
      GO TO 40
C
C --- SIGMA - CASCADE ---
 322  CONTINUE
      IF (NPRT(9)) PRINT 2020
 2020 FORMAT(' *GHEISH* ROUTINE CASSM WILL BE CALLED')
      CALL CASSM(J,INT,NFL)
      GO TO 40
C
C --- SIGMA + BAR CASCADE ---
 323  CONTINUE
      IF (NPRT(9)) PRINT 2021
 2021 FORMAT(' *GHEISH* ROUTINE CASASP WILL BE CALLED')
      CALL CASASP(J,INT,NFL)
      GO TO 40
C
C --- SIGMA 0 BAR ==> NO CASCADE ---
 324  CONTINUE
      GO TO 40
C
C --- SIGMA - BAR CASCADE ---
 325  CONTINUE
      IF (NPRT(9)) PRINT 2022
 2022 FORMAT(' *GHEISH* ROUTINE CASASM WILL BE CALLED')
      CALL CASASM(J,INT,NFL)
      GO TO 40
C
C --- XI 0 CASCADE ---
 326  CONTINUE
      IF (NPRT(9)) PRINT 2023
 2023 FORMAT(' *GHEISH* ROUTINE CASX0 WILL BE CALLED')
      CALL CASX0(J,INT,NFL)
      GO TO 40
C
C --- XI - CASCADE ---
 327  CONTINUE
      IF (NPRT(9)) PRINT 2024
 2024 FORMAT(' *GHEISH* ROUTINE CASXM WILL BE CALLED')
      CALL CASXM(J,INT,NFL)
      GO TO 40
C
C --- XI 0 BAR CASCADE ---
 328  CONTINUE
      IF (NPRT(9)) PRINT 2025
 2025 FORMAT(' *GHEISH* ROUTINE CASAX0 WILL BE CALLED')
      CALL CASAX0(J,INT,NFL)
      GO TO 40
C
C --- XI - BAR CASCADE ---
 329  CONTINUE
      IF (NPRT(9)) PRINT 2026
 2026 FORMAT(' *GHEISH* ROUTINE CASAXM WILL BE CALLED')
      CALL CASAXM(J,INT,NFL)
      GO TO 40
C
C --- OMEGA - CASCADE ---
 330  CONTINUE
      IF (NPRT(9)) PRINT 2027
 2027 FORMAT(' *GHEISH* ROUTINE CASOM WILL BE CALLED')
      CALL CASOM(J,INT,NFL)
      GO TO 40
C
C --- OMEGA - BAR CASCADE ---
 331  CONTINUE
      IF (NPRT(9)) PRINT 2028
 2028 FORMAT(' *GHEISH* ROUTINE CASAOM WILL BE CALLED')
      CALL CASAOM(J,INT,NFL)
      GO TO 40
C
C --- HEAVY FRAGMENT CASCADE ---
 390  CONTINUE
      IF (NPRT(9)) PRINT 2090
 2090 FORMAT(' *GHEISH* ROUTINE CASFRG WILL BE CALLED')
      NUCFLG=0
      CALL CASFRG(NUCFLG,INT,NFL)
      IF (NUCFLG .NE. 0) GO TO 50
C
C *** CHECK WHETHER THERE ARE NEW PARTICLES GENERATED ***
 40   CONTINUE
      IF ((NTOT .NE. 0) .OR. (KKPART .NE. KPART)) GO TO 50
C
C --- NO SECONDARIES GENERATED AND PARTICLE IS STILL THE SAME ---
C --- ==> COPY EVERYTHING BACK IN THE CURRENT GEANT STACK ---
      NGKINE=0
      TOFG=TOFG+TOF*0.5E-10
C --- In case of crazy momentum value ==> no change to GEANT stack ---
      IF (P .LT. 0.) GO TO 41
      VECT(4)=PX
      VECT(5)=PY
      VECT(6)=PZ
      VECT(7)=P
      GETOT=EN
      GEKIN=EK
C --- CHECK KINETIC ENERGY ---
      CALL GEKBIN
      EDEP=ABS(ENOLD-EN)
      RMASSI=EN-EK
      IF (NPRT(9) .AND. (EN .GT. ENOLD))
     $ PRINT 8888,EDEP,ENOLD,EN,EK,RMASSI
 8888 FORMAT(' *GHEISH* EDEP,ENOLD,EN,EK,M = ',5(G12.5,1X)/
     $ ' *GHEISH* =======> EDEP WOULD BE NEGATIVE <========')
      IF (ISTOP .EQ. 0) DESTEP=DESTEP+EDEP
C
C --- RE-INITIALIZE THE PROBABILITY FOR HADRONIC INTERACTION ---
 41   CONTINUE
      CALL GRNDM(RNDM,1)
      IF ((RNDM(1) .LE. 0.) .OR. (RNDM(1) .GE. 1.)) GO TO 41
      ZINTHA=-LOG(RNDM(1))
      SLHADR=SLENG
      STEPHA=1.0E10
C
      IF(IPART.EQ.70)THEN
        NVEDUM=KIPART(IPART-60)
      ELSE
        NVEDUM=KIPART(IPART)
      ENDIF
      IF (NPRT(9)) PRINT 1003,NTOT,IPART,KPART,KKPART,NVEDUM
 1003 FORMAT(' *GHEISH* NO SEC. GEN. NTOT,IPART,KPART,KKPART,KIPART = ',
     $ 5(I3,1X)/
     $ ' CURRENT PARTICLE ON THE STACK AGAIN')
      GO TO 9999
C
C *** CURRENT PARTICLE IS NOT THE SAME AS IN THE BEGINNING OR/AND ***
C *** ONE OR MORE SECONDARIES HAVE BEEN GENERATED ***
 50   CONTINUE
C
      IF(IPART.EQ.70)THEN
        NVEDUM=KIPART(IPART-60)
      ELSE
        NVEDUM=KIPART(IPART)
      ENDIF
      IF (NPRT(9)) PRINT 1004,NTOT,IPART,KPART,KKPART,NVEDUM
 1004 FORMAT(' *GHEISH* SEC. GEN. NTOT,IPART,KPART,KKPART,KIPART = ',
     $ 5(I3,1X))
C
C --- INITIAL PARTICLE TYPE HAS BEEN CHANGED ==> PUT NEW TYPE ON ---
C --- THE GEANT TEMPORARY STACK ---
C
C --- MAKE CHOICE BETWEEN K0 LONG / K0 SHORT ---
      IF ((KPART .NE. 11) .AND. (KPART .NE. 12)) GO TO 52
      CALL GRNDM(RNDM,1)
      KPART=11.5+RNDM(1)
C
 52   CONTINUE
      ITY=IKPART(KPART)
      IF(ITY.EQ.11.AND.IPART.EQ.70)ITY=ITY+60
      LNVE=LQ(JPART-ITY)
      IF (LNVE .LE. 0) PRINT 1234,NTOT,ITY,LNVE
 1234 FORMAT('0*GHEISH* 1234 NTOT,ITY,LNVE = ',3(I10,1X))
      IF (LNVE .LE. 0) STOP
      IF (ISTOP .EQ. 0) ISTOP=1
C
C --- IN CASE THE NEW PARTICLE IS A NEUTRINO ==> FORGET IT ---
      IF (KPART .EQ. 2) GO TO 60
C
C --- PUT PARTICLE ON THE STACK ---
      GKIN(1,1)=PX*P
      GKIN(2,1)=PY*P
      GKIN(3,1)=PZ*P
      GKIN(4,1)=SQRT(P*P+RMASS(KPART)**2)
      GKIN(5,1)=ITY
      TOFD(1)=TOF*0.5E-10
      NGKINE = 1
      GPOS(1,1) = VECT(1)
      GPOS(2,1) = VECT(2)
      GPOS(3,1) = VECT(3)
C
      IF (NPRT(9)) PRINT 1005,ITY,NGKINE
 1005 FORMAT(' *GHEISH* GEANT PART. ',I3,' PUT ONTO STACK AT POS. ',I3)
C
C *** CHECK WHETHER SECONDARIES HAVE BEEN GENERATED AND COPY THEM ***
C *** ALSO ON THE GEANT STACK ***
 60   CONTINUE
C
C --- ALL QUANTITIES ARE TAKEN FROM THE GHEISHA STACK WHERE THE ---
C --- CONVENTION IS THE FOLLOWING ---
C
C EVE(INDEX+ 1)= X
C EVE(INDEX+ 2)= Y
C EVE(INDEX+ 3)= Z
C EVE(INDEX+ 4)= NCAL
C EVE(INDEX+ 5)= NCELL
C EVE(INDEX+ 6)= MASS
C EVE(INDEX+ 7)= CHARGE
C EVE(INDEX+ 8)= TOF
C EVE(INDEX+ 9)= PX
C EVE(INDEX+10)= PY
C EVE(INDEX+11)= PZ
C EVE(INDEX+12)= TYPE
C
      IF (NTOT .LE. 0) GO TO 9999
C
C --- ONE OR MORE SECONDARIES HAVE BEEN GENERATED ---
      DO 61 L=1,NTOT
      INDEX=(L-1)*12
      JND=EVE(INDEX+12)
C
C --- MAKE CHOICE BETWEEN K0 LONG / K0 SHORT ---
      IF ((JND .NE. 11) .AND. (JND .NE. 12)) GO TO 63
      CALL GRNDM(RNDM,1)
      JND=11.5+RNDM(1)
C
C --- FORGET ABOUT NEUTRINOS ---
 63   CONTINUE
      IF (JND .EQ. 2) GO TO 61
C
C --- SWITH TO GEANT QUANTITIES ---
      ITY=IKPART(JND)
      IF(ITY.EQ.11.AND.IPART.EQ.70)ITY=ITY+60
      JTY=LQ(JPART-ITY)
      IF (JTY .LE. 0) PRINT 1235,NTOT,ITY,JTY
 1235 FORMAT('0*GHEISH* 1235 NTOT,ITY,JTY = ',3(I10,1X))
      IF (JTY .LE. 0) STOP
*     ITRT=Q(JTY+6)
      PLX=EVE(INDEX+9)
      PLY=EVE(INDEX+10)
      PLZ=EVE(INDEX+11)
      ELT=SQRT(PLX*PLX+PLY*PLY+PLZ*PLZ+Q(JTY+7)**2)
C
C --- ADD PARTICLE TO THE STACK IF STACK NOT YET FULL ---
      IF (NGKINE .GE. MXGKIN) THEN
          WRITE(CHMAIL,1236) NTOT, L
 1236     FORMAT(' *** GHEISH: ',I9,' particle produced but only ',
     +           I9,' put on the GEANT stack!')
          CALL GMAIL(1,1)
          GO TO 9999
      ENDIF
      NGKINE=NGKINE+1
      GKIN(1,NGKINE)=PLX
      GKIN(2,NGKINE)=PLY
      GKIN(3,NGKINE)=PLZ
      GKIN(4,NGKINE)=ELT
      GKIN(5,NGKINE)=ITY
      TOFD(NGKINE)=EVE(INDEX+8)*0.5E-10
      GPOS(1,NGKINE) = VECT(1)
      GPOS(2,NGKINE) = VECT(2)
      GPOS(3,NGKINE) = VECT(3)
C
      IF (NPRT(9)) PRINT 1006,ITY,NGKINE,L,(EVE(INDEX+J),J=1,12)
 1006 FORMAT(' *GHEISH* GEANT PART. ',I3,' ALSO PUT ONTO STACK AT',
     $ ' POS. ',I3/
     $ ' EVE(',I2,') = '/12(1H ,12X,G12.5/))
C
 61   CONTINUE
C
 9999 CONTINUE
C --- LIMIT THE VALUE OF NGKINE IN CASE OF OVERFLOW ---
      NGKINE=MIN(NGKINE,MXGKIN)
      END
CDECK  ID>, FLUFIN. 

CDECK  ID>, GSKING. 
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GSKING (IGK)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GSKING (IGK)                                          *
C.    *                                                                *
C.    *   Stores in stack JSTAK either the IGKth track of /GCKING/,    *
C.    *    or the NGKINE tracks when IGK is 0.                         *
C.    *                                                                *
C.    *   Called by : 'User'                                           *
C.    *   Authors   : R.Brun, F.Bruyant                                *
C.    *                                                                *
C.    ******************************************************************
C.
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
C.
      COMMON/VTXKIN/NVTX,ITR
      REAL PSTO(3), VSTO(3)
C.
C.    ------------------------------------------------------------------
*
      IF (NGKINE.LE.0) GO TO 999
      IF (IGK.EQ.0) THEN
         N1 = 1
         N2 = NGKINE
      ELSE
         IF (IGK.LT.0.OR.IGK.GT.NGKINE) THEN
            WRITE(CHMAIL,10000) IGK, NGKINE
10000 FORMAT(' GSKING - Abnormal request ',I4,1X,I4)
            CALL GMAIL(0,0)
            GO TO 999
         ENDIF
         N1 = IGK
         N2 = IGK
      ENDIF
*
* *** Save original information from /GCKINE/ and /GCTRAK/
*
      IPASTO = IPART
      TOFSTO = TOFG
      DO 10 I = 1,3
         VSTO(I) = VERT(I)
         PSTO(I) = PVERT(I)
   10 CONTINUE
*
* *** Store required tracks in stack JSTAK
*
      IVSTO = 0
      IFLGK(NGKINE+1) = 0
      DO 30 N = N1,N2
         IF (IFLGK(N).LT.0) GO TO 30
         TOFG  = TOFSTO +TOFD(N)
         IPART = GKIN(5,N)
         DO 20 I = 1,3
            VERT(I)  = GPOS(I,N)
            PVERT(I) = GKIN(I,N)
   20    CONTINUE
*
* *** Give control to user for track selection
*
C+SELF,IF=-USRJMP.
         CALL GUSKIP(ISKIP)
C+SELF,IF= USRJMP.
C         CALL JUMPT1(JUSKIP,ISKIP)
C+SELF.
         IF (ISKIP.NE.0) GO TO 30
         IF (IFLGK(N).EQ.1) THEN
            IF (IVSTO.EQ.0) THEN
               IVSTO = 1
               IFLAG = 1
            ELSE
               IFLAG = -NVERTX
            ENDIF
         ELSE
            IFLAG = -IFLGK(N)
         ENDIF
         CALL GSSTAK (IFLAG)
         IF (IFLAG.NE.0) THEN
            IFLGK(N) = ITR
            IFLGK(NGKINE+1) = NVTX
         ELSE
            IFLGK(N) = 0
         ENDIF
   30 CONTINUE
*
* *** Restore original information in /GCKINE/ and /GCTRAK/
*
      IPART = IPASTO
      TOFG  = TOFSTO
      DO 40 I = 1,3
         VERT(I)  = VSTO(I)
         PVERT(I) = PSTO(I)
   40 CONTINUE
*                                                             END GSKING
  999 END
CDECK  ID>, GSSTAK. 
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GSSTAK (IFLAG)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GSSTAK (IFLAG)                                        *
C.    *                                                                *
C.    *   Stores in auxiliary stack JSTAK the particle currently       *
C.    *    described in common /GCKINE/.                               *
C.    *                                                                *
C.    *   On request, creates also an entry in structure JKINE :       *
C.    *    IFLAG =                                                     *
C.    *     0 : No entry in JKINE structure required (user)            *
C.    *     1 : New entry in JVERTX / JKINE structures required (user) *
C.    *    <0 : New entry in JKINE structure at vertex -IFLAG (user)   *
C.    *     2 : Entry in JKINE structure exists already (from GTREVE)  *
C.    *                                                                *
C.    *   Called by : GSKING, GTREVE                                   *
C.    *   Author    : S.Banerjee, F.Bruyant                            *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCMZFO/IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO(13)
C
      INTEGER       IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
*
      COMMON/VTXKIN/NVTX,ITR
      DIMENSION UBUF(1)
      DATA UBUF/0./
C.
C.    ------------------------------------------------------------------
*
      IF (IABS(IPART).GT.NPART) THEN
         PRINT *, ' GSSTAK - Unknown particle code, skip track ', IPART
         GO TO 999
      ENDIF
*
* *** Check if an entry in JKINE structure is required
*
      IF (IFLAG.EQ.1) THEN
         CALL GSVERT (VERT, ITRA, 0, UBUF, 0, NVTX)
         CALL GSKINE (PVERT, IPART, NVTX, UBUF, 0, ITR)
      ELSE IF (IFLAG.LT.0) THEN
         NVTX = -IFLAG
         CALL GSKINE (PVERT, IPART, NVTX, UBUF, 0, ITR)
      ELSE
         IF (IFLAG.EQ.0) THEN
*          Store -ITRA in stack for a track without entry in JKINE
            ITR = -ITRA
         ELSE
            ITR = ITRA
         ENDIF
      ENDIF
*
* *** Store information in stack
*
      IF (JSTAK.EQ.0) THEN
         NDBOOK = NTSTKP*NWSTAK +3
         NDPUSH = NTSTKS*NWSTAK
         CALL MZBOOK (IXCONS,JSTAK,JSTAK,1,'STAK', 0,0,NDBOOK, IOSTAK,3)
         IQ(JSTAK+2) = NTSTKP
      ELSE IF (IQ(JSTAK+1).EQ.IQ(JSTAK+2)) THEN
         CALL MZPUSH (IXCONS, JSTAK, 0, NDPUSH, 'I')
         IQ(JSTAK+2) = IQ(JSTAK+2) +NTSTKS
      ENDIF
*
      JST = JSTAK +IQ(JSTAK+1)*NWSTAK +3
      IQ(JSTAK+1) = IQ(JSTAK+1) +1
      IF (IQ(JSTAK+3).EQ.0) IQ(JSTAK+3) = IQ(JSTAK+1)
      IF (IQ(JSTAK+1).GT.NSTMAX)  NSTMAX = IQ(JSTAK+1)
*
      IQ(JST+1)   = ITR
      IQ(JST+2)   = IPART
      IQ(JST+3)   = 0
      DO 90 I = 1,3
         Q(JST+3+I) = VERT(I)
         Q(JST+6+I) = PVERT(I)
   90 CONTINUE
      Q(JST+10) = TOFG
      Q(JST+11) = SAFETY
      Q(JST+12) = UPWGHT
*
      NALIVE = NALIVE +1
*                                                             END GSSTAK
  999 END
CDECK  ID>, GTHADR. 
*CMZ :  3.21/02 03/07/94  17.57.24  by  S.Giani
*-- Author :
      SUBROUTINE GTHADR
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Charged hadron type track. Computes step size and propagates *
C.    *    particle through step.                                      *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *       Authors    R.Brun, F.Bruyant, M.Maire ********           *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C

      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
      PARAMETER (EPSMAC=1.E-6)
      DOUBLE PRECISION GKR,DEMEAN,STOPP,STOPMX,STOPRG,STOPC,EKIPR
      DOUBLE PRECISION ONE,XCOEF1,XCOEF2,XCOEF3,YCOEF1,YCOEF2,YCOEF3
      PARAMETER (THRESH=0.7,ONE=1)
      REAL VNEXT(6)
      SAVE CFLD,CHARG2,RMASS,CUTPRO,IKCUT,STOPC
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ? short circuit
*
      IF (GEKIN.LE.CUTHAD) GO TO 100
*
* *** Update local pointers if medium has changed
*
      IF (IUPD.EQ.0) THEN
         IUPD  = 1
         JLOSS = LQ(JMA-3)
         JRANG = LQ(JMA-16) + NEK1
         JCOEF = LQ(JMA-18) + 3*NEK1
         CHARG2 = CHARGE*CHARGE
         RMASS  = PMASS/AMASS
         OMCMOL = Q(JPROB+21)*CHARG2
         CHCMOL = Q(JPROB+25)*ABS(CHARGE)
         CUTPRO = MAX(CUTHAD*RMASS,ELOW(1))
         IKCUT = GEKA*LOG10(CUTPRO) + GEKB
         GKR   = (CUTPRO - ELOW(IKCUT))/(ELOW(IKCUT+1) - ELOW(IKCUT))
         STOPC = (1.-GKR)*Q(JRANG+IKCUT) + GKR*Q(JRANG+IKCUT+1)
         IF (FIELDM.NE.0.) THEN
            CFLD = 3333.*DEGRAD*TMAXFD/ABS(FIELDM*CHARGE)
         ELSE
            CFLD = BIG
         ENDIF
         IF(IMCKOV.EQ.1) THEN
            JTCKOV = LQ(JTM-3)
            JABSCO = LQ(JTCKOV-1)
            JEFFIC = LQ(JTCKOV-2)
            JINDEX = LQ(JTCKOV-3)
            JCURIN = LQ(JTCKOV-4)
            NPCKOV = Q(JTCKOV+1)
         ENDIF
         IF(ISTRA.GT.0) THEN
            JTSTRA = LQ(JMA-19)
            JTSTCO = LQ(JTSTRA-1)
            JTSTEN = LQ(JTSTRA-2)
         ENDIF
      ENDIF
*
* *** Compute current step size
*
      STEP   = STEMAX
      IPROC  = 103
      GEKRT1 = 1. -GEKRAT
*
*  **   Step limitation due to hadron interaction ?
*
      IF (IHADR.GT.0) THEN
         CALL GUPHAD
         IF (SHADR.LT.STEP) THEN
            IF (SHADR.LE.0.) SHADR = PREC
            STEP  = SHADR
            IPROC = 12
         ENDIF
      ENDIF
*
*  **   Step limitation due to decay ?
*
      IF (IDCAY.GT.0) THEN
         SDCAY = SUMLIF*VECT(7)/AMASS
         IF (SDCAY.LT.STEP) THEN
            STEP  = SDCAY
            IPROC = 5
         ENDIF
      ENDIF
*
*  ** Step limitation due to delta-ray production ?
*       (Cannot be tabulated easily because dependent on AMASS)
*
      IF (IDRAY.GT.0) THEN
         STEPDR = BIG
         IF (GEKIN.GT.DCUTM) THEN
            GAMASS = GETOT +AMASS
            TMAX   = EMASS*GEKIN*GAMASS/(0.5*AMASS*AMASS+EMASS*GETOT)
            IF (TMAX.GT.DCUTM) THEN
               BET2 = GEKIN*GAMASS/(GETOT*GETOT)
               Y    = DCUTM/TMAX
               SIG  = (1.-Y+BET2*Y*LOG(Y))/DCUTM
*              extra term for spin 1/2
               IF (AMASS.GT.0.9) SIG=SIG+0.5*(TMAX-DCUTM)/(GETOT*GETOT)
               SIG = SIG*Q(JPROB+17)*CHARG2*EMASS/BET2
*
               IF (SIG.GT.0.) THEN
                  STEPDR = 1./SIG
                  SDRAY  = STEPDR*ZINTDR
                  IF (SDRAY.LE.STEP) THEN
                     STEP  = SDRAY
                     IPROC = 10
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
      IF (STEP.LE.0.) THEN
         STEP  = 0.
         GO TO 110
      ENDIF
*
*  **   Step limitation due to energy loss (stopping range) ?
*
      IF (ILOSL.GT.0) THEN
         IF(GEKRAT.LT.THRESH) THEN
            I1 = MAX(IEKBIN-1,1)
         ELSE
            I1 = MIN(IEKBIN,NEKBIN-1)
         ENDIF
         I1 = 3*(I1-1)+1
         XCOEF1 = Q(JCOEF+I1)
         XCOEF2 = Q(JCOEF+I1+1)
         XCOEF3 = Q(JCOEF+I1+2)
         IF(XCOEF1.NE.0) THEN
            STOPP = -XCOEF2+SIGN(ONE,XCOEF1)* SQRT(XCOEF2
     +      **2 -(XCOEF3-GEKIN*RMASS/XCOEF1))
         ELSE
            STOPP = - (XCOEF3-GEKIN*RMASS)/XCOEF2
         ENDIF
         STOPMX = (STOPP - STOPC)/(RMASS*CHARG2)
         IF (STOPMX.LT.MIN(STEP,STMIN)) THEN
            STEP = STOPMX
            IPROC = 0
            IF(STEP.LE.0.)THEN
               GO TO 100
            ENDIF
            GO TO 10
         ENDIF
         EKF = (1. - DEEMAX)*GEKIN*RMASS
         IF (EKF.LT.ELOW(1)) THEN
            EKF = ELOW(1)
         ELSEIF (EKF.GE.ELOW(NEK1)) THEN
            EKF = ELOW(NEK1)*0.99
         ENDIF
         IKF=GEKA*LOG10(EKF)+GEKB
         GKR=(EKF-ELOW(IKF))/(ELOW(IKF+1)-ELOW(IKF))
         IF(GKR.LT.THRESH) THEN
            IK1 = MAX(IKF-1,1)
         ELSE
            IK1 = MIN(IKF,NEKBIN-1)
         ENDIF
         IK1 = 3*(IK1-1)+1
         YCOEF1=Q(JCOEF+IK1)
         YCOEF2=Q(JCOEF+IK1+1)
         YCOEF3=Q(JCOEF+IK1+2)
         IF(YCOEF1.NE.0.) THEN
            SLOSP = -YCOEF2+SIGN(ONE,YCOEF1)*SQRT(YCOEF2**2- (YCOEF3-
     +      EKF/YCOEF1))
         ELSE
            SLOSP = - (YCOEF3-EKF)/YCOEF2
         ENDIF
         SLOSP = STOPP - SLOSP
         SLOSS = MAX(STMIN, SLOSP/(RMASS*CHARG2) )
         IF (SLOSS.LT.STEP) THEN
            STEP = SLOSS
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to bending in magnetic field ?
*
      IF (IFIELD.NE.0) THEN
         SFIELD = CFLD*VECT(7)
         SFIELD=MAX(SFIELD, STMIN)
         IF (SFIELD.LT.STEP) THEN
            STEP  = SFIELD
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to multiple scattering ?
*
      IF (IMULL.GT.0) THEN
         SMULS=MIN(2232.*RADL*((VECT(7)**2)/(GETOT*CHARGE))**2,10.*RADL)
         SMULS  = MAX(STMIN, SMULS )
         IF (SMULS.LT.STEP) THEN
            STEP  = SMULS
            IPROC = 0
         ENDIF
      ENDIF
*
   10 CONTINUE
*
*  **   Step limitation due to Cerenkov production ?
*
      IF (IMCKOV.GT.0) THEN
         CALL GNCKOV
         STCKOV = MXPHOT/MAX(3.*DNDL,1E-10)
         SMULS  = MAX(STMIN, STCKOV)
         IF (SMULS.LT.STEP) THEN
            STEP  = STCKOV
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.0.95*SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP   = SNEXT + PREC
            IPROC = 0
         ENDIF
*
*        Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 20 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST    = JSTAK + 3 + (IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   20       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
* *** Linear transport when no field or very short step
*
      IF (IFIELD.EQ.0.OR.STEP.LE.PREC) THEN
*
         IF (IGNEXT.NE.0) THEN
            DO 30 I = 1,3
               VECTMP  = VECT(I) +STEP*VECT(I+3)
               IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
                  IF(VECT(I+3).NE.0.) THEN
                     VECTMP =
     +               VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*EPSMAC
                     IF(NMEC.GT.0) THEN
                        IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                     ENDIF
                     NMEC=NMEC+1
                     LMEC(NMEC)=104
                  ENDIF
               ENDIF
               VECT(I) = VECTMP
   30       CONTINUE
            INWVOL = 2
            NMEC = NMEC +1
            LMEC(NMEC) = 1
         ELSE
            DO 40 I = 1,3
               VECT(I)  = VECT(I) +STEP*VECT(I+3)
   40       CONTINUE
         ENDIF
      ELSE
*
* ***   otherwise, swim particle in magnetic field
*
         if(mycoun.gt.1.and.nfmany.gt.0.and.step.ge.safety)then
           nlevel=manyle(nfmany)
           do 99 i=1,nlevel
             names(i)=manyna(nfmany,i)
             number(i)=manynu(nfmany,i)
 99        continue
           call glvolu(nlevel,names,number,ier)
           if(ier.ne.0)print *,'Fatal error in GLVOLU'
           ingoto=0
         endif
         NMEC = NMEC +1
         LMEC(NMEC) = 4
*
   50    CALL GUSWIM (CHARGE, STEP, VECT, VOUT)
*
*  ** When near to boundary, take proper action (cut-step,crossing...)
*
         IF(STEP.GE.SAFETY)THEN
            INEAR = 0
            IF (IGNEXT.NE.0) THEN
               DO 60 I = 1,3
                  VNEXT(I+3) = VECT(I+3)
                  VNEXT(I) = VECT(I) +SNEXT*VECT(I+3)
   60          CONTINUE
               DO I = 1,3
                  IF (ABS(VOUT(I)-VNEXT(I)).GT.EPSIL) GOTO 70
               ENDDO
               INWVOL = 2
               NMEC = NMEC +1
               LMEC(NMEC) = 1
               GOTO 80
   70          CONTINUE
               INEAR = 1
            ENDIF
*
            CALL GINVOL (VOUT, ISAME)
            IF (ISAME.EQ.0)THEN
               IF ((INEAR.NE.0).OR.(STEP.LT.EPSIL)) THEN
                  INWVOL = 2
                  NMEC = NMEC +1
                  LMEC(NMEC) = 1
               ELSE
*              Cut step
                  STEP = 0.5*STEP
                  IF (LMEC(NMEC).NE.24) THEN
                     NMEC = NMEC +1
                     LMEC(NMEC) = 24
                  ENDIF
                  GO TO 50
               ENDIF
            ENDIF
         ENDIF
*
   80    CONTINUE
         DO 90 I = 1,6
            VECT(I) = VOUT(I)
   90    CONTINUE
         DP = VOUT(7) - VECT(7)
*
      ENDIF
*
* *** Correct the step due to multiple scattering
      IF (IMULL.NE.0) THEN
         STMULS = STEP
         CORR=0.0001*CHARG2*(STEP/RADL)*(GETOT/(VECT(7)*VECT(7)))**2
         IF (CORR.GT.0.25) CORR = 0.25
         STEP  = (1.+CORR)*STEP
      ENDIF
*
      SLENG = SLENG + STEP
*
* *** Generate Cherenkov photons if required
*
C      IF(IMCKOV.EQ.1) THEN
C         CALL GGCKOV
C         IF(NGPHOT.NE.0) THEN
C            NMEC=NMEC+1
C            LMEC(NMEC)=105
C         ENDIF
C      ENDIF
*
* *** apply energy loss : find the kinetic energy corresponding
*      to the new stopping range = stopmx - step
*
      IF (ILOSL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 3
         STOPRG = STOPP - STEP*RMASS*CHARG2
         IF (STOPRG.LE.STOPC) THEN
            STEP = STOPMX
            GO TO 100
         ENDIF
         IF(XCOEF1.NE.0.) THEN
            EKIPR = XCOEF1*(XCOEF3+STOPRG*(2.*XCOEF2+STOPRG))
         ELSE
            EKIPR = XCOEF2*STOPRG+XCOEF3
         ENDIF
         DEMEAN=GEKIN - EKIPR/RMASS
         IF(DEMEAN.LE.5.*GEKIN*EPSMAC) THEN
            DEMEAN=(GEKRT1*Q(JLOSS+IEKBIN)+GEKRAT*Q(JLOSS+IEKBIN+1))
     +      *STEP*CHARG2
         ENDIF
         IF (ILOSS.EQ.4.OR.IEKBIN.LE.IKCUT+1) THEN
            DESTEP = DEMEAN
         ELSE
            DEMS   = DEMEAN/CHARG2
            CALL GFLUCT(DEMS,DESTEP)
            DESTEP = DESTEP*CHARG2
         ENDIF
         DESTEP=MAX(DESTEP,0.)
         GEKINT = GEKIN -DESTEP
         IF (GEKINT.LE.(1.01*CUTHAD)) GO TO 100
         DESTEL = DESTEP
         GEKIN  = GEKINT
         GETOT  = GEKIN +AMASS
         VECT(7)= SQRT((GETOT+AMASS)*GEKIN)
         CALL GEKBIN
      ENDIF
*
* *** Apply multiple scattering.
*
      IF (IMULL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 2
         CALL GMULTS
      ENDIF
*
* *** Update time of flight
*
      SUMLIF = SUMLIF -STEP*AMASS/VECT(7)
      TOFG   = TOFG +STEP*GETOT/(VECT(7)*CLIGHT)
      IF (TOFG.GE.TOFMAX) THEN
         ISTOP = 4
         NMEC  = NMEC +1
         LMEC(NMEC) = 22
         GO TO 999
      ENDIF
*
* *** Update interaction probabilities
*
      IF (IHADR.GT.0) ZINTHA = ZINTHA*(1.-STEP/SHADR)
      IF (IDRAY.GT.0) ZINTDR = ZINTDR -STEP/STEPDR
*
* *** Update the total energy, if the hadron has been accelrated
*
      IF(ABS(DP) .GT. 1.E-6)THEN
        VECT(7) = VECT(7) + DP
        GETOT = SQRT(VOUT(7)**2+AMASS*AMASS)
        GEKIN = GETOT - AMASS
        CALL GEKBIN
      ENDIF 
*
      GO TO 110
*
*  **   Special treatment for overstopped tracks
*
  100 DESTEP = GEKIN
      DESTEL = DESTEP
      GEKIN  = 0.
      GAMMA  = GETOT/AMASS
      GETOT  = AMASS
      VECT(7)= 0.
      INWVOL = 0
      ISTOP  = 2
      NMEC = NMEC + 1
      LMEC(NMEC) = 30
      IF (IHADR.EQ.0) THEN
         IF (IDCAY.NE.0) THEN
            TOFG = TOFG +0.5*(1+GAMMA)*SUMLIF/CLIGHT
            IF (TOFG.GE.TOFMAX) THEN
               ISTOP = 4
               NMEC = NMEC + 1
               LMEC(NMEC) = 22
               NGKINE = 0
            ELSE
               NMEC = NMEC + 1
               LMEC(NMEC) = 5
               ISTOP =1
               CALL GDECAY
            ENDIF
            TOFD(NGKINE+1) = 0.5*(1+GAMMA)*SUMLIF/CLIGHT
            SUMLIF = 0.
         ENDIF
         GO TO 999
      ENDIF
      IPROC = 12
*
* *** apply slected process if any
*
  110 IF (IPROC.EQ.0) GO TO 999
      NMEC = NMEC +1
      LMEC(NMEC) = IPROC
*
*  **   Hadron interaction ?
*
      IF (IPROC.EQ.12) THEN
         CALL GUHADR
*   *   Check time cut-off for decays at rest
         IF (LMEC(NMEC).EQ.5) THEN
            TOFG   = TOFG +SUMLIF/CLIGHT
            IF (TOFG.GE.TOFMAX) THEN
               NGKINE = 0
               ISTOP  = 4
               LMEC(NMEC) = 22
            ENDIF
            TOFD(NGKINE+1) = SUMLIF/CLIGHT
            SUMLIF = 0.
         ENDIF
*
*  **   Decay ?
*
      ELSE IF (IPROC.EQ.5) THEN
         ISTOP = 1
         CALL GDECAY
*
*  **   Delta-ray ?
*
      ELSE IF (IPROC.EQ.10) THEN
         CALL GDRAY
      ENDIF
  999 IF(NGPHOT.GT.0) THEN
         IF(ITCKOV.EQ.2.AND.ISTOP.EQ.0) THEN
*
*  The hadron has produced Cerenkov photons and it is still alive
*  we put it in the stack and we let the photons to be tracked
            NGKINE = NGKINE+1
            GKIN(1,NGKINE) = VECT(4)*VECT(7)
            GKIN(2,NGKINE) = VECT(5)*VECT(7)
            GKIN(3,NGKINE) = VECT(6)*VECT(7)
            GKIN(4,NGKINE) = GETOT
            GKIN(5,NGKINE) = IPART
            TOFD(NGKINE) = 0.
            ISTOP = 1
c----put position as well
            GPOS(1,NGKINE)=VECT(1)
            GPOS(2,NGKINE)=VECT(2)
            GPOS(3,NGKINE)=VECT(3)
         ENDIF
      ENDIF
      END
CDECK  ID>, GTMUON. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTMUON
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Muon track. Computes step size and propagates particle       *
C.    *    through step.                                               *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *      Authors     R.Brun, F.Bruyant, M.Maire ********           *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C

      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
      PARAMETER (EPSMAC=1.E-6)
      DOUBLE PRECISION DEMEAN,STOPRG,STOPMX,STOPC
      DOUBLE PRECISION ONE,XCOEF1,XCOEF2,XCOEF3
      PARAMETER (ONE=1)
      REAL VNEXT(6)
      SAVE IKCUT,STOPC
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ? short circuit
*
      IF (GEKIN.LE.CUTMUO) GO TO 100
*
* *** Update local pointers if medium has changed
      IF (IUPD.EQ.0) THEN
         IUPD  = 1
         JLOSS = LQ(JMA-2)
         JBREM = LQ(JMA-9)
         JPAIR = LQ(JMA-10)
         JDRAY = LQ(JMA-11)
         JMUNU = LQ(JMA-14)
         JRANG = LQ(JMA-16)
         JCOEF = LQ(JMA-18)
         JMULOF= LQ(JTM-2)
         IF(IMCKOV.EQ.1) THEN
            JTCKOV = LQ(JTM-3)
            JABSCO = LQ(JTCKOV-1)
            JEFFIC = LQ(JTCKOV-2)
            JINDEX = LQ(JTCKOV-3)
            JCURIN = LQ(JTCKOV-4)
            NPCKOV = Q(JTCKOV+1)
         ENDIF
         OMCMOL= Q(JPROB+21)
         CHCMOL= Q(JPROB+25)
         IKCUT = Q(JMULOF+NEK1+1)
         STOPC = Q(JMULOF+NEK1+2)
         IF(ISTRA.GT.0) THEN
            JTSTRA = LQ(JMA-19)
            JTSTCO = LQ(JTSTRA-1)
            JTSTEN = LQ(JTSTRA-2)
         ENDIF
      ENDIF
*
* *** Compute current step size
*
      STEP   = STEMAX
      IPROC  = 103
      GEKRT1 = 1. -GEKRAT
      IEK1   = IEKBIN+NEK1
      IEK2   = IEKBIN+2*NEK1
*
*  **   Step limitation due to bremsstrahlung ?
*
      IF (IBREM.GT.0) THEN
         STEPBR = GEKRT1*Q(JBREM+IEK2) +GEKRAT*Q(JBREM+IEK2+1)
         SBREM  = STEPBR*ZINTBR
         IF (SBREM.LT.STEP) THEN
            STEP  = SBREM
            IPROC = 9
         ENDIF
      ENDIF
*
*  **   Step limitation due to pair production ?
*
      IF (IPAIR.GT.0) THEN
         STEPPA = GEKRT1*Q(JPAIR+IEK1) +GEKRAT*Q(JPAIR+IEK1+1)
         SPAIR  = STEPPA*ZINTPA
         IF (SPAIR.LT.STEP) THEN
            STEP  = SPAIR
            IPROC = 6
         ENDIF
      ENDIF
*
*  **   Step limitation due to decay ?
*
      IF (IDCAY.NE.0) THEN
         SDCAY = SUMLIF*VECT(7)/AMASS
         IF (SDCAY.LT.STEP) THEN
            STEP  = SDCAY
            IPROC = 5
         ENDIF
      ENDIF
*
*  **   Step limitation due to delta-ray ?
*
      IF (IDRAY.GT.0) THEN
         STEPDR = GEKRT1*Q(JDRAY+IEK2) +GEKRAT*Q(JDRAY+IEK2+1)
         SDRAY  = STEPDR*ZINTDR
         IF (SDRAY.LT.STEP) THEN
            STEP  = SDRAY
            IPROC = 10
         ENDIF
      ENDIF
*
*  **   Step limitation due to nuclear interaction ?
*
      IF (IMUNU.GT.0) THEN
         IF(GEKIN.GE.5.)THEN
            STEPMU = GEKRT1*Q(JMUNU+IEKBIN) +GEKRAT*Q(JMUNU+IEKBIN+1)
            SMUNU = STEPMU*ZINTMU
            IF (SMUNU.LT.STEP) THEN
               STEP  = SMUNU
               IPROC = 21
            ENDIF
         ELSE
            STEPMU = BIG
         ENDIF
      ENDIF
*
      IF (STEP.LE.0.) THEN
         STEP = 0.
         GO TO 90
      ENDIF
*
*  **   Step limitation due to energy-loss,multiple scattering
*             or magnetic field ?
*
      IF (JMULOF.NE.0) THEN
         SMULOF  = GEKRT1*Q(JMULOF+IEKBIN) +GEKRAT*Q(JMULOF+IEKBIN+1)
         IF (SMULOF.LT.STEP) THEN
            STEP  = SMULOF
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.0.95*SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP  = SNEXT + PREC
            IPROC = 0
         ENDIF
*
*        Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 10 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST    = JSTAK + 3 + (IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   10       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
* *** Linear transport when no field or very short step
*
      IF (IFIELD.EQ.0.OR.STEP.LE.PREC) THEN
*
         IF (IGNEXT.NE.0) THEN
            DO 20 I = 1,3
               VECTMP  = VECT(I) +STEP*VECT(I+3)
               IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
                  IF(VECT(I+3).NE.0.) THEN
                     VECTMP =
     +               VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*EPSMAC
                     IF(NMEC.GT.0) THEN
                        IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                     ENDIF
                     NMEC=NMEC+1
                     LMEC(NMEC)=104
                  ENDIF
               ENDIF
               VECT(I) = VECTMP
   20       CONTINUE
            INWVOL = 2
            NMEC = NMEC +1
            LMEC(NMEC) = 1
         ELSE
            DO 30 I = 1,3
               VECT(I)  = VECT(I) +STEP*VECT(I+3)
   30       CONTINUE
         ENDIF
      ELSE
*
* ***   otherwise, swim particle in magnetic field
*
         if(mycoun.gt.1.and.nfmany.gt.0.and.step.ge.safety)then
           nlevel=manyle(nfmany)
           do 99 i=1,nlevel
             names(i)=manyna(nfmany,i)
             number(i)=manynu(nfmany,i)
 99        continue
           call glvolu(nlevel,names,number,ier)
           if(ier.ne.0)print *,'Fatal error in GLVOLU'
           ingoto=0
         endif
         NMEC = NMEC +1
         LMEC(NMEC) = 4
*
   40    CALL GUSWIM (CHARGE, STEP, VECT, VOUT)
*
*  ** When near to boundary, take proper action (cut-step,crossing...)
*
         IF(STEP.GE.SAFETY)THEN
            INEAR = 0
            IF (IGNEXT.NE.0) THEN
               DO 50 I = 1,3
                  VNEXT(I+3) = VECT(I+3)
                  VNEXT(I) = VECT(I) +SNEXT*VECT(I+3)
   50          CONTINUE
               DO I = 1,3
                  IF (ABS(VOUT(I)-VNEXT(I)).GT.EPSIL) GO TO 60
               ENDDO
               INWVOL = 2
               NMEC = NMEC +1
               LMEC(NMEC) = 1
               GOTO 70
   60          CONTINUE
               INEAR = 1
            ENDIF
*
            CALL GINVOL (VOUT, ISAME)
            IF (ISAME.EQ.0)THEN
               IF ((INEAR.NE.0).OR.(STEP.LT.EPSIL)) THEN
                  INWVOL = 2
                  NMEC = NMEC +1
                  LMEC(NMEC) = 1
               ELSE
*              Cut step
                  STEP = 0.5*STEP
                  IF (LMEC(NMEC).NE.24) THEN
                     NMEC = NMEC +1
                     LMEC(NMEC) = 24
                  ENDIF
                  GO TO 40
               ENDIF
            ENDIF
         ENDIF
*
   70    CONTINUE
         DO 80 I = 1,6
            VECT(I) = VOUT(I)
   80    CONTINUE
         DP = VOUT(7) - VECT(7)
*
      ENDIF
*
* *** Correct the step due to multiple scattering
      IF (IMULL.NE.0) THEN
         STMULS = STEP
         CORR=0.0001*(STEP/RADL)*(GETOT/(VECT(7)*VECT(7)))**2
         IF (CORR.GT.0.25) CORR = 0.25
         STEP  = (1.+CORR)*STEP
      ENDIF
*
      SLENG = SLENG + STEP
*
* *** Generate Cherenkov photons if required
*
C      IF(IMCKOV.EQ.1) THEN
C         CALL GGCKOV
C         IF(NGPHOT.NE.0) THEN
C            NMEC=NMEC+1
C            LMEC(NMEC)=105
C         ENDIF
C      ENDIF
*
* *** apply energy loss : find the kinetic energy corresponding
*      to the new stopping range = stopmx - step
*
      IF (ILOSL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 3
         IF(GEKRAT.LT.0.7) THEN
            I1 = MAX(IEKBIN-1,1)
         ELSE
            I1 = MIN(IEKBIN,NEKBIN-1)
         ENDIF
         I1 = 3*(I1-1)+1
         XCOEF1 = Q(JCOEF+I1)
         XCOEF2 = Q(JCOEF+I1+1)
         XCOEF3 = Q(JCOEF+I1+2)
         IF(XCOEF1.NE.0.) THEN
            STOPMX = -XCOEF2+SIGN(ONE,XCOEF1)*SQRT(XCOEF2**2 - (XCOEF3-
     +      GEKIN/XCOEF1))
         ELSE
            STOPMX = - (XCOEF3-GEKIN)/XCOEF2
         ENDIF
         STOPRG = STOPMX - STEP
         IF (STOPRG.LT.STOPC) THEN
            STEP = STOPMX - STOPC
            GO TO 100
         ENDIF
*
         IF(XCOEF1.NE.0.) THEN
            DEMEAN=GEKIN-XCOEF1*(XCOEF3+STOPRG*(2.*XCOEF2+STOPRG))
         ELSE
            DEMEAN=GEKIN-XCOEF2*STOPRG-XCOEF3
         ENDIF
         IF(DEMEAN.LE.5.*GEKIN*EPSMAC) THEN
            DEMEAN=(GEKRT1*Q(JLOSS+IEKBIN)+GEKRAT*Q(JLOSS+IEKBIN+1))
     +      *STEP
         ENDIF
         IF (ILOSS.EQ.4.OR.IEKBIN.LE.IKCUT+1) THEN
            DESTEP = DEMEAN
         ELSE
            DEMS = DEMEAN
            CALL GFLUCT(DEMS,DESTEP)
         ENDIF
         IF (DESTEP.LT.0.) DESTEP = 0.
         GEKINT = GEKIN -DESTEP
         IF (GEKINT.LE.(1.01*CUTMUO)) GO TO 100
         DESTEL = DESTEP
         GEKIN  = GEKINT
         GETOT  = GEKIN +AMASS
         VECT(7)= SQRT((GETOT+AMASS)*GEKIN)
         CALL GEKBIN
      ENDIF
*
* *** Apply multiple scattering.
*
      IF (IMULL.NE.0) THEN
         NMEC   = NMEC +1
         LMEC(NMEC) = 2
         CALL GMULTS
      ENDIF
*
* *** Update time of flight
*
      SUMLIF = SUMLIF -STEP*AMASS/VECT(7)
      TOFG = TOFG +STEP*GETOT/(VECT(7)*CLIGHT)
      IF (TOFG.GE.TOFMAX) THEN
         ISTOP = 4
         NMEC  = NMEC +1
         LMEC(NMEC) = 22
         GO TO 999
      ENDIF
*
* *** Update interaction probabilities
*
      IF (IBREM.GT.0) ZINTBR = ZINTBR -STEP/STEPBR
      IF (IPAIR.GT.0) ZINTPA = ZINTPA -STEP/STEPPA
      IF (IDRAY.GT.0) ZINTDR = ZINTDR -STEP/STEPDR
      IF (IMUNU.GT.0) ZINTMU = ZINTMU -STEP/STEPMU
*
* ***   otherwise, apply the selected process if any
*
   90 IF (IPROC.EQ.0) GO TO 999
      NMEC = NMEC +1
      LMEC(NMEC) = IPROC
*
*  **   Bremsstrahlung ?
*
      IF (IPROC.EQ.9) THEN
         CALL GBREMM
*
*  **   Pair production ?
*
      ELSE IF (IPROC.EQ.6) THEN
         CALL GPAIRM
*
*  **   Decay ?
*
      ELSE IF (IPROC.EQ.5) THEN
         ISTOP = 1
         CALL GDECAY
*
*  **   Delta-ray ?
*
      ELSE IF (IPROC.EQ.10) THEN
         CALL GDRAY
*
*  **   Nuclear interaction ?
*
      ELSE IF (IPROC.EQ.21) THEN
         CALL GMUNU
      ENDIF
*
* *** Update the total energy, if the muon has been accelrated
*
      IF(ABS(DP) .GT. 1.E-6)THEN
        VECT(7) = VECT(7) + DP
        GETOT = SQRT(VOUT(7)**2+AMASS*AMASS)
        GEKIN = GETOT - AMASS
        CALL GEKBIN
      ENDIF
*
      GO TO 999
*
* *** Special treatment for overstopped tracks
*
  100 DESTEP = GEKIN
      DESTEL = DESTEP
      GEKIN  = 0.
      GETOT  = AMASS
      VECT(7)= 0.
      INWVOL = 0
      NMEC   = NMEC +1
      LMEC(NMEC) = 30
      IF (IDCAY.EQ.0) THEN
         write(6,*)' I am here!!! '
         ISTOP = 2
      ELSE
         NMEC   = NMEC +1
         TOFG   = TOFG +SUMLIF/CLIGHT
         IF (TOFG.GE.TOFMAX) THEN
            ISTOP = 4
            LMEC(NMEC) = 22
            SUMLIF = 0.
            GO TO 999
         ENDIF
         LMEC(NMEC) = 5
         ISTOP = 1
         CALL GDECAY
         TOFD(NGKINE+1) = SUMLIF/CLIGHT
         SUMLIF = 0.
      ENDIF
  999 IF(NGPHOT.GT.0) THEN
         IF(ITCKOV.EQ.2.AND.ISTOP.EQ.0) THEN
*
*  The muon has produced Cerenkov photons and it is still alive
*  we put it in the stack and we let the photons to be tracked
            NGKINE = NGKINE+1
            GKIN(1,NGKINE) = VECT(4)*VECT(7)
            GKIN(2,NGKINE) = VECT(5)*VECT(7)
            GKIN(3,NGKINE) = VECT(6)*VECT(7)
            GKIN(4,NGKINE) = GETOT
            GKIN(5,NGKINE) = IPART
            TOFD(NGKINE) = 0.
            ISTOP = 1
c----put position as well
            GPOS(1,NGKINE)=VECT(1)
            GPOS(2,NGKINE)=VECT(2)
            GPOS(3,NGKINE)=VECT(3)
         ENDIF
      ENDIF
      END
CDECK  ID>, GTNEUT. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTNEUT
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Neutral hadron type track. Computes step size and propagates *
C.    *    particle through step.                                      *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *      Authors     R.Brun, F.Bruyant  *********                  *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      PARAMETER (EPSMAC=1.E-6)
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ? Special treatment
*
      IF (GEKIN.LE.CUTNEU) THEN
         GEKIN  = 0.
         GETOT  = AMASS
         VECT(7)= 0.
         ISTOP  = 2
         NMEC = NMEC + 1
         LMEC(NMEC) = 30
         IF (IHADR.EQ.0.AND.AMASS.GT.0.) THEN
            IF (IDCAY.NE.0) THEN
               GAMMA  = GETOT/AMASS
               TOFG = TOFG +GAMMA*SUMLIF/CLIGHT
               IF (TOFG.GE.TOFMAX) THEN
                  ISTOP = 4
                  NMEC = NMEC + 1
                  LMEC(NMEC) = 22
                  NGKINE = 0
               ELSE
                  NMEC = NMEC + 1
                  LMEC(NMEC) = 5
                  ISTOP =1
                  CALL GDECAY
               ENDIF
               TOFD(NGKINE+1) = GAMMA*SUMLIF/CLIGHT
               SUMLIF = 0.
            ENDIF
            GO TO 999
         ENDIF
         IPROC = 12
         GO TO 40
      ENDIF
*
* *** Compute step size
*
      IPROC  = 103
      STEP   = STEMAX
*
*  **   Step limitation due to hadron interaction ?
*
      IF (IHADR.GT.0) THEN
         CALL GUPHAD
         IF (SHADR.LT.STEP) THEN
            IF (SHADR.LE.0.) SHADR = PREC
            STEP  = SHADR
            IPROC = 12
         ENDIF
      ENDIF
*
*  **   Step limitation due to decay ?
*
      IF ((IDCAY.NE.0).AND.(AMASS.GT.0.)) THEN
         SDCAY = SUMLIF*VECT(7)/AMASS
         IF (SDCAY.LT.STEP) THEN
            STEP  = SDCAY
            IPROC = 5
         ENDIF
      ENDIF
*
      IF (STEP.LT.0.) STEP = 0.
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP = SNEXT + PREC
            IPROC = 0
            INWVOL = 2
            NMEC = NMEC + 1
            LMEC(NMEC)  = 1
         ENDIF
*
*        Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 10 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST = JSTAK +3 +(IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   10       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
*
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
* *** Linear transport
*
      IF (INWVOL.EQ.2) THEN
         DO 20 I = 1,3
            VECTMP  = VECT(I) +STEP*VECT(I+3)
            IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
               IF(VECT(I+3).NE.0.) THEN
                  VECTMP = VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*
     +            EPSMAC
                  IF(NMEC.GT.0) THEN
                     IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                  ENDIF
                  NMEC=NMEC+1
                  LMEC(NMEC)=104
               ENDIF
            ENDIF
            VECT(I) = VECTMP
   20    CONTINUE
      ELSE
         DO 30 I = 1,3
            VECT(I)  = VECT(I) +STEP*VECT(I+3)
   30    CONTINUE
      ENDIF
*
      SLENG = SLENG +STEP
*
* *** Update time of flight
*
      SUMLIF = SUMLIF -STEP*AMASS/VECT(7)
      TOFG = TOFG +STEP*GETOT/(VECT(7)*CLIGHT)
      IF (TOFG.GE.TOFMAX) THEN
         ISTOP = 4
         NMEC  = NMEC +1
         LMEC(NMEC) = 22
         GO TO 999
      ENDIF
*
* *** Update interaction probabilities
*
      IF (IHADR.GT.0) ZINTHA = ZINTHA*(1.-STEP/SHADR)
*
* *** apply the selected process if any
*
   40 IF (IPROC.EQ.0) GO TO 999
      NMEC = NMEC +1
      LMEC(NMEC) = IPROC
*
*  **   Hadron interaction ?
*
      IF (IPROC.EQ.12) THEN
         CALL GUHADR
*          Check time cut-off for decays at rest
         IF (LMEC(NMEC).EQ.5) THEN
            TOFG   = TOFG +SUMLIF/CLIGHT
            IF (TOFG.GE.TOFMAX) THEN
               ISTOP = 4
               LMEC(NMEC) = 22
               NGKINE = 0
            ENDIF
            TOFD(NGKINE+1) = SUMLIF/CLIGHT
            SUMLIF = 0.
         ENDIF
*
*  **   Decay ?
*
      ELSE IF (IPROC.EQ.5) THEN
         ISTOP = 1
         CALL GDECAY
      ENDIF
*                                                             END GTNEUT
  999 END
CDECK  ID>, GTREVE. 
*CMZ :  3.21/03 07/10/94  18.07.13  by  S.Giani
*-- Author :
      SUBROUTINE GTREVE
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GTREVE                                                *
C.    *                                                                *
C.    *   Controls tracking of all particles belonging to the current  *
C.    *    event.                                                      *
C.    *                                                                *
C.    *   Called by : GUTREV, called by GTRIG                          *
C.    *   Authors   : R.Brun, F.Bruyant                                *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
      REAL UBUF(2)
      EQUIVALENCE (UBUF(1),WS(1))
      LOGICAL   BTEST
C.
C.    ------------------------------------------------------------------
      NTMSTO = 0
      NSTMAX = 0
      NALIVE = 0
*
      NV = NVERTX
      DO 290 IV = 1,NV
*
* ***   For each vertex in turn ..
*
         JV = LQ(JVERTX-IV)
         NT = Q(JV+7)
         IF (NT.LE.0) GO TO 290
         TOFG   = Q(JV+4)
         SAFETY = 0.
         IF (NJTMAX.GT.0) THEN
            CALL GMEDIA (Q(JV+1), NUMED)
            IF (NUMED.EQ.0) THEN
               WRITE (CHMAIL, 1001) (Q(JV+I), I=1,3)
               CALL GMAIL (0, 0)
               GO TO 290
            ENDIF
            CALL GLSKLT
         ENDIF
*
*  **   Loop over tracks attached to current vertex
*
         DO 190 IT = 1,NT
            JV   = LQ(JVERTX-IV)
            ITRA = Q(JV+7+IT)
            IF (BTEST(IQ(LQ(JKINE-ITRA)),0)) GO TO 190
            CALL GFKINE (ITRA, VERT, PVERT, IPART, IVERT, UBUF, NWBUF)
            IF (IVERT.NE.IV) THEN
               WRITE (CHMAIL, 1002) IV, IVERT
               CALL GMAIL (0, 0)
               GO TO 999
            ENDIF
*
* *** Give control to user for track selection
*
C+SELF,IF=-USRJMP.
      CALL GUSKIP(ISKIP)
C+SELF,IF= USRJMP.
C      CALL JUMPT1(JUSKIP,ISKIP)
C+SELF.
      IF (ISKIP.NE.0) GO TO 190
*
*   *      Store current track parameters in stack JSTAK
*
            CALL GSSTAK (2)
  190    CONTINUE
*
*  **   Start tracking phase
*
  210    IF (NALIVE.NE.0) THEN
            NALIVE = NALIVE -1
*
*   *      Pick-up next track in stack JSTAK, if any
*
            IF (IQ(JSTAK+1).GT.0) THEN
*
*   *         Initialize tracking parameters
*
               CALL GLTRAC
               IF (NUMED.EQ.0) GO TO 210
            ELSE
*
*   *         otherwise, select next track segment from stack JTRACK
*
               CALL GFTRAC
*
            ENDIF
*
*   *       Resume tracking
*
            IF(RAYTRA.EQ.1.)THEN
            ELSE
              CALL GUTRAK
            ENDIF
            IF (IEOTRI.NE.0) GO TO 999
            GO TO 210
         ENDIF
*
  290 CONTINUE
*
 1001 FORMAT (' GTREVE : Vertex outside setup, XYZ=',3G12.4)
 1002 FORMAT (' GTREVE : Abnormal track/vertex connection',2I8)
*                                                             END GTREVE
  999 END
*
CDECK  ID>, GFINDS. 
*CMZ :  3.21/02 06/07/94  18.26.03  by  S.Giani
*-- Author :
      SUBROUTINE GFINDS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Returns the set/volume parameters corresponding to       *
C.    *       the current space point in /GCTRAK/                      *
C.    *       and fill common /GCSETS/                                 *
C.    *                                                                *
C.    *       IHSET  user set identifier                               *
C.    *       IHDET  user detector identifier                          *
C.    *       ISET set number in JSET                                  *
C.    *       IDET   detector number in JS=LQ(JSET-ISET)               *
C.    *       IDTYPE detector type (1,2)                               *
C.    *       NUMBV  detector volume numbers (array of length NVNAME)  *
C.    *       NVNAME number of volume levels                           *
C.    *                                                                *
C.    *    ==>Called by : GTRACK                                       *
C.    *       Author    R.Brun  *********                              *
C.    *       Modified  V.Perev                                        *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCSETS/IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)
C
      INTEGER       IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      CHARACTER*4 VNAME
      CHARACTER*3 VNAME1,VNAME2
C
      JATTF(JV) = JV + Q(JV+5) + 6
C.
C.    ------------------------------------------------------------------
C.
*
*
      IHSET = 0
      IHDET = 0
      ISET  = 0
      IDET  = 0
      IDTYPE = 0
      NVNAME = 0
*
      DO 10 NLEV = NLEVEL,1,-1
         JVO = LQ(JVOLUM-LVOLUM(NLEV))
         JAT = JATTF(JVO)
         IDET = Q(JAT+8)
         IF(IDET.NE.0)GO TO 15
  10  CONTINUE
      GOTO 99
*
  15  ISET   = Q(JAT+7)
      IDTYPE = Q(JAT+9)
      IHSET  = IQ(JSET+ISET)
      JS     = LQ(JSET-ISET)
      IHDET  = IQ(JS+IDET)
      JD     = LQ(JS-IDET)
      NVNAME = IQ(JD+2)
*
      DO 40 I=NVNAME,1,-1
            NAME=IQ(JD+2*I+9)
            NUMBV(I)=0
            DO 30 J=NLEV,1,-1
               IF(NAMES(J).EQ.NAME)THEN
                  NUMBV(I)=NUMBER(J)
                  NLEV=J-1
                  GO TO 40
               ENDIF
               CALL UHTOC(NAME,4,VNAME,4)
               IF(VNAME.EQ.'    ')THEN
                  NUMBV(I)=NUMBER(J)
                  NLEV=J-1
                  GO TO 40
               ENDIF
               CALL UHTOC(NAME    ,4,VNAME1,3)
               CALL UHTOC(NAMES(J),4,VNAME2,3)
               IF(VNAME1.EQ.VNAME2)THEN
                  NUMBV(I)=NUMBER(J)
                  NLEV=J-1
                  GO TO 40
               ENDIF
  30        CONTINUE
  40  CONTINUE
C
   99 END
C
CDECK  ID>, GTELEC. 
*CMZ :  3.21/04 22/02/95  16.08.31  by  S.Giani
*-- Author :
      SUBROUTINE GTELEC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Electron type track. Computes step size and propagates       *
C.    *    particle through step.                                      *
C.    *                                                                *
C.    *   ==> Called by : GTRACK                                       *
C.    *       Authors    R.Brun, F.Bruyant, M.Maire L.Urban ********   *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C

      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
      PARAMETER (EPSMAC=1.E-6)
      DOUBLE PRECISION DEMEAN,STOPRG,STOPMX,STOPC
      DOUBLE PRECISION ONE,XCOEF1,XCOEF2,XCOEF3,ZERO
      PARAMETER (ONE=1,ZERO=0)
      REAL VNEXT(6)
      SAVE IKCUT,STOPC
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      DIMENSION RNDM(3)
      PARAMETER ( TLIM = 0.0002)
      IABAN = NINT(DPHYS1)
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ? short circuit
*
      IF (GEKIN.LE.CUTELE) GO TO 100
*
* *** Update local pointers if medium or particle code has changed
      IF (IUPD.EQ.0) THEN
         IUPD  = 1
         JMULOF= LQ(JTM-1)
         IF (CHARGE.LT.0.) THEN
            JBREM = LQ(JMA-9)
            JLOSS = LQ(JMA-1)
            JDRAY = LQ(JMA-11)
            JRANG = LQ(JMA-15)
            JCOEF = LQ(JMA-17)
         ELSE
            JBREM = LQ(JMA-9)  +NEK1
            JLOSS = LQ(JMA-1)  +NEK1
            JDRAY = LQ(JMA-11) +NEK1
            JRANG = LQ(JMA-15) +NEK1
            JCOEF = LQ(JMA-17) +3*NEK1
            JANNI = LQ(JMA-7)
         ENDIF
         IF(IMCKOV.EQ.1) THEN
            JTCKOV = LQ(JTM-3)
            JABSCO = LQ(JTCKOV-1)
            JEFFIC = LQ(JTCKOV-2)
            JINDEX = LQ(JTCKOV-3)
            JCURIN = LQ(JTCKOV-4)
            NPCKOV = Q(JTCKOV+1)
         ENDIF
         OMCMOL = Q(JPROB+21)
         CHCMOL = Q(JPROB+25)
         IKCUT  = Q(JMULOF+NEK1+1)
         STOPC  = Q(JMULOF+NEK1+2)
         IF(ISTRA.GT.0) THEN
            JTSTRA = LQ(JMA-19)
            JTSTCO = LQ(JTSTRA-1)
            JTSTEN = LQ(JTSTRA-2)
         ENDIF
      ENDIF
*
* *** Compute current step size
*
      STEP   = STEMAX
      IPROC  = 103
      GEKRT1 = 1. -GEKRAT
*
*  **   Step limitation due to bremsstrahlung ?
*
      IF (IBREM.GT.0) THEN
         STEPBR = GEKRT1*Q(JBREM+IEKBIN) +GEKRAT*Q(JBREM+IEKBIN+1)
         SBREM  = STEPBR*ZINTBR
         IF (SBREM.LT.STEP) THEN
            STEP  = SBREM
            IPROC = 9
         ENDIF
      ENDIF
*
*  **   Step limitation due to delta-ray production ?
*
      IF (IDRAY.GT.0) THEN
         STEPDR = GEKRT1*Q(JDRAY+IEKBIN) +GEKRAT*Q(JDRAY+IEKBIN+1)
         SDRAY  = STEPDR*ZINTDR
         IF (SDRAY.LT.STEP) THEN
            STEP  = SDRAY
            IPROC = 10
         ENDIF
      ENDIF
*
*  **   Step limitation due to annihilation ?
*
      IF (CHARGE.GT.0.) THEN
         IF (IANNI.GT.0) THEN
            STEPAN = GEKRT1*Q(JANNI+IEKBIN) +GEKRAT*Q(JANNI+IEKBIN+1)
            SANNI  = STEPAN*ZINTAN
            IF (SANNI.LT.STEP) THEN
               STEP  = SANNI
               IPROC = 11
            ENDIF
         ENDIF
      ENDIF
*
      IF (STEP.LE.0.) THEN
         STEP = 0.
         GO TO 90
      ENDIF
*
*  **   Step limitation due to energy-loss,multiple scattering
*             or magnetic field ?
*
      IF (JMULOF.NE.0) THEN
         SMULOF  = GEKRT1*Q(JMULOF+IEKBIN) +GEKRAT*Q(JMULOF+IEKBIN+1)
         IF (SMULOF.LT.STEP) THEN
            STEP  = SMULOF
            IPROC = 0
         ENDIF
      ENDIF
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.0.95*SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP   = SNEXT + PREC
            IPROC = 0
         ENDIF
*
*        Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 10 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST    = JSTAK + 3 + (IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   10       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
      IF (IFIELD.EQ.0.OR.STEP.LE.PREC) THEN
*
* *** Linear transport when no field or very short step
*
         IF (IGNEXT.NE.0) THEN
*
* *** Particle is supposed to cross the boundary during step
*
            DO 20 I = 1,3
               VECTMP  = VECT(I) +STEP*VECT(I+3)
               IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
                  IF(VECT(I+3).NE.0.) THEN
                     VECTMP =
     +               VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*EPSMAC
                     IF(NMEC.GT.0) THEN
                        IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                     ENDIF
                     NMEC=NMEC+1
                     LMEC(NMEC)=104
                  ENDIF
               ENDIF
               VECT(I) = VECTMP
   20       CONTINUE
            INWVOL = 2
            NMEC = NMEC +1
            LMEC(NMEC) = 1
         ELSE
            DO 30 I = 1,3
               VECT(I)  = VECT(I) +STEP*VECT(I+3)
   30       CONTINUE
         ENDIF
      ELSE
*
* *** otherwise, swim particle in magnetic field
*
         if(mycoun.gt.1.and.nfmany.gt.0.and.step.ge.safety)then
           nlevel=manyle(nfmany)
           do 99 i=1,nlevel
             names(i)=manyna(nfmany,i)
             number(i)=manynu(nfmany,i)
 99        continue
           call glvolu(nlevel,names,number,ier)
           if(ier.ne.0)print *,'Fatal error in GLVOLU'
           ingoto=0
         endif
         NMEC = NMEC +1
         LMEC(NMEC) = 4
*
   40    CALL GUSWIM (CHARGE, STEP, VECT, VOUT)
*
*  ** When near to boundary, take proper action (cut-step,crossing...)
*
         IF(STEP.GE.SAFETY)THEN
            INEAR = 0
            IF (IGNEXT.NE.0) THEN
               DO 50 I = 1,3
                  VNEXT(I+3) = VECT(I+3)
                  VNEXT(I) = VECT(I) +SNEXT*VECT(I+3)
   50          CONTINUE
               DO I = 1,3
                  IF (ABS(VOUT(I)-VNEXT(I)).GT.EPSIL) GO TO 60
               ENDDO
               INWVOL = 2
               NMEC = NMEC +1
               LMEC(NMEC) = 1
               GOTO 70
   60          CONTINUE
               INEAR = 1
            ENDIF
*
            CALL GINVOL (VOUT, ISAME)
            IF (ISAME.EQ.0) THEN
               IF ((INEAR.NE.0).OR.(STEP.LT.EPSIL)) THEN
                  INWVOL = 2
                  NMEC = NMEC +1
                  LMEC(NMEC) = 1
               ELSE
*              Cut step
                  STEP = 0.5*STEP
                  IF (LMEC(NMEC).NE.24) THEN
                     NMEC = NMEC +1
                     LMEC(NMEC) = 24
                  ENDIF
                  GO TO 40
               ENDIF
            ENDIF
         ENDIF
*
   70    CONTINUE
         DO 80 I = 1,6
            VECT(I) = VOUT(I)
   80    CONTINUE
         DP = VOUT(7) - VECT(7)
*
      ENDIF
*
*
* *** Correct the step due to multiple scattering
      IF (IMULL.NE.0) THEN
         STMULS = STEP
         CORR=0.0001*(STEP/RADL)*(GETOT/(VECT(7)*VECT(7)))**2
         IF (CORR.GT.0.25) CORR = 0.25
         STEP  = (1.+CORR)*STEP
      ENDIF
*
      SLENG = SLENG + STEP
*
*  **  Apply synchrotron radiation if required
*
      IF(ISYNC*IFIELD.NE.0) THEN
         CALL GSYNC
         NMEC = NMEC+1
         LMEC(NMEC) = 108
      ENDIF
*
* *** Generate Cherenkov photons if required
*
C      IF(IMCKOV.EQ.1) THEN
C         CALL GGCKOV
C         IF(NGPHOT.NE.0) THEN
C            NMEC = NMEC+1
C            LMEC(NMEC)=105
C         ENDIF
C      ENDIF
*
* *** Apply energy loss : find the kinetic energy corresponding
*      to the new stopping range = stopmx - step
*
      IF (ILOSL.NE.0) THEN
         NMEC = NMEC +1
         IF(GEKRAT.LT.0.7) THEN
            I1 = MAX(IEKBIN-1,1)
         ELSE
            I1 = MIN(IEKBIN,NEKBIN-1)
         ENDIF
         I1 = 3*(I1-1)+1
         XCOEF1 = Q(JCOEF+I1)
         XCOEF2 = Q(JCOEF+I1+1)
         XCOEF3 = Q(JCOEF+I1+2)
         IF(XCOEF1.NE.0.) THEN
            STOPMX = -XCOEF2+SIGN(ONE,XCOEF1)*SQRT(XCOEF2**2 - (XCOEF3-
     +      GEKIN/XCOEF1))
         ELSE
            STOPMX = - (XCOEF3-GEKIN)/XCOEF2
         ENDIF
*
         STOPRG = STOPMX - STEP
         IF (STOPRG.LT.STOPC) THEN
            STEP = MAX(STOPMX - STOPC,ZERO)
            GO TO 100
         ENDIF
*
         LMEC(NMEC) = 3
         IF(XCOEF1.NE.0.) THEN
            DEMEAN=GEKIN-XCOEF1*(XCOEF3+STOPRG*(2.*XCOEF2+STOPRG))
         ELSE
            DEMEAN=GEKIN-XCOEF2*STOPRG-XCOEF3
         ENDIF
         IF(DEMEAN.LE.5.*GEKIN*EPSMAC) THEN
            DEMEAN=(GEKRT1*Q(JLOSS+IEKBIN)+GEKRAT*Q(JLOSS+IEKBIN+1))
     +     *STEP
         ENDIF
         IF (ILOSS.EQ.4.OR.IEKBIN.LE.IKCUT+1) THEN
            DESTEP = DEMEAN
         ELSE
            DEMS = DEMEAN
            CALL GFLUCT (DEMS,DESTEP)
         ENDIF
         DESTEP=MAX(DESTEP,0.)
         GEKINT = GEKIN -DESTEP
         IF (GEKINT.LE.(1.01*CUTELE)) GO TO 100
         DESTEL = DESTEP
         GEKIN  = GEKINT
         GETOT  = GEKIN +AMASS
         VECT(7)= SQRT((GETOT+AMASS)*GEKIN)
         CALL GEKBIN
*
*        IABAN = 1 does not distinguish between sensitive and
*        non-sensitive volumes, and can stop particles above the CUTE
*
         IF(IABAN.EQ.1) THEN
*
*        STOP electron/positron if range < safety AND no brems
*
           IF(STOPMX.LE.SAFETY) THEN
             IF(SBREM.GT.STOPMX) THEN
*          + condition in case of Cherenkov generation:
*          STOP if E/p >refractive index (i.e. e+/e- is below threshold)
               IF(IMCKOV.EQ.1) THEN
                 THRIND=GETOT/VECT(7)
                 IF(THRIND.GT.Q(JINDEX+1)) GOTO 98
               ELSE
                 GOTO 98
               ENDIF
             ENDIF
           ENDIF
           STOPRG = STOPMX - STEP
           IF (STOPRG.LT.STOPC) THEN
            STEP = MAX(STOPMX - STOPC,ZERO)
            GO TO 98
           ENDIF
         END IF
*
*        IABAN = 2 distinguishes between sensitive and non-sensitive
*        volumes.
*        In sensitive volumes additional tests are applied before
*        the particle is stopped
*
         IF(IABAN.EQ.2) THEN
           IF(ISVOL.LE.0) THEN
             IF(STOPMX.LE.SAFETY) THEN
*
*        test for brems and annihilation only
*
               IF((IBREM.GT.0).AND.(SBREM.LE.STOPMX)) GOTO 97
               IF((CHARGE.GT.0.).AND.(IANNI.GT.0).and.
     +                                 (SANNI.LE.STOPMX)) GOTO 97
               GOTO 98
             END IF
           ELSE
*
*        sensitive volume ---> more tests !!
*        is energy below TLIM (=200 keV ) ?
*
             IF(GEKIN.LE.TLIM) THEN
*
*     range of the particle is the overestimated stopping range here
*
                TOSTOP=STOPMX-STEP*DESTEP/DEMEAN-STOPC
*
*        does the track remain in the actual volume?
*
                IF(TOSTOP.LE.SAFETY) THEN
*
*        is there no delta ray, brems, annihilation ?
*
                  IF((IDRAY.GT.0).AND.(SDRAY.LE.TOSTOP)) GOTO 97
                  IF((IBREM.GT.0).AND.(SBREM.LE.TOSTOP)) GOTO 97
                  IF((CHARGE.GT.0.).AND.(IANNI.GT.0).and.
     +                                   (SANNI.LE.TOSTOP)) GOTO 97
*
*        extra condition in case of Cherenkov generation:
*
                  IF(IMCKOV.EQ.1) THEN
                     THRIND=GETOT/VECT(7)
*
*        continue only if e+/e- below threshold
*
                     IF(THRIND.LT.Q(JINDEX+1)) GOTO 97
                   ELSE
*
*        do not make transport if this estimated range negative...
*
                     IF(TOSTOP.LE.0.) GOTO 98
*
*        estimate final position/direction of the particle
*        from energy loss + multiple scattering
*       ( multiple scattering with path length = range of the particle)
*
                     ALFA=0.18*Z
                     ALFA1=ALFA+1.
                     TGTH2=ALFA*ALFA1/(1.2+ALFA)
                     S=TOSTOP*SQRT(1.+TGTH2)/ALFA1
                     TGTH=SQRT(TGTH2)
                     THET=ATAN(TGTH)
                     IF(THET.Lt.0.) THET=PI-THET
*
*        correct direction
*
                     CT=COS(THET)
                     ST=SQRT(1.-CT*CT)
                     CALL GRNDM(RNDM,1)
                     PHI=TWOPI*RNDM(1)
*
                     D1=ST*COS(PHI)
                     D2=ST*SIN(PHI)
                     D3=CT
                     VMM=SQRT(VECT(4)*VECT(4)+VECT(5)*VECT(5))
                     IF(VMM.NE.0.) THEN
                       PD1=VECT(4)/VMM
                       PD2=VECT(5)/VMM
                       V4=PD1*VECT(6)*D1-PD2*D2+VECT(4)*D3
                       V5=PD2*VECT(6)*D1+PD1*D2+VECT(5)*D3
                       V6=-VMM*D1+VECT(6)*D3
                     ELSE
                       V4=D1
                       V5=D2
                       V6=D3
                     ENDIF
                     VP=1./SQRT(V4*V4+V5*V5+V6*V6)
                     VECT(4)=V4*VP
                     VECT(5)=V5*VP
                     VECT(6)=V6*VP
*
*       transport particle ( assuming FIELD = 0. )
*
                     DO 123 I=1,3
                     VECT(I)=VECT(I)+S*VECT(I+3)
123                  CONTINUE
*
*       put back into GEKIN the original value in order to have
*       a correct DESTEP
*
                  GOTO 98
                END IF
              END IF
            END IF
*++++++++++++++++++++++++++++++++++++++++++++++++++++++
          END IF
97        CONTINUE
        ENDIF
      ENDIF
*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*
* *** Apply multiple scattering.
*
      IF (IMULL.NE.0) THEN
         NMEC = NMEC +1
         LMEC(NMEC) = 2
         CALL GMULTS
      ENDIF
*
* *** Update time of flight
*
      TOFG = TOFG +STEP*GETOT/(VECT(7)*CLIGHT)
      IF (TOFG.GE.TOFMAX) THEN
         ISTOP = 4
         NMEC  = NMEC +1
         LMEC(NMEC) = 22
         GO TO 999
      ENDIF
*
* *** Update interaction probabilities
*
      IF (IBREM.GT.0)    ZINTBR = ZINTBR -STEP/STEPBR
      IF (IDRAY.GT.0)    ZINTDR = ZINTDR -STEP/STEPDR
      IF (CHARGE.GT.0.) THEN
         IF (IANNI.GT.0) ZINTAN = ZINTAN -STEP/STEPAN
      ENDIF
*
* *** Apply the selected process if any
*
   90 IF (IPROC.EQ.0) GO TO 999
      NMEC = NMEC +1
      LMEC(NMEC) = IPROC
*
*  **   Bremsstrahlung ?
*
      IF (IPROC.EQ.9) THEN
         CALL GBREME
*
*  **   Delta ray ?
*
      ELSE IF (IPROC.EQ.10) THEN
*
       IF((IPART.EQ.2).OR.((IPART.EQ.3).AND.(GEKIN.GT.2.*DCUTE))) THEN
         CALL GDRAY
       ELSE
         GOTO 98
       ENDIF
*
*  **   Positron annihilation ?
*
      ELSE IF (IPROC.EQ.11) THEN
         CALL GANNI
      ENDIF
*
* *** Update the total energy, if the electron has been accelrated
*
      IF(ABS(DP) .GT. 1.E-6)THEN
        VECT(7) = VECT(7) + DP
        GETOT = SQRT(VOUT(7)**2+AMASS*AMASS)
        GEKIN = GETOT - AMASS
        CALL GEKBIN
      ENDIF
*
      GO TO 999
*
* *** Special treatment for overstopped tracks
*
  98  GEKIN=GEKIN+DESTEP
  100 DESTEP = GEKIN
      DESTEL = DESTEP
      GEKIN  = 0.
      GETOT  = AMASS
      VECT(7)= 0.
      INWVOL = 0
      NMEC   = NMEC +1
      LMEC(NMEC) = 30
      IF ((CHARGE.LT.0.).OR.(IANNI.EQ.0)) THEN
         ISTOP = 2
      ELSE
         NMEC = NMEC +1
         LMEC(NMEC) = 11
         CALL GANNIR
      ENDIF
  999 IF(NGPHOT.GT.0) THEN
         IF(ITCKOV.EQ.2.AND.ISTOP.EQ.0) THEN
*
*  The electron has produced Cerenkov photons and it is still alive
*  we put it in the stack and we let the photons to be tracked
            NGKINE = NGKINE+1
            GKIN(1,NGKINE) = VECT(4)*VECT(7)
            GKIN(2,NGKINE) = VECT(5)*VECT(7)
            GKIN(3,NGKINE) = VECT(6)*VECT(7)
            GKIN(4,NGKINE) = GETOT
            GKIN(5,NGKINE) = IPART
            TOFD(NGKINE) = 0.
            ISTOP = 1
c----put position as well
            GPOS(1,NGKINE)=VECT(1)
            GPOS(2,NGKINE)=VECT(2)
            GPOS(3,NGKINE)=VECT(3)
         ENDIF
      ENDIF
      END
CDECK  ID>, GTGAMA. 
*CMZ :          06/03/96  10.38.21  by  L. Felawka
*         Bug fix - return BIG instead of 1/BIG when cross section is zero
*CMZ :  1.00/00 16/11/94  09.29.32  by  L. Felawka
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTGAMA
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Photon track. Computes step size and propagates particle     *
C.    *    through step.                                               *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *      Authors    R.Brun, F.Bruyant L.Urban ********             *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      PARAMETER (EPSMAC=1.E-6)
      DOUBLE PRECISION ONE,XCOEF1,XCOEF2,XCOEF3,ZERO
      PARAMETER (ONE=1,ZERO=0)
      PARAMETER (EPCUT=1.022E-3)
*
      SAVE IRAB, A, DENS
      IABAN = NINT(DPHYS1)
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ?  Short circuit
*
*
      IF (GEKIN.LE.CUTGAM) GOTO 998
*
* *** Update local pointers if medium has changed
*
      IF(IUPD.EQ.0)THEN
         IUPD  = 1
         JMIXT = LQ(JMA-5)
         JPHOT = LQ(JMA-6)
         JCOMP = LQ(JMA-8)
         JPAIR = LQ(JMA-10)
         JPFIS = LQ(JMA-12)
         JRAYL = LQ(JMA-13)
         IRAB  = NINT(Q(JTM+15))
         IF (IRAB.GT.0) THEN
            IF (JMIXT.GT.0) THEN
               NLM    =  NINT(Q(JMA+11))
               A      =  Q(JMIXT+IRAB)
               DENS   =  Q(JMA+8)*Q(JMIXT+2*NLM+IRAB)
            ELSE
               A      =  Q(JMA+6)
               DENS   =  Q(JMA+8)
            ENDIF
         ENDIF
         IF(IMCKOV.EQ.1) THEN
            JTCKOV = LQ(JTM-3)
            JABSCO = LQ(JTCKOV-1)
            JEFFIC = LQ(JTCKOV-2)
            JINDEX = LQ(JTCKOV-3)
            JCURIN = LQ(JTCKOV-4)
            NPCKOV = Q(JTCKOV+1)
         ENDIF
      ENDIF
*
* *** Compute current step size
*
      IPROC  = 103
      STEP   = STEMAX
      GEKRT1 = 1 .-GEKRAT
*
*  **   Step limitation due to pair production ?
*
      IF (GETOT.GT.EPCUT) THEN
         IF (IPAIR.GT.0) THEN
            STEPPA = GEKRT1*Q(JPAIR+IEKBIN) +GEKRAT*Q(JPAIR+IEKBIN+1)
            SPAIR  = STEPPA*ZINTPA
            IF (SPAIR.LT.STEP) THEN
               STEP  = SPAIR
               IPROC = 6
            ENDIF
         ENDIF
      ENDIF
*
*  **   Step limitation due to Compton scattering ?
*
      IF (ICOMP.GT.0) THEN
         STEPCO = GEKRT1*Q(JCOMP+IEKBIN) +GEKRAT*Q(JCOMP+IEKBIN+1)
         SCOMP  = STEPCO*ZINTCO
         IF (SCOMP.LT.STEP) THEN
            STEP  = SCOMP
            IPROC = 7
         ENDIF
      ENDIF
*
*  **   Step limitation due to photo-electric effect ?
*
      IF (GEKIN.LT.0.4) THEN
         IF (IPHOT.GT.0) THEN
            STEPPH = GEKRT1*Q(JPHOT+IEKBIN) +GEKRAT*Q(JPHOT+IEKBIN+1)
            SPHOT  = STEPPH*ZINTPH
            IF (SPHOT.LT.STEP) THEN
               STEP  = SPHOT
               IPROC = 8
            ENDIF
         ENDIF
      ENDIF
*
*  **   Step limitation due to photo-fission ?
*
      IF (JPFIS.GT.0) THEN
*
*  **   Step limitation due to resonant photo-absorption on Nitrogen-14 ?
*
         IF (IRAB.GT.0) THEN
            STEPPF = SIGN14R(GEKIN,A,DENS)
            IF (STEPPF.GT.0.) THEN
               STEPPF = 1./STEPPF
            ELSE
               STEPPF = BIG
            ENDIF
         ELSE
            STEPPF = GEKRT1*Q(JPFIS+IEKBIN) +GEKRAT*Q(JPFIS+IEKBIN+1)
         ENDIF
         SPFIS  = STEPPF*ZINTPF 
         IF (SPFIS.LT.STEP) THEN
            STEP  = SPFIS
            IPROC = 23
         ENDIF
      ENDIF
*
*  **   Step limitation due to Rayleigh scattering ?
*
      IF (IRAYL.GT.0) THEN
         IF (GEKIN.LT.0.01) THEN
            STEPRA = GEKRT1*Q(JRAYL+IEKBIN) +GEKRAT*Q(JRAYL+IEKBIN+1)
            SRAYL  = STEPRA*ZINTRA
            IF (SRAYL.LT.STEP) THEN
               STEP  = SRAYL
               IPROC = 25
            ENDIF
         ENDIF
      ENDIF
*
      IF (STEP.LT.0.) STEP = 0.
*
*  **   Step limitation due to geometry ?
*
      IF (STEP.GE.SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
            STEP   = SNEXT + PREC
            INWVOL= 2
            IPROC = 0
            NMEC =  1
            LMEC(1)=1
         ENDIF
*
*        Update SAFETY in stack companions, if any
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 10 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST = JSTAK +3 +(IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   10       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
*
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
* *** Linear transport
*
      IF (INWVOL.EQ.2) THEN
         DO 20 I = 1,3
            VECTMP  = VECT(I) +STEP*VECT(I+3)
            IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
               IF(VECT(I+3).NE.0.) THEN
                  VECTMP = VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*
     +            EPSMAC
                  IF(NMEC.GT.0) THEN
                     IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                  ENDIF
                  NMEC=NMEC+1
                  LMEC(NMEC)=104
               ENDIF
            ENDIF
            VECT(I) = VECTMP
   20    CONTINUE
      ELSE
         DO 30 I = 1,3
            VECT(I)  = VECT(I) +STEP*VECT(I+3)
   30    CONTINUE
      ENDIF
*
      SLENG = SLENG +STEP
*
* *** Update time of flight
*
      TOFG = TOFG +STEP/CLIGHT
*
* *** Update interaction probabilities
*
      IF (GETOT.GT.EPCUT) THEN
         IF (IPAIR.GT.0) ZINTPA = ZINTPA -STEP/STEPPA
      ENDIF
      IF (ICOMP.GT.0)    ZINTCO = ZINTCO -STEP/STEPCO
      IF (GEKIN.LT.0.4) THEN
         IF (IPHOT.GT.0) ZINTPH = MAX(ZINTPH -STEP/STEPPH,0.0)
      ENDIF
      IF (JPFIS.GT.0)    ZINTPF = ZINTPF -STEP/STEPPF
      IF (IRAYL.GT.0) THEN
         IF (GEKIN.LT.0.01) ZINTRA = ZINTRA -STEP/STEPRA
      ENDIF
*
      IF (IPROC.EQ.0) GO TO 999
      NMEC = 1
      LMEC(1) = IPROC
*
*  ** Pair production ?
*
      IF (IPROC.EQ.6) THEN
         CALL GPAIRG
*
*  ** Compton scattering ?
*
      ELSE IF (IPROC.EQ.7) THEN
         CALL GCOMP
*
*  ** Photo-electric effect ?
*
      ELSE IF (IPROC.EQ.8) THEN
*
*
*        IABAN > 0  can stop particles above the CUTE
*
       IF(IABAN.GT.0) THEN
* 
*        STOP gamma IF estimated_range(photoelectron) < safety
*
*
*       Calculate range of the photoelectron ( with kin. energy Ephot)
*
        IF(GEKIN.LE.0.001)  THEN
            JCOEF = LQ(JMA-17)
         IF(GEKRAT.LT.0.7) THEN
            I1 = MAX(IEKBIN-1,1)
         ELSE
            I1 = MIN(IEKBIN,NEKBIN-1)
         ENDIF
         I1 = 3*(I1-1)+1
         XCOEF1 = Q(JCOEF+I1)
         XCOEF2 = Q(JCOEF+I1+1)
         XCOEF3 = Q(JCOEF+I1+2)
         IF(XCOEF1.NE.0.) THEN
            STOPMX = -XCOEF2+SIGN(ONE,XCOEF1)*SQRT(XCOEF2**2 - (XCOEF3-
     +      GEKIN/XCOEF1))
         ELSE
            STOPMX = - (XCOEF3-GEKIN)/XCOEF2
         ENDIF
*
*        DO NOT call GPHOT if this (overestimated) range is smaller
*                than SAFETY
*
         IF (STOPMX.LE.SAFETY) GOTO 998
        ENDIF
*
       ENDIF

         CALL GPHOT
*
*  ** Rayleigh effect ?
*
      ELSE IF (IPROC.EQ.25) THEN
         CALL GRAYL
*
*  ** Photo-fission ?
*
      ELSE IF (IPROC.EQ.23) THEN
         CALL GPFIS
*
      ENDIF
*
         GOTO 999
998      DESTEP = GEKIN
         GEKIN  = 0.
         GETOT  = 0.
         VECT(7)= 0.
         ISTOP  = 2
         NMEC   = 1
         LMEC(1)= 30
  999 END
CDECK  ID>, GFTMAT.
*CMZ :          07/08/97  22.19.13  by  Art Olin
*CMZ :  3.21/04 11/01/95  08.57.12  by  S.Ravndal
*-- Author :
      SUBROUTINE GFTMAT(IMATE,IPATT,CHMECA,KDIM,TKIN,VALUE,PCUT,IXST)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       FETCH and INTERPOLATE the DE/DX and Cross sections       *
C.    *       tabulated in JMATE banks coresponding to  :              *
C.    *       material IMATE, particle IPATT, mecanism name CHMECA,    *
C.    *       kinetic energies TKIN                                    *
C.    *                                                                *
C.    *      The CHMECAnism name can be :                              *
C.    *      'HADF'   'INEF'   'ELAF'   'FISF'   'CAPF'                *
C.    *      'HADG'   'INEG'   'ELAG'   'FISG'   'CAPG'                *
C.    *      'LOSS'   'PHOT'   'ANNI'   'COMP'   'BREM'                *
C.    *      'PAIR'   'DRAY'   'PFIS'   'RAYL'   'HADG'                *
C.    *      'MUNU'   'RANG'   'STEP'                                  *
C.    *                                                                *
C.    *       For Hadronic particles it also computes the              *
C.    *       hadronic cross section from FLUKA ( '***F' ) or          *
C.    *       GHEISHA ( '***G' ) programs:                             *
C.    *       HADF or HADG -- total                                    *
C.    *       INEF or INEG -- inelastic                                *
C.    *       ELAF or ELAG -- elastic                                  *
C.    *       FISF or FISG -- fission (0.0 for FLUKA)                  *
C.    *       CAPF or CAPG -- neutron capture (0.0 for FLUKA)          *
C.    *                                                                *
C.    *             Input parameters                                   *
C.    *  IMATE   Geant material number                                 *
C.    *  IPATT   Geant particle number                                 *
C.    *  CHMECA   mechanism name of the bank to be fetched             *
C.    *  KDIM   dimension of the arrays TKIN , VALUE                   *
C.    *  TKIN   array of kinetic energy of incident particle (in Gev)  *
C.    *                                                                *
C.    *             Output parameters                                  *
C.    *  VALUE  array of energy loss (in Mev/cm) ,                     *
C.    *               or stopping range (cm) ,or continuous step (cm)  *
C.    *               or macroscopic cross section (in 1/cm)           *
C.    *  PCUT(5)  array of the physical cuts in material IMATE  (Gev)  *
C.    *  IXST   flag = 1 if the array VALUE is filled ,  =0 otherwise  *
C.    *                                                                *
C.    *    ==>Called by : <USER>  GPLMAT  GRPMAT                       *
C.    *       Authors    R.Brun, M.Maire    *********                  *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      REAL          PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      REAL          EMMU,PMASS,AVO
C
      COMMON/GCONST/PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      COMMON/GCONSX/EMMU,PMASS,AVO
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      INTEGER K0FLAG
      REAL AIEL,AIIN,AIFI,AICA,ALAM
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      COMMON/ GFKDIS/ ZINE, ZELA, ZTOT, INT
     +               ,SINE, SELA, FSIG, IFMAT, IGF
C
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
*
      LOGICAL CERKOV
      CHARACTER*4  CHMECA
      DIMENSION TKIN(KDIM), VALUE(KDIM), PCUT(5)
      PARAMETER(NMECA=45,IBLOWN=23)
      CHARACTER*4 CHNMEC(NMECA)
*
* *** Two blank spaces for FISF and CAPF which do not exist
*
* *** The low-neutron cross-sections have to be at the end for the
* *** logics of the routine to work, and they have to start at
* *** IBLOWN
      DATA CHNMEC /
     +     'HADF',  'INEF',  'ELAF',  'NULL',  'NULL',
     +     'HADG',  'INEG',  'ELAG',  'FISG',  'CAPG',
     +     'LOSS',  'PHOT',  'ANNI',  'COMP',  'BREM',
     +     'PAIR',  'DRAY',  'PFIS',  'RAYL',  'MUNU',
     +     'RANG',  'STEP',  'LOWN',  'NABS',  'NELA',
     +     'NINE',  'NINC',  'N,2N',  'N,3N',  'N,NA',
     +     'N2NA',  'N,NP',  'NFIS',  'N,GA',  'N,PR',
     +     'N,DE',  'N,TR',  'N3HE',  'N,AL',  'N,2A',
     +     'N,3A',  'N,2P',  'N,PA',  'NT2A',  'ND2A'/
*
*KEND.
      DOUBLE PRECISION ONE,TWOTHR
      PARAMETER (ONE=1)
      PARAMETER (TWOTHR=2*ONE/3)
*
*     ------------------------------------------------------------------
*
      IXST = 0
      IF(KDIM.LE.0) GO TO 999
      IMECA = 0
      DO 10  KMECA=1,NMECA
         IF(CHMECA.EQ.CHNMEC(KMECA)) THEN
            IMECA = KMECA
         ENDIF
   10 CONTINUE
      IF(IMECA.EQ.0) THEN
         WRITE(CHMAIL,'('' *** GFTMAT: Mechanism '',A,                 '
     +   //'    ''not implemented'')') CHMECA
         CALL GMAIL(0,0)
         GOTO 999
      ENDIF
      DO 20  IDIM=1, KDIM
         VALUE(IDIM)=0.
   20 CONTINUE
      DO 30  ICUT=1,5
         PCUT(ICUT)=0.
   30 CONTINUE
*
      IF(JMATE.LE.0) GO TO 999
      IF(IMATE.LE.0) GO TO 999
      IF(IMATE.GT.NMATE) GO TO 110
      JMA = LQ(JMATE-IMATE)
      IF(JMA.LE.0) GO TO 110
      A      =  Q(JMA+6)
      Z      =  Q(JMA+7)
      IF(Z.LT.1.) GO TO 999
      DENS   =  Q(JMA+8)
      RADL   =  Q(JMA+9)
      NLM    =  Q(JMA+11)
      JPROB  = LQ(JMA-4)
      AZRO   =  Q(JPROB+8)
      AHEFF  =  A
      IF(NLM .GT.1) THEN
         JMIXT = LQ(JMA-5)
         JMI1  = LQ(JMIXT-1)
         AHEFF =  Q(JMI1+1)
      ENDIF
*
      IF(JTMED.LE.0) GO TO 999
      IF(NTMED.LE.0) GO TO 999
      JBANK = JTMED
      DO 40 ITM = 1,NTMED
         JTM = LQ(JTMED-ITM)
         IF(JTM.LE.0) GO TO 40
         JTMN =  0
         IMAT =  Q(JTM+6)
         IF(IMAT.EQ.IMATE) THEN
            JTMN = LQ(JTM)
            IF(JTMN.NE.0) JBANK = JTMN
            GO TO 50
         ENDIF
   40 CONTINUE
   50 CALL UCOPY( Q(JBANK+6),PCUT(1),5)
      CUTHAD = Q(JBANK+ 4)
      ILOSS  = Q(JBANK+21)
      IMULS  = Q(JBANK+22)
      IFIELD = Q(JTM +  8)
      FIELDM = Q(JTM +  9)
      TMAXFD = Q(JTM + 10)
      STEMAX = Q(JTM + 11)
      DEEMAX = Q(JTM + 12)
      STMIN  = Q(JTM + 14)
*
      IF(JPART.LE.0) GO TO 999
      IF(IPATT.LE.0) GO TO 999
      IF(IPATT.GT.NPART) GO TO 110
      JPA = LQ(JPART-IPATT)
      IF(JPA.LE.0) GO TO 110
      ITYPE  =  Q(JPA+6)
      AMASS  =  Q(JPA+7)
      CHARGE =  Q(JPA+8)
      CHARG1 = CHARGE
      CHARG2 = CHARGE*CHARGE
      CHAR23 = ONE/CHARGE**TWOTHR
*
* *** Find the correct pointer
*
      JBANK  = 0
      ISHIF  = 0
      RMASS  = 1.
*
* *** Photons
*
      IF (ITYPE.EQ.1) THEN
         IF (CHMECA.EQ.'PHOT') JBANK = LQ(JMA- 6)
         IF (CHMECA.EQ.'COMP') JBANK = LQ(JMA- 8)
         IF (CHMECA.EQ.'PAIR') JBANK = LQ(JMA-10)
         IF (CHMECA.EQ.'PFIS') JBANK = LQ(JMA-12)
         IF (CHMECA.EQ.'RAYL') JBANK = LQ(JMA-13)
*
* *** Electrons / positons
*
      ELSE IF (ITYPE.EQ.2) THEN
         IF (CHMECA.EQ.'LOSS') THEN
            JBANK = LQ(JMA- 1)
            IF (CHARGE.GT.0.) ISHIF = NEK1
         ELSE IF (CHMECA.EQ.'RANG') THEN
            JBANK = LQ(JMA- 15)
            IF (CHARGE.GT.0.) ISHIF = NEK1
         ELSE IF (CHMECA.EQ.'STEP') THEN
            JBANK = LQ(JTM- 1)
         ELSE IF ((CHMECA.EQ.'ANNI').AND.(CHARGE.GT.0.)) THEN
            JBANK = LQ(JMA- 7)
         ELSE IF (CHMECA.EQ.'BREM') THEN
            JBANK = LQ(JMA- 9)
         ELSE IF (CHMECA.EQ.'DRAY') THEN
            JBANK = LQ(JMA-11)
            IF (CHARGE.GT.0.) ISHIF = NEK1
         ENDIF
*
* *** Neutral hadrons (***F for FLUKA cross sections
*                      ***G for GHEISHA cross sections
*                      LOWN and N*** for MICAP cross sections)
*
      ELSE IF (ITYPE.EQ.3) THEN
         IF((CHMECA.EQ.'HADF').OR.(CHMECA.EQ.'INEF')
     +   .OR.(CHMECA.EQ.'ELAF') .OR.(CHMECA.EQ.'FISF')
     +   .OR.(CHMECA.EQ.'CAPF')) THEN
            JBANK = -3
            IF (IFINIT(5) .EQ. 0) CALL FLINIT
         ELSE IF((CHMECA.EQ.'HADG').OR.(CHMECA.EQ.'INEG')
     +   .OR.(CHMECA.EQ. 'ELAG').OR.(CHMECA.EQ.'FISG')
     +   .OR.(CHMECA.EQ.'CAPG')) THEN
            JBANK = -4
            CALL GHEINI
         ELSE IF(IMECA.GE.IBLOWN.AND.IPATT.EQ.13) THEN
            IF (IFINIT(7) .EQ. 0) CALL GMORIN
            JBANK = -5
         ENDIF
         K0OLD = K0FLAG
*
* *** Charged hadrons (***F for FLUKA cross sections
*                      ***G for GHEISHA cross sections)
* *** Heavy ions
*
      ELSE IF (ITYPE.EQ.4.OR.ITYPE.EQ.8) THEN
         RMASS = PMASS/AMASS
         IF (CHMECA.EQ.'LOSS') THEN
            JBANK = LQ(JMA- 3)
         ELSE IF (CHMECA.EQ.'RANG') THEN
            JBANK = LQ(JMA- 16) + NEK1
         ELSE IF (CHMECA.EQ.'STEP') THEN
            JBANK = -1
            JRANG = LQ(JMA -16) + NEK1
            CUTPRO = CUTHAD*RMASS
            CUTPRO = MAX(ELOW(1), MIN( CUTPRO, ELOW(NEK1)*0.99))
            IKCUT  = GEKA*LOG10(CUTPRO) + GEKB
            GKC = (CUTPRO - ELOW(IKCUT))/(ELOW(IKCUT+1) - ELOW(IKCUT))
            STOPC = (1.-GKC)*Q(JRANG+IKCUT) + GKC*Q(JRANG+IKCUT+1)
         ELSE IF (CHMECA.EQ.'DRAY') THEN
            JBANK = -2
            JPROB = LQ(JMA-4)
            AZRO  =  Q(JPROB+17)
            DCUTM =  PCUT(4)
         ELSE IF(((CHMECA.EQ.'HADF').OR.(CHMECA.EQ.'INEF')
     +   .OR.(CHMECA.EQ. 'ELAF').OR.(CHMECA.EQ.'FISF')
     +   .OR.(CHMECA.EQ.'CAPF')).AND.ITYPE.NE.8) THEN
            JBANK = -3
            IF (IFINIT(5) .EQ. 0) CALL FLINIT
         ELSE IF(((CHMECA.EQ.'HADG').OR.(CHMECA.EQ.'INEG')
     +   .OR.(CHMECA.EQ. 'ELAG').OR.(CHMECA.EQ.'FISG')
     +   .OR.(CHMECA.EQ.'CAPG')).AND.ITYPE.NE.8) THEN
            JBANK = -4
            CALL GHEINI
         ENDIF
         K0OLD = K0FLAG
*
* *** Muons
*
      ELSE IF (ITYPE.EQ.5) THEN
         IF (CHMECA.EQ.'LOSS') THEN
            JBANK = LQ(JMA- 2)
         ELSE IF (CHMECA.EQ.'RANG') THEN
            JBANK = LQ(JMA- 16)
         ELSE IF (CHMECA.EQ.'STEP') THEN
            JBANK = LQ(JTM- 2)
         ELSE IF (CHMECA.EQ.'MUNU') THEN
            JBANK = LQ(JMA- 14)
         ELSE IF (CHMECA.EQ.'BREM') THEN
            JBANK = LQ(JMA- 9)
            ISHIF = 2*NEK1
         ELSE IF (CHMECA.EQ.'PAIR') THEN
            JBANK = LQ(JMA- 10)
            ISHIF = NEK1
         ELSE IF (CHMECA.EQ.'DRAY') THEN
            JBANK = LQ(JMA-11)
            ISHIF = 2*NEK1
         ENDIF
*
* *** Geantinos
*
      ELSEIF (ITYPE.EQ.6) THEN
         WRITE(CHMAIL,10000)
         CALL GMAIL(0,0)
         JBANK = 0
*
* *** Cerenkov
*
      ELSEIF (ITYPE.EQ.7) THEN
         IF (CHMECA.EQ.'LABS') THEN
*
* *** Not implemented yet!
            JBANK=0
         ENDIF
*
      ENDIF
      CERKOV=.FALSE.
      IF(CHARGE.NE.0.AND.ITCKOV.NE.0) THEN
         IF(IQ(JTM-2).GE.3) THEN
            IF(LQ(JTM-3).NE.0.AND.LQ(LQ(JTM-3)-3).NE.0) THEN
*
* ***  In this tracking medium Cerenkov photons are generated and
* ***  tracked. Set to 1 the corresponding flag and calculate the
* ***  relevant pointers.
*
               CERKOV = .TRUE.
               JTCKOV = LQ(JTM-3)
               JABSCO = LQ(JTCKOV-1)
               JEFFIC = LQ(JTCKOV-2)
               JINDEX = LQ(JTCKOV-3)
               JCURIN = LQ(JTCKOV-4)
               NPCKOV = Q(JTCKOV+1)
            ENDIF
         ENDIF
      ENDIF

      IF(JBANK.EQ.0 .AND. NINT(A).NE.14) GO TO 999
      IXST = 1
*
*
      JBANK = JBANK + ISHIF
      DO 100 IKB = 1,KDIM
*        Find bin number in table JMATE
         EKP = TKIN(IKB)*RMASS
         EKP = MAX(ELOW(1), MIN( EKP, ELOW(NEK1)*0.99))
         IKP=GEKA*LOG10(EKP)+GEKB +0.001
         GKRA=(EKP-ELOW(IKP))/(ELOW(IKP+1)-ELOW(IKP))
*
         IF(JBANK.GT.0) THEN
*           Retieve value from bank JMATE
            VALUE(IKB) =  (1.-GKRA)*Q(JBANK+IKP) + GKRA*Q(JBANK+IKP+1)
            IF ((CHMECA.EQ.'PHOT').AND.(EKP.GE.0.05 )) VALUE(IKB) =
     +      BIG
            IF ((CHMECA.EQ.'MUNU').AND.(EKP.LT.0.05 )) VALUE(IKB) =
     +      BIG
            IF ( CHMECA.EQ.'LOSS') THEN
              IF((ITYPE .EQ. 4) .OR. (ITYPE .EQ. 8)) THEN
*
* *** Compute energy dependent effective charge for heavy ions
*
		 TETOT = TKIN(IKB) +AMASS
	         TAMASS=TETOT+AMASS
	         BET2=TKIN(IKB)*TAMASS/(TETOT*TETOT)
	         BET=SQRT(BET2)
	         W1=1.034-0.1777*EXP(-0.08114*CHARGE)
	         W2=BET*CHAR23
	         W3=121.4139*W2+0.0378*SIN(190.7165*W2)
	         CHARG1=CHARGE*(1.-W1*EXP(-W3))
*
*              the effective charge  CHARG1
*            can be negative only for very low energy and
*     for CHARGE > 20 ( very low energy : T/A < 20 keV/nucleon)
*              in this case short circuit
*
	         IF(CHARG1.LT.0.) GOTO 999
	         CHARG2=CHARG1**2
*
              ENDIF ! HEAVY ION
              VALUE(IKB) = VALUE(IKB)*CHARG2*1.E+3
            ELSE IF (CHMECA.EQ.'RANG') THEN
               VALUE(IKB) = VALUE(IKB)/(RMASS*CHARG2)
            ELSE IF (CHMECA.NE.'STEP') THEN
               IF (VALUE(IKB).GT.0.) THEN
                  VALUE(IKB) = 1./VALUE(IKB)
               ELSE
                  VALUE(IKB) = 1./BIG
               ENDIF
            ENDIF
*
         ELSEIF (JBANK.EQ.-1) THEN
*           Compute step due to muls + loss + field
            GEKIN=TKIN(IKB)
            GETOT=GEKIN+AMASS
            PMOM =SQRT(GEKIN*(GETOT+AMASS))
            SFIELD = BIG
            SMULS  = BIG
            SLOSS  = BIG
            STOPMX = BIG
            IF (IFIELD*FIELDM.NE.0.)
     +         SFIELD = 3333.*DEGRAD*TMAXFD*PMOM/ABS(FIELDM*CHARGE)
            IF (IMULS.GT.0.)
     +         SMULS = MIN (2232.*RADL*((PMOM**2)/(GETOT*CHARGE))**2 ,
     +                      10.*RADL )
            IF (ILOSS*DEEMAX.GT.0.) THEN
               STOPP  = (1.-GKRA)*Q(JRANG+IKP) + GKRA*Q(JRANG+IKP+1)
               STOPMX = (STOPP - STOPC)/(RMASS*CHARGE*CHARGE)
               IF (STOPMX.LT.0.) STOPMX = 0.
               EKF = MAX ( ELOW(1) , (1.-DEEMAX)*EKP )
               IKF = GEKA*LOG10(EKF) + GEKB
               GKF = (EKF-ELOW(IKF))/(ELOW(IKF+1)-ELOW(IKF))
               SLOSP= STOPP - (1.-GKF)*Q(JRANG+IKF) - GKF*Q(JRANG+IKF+1)
               SLOSS = SLOSP/(RMASS*CHARGE*CHARGE)
            ENDIF
            IF(CERKOV) THEN
               VECT(7)=SQRT(TKIN(IKB)*(TKIN(IKB)+2*AMASS))
               CALL GNCKOV
               STCKOV = MXPHOT/MAX(3.*DNDL,1E-10)
            ELSE
               STCKOV=BIG
            ENDIF
*
            IF (STOPMX.LE.STMIN) THEN
               VALUE(IKB) = STOPMX
            ELSE
               VALUE(IKB) = MAX(STMIN,MIN(STCKOV,SLOSS,SFIELD,SMULS,
     +         STEMAX))
            ENDIF
*
         ELSEIF (JBANK.EQ.-2) THEN
*           Compute delta ray cross section for hadrons
            GEKIN=TKIN(IKB)
            GETOT=GEKIN+AMASS
            GAMASS=GETOT+AMASS
            BET2=GEKIN*GAMASS/(GETOT*GETOT)
            TMAX=EMASS*GEKIN*GAMASS/(0.5*AMASS*AMASS+EMASS*GETOT)
            IF(TMAX.GT.DCUTM)THEN
               Y=DCUTM/TMAX
               SIG=(1.-Y+BET2*Y*LOG(Y))/DCUTM
               IF(AMASS.GT.0.9)SIG=SIG+0.5*(TMAX-DCUTM)/(GETOT*GETOT)
               VALUE(IKB)=SIG*AZRO*CHARGE*CHARGE*EMASS/BET2
            ELSE
               VALUE(IKB)=1./BIG
            ENDIF
*
*           compute hadronic cross section from FLUKA code
*
         ELSEIF (JBANK.EQ.-3) THEN
            GEKIN=TKIN(IKB)
            PMOM = SQRT(GEKIN*(GEKIN+2*AMASS))
            NMAT = IMATE
            VECT(7) = PMOM
            IF (IPATT.NE.IPART) THEN
                IOLDP = IPART
                IPART = IPATT
                CALL FLDIST
                IPART = IOLDP
            ELSE
                CALL FLDIST
            END IF
            IF (CHMECA.EQ.'HADF') VALUE(IKB)= FSIG
            IF (CHMECA.EQ.'INEF') VALUE(IKB)= SINE
            IF (CHMECA.EQ.'ELAF') VALUE(IKB)= SELA
            IF (CHMECA.EQ.'FISF') VALUE(IKB)= 0.0
            IF (CHMECA.EQ.'CAPF') VALUE(IKB)= 0.0
*
*           compute hadronic cross section from GHEISHA code
*
         ELSEIF (JBANK.EQ.-4) THEN
            GEKIN=TKIN(IKB)
            PMOM = SQRT(GEKIN*(GEKIN+2*AMASS))
            K0FLAG = 1
*           (compounds)
            IF(NLM.GT.1) THEN
               HHHH=0.
               IF (JTMN.GT.0) HHHH=Q(JTMN+26)
               VALUE(IKB)=GHESIG(PMOM,GEKIN,A,Q(JMIXT+1), Q(JMIXT+NLM+
     +         1),Q(JMIXT+2*NLM+1),NLM,DENS,HHHH,IPATT)
               IF (CHMECA .EQ. 'INEG') THEN
                  VALUE(IKB) = 0.
                  DO 60 K=1,NLM
                     VALUE(IKB) = AIIN(K) + VALUE(IKB)
   60             CONTINUE
               ELSE IF (CHMECA .EQ. 'ELAG') THEN
                  VALUE(IKB) = 0.
                  DO 70 K=1,NLM
                     VALUE(IKB) = AIEL(K) + VALUE(IKB)
   70             CONTINUE
               ELSE IF (CHMECA .EQ. 'FISG') THEN
                  VALUE(IKB) = 0.
                  DO 80 K=1,NLM
                     VALUE(IKB) = AIFI(K) + VALUE(IKB)
   80             CONTINUE
               ELSE IF (CHMECA .EQ. 'CAPG') THEN
                  VALUE(IKB) = 0.
                  DO 90 K=1,NLM
                     VALUE(IKB) = AICA(K) + VALUE(IKB)
   90             CONTINUE
               ENDIF
            ELSE
*           (simple elements)
               VALUE(IKB)=GHESIG(PMOM,GEKIN,A,A,Z,1.,1,DENS,0.,IPATT)
               IF (CHMECA .EQ. 'INEG') VALUE(IKB) = AIIN(1)
               IF (CHMECA .EQ. 'ELAG') VALUE(IKB) = AIEL(1)
               IF (CHMECA .EQ. 'FISG') VALUE(IKB) = AIFI(1)
               IF (CHMECA .EQ. 'CAPG') VALUE(IKB) = AICA(1)
            END IF
            K0FLAG = K0OLD
*
*           compute the cross-section for low-energy neutrons
*           from MICAP code
*
         ELSEIF (JBANK.EQ.-5) THEN
            GEKIN=TKIN(IKB)
            IF (GEKIN.LE..02) THEN
               IF (CHMECA .EQ. 'LOWN') THEN
                  VALUE(IKB) = SIGMOR(GEKIN*1.E+9,IMATE)
               ELSE
                  NMAT = IMATE
                  CALL GMXSEC (IMECA,VALUE(IKB))
               ENDIF
            ELSE
               VALUE(IKB) = 0.
            ENDIF
*
*           compute the cross-section for resonant photoabsorption on
*           Nitrogen-14
*
         ELSEIF (JBANK.EQ.0 .AND. NINT(A).EQ.14) THEN
            GEKIN=TKIN(IKB)
            VALUE(IKB) = SIGN14R(GEKIN,A,DENS)
*
         ENDIF
  100 CONTINUE
*
      GO TO 999
  110 WRITE(CHMAIL,10100) IMATE ,IPATT
      CALL GMAIL(0,0)
*
10000 FORMAT(' ***** GFTMAT: No processes active for geantinos')
10100 FORMAT(' ***** GFTMAT error : material',I4,
     +       '  or particle',I4,' not defined'   )
  999 END
CDECK  ID>, GPHYSI. 
*CMZ :  1.00/00 19/11/94  12.22.49  by  L. Felawka
*CMZ :  3.21/03 06/10/94  16.31.40  by  S.Ravndal
*-- Author :
      SUBROUTINE GPHYSI
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Initialise material constants for all the physics        *
C.    *       mechanisms used by GEANT                                 *
C.    *                                                                *
C.    *    ==>Called by : <USER>, UGINIT                               *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCLIST/NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     +       ,NSTAT,LHSTA(20),LGET (20),LSAVE(20),LSETS(20),LPRIN(20)
     +             ,LGEOM(20),LVIEW(20),LPLOT(20),LSTAT(20)
C
      INTEGER       NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     + ,NSTAT,LHSTA,LGET ,LSAVE,LSETS,LPRIN,LGEOM,LVIEW,LPLOT,LSTAT
C
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCTIME/TIMINT,TIMEND,ITIME,IGDATE,IGTIME
      INTEGER ITIME,IGDATE,IGTIME
      REAL TIMINT,TIMEND
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      DIMENSION CUTS(10),UCUT(10),MECA(5,13)
      EQUIVALENCE (CUTS(1),CUTGAM),(MECA(1,1),IPAIR)
      CHARACTER*4 DNAME,KCUT(10)
      CHARACTER*20 CHTITL
      LOGICAL NUCRIN
C.
C.    ------------------------------------------------------------------
C.
C              Write RUN parameters, version numbers and CUTS
C
      WRITE(CHMAIL,10000)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10200)GVERSN,IGDATE,IGTIME
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10300)IDRUN
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10400)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10500)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10600)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
C
C              Get the version number of the original INIT structure
C
      OLDGVE=BIG
*
*        Set NUMOLD to 0 to force recalculation of
*        pointers in the tracking routines
      NUMOLD=0
      IF(JRUNG.NE.0)THEN
         OLDGVE =  Q(JRUNG+21)
         IQ(JRUNG+11)=IGDATE
         IQ(JRUNG+12)=IGTIME
         Q(JRUNG+21)=GVERSN
         Q(JRUNG+22)=ZVERSN
C
         DNAME='INIT'
         WRITE(CHMAIL,10700) DNAME,IQ(JRUNG+11),IQ(JRUNG+12), Q(JRUNG+
     +   21), Q(JRUNG+22)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
         DNAME='KINE'
         WRITE(CHMAIL,10700) DNAME,IQ(JRUNG+13),IQ(JRUNG+14), Q(JRUNG+
     +   23), Q(JRUNG+24)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
         DNAME='HITS'
         WRITE(CHMAIL,10700) DNAME,IQ(JRUNG+15),IQ(JRUNG+16), Q(JRUNG+
     +   25), Q(JRUNG+26)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
         DNAME='DIGI'
         WRITE(CHMAIL,10700) DNAME,IQ(JRUNG+17),IQ(JRUNG+18), Q(JRUNG+
     +   27), Q(JRUNG+28)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
         IF(NRNDM(1).EQ.0.AND.NRNDM(2).EQ.0) THEN
*
*             The random number sequence has not been explicitely
*             initialised via a data card. See whether we can initialise
*             it with the words 19/20 of the JRUNG data structure.
            IF(IQ(JRUNG+19).NE.0.OR.IQ(JRUNG+20).NE.0) THEN
               NRNDM(1) = IQ(JRUNG+19)
               NRNDM(2) = IQ(JRUNG+20)
               CALL GRNDMQ(NRNDM(1), NRNDM(2), 0, 'S')
            ENDIF
         ENDIF
         CALL GRNDMQ(IQ(JRUNG+19), IQ(JRUNG+20), 0, 'G')
         WRITE(CHMAIL,10900) IQ(JRUNG+19), IQ(JRUNG+20)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,11000)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
      ENDIF
C
C             Create energy loss and cross-section banks
C
      IF(NEKBIN.LE.0.OR.NEKBIN.GT.199)NEKBIN=90
      IF(EKMIN.GE.EKMAX.OR.EKMIN.LE.0.)THEN
         EKMIN=1.E-5
         EKMAX=1.E+4
      ENDIF
      NEK1=NEKBIN+1
      EKINV=1./LOG10(EKMAX/EKMIN)
      EKBIN(1)=LOG10(EKMIN)
      ELOW(1)=EKMIN
      GEKA=NEKBIN*EKINV
      GEKB=1.-GEKA*EKBIN(1)
      DO 10 I=2,NEK1
         EL=EKBIN(1)+(I-1)/GEKA
         EKBIN(I)=EL
         ELOW(I)=10.**EL
   10 CONTINUE
      ILOW=0
      IF(NMATE.LE.0)GO TO 999
      IF(JMATE.LE.0)GO TO 999
      IF(JTMED.LE.0)GO TO 999
C
      IF(IQ(JTMED-1).LT.40) THEN
         NPUSH=40-IQ(JTMED-1)
         CALL MZPUSH(IXCONS,JTMED,0,NPUSH,'I')
      END IF
      Q(JTMED+31)=ILABS
      Q(JTMED+32)=ISYNC
      Q(JTMED+33)=ISTRA
*
*             If Landau fluctuations activated, cancel delta rays
      KLOS=Q(JTMED+21)
      IF (KLOS .EQ. 0) Q(JTMED+15) = 0.
      IF (KLOS .EQ. 2) THEN
         Q(JTMED+ 8)=9999.
         Q(JTMED+ 9)=9999.
         Q(JTMED+15)=0.
      ENDIF
*
* If Cerenkov generation is on, activate Light absorbtion unless
* explicitely switched off by the user
*
      KLABS=Q(JTMED+31)
      IF(ITCKOV.NE.0) THEN
         IF(KLABS.EQ.-1) THEN
            Q(JTMED+31)=1
         ENDIF
      ENDIF
      Q(JTMED+31)=MAX(Q(JTMED+31),0.)
*
*             If BCUTE,BCUTM,DCUTE,DCUTM,PPCUTM not initialized (=BIG)
*             Set them to CUTGAM,CUTGAM,CUTELE,CUTELE respectively
*
      IF(Q(JTMED+ 6).GT.0.9*BIG)Q(JTMED+ 6)=Q(JTMED+1)
      IF(Q(JTMED+ 7).GT.0.9*BIG)Q(JTMED+ 7)=Q(JTMED+1)
      IF(Q(JTMED+ 8).GT.0.9*BIG)Q(JTMED+ 8)=Q(JTMED+2)
      IF(Q(JTMED+ 9).GT.0.9*BIG)Q(JTMED+ 9)=Q(JTMED+2)
      IF(Q(JTMED+10).GT.0.9*BIG)Q(JTMED+10)=0.010
      IF(Q(JTMED+10).LT.4.*EMASS)Q(JTMED+10)=4.*EMASS
C
      DO 20 K=1,10
   20 CALL GEVKEV(Q(JTMED+K),UCUT(K),KCUT(K))
      WRITE(CHMAIL,10800)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11200)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11300) (UCUT(K),KCUT(K),K=1,3)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11400) (UCUT(K),KCUT(K),K=4,5)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11500) (UCUT(K),KCUT(K),K=6,7)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11600) (UCUT(K),KCUT(K),K=8,10)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11700) (Q(JTMED+K),K=11,13)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,11800) (Q(JTMED+K),K=14,16)
      CALL GMAIL(0,0)
      IF(Q(JTMED+18).EQ.3.) THEN
         NUCRIN = .TRUE.
         Q(JTMED+18)=1.
      ELSE
         NUCRIN = .FALSE.
      ENDIF
      WRITE(CHMAIL,11900) (Q(JTMED+K),K=17,19)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,12000) (Q(JTMED+K),K=20,22)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,12100) Q(JTMED+23),Q(JTMED+31),Q(JTMED+32)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,12110) Q(JTMED+33)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      IF(NUCRIN) THEN
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,12800)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,12900)
         CALL GMAIL(0,0)
         WRITE(CHMAIL,10100)
         CALL GMAIL(0,0)
      ENDIF
*
* ***    Here we clean up the old cross section tables if any
      DO 40  IMA=1,NMATE
         JMA=LQ(JMATE-IMA)
         IF(JMA.NE.0) THEN
            DO 30  J=1,20
               IF(LQ(JMA-J).NE.0.AND.J.NE.5) THEN
                  CALL MZDROP(IXCONS,LQ(JMA-J),'L')
               ENDIF
   30       CONTINUE
         ENDIF
   40 CONTINUE
*
* *** Call initialisation of the phtotelectric effect constants
      CALL GPHINI
      DO 180 ITM=1,NTMED
         JTM=LQ(JTMED-ITM)
         IF(JTM.LE.0) GO TO 180
         NL=10-IQ(JTM-2)
         IF(NL.GT.0)THEN
            CALL MZPUSH(IXCONS,JTM,NL,0,'I')
            JTM=LQ(JTMED-ITM)
         ENDIF
*         IF(IQ(JTM-1).LT.40) THEN
*           NPUSH=40-IQ(JTM-1)
*           CALL MZPUSH(IXCONS,JTM,0,NPUSH,'I')
*           JTM=LQ(JTMED-ITM)
*         ENDIF
         ISVOL = Q(JTM + 7)
         IFIELD = Q(JTM + 8)
         FIELDM = Q(JTM + 9)
         TMAXFD = Q(JTM + 10)
         STEMAX = Q(JTM + 11)
         DEEMAX = Q(JTM + 12)
         EPSIL = Q(JTM + 13)
         STMIN = Q(JTM + 14)
         IF (TMAXFD.LE.0..OR. (IGAUTO.NE.0.AND.TMAXFD.GT.20.)) THEN
            TMAXFD=20.
            Q(JTM+10) = TMAXFD
         ENDIF
         NMAT = Q(JTM+6)
         JMA = LQ(JMATE-NMAT)
         IF(JMA.LE.0)THEN
            WRITE(CHMAIL,12200)NMAT,ITM
            CALL GMAIL(1,1)
            GO TO 180
         ENDIF
C
C=====>       Get material parameters
C
         A=Q(JMA+6)
         Z=Q(JMA+7)
         DENS=Q(JMA+8)
         RADL=Q(JMA+9)
         IF (Z.LT.1.) THEN
            DEEMAX=0.
            STMIN =0.
            JTP=LQ(JTM)
            IF(JTP.EQ.0) THEN
               CALL MZBOOK(IXCONS,JTP,JTM,0,'TCUT',0,0,40,3,0)
               IQ(JTP-5)=ITM
               DO 50 I=1,23
                  Q(JTP+I)=Q(JTMED+I)
   50          CONTINUE
               Q(JTP+31)=Q(JTMED+31)
               Q(JTP+32)=Q(JTMED+32)
               Q(JTP+33)=Q(JTMED+33)
            ELSEIF(IQ(JTP-1).LT.40) THEN
               NPUSH=40-IQ(JTP-1)
               CALL MZPUSH(IXCONS,JTP,0,NPUSH,'I')
               JTP=LQ(JTM)
               Q(JTP+31)=Q(JTMED+31)
               Q(JTP+32)=Q(JTMED+32)
               Q(JTP+33)=Q(JTMED+33)
            ENDIF
C
C=====>     decay and synch. rad. in vacuum
C
            DO 60 I=11,23
               Q(JTP+I)=0.
   60       CONTINUE
            Q(JTP+20) = Q(JTMED+20)
            Q(JTP+31) = 0.
            Q(JTP+32) = Q(JTMED+32)
            Q(JTP+33) =0.
         ENDIF
C
C=====>       Get tracking medium parameters
C
         JTP=JTMED
         IF(LQ(JTM).NE.0)JTP=LQ(JTM)
         IF(JTP.NE.JTMED)THEN
            IF(IQ(JTP-1).LT.40) THEN
               NPUSH=40-IQ(JTP-1)
               CALL MZPUSH(IXCONS,JTP,0,NPUSH,'I')
               JTP=LQ(JTM)
               Q(JTP+31)=Q(JTMED+31)
               Q(JTP+32)=Q(JTMED+32)
               Q(JTP+33)=Q(JTMED+33)
            ENDIF
            KLOS=Q(JTP+21)
            IF (KLOS .EQ. 2) THEN
               Q(JTP+ 8)=9999.
               Q(JTP+ 9)=9999.
               Q(JTP+15)=0.
            ENDIF
*
* If Cerenkov generation is on, activate Light absorbtion unless
* explicitely switched off by the user
*
            KLABS=Q(JTP+31)
            IF(ITCKOV.NE.0) THEN
               IF(KLABS.EQ.-1) THEN
                  Q(JTP+31)=1
               ENDIF
            ENDIF
            Q(JTP+31)=MAX(Q(JTP+31),0.)
            IF(Q(JTP+ 6).GT.0.9*BIG)Q(JTP+ 6)=Q(JTP+1)
            IF(Q(JTP+ 7).GT.0.9*BIG)Q(JTP+ 7)=Q(JTP+1)
            IF(Q(JTP+ 8).GT.0.9*BIG)Q(JTP+ 8)=Q(JTP+2)
            IF(Q(JTP+ 9).GT.0.9*BIG)Q(JTP+ 9)=Q(JTP+2)
            IF(Q(JTP+10).GT.0.9*BIG)Q(JTP+10)=0.010
            IF(Q(JTP+10).LT.4.*EMASS)Q(JTP+10)=4.*EMASS
*
            CALL UHTOC(IQ(JTM+1),4,CHTITL,20)
            LAST=LNBLNK(CHTITL)
            IF(LAST.GT.0) THEN
               IF(CHTITL(LAST:LAST).EQ.'$') LAST=LAST-1
               IF(LAST.LT.20) CHTITL(LAST+1:20)=' '
            ENDIF
*
            DO 70 K=1,10
   70       CALL GEVKEV(Q(JTP+K),UCUT(K),KCUT(K))
            WRITE(CHMAIL,10100)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,12300)ITM,CHTITL
            CALL GMAIL(0,0)
            WRITE(CHMAIL,12400)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11300) (UCUT(K),KCUT(K),K=1,3)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11400) (UCUT(K),KCUT(K),K=4,5)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11500) (UCUT(K),KCUT(K),K=6,7)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11600) (UCUT(K),KCUT(K),K=8,10)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11700) (Q(JTP+K),K=11,13)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,11800) (Q(JTP+K),K=14,16)
            CALL GMAIL(0,0)
            IF(Q(JTP+18).EQ.3.) THEN
               NUCRIN = .TRUE.
               Q(JTP+18)=1.
            ELSE
               NUCRIN = .FALSE.
            ENDIF
            WRITE(CHMAIL,11900) (Q(JTP+K),K=17,19)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,12000) (Q(JTP+K),K=20,22)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,12100) Q(JTP+23),Q(JTP+31),Q(JTP+32)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,12110) Q(JTP+33)
            CALL GMAIL(0,0)
            WRITE(CHMAIL,10100)
            CALL GMAIL(0,0)
            IF(NUCRIN) THEN
               WRITE(CHMAIL,10100)
               CALL GMAIL(0,0)
               WRITE(CHMAIL,12800)
               CALL GMAIL(0,0)
               WRITE(CHMAIL,12900)
               CALL GMAIL(0,0)
               WRITE(CHMAIL,10100)
               CALL GMAIL(0,0)
            ENDIF
         ENDIF
C
         DO 80 I=1,10
            CUTS(I)=Q(JTP+I)
   80    CONTINUE
         DO 90 I=1,13
            MECA(1,I)=Q(JTP+10+I)
   90    CONTINUE
         ILABS=Q(JTP+10+21)
         ISYNC=Q(JTP+10+22)
         ISTRA=Q(JTP+10+23)
C
         IF(ILOW.EQ.0)THEN
            DO 100 I=1,10
               IF(Q(JTP+I).LT.0.0000099)THEN
                  WRITE(CHMAIL,12500)
                  CALL GMAIL(1,1)
                  ILOW=1
               ENDIF
  100       CONTINUE
         ENDIF
C
C             Check consistency of different tracking media
C             referencing the same material
C
         DO 120 ITM2=ITM+1,NTMED
            JTM2=LQ(JTMED-ITM2)
            IF(JTM2.NE.0)THEN
               NMAT2=Q(JTM2+6)
               IF(NMAT2.EQ.NMAT)THEN
                  JTP2=JTMED
                  IF(LQ(JTM2).NE.0)JTP2=LQ(JTM2)
                  IF(JTP.NE.JTP2)THEN
                     IF(JTP2.NE.JTMED)THEN
                        KLOS=Q(JTP2+21)
                        IF (KLOS .EQ. 2) THEN
                           Q(JTP2+ 8)=9999.
                           Q(JTP2+ 9)=9999.
                           Q(JTP2+15)=0.
                        ENDIF
                        IF(Q(JTP2+ 6).GT.0.9*BIG)Q(JTP2+ 6)=Q(JTP2+1)
                        IF(Q(JTP2+ 7).GT.0.9*BIG)Q(JTP2+ 7)=Q(JTP2+1)
                        IF(Q(JTP2+ 8).GT.0.9*BIG)Q(JTP2+ 8)=Q(JTP2+2)
                        IF(Q(JTP2+ 9).GT.0.9*BIG)Q(JTP2+ 9)=Q(JTP2+2)
                        IF(Q(JTP2+10).GT.0.9*BIG)Q(JTP2+10)=0.010
                        IF(Q(JTP2+10).LT.4.*EMASS)Q(JTP2+10)=4.*EMASS
                     ENDIF
                     DO 110 I=6,10
                        IF(Q(JTP+I).NE.Q(JTP2+I))THEN
                           WRITE(CHMAIL,12600)NMAT
                           CALL GMAIL(1,0)
                           WRITE(CHMAIL,12700)ITM,ITM2
                           CALL GMAIL(0,1)
                           GO TO 120
                        ENDIF
  110                CONTINUE
                  ENDIF
               ENDIF
            ENDIF
  120    CONTINUE
         IF (DEEMAX.LT.0.) THEN
            IF(ISVOL.EQ.0)THEN
               DEEMAX=0.25
               IF(RADL.GT.2.)DEEMAX=0.25-0.2/SQRT(RADL)
            ELSE
               DEEMAX = 0.2/SQRT(RADL)
            ENDIF
         ENDIF
         IF(OLDGVE.LT.3.15.OR.STEMAX.LE.0.) THEN
*
*       Before version 3.15 there was no STEMAX, so we put it to BIG
            STEMAX=BIG
         ENDIF
         Q(JTM+11) = STEMAX
         Q(JTM+12) = DEEMAX
C
*
*       It can happen that several tracking media refer to the
*       same material. In this case we do not fill the cross section
*       tables more than once. But we still fill the banks of the
*       tracking medium.
         IF(LQ(JMA-1).NE.0) GOTO 160
         NPUSH=20-IQ(JMA-2)
         IF(NPUSH.GT.0)THEN
            CALL MZPUSH(IXCONS,JMA,NPUSH,0,'I')
            JTM=LQ(JTMED-ITM)
            JMA=LQ(JMATE-NMAT)
         ENDIF
*
*     Energy loss and cross-section tables
         IF(ISTRA.EQ.0) THEN
         CALL MZBOOK(IXCONS,LBANK,JMA, -1,'MAEL',0,0,2*NEK1,3,0)
         CALL MZBOOK(IXCONS,LBANK,JMA, -2,'MAMU',0,0, NEK1,3,0)
         ELSE
         CALL MZBOOK(IXCONS,LBANK,JMA, -1,'MAEL',0,0,3*NEK1,3,0)
         CALL MZBOOK(IXCONS,LBANK,JMA, -2,'MAMU',0,0,2*NEK1,3,0)
         ENDIF
         CALL MZBOOK(IXCONS,LBANK,JMA, -3,'MAAL',0,0, NEK1,3,0)
         CALL MZBOOK(IXCONS,JPROB,JMA, -4,'MAPR',0,0, 40,3,0)
         CALL MZBOOK(IXCONS,JPHOT,JMA, -6,'MAPH',2,2, NEK1,3,0)
         CALL MZBOOK(IXCONS,JANNI,JMA, -7,'MAAN',0,0, NEK1,3,0)
         CALL MZBOOK(IXCONS,JCOMP,JMA, -8,'MACO',0,0, NEK1,3,0)
         CALL MZBOOK(IXCONS,JBREM,JMA, -9,'MABR',0,0,3*NEK1,3,0)
         CALL MZBOOK(IXCONS,JPAIR,JMA,-10,'MAPA',0,0,2*NEK1,3,0)
         CALL MZBOOK(IXCONS,JDRAY,JMA,-11,'MADR',0,0,3*NEK1,3,0)
*
* *** Special case for heavy materials and N-14, photo-fission
* *** Presence of N-14 is signalled by non-zero user word in JTMED structure
         IF(((A.GE.230..AND.A.LE.240.).OR.(Q(JTM+15).GT.0.))
     +                          .AND.
     +                        IPFIS.NE.0                     )THEN
            CALL MZBOOK(IXCONS,JPFIS,JMA,-12,'MAPF',0,0,2*NEK1,3,0)
         ENDIF
*
* *** Rayleigh effect
         CALL MZBOOK(IXCONS,JRAYL,JMA,-13,'MARA',0,0,2*NEK1,3,0)
*
* *** Muon nuclear interactions
         IF(IMUNU.EQ.0)THEN
            JMUNU=0
         ELSE
            CALL MZBOOK(IXCONS,JMUNU,JMA,-14,'MAMN',0,0,NEK1,3,0)
         ENDIF
*
* *** stopping range
         CALL MZBOOK(IXCONS,LBANK,JMA,-15,'MASE',0,0,2*NEK1,3,0)
         CALL MZBOOK(IXCONS,LBANK,JMA,-16,'MASM',0,0,2*NEK1,3,0)
*
* *** Special for photeffect
         CALL GPHXSI
*
* *** coefficients for energy loss
         CALL MZBOOK(IXCONS,LBANK,JMA,-17,'MACE',0,0,6*NEK1,3,0)
         CALL MZBOOK(IXCONS,LBANK,JMA,-18,'MACM',0,0,6*NEK1,3,0)
*
* *** auxiliary tables for integration of dE/dx
         CALL GWORK(NEKBIN*4)
*
         DO 130 JWORK=1, NEKBIN*4
            WS(JWORK) = 0.
  130    CONTINUE
*
* *** Straggling for thin layers, if in effect
         IF(ISTRA.GT.0) THEN
            CALL MZBOOK(IXCONS,JTSTRA,JMA,-19,'MAST',2,2,1,3,0)
         ENDIF
*
         DO 140 J=1,20
            JB=LQ(JMA-J)
            IF(JB.NE.0)IQ(JB-5)=NMAT
  140    CONTINUE
C
         JPROB=LQ(JMA-4)
         JMIXT=LQ(JMA-5)
         JPFIS=LQ(JMA-12)
*
* *** Fill above tables (energy losses,cross-sections,stopping ranges)
*
         CALL GPROBI
C
         DO 150 IEKBIN=1,NEK1
C
            CALL GDRELA
            CALL GBRELA
            CALL GPRELA
C
            CALL GPHOTI
            CALL GRAYLI
            CALL GANNII
            CALL GCOMPI
            CALL GBRSGA
            CALL GPRSGA
            CALL GDRSGA
            CALL GMUNUI
            CALL GPFISI
  150    CONTINUE
*
*           Stopping ranges
*
         CALL GRANGI
*
*           Energy loss coefficients
*
         CALL GCOEFF
* *** The table for the energy loss in thin gas layers if the tracking
*     media is defined as such
*
         IF(ISTRA.GT.0) THEN
            CALL GSTINI
         ENDIF
*
* *** Multiple scattering,energy-loss and mag.field steps
  160    DO 170 J=1,2
            IF(LQ(JTM-J).NE.0) THEN
               CALL MZDROP(IXCONS,LQ(JTM-J),'L')
            ENDIF
  170    CONTINUE
         CALL MZBOOK(IXCONS,LBANK,JTM, -1,'MUEL',0,0,NEK1+2,3,0)
         IQ(LBANK-5)=ITM
         CALL MZBOOK(IXCONS,LBANK,JTM, -2,'MUMU',0,0,NEK1+2,3,0)
         IQ(LBANK-5)=ITM
         CALL GMULOF
C
  180 CONTINUE
*
      WRITE(CHMAIL,10100)
      CALL GMAIL(0,0)
      WRITE(CHMAIL,10400)
      CALL GMAIL(0,2)
C
10000 FORMAT(
     +'1************************************************************')
10100 FORMAT(
     +' *                                                          *')
10200 FORMAT(
     +' *    G E A N T  Version',F7.4,'      DATE/TIME',I7,'/',
     +   I4,2X,'*')
10300 FORMAT(
     +' *                      R U N  ',I5,10X,'              *')
10400 FORMAT(
     +' ************************************************************')
10500 FORMAT(
     +' *      Data structure   Date   Time    GVERSN    ZVERSN    *')
10600 FORMAT(
     +' *      --------------   ----   ----    ------    ------    *')
10700 FORMAT(' *',11X,A,6X,I7,2X,I4,3X,F7.4,2X,F7.2,5X,'*')
10800 FORMAT(
     +' *----------------------------------------------------------*')
10900 FORMAT(' *     Random number seeds: ',3X,I10,3X,I10,6X,'*')
11000 FORMAT(
     +' *     --------------------                                 *')
11100 FORMAT(
     +' *              Standard TPAR for this run are              *')
11200 FORMAT(
     +' *              ------------------------------              *')
11300 FORMAT(
     +' *  CUTGAM=',F6.2,A4,'  CUTELE=',F6.2,A4,'  CUTNEU=',F6.2,A4,1X,
     + '*')
11400 FORMAT(
     +' *  CUTHAD=',F6.2,A4,'  CUTMUO=',F6.2,A4,20X,'*')
11500 FORMAT(
     +' *  BCUTE =',F6.2,A4,'  BCUTM =',F6.2,A4,20X,'*')
11600 FORMAT(
     +' *  DCUTE =',F6.2,A4,'  DCUTM =',F6.2,A4,'  PPCUTM=',F6.2,A4,1X,
     + '*')
11700 FORMAT(
     +' *  IPAIR =',F10.0,'  ICOMP =',F10.0,'  IPHOT =',F10.0,1X,'*')
11800 FORMAT(
     +' *  IPFIS =',F10.0,'  IDRAY =',F10.0,'  IANNI =',F10.0,1X,'*')
11900 FORMAT(
     +' *  IBREM =',F10.0,'  IHADR =',F10.0,'  IMUNU =',F10.0,1X,'*')
12000 FORMAT(
     +' *  IDCAY =',F10.0,'  ILOSS =',F10.0,'  IMULS =',F10.0,1X,'*')
12100 FORMAT(
     +' *  IRAYL =',F10.0,'  ILABS =',F10.0,'  ISYNC =',F10.0,1X,'*')
12110 FORMAT(
     +' *  ISTRA =',F10.0, 39X,                                  '*')

12200 FORMAT(' ***** GPHYSI error, Material Nr=',I3,
     + ' referenced by Medium Nr=',I3)
12300 FORMAT(
     +' *     Special TPAR for TMED',I4,3X,A,5X,'*')
12400 FORMAT(
     +' *     -------------------------                            *')
12500 FORMAT(' ***** GPHYSI error, CUTS must be',
     + ' greater than 10 KeV *****')
12600 FORMAT(' ***** GPHYSI error for material nr ',I4)
12700 FORMAT(7X,'Tracking medium NR',I4,' and',I4,
     +' have different parameters')
12800 FORMAT(
     +' *  IHADR=3 not supported any more. GHEISHA will handle     *')
12900 FORMAT(
     +' *  hadronic interactions for the above tracking medium     *')
  999 END
C.
      FUNCTION sign14r(tkin, a, dens)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     CALCULATION OF RESONANT PHOTO-ABSORBTION                   *
C.    *                                                                *
C.    *     CROSS SECTIONS (IN UNITS of 1/CM) IN NITROGEN-14           *
C.    *                                                                *
C.    *   ==>Called by : GFTMAT, GTGAMA                                *
C.    *                                                                *
C.    *      Author: L. Felawka                                        *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT NONE
C.
      REAL sign14r, tkin, a, dens
C.
C.    Cross sections in 1/cm, Energyis in GeV - Breit-Wigner shape assumed
C. 
C.      REAL        rxsn14
C.      PARAMETER  (rxsn14 = 2582.0)	! N-14 res. abs. X-Sect in mb.
C.
      REAL 	  rxsn14
      PARAMETER  (rxsn14 = 0.11098)	! N-14 res. abs. X-Sect in cm**2/gm
C.
      REAL	  E0
      PARAMETER  (E0 = 9.1760)		! Resonant energy [MeV]
C.
      REAL	  rabs_width
      PARAMETER  (rabs_width = 130)	! resonant absorption width [eV]
C.
      sign14r = 1.0 / (1.0 + ((tkin-(0.001*E0))/(0.5E-9*rabs_width))**2)
      sign14r = sign14r*rxsn14*dens
C.
      RETURN
      END
CDECK  ID>, GTRACK. 
*CMZ :  3.21/02 29/03/94  15.41.24  by  S.Giani
*-- Author :
      SUBROUTINE GTRACK
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Controls tracking of current particle,                   *
C.    *        up to end of track for sequential tracking mode, or     *
C.    *        through current volume for parallel tracking mode.      *
C.    *                                                                *
C.    *    ==>Called by : GUTRAK                                       *
C.    *       Authors   : R.Brun, F.Bruyant                            *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      COMMON/GCPARM/IPARAM,PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU
     +             ,NSPARA,MPSTAK,NPGENE
      REAL PACUTS(5)
      EQUIVALENCE (PACUTS(1),PCUTGA)
      INTEGER IPARAM,MPSTAK,NSPARA,NPGENE
      REAL    PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU
C
      COMMON/GCSETS/IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)
C
      INTEGER       IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      COMMON/GCCHAN/LSAMVL
      LOGICAL LSAMVL
*
      DIMENSION CUTS(10),MECA(5,13)
      EQUIVALENCE (CUTS(1),CUTGAM),(MECA(1,1),IPAIR)
      SAVE PRECOR
      PARAMETER (EPSMAC=1.E-6)
C.
      include 'uenergy.inc'              !local
      include 'ukine.inc'                !local
C.
C.    ------------------------------------------------------------------
      ISTOP = 0
      EPSCUR = EPSMAC
      NSTOUT = 0
      INWOLD = 0
      LSAMVL = .FALSE.
*
* *** Check validity of tracking medium and material parameters
*
   10 IF (NUMED.NE.NUMOLD) THEN
         NUMOLD = NUMED
         IUPD   = 0
         JTM    = LQ(JTMED- NUMED)
         DO 20 I = 1,5
            NATMED(I) = IQ(JTM+I)
   20    CONTINUE
         NMAT     = Q(JTM + 6)
         ISVOL    = Q(JTM + 7)
         IFIELD   = Q(JTM + 8)
         FIELDM   = Q(JTM + 9)
         TMAXFD   = Q(JTM + 10)
         STEMAX   = Q(JTM + 11)
         DEEMAX   = Q(JTM + 12)
         EPSIL    = Q(JTM + 13)
         STMIN    = Q(JTM + 14)
         PRECOR   = MIN(0.1*EPSIL, 0.0010)
         IF (LQ(JTM).EQ.0) THEN
            IF (ISTPAR.NE.0) THEN
               DO 30 I = 1,10
                  CUTS(I) = Q(JTMED+I)
   30          CONTINUE
               DO 40 I = 1,13
                  MECA(1,I) = Q(JTMED+10+I)
   40          CONTINUE
               ILABS = Q(JTMED+10+21)
               ISYNC = Q(JTMED+10+22)
               ISTRA = Q(JTMED+10+23)
               ISTPAR = 0
            ENDIF
         ELSE
            JTMN = LQ(JTM)
            DO 50 I = 1,10
               CUTS(I) = Q(JTMN+I)
   50       CONTINUE
            DO 60 I = 1,13
               MECA(1,I) = Q(JTMN+10+I)
   60       CONTINUE
            ILABS = Q(JTMN+10+21)
            ISYNC = Q(JTMN+10+22)
            ISTRA = Q(JTMN+10+23)
            ISTPAR = 1
         ENDIF
*
         JMA   = LQ(JMATE-NMAT)
         JPROB = LQ(JMA-4)
         JMIXT = LQ(JMA-5)
         DO 70 I = 1,5
            NAMATE(I) = IQ(JMA+I)
   70    CONTINUE
         A    = Q(JMA +6)
         Z    = Q(JMA +7)
         DENS = Q(JMA +8)
         RADL = Q(JMA +9)
         ABSL = Q(JMA +10)
         IF(IQ(JTM-2).GE.3.AND.LQ(JTM-3).NE.0.AND.ITCKOV.NE.0.AND.
     +      LQ(LQ(JTM-3)-3).NE.0.AND.Z.GE.1.) THEN
*
* ***  In this tracking medium Cerenkov photons are generated and
* ***  tracked. Set to 1 the corresponding flag.
*
            IMCKOV = 1
         ELSE
            IMCKOV = 0
         ENDIF
*
*
*  **   Update precomputed quantities
*
         IMULL = IMULS
         IF (ILOSS.LE.0) THEN
            DEEMAX = 0.
            ILOSL = 0
         ELSEIF (DEEMAX.GT.0.) THEN
            ILOSL = ILOSS
         ELSE
            ILOSL = 0
         ENDIF
      ENDIF
*
      IF(LSAMVL) THEN
*
*       If now the particle is entering in the same volume where
*       it was exiting from last step, and if it has done this for
*       more than 5 times, we decrease the precision of tracking
         NSTOUT=NSTOUT+1
         IF(MOD(NSTOUT,5).EQ.0) THEN
            EPSCUR=NSTOUT*EPSMAC
C            WRITE(CHMAIL,10000)ITRA,ISTAK,NTMULT,NAPART
C10000          FORMAT(' *** GTRACK *** Boundary loop: track ',
C     +         I4,' stack ',I4,' NTMULT ',I5,1X,5A4)
C            CALL GMAIL(1,0)
C            WRITE (CHMAIL,10250) IEVENT,IDEVT,(NRNDM(I),I = 1,2)
C            CALL GMAIL(0,0)
C            WRITE(CHMAIL,10100) EPSCUR
C10100          FORMAT('                Precision now set to ',G10.3)
C            CALL GMAIL(0,1)
         ENDIF
      ELSE
         NSTOUT = 0
         EPSCUR = EPSMAC
      ENDIF
*
      INWVOL = 1
*
* *** Compute SET and DET number if volume is sensitive
*
      IF (JSET.GT.0) THEN
         IF(ISVOL.GT.0) THEN
            CALL GFINDS
         ELSE
            IHSET = 0
            IHDET = 0
            ISET = 0
            IDET = 0
            IDTYPE = 0
            NVNAME = 0
         ENDIF
      ENDIF
*
*    Clear step dependent variables
*
   80 NMEC   = 0
      STEP   = 0.
      DESTEL = 0.
      DESTEP = 0.
      NGKINE = 0
      NGPHOT = 0.
      IGNEXT = 0
      INWOLD = INWVOL
      PREC   = MAX(PRECOR,MAX(ABS(VECT(1)),ABS(VECT(2)),
     +                        ABS(VECT(3)),SLENG)*EPSCUR)
*
*     Give control to user at entrance of volume (INWVOL=1)
*
      IF (INWVOL.EQ.1) THEN
         CALL GUSTEP
         IF (ISTOP.NE.0) GO TO 999
         INWVOL = 0
      ENDIF
*
* *** Propagate particle up to next volume boundary or end of track
*
      INGOTO = 0
      NLEVIN = NLEVEL
      IF (IPARAM.NE.0) THEN
         IF (GEKIN.LE.PACUTS(ITRTYP)) THEN
            NMEC = NMEC+1
            LMEC(NMEC) = 26
            ISTOP = 2
            CALL GUPARA
            GO TO 90
         ENDIF
      ENDIF
      IF      (ITRTYP.EQ.1) THEN
         CALL GTGAMA
      ELSE IF (ITRTYP.EQ.2) THEN
         CALL GTELEC
      ELSE IF (ITRTYP.EQ.3) THEN
         CALL GTNEUT
      ELSE IF (ITRTYP.EQ.4) THEN
         CALL GTHADR
      ELSE IF (ITRTYP.EQ.5) THEN
         CALL GTMUON
      ELSE IF (ITRTYP.EQ.6) THEN
         CALL GTNINO
      ELSE IF (ITRTYP.EQ.7) THEN
         CALL GTCKOV
      ELSE IF (ITRTYP.EQ.8) THEN
         CALL GTHION
      ENDIF
      IF(JGSTAT.NE.0) CALL GFSTAT(10+ITRTYP)
      STLOSS=STEP
*
*     Check for possible endless loop
*
   90 NSTEP = NSTEP +1
      IF (NSTEP.GT.max_step) THEN
         IF (ISTOP.EQ.0) THEN
            nloss(4) = nloss(4) + 1
            ISTOP = 99
            NMEC  = NMEC +1
            LMEC(NMEC) = 30
C            WRITE(CHMAIL,10200) MAXNST
C            CALL GMAIL(1,0)
C            WRITE(CHMAIL,10250) IEVENT,IDEVT,(NRNDM(I),I = 1,2)
C            CALL GMAIL(0,0)
C            WRITE(CHMAIL,10300)ITRA,ISTAK,NTMULT,(NAPART(I),I=1,5),
C     +      TOFG*1.E9
C            CALL GMAIL(0,1)
C10200       FORMAT(' *** GTRACK *** More than ',I6,
C     +             ' steps, tracking abandoned!')
C10250       FORMAT('                IEVENT=',I7,' IDEVT=',I7,
C     +             ' Random Seeds = ',I10,2X,I10)
C10300       FORMAT('                Track ',I5,' stack ',I5,' NTMULT ',
C     +             I5,1X,5A4,1X,'Time of flight ',F10.3,' ns')
         ENDIF
      ENDIF
*
* *** Give control to user at end of each tracking step
*
      SAFETY = SAFETY -STEP
      CALL GUSTEP
*
      IF (ISTOP.NE.0) GO TO 999
*
*      Renormalize direction cosines
*
      PMOM = SQRT(VECT(4)**2+VECT(5)**2+VECT(6)**2)
      IF(PMOM.GT.0.) THEN
         CMOD = 1./PMOM
         VECT(4) = VECT(4)*CMOD
         VECT(5) = VECT(5)*CMOD
         VECT(6) = VECT(6)*CMOD
      ENDIF
*
      IF (INWVOL.EQ.0) GO TO 80
*
      IF (NJTMAX.GT.0) THEN
         CALL GSTRAC
         IF (NLEVIN.EQ.0) GO TO 100
         GO TO 999
      ELSE
         IF (NLEVIN.GE.NLEVEL) THEN
            INFROM = 0
         ELSE
            IF (NLEVIN.EQ.0) GO TO 100
            INFROM = LINDEX(NLEVIN+1)
         ENDIF
         IF (NLEVIN.NE.NLEVEL) INGOTO = 0
         NLEVEL = NLEVIN
*
         CALL GTMEDI (VECT, NUMED)
         IF (NUMED.NE.0) THEN
            SAFETY = 0.
            GO TO 10
         ENDIF
      ENDIF
*
*     Track outside setup, give control to user (INWVOL=3)
*
  100 INWVOL = 3
      ISTOP  = 1
      ISET   = 0
      IDET   = 0
      NMEC   = 0
      STEP   = 0.
      DESTEL = 0.
      DESTEP = 0.
      NGKINE = 0
      NLCUR  = NLEVEL
      NLEVEL = 1
      CALL GUSTEP
      NLEVEL = NLCUR
*                                                             END GTRACK
  999 END
CDECK  ID>, GTCKOV. 
*CMZ :  3.21/04 13/12/94  15.17.13  by  S.Giani
*-- Author :
      SUBROUTINE GTCKOV
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   This routine is called to follow the Cherenkov photons       *
C.    *   created during the tracking of charged particles and         *
C.    *   simulate the relevant processes along the way, until either  *
C.    *   the photon is absorbed or exits the detector. Processes      *
C.    *   currently simulated are absorption in-flight, and reflection *
C.    *   /transmission/absorption at a medium boundary. There are two *
C.    *   boundary types: dielectric-metal and dielectric-dielectric.  *
C.    *   For each of these there is a continuum of reflectivity       *
C.    *   and of surface quality from mirror finish to matte. The      *
C.    *   surface model is contained in routine GHSURF.                *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *      Authors    F.Carminati, R.Jones ************              *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
      COMMON/GCFLAX/BATCH, NOLOG
      LOGICAL BATCH, NOLOG
C
      INTEGER       IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      COMMON/GCVOL1/NLEVL1,NAMES1(15),NUMBR1(15),LVOLU1(15)
C
      INTEGER NAMES1,NUMBR1,LVOLU1
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCVDMA/NVMANY,MANYLE(20),MANYNA(20,15),
     +MANYNU(20,15),NFMANY,MYCOUN,IMYSE,RAYTRA,VECCOS(3)
C
      INTEGER NVMANY,MANYLE,MANYNA,MANYNU,
     +        NFMANY,MYCOUN,IMYSE
      REAL RAYTRA,VECCOS
C
*
* ** The following common is in GTMEDI. LSAMVL is set to true if
* ** we did not change volume yet
*
      COMMON/GCCHAN/LSAMVL
      LOGICAL LSAMVL
*
      DIMENSION R(3),D(3),U(3),QQ(3),vin(3),E1PL(3),E1PP(3)
      DIMENSION VBOU(3)
      LOGICAL LOLDTR
      PARAMETER (EPSMAC=1.E-6)
      PARAMETER (MXPUSH=10)
      SAVE RIN1,EFFIC
C.
      include 'uenergy.inc'              !local
C.
C.    ------------------------------------------------------------------
*
* *** Update local pointers if medium has changed
*
      LOLDTR=.FALSE.
      IF(IUPD.EQ.0)THEN
         IUPD  = 1
         JTCKOV = LQ(JTM-3)
         IF(JTCKOV.EQ.0) THEN
*
* *** This Cerenkov photon has crossed into a black medium.
* *** Just absorb it.
            IPROC = 101
            STEP  = 0.
            SLABS = 0.
            ISTOP = 2
            DESTEP = 0.
            GOTO 110
         ENDIF
         NPCKOV = Q(JTCKOV+1)
         JABSCO = LQ(JTCKOV-1)
         JEFFIC = LQ(JTCKOV-2)
         JINDEX = LQ(JTCKOV-3)
         JPOLAR = LQ(JSTAK-1)
      ENDIF
      IF(SLENG.LE.0.) THEN
*
* *** Calculate GEKRAT for the particle
         IF(VECT(7).GE.Q(JTCKOV+NPCKOV+1)) THEN
            GEKRAT=1.
            IEKBIN=NPCKOV-1
         ELSEIF(VECT(7).LT.Q(JTCKOV+2)) THEN
*
* *** Particle below energy threshold ?  Short circuit
* *** This should never happen because the photons are generated
* *** only above threshold
*
*            GEKIN = 0.
*            GETOT = 0.
*            VECT(7)= 0.
*            ISTOP = 2
*            NMEC = 1
*            LMEC(1)= 30
*            GO TO 110

            GEKRAT=0.
            IEKBIN=1
         ELSE
            JMIN = 1
            JMAX = NPCKOV
   10       JMED = (JMIN+JMAX)/2
            IF(Q(JTCKOV+JMED+1).LT.VECT(7)) THEN
               JMIN = JMED
            ELSE
               JMAX = JMED
            ENDIF
            IF(JMAX-JMIN.GT.1) GO TO 10
            IEKBIN = JMIN
            GEKRAT = (VECT(7) - Q(JTCKOV+IEKBIN+1))/
     +      (Q(JTCKOV+IEKBIN+2)-Q(JTCKOV+IEKBIN+1))
         ENDIF
         GEKRT1=1.-GEKRAT
         RIN1=Q(JINDEX+IEKBIN)*GEKRT1+Q(JINDEX+IEKBIN+1)*GEKRAT
         EFFIC=Q(JEFFIC+IEKBIN)*GEKRT1+Q(JEFFIC+IEKBIN+1)*GEKRAT
         STEPLA=Q(JABSCO+IEKBIN)*GEKRT1+Q(JABSCO+IEKBIN+1)*GEKRAT
      ENDIF
*
* *** Compute current step size
*
      IPROC  = 103
      STEP   = STEMAX
*
*  **   Step limitation due to in flight absorbtion ?
*
      IF (ILABS.GT.0) THEN
         SLABS  = STEPLA*ZINTLA
         IF (SLABS.LT.STEP) THEN
            STEP  = SLABS
            IPROC = 101
         ENDIF
      ENDIF
*
      IF (STEP.LT.0.) STEP = 0.
*
*  **   Step limitation due to geometry ?
*
      STEPT=0.
      IF (STEP.GE.SAFETY) THEN
         CALL GTNEXT
         IF (IGNEXT.NE.0) THEN
*
* **    We are going to cross a boundary, so we need to simulate
* **    boundary effects and to know what is on the other side.
* **    For the moment save the current vector in the geometry tree.
*
         if(mycoun.gt.1.and.nfmany.gt.0)then
           nlevel=manyle(nfmany)
           do 99 i=1,nlevel
             names(i)=manyna(nfmany,i)
             number(i)=manynu(nfmany,i)
 99        continue
           call glvolu(nlevel,names,number,ier)
           if(ier.ne.0)print *,'Fatal error in GLVOLU'
           ingoto=0
         endif
*
            NLEVL1 = NLEVEL
            DO 20   I=1,NLEVEL
               NAMES1(I) = NAMES(I)
               NUMBR1(I) = NUMBER(I)
               LVOLU1(I) = LVOLUM(I)
   20       CONTINUE
*
* *** This is different from the other tracking routines.
* *** We get to the boundary and then we just jump over it
* *** So, linear transport till we are very near the boundary
*
            STEP = MAX(SNEXT-PREC,0.)
            If(step.le.prec)then
              nloss(3) = nloss(3) + 1
              istop = 1
            Endif
C
C In case of Cherenkovs beeing near a corner, particle holding is
C prevented by a bigger step size. The value of VBOU is only used to
C calculate the correct surface normal (by GLISUR and GGPERP). The
C tracking of the Cherenkov is still using STEP
C
            IF(SNEXT.GT.0.) THEN
               DO 25 I=1,3
                  VBOU(I)=VECT(I)+SNEXT*VECT(I+3)
   25          CONTINUE
            END IF
C
            IF(STEP.GT.0.) THEN
               DO 30 I=1,3
                  VECT(I)=VECT(I)+STEP*VECT(I+3)
   30          CONTINUE
            ENDIF
            STEPT=STEP
            STEP  = SNEXT - STEP + PREC
            IPROC = 0
            INWVOL= 2
            NMEC =  1
            LMEC(1)=1
         ENDIF
*
*        Update SAFETY in stack companions, if any
*        This may well not work.
         IF (IQ(JSTAK+3).NE.0) THEN
            DO 40 IST = IQ(JSTAK+3),IQ(JSTAK+1)
               JST = JSTAK +3 +(IST-1)*NWSTAK
               Q(JST+11) = SAFETY
   40       CONTINUE
            IQ(JSTAK+3) = 0
         ENDIF
*
      ELSE
         IQ(JSTAK+3) = 0
      ENDIF
*
* *** Linear transport
*
      VIN(1) = VECT(1)
      VIN(2) = VECT(2)
      VIN(3) = VECT(3)
      IF (INWVOL.EQ.2) THEN
         NBPUSH = 0
   50    CONTINUE
         DO 60 I = 1,3
            VECTMP  = VECT(I) +STEP*VECT(I+3)
            IF(VECTMP.EQ.VECT(I)) THEN
*
* *** Correct for machine precision
*
               IF(VECT(I+3).NE.0.) THEN
                  VECTMP = VECT(I)+ABS(VECT(I))*SIGN(1.,VECT(I+3))*
     +            EPSMAC
                  IF(NMEC.GT.0) THEN
                     IF(LMEC(NMEC).EQ.104) NMEC=NMEC-1
                  ENDIF
                  NMEC=NMEC+1
                  LMEC(NMEC)=104
               ENDIF
            ENDIF
            VOUT(I) = VECTMP
   60    CONTINUE
         NUMED1=NUMED
         CALL GTMEDI(VOUT,NUMED2)
         LOLDTR=.TRUE.
         IF(NUMED2.LE.0) THEN
            VECT(1) = VOUT(1)
            VECT(2) = VOUT(2)
            VECT(3) = VOUT(3)
            GO TO 110
         ENDIF
         JVO=LQ(JVOLUM-LVOLUM(NLEVEL))
         IF(LSAMVL.AND.Q(JVO+2).NE.12.) THEN
*
* *** In spite of our efforts we have not crossed the boundary
* *** we increase the step size and try again
*
            NBPUSH = NBPUSH + 1
            IF (NBPUSH.LE.MXPUSH) THEN
              STEP = STEP + NBPUSH*PREC
              GOTO 50
            ELSE
              INWVOL = 0
            ENDIF

         ENDIF
         IF(NUMED1.EQ.NUMED2) THEN
*
* *** If we are in the same medium, nothing needs to be done!
*
            VECT(1)=VOUT(1)
            VECT(2)=VOUT(2)
            VECT(3)=VOUT(3)
            IPROC=0
         ELSE
            JTM2 = LQ(JTMED-NUMED2)
            IF(IQ(JTM2-2).GE.3) THEN
               JTCKV2 = LQ(JTM2-3)
            ELSE
               JTCKV2 = 0
            ENDIF
            IF(JTCKV2.GT.0) THEN
               NPCKV2 = Q(JTCKV2+1)
               JABSC2 = LQ(JTCKV2-1)
               JEFFI2 = LQ(JTCKV2-2)
               JINDX2 = LQ(JTCKV2-3)
               IF(VECT(7).GE.Q(JTCKV2+NPCKV2+1)) THEN
                  GEKRT2=1.
                  IEKBI2=NPCKV2-1
               ELSEIF(VECT(7).LT.Q(JTCKV2+2)) THEN
                  GEKRT2=0.
                  IEKBI2=1
               ELSE
                  JMIN = 1
                  JMAX = NPCKV2
   64             JMED = (JMIN+JMAX)/2
                  IF(Q(JTCKV2+JMED+1).LT.VECT(7)) THEN
                     JMIN = JMED
                  ELSE
                     JMAX = JMED
                  ENDIF
                  IF(JMAX-JMIN.GT.1) GO TO 64
                  IEKBI2 = JMIN
                  GEKRT2 = (VECT(7) - Q(JTCKV2+IEKBI2+1))/
     +            (Q(JTCKV2+IEKBI2+2)-Q(JTCKV2+IEKBI2+1))
               ENDIF
               GEKRT1=1.-GEKRT2
               ABSCO2=Q(JABSC2+IEKBI2)*GEKRT1+Q(JABSC2+IEKBI2+1)*GEKRT2
               EFFIC2=Q(JEFFI2+IEKBI2)*GEKRT1+Q(JEFFI2+IEKBI2+1)*GEKRT2
               IF(JINDX2.GT.0) THEN
                 RIN2=Q(JINDX2+IEKBI2)*GEKRT1+Q(JINDX2+IEKBI2+1)*GEKRT2
               ELSE
                 RIN2=0.
               ENDIF
               IPROC = 102
            ELSE
               ISTOP=2
               DESTEP=0
               NMEC = NMEC+1
               LMEC(NMEC)=30
               VECT(1)=VOUT(1)
               VECT(2)=VOUT(2)
               VECT(3)=VOUT(3)
               GOTO 110
            ENDIF
         ENDIF
      ELSE
         DO 70 I = 1,3
            VECT(I)  = VECT(I) +STEP*VECT(I+3)
   70    CONTINUE
      ENDIF
*
      STEP = STEPT + STEP
      SLENG = SLENG +STEP
*
* *** Update time of flight
*
      TOFG = TOFG +STEP*RIN1/CLIGHT
*
* *** Update interaction probabilities
*
      IF (ILABS.GT.0)    ZINTLA = ZINTLA -STEP/STEPLA
*
      IF (IPROC.EQ.0) GO TO 110
      NMEC = NMEC+1
      LMEC(NMEC) = IPROC
*
*  ** Absorbtion in flight ?
*
      IF (IPROC.EQ.101) THEN
         nloss(1) = nloss(1) + 1
         ISTOP=2
         CALL GRNDM(RNDM,1)
         IF(RNDM.LT.EFFIC) THEN
*
* ***  Destep =/= 0 means that the photon has been detected
*
            DESTEP=VECT(7)
         ELSE
            DESTEP=0.
         ENDIF
*
*  ** Boundary action?
*
      ELSE IF (IPROC.EQ.102) THEN
         IF(JINDX2.EQ.0) THEN
*
* *** Case dielectric -> metal
*
            CALL GRNDM(RNDM,1)
            IF(RNDM.LT.ABSCO2) THEN
*
* *** Photon is absorbed in the next medium
*
               NMEC=NMEC+1
               LMEC(NMEC)=101
               nloss(2) = nloss(2) + 1
               ISTOP = 2
               CALL GRNDM(RNDM,1)
               IF(RNDM.LT.EFFIC2) THEN
*
* ***  Destep =/= 0 means that the photon has been detected
*
                  DESTEP=VECT(7)
               ELSE
                  DESTEP = 0.
               END IF
               VECT(1) = VOUT(1)
               VECT(2) = VOUT(2)
               VECT(3) = VOUT(3)
               GOTO 110
            ELSE
*
* *** Photon is reflected (no polarization effects)
*
               CALL GLISUR(VECT,VBOU,NUMED1,NUMED2,U,PDOTU,IERR)
               IF (IERR.NE.0) THEN
C                  WRITE(CHMAIL,10200) IERR
C                  CALL GMAIL(0,0)
                  nloss(6) = nloss(6) + 1
                  ISTOP=1
                  GO TO 110
               END IF
*
* ** Restore old volume tree, the photon does not cross the boundary
*
*              CALL GLVOLU(NLEVL1,NAMES1,NUMBR1,IERR)
               CALL GTMEDI(VIN,N)
               LOLDTR=.FALSE.
*
*
* *** Reflect, but make sure we are in the old volume before
*
               NBPUSH = 0
   80          NBPUSH = NBPUSH+1
               IF(NBPUSH.GT.MXPUSH) THEN
C                  WRITE(CHMAIL,10300) NTMULT,NSTEP
C                  CALL GMAIL(0,0)
                  nloss(5) = nloss(5) + 1
                  ISTOP=1
                  GOTO 110
               ELSE
                  CALL GINVOL(VECT,ISAME)
                  IF(ISAME.EQ.0) THEN
                     PRECN = NBPUSH*PREC
                     VECT(1) = VECT(1) - PRECN*VECT(4)
                     VECT(2) = VECT(2) - PRECN*VECT(5)
                     VECT(3) = VECT(3) - PRECN*VECT(6)
                     GO TO 80
                  ENDIF
               ENDIF
*
               NMEC=NMEC+1
               LMEC(NMEC)=106
               EDOTU = POLAR(1)*U(1) + POLAR(2)*U(2) + POLAR(3)*U(3)
               VECT(4) = +VECT(4) - 2*PDOTU*U(1)
               VECT(5) = +VECT(5) - 2*PDOTU*U(2)
               VECT(6) = +VECT(6) - 2*PDOTU*U(3)
               POLAR(1) = -POLAR(1) + 2*EDOTU*U(1)
               POLAR(2) = -POLAR(2) + 2*EDOTU*U(2)
               POLAR(3) = -POLAR(3) + 2*EDOTU*U(3)
               INWVOL  = 0
            ENDIF
         ELSE
*
*  case dielectric-dielectric:
*
            CALL GLISUR(VECT,VBOU,NUMED1,NUMED2,U,PDOTU,IERR)
            IF (IERR.NE.0) THEN
C               WRITE(CHMAIL,10200) IERR
C               CALL GMAIL(0,0)
               nloss(6) = nloss(6) + 1
               ISTOP=1
               GO TO 110
            END IF
            EDOTU = POLAR(1)*U(1) + POLAR(2)*U(2) + POLAR(3)*U(3)
            COST1 = -PDOTU
            IF (ABS(COST1).LT.1.) THEN
               SINT1 = SQRT((1-COST1)*(1+COST1))
               SINT2 = SINT1*RIN1/RIN2
            ELSE
               SINT1 = 0.0
               SINT2 = 0.0
            END IF
            IF (SINT2.GE.1) THEN
*
* ***  Simulate total internal reflection
*
               NMEC=NMEC+1
               LMEC(NMEC)=106
*
* ** Restore old volume tree, the photon does not cross the boundary
*
*              CALL GLVOLU(NLEVL1,NAMES1,NUMBR1,IERR)
               CALL GTMEDI(VIN,N)
               LOLDTR=.FALSE.
*
* *** Reflect, but make sure we are in the old volume before
*
               NBPUSH = 0
   90          NBPUSH = NBPUSH+1
               IF(NBPUSH.GT.MXPUSH) THEN
C                  WRITE(CHMAIL,10300) NTMULT,NSTEP
C                  CALL GMAIL(0,0)
                  nloss(5) = nloss(5) + 1
                  ISTOP=1
                  GOTO 110
               ELSE
                  CALL GINVOL(VECT,ISAME)
                  IF(ISAME.EQ.0) THEN
                     PRECN = NBPUSH*PREC
                     VECT(1) = VECT(1) - PRECN*VECT(4)
                     VECT(2) = VECT(2) - PRECN*VECT(5)
                     VECT(3) = VECT(3) - PRECN*VECT(6)
                     GO TO 90
                  ENDIF
               ENDIF
*
               VECT(4) = +VECT(4) - 2*PDOTU*U(1)
               VECT(5) = +VECT(5) - 2*PDOTU*U(2)
               VECT(6) = +VECT(6) - 2*PDOTU*U(3)
               POLAR(1) = -POLAR(1) + 2*EDOTU*U(1)
               POLAR(2) = -POLAR(2) + 2*EDOTU*U(2)
               POLAR(3) = -POLAR(3) + 2*EDOTU*U(3)
               INWVOL = 0
            ELSE
*
* ***  Calculate amplitude for transmission (Q = P x U)
*
               COST2 = SIGN(1.0,COST1)*SQRT((1-SINT2)*(1+SINT2))
               IF (SINT1.GT.1.E-4) THEN
                  QQ(1) = VECT(5)*U(3) - VECT(6)*U(2)
                  QQ(2) = VECT(6)*U(1) - VECT(4)*U(3)
                  QQ(3) = VECT(4)*U(2) - VECT(5)*U(1)
C                  EPERP1 = (POLAR(1)*QQ(1) + POLAR(2)*QQ(2) + POLAR(3)*
C     +            QQ(3))
C                  ENORM  = SQRT(EPERP1**2+EDOTU**2)
C                  EPERP1 = EPERP1/ENORM
C                  EPARL1 = EDOTU/ENORM
                  ENORM = SQRT(QQ(1)*QQ(1)+QQ(2)*QQ(2)+QQ(3)*QQ(3))
                  EPERP1 = (POLAR(1)*QQ(1) + POLAR(2)*QQ(2) + POLAR(3)*
     +            QQ(3)) / ENORM
                  E1PP(1) = EPERP1 * QQ(1)/ENORM
                  E1PP(2) = EPERP1 * QQ(2)/ENORM
                  E1PP(3) = EPERP1 * QQ(3)/ENORM
                  E1PL(1) = POLAR(1) - E1PP(1)
                  E1PL(2) = POLAR(2) - E1PP(2)
                  E1PL(3) = POLAR(3) - E1PP(3)
                  EPARL1 = SQRT(E1PL(1)*E1PL(1)+E1PL(2)*E1PL(2)+E1PL(3)*
     +            E1PL(3))
               ELSE
                  QQ(1) = POLAR(1)
                  QQ(2) = POLAR(2)
                  QQ(3) = POLAR(3)
*
*     Here we follow Jackson's conventions and we set the parallel
*     component = 1 in case of a ray perpendicular to the surface
                  EPERP1 = 0.
                  EPARL1 = 1.
               END IF
               IF(COST1.NE.0.) THEN
                  S1 = RIN1*COST1
                  EPERP2 = 2*S1*EPERP1/(RIN1*COST1+RIN2*COST2)
                  EPARL2 = 2*S1*EPARL1/(RIN2*COST1+RIN1*COST2)
                  E2 = EPERP2**2 + EPARL2**2
                  S2 = RIN2*COST2*E2
                  TCOEF = S2/S1
               ELSE
                  TCOEF = 0.
               ENDIF
               CALL GRNDM(RNDM,1)
               IF (RNDM.GT.TCOEF) THEN
*
* *** Simulate reflection
*
                  NMEC=NMEC+1
                  LMEC(NMEC)=106
*
* *** Restore old volume tree, the photon does not cross the boundary
*
*                 CALL GLVOLU(NLEVL1,NAMES1,NUMBR1,IERR)
                  CALL GTMEDI(VIN,N)
                  LOLDTR=.FALSE.
*
* *** Reflect, but make sure we are in the old volume before
*
                  NBPUSH = 0
  100             NBPUSH = NBPUSH+1
                  IF(NBPUSH.GT.MXPUSH) THEN
C                     WRITE(CHMAIL,10300) NTMULT,NSTEP
C                     CALL GMAIL(0,0)
                     nloss(5) = nloss(5) + 1
                     ISTOP=1
                     GOTO 110
                  ELSE
                     CALL GINVOL(VECT,ISAME)
                     IF(ISAME.EQ.0) THEN
                        PRECN = NBPUSH*PREC
                        VECT(1) = VECT(1) - PRECN*VECT(4)
                        VECT(2) = VECT(2) - PRECN*VECT(5)
                        VECT(3) = VECT(3) - PRECN*VECT(6)
                        GO TO 100
                     ENDIF
                  ENDIF
*
                  VECT(4) = VECT(4) - 2*PDOTU*U(1)
                  VECT(5) = VECT(5) - 2*PDOTU*U(2)
                  VECT(6) = VECT(6) - 2*PDOTU*U(3)
                  IF(SINT1.GT.1E-4) THEN
                     EPARL2 = RIN2*EPARL2/RIN1-EPARL1
                     EPERP2 = EPERP2-EPERP1
                     E2 = EPERP2**2 + EPARL2**2
                     R(1) = U(1) + PDOTU*VECT(4)
                     R(2) = U(2) + PDOTU*VECT(5)
                     R(3) = U(3) + PDOTU*VECT(6)
                     EABS = SQRT(E2)*SINT1
                     CPARL = EPARL2/EABS
                     CPERP = EPERP2/EABS
                     POLAR(1) = CPARL*R(1) - CPERP*QQ(1)
                     POLAR(2) = CPARL*R(2) - CPERP*QQ(2)
                     POLAR(3) = CPARL*R(3) - CPERP*QQ(3)
                  ELSEIF(RIN2.GT.RIN1) THEN
*
* *** Case of ray perpendicular to the surface. No change or
* *** an inversion of phase.
                     POLAR(1) = -POLAR(1)
                     POLAR(2) = -POLAR(2)
                     POLAR(3) = -POLAR(3)
                  ENDIF
                  INWVOL = 0
               ELSE
*
* *** Simulate transmission/refraction
*
                  NMEC=NMEC+1
                  LMEC(NMEC)=107
                  VECT(1) = VOUT(1)
                  VECT(2) = VOUT(2)
                  VECT(3) = VOUT(3)
                  IF(SINT1.GT.1E-4) THEN
                     ALPHA = COST1-COST2*(RIN2/RIN1)
                     D(1) = VECT(4) + ALPHA*U(1)
                     D(2) = VECT(5) + ALPHA*U(2)
                     D(3) = VECT(6) + ALPHA*U(3)
                     DABS = SQRT(D(1)**2+D(2)**2+D(3)**2)
                     VECT(4) = D(1)/DABS
                     VECT(5) = D(2)/DABS
                     VECT(6) = D(3)/DABS
                     PDOTU = -COST2
                     R(1) = U(1) - PDOTU*VECT(4)
                     R(2) = U(2) - PDOTU*VECT(5)
                     R(3) = U(3) - PDOTU*VECT(6)
                     EABS = SQRT(E2)
                     CPARL = EPARL2/(EABS*SINT2)
                     CPERP = EPERP2/(EABS*SINT1)
                     POLAR(1) = CPARL*R(1) + CPERP*QQ(1)
                     POLAR(2) = CPARL*R(2) + CPERP*QQ(2)
                     POLAR(3) = CPARL*R(3) + CPERP*QQ(3)
                  ENDIF
                  GEKRAT = GEKRT2
                  IEKBIN = IEKBI2
                  STEPLA = ABSCO2
                  EFFIC = EFFIC2
                  RIN1 = RIN2
               END IF
            END IF
         END IF
      ENDIF
      ENORM = SQRT(POLAR(1)**2+POLAR(2)**2+POLAR(3)**2)
      POLAR(1) = POLAR(1)/ENORM
      POLAR(2) = POLAR(2)/ENORM
      POLAR(3) = POLAR(3)/ENORM
*                                                             END GTCKOV
  110 IF(LOLDTR) CALL GLVOLU(NLEVL1,NAMES1,NUMBR1,IERR)
10200  FORMAT(' **** GTCKOV: error from GLISUR = ',I10)
10300  FORMAT(' **** GTCKOV: unable to reflect at NTMULT = ',
     +        I8,' step No. ',I8,' photon abandoned!')
      END
CDECK  ID>, GMULOF. 
*CMZ :  3.21/02 29/03/94  15.41.22  by  S.Giani
*-- Author :
      SUBROUTINE GMULOF
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Calculates table of steps for multiple scattering             *
C.    *     energy loss and magnetic field for electrons,muons         *
C.    *           (cannot be tabuled for hadrons)                      *
C.    *   : smuls  = min (Tbethe , 10*radl)                            *
C.    *   : sloss  = DEEMAX*GEKIN/DEDX                                 *
C.    *   : sfield = CFLD*P                                            *
C.    *                                                                *
C.    *    ==>Called by : GPHYSI                                       *
C.    *       Authors    R.Brun, Y.Dufour, M.Maire  *********          *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
     +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)
C
      REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
      REAL GEKA,GEKB,EKBIN
      INTEGER NEKBIN,NEK1
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL
C
      INTEGER NMAT,NAMATE
      REAL A,Z,DENS,RADL,ABSL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
*
      LOGICAL CERKOV
*
*-----------------------------------------------------------------------
*
      SMULS  = BIG
      SLOSS  = BIG
      SFIELD = BIG
      STOPMX = BIG
      STCKOV = BIG
      JPROB  = LQ(JMA-4)
      JMIXT  = LQ(JMA-5)
      OMC    = Q(JPROB+21)
      CHC2   = Q(JPROB+25)**2
      NLMAT=Q(JMA+11)
      NLM=IABS(NLMAT)
      IF (FIELDM.NE.0.) CFLD = 3333.*DEGRAD*TMAXFD/ABS(FIELDM)
*
      IF(ITCKOV.NE.0.AND.IQ(JTM-2).GE.3.AND. LQ(JTM-3)
     +.NE.0.AND.LQ(LQ(JTM-3)-3).NE.0) THEN
*
* ***  In this tracking medium Cerenkov photons are generated and
* ***  tracked. Set to 1 the corresponding flag and calculate the
* ***  relevant pointers.
*
         CERKOV = .TRUE.
         JTCKOV = LQ(JTM-3)
         JABSCO = LQ(JTCKOV-1)
         JEFFIC = LQ(JTCKOV-2)
         JINDEX = LQ(JTCKOV-3)
         JCURIN = LQ(JTCKOV-4)
         NPCKOV = Q(JTCKOV+1)
      ELSE
         CERKOV = .FALSE.
      ENDIF
*
* *** Electrons
*
      JRANG = LQ(JMA-15)
      IKCUT = MAX((GEKA*LOG10(CUTELE) + GEKB),1.)
      GKC   = (CUTELE-ELOW(IKCUT))/(ELOW(IKCUT+1)-ELOW(IKCUT))
      STOPC = (1.-GKC)*Q(JRANG+IKCUT) + GKC*Q(JRANG+IKCUT+1)
      JMULOF = LQ(JTM-1)
      Q(JMULOF+NEK1+1) = IKCUT
      Q(JMULOF+NEK1+2) = STOPC
*
* *** Recompute STMIN ?
*     set STMIN to the range of an electron at energy=CUTELE + 200KeV
*     divided by sqrt(RADL) (important for light materials)
*
      IF(STMIN.LT.0.)THEN
         XES=CUTELE+2.E-4
         IKS = MAX((GEKA*LOG10(XES) + GEKB),1.)
         GKS   = (XES-ELOW(IKS))/(ELOW(IKS+1)-ELOW(IKS))
         STMIN = (1.-GKS)*Q(JRANG+IKS) + GKS*Q(JRANG+IKS+1) - STOPC
         IF(Q(JTM+7).EQ.0.)THEN
            STMIN = 2.*STMIN/SQRT(RADL)
         ELSE
            STMIN = 5.*STMIN/RADL
         ENDIF
      ENDIF
      Q(JTM+14)=STMIN
*
      DO 10 IEKBIN=1,NEK1
         GEKIN = ELOW(IEKBIN)
         GETOT = GEKIN + EMASS
         PMOM2 = GEKIN*(GETOT+EMASS)
         PMOM  = SQRT(PMOM2)
         BETA2 = PMOM2/(GETOT**2)
*
         IF (IMULS.GT.0.) THEN
            IF(JMIXT.LE.0)THEN
               CALL GMOLIO(A,Z,1.,1,DENS,BETA2,1.,OMC)
            ELSE
               CALL GMOLIO(Q(JMIXT+1),Q(JMIXT+NLM+1),Q(JMIXT+2*NLM+1),
     +                    NLM,DENS,BETA2,1.,OMC)
            ENDIF
            PMCH2  = PMOM2/CHC2
            TBETHE = (PMCH2*BETA2)/LOG(OMC*PMCH2)
            TMXCOR = 2232.*RADL*PMOM2*BETA2
            SMULS  = MIN(TBETHE,TMXCOR,10.*RADL)
         ENDIF
*
         IF (IFIELD*FIELDM.NE.0.) THEN
            SFIELD = CFLD*PMOM
         ENDIF
*
         IF (ILOSS*DEEMAX.GT.0.) THEN
            IF (IEKBIN.LE.IKCUT) THEN
               STOPMX = 0.
               SLOSS  = 0.
            ELSE
               STOPMX = Q(JRANG+IEKBIN)
               EKF = (1.-DEEMAX)*GEKIN
               IF (EKF.LE.ELOW(1)) EKF = ELOW(1)
               IKF = MAX((GEKA*LOG10(EKF) + GEKB),1.)
               GKR = (EKF-ELOW(IKF))/(ELOW(IKF+1)-ELOW(IKF))
               SLOSS = STOPMX-(1.-GKR)*Q(JRANG+IKF)-GKR*Q(JRANG+IKF+1)
               IF (SLOSS.LE.0.) SLOSS = 0.
               STOPMX = STOPMX-STOPC
               IF (STOPMX.LE.0.) STOPMX = 0.
            ENDIF
         ENDIF
         IF(CERKOV) THEN
            CHARGE = 1.
            VECT(7) = PMOM
            CALL GNCKOV
            STCKOV = MXPHOT/MAX(3.*DNDL,1E-10)
         ENDIF
*
         STEP = MIN(SMULS,SLOSS,SFIELD,STCKOV)
         IF (STEP.LT.STMIN) THEN
            STEP = MIN(STMIN,STOPMX)
         ENDIF
         Q(JMULOF+IEKBIN) = STEP
   10 CONTINUE
      DO 20 I=1,IKCUT
         Q(JMULOF+I)=0.5*Q(JMULOF+IKCUT+1)
   20 CONTINUE
*
* *** Muons
*
      JRANG = LQ(JMA-16)
      IKCUT = GEKA*LOG10(CUTMUO) + GEKB
      GKC   = (CUTMUO-ELOW(IKCUT))/(ELOW(IKCUT+1)-ELOW(IKCUT))
      STOPC = (1.-GKC)*Q(JRANG+IKCUT) + GKC*Q(JRANG+IKCUT+1)
      JMULOF = LQ(JTM-2)
      Q(JMULOF+NEK1+1)=IKCUT
      Q(JMULOF+NEK1+2)=STOPC
*
      DO 30 IEKBIN=1,NEK1
         GEKIN = ELOW(IEKBIN)
         GETOT = GEKIN + EMMU
         PMOM2 = GEKIN*(GETOT+EMMU)
         PMOM  = SQRT(PMOM2)
         BETA2 = PMOM2/(GETOT**2)
*
         IF (IMULS.GT.0.) THEN
            IF(JMIXT.LE.0)THEN
               CALL GMOLIO(A,Z,1.,1,DENS,BETA2,1.,OMC)
            ELSE
               CALL GMOLIO(Q(JMIXT+1),Q(JMIXT+NLM+1),Q(JMIXT+2*NLM+1),
     +                    NLM,DENS,BETA2,1.,OMC)
            ENDIF
            PMCH2  = PMOM2/CHC2
            TBETHE = (PMCH2*BETA2)/LOG(OMC*PMCH2)
            TMXCOR = 2232.*RADL*PMOM2*BETA2
            SMULS  = MIN(TBETHE,TMXCOR,10.*RADL)
         ENDIF
*
         IF (IFIELD*FIELDM.NE.0.) THEN
            SFIELD = CFLD*PMOM
         ENDIF
*
         IF (ILOSS*DEEMAX.GT.0.) THEN
            IF (IEKBIN.LE.IKCUT) THEN
               STOPMX = 0.
               SLOSS  = 0.
            ELSE
               STOPMX = Q(JRANG+IEKBIN)
               EKF = (1.-DEEMAX)*GEKIN
               IF (EKF.LE.ELOW(1)) EKF = ELOW(1)
               IKF = GEKA*LOG10(EKF) + GEKB
               GKR = (EKF-ELOW(IKF))/(ELOW(IKF+1)-ELOW(IKF))
               SLOSS = STOPMX-(1.-GKR)*Q(JRANG+IKF)-GKR*Q(JRANG+IKF+1)
               IF (SLOSS.LE.0.) SLOSS = 0.
               STOPMX = STOPMX-STOPC
               IF (STOPMX.LE.0.) STOPMX = 0.
            ENDIF
         ENDIF
         IF(CERKOV) THEN
            CHARGE = 1.
            VECT(7) = PMOM
            CALL GNCKOV
            STCKOV = MXPHOT/MAX(3.*DNDL,1E-10)
         ENDIF
*
         STEP = MIN(SMULS,SLOSS,SFIELD,STCKOV)
         IF (STEP.LT.STMIN) THEN
            STEP = MIN(STMIN,STOPMX)
         ENDIF
         Q(JMULOF+IEKBIN) = STEP
   30 CONTINUE
      DO 40 I=1,IKCUT
         Q(JMULOF+I)=0.5*Q(JMULOF+IKCUT+1)
   40 CONTINUE
*
      END
CDECK  ID>, GSKPHO. 
*CMZ :  3.21/02 03/07/94  19.07.28  by  S.Giani
*-- Author :
      SUBROUTINE GSKPHO (IGK)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *   Stores in stack JSTAK either the IGKth Cherenkov photon of   *
C.    *   /GCKIN2/, or the NPHOT tracks when IGK is 0.                 *
C.    *                                                                *
C.    *   Called by : 'User'                                           *
C.    *   Authors   : F.Carminati                                      *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
     +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
     +                  ,JMULOF,JCOEF,JRANG
C
      COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO
C
      EQUIVALENCE (JLASTV,JTSTEN)
C
      INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
     +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
     +                   ,JTASHO
C
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK,MXPHOT,NGPHOT
      REAL          GKIN,TOFD,XPHOT
C
      PARAMETER (MXPHOT=20000)
      COMMON/GCKIN2/NGPHOT,XPHOT(11,MXPHOT)
C
      COMMON/GCKIN3/GPOS(3,MXGKIN)
      REAL          GPOS
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCMZFO/IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO(13)
C
      INTEGER       IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
*
      PARAMETER (NWPOLA=3)
C.
C.    ------------------------------------------------------------------
C.
*
* *** Make sure the request is reasonable
      IF (NGPHOT.LE.0) GO TO 999
      IF (IGK.EQ.0) THEN
         N1 = 1
         N2 = NGPHOT
      ELSE
         IF (IGK.LT.0.OR.IGK.GT.NGPHOT) THEN
            WRITE(CHMAIL,10000) IGK, NGPHOT
10000 FORMAT(' **** GSKPHO: ',I6,'th photon requested, ',
     + I6,' in stack')
            CALL GMAIL(0,0)
            GO TO 999
         ENDIF
         N1 = IGK
         N2 = IGK
      ENDIF
*
* *** Store photons in stack JSTAK
*
* *** Check that the bank is there and big enough
      IF (JSTAK.EQ.0) THEN
         NDBOOK = NTSTKP*NWSTAK +3
         NDPUSH = NTSTKS*NWSTAK
         CALL MZBOOK (IXCONS,JSTAK,JSTAK,1,'STAK', 1,1,NDBOOK, IOSTAK,3)
         IQ(JSTAK+2) = NTSTKP
      ENDIF
      IF(IQ(JSTAK-2).EQ.0) THEN
         CALL MZPUSH(IXCONS,JSTAK,1,0,'I')
      ENDIF
      IF(LQ(JSTAK-1).EQ.0) THEN
         CALL MZBOOK(IXCONS,JPOLAR,JSTAK,-1,'POLA',0,0,
     +               NTSTKP*NWPOLA,3,-1)
      ENDIF
      MISSNG=IQ(JSTAK+1)+N2-N1+1-IQ(JSTAK+2)
      IF (MISSNG.GT.0) THEN
         CALL MZPUSH (IXCONS, JSTAK, 0, MISSNG*NWSTAK+NDPUSH, 'I')
         IQ(JSTAK+2) = IQ(JSTAK+2) +NTSTKS+MISSNG
      ENDIF
      MSSPOL = IQ(JSTAK+1)+N2-N1+1-IQ(JPOLAR-1)/3
      IF(MSSPOL.GT.0) THEN
         CALL MZPUSH (IXCONS,JPOLAR,0,(NTSTKS+MSSPOL)*NWPOLA, 'I')
      ENDIF
*
* *** Now we can go
      DO 20  NN=N1,N2
         JST = JSTAK  +IQ(JSTAK+1)*NWSTAK +3
         JPO = JPOLAR +IQ(JSTAK+1)*NWPOLA
         IQ(JSTAK+1) = IQ(JSTAK+1) +1
         IF (IQ(JSTAK+3).EQ.0) IQ(JSTAK+3) = IQ(JSTAK+1)
         IF (IQ(JSTAK+1).GT.NSTMAX) NSTMAX = IQ(JSTAK+1)
*
         IQ(JST+1) = -ITRA
         IQ(JST+2) = 50
         IQ(JST+3) = 0
         DO 10 I = 1,3
            Q(JST+3+I) = XPHOT(I  ,NN)
            Q(JST+6+I) = XPHOT(I+3,NN)*XPHOT(7,NN)
            Q(JPO+  I) = XPHOT(I+7,NN)
   10    CONTINUE
         Q(JST+10) = XPHOT(11,NN)
         Q(JST+11) = SAFETY
         Q(JST+12) = UPWGHT
*
         NALIVE = NALIVE +1
   20 CONTINUE
*                                                             END GSKPHO
  999 END
CDECK  ID>, GLTRAC. 
*CMZ :  3.21/04 13/12/94  15.36.22  by  S.Giani
*-- Author :
      SUBROUTINE GLTRAC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GLTRAC                                                *
C.    *                                                                *
C.    *   Extracts next track from stack JSTAK and prepares commons    *
C.    *    /GCTRAK/, /GCKINE/ and /GCVOLU/                             *
C.    *                                                                *
C.    *   Called by : GTREVE                                           *
C.    *   Authors   : R.Brun, F.Bruyant                                *
C.    *                                                                *
C.    ******************************************************************
C.
      INTEGER IQ,LQ,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JCG
      INTEGER KWBANK,KWWORK,IWS
      REAL GVERSN,ZVERSN,FENDQ,WS,Q
C
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      INTEGER       JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO
C
      DOUBLE PRECISION PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      DOUBLE PRECISION EMMU,PMASS,AVO
*
      PARAMETER (PI=3.14159265358979324D0)
      PARAMETER (TWOPI=6.28318530717958648D0)
      PARAMETER (PIBY2=1.57079632679489662D0)
      PARAMETER (DEGRAD=0.0174532925199432958D0)
      PARAMETER (RADDEG=57.2957795130823209D0)
      PARAMETER (CLIGHT=29979245800.D0)
      PARAMETER (BIG=10000000000.D0)
      PARAMETER (EMASS=0.0005109990615D0)
      PARAMETER (EMMU=0.105658387D0)
      PARAMETER (PMASS=0.9382723128D0)
      PARAMETER (AVO=0.60221367D0)
*
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
      COMMON/GCPHLT/ILABS,SLABS,SLLABS,ZINTLA,STEPLA
     +             ,ISYNC
     +             ,ISTRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL,ILABS,ISYNC,ISTRA
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA,SLABS,SLLABS,ZINTLA,STEPLA
C
      INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
     +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
      INTEGER ISTORD
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
      EQUIVALENCE (ISTORD,NJTMIN)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
      COMMON/GCTLIT/THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV
C
      INTEGER       NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD
      REAL          FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC
      INTEGER       JMIN,NPCKOV,IMCKOV,ITCKOV
      REAL          THRIND,PMIN,DP,DNDL
C
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
     + ,MAXME1,NAMEC1
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      REAL POLAR
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
      PARAMETER (MAXME1=30)
      COMMON/GCTPOL/POLAR(3), NAMEC1(MAXME1)
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX
      DIMENSION RNDM(5)
      DOUBLE PRECISION P2,GETOTD,GEKIND
      DOUBLE PRECISION PXD,PYD,PZD,ONE,HNORM,DAMASS,PP
      PARAMETER (ONE=1)
C.
C.    ------------------------------------------------------------------
*
* *** Extract next track from stack JSTAK
*
      IF(ISTORD.EQ.1) THEN
*
* *** User ordering of tracks if requested
         CALL GSTORD
      ENDIF
      ISTAK = IQ(JSTAK+1)
      IQ(JSTAK+1) = ISTAK -1
      JST = JSTAK +NWSTAK*IQ(JSTAK+1) +3
      ITRA   = IQ(JST+1)
      IF (ITRA.LT.0) THEN
         ITRA = -ITRA
      ELSE
*
*        This is a new track. We set to zero the stack number and
*        update the vertex number
         ISTAK = 0
         JK=LQ(JKINE-ITRA)
         IVERT=Q(JK+6)
      ENDIF
      IPART  = IQ(JST+2)
      DO 60 I = 1,3
         VERT(I) = Q(JST+3+I)
        PVERT(I) = Q(JST+6+I)
   60 CONTINUE
      TOFG   = Q(JST+10)
      SAFETY = Q(JST+11)
      UPWGHT = Q(JST+12)
*
* *** Prepare tracking parameters
*
      VECT(1) = VERT(1)
      VECT(2) = VERT(2)
      VECT(3) = VERT(3)
      PXD = PVERT(1)
      PYD = PVERT(2)
      PZD = PVERT(3)
      P2 = PXD**2+PYD**2+PZD**2
      IF(P2.GT.0.) THEN
         PP    = SQRT(P2)
         HNORM = ONE/PP
         VECT(4) = PVERT(1)*HNORM
         VECT(5) = PVERT(2)*HNORM
         VECT(6) = PVERT(3)*HNORM
         VECT(7) = PP
      ELSE
         VECT(4) = 0.
         VECT(5) = 0.
         VECT(6) = 1.
         VECT(7) = 0.
      ENDIF
*
*  ** Reload Particle characteristics, if needed
*
      IF (IPART.NE.IPAOLD) THEN
         JPA = LQ(JPART-IPART)
         DO 90 I = 1,5
            NAPART(I) = IQ(JPA+I)
   90    CONTINUE
         ITRTYP = Q(JPA+6)
         AMASS  = Q(JPA+7)
         CHARGE = Q(JPA+8)
         TLIFE  = Q(JPA+9)
         IUPD   = 0
         IPAOLD = IPART
      ENDIF
*
      DAMASS = AMASS
      GETOTD = SQRT(P2+DAMASS**2)
      GEKIND = GETOTD - DAMASS
      GETOT  = GETOTD
      GEKIN  = GEKIND
*
      IF (ITRTYP.EQ.7) THEN
*
* *** Cerenkov photon. Retrieve polarisation
         JPO = LQ(JSTAK-1)+(ISTAK-1)*3
         POLAR(1) = Q(JPO+1)
         POLAR(2) = Q(JPO+2)
         POLAR(3) = Q(JPO+3)
      ELSE
         CALL GEKBIN
      ENDIF
*
      SLENG  = 0.
      NSTEP  = 0
      NTMSTO = NTMSTO +1
      NTMULT = NTMSTO
      ISTORY = 0
*
*  ** Initialize interaction probabilities
*
      IF (ITRTYP.EQ.1) THEN
*      Gammas
         CALL GRNDM(RNDM,5)
         ZINTPA = -LOG(RNDM(1))
         ZINTCO = -LOG(RNDM(2))
         ZINTPH = -LOG(RNDM(3))
         ZINTPF = -LOG(RNDM(4))
         ZINTRA = -LOG(RNDM(5))
      ELSE IF (ITRTYP.EQ.2) THEN
*       Electrons
         CALL GRNDM(RNDM,3)
         ZINTBR = -LOG(RNDM(1))
         ZINTDR = -LOG(RNDM(2))
         ZINTAN = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.3) THEN
*       Neutral hadrons
         CALL GRNDM(RNDM,2)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTHA = -LOG(RNDM(2))
      ELSE IF (ITRTYP.EQ.4) THEN
*       Charged hadrons
         CALL GRNDM(RNDM,3)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTHA = -LOG(RNDM(2))
         ZINTDR = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.5) THEN
*       Muons
         CALL GRNDM(RNDM,5)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTBR = -LOG(RNDM(2))
         ZINTPA = -LOG(RNDM(3))
         ZINTDR = -LOG(RNDM(4))
         ZINTMU = -LOG(RNDM(5))
      ELSE IF (ITRTYP.EQ.7) THEN
*       Cerenkov photons
         CALL GRNDM(RNDM,1)
         ZINTLA = -LOG(RNDM(1))
      ELSE IF (ITRTYP.EQ.8) THEN
*       Ions
         CALL GRNDM(RNDM,3)
         SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
         ZINTHA = -LOG(RNDM(2))
         ZINTDR = -LOG(RNDM(3))
      ENDIF
*
*   * Prepare common /GCVOLU/ and structure JGPAR, if needed
*
      IF (NJTMAX.LE.0) THEN
        IF (GONLY(NLEVEL).EQ.0.) NLEVEL=0
        CALL GMEDIA (VECT, NUMED)
      ENDIF
      INFROM = 0
*                                                             END GLTRAC
      END
