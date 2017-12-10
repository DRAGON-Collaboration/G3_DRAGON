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
