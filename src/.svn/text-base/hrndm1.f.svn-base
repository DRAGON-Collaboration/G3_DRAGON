# 1 "hrndm1.F"
*
* $Id: hrndm1.f,v 1.1.1.1 2003-09-25 22:25:18 dragoncvs Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
* #include "hbook/pilot.h"
*CMZ :  4.19/00 26/04/93  11.57.51  by  Rene Brun
*-- Author :
      FUNCTION HRNDM1(IDD)
*.==========>
*.           RETURN IN HRNDM1 A RANDOM NUMBER DISTRIBUTED
*.           FOLLOWING THE CONTENTS OF HISTOGRAM IDD.
*.                  ( IDD CAN CONTAINS A FUNCTION  )
*..=========> ( R.Brun )
# 1 "/cern/pro/src/packlib/hbook//hbook/hcbook.inc" 1 
*
* $Id: hrndm1.f,v 1.1.1.1 2003-09-25 22:25:18 dragoncvs Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*


*
*
* hcbook.inc
*
*CMZ :  4.19/01 30/04/93  17.22.15  by  Rene Brun
*-- Author :
      INTEGER     NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,        LMAIN
      REAL                                       FENC   ,      HCV
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      INTEGER   IQ        ,LQ
      REAL            Q
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      INTEGER       HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     *LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     *LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     *LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM,
     *LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     *LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     *LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     *LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM(9),
     *LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*
# 1 "/cern/pro/src/packlib/hbook//hbook/hck.inc" 1 
*
* $Id: hrndm1.f,v 1.1.1.1 2003-09-25 22:25:18 dragoncvs Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*


*
*
* hck.inc
*
*CMZ :  4.19/00 26/04/93  12.34.29  by  Rene Brun
*-- Author :
      INTEGER   KNCX   ,KXMIN  ,KXMAX  ,KMIN1  ,KMAX1 ,KNORM  , KTIT1,
     *          KNCY   ,KYMIN  ,KYMAX  ,KMIN2  ,KMAX2 ,KSCAL2 , KTIT2,
     *          KNBIT  ,KNOENT ,KSTAT1 ,KNSDIR  ,KNRH ,
     *          KCON1  ,KCON2  ,KBITS  ,KNTOT
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KMIN1=7,KMAX1=8,KNORM=9,KTIT1=10,
     *          KNCY=7,KYMIN=8,KYMAX=9,KMIN2=6,KMAX2=10,KSCAL2=11,
     *          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     *          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*


# 36 "/cern/pro/src/packlib/hbook//hbook/hcbook.inc" 2 


# 19 "hrndm1.F" 2 
# 1 "/cern/pro/src/packlib/hbook//hbook/hcflag.inc" 1 
*
* $Id: hrndm1.f,v 1.1.1.1 2003-09-25 22:25:18 dragoncvs Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*


*
*
* hcflag.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
      INTEGER       ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     *       NCHAR ,NRHIST,IERR  ,NV
      COMMON/HCFLAG/ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     *       NCHAR ,NRHIST,IERR  ,NV
*


# 20 "hrndm1.F" 2 
      SAVE JDLAST,NER,NCHA,XMIN,XWID
      DATA JDLAST/0/
      DATA NER/0/
*.___________________________________________
      IF(IDD.NE.IDLAST)GO TO 5
      IF(IDD.EQ.JDLAST)GO TO 40
*
*             LOCATE IDD IN BLANK COMMON
*
   5  JDLAST=IDD
      ID=IDD
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)GO TO 99
      LCID=LQ(LTAB-IDPOS)
      LCONT=LQ(LCID-1)
      IDLAST=IDD
*
      NCHA=IQ(LCID+KNCX)
      XMIN=Q(LCID+KXMIN)
      XWID=(Q(LCID+KXMAX)-Q(LCID+KXMIN))/FLOAT(IQ(LCID+KNCX))
*
*             COMPUTE INTEGRALE AND NORMALIZE
      IF(IQ(LCONT+KNOENT).EQ.-1)GO TO 40
      IF(IQ(LCONT+KNBIT).GE.32)GO TO 7
      IF(NER.EQ.0)CALL HBUG('Histogram with packing','HRNDM1',IDD)
      NER=1
      GO TO 99
*
  7   IQ(LCONT+KNOENT)=-1
  10  CONTINUE
*
      DO 20 I=2,NCHA
  20  Q(LCONT+KCON1+I)=Q(LCONT+KCON1+I)+Q(LCONT+KCON1+I-1)
      IF(Q(LCONT+KCON1+NCHA).NE.0.)GO TO 27
*
      DO 25 I=1,NCHA
  25  Q(LCONT+KCON1+I)=1.
      CALL HBUG('Integral is zero','HRNDM1',IDD)
      GO TO 10
*
  27  CONTINUE
      DO 30 I=1,NCHA
  30  Q(LCONT+KCON1+I)=Q(LCONT+KCON1+I)/Q(LCONT+KCON1+NCHA)
      Q(LCONT+KCON1)=0.
      CALL SBIT0(IQ(LCID),5)
*
*
*             NORMAL ENTRY
*
*****************************************************************************
* 14.08.2003 C. Ruiz: updated call to RNDM (obsolete CERN function) to the  *
* newer GRNDM (RANECU) routine to ensure synchronisation with other         *
* generators in GEANT                                                       *
*****************************************************************************
  40  CALL GRNDM(R,1)
* R=RNDM(XMIN)
      ICHA=LOCATF(Q(LCONT+KCON1+1),NCHA,R)
      KCHA=IABS(ICHA)
      ICHA=KCHA+LCONT+KCON1+1
      DY=Q(ICHA)-Q(ICHA-1)
      IF(DY.GT.0.)THEN
         HRNDM1=XMIN+(R-Q(ICHA-1))*XWID/DY + XWID*FLOAT(KCHA)
      ELSE
         HRNDM1=XMIN + XWID*FLOAT(KCHA)
      ENDIF
      RETURN
*
  99  HRNDM1=0.
      IDLAST=0
      END
