CDECK  ID>, GTHION. 
*CMZ :  3.21/02 03/07/94  17.58.49  by  S.Giani
*-- Author :
      SUBROUTINE GTHION
      IMPLICIT NONE
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   Heavy  ion  type track. Computes step size and propagates    *
C.    *    particle through step.                                      *
C.    *                                                                *
C.    *   The ionisation energy loss is calculated here (mean +        *
C.    *       fluctuations)                                            *
C.    *   The fluctuations are the same for ILOSS=1,2,3 and            *
C.    *       there is no fluctuation for ILOSS=4.                     *
C.    *                                                                *
C.    *   ==>Called by : GTRACK                                        *
C.    *       Authors    R.Brun, F.Bruyant, M.Maire, L.Urban ***       *
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
      REAL*4 DELTAP
C
      REAL epsmac,thresh,twothr,amu,dme,cnorm

      PARAMETER (EPSMAC=1.E-6)
      DOUBLE PRECISION GKR,DEMEAN,STOPP,STOPMX,STOPRG,STOPC,EKIPR
      DOUBLE PRECISION ONE,XCOEF1,XCOEF2,XCOEF3,YCOEF1,YCOEF2,YCOEF3
      PARAMETER (THRESH=0.7,ONE=1)
      PARAMETER (TWOTHR=2*ONE/3,AMU=0.9314943)
      PARAMETER (DME=7.84572E-8,CNORM=2.5)
      REAL VNEXT(6)
      REAL RNDM(2),RMASS,CUTPRO,FACFLU,CHAR23
      REAL charg1, GAMASS,bet2,bet,w1,w2,w3,charg2,cfld,gekrt1
      real tmax,y,sig,ekf,slosp,stckov,VECTMP,corr,sigma2,ta
      REAL tam,sigma,cappa,gekint,gamma,defluc
      integer iproc,i1,ikf,ik1,ikcut,ist,jst,i,ier,inear,isame
      SAVE RMASS,CUTPRO,IKCUT,STOPC,FACFLU,CHAR23
C.
C.    ------------------------------------------------------------------
*
* *** Particle below energy threshold ? short circuit
*
      DELTAP = 0.0
      IF (GEKIN.LE.CUTHAD) GO TO 100
*
* *** Update local pointers if medium has changed
*
      IF (IUPD.EQ.0) THEN
         IUPD  = 1
         JLOSS = LQ(JMA-3)
         JRANG = LQ(JMA-16) + NEK1
         JCOEF = LQ(JMA-18) + 3*NEK1
         RMASS  = PMASS/AMASS
         CUTPRO = MAX(CUTHAD*RMASS,ELOW(1))
         IKCUT = GEKA*LOG10(CUTPRO) + GEKB
         GKR   = (CUTPRO - ELOW(IKCUT))/(ELOW(IKCUT+1) - ELOW(IKCUT))
         STOPC = (1.-GKR)*Q(JRANG+IKCUT) + GKR*Q(JRANG+IKCUT+1)
         FACFLU = DME*(Z*DENS/A)
         CHAR23 = ONE/CHARGE**TWOTHR
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
* *** Retrieve the user effective charge
*
      CHARG1 = Q(LQ(JPART-IPART)+10) 
*
* *** Compute energy dependent parameters
*
      IF(NUMED.le.4)THEN         ! for vacuum -> user effective charge !
        CHARG1 = Q(LQ(JPART-IPART)+10)
      ELSE
        GAMASS=GETOT+AMASS
        BET2=GEKIN*GAMASS/(GETOT*GETOT)
        BET=SQRT(BET2)
        W1=1.034-0.1777*EXP(-0.08114*CHARGE)
        W2=BET*CHAR23
        W3=121.4139*W2+0.0378*SIN(190.7165*W2)
        CHARG1=CHARGE*(1.-W1*EXP(-W3))
      END IF
*
*              the effective charge  CHARG1
*            can be negative only for very low energy and
*     for CHARGE > 20 ( very low energy : T/A < 20 keV/nucleon)
*              in this case short circuit
*
      IF(CHARG1.LT.0.) GOTO 100
      CHARG2=CHARG1**2
*
      OMCMOL=Q(JPROB+21)*CHARG2
      CHCMOL=Q(JPROB+25)*ABS(CHARG1)
      IF(FIELDM.NE.0.) THEN
         CFLD=3333.*DEGRAD*TMAXFD/ABS(FIELDM*CHARG1)
      ELSE
         CFLD=BIG
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
*  ==   Particle decays for heavy ions AO 8-22-97
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
            TMAX   = EMASS*GEKIN*GAMASS/(0.5*AMASS*AMASS+EMASS*GETOT)
            IF (TMAX.GT.DCUTM) THEN
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
*  **   Step limitation due to energy loss in magnetic field ?
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
         SMULS=MIN(2232.*RADL*((VECT(7)**2)/(GETOT*CHARG1))**2,10.*RADL)
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
   50    CALL GUSWIM (CHARG1, STEP, VECT, VOUT)
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
         DELTAP = VOUT(7) - VECT(7)
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
C           NMEC=NMEC+1
C           LMEC(NMEC)=105
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
     +             *STEP*CHARG2
         ENDIF
*
*        fluctuations : differ from that of 'ordinary' hadrons
*
         IF (ILOSS.EQ.4.OR.IEKBIN.LE.IKCUT+1) THEN
            DESTEP = DEMEAN
         ELSE
*
*     Charge exchange fluctuations + Gaussian 'Landau' fluctuations
*           (it is the same for ILOSS=1,2,3 !)
*
            SIGMA2=CNORM*CHARG1*(1.-CHARG1/CHARGE)
            SIGMA2=MAX(SIGMA2,0.)
            TA = RMASS*GEKIN
            TAM=TA/AMU
            SIGMA2=SIGMA2+2.+TAM*(2.+TAM)
*
            SIGMA2=FACFLU*CHARG2*STEP*SIGMA2
            IF(SIGMA2.GT.0.0) THEN
                SIGMA=SQRT(SIGMA2)
            ELSE
                SIGMA= 0.0
            END IF
*
*     Check if we are in 'Gaussian' regime ...
*
            CAPPA=(1.+TAM)/(TAM*(2.+TAM)*EMASS)
            CAPPA=0.5*CAPPA**2*FACFLU*CHARG2*STEP
*
*     ... if not , correct SIGMA !

            IF( (CAPPA.LT.10.) .AND. (CAPPA.GT.0.0) ) THEN
               SIGMA=SIGMA/(0.97+0.03*SQRT(10./CAPPA))
            ENDIF
*
            CALL GRNDM(RNDM,2)
            DEFLUC=SIGMA*SIN(TWOPI*RNDM(1))*SQRT(-2.*LOG(RNDM(2)))
            DESTEP=DEMEAN+DEFLUC
         ENDIF
*
*     protection against negative destep
*
         IF(DESTEP.LT.0.) DESTEP=DEMEAN
*                          IF (DESTEP.LT.0.) DESTEP = 0.
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
*      check charge dependence ...........!!!!!!!  (later..)
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
* *** Update the total energy, if the ion has been accelrated
*
      IF(ABS(DELTAP) .GT. 1.E-6)THEN
        VECT(7) = VECT(7) + DELTAP
c**        GETOT = SQRT(VOUT(7)**2+AMASS*AMASS) !Wrong energy
        GETOT = SQRT(VECT(7)**2+AMASS*AMASS) !Wrong energy

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
* *** apply the selected process if any
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
* ** ADD PROCESSING OF HI DECAYS  AO 22/8/97
*
*  **   Decay ?
* 
      ELSE IF (IPROC .EQ. 5) THEN
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
*  The ion has produced Cerenkov photons and it is still alive
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
