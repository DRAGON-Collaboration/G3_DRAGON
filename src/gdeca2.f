*
* $Id: gdeca2.f,v 1.2 2003-10-02 00:10:28 olin Exp $
*
* $Log: not supported by cvs2svn $
* Revision 1.1.1.1  2003/09/25 22:25:18  dragoncvs
* Original Sources - A.Olin 24/09/03
*
* Revision 1.1.1.1  1995/10/24 10:21:23  cernlib
* Geant
*
*
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GDECA2(XM0,XM1,XM2,PCM)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Simulates two body decay process with isotropic angular       *
C.    *  distribution in CMS.                                          *
C.    *                                                                *
C.    *    ==>Called by : GDECAY                                       *
C.    *       Author    G.Patrick  *********                           *
C.    *                                                                *
C.    ******************************************************************
C.
      include 'gconsp.inc'
      include 'gckine.inc'
      include 'history.inc'
      DIMENSION PCM(4,2)
      DIMENSION RNDM(2)
      LOGICAL HEXIST
C.
C.--> Decay number
C.
      Nodec = Nodec + 1
      
C.
C.    ------------------------------------------------------------------
C.
C             Generate first decay product in CMS.
C
      E1=(XM0*XM0+XM1*XM1-XM2*XM2)/(2.*XM0)
      P1=SQRT(ABS((E1-XM1)*(E1+XM1)))
C
C             Isotropic decay angular distribution code.
C
      CALL GRNDM(RNDM,2)
      COSTH=2.*RNDM(1)-1.
      IF(ABS(COSTH).GE.1.0) THEN
         COSTH=SIGN(1.,COSTH)
         SINTH=0.
      ELSE
         SINTH=SQRT((1.-COSTH)*(1.+COSTH))
      ENDIF
      PHI=TWOPI*RNDM(2)
C This decays  the resonance branches to all states with angular distribution
C defined in angdist.f - currently quadrupole.
C it is intended to expand this so that the multipolarity for each decay branch
C can be specified in the reaction input file.
C decays in
C
C
C             Angular Distribution from user function
C    - HBOOK functions are used to generate angular distributions,
C      Histograms 250-300 will be reserved for these.
C
C      test for histogram for current decay (set to 250 for test)
       
       If (Hexist(250) .AND. IPART.EQ.81) then
        COSTH = HRNDM1(250)
        SINTH = SQRT((1.-COSTH)*(1.+COSTH))
       else

       Endif
C.
C.-->  Gamma Decay Ntuples in CM frame
C.
       If(Nodec.ne.0)then
        cost_g(Nodec) = COSTH
        phi_g(Nodec) = PHI
        E_g(Nodec) = (XM0-XM2)*1000.
       Endif
c       print*, E_g

C
C             Polar co-ordinates to momentum components.
C
      PCM(1,1)=P1*SINTH*COS(PHI)
      PCM(2,1)=P1*SINTH*SIN(PHI)
      PCM(3,1)=P1*COSTH
      PCM(4,1)=E1
C
C             Generate second decay product.
C
      PCM(1,2)=-PCM(1,1)
      PCM(2,2)=-PCM(2,1)
      PCM(3,2)=-PCM(3,1)
      PCM(4,2)=SQRT(PCM(1,2)**2+PCM(2,2)**2+PCM(3,2)**2+XM2*XM2)
C
C
      END










