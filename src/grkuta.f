      SUBROUTINE GRKUTA (CHARGE, AMASS, STEP, TOFG, VECT, VOUT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Runge-Kutta method for tracking a particle through a magnetic *
C.    *  field. Uses Nystroem algorithm (See Handbook Nat. Bur. of     *
C.    *  Standards, procedure 25.5.20)                                 *
C.    *                                                                *
C.    *  Input parameters                                              *
C.    *       CHARGE    Particle charge                                *
C.    *       AMASS     Particle Mass                                  *
C.    *       STEP      Step size                                      *
C.    *       TOFG      Time of flight, used to compute E.M. Field     *
C.    *       VECT      Initial co-ords,direction cosines,momentum     *
C.    *  Output parameters                                             *
C.    *       VOUT      Output co-ords,direction cosines,momentum      *
C.    *  User routine called                                           *
C.    *       CALL GUEFLD(X,T,F,E)                                     *
C.    *          X : vector, dim. to 3                                 *
C.    *          T : scalar, time at which fields are computed         *
C.    *          F : Magnetic field, kGauss                            *
C.    *          E : Electric field, GeV/centimeter.                   *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUSWIM                               *
C.    *       Authors    R.Brun, M.Hansroul  *********                 *
C.    *                  V.Perevoztchikov (CUT STEP implementation)    *
C.    *                                                                *
C.    *      Upgrade for electric field.  Paul LeBrun Dec 1997	       *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      REAL CHARGE, STEP, VECT(*), VOUT(*), F(4)
      REAL XYZT(3), XYZ(3), X, Y, Z, XT, YT, ZT
      EQUIVALENCE (X,XYZ(1)),(Y,XYZ(2)),(Z,XYZ(3)),
     +            (XT,XYZT(1)),(YT,XYZT(2)),(ZT,XYZT(3))
C.
      DOUBLE PRECISION SECXS(4),SECYS(4),SECZS(4),HXP(3)
C.
      REAL AMASS, TOFG, TH2, TH4, E(3)
      DOUBLE PRECISION PK(3,3), DPC(3,3), PNEW(3), DPCN(3)
C.
      INTEGER MAXIT, MAXCUT
      INTEGER IX, IY, IZ, IPX, IPY, IPZ
C.
      DOUBLE PRECISION EC, DLT, DLT32
      DOUBLE PRECISION ZERO, ONE, TWO, THREE, THIRD, HALF
      DOUBLE PRECISION PISQUA
      DOUBLE PRECISION CLIGHT
C.
      PARAMETER (MAXIT = 1992, MAXCUT = 11)
      PARAMETER (EC=2.99792458D-4, DLT=1D-4, DLT32=DLT/32)
      PARAMETER (ZERO=0.D0, ONE=1.D0, TWO=2.D0, THREE=3.D0)
      PARAMETER (THIRD=ONE/THREE, HALF=ONE/TWO)
      PARAMETER (PISQUA=.986960440109D+01)
      PARAMETER      (IX=1,IY=2,IZ=3,IPX=4,IPY=5,IPZ=6)
C.
      PARAMETER (CLIGHT = 2.9979245800D0)
C.
      INTEGER ITER, NCUT, J, K
      DOUBLE PRECISION PINV, TL, REST, A, B, C, H, H2, H4, PH, PH2
      DOUBLE PRECISION ANG2, EST, DXT, DYT, DZT, AT, BT, CT
      DOUBLE PRECISION CBA, F1, F2, F3, F4, TET, HNORM, HP
      DOUBLE PRECISION RHO, RHO1, SINT, COST, G1, G2, G3, G4, G5, G6
      DOUBLE PRECISION AMASS2, E1, E2, E3, P1, P2, ENORM, BETA, DIST
C.
C.    ------------------------------------------------------------------
C.
C.             This constant is for units CM,GEV/C and KGAUSS
C.
      ITER = 0
      NCUT = 0
      DO 10 J=1,7
         VOUT(J)=VECT(J)
   10 CONTINUE
      PINV   = EC * CHARGE / VECT(7)
      TL = 0.D0
      H      = STEP
C.
      AMASS2 = AMASS**2

   20 REST  = STEP-TL
      IF (ABS(H).GT.ABS(REST)) H = REST
      CALL GUEFLD(VOUT, TOFG, F, E)
C.
C.             Start of integration
C.
      X      = VOUT(1)
      Y      = VOUT(2)
      Z      = VOUT(3)
      A      = VOUT(4)
      B      = VOUT(5)
      C      = VOUT(6)
C.
      P1     = VOUT(7)
C.
      H2     = HALF * H
      H4     = HALF * H2
      PH     = PINV * H
      PH2    = HALF * PH
C.
C.	       Compute the acceleration after step H2 stage. 
C.
      ENORM = SQRT(E(1)**2 + E(2)**2 + E(3)**2)
      E1 = DSQRT(AMASS2 + P1 * P1)
C.
      IF (ENORM .GT. 0.D0) THEN
C.
C.	Acceleration of step of H2 will be ..
C.         
         DO K = 1,3
            DPC(K,1) = CHARGE*E(K)*H2*E1/P1
            PK(K,1) = VOUT(3+K)*P1 + DPC(K,1)
         ENDDO
         PNEW(1) = DSQRT(PK(1,1)**2 + PK(2,1)**2 + PK(3,1)**2)
         DPCN(1) = DSQRT(DPC(1,1)**2 + DPC(2,1)**2 + DPC(3,1)**2) 
         E2 = DSQRT(AMASS2 + PNEW(1) * PNEW(1))
         BETA = PNEW(1)/E2
         SECXS(1) = DPC(1,1)/P1
         SECYS(1) = DPC(2,1)/P1
         SECZS(1) = DPC(3,1)/P1
      ELSE
         PNEW(1) = P1
         BETA = P1/E1
         SECXS(1) = 0.D0
         SECYS(1) = 0.D0
         SECZS(1) = 0.D0
      END IF
C.  
      SECXS(1) = SECXS(1) + (B * F(3) - C * F(2)) * PH2
      SECYS(1) = SECYS(1) + (C * F(1) - A * F(3)) * PH2
      SECZS(1) = SECZS(1) + (A * F(2) - B * F(1)) * PH2
      ANG2 = (SECXS(1)**2 + SECYS(1)**2 + SECZS(1)**2)
      IF (ANG2.GT.PISQUA) GO TO 40
      DXT    = H2 * A + H4 * SECXS(1)
      DYT    = H2 * B + H4 * SECYS(1)
      DZT    = H2 * C + H4 * SECZS(1)
      XT     = X + DXT
      YT     = Y + DYT
      ZT     = Z + DZT
C.
C.             Second intermediate point
C.
      EST = ABS(DXT)+ABS(DYT)+ABS(DZT)
      IF (EST.GT.H) GO TO 30
C.      
      DIST = DSQRT(DXT*DXT + DYT*DYT + DZT*DZT)
      TH2 = TOFG + DIST/(BETA*CLIGHT)
C.
      P2 = PNEW(1)
      E2 = DSQRT(AMASS2 + P2 * P2)
C.
      CALL GUEFLD(XYZT, TH2, F, E)
      ENORM = SQRT(E(1)**2 + E(2)**2 + E(3)**2)
C.
      IF (ENORM .GT. 0.D0) THEN 
         DO K = 1,3
            DPC(K,2) = CHARGE * E(K) * H2 * E2/P2
            PK(K,2) = PK(K,1) + DPC(K,2)
         ENDDO
         PNEW(2) = DSQRT(PK(1,2)**2 + PK(2,2)**2 + PK(3,2)**2)
         DPCN(2) = DSQRT(DPC(1,2)**2 + DPC(2,2)**2 + DPC(3,2)**2)
         E3 = DSQRT(AMASS2 + PNEW(2) * PNEW(2))
         BETA = PNEW(2)/E3
         SECXS(2) = DPC(1,2)/P2
         SECYS(2) = DPC(2,2)/P2
         SECZS(2) = DPC(3,2)/P2
         SECXS(3) = SECXS(2)
         SECYS(3) = SECYS(2)
         SECZS(3) = SECZS(2)
      ELSE
         PNEW(2) = P2
         BETA = P2/E2
         SECXS(2) = 0.D0
         SECYS(2) = 0.D0
         SECZS(2) = 0.D0
         SECXS(3) = 0.D0
         SECYS(3) = 0.D0
         SECZS(3) = 0.D0
      END IF
C.  
      AT     = A + SECXS(1)
      BT     = B + SECYS(1)
      CT     = C + SECZS(1)
C.
      SECXS(2) = SECXS(2)+(BT * F(3) - CT * F(2)) * PH2
      SECYS(2) = SECYS(2)+(CT * F(1) - AT * F(3)) * PH2
      SECZS(2) = SECZS(2)+(AT * F(2) - BT * F(1)) * PH2
      AT     = A + SECXS(2)
      BT     = B + SECYS(2)
      CT     = C + SECZS(2)
      SECXS(3) = SECXS(3) + (BT * F(3) - CT * F(2)) * PH2
      SECYS(3) = SECYS(3) + (CT * F(1) - AT * F(3)) * PH2
      SECZS(3) = SECZS(3) + (AT * F(2) - BT * F(1)) * PH2
      DXT    = H * (A + SECXS(3))
      DYT    = H * (B + SECYS(3))
      DZT    = H * (C + SECZS(3))
      XT     = X + DXT
      YT     = Y + DYT
      ZT     = Z + DZT
      AT     = A + TWO*SECXS(3)
      BT     = B + TWO*SECYS(3)
      CT     = C + TWO*SECZS(3)
C.
      EST = ABS(DXT)+ABS(DYT)+ABS(DZT)
      IF (EST.GT.2.*ABS(H)) GO TO 30
C.
      DIST = DSQRT(DXT*DXT + DYT*DYT + DZT*DZT)
      TH4 = TOFG + DIST/(BETA*CLIGHT)
C.
      CALL GUEFLD(XYZT, TH4, F, E)
      ENORM = SQRT(E(1)**2 + E(2)**2 + E(3)**2)
C.
      IF (ENORM .GT. 0.D0) THEN 
         DO K = 1,3
            DPC(K,3) = CHARGE * E(K) * H2 * E1/P1
            PK(K,3) = VOUT(3+K)*P1 + 2.d0 * DPC(K,3)
         ENDDO
         PNEW(3) = DSQRT(PK(1,3)**2 + PK(2,3)**2 + PK(3,3)**2)
         DPCN(3) = DSQRT(DPC(1,3)**2 + DPC(2,3)**2 + DPC(3,3)**2)
         SECXS(4) = DPC(1,3)/P1
         SECYS(4) = DPC(2,3)/P1
         SECZS(4) = DPC(3,3)/P1
      ELSE
         PNEW(3) = P1
         SECXS(4) = 0.D0
         SECYS(4) = 0.D0
         SECZS(4) = 0.D0
      END IF  
C.
      Z      = Z + (C + (SECZS(1) + SECZS(2) + SECZS(3)) * THIRD) * H
      Y      = Y + (B + (SECYS(1) + SECYS(2) + SECYS(3)) * THIRD) * H
      X      = X + (A + (SECXS(1) + SECXS(2) + SECXS(3)) * THIRD) * H
C.
      SECXS(4) = SECXS(4) + (BT*F(3) - CT*F(2))* PH2
      SECYS(4) = SECYS(4) + (CT*F(1) - AT*F(3))* PH2
      SECZS(4) = SECZS(4) + (AT*F(2) - BT*F(1))* PH2
      A  = A+(SECXS(1)+SECXS(4)+TWO * (SECXS(2)+SECXS(3))) * THIRD
      B  = B+(SECYS(1)+SECYS(4)+TWO * (SECYS(2)+SECYS(3))) * THIRD
      C  = C+(SECZS(1)+SECZS(4)+TWO * (SECZS(2)+SECZS(3))) * THIRD
C.
      EST    = ABS(SECXS(1)+SECXS(4) - (SECXS(2)+SECXS(3)))
     &+        ABS(SECYS(1)+SECYS(4) - (SECYS(2)+SECYS(3)))
     &+        ABS(SECZS(1)+SECZS(4) - (SECZS(2)+SECZS(3)))
C.
C.      Check also if the momentum kick did not changed too much. 
C.      This type of accuracy might need to be adjusted depending on the
C.      problem. Choose much lower tolerance if electric field involved.
C.
      IF (ENORM .GT. 1.D-6) THEN 
         IF (EST.GT. 1.D-3 .AND. ABS(H).GT.1.D-4) GO TO 30 
         IF ((DPCN(3) - (DPCN(1) + DPCN(2)))/DPCN(3) .GT. 1.D-4) GOTO 30
      ELSE
         IF (EST.GT. DLT   .AND. ABS(H).GT.1.D-4) GO TO 30
      ENDIF
C.    
      ITER = ITER + 1
      NCUT = 0
*               If too many iterations, go to HELIX
      IF (ITER.GT.MAXIT) GO TO 40
*
      TL = TL + H
      IF (EST.LT.(DLT32)) THEN
         H = H*TWO
      ENDIF
      CBA    = ONE/ DSQRT(A*A + B*B + C*C)
      VOUT(1) = X
      VOUT(2) = Y
      VOUT(3) = Z
      VOUT(4) = CBA*A
      VOUT(5) = CBA*B
      VOUT(6) = CBA*C
C.
      VOUT(7) = (PNEW(2) + PNEW(3))/2.D0
C.
      REST = STEP - TL
      IF (STEP.LT.0.) REST = -REST
      IF (REST .GT. 1.D-5*ABS(STEP)) GO TO 20
C.
      GO TO 999
C.
C.              CUT STEP
   30 NCUT = NCUT + 1
C.               If too many cuts , go to HELIX
      IF (NCUT.GT.MAXCUT)       GO TO 40
      H = H*HALF
      GO TO 20
C.
C.              ANGLE TOO BIG, USE HELIX
C.
C.	Not supported if there is an electric field, the algorithm 
C.	will fail... 
C.
   40 CONTINUE
C.
      IF (ENORM .GT. 0.D0) then 
	   print *, ' Acceleration failure in grkuta '
      ENDIF
C.
      F1  = F(1)
      F2  = F(2)
      F3  = F(3)
      F4  = DSQRT(F1**2+F2**2+F3**2)
      RHO = -F4*PINV
      TET = RHO * STEP
C.
      IF(TET.NE.0.) THEN
C.
         HNORM = ONE/F4
         F1 = F1*HNORM
         F2 = F2*HNORM
         F3 = F3*HNORM
C.
         HXP(1) = F2*VECT(IPZ) - F3*VECT(IPY)
         HXP(2) = F3*VECT(IPX) - F1*VECT(IPZ)
         HXP(3) = F1*VECT(IPY) - F2*VECT(IPX)
C.
         HP = F1*VECT(IPX) + F2*VECT(IPY) + F3*VECT(IPZ)
C.
         RHO1 = ONE/RHO
         SINT = SIN(TET)
         COST = TWO*SIN(HALF*TET)**2
C.
         G1 = SINT*RHO1
         G2 = COST*RHO1
         G3 = (TET-SINT) * HP*RHO1
         G4 = -COST
         G5 = SINT
         G6 = COST * HP
 
         VOUT(IX) = VECT(IX) + (G1*VECT(IPX) + G2*HXP(1) + G3*F1)
         VOUT(IY) = VECT(IY) + (G1*VECT(IPY) + G2*HXP(2) + G3*F2)
         VOUT(IZ) = VECT(IZ) + (G1*VECT(IPZ) + G2*HXP(3) + G3*F3)
C.
         VOUT(IPX) = VECT(IPX) + (G4*VECT(IPX) + G5*HXP(1) + G6*F1)
         VOUT(IPY) = VECT(IPY) + (G4*VECT(IPY) + G5*HXP(2) + G6*F2)
         VOUT(IPZ) = VECT(IPZ) + (G4*VECT(IPZ) + G5*HXP(3) + G6*F3)
C.
      ELSE
C.
         VOUT(IX) = VECT(IX) + STEP*VECT(IPX)
         VOUT(IY) = VECT(IY) + STEP*VECT(IPY)
         VOUT(IZ) = VECT(IZ) + STEP*VECT(IPZ)
C.
      ENDIF
C.
  999 END
