      SUBROUTINE GUSWIM(CHRG,STP,VCT,VT) 
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to control tracking of one track            *
C.    *       in a magnetic field                                      *
C.    *                                                                *
C.    *    ==>Called by : GTELEC,GTHADR,GTMUON                         *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      include 'gctmed.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gctrak.inc'          !geant
C.
      REAL CHRG, STP
      REAL VCT(7),VT(7)
C.
C.    ------------------------------------------------------------------
C.
         IF (IFIELD.EQ.3)THEN
            CALL GHELX3(FIELDM*CHRG,STP,VCT,VT)
         ELSEIF(IFIELD.EQ.2)THEN
            CALL GHELIX(CHRG, STP, VCT, VT)
         ELSE
            CALL GRKUTA(CHRG, AMASS, STP, TOFG, VCT, VT)
         ENDIF
C
      END
