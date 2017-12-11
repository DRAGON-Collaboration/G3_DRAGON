C.
      SUBROUTINE guphad
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to compute Hadron. inter. probabilities     *
C.    *                                                                *
C.    *    ==>Called by : GTHADR,GTNEUT                                *
C.    *                                                                *
C.    ******************************************************************
C.
C.    ------------------------------------------------------------------
C.
      IMPLICIT none
C.
      include 'gcphys.inc'          !geant
C.
C.          GHEISHA hadronic shower code if IHADR < 3
C.          FLUKA   hadronic shower code if IHADR = 4
C.          FLUKA/MICAP had. shower code if IHADR = 5
C.
      IF (IHADR.LT.3) THEN
        CALL GPGHEI
      ELSE IF (IHADR.EQ.4) THEN
        CALL FLDIST
      ELSE IF (IHADR.EQ.5) THEN
        CALL GFMDIS
      END IF
C.
      END
C.
