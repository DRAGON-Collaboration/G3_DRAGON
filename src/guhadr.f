C.
      SUBROUTINE guhadr
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to generate one hadronic interaction        *
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
        CALL GHEISH
      ELSE IF (IHADR.EQ.4) THEN
        CALL FLUFIN
      ELSE IF (IHADR.EQ.5) THEN
        CALL GFMFIN
      END IF
C.
      RETURN
      END
C.
