C.
      SUBROUTINE gutrak
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to control tracking of one track            *
C.    *                                                                *
C.    *    ==>Called by : GTREVE                                       *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      include 'mitray_diag.inc'         !local
      include 'diagnostic.inc'          !local
C.
C.    ------------------------------------------------------------------
C.
      jstop  = 0
      jslit  = 0
C.
      CALL gtrack
C.
      RETURN
      END
C.
