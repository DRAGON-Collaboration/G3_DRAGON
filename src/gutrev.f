C.
      SUBROUTINE gutrev
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      GEANT3 user routine to control tracking of one event      *
C.    *                                                                *
C.    *   ==>Called by GRUN                                            *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      include 'uenergy.inc'             !local
C.
C.    ------------------------------------------------------------------
C.
C.    User event variables are initialized here
C.
      n_flag = 0
      e_detect = 0.0
C.
      CALL vzero(nloss,6)
C.
      CALL gtreve
C.
      RETURN
      END
C.
