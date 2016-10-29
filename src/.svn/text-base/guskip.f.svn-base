C.
      SUBROUTINE guskip(iskip)
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine is called from GSSTAK                                    C
C                                                                      C
C     Note: Decisions can only be based on the following variables     C
C           in (temporary) GCKINE: TOFG, IPART, VERT and PVERT         C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      INTEGER iskip
C.
      include 'gckine.inc'          !geant
C.
      iskip = 0
C.
      If(ipart.eq.4)iskip = 1
C.
      RETURN
      END

