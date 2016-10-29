C.
      SUBROUTINE ghipmt
C.
C *** Description: Give actions when the optical photon enters PMT
C.
      IMPLICIT none
C.
      include 'gctrak.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gcsets.inc'          !geant
C.
      include 'geometry.inc'            !local
C.
      INTEGER ihit
C.
      REAL hits(5)
C.
      If(idtype.eq.2)then
C.
        hits(1) = x_fngr(number(nlevel-1))
        hits(2) = y_fngr(number(nlevel-1))
        hits(3) = z_fngr(number(nlevel-1))
        hits(4) = tofg
        hits(5) = 1.0
C.
        CALL gsahit(iset,idet,itra,numbv,hits,ihit)
C.
        CALL hfill( 81,1.*nstep+0.5,0.0,1.0)
        CALL hfill( 82,sleng,0.0,1.0)
C.
        istop = 3
C.
      Endif
C.
      RETURN
      END
C.
