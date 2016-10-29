C.
      SUBROUTINE ghidet
C.
C *** Description: Give actions when the current particle is in detector
C.
      IMPLICIT none
C.
      include 'gcflag.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gcking.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gcsets.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gccuts.inc'          !geant
C.
      include 'uenergy.inc'		!local
C.
      INTEGER i, ihit
C.
      REAL hits(5), edep, tofin, tofout, x1(3), x2(3), xd(3)
C.
      If(idtype.le.0)goto 999
      If(inwvol.eq.1.and.istop .ne.0)goto 999
C.
C *** INWVOL = 1     store entrance quantities
C.
      If(inwvol.eq.1)then
        edep  = 0.0
        tofin = tofg
        CALL ucopy(vect(1),x1(1),3)
        CALL gmtod(x1,xd,1)
C this is to record depth instead of z        x1(3) = xd(3)
        goto 999
      Endif
C.
C *** INWVOL = 0    Accumulate EDEP
C.
      If(inwvol.eq.0)then
        If(istop.ne.0)goto 10
          If(iscnt.gt.0.and.destep.gt.0.0)CALL ggscnt
          edep = edep + destep
          goto 999
      Endif
C.
C *** INWVOL = 2   Close hit (also when ISTOP # 0)
C.
   10 CONTINUE
C.
      If(iscnt.gt.0.and.destep.gt.0.0)CALL ggscnt
C.
      edep = edep + destep
      tofout = tofg
      CALL ucopy(vect(1),x2(1),3)
      CALL gmtod(x2,xd,1)
C this is to record depth instead of z       x2(3) = xd(3)
C.
      Do i = 1, nmec
         If(lmec(i).eq.5)then
           If(itrtyp.eq.3.and.gekin.le.cutneu)goto 1
           If(itrtyp.eq.4.and.gekin.le.cuthad)goto 1
           If(itrtyp.eq.5.and.gekin.le.cutmuo)goto 1
           If(itrtyp.eq.8.and.gekin.le.cuthad)goto 1
           goto 2
         Endif
      Enddo 
      goto 2
    1 Continue
      tofout = tofout - tofd(ngkine+1)
    2 Continue
C.
      If(idtype.eq.1)then
        hits(1) = (x1(1)+x2(1))/2.
        hits(2) = (x1(2)+x2(2))/2.
        hits(3) = (x1(3)+x2(3))/2.
        hits(4) = (tofin + tofout)/2. * 1.E9
        hits(5) = 1000. * edep
C.
C.--> Note: Use the factor used in file ugeom.f; 
C.-->       subroutine UDET; variable fact_dedx
C.
        If(edep.gt.1.E-6)CALL gsahit(iset,idet,itra,numbv,hits,ihit)
C.
      Endif
C.
  999 Continue
C.
      RETURN
      END
