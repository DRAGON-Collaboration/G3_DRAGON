C.
      SUBROUTINE gukine_gbox
C.
************************************************************************
*                                                                      *
*        (generation of isotropic photon beam along line source)       *
*        -------------------------------------------------------       *
*                                                                      *
*  KINE card: MKINE    : number of photons at initial vertex           *
*             GKINE 1  : x of photon origin distribution [cm]          *
*                   2  : y of photon origin distribution [cm]          *
*                   3  : z of photon origin distribution [cm]          *
*                   4  : half length of photon origin x-dimension [cm] *
*                   5  : half length of photon origin y-dimension [cm] *
*                   6  : half length of photon origin z-dimension [cm] *
*             GKINE 7  : photon energy [MeV]                           *
*                   8  : theta [degree]                                *
*                   9  : phi [degree]                                  *
*                  10  : emittance angle [degree]                      *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gcunit.inc'          !geant
      include 'gconst.inc'          !geant
C.
      include 'uevent.inc'          !local
C.
      INTEGER ip, nvert, nt, ngamma
C.
      REAL pbeam, vertex(3), plab(3)
      REAL theta, ctheta, stheta, phi, cphi, sphi
C.
      REAL dir(3), costh, sinth, cosph, sinph
      LOGICAL rotate
C.
      REAL rndm(3)
C.
      CALL flush(lout)			! flush output after GTRIGI
C.
      ip = 1				! Original particle is a photon
C.
C. --> First deal with the photon origin
C.
      CALL ucopy(gkine(1),vertex(1),3)
C.
      CALL grndm(rndm,3)
C.
      vertex(1) = vertex(1) + gkine(4) * (2.*rndm(1)-1.)
      vertex(2) = vertex(2) + gkine(5) * (2.*rndm(2)-1.)
      vertex(3) = vertex(3) + gkine(6) * (2.*rndm(3)-1.)
C.
      CALL gsvert(vertex,0,0,0,0,nvert)
C.
      ngamma = 0
C.
  100 Continue
C.
      If(gkine(8).eq.0.0.and.gkine(9).eq.0.0.and.gkine(10).eq.0.0)then
C.
        CALL grndm(rndm,2)
C.
        ctheta = 2. * rndm(1) - 1.
        stheta = sqrt((1. - ctheta) * (1. + ctheta))
C.
        phi  = twopi * rndm(2)
        cphi = cos(phi)
        sphi = sin(phi)
C.
        plab(1) =  stheta * cphi
        plab(2) =  stheta * sphi
        plab(3) =  ctheta
C.
      Else
C.
C. -->  Establish cone direction
C.
        If(gkine(8).ne.0.0)then
C.
          theta = gkine(8)
          phi   = gkine(9)
C.
          dir(1) = sin(degrad*theta) * cos(degrad*phi)
          dir(2) = sin(degrad*theta) * sin(degrad*phi)
          dir(3) = cos(degrad*theta)
C.
        Else
C.
          dir(1) =  0.0
          dir(2) =  0.0
          dir(3) =  1.0
C.
        Endif
C.
C. -->  Establish direction in a cone
C.
        If(gkine(10).ne.0.0)then
C.
          CALL grndm(rndm,2)
C.
          ctheta = 1. - rndm(1) * (1. - cos(degrad*gkine(10)/2.))
          stheta = sqrt((1. - ctheta) * (1. + ctheta))
C.
          phi  = twopi * rndm(2)
          cphi = cos(phi)
          sphi = sin(phi)
C.
          plab(1) =  stheta * cphi
          plab(2) =  stheta * sphi
          plab(3) =  ctheta
C.
        Else
C.
          plab(1) = 0.0
          plab(2) = 0.0
          Plab(3) = 1.0
C.
        Endif
C.
C. -->  Rotate cone into direction
C.
        CALL gfang(dir(1),costh,sinth,cosph,sinph,rotate)
        If(rotate)CALL gdrot(plab(1),costh,sinth,cosph,sinph)
C.
      Endif
C.
      ngamma = ngamma + 1
C.
      pbeam = gkine(7)                          ! Energy in MeV
      If(ngamma.le.10)then
        If(egamma(ngamma).gt.0.0)pbeam = egamma(ngamma)
      Endif
      pbeam = pbeam / 1.E3              	! GEANT wants GeV/c
C.
      CALL vscale(plab,pbeam,plab,3)
C.
      CALL gskine(plab,ip,nvert,0,0,nt)
C.
      If(mkine.gt.ngamma)goto 100
C.
      RETURN
      END
C.









