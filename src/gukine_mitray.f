C.
      SUBROUTINE gukine_mitray
C.
************************************************************************
*                                                                      *
*  For iswit(4).eq.0 and iswit(5).eq.0                                 *
*  -----------------------------------                                 *
*  KINE card:  IKINE    : GEANT particle type                          *
*              PKINE 1  : particle origin x position (cm)              *
*                    2  : particle origin y position (cm)              *
*                    3  : particle origin z position (cm)              *
*                    4  : particle central momentum value (MeV/c)      *
*                    5  : RAYTRACE theta [mr]                          *
*                    6  : RAYTRACE phi   [mr]                          *
*                                                                      *
*  For iswit(4).eq.0 and iswit(5).eq.1                                 *
*  -----------------------------------                                 *
*  KINE card:  IKINE    : GEANT particle type                          *
*              PKINE 1  : particle origin x position (cm)              *
*                    2  : particle origin y position (cm)              *
*                    3  : particle origin z position (cm)              *
*                    4  : particle central momentum value (MeV/c)      *
*                    5  : width of x origin [1cm]                      *
*                    6  : width of y origin [1cm]                      *
*                    7  : horizontal emittance [100mr]                 *
*                    8  : vertical   emittance [100mr]                 *
*                    9  : 1 +- deltaP/P                                *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gcvolu.inc'          !geant
C.
      include 'geom_sole.inc'       !local
C.
      include 'diagnostic.inc'      !local
C.
      include 'ukine.inc'           !local
C.
      INTEGER ip, nvert, nt
C.
      REAL vertex(3), vrtx(3), plab(3), pbeam, scale
      REAL x_max, y_max, rndm(2)
C.
      INTEGER i, nout / 0 /, isotok / 0 /
      REAL bmass, tmp(4)
      CHARACTER*2 label(3)
C.
      CHARACTER*40 CARDNAME
      CHARACTER*7  RAYFILE
      LOGICAL fexist
C.
      REAL theta, phi
C.
      INTEGER mlevel, lnam(15), lnum(15), ier
C.
      REAL xd(3), xdd(3)
C.
      If(iswit(4).eq.0.and.iswit(5).eq.0)then
C.
        ip = ikine
C.
        CALL ucopy(pkine(1),vertex(1),3)
C.
        pbeam = pkine(4) / 1.E3              ! GEANT wants GeV/c
C.
        plab(1) = sin(pkine(5)*1.E-3)*cos(pkine(6)*1.E-3)
        plab(2) =                     sin(pkine(6)*1.E-3)
        plab(3) = cos(pkine(5)*1.e-3)*cos(pkine(6)*1.E-3)
C.
        If(irevs.eq.1)plab(3) = -plab(3)
C.
      Elseif(iswit(4).ge.1.and.iswit(5).eq.0)then
C.
        If(nout.eq.0)then
          RAYFILE = 'RAYFILE'
          CALL getenv(RAYFILE,CARDNAME)
          INQUIRE (file=CARDNAME, exist=fexist)
          If(fexist)then
            OPEN(UNIT=lunits(5), FILE=CARDNAME, FORM='FORMATTED',
     &           ACCESS = 'SEQUENTIAL', STATUS = 'OLD', err=999)
          Else
            goto 999
          Endif
        Endif
C.
  100   Continue
C.
        isotok = isotok + 1
C.
        READ(lunits(5),111,end=998)
     &                     pkine(1),pkine(5),pkine(2),pkine(6),pkine(4),
     &                     bmass,tmp(1),tmp(2),pkine(3),tmp(3),tmp(4),
     &                     nout,(label(i),i=1,3)
C.
  111   FORMAT (1X, F10.6,F10.4,F10.6,F10.4,F12.8,
     &          F11.6,2E11.3,F9.4,E12.4,F9.4 , I8, 1X, 3A2)
C.
        If(iswit(4).gt.1.and.(isotok.lt.iswit(4)))goto 100
C.
        pkine(3) = 1.E2 * pkine(3)
C.
        ip = 61
C.
        CALL ucopy(pkine(1),vertex(1),3)
C.
        pkine(4) = 1.E3 * pkine(4)        ! read in GeV/c -> MeV/c (as above)
C.
        pbeam = pkine(4) / 1.E3           ! GEANT wants GEV/c

        plab(1) = sin(pkine(5)*1.E-3)*cos(pkine(6)*1.E-3)
        plab(2) =                     sin(pkine(6)*1.E-3)
        plab(3) = cos(pkine(5)*1.e-3)*cos(pkine(6)*1.E-3)
C.
        If(irevs.eq.1)then
          CALL vcopyn(plab,plab,3)
        Endif
C.
      Elseif(iswit(4).eq.0.and.iswit(5).eq.1)then
C.
        ip = ikine
C.
        CALL ucopy(pkine(1),vertex(1),3)
C.
        CALL grndm(rndm,2)
        vertex(1) = vertex(1) + (2.0*rndm(1)-1.0) * pkine(5)/2.
        vertex(2) = vertex(2) + (2.0*rndm(2)-1.0) * pkine(6)/2.
C.
        CALL grndm(rndm,1)
        pbeam = (1.0+(pkine(9)-1.0)*(2.0*rndm(1)-1.0)) * pkine(4)
        pbeam = pbeam / 1.E3              ! GEANT wants GeV/c
C.
C *** Pick a uniform direction for the particle into a rectangular aperture
C.
        x_max = tan(pkine(7)*1.E-3/2.)
        y_max = tan(pkine(8)*1.E-3/2.)
C.
        CALL grndm(rndm,2)
C.
        plab(1) = 2.*x_max*rndm(1) - x_max
        plab(2) = 2.*y_max*rndm(2) - y_max
        plab(3) =          1.0
C.
        If(irevs.eq.1)plab(3) = -plab(3)
C.
      Endif
C.
      CALL vunit(plab(1),plab(1),3)
C.
      CALL uctoh('WRLD',lnam(1),4,4)
      CALL uctoh('STRV',lnam(2),4,4)
C.
      lnum(1) = 1
      lnum(2) = 1
C.
      nlevel = 0
      mlevel = 2
C.
      CALL glvolu(mlevel,lnam,lnum,ier)
C.
      If(ier.ne.0)then
        ieorun = 1
        write(lout,*)'Fatal error in GLVOLU'
        STOP
      Endif
C.
      CALL ucopy(vertex(1), xd(1),3)
      CALL ucopy(plab(1)  ,xdd(1),3)
C.
      CALL gdtom(xd,vertex,1)
      CALL gdtom(xdd, plab,2)
C.
      CALL vscale(plab,pbeam,plab,3)
C.
      CALL gsvert(vertex,0,0,0,0,nvert)
      CALL gskine(plab,ip,nvert,0,0,nt)
C.
      If(iswit(4).ge.1)then
        scale = -vertex(3)/plab(3)
        CALL vline(vertex,1.,plab,scale,vrtx,3)
      Else
        CALL ucopy(vertex(1),vrtx(1),3)
      Endif      
C.
      CALL hfill(1,vrtx(1),0.0,1.0)
      CALL hfill(2,vrtx(2),0.0,1.0)
C.
      CALL hfill(101,vrtx(1),vrtx(2),1.0)
C.
      CALL vunit(plab(1),plab(1),3)
C.
      theta = 0.0
      If(plab(1).ne.0.0.or.plab(3).ne.0.0)then
         theta = 1000.*atan2(plab(1),plab(3))
      Endif
      phi = 1000.*asin(plab(2))
C.
      CALL hfill(3,theta,0.0,1.0)
      CALL hfill(4,phi  ,0.0,1.0)
C.
      CALL hfill(102,theta,phi,1.0)
      CALL hfill(105,vrtx(1),theta,1.0)
      CALL hfill(106,vrtx(2),phi  ,1.0)
C.
      pbeam = 100.*(pbeam/0.25855443-1.0)
C.
      CALL hfill( 9,pbeam  ,0.0,1.0)
C.
      RETURN
C.
  998 Continue
C.
      CLOSE(lunits(5))
C.
      ievent = ievent - 1
      idevt  = idevt  - 1
      ieorun = 1
      ieotri = 1
C.
      RETURN
C.
  999 write(lout,*)' No RAYFILE file opened for simulation! '
C.
      STOP
      END
C.
