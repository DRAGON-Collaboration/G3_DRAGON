
      SUBROUTINE gukine_full_up
C.
C======================================================================C
C      Initial beam and reaction information passed via card           C
C      FKIN - LKINE FKINE(10)                                          C
C                                                                      C
C      LKINE reaction number                                           C
C                                                                      C
C      ( 1) 13N(p,g)14O                                                C
C      ( 2) 15O(a,g)19Ne                                               C
C      ( 3) 25Al(p,g)26Si                                              C
C      ( 4) 17F(p,g)18Ne                                               C
C      ( 5) 18F(p,g)19Ne                                               C
C      ( 6) 19Ne(p,g)20Na                                              C
C      ( 7) 20Na(p,g)21Mg                                              C
C      ( 8) 21Na(p,g)22Mg                                              C
C      ( 9) 23Mg(p,g)24Al                                              C
C      (10) 26mAl(p,g)27                                               C
C      (11) 7Be(p,g)8B                                                 C
C                                                                      C
C======================================================================C
C.
      IMPLICIT none
C.
      include 'gcbank.inc'
      include 'gcflag.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gctrak.inc'          !geant  - trick! TOFG -> VERTX bank
C.
      include 'uggeom.inc'          

      include 'rescom.inc'
          
      include 'beamcom.inc'
      include 'uevent.inc'
      include 'history.inc'
      include 'gammahit.inc'

      include 'cntrs.inc'
C.
      INTEGER init, ip, nvtx, nt, JPA, i,j
C.
      REAL vertex(3), plab(3), plabu, beammom
C.
      REAL rndm(2), phi, thet, mag, rx, ry, r, inittheta
C.
      REAL beame, beamv
      REAL beamx, beamy, beamz, beama, beamb, beamt, tdrift
      REAL cosx, cosy, cosz
C.

C.
C==
C     Emittances from Laxtal area divided by 4 pi
C     delx, dely are 2 sigma values for spot size
C===
      REAL betagamma
C===
C     Pencil beam
C===
C      REAL ex0/ 0. /, ey0/ 0. /, el0/ 0. /
C      REAL betagamma0 / 0.01851 /
C      REAL delx/ 0. /, dely/ 0. /
C
      INTEGER imate, partid, ixst
       REAL  dedx, pcut(5), hrndm1, newm, xx, yy
C.
C.    Initialize all ntuple variables
       print*, 'gukine_full.f'
      E_int = 0.
      E_rec = 0.
      cost_r = 99.
      cosp_r = 99.
      react = 0
      Nodec = 0
      recdet = 0
      DO i = 1, 15
       E_g(i) = 0.
       E_gp(i) = 0.
       cost_g(i) = 99.
       cost_gp(i) = 99.
       phi_g(i) = 99.
      ENDDO
      x_r = 0.
      y_r = 0.
      z_r = 0.
      thet_r = 0.
      xstop = 0.
      ystop = 0.
      zstop = 0.
      xint = 0.
      yint = 0.
      zint = 0.
      x = 99.
      y = 99.
      xp = 99.
      yp = 99.
      DO i = 1, 10
         xtest(i) = 99.
         ytest(i) = 99.
         etest(i) = 0.
      ENDDO
      dsssdpos = 0.
      recoil_hit_ENDV = 0
      num_bgos_hit = 0
      e_bgos_total = 0
      num_bgo_first = 0
      e_bgo_first = 0
      num_bgo_second = 0
      e_bgo_second = 0
      num_bgos_hit_ab = 0
      num_bgo_first_ab = 0
      e_bgo_first_ab = 0
      num_bgo_second_ab = 0
      e_bgo_second_ab = 0
      pair_productions = 0
      McpHit = .FALSE.
C.
C.--> if beam, reset its charge to default
C.
      If(ipart.eq.80)then
       JPA = LQ(JPART-IPART)
       Q(JPA+10) = FKINE(1)
      Endif


      if(alpha)then
         irecoil = 80
c         CALL granor(rndm(1))
c         beame = beamenerg*(1. + rndm(1)*0.01)
         beame = HRNDM1(523)/1000.
         beammom=sqrt(beame*(beame+2000.*beammass))*0.001
c         print*, beammom, alpha, beame, beammass
         ip = 80
         CALL granor(rndm(1),rndm(2))
         CALL NRAN(rndm,2)
         r = sqrt(rndm(1)*(0.04))
         inittheta = rndm(2)*2.*3.141592654
         vertex(1) = r*cos(inittheta) !rndm(1)*0.0851 ! FWHM of 2mm
         vertex(2) = r*sin(inittheta) !rndm(2)*0.0851
         vertex(1) = vertex(1) + 0.0
         vertex(2) = vertex(2) + 0.0
         vertex(3) = offset(3) + 0.0
         CALL gsvert(vertex,0,0,0,0,nvtx)
         CALL NRAN(rndm,2)
         phi = 3.141592654/2.0*rndm(1)+3.141592654/4.0 ! Just takes a 1/4 of a pie by up notch
         thet = acos(1.0-rndm(2)*0.0003125) !0.0003125 between 0 and 25 mrad
         
c      thet = 0.
         cost_r = cos(thet)
         cosp_r = cos(phi)
         ! print*, phi, thet
         !thet = 0
         x_r = sin(thet) * cos(phi)
         y_r = sin(thet) * sin(phi)
         z_r = cos(thet)
         cosx = sin(thet) * cos(phi)
         cosy = sin(thet) * sin(phi)
         cosz = cos(thet)
         mag = sqrt(cosx**2 + cosy**2 + cosz**2)
         cosx = cosx/mag
         cosy = cosy/mag
         cosz = cosz/mag
      
         plab(1) = beammom*cosx
         plab(2) = beammom*cosy
         plab(3) = beammom*cosz
         CALL gskine(plab,ip,nvtx,0,0,nt) 
      else
C.
C.--> fill upright long. ellipse at the beam waist
C.
       CALL granor(rndm(1),rndm(2))
C.
      beame = beamenerg + rndm(1)*emax
*      print*, beamenerg, emax, beame
      beamt = rndm(2)*buncht
C.
C.--> ERES in MeV
C.
       CALL granor(rndm(1),rndm(2))
C.
      IF (LKINE.NE.19) THEN
      eres = eres0*(resenerg + rndm(1)*reswidth)
      ENDIF
      IF(LKINE.GE.19) THEN
 888     xx = hrndm1(501)
      eres = eres0*xx
      erescm = xx
      IF (eres.gt.beame*0.001) THEN
       
       GOTO 888
      ENDIF
*      print*, 'Erec (CM)', erescm

c      yy = xx**2/(2.*(resmass-beammass-xx/1000.))
c      print*, eres, beame*0.001, xx, eres0
c      CALL hfill(502,xx,0.,1.0)


      
      
      ENDIF
C.
!      beamv = clight*sqrt(2.*beame/abeam/amumev)
!      betagamma = beamv/sqrt(clight**2+beamv*beamv)
      beammom=sqrt(beame*(beame+2000.*beammass))  !in Mev/c        
      beamv = clight*beammom*.001/beammass
      betagamma = beamv/clight/sqrt(1.0 - (beamv/clight)**2)  
C.
C.--> fill upright trans. ellipses at the beam waist
C.
      CALL granor(rndm(1),rndm(2))
C.
      beamx = rndm(1)*sigx
      beama = rndm(2)*amax
      x = beamx
      xp = beama
C.
      CALL granor(rndm(1),rndm(2))
C.
      beamy = rndm(1)*sigy
      beamb = rndm(2)*bmax
      y = beamy
      yp = beamb

      print*, 'amax & bmax: ', amax, bmax
C.
C.--> add positional offset from mistuned beam
C.
      
      beamx = beamx + offset(1)
      beamy = beamy + offset(2)
     
C.
C.--> distribute particles over a 2 sig range about beam waist at z=0.0
C.
      CALL granor(rndm(1),rndm(2))
C.
      beamz = bunchl*rndm(1)
C.
C.--> add beam direction axis components
C. 
      
      beama = beama + offset(3)
      beamb = beamb + offset(4)
c      beama = 0.
c      beamb = 0.
      
C.
C.--> calculate direction cosines
C.
      cosz = 1./sqrt(1+beama*beama+beamb*beamb)
      cosx = cosz*beama
      cosy = cosz*beamb
C.      print*, beama, beamb, cosz, cosx, cosy
C.
C.--> DEFINE PARTICLE MOMENTUM (GEV/c ) AND TYPE
C.
      plabu = beammom*0.001
C.
      plab(1) = plabu*cosx
      plab(2) = plabu*cosy
      plab(3) = plabu*cosz

C.
C.--> DEFINE PARTICLE ORIGIN (VERTEX)
C.
C.--> drift back half the target length to obtain entrance coords (x,y,z)
C.
      tdrift = (TLrms*.999)/beamv
C.
      vertex(1) = beamx - beamv*cosx*tdrift
      vertex(2) = beamy - beamv*cosy*tdrift
      vertex(3) = beamz - beamv*cosz*tdrift
      
C.
      ip = 80
C.
      tofg = beamt
C.
      CALL gsvert(vertex,0,0,0,0,nvtx)
      CALL gskine(plab,ip,nvtx,0,0,nt)
C.

          CALL hfill(1,vertex(1),0.0,1.0)
          CALL hfill(2,vertex(2),0.0,1.0)
          CALL hfill(3,cosx*1000.,0.0,1.0)
          CALL hfill(4,cosy*1000.,0.0,1.0)
          CALL hfill(9,plabu*1000.,0.0,1.0)
          

       endif
      RETURN
      END
C.







