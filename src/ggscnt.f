C.
      SUBROUTINE ggscnt
C.
C.    ******************************************************************
C.    *                                                                *
C.    *   This routine is called for each tracking step of a charged   *
C.    *   particle in a scintillator. A Poisson-distributed number of  *
C.    *   photons is generated according to the energy lost during     *
C.    *   step and the properties of the scintillator material. These  *
C.    *   photons are then distributed evenly along the track segment  *
C.    *   and emitted uniformly into 4pi. The parameters are then      *
C.    *   then transformed into the Master Reference System, and they  *
C.    *   are added to the particle stack.                             *
C.    *                                                                *
C.    *   ==>Called by : GUSTEP                                        *
C.    *      Author      P.Gumplinger                                  *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      INTEGER j, kmin, kmax, kmed, nphoton
C.
      REAL ds, dgphot, pphot, tau, dt
      REAL phi, cost, sint, cosp, sinp
C.
      REAL effic, ratio, rati1
C.
      REAL rndm(5), d(3), p(3), u(3), v(3)
C.
      DATA tau / 46.6E-9 /		!decay constant of LSO (46.6ns)
C.
      INTEGER init, nbins
C.
      PARAMETER (nbins = 11)
C.
      REAL y(nbins), xlo, xwi, xran, confac
C.
      DATA init / 0 /, xlo / 375. /, xwi /  25. /, confac / 1243.125 /
C.
      DATA y /   0.0, 40.0, 85.0, 78.0, 48.0, 
     &          36.0, 24.0, 18.0, 12.0,  8.0,  4.0 /
C.
      include 'gcbank.inc'          !geant
      include 'gcjloc.inc'          !geant
      include 'gctmed.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gcking.inc'          !geant
      include 'gconst.inc'          !geant
C.
      include 'uenergy.inc'		!local
C.
C *** See whether we generate at least one photon
C.
      If(init.eq.0)then
        init = 1
        CALL hispre(y,nbins)
      Endif
C.
      pmin = q(jtckov+2)
      dp   = q(jtckov+npckov+1) - pmin
C.
C *** Sample the number of photons
C.
      dgphot = photon_yield*destep*1.E3
      If(iscnt.eq.2)dgphot = dgphot/100.
C.      dgphot = 1.
C.
C **********************************************************************
C.
      CALL granor(rndm(1),rndm(2))
C.
      nphoton = dgphot + resolution_scale * sqrt(dgphot) * rndm(1)
      CALL hfill( 70,1.*nphoton+0.5,0.0,1.0)
C.
C.      CALL gpoiss(dgphot,nphoton,1)
C.
C **********************************************************************
C.
      If(nphoton.le.0)goto 999
C.
      ngphot = 0
C.
C *** Distribute the photons in momentum, origin, direction
C.
      Do 100 j = 1, nphoton
C.
         CALL grndm(rndm,2)
C.
C ***    Sample the momentum of the photon
C.
         CALL hisran(y,nbins,xlo,xwi,xran)
C.
         pphot = (confac/xran)*1.E-9
C.
C.         pphot = pmin + rndm(1)*dp
C.
C ***      Find in which bin we are
C.
           kmin = 1
           kmax = npckov
   10      kmed = (kmin+kmax)/2
           If(q(jtckov+1+kmed).lt.pphot)then
             kmin = kmed
           Else
             kmax = kmed
           Endif
           If(kmax-kmin.gt.1)goto 10
           ratio = (pphot-q(jtckov+1+kmin))/
     &             (q(jtckov+kmin+2)-q(jtckov+1+kmin))
           rati1  = (1.-ratio)
C.
         effic = q(jeffic+kmin)*rati1+q(jeffic+kmin+1)*ratio
C.
         If(rndm(2).gt.effic)goto 100
C.
         ngphot = ngphot + 1
C.
         CALL grndm(rndm,5)
C.
         xphot( 7,ngphot) = pphot
C.
         If(charge.eq.0.0)then
           ds = 0.0
         Else
           If(ignext.ne.0)then
             ds = (step-prec)*rndm(1) + prec
           Else
             ds = step*rndm(1)
           Endif
         Endif
C.
         xphot( 1,ngphot) = vect(1)-vect(4)*ds
         xphot( 2,ngphot) = vect(2)-vect(5)*ds
         xphot( 3,ngphot) = vect(3)-vect(6)*ds
C.
C ***    Sample the photon direction (uniform)
C.
         cost = 1. - 2.*rndm(2)
         sint = sqrt((1.-cost)*(1.+cost))
         phi  = twopi*rndm(3)
         sinp = sin(phi)
         cosp = cos(phi)
C.
         d(1) = sint*cosp
         d(2) = sint*sinp
         d(3) = cost
C.
         CALL ucopy(d(1),xphot(4,ngphot),3)
C.
C ***    Calculate the polarization from the direction
C.
         u(1) = cost*cosp
         u(2) = cost*sinp
         u(3) = -sint
C.
         CALL cross(u,d,v)
C.
         phi  = twopi*rndm(4)
         sinp = sin(phi)
         cosp = cos(phi)
C.
         CALL vline(u,cosp,v,sinp,p,3)
         CALL vunit(p(1),p(1),3)
C.
         CALL ucopy(p(1),xphot(8,ngphot),3)
C.
C ***    Sample the photon time off-set
C.
         dt = -tau*log(rndm(5))
C.
         If(charge.eq.0.0.or.istop.gt.0)then
           xphot(11,ngphot) = tofg + dt
         Else
           xphot(11,ngphot) = tofg + (step-ds)*getot/(vect(7)*clight) 
     &                             + dt
         Endif
C.
         If(ngphot.gt.mxphot)then
           write(chmail,10000)ngphot-mxphot
10000      format(' **** GGSCNT Overflow in the photon stack, ',I10,
     &            'photons are lost')
           CALL gmail(0,0)
           ngphot = mxphot
           goto 999
         Endif
C.
  100 Continue
C.
      CALL hfill( 71,1.*ngphot+0.5,0.0,1.0)
C.
  999 RETURN
      END
