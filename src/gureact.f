C.
      SUBROUTINE gureact
C.
C======================================================================C
C                                                                      C
C                KINEMATICS OF CAPTURE GAMMA REACTION                  C
C                                                                      C
C======================================================================C
C.
      IMPLICIT none
C.
      include 'gcking.inc'          !geant
      include 'gctrak.inc'          !geant
     
C.
      INTEGER ipart, itrtyp, nwbuf
C.
      REAL amass, charge, tlife, ubuf
C.
      CHARACTER*20 napart
C.
      CHARACTER* 4 mec 
      DATA mec / 'RESR' /
C.
      CALL uctoh(mec,kcase,4,4)
C.
      ngkine = 1
C.
      gkin(1,ngkine) = vect(4)*vect(7)
      gkin(2,ngkine) = vect(5)*vect(7)
      gkin(3,ngkine) = vect(6)*vect(7)
C.
C.--> Produce resonant state as a new particle
C.
      ipart = 81
C.
      CALL gfpart(ipart,napart,itrtyp,amass,charge,tlife,ubuf,nwbuf)
C.
C.      print*, ipart,napart,itrtyp,amass,charge,tlife,ubuf
      gkin(4,ngkine) = sqrt(amass**2 + vect(7)*vect(7))
C.
      gkin(5,ngkine) = ipart
C.
      gpos(1,ngkine) = vect(1)
      gpos(2,ngkine) = vect(2)
      gpos(3,ngkine) = vect(3)
C.
      tofd(ngkine) = 0.0
C.
      RETURN
      END














