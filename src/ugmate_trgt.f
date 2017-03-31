      SUBROUTINE ugmate_trgt
      IMPLICIT NONE
      include 'uggeom.inc'
      include 'rescom.inc'
      INTEGER nlmat
      REAL a, z, dens, radl, pdens, ptarg
      REAL amat(6), zmat(6), wmat(6)
      REAL centralvac_factor
C
C     Stainless Steel (Krupp n.-301) 26
      mssteel = 26
C
      nlmat = 6
      dens = 7.705
      zmat(1) = 26.
      zmat(2) = 24.
      zmat(3) = 28.
      zmat(4) = 25.
      zmat(5) = 14.
      zmat(6) =  6.
      amat(1) = 55.85
      amat(2) = 52.
      amat(3) = 58.69
      amat(4) = 54.94
      amat(5) = 28.09
      amat(6) = 12.01
      wmat(1) =  0.7085
      wmat(2) =  0.18
      wmat(3) =  0.08
      wmat(4) =  0.02
      wmat(5) =  0.01
      wmat(6) =  0.0015
      CALL gsmixt(mssteel,'STAINLESS STEEL',amat,zmat,dens,nlmat,wmat)

C     Target materials (1 atm 20 deg C)
      a = atarg
      if(atarg.lt.1.2)then
C     Hydrogen
         z = 1
         dens = 8.38E-5
         radl = 752300
      else
C     Helium
         z = 2
         a = 4.0026
         dens = 1.6586E-4
         radl = 568686
      endif
C
C     Target pressure
      ptarg = 5.0/760.
      targetl = 5.5
      exitdens = targetl
      entdens = targetl
C
C     Solid target, still want to set mtarg
      if(atarg.eq.12.)then
         mtarg = 62
      else
         mtarg = 27
         CALL gsmate(mtarg,'target',a,z,ptarg*dens,radl/ptarg,0,0,0)
      endif
C
C     Central vacuum material (1/500th of central pressure)
      mcent = 28
      centralvac_factor = 0.002
      pdens = ptarg*centralvac_factor
      CALL gsmate(mcent,'centralvacuum',a,z,pdens*dens,radl/pdens,0,0,0)
C
C     gas pressures for inside stepped collimator sections and eff.length
      ment(1) = 29
      pdens = ptarg*0.05 !! ratio from Knudsen equation.
      entdens = entdens + pdens*5.08/ptarg !! (5.08cm section length)
      CALL gsmate(ment(1),'ent1',a,z,pdens*dens,radl/pdens,0,0,0)
C
      ment(2) = 30
      pdens = ptarg*0.036 !! ratio from Knudsen equation.
      entdens = entdens + pdens*5.08/ptarg
      CALL gsmate(ment(2),'ent2',a,z,pdens*dens,radl/pdens,0,0,0)
C
      ment(3) = 31
      pdens = ptarg*0.016 !! ratio from Knudsen equation.
      entdens = entdens + pdens*5.08/ptarg
      CALL gsmate(ment(3),'ent3',a,z,pdens*dens,radl/pdens,0,0,0)
C
      mex(1) = 32
      pdens = ptarg*0.05 !! ratio from Knudsen equation.
      exitdens = exitdens + pdens*5.08/ptarg !! (5.08cm section length)
      CALL gsmate(mex(1),'ext1',a,z,pdens*dens,radl/pdens,0,0,0)
C
      mex(2) = 33
      pdens = ptarg*0.036 !! ratio from Knudsen equation.
      exitdens = exitdens + pdens*5.08/ptarg
      CALL gsmate(mex(2),'ext2',a,z,pdens*dens,radl/pdens,0,0,0)
C
      mex(3) = 34
      pdens = ptarg*0.016 !! ratio from Knudsen equation.
      exitdens = exitdens + pdens*5.08/ptarg
      CALL gsmate(mex(3),'ext3',a,z,pdens*dens,radl/pdens,0,0,0)
C
C     Box gas baseline pressure
      mbox = 35
      pdens = ptarg*0.056 !! DAH Gas target profile report June 2002
      entdens = entdens + pdens*2.2/ptarg
      exitdens = exitdens + pdens*2.2/ptarg
      CALL gsmate(mbox,'base',a,z,pdens*dens,radl,pdens,0,0,0)
C
      RETURN
      END
