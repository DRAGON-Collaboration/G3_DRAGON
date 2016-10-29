C     Small Pumping tubes
      SUBROUTINE ugeo_trgt_small
      IMPLICIT NONE
      include 'uggeom.inc'
      include 'geometry.inc'
      INTEGER nmed, npar, ivolu
      REAL z, par(10), zcol, zex2, lex11, shape(3), pos(3), d(3)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Al entrance tubes
      nmed = 6
      Rrms = 6.0
      npar   = 5
      par(1) = len2
      par(2) = rilen2
      par(3) =  Rrms
      par(4) = riren2
      par(5) = Rrms
C
      CALL gsvolu('EN2C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EN2C','SEEN',1)
C
      npar   = 5
      par(1) = len3
      par(2) = rilen3
      par(3) =  Rrms
      par(4) = riren3
      par(5) = Rrms
C
      CALL gsvolu('EN3C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EN3C','SEEN',1)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Exit apertures (taken from Dave O's Feb 06 drawings)

      npar = 5
      par(1) = 3.52/2.
      par(2) = 1.81/2.
      par(3) = Rrms
      par(4) = 1.81/2.
      par(5) = Rrms
      CALL gsvolu('EX2C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX2C','SEEN',1)
C
      par(1) = 5.5/2.
      par(2) = 1.8/2.
      par(3) = Rrms
      par(4) = 2.01/2.
      par(5) = Rrms
      CALL gsvolu('EX3C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX3C','SEEN',1)
C
      par(1) = 4.16/2.
      par(2) = 2.67/2.
      par(3) = Rrms
      par(4) = 2.67/2.
      par(5) = Rrms      
      CALL gsvolu('EX4C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX4C','SEEN',1)
C
      par(1) = 24.9/2.
      par(2) = 2.71/2.
      par(3) = Rrms
      par(4) = 3.67/2.
      par(5) = Rrms
      CALL gsvolu('EX5C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX5C','SEEN',1)
C
      par(1) = 22.9/2.
      par(2) = 5.45/2.
      par(3) = Rrms
      par(4) = 6.51/2.
      par(5) = Rrms
      CALL gsvolu('EX6C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX6C','SEEN',1)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define entrance gas volumes and place the collimators inside
      nmed = mcent
C
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len1
C     Position new gas volumes to match new collimator 
      par(3) = 0.5*0.68
      CALL gsvolu('EN1G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN1G','SEEN',1)
      zcol = -(0.5*box_length -wall(2) - col_collar_length +
     +         col_length + par(3) )
      CALL gspos('EN1G', 1,'DETE',0.0,0.0,zcol,0,'ONLY')
      z = zent(1)
C
      par(3) = 0.5 * (zent(1) - zent(3) - len1-len2)
C
      CALL gsvolu('EN2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN2G','SEEN',1)
C
      z = .5 * (zent(1) + zent(3)-len1 + len2)
C
      CALL gspos('EN2G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
C     Define entrance 3 + 4 gas volumes
C
C
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len2
C
      CALL gsvolu('EN3G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN3G','SEEN',1)
C
      CALL gspos('EN2C', 1, 'EN3G', 0.0, 0.0, 0.0, 0, 'ONLY')
      z = zent(3)
      CALL gspos('EN3G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(3) = 0.5 * (zent(3) - zent(5) - len3 -len2)
C
      CALL gsvolu('EN4G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN4G','SEEN',1)
C
      z = .5 * (zent(3) + zent(5) +len3 -len2)
C
      CALL gspos('EN4G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
C     Define entrance 5 + 6 gas volumes
C
C
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len3
C
      CALL gsvolu('EN5G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN5G','SEEN',1)
C
      CALL gspos('EN3C', 1, 'EN5G', 0.0, 0.0, 0.0, 0, 'ONLY')
C
      z = zent(5)
C
      CALL gspos('EN5G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
C
      par(3) = 0.5 * (TLrms + zent(5) - len3)
C
      CALL gsvolu('EN6G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN6G','SEEN',1)
C
      z = -TLrms + par(3)
C
      CALL gspos('EN6G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Create exit gas volumes, place collimators within them
      npar = 3
      par(1) = 0
      par(2) = Rrms
      par(3) = 6.7/2.
      CALL gsvolu('EX2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX2G','SEEN',1)
      z = 26.25
      CALL gspos('EX2G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 3.52/2.
      CALL gsvolu('EX3G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX3G','SEEN',1)
      CALL gspos('EX2C',1,'EX3G',0.,0.,0.,0,'ONLY')
      z = 31.44
      CALL gspos('EX3G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 4.2/2.
      CALL gsvolu('EX4G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX4G','SEEN',1)
      z = 35.3
      CALL gspos('EX4G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 5.5/2.
      CALL gsvolu('EX5G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX5G','SEEN',1)
      CALL gspos('EX3C',1,'EX5G',0.,0.,0.,0,'ONLY')
      z = 40.15
      CALL gspos('EX5G',1,'DETE',0.,0.,z,0,'ONLY')
C     
      par(1) = 0
      par(2) = Rrms
      par(3) = 5.24/2.
      CALL gsvolu('EX6G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX6G','SEEN',1)
      z = 45.48
      CALL gspos('EX6G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 4.16/2.
      CALL gsvolu('EX7G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX7G','SEEN',1)
      CALL gspos('EX4C',1,'EX7G',0.,0.,0.,0,'ONLY')
      z = 50.22
      CALL gspos('EX7G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 7.85/2.
      CALL gsvolu('EX8G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX8G','SEEN',1)
      z = 56.175
      CALL gspos('EX8G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 24.9/2.
      CALL gsvolu('EX9G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX9G','SEEN',1)
      CALL gspos('EX5C',1,'EX9G',0.,0.,0.,0,'MANY')
      z = 72.55
      CALL gspos('EX9G',1,'DETE',0.,0.,z,0,'ONLY')
C
      par(1) = 0
      par(2) = Rrms
      par(3) = 3.0/2.
      CALL gsvolu('EX10', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX10','SEEN',1)
      z = 86.5
      CALL gspos('EX10',1,'DETE',0.,0.,z,0,'ONLY')
C
C     Since the following collimator extends into Q1, it is placed
C     in the WRLD coordinates with no gas 
      z = 106.05
      CALL gspos('EX6C',1,'WRLD',0.,0.,z,0,'ONLY')
C     Need to define a TEND to change particle charge state
c$$$      par(3)=5.
c$$$      z = TLrms-par(3) 
c$$$      CALL gsvolu( 'TEND', 'TUBE', nmed, par, npar, ivolu)
c$$$      CALL gsatt('TEND','SEEN',1)
c$$$      CALL gspos('TEND', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
      RETURN
      END
C
