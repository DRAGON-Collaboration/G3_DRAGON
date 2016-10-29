C     Large pumping tubes
      SUBROUTINE ugeo_trgt_large
      IMPLICIT none
      include 'uggeom.inc'              !local
      include 'geometry.inc'            !local
      INTEGER   nmed, npar, ivolu
      REAL      z, par(10), zcol, zex2, lex11

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Al Entrance tubes
      nmed   = 6
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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Exit apertures (As Dave O's Feb 06 drawings)
      
      npar = 5
      par(1) = 12.9/2.
      par(2) = 2.34/2.
      par(3) = Rrms
      par(4) = 2.92/2.
      par(5) = Rrms
      CALL gsvolu('EX2C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX2C','SEEN',1)
C
      par(1) = 6.03/2.
      par(2) = 3.78/2.
      par(3) = Rrms
      par(4) = 3.78/2.
      par(5) = Rrms
      CALL gsvolu('EX3C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX3C','SEEN',1)
C
      par(1) = 13.8/2.
      par(2) = 3.96/2.
      par(3) = Rrms
      par(4) = 4.725/2.
      par(5) = Rrms
      CALL gsvolu('EX4C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX4C','SEEN',1)
C
      par(1) = 22.9/2.
      par(2) = 5.45/2.
      par(3) = Rrms
      par(4) = 6.51/2.
      par(5) = Rrms
      CALL gsvolu('EX6C','CONE',nmed,par,npar,ivolu)
      CALL gsatt('EX6C','SEEN',1)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Define entrance gas volumes and place the collimators inside
      nmed = mcent
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
C     Create exit gas volumes, place collimators inside them

c$$$      npar   = 3
c$$$      zcol = -(0.5*box_length + 0.945 + 1.026 + 2.54 + 0.624 +
c$$$     &        3.54 + 2.88 + 2.02 + 0.585*2.)
c$$$      lex11 = (zex(3) - lex2 + zcol)/2.
c$$$      zex2 = -zcol + lex11
c$$$      par(3) = lex11
c$$$   
c$$$
c$$$C.
c$$$      npar   = 3
c$$$      par(1) = 0.
c$$$      par(2) = Rrms

      par(1) = 0.
      par(2) = Rrms
      par(3) = 6.97/2.
      CALL gsvolu('EX2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX2G','SEEN',1)
      z = 26.465
      CALL gspos('EX2G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 12.9/2.
      CALL gsvolu('EX3G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX3G','SEEN',1)
      CALL gspos('EX2C', 1, 'EX3G', 0.0, 0.0, 0.0, 0, 'ONLY')
      z = 36.4
      CALL gspos('EX3G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 4.34/2.
      CALL gsvolu('EX4G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX4G','SEEN',1)
      z = 45.02
      CALL gspos('EX4G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 6.03/2.
      CALL gsvolu('EX5G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX5G','SEEN',1)
      CALL gspos('EX3C', 1, 'EX5G', 0.0, 0.0, 0.0, 0, 'ONLY')
      z = 50.205
      CALL gspos('EX5G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 8.35/2.
      CALL gsvolu('EX6G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX6G','SEEN',1)
      z = 57.395
      CALL gspos('EX6G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 13.8/2.
      CALL gsvolu('EX7G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX7G','SEEN',1)
      CALL gspos('EX4C', 1, 'EX7G', 0.0, 0.0, 0.0, 0, 'ONLY')
      z = 68.47
      CALL gspos('EX7G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C
      par(1) = 0.
      par(2) = Rrms
      par(3) = 12.63/2.
      CALL gsvolu('EX8G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX8G','SEEN',1)
      z = 81.685
      CALL gspos('EX8G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
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
C.
