C.
      SUBROUTINE ugeo_trgt
C.
      IMPLICIT none
C.
      include 'uggeom.inc'              !local
      include 'geometry.inc'            !local
C.
      INTEGER   nmed, npar, ivolu
      REAL      z, par(10), zcol
C.
C.    ------------------------------------------------------------------
C.
C.--> Define Al Entrance tubes
C.
      nmed   = 6
C. Aluminum
      npar   = 5
      par(1) = len1
      par(2) = rilen1
      par(3) =  Rrms
      par(4) = riren1
      par(5) = Rrms
C.
C.      CALL gsvolu('EN1C', 'CONE', nmed, par, npar, ivolu)
C.      CALL gsatt('EN1C','SEEN',1)
C.

      npar   = 5
      par(1) = len2
      par(2) = rilen2
      par(3) =  Rrms
      par(4) = riren2
      par(5) = Rrms
C.
      CALL gsvolu('EN2C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EN2C','SEEN',1)
C.
      npar   = 5
      par(1) = len3
      par(2) = rilen3
      par(3) =  Rrms
      par(4) = riren3
      par(5) = Rrms
C.
      CALL gsvolu('EN3C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EN3C','SEEN',1)
C.
C.--> Define exit apertures
C.
      npar   = 5
      par(1) = lex1
      par(2) = rilex1
      par(3) = Rrms
      par(4) = rirex1
      par(5) = Rrms
C.
C.      CALL gsvolu('EX1C', 'CONE', nmed, par, npar, ivolu)
C.      CALL gsatt('EX1C','SEEN',1)
C.
      par(1) = lex2
      par(2) = rilex2
      par(3) = Rrms
      par(4) = rirex2
      par(5) = Rrms
C.
      CALL gsvolu('EX2C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX2C','SEEN',1)
C.
      par(1) = lex3
      par(2) = rilex3
      par(3) = Rrms
      par(4) = rirex3
      par(5) = Rrms
C.
      CALL gsvolu('EX3C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX3C','SEEN',1)
C.
      par(1) = lex4
      par(2) = rilex4
      par(3) = Rrms
      par(4) = rirex4
      par(5) = Rrms
C.
      CALL gsvolu('EX4C', 'CONE', nmed, par, npar, ivolu)
      CALL gsatt('EX4C','SEEN',1)
C.
C.--> Define entrance 1 + 2 gas volumes
C.
C.    nmed = ment(1) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len1

C.--> Position new gas volumes to match new collimator 

      par(3) = 0.5*0.68
      CALL gsvolu('EN1G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN1G','SEEN',1)
      zcol = -(0.5*box_length -wall(2) - col_collar_length +
     +         col_length + par(3) )
      CALL gspos('EN1G', 1,'DETE',0.0,0.0,zcol,0,'ONLY')

      z = zent(1)

C.    nmed = ment(2) ! Need vacuum for solid carbon target
      nmed = mcent  
C.
      par(3) = 0.5 * (zent(1) - zent(3) - len1-len2)
C.
      CALL gsvolu('EN2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN2G','SEEN',1)
C.
      z = .5 * (zent(1) + zent(3)-len1 + len2)
C.
      CALL gspos('EN2G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define entrance 3 + 4 gas volumes
C.
C.    nmed = ment(3) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len2
C.
      CALL gsvolu('EN3G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN3G','SEEN',1)
C.
      CALL gspos('EN2C', 1, 'EN3G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zent(3)
C.
      CALL gspos('EN3G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.    nmed = ment(4) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      par(3) = 0.5 * (zent(3) - zent(5) - len3 -len2)
C.
      CALL gsvolu('EN4G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN4G','SEEN',1)
C.
      z = .5 * (zent(3) + zent(5) +len3 -len2)
C.
      CALL gspos('EN4G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define entrance 5 + 6 gas volumes
C.
C.    nmed = ment(5) ! Need vacuum for solid carbon target
      nmed = mcent
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = len3
C.
      CALL gsvolu('EN5G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN5G','SEEN',1)
C.
      CALL gspos('EN3C', 1, 'EN5G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zent(5)
C.
      CALL gspos('EN5G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.    nmed = ment(6) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      par(3) = 0.5 * (TLrms + zent(5) - len3)
C.
      CALL gsvolu('EN6G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EN6G','SEEN',1)
C.
      z = -TLrms + par(3)
C.
      CALL gspos('EN6G', 1,'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define exit 1 + 2  gas volumes
C.
C.    nmed = mex(1) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = lex1
C      par(3) = 0.5*0.68
      CALL gsvolu('EX1G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX1G','SEEN',1)
      z = -zcol
      CALL gspos('EX1G', 1,'DETE',0.0,0.0,z,0,'ONLY')

C      z = zex(1)

C.    nmed = mex(2) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
C     JS Change to par(3) done for new pumping tube system
C      par(3) = (zex(3) - zex(1) - lex1 - lex2)/2.
      par(3) = (zex(3) + zcol - lex1 - lex2)/2.
C.
      CALL gsvolu('EX2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX2G','SEEN',1)
C     JS Change to z done for new pumping tube system
C      z = (zex(1) + zex(3) + lex1 - lex2)/2.
      z = (-zcol + zex(3) + lex1 - lex2)/2.      
C.
      CALL gspos('EX2G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define exit 3 + 4  gas volumes
C.
C.    nmed = mex(3) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = lex2
C.
      CALL gsvolu('EX3G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX3G','SEEN',1)
C.
      CALL gspos('EX2C', 1, 'EX3G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zex(3)
C.
      CALL gspos('EX3G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.    nmed = mex(4) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = (zex(5) - zex(3)- lex2 - lex3)/2.
C.
      CALL gsvolu('EX4G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX4G','SEEN',1)
C.
      z = (zex(3) + zex(5) + lex2 - lex3)/2.
C.
      CALL gspos('EX4G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define exit 5 + 6  gas volumes
C.
C.    nmed = mex(5) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = lex3
C.
      CALL gsvolu('EX5G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX5G','SEEN',1)
C.
      CALL gspos('EX3C', 1, 'EX5G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zex(5)
C.
      CALL gspos('EX5G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.    nmed = mex(6) ! Need vacuum for solid carbon target
      nmed = mcent
C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = (zex(7) - zex(5) - lex3- lex4)/2.
C.
C.
      CALL gsvolu('EX6G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX6G','SEEN',1)
C.
      z = (zex(7) + zex(5) + lex3 - lex4)/2.
C.
      CALL gspos('EX6G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
      npar = 3
      par(1) = 0.
      par(2) = Rrms
      par(3) = lex4
C. JS: In new pumping tube system EX7G is inside Quad 1.  This causes serious
C. problems with GEANTS interpretation of the magnetic fields.  DH has
C. suggested removing EX7G from the simulation
C      CALL gsvolu('EX7G', 'TUBE', nmed, par, npar, ivolu)
C      CALL gsatt('EX7G','SEEN',1)
C.
C      CALL gspos('EX4C', 1, 'EX7G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zex(7)
C. JS - EX7G removed in new pumping tube system.  See above comment.
C      CALL gspos('EX7G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C JS changed par(3) for new pumping tube system
      par(3) = (TLrms+30.0 -zex(7)+lex4)/2.
C      par(3) =  (TLrms -zex(7)+lex4)/2.
C JS changed z for new pumping tube system
      z = TLrms+30.0-par(3)
C      z = TLrms-par(3)
C.
C      JS has just outright removed TEND from the simulation
C      It doesn't work right now that it's inside Q1
C      CALL gsvolu( 'TEND', 'TUBE', nmed, par, npar, ivolu)
C      CALL gsatt('TEND','SEEN',1)
C.
C      CALL gspos('TEND', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
      RETURN
      END
C.
