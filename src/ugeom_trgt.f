C.
      SUBROUTINE ugeo_trgt
C.
      IMPLICIT none
C.
      include 'uggeom.inc'              !local
      include 'geometry.inc'            !local
C.
      INTEGER   nmed, npar, ivolu
      REAL      z, par(10), zcol, zex2, lex11
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
      nmed = ment(1)
      if (targtype.eq.1) nmed=mcent
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

      nmed = ment(2)
      if (targtype.eq.1) nmed=mcent
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
      nmed = ment(3)
      if (targtype.eq.1) nmed=mcent
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
      nmed = ment(4)
      if (targtype.eq.1) nmed=mcent
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
      nmed = ment(5)
      if (targtype.eq.1) nmed=mcent
C.
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
      nmed = ment(6)
      if (targtype.eq.1) nmed=mcent
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
      nmed = mex(1)
      if (targtype.eq.1) nmed=mcent
C.
      npar   = 3
      
      if(tubetype.eq.1)then
         zcol = -(0.5*box_length + 0.945 + 1.026 + 2.54 + 0.624 +
     &        3.54 + 2.88 + 2.02 + 0.585*2.)
      else
         zcol = -22.5125
      endif
     

c      CALL gsvolu('EX1G', 'TUBE', nmed, par, npar, ivolu)
c      CALL gsatt('EX1G','SEEN',1)
c      z = -zcol
c      CALL gspos('EX1G', 1,'DETE',0.0,0.0,z,0,'ONLY')


      nmed = mex(2)
      if (targtype.eq.1) nmed=mcent

         lex11 = (zex(3) - lex2 + zcol)/2.
         zex2 = -zcol + lex11
         par(3) = lex11
   

C.
      npar   = 3
      par(1) = 0.
      par(2) = Rrms

     
C.
      CALL gsvolu('EX2G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX2G','SEEN',1)
C.
      z = zex2
C.
      CALL gspos('EX2G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
C.
C.--> Define exit 3 + 4  gas volumes
C.
      nmed = mex(3)
      if (targtype.eq.1) nmed=mcent
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
      nmed = mex(4)
      if (targtype.eq.1) nmed=mcent
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
      nmed = mex(5)
      if (targtype.eq.1) nmed=mcent
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
      nmed = mex(6)
      if (targtype.eq.1) nmed=mcent
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
      par(3) = lex4
C.    EX7G does not work properly in larger tubes.
C.    For larger tubes, it's constructed in mitray_setup.f
      if (tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7)) then
      CALL gsvolu('EX7G', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('EX7G','SEEN',1)
C.
      CALL gspos('EX4C', 1, 'EX7G', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      z = zex(7)
C.
      CALL gspos('EX7G', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
      endif
C.
      par(3) =  (TLrms -zex(7)+lex4)/2.
      if (tubetype.eq.1) par(3)=5.

      z = TLrms-par(3)
      if (tubetype.eq.1) z=0.
C. 
      CALL gsvolu( 'TEND', 'TUBE', nmed, par, npar, ivolu)
      CALL gsatt('TEND','SEEN',1)
C.
      if (tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7)) then
      CALL gspos('TEND', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')
      elseif (tubetype.eq.1) then
      CALL gspos('TEND', 1, 'EX6G', 0.0, 0.0, 0.0, 0, 'ONLY')
      endif

C.
      RETURN
      END
C.
