C.
      SUBROUTINE ugeo_dipole(k,uh,irot)
C.
************************************************************************
*                                                                      *
*                     Define a RAYTRACE dipole magnet                  *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER irot, iirot, ivol, jr1, jr2, nxtrotm
C.
      REAL r, dr, gap, z11, z22, alpha, beta, phi, theta, rho
C.
      REAL x1, x2, x11, x12, x21, x22
      REAL dx, dy, dz, the1, phi1, the2, phi2, the3, phi3
      REAL dxp,dyp,dzp
C.
      REAL shape(11), pos(3), uh(3), dd(3)
C.
      INTEGER k, i, n, lnam(15), lnum(15), ier
C.
      CHARACTER*4 kname, lname, vvname
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C.
      include 'gcbank.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant 
C.
      include 'geom_dipole.inc'         !local
C.
      If(k.lt.10)then
        write(wvol,10)k
   10   Format(I1)
        kname = 'D'//wvol//'  '
        lname = 'BN'//wvol//' '
      Else
        write(vvol,20)k
   20   Format(I2)
        kname = 'D'//vvol//' '
        lname = 'BN'//vvol
      Endif
C.
      CALL ucopy(pos_dipole(1,k),pos(1),3)
      gap   =            gap_dipole(k)
      r     =          abs(r_dipole(k))
      dr    =             dr_dipole(k)
      phi   = degrad *   phi_dipole(k)
      alpha = degrad * alpha_dipole(k)
      beta  = degrad *  beta_dipole(k)
      z11   =            z11_dipole(k)
      z22   =            z22_dipole(k)
C.
      x21 = r*sin(phi/2.)
      x21 = x21 + r*(1.-cos(phi/2.))*tan(phi/2.-alpha)
      x21 = x21 + z11/cos(phi/2.-alpha)
      x21 = x21 + dr * tan(phi/2.-alpha)
C.
      x11 = x21 - (r+dr) * tan(phi/2.-alpha)
C.
      x22 = r*sin(phi/2.)
      x22 = x22 + r*(1.-cos(phi/2.))*tan(phi/2.- beta)
      x22 = x22 + z22/cos(phi/2.- beta)
      x22 = x22 + dr * tan(phi/2.- beta)
C.
      x12 = x22 - (r+dr) * tan(phi/2.- beta)
C.
      x1 = (x11 + x12)/2.
      x2 = (x21 + x22)/2.
C.
      theta = atan((x2-x1-(r+dr)*tan(phi/2.-alpha))/(r+dr))
C.
      shape( 1) = gap/2.
      shape( 2) = 0.0
      shape( 3) = 0.0
      shape( 4) = (r+dr)/2.
      shape( 5) = x1
      shape( 6) = x2
      shape( 7) = raddeg * theta
      shape( 8) = shape( 4)
      shape( 9) = shape( 5)
      shape(10) = shape( 6)
      shape(11) = shape( 7)
C.
C      CALL gsvolu (kname, 'TRAP', 1, shape, 11, ivol)
      CALL gsvolu (kname, 'TRAP', 2, shape, 11, ivol)
      CALL gsatt(kname,'SEEN',1)
C.
      dx = -(r+dr)/2.*tan(theta)
      dx = dx - (x1-x11)
      dy = -(r+dr)/2.
      dz = 0.0

CC Vacuum Vessel Componants

c     Rotation Matrices 
      CALL GSROTM (200,90.0,25.0,90.0,115.0,0.0,0.0)
      CALL GSROTM (201,90.0,-6.0,90.0,84.0,0.0,0.0)
      CALL GSROTM (202,90.0,-25.0,90.0,65.0,0.0,0.0)
      CALL GSROTM (203,90.0,12.0,90.0,102.0,0.0,0.0)
      CALL GSROTM (204,90.0,-33.0,90.0,57.0,0.0,0.0)

      if (k.eq.2) then
c     MD2
         shape( 1) = r-9.0
         shape( 2) = r-6.0
         shape( 3) = 5.0
         shape( 4) = 90. - raddeg * phi/2.
         shape( 5) = 90. + raddeg * phi/2.

         vvname = 'VV1 '
         CALL gsvolu (vvname, 'TUBS', 20, shape,  5, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname, k, kname, dx, dy, dz, 0, 'MANY')
         
         shape( 1) = r+6.0
         shape( 2) = r+9.0
         shape( 3) = 5.0
         shape( 4) = 90. - raddeg * phi/2.
         shape( 5) = 90. + raddeg * phi/2.

         vvname = 'VV2 '
         CALL gsvolu (vvname, 'TUBS', 20, shape,  5, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname, k, kname, dx, dy, dz, 0, 'MANY')

         shape( 1) = r-9.0
         shape( 2) = r+9.0
         shape( 3) = 0.375/2.0
         shape( 4) = 90. - raddeg * phi/2.
         shape( 5) = 90. + raddeg * phi/2.

         vvname = 'VV3 '
         CALL gsvolu (vvname, 'TUBS', 20, shape,  5, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dx,dy,dz+5.0+0.375/2.0,0,'MANY')
         
         shape( 1) = r-9.0
         shape( 2) = r+9.0
         shape( 3) = 0.375/2
         shape( 4) = 90. - raddeg * phi/2.
         shape( 5) = 90. + raddeg * phi/2.

         vvname = 'VV4 '
         CALL gsvolu (vvname, 'TUBS', 20, shape,  5, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dx,dy,dz-5.0-0.375/2.0,0,'MANY')
         
      else if (k.eq.1) then
c     MD1

c        Part 1
         dxp = dx - (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*cos(65.*degrad)-
     +        1.75*2.54/2.0*cos(25.0*degrad)
         dyp = dy + (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*sin(65.*degrad)-
     +        1.75*2.54/2.0*sin(25.0*degrad)
         dzp = dz
         
         shape( 1) = 1.75/2.0*2.54
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV5 '
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,200,'MANY')

c        Part 2
         dxp = dx - (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*cos(65.*degrad)+
     +        22.8460*cos(25.0*degrad)
         dyp = dy + (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*sin(65.*degrad)+
     +        22.8460*sin(25.0*degrad)
         dzp = dz
         
         shape( 1) = 27.291
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV6 '
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,200,'MANY')

c        Part 3
         dxp = dx - (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*cos(65.*degrad)+
     +        (22.8460+27.291)*cos(25.0*degrad) + 26.515*cos(6.0*degrad)
         dyp = dy + (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*sin(65.*degrad)+
     +        (22.8460+27.291)*sin(25.0*degrad) - 26.515*sin(6.0*degrad)
         dzp = dz
         
         shape( 1) = 26.515
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV7 '
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,201,'MANY')

c        Part 4
         dxp = dx - (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*cos(65.*degrad)+
     +        (22.8460+27.291)*cos(25.0*degrad) + 53.03*cos(6.0*degrad)+
     +        6.836935*cos(25.0*degrad)
         dyp = dy + (100.0+6.3/2.0*2.54+0.375/2.0*2.54)*sin(65.*degrad)+
     +        (22.8460+27.291)*sin(25.0*degrad) - 53.03*sin(6.0*degrad)-
     +        6.836935*sin(25.0*degrad)
         dzp = dz
         
         shape( 1) = 6.836935
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV8 '
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,202,'MANY')

c        Part 5
         dxp = dx - (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*cos(65.*degrad)+
     +        19.234*cos(12.0*degrad)
         dyp = dy + (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*sin(65.*degrad)+
     +        19.234*sin(12.0*degrad)
         dzp = dz
         
         shape( 1) = 19.234
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV9 '
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,203,'MANY')

c        Part 6
         dxp = dx - (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*cos(65.*degrad)+
     +        2.0*19.234*cos(12.0*degrad) + 20.0495*cos(33.0*degrad)
         dyp = dy + (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*sin(65.*degrad)+
     +        2.0*19.234*sin(12.0*degrad) - 20.0495*sin(33.0*degrad)
         dzp = dz
         
         shape( 1) = 20.0495
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV10'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,204,'MANY')

c        Part 7
         dxp = dx - (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*cos(65.*degrad)+
     +        2.0*19.234*cos(12.0*degrad) + 40.099*cos(33.0*degrad) +
     +        6.836935*cos(25.0*degrad)
         dyp = dy + (100.0-6.3/2.0*2.54-0.375/2.0*2.54)*sin(65.*degrad)+
     +        2.0*19.234*sin(12.0*degrad) - 40.099*sin(33.0*degrad) -
     +        6.836935*sin(25.0*degrad)
         dzp = dz
         
         shape( 1) = 6.836935
         shape( 2) = 0.375/2.0*2.54
         shape( 3) = 4.0
         
         vvname = 'VV11'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,202,'MANY')

c        Part 8
         dxp = dx - (100.0)*cos(65.*degrad)+20.0*cos(25.0*degrad)
         dyp = dy + (100.0)*sin(65.*degrad)+20.0*sin(25.0*degrad)
         dzp = dz + 4.0+0.375*2.54/2.0
         
         shape( 1) = 20.0+1.75*2.54
         shape( 2) = (6.3+2.0*0.375)*2.54/2.0+10.0
         shape( 3) = 0.375/2.0*2.54
         
         vvname = 'VV12'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,200,'MANY')

c        Part 9
         dxp = dx - (100.0)*cos(65.*degrad)+20.0*cos(25.0*degrad)+40.0
         dyp = dy + (100.0)*sin(65.*degrad)+20.0*sin(25.0*degrad)-10.0
         dzp = dz + 4.0+0.375*2.54/2.0
         
         shape( 1) = 20.0+1.75*2.54+13.0
         shape( 2) = (6.3+2.0*0.375)*2.54/2.0+10.0+32.0
         shape( 3) = 0.375/2.0*2.54
         
         vvname = 'VV13'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,202,'MANY')

c        Part 10
         dxp = dx - (100.0)*cos(65.*degrad)+20.0*cos(25.0*degrad)
         dyp = dy + (100.0)*sin(65.*degrad)+20.0*sin(25.0*degrad)
         dzp = dz - 4.0-0.375*2.54/2.0
         
         shape( 1) = 20.0+1.75*2.54
         shape( 2) = (6.3+2.0*0.375)*2.54/2.0+10.0
         shape( 3) = 0.375/2.0*2.54
         
         vvname = 'VV14'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,200,'MANY')

c        Part 11
         dxp = dx - (100.0)*cos(65.*degrad)+20.0*cos(25.0*degrad)+40.0
         dyp = dy + (100.0)*sin(65.*degrad)+20.0*sin(25.0*degrad)-10.0
         dzp = dz - 4.0-0.375*2.54/2.0
         
         shape( 1) = 20.0+1.75*2.54+13.0
         shape( 2) = (6.3+2.0*0.375)*2.54/2.0+10.0+32.0
         shape( 3) = 0.375/2.0*2.54
         
         vvname = 'VV15'
         CALL gsvolu (vvname, 'BOX ', 20, shape, 3, ivol)
         CALL gsatt(vvname,'SEEN',1)
         CALL gspos(vvname,k,kname,dxp,dyp,dzp,202,'MANY')
         
      else 
         write(*,*) "Wow, three MD's, add my vacuum vessel please."
         stop
      endif




CC End of Vacuum Vessel

C.
      CALL vscale(uh,-dx,dd,3)
      CALL vadd(pos,dd,pos,3)
C.
      shape( 1) = r-dr
      shape( 2) = r+dr
      shape( 3) = gap/2.
      shape( 4) = 90. - raddeg * phi/2.
      shape( 5) = 90. + raddeg * phi/2.
C.
CCC      CALL gsvolu (lname, 'TUBS', 3, shape,  5, ivol)
CCC      CALL gsatt(lname,'SEEN',1)
C.
CCC      CALL gspos(lname, k, kname, dx, dy, dz, 0, 'ONLY')
C.
      rho = raddeg * atan((z11/cos(alpha))/r)
      rho = 90.0 - raddeg*phi/2. - rho
      rho = degrad * rho
C.
C. ***** To position the A-frame at the edge of the entrance fringe field
C.
C.      dx = dx - sqrt(r**2+(z11/cos(alpha))**2) * cos(rho)
C.      dy = dy + sqrt(r**2+(z11/cos(alpha))**2) * sin(rho)
C.      dz = 0.0
C.
C. ***** To position the A-frame at the edge of the entrance EFB
C.
      dx = dx - r * sin(phi/2.)
      dy = dy + r * cos(phi/2.)
      dz = 0.0
C.
      shape( 1) = 2.*(r+dr)
      shape( 2) = gap/2.
      shape( 3) = 2.*x2
C.
CCC      CALL gsvolu (mname, 'BOX ', 2, shape,  3, ivol)
CCC      CALL gsatt(mname,'SEEN',0)
C.
      the1 =  90.
      phi1 =  90. + raddeg*phi/2.
      the2 =   0.
      phi2 =   0.
      the3 =  90.
      phi3 =        raddeg*phi/2.
C.
      iirot = nxtrotm()
      CALL gsrotm(iirot,the1,phi1,the2,phi2,the3,phi3)
C.
CCC      CALL gspos(mname, k, kname, dx, dy, dz, iirot, 'MANY')
C.
      dx_dipole(1,k) = dx
      dx_dipole(2,k) = dy
      dx_dipole(3,k) = dz
C.
      irot_dipole(k) = iirot
C.
      CALL gspos(kname,k,'WRLD',pos(1),pos(2),pos(3),irot,'MANY')
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_edipol(k,uh,irot)
C.
************************************************************************
*                                                                      *
*               Define a RAYTRACE electrostatic deflector              *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER irot, iirot, ivol, jr1, jr2, nxtrotm
C.
      REAL r, dr, gap, z11, z22, phi, theta, rho
C.
      REAL x1, x2, x11, x12, x21, x22
      REAL dx, dy, dz, the1, phi1, the2, phi2, the3, phi3
C.
      REAL shape(11), pos(3), uh(3), dd(3)
C.
      INTEGER k, i, n, lnam(15), lnum(15), ier
C.
      CHARACTER*4 kname, lname, lnameb
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C.
      include 'gcbank.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant 
C.
      include 'geom_edipol.inc'         !local
C.
      If(k.lt.10)then
        write(wvol,10)k
   10   Format(I1)
        kname = 'E'//wvol//'  '
        lname = 'PN'//wvol//' '
        lnameb = 'PM'//wvol//' '
      Else
        write(vvol,20)k
   20   Format(I2)
        kname = 'E'//vvol//' '
        lname = 'PN'//vvol
        lnameb = 'PM'//vvol
      Endif
C.
      CALL ucopy(pos_edipol(1,k),pos(1),3)
      gap   =            gap_edipol(k)
      r     =          abs(r_edipol(k))
      dr    =             dr_edipol(k)
      phi   = degrad *   phi_edipol(k)
      z11   =            z11_edipol(k)
      z22   =            z22_edipol(k)
C.
      x21 = r*sin(phi/2.)
      x21 = x21 + r*(1.-cos(phi/2.))*tan(phi/2.)
      x21 = x21 + z11/cos(phi/2.)
      x21 = x21 + dr * tan(phi/2.)
C.
      x11 = x21 - (r+dr) * tan(phi/2.)
C.
      x22 = r*sin(phi/2.)
      x22 = x22 + r*(1.-cos(phi/2.))*tan(phi/2.)
      x22 = x22 + z22/cos(phi/2.)
      x22 = x22 + dr * tan(phi/2.)
C.
      x12 = x22 - (r+dr) * tan(phi/2.)
C.
      x1 = (x11 + x12)/2.
      x2 = (x21 + x22)/2.
C.
      theta = atan((x2-x1-(r+dr)*tan(phi/2.))/(r+dr))
C.
      shape( 1) = gap/2.
      shape( 2) = 0.0
      shape( 3) = 0.0
      shape( 4) = (r+dr)/2.
      shape( 5) = x1
      shape( 6) = x2
      shape( 7) = raddeg * theta
      shape( 8) = shape( 4)
      shape( 9) = shape( 5)
      shape(10) = shape( 6)
      shape(11) = shape( 7)
C.
C      CALL gsvolu (kname, 'TRAP', 1, shape, 11, ivol)
      CALL gsvolu (kname, 'TRAP', 2, shape, 11, ivol)
      CALL gsatt(kname,'SEEN',1)
C.
      dx = -(r+dr)/2.*tan(theta)
      dx = dx - (x1-x11)
      dy = -(r+dr)/2.
      dz = 0.0
C.
      CALL vscale(uh,-dx,dd,3)
      CALL vadd(pos,dd,pos,3)
C.
      shape( 1) = r-8.0
      shape( 2) = r-5.0
      if (k.eq.1) then
         shape(3) = 14.0
      else
         shape(3) = 15.0
      end if
      shape( 4) = 90. - raddeg * phi/2.
      shape( 5) = 90. + raddeg * phi/2.
C.
      CALL gsvolu (lname, 'TUBS', 20, shape,  5, ivol)
      CALL gsatt(lname,'SEEN',1)
      CALL gspos(lname, k, kname, dx, dy, dz, 0, 'MANY')

      shape( 1) = r+5.0
      shape( 2) = r+8.0
      if (k.eq.1) then
         shape(3) = 14.0
      else
         shape(3) = 15.0
      end if
      shape( 4) = 90. - raddeg * phi/2.
      shape( 5) = 90. + raddeg * phi/2.
C.
      CALL gsvolu (lnameb, 'TUBS', 20, shape,  5, ivol)
      CALL gsatt(lnameb,'SEEN',1)
      CALL gspos(lnameb, k, kname, dx, dy, dz, 0, 'MANY')
C.
      rho = raddeg * atan(z11/r)
      rho = 90.0 - raddeg*phi/2. - rho
      rho = degrad * rho
C.
C. ***** To position the A-frame at the edge of the entrance fringe field
C.
C.      dx = dx - sqrt(r**2+z11**2) * cos(rho)
C.      dy = dy + sqrt(r**2+z11**2) * sin(rho)
C.      dz = 0.0
C.
C. ***** To position the A-frame at the edge of the entrance EFB
C.
      dx = dx - r * sin(phi/2.)
      dy = dy + r * cos(phi/2.)
      dz = 0.0
C.
      shape( 1) = 2.*(r+dr)
      shape( 2) = gap/2.
      shape( 3) = 2.*x2
C.
CCC      CALL gsvolu (mname, 'BOX ', 2, shape,  3, ivol)
CCC      CALL gsatt(mname,'SEEN',0)
C.
      the1 =  90.
      phi1 =  90. + raddeg*phi/2.
      the2 =   0.
      phi2 =   0.
      the3 =  90.
      phi3 =        raddeg*phi/2.
C.
      iirot = nxtrotm()
      CALL gsrotm(iirot,the1,phi1,the2,phi2,the3,phi3)
C.
CCC      CALL gspos(mname, k, kname, dx, dy, dz, iirot, 'MANY')
C.
      dx_edipol(1,k) = dx
      dx_edipol(2,k) = dy
      dx_edipol(3,k) = dz
C.
      irot_edipol(k) = iirot
C.
      CALL gspos(kname,k,'WRLD',pos(1),pos(2),pos(3),irot,'MANY')
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_mpole(k,irot)
C.
************************************************************************
*                                                                      *
*                   Define a RAYTRACE mulipoles magnet                 *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER k, ivol, irot, nxtrotm
C.
      REAL shape(3), pos(3), dx, dy, dz, shape_pip(3)
      REAL the1, phi1, the2, phi2, the3, phi3
C.
      CHARACTER*4 kname
      CHARACTER*5 pname
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C. 
      include 'geom_mpole.inc'          !local
C.
      If(k.lt.10)then
        write(wvol,10)k
   10   Format(I1)
        kname = 'Q'//wvol//'  '
        pname = 'BP'//wvol//'  ' !! beampipe section
      Else
        write(vvol,20)k
   20   Format(I2)
        kname = 'Q'//vvol//' '
        pname = 'BP'//vvol//' '
      Endif
C.
      CALL ucopy(pos_mpole(1,k),pos(1),3)
C.
      shape( 1) = 0.0
      shape( 2) = r_mpole(k)
      shape( 3) = (efblength_mpole(k)+z11_mpole(k)+z22_mpole(k))/2.
      if(kname.eq.'Q1 ') print*, 'Q1.....',shape(3)
C.
C      CALL gsvolu (kname, 'TUBE', 1, shape, 3, ivol)
      CALL gsvolu (kname, 'TUBE', 2, shape, 3, ivol)
      CALL gsatt(kname,'SEEN',1)
C.
C. ***** To position the A-frame at the edge of the entrance EFB
C.
      dx = 0.0
      dy = 0.0
      dz = (z11_mpole(k)-z22_mpole(k)-efblength_mpole(k))/2.
C.
      shape( 1) = 2.*shape(2)
      shape( 2) = 2.*shape(2)
      shape( 3) = 2.*shape(3)
C.
CCC      CALL gsvolu (mname, 'BOX ', 2, shape,  3, ivol)
CCC      CALL gsatt(mname,'SEEN',0)
C.
CCC      CALL gspos(mname, k, kname, dx, dy, dz, 0, 'MANY')
C.
      dx_mpole(1,k) = dx
      dx_mpole(2,k) = dy
      dx_mpole(3,k) = dz
C.
      CALL gspos(kname,k,'WRLD',pos(1),pos(2),pos(3),irot,'MANY')
      if(kname.eq.'Q1 ') print*, 'Q1.....',pos(3)
C.
C.--> insert beampipe in same space as multipole
C.    (comment this out if not required)
      shape_pip(1) = dxcol_mpole(1,k) ! inner radius
      shape_pip(2) = shape_pip(1)+0.15875 ! plus 1/16th inch
      shape_pip(3) = (efblength_mpole(k)+z11_mpole(k)+z22_mpole(k))/2.
      CALL gsvolu(pname,'TUBE',20,shape_pip,3,ivol)
      CALL gspos(pname,k,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY')
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_sole(k,irot)
C.
************************************************************************
*                                                                      *
*                    Define a RAYTRACE solenoid magnet                 *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER k, ivol, irot, nxtrotm
C.
      REAL shape(3), pos(3), dx, dy, dz
      REAL the1, phi1, the2, phi2, the3, phi3
C.
      CHARACTER*4 kname
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C. 
      include 'geom_sole.inc'           !local
C.
      If(k.lt.10)then
        write(wvol,10)k
   10   Format(I1)
        kname = 'S'//wvol//'  '
      Else
        write(vvol,20)k
   20   Format(I2)
        kname = 'S'//vvol//' '
      Endif
C.
      CALL ucopy(pos_sole(1,k),pos(1),3)
C.
      shape( 1) = 0.0
      shape( 2) = r_sole(k)
      shape( 3) = (efblength_sole(k)+z11_sole(k)+z22_sole(k))/2.
C.
C      CALL gsvolu (kname, 'TUBE', 1, shape, 3, ivol)
      CALL gsvolu (kname, 'TUBE', 2, shape, 3, ivol)
      CALL gsatt(kname,'SEEN',1)
C.
C. ***** To position the A-frame at the edge of the entrance EFB
C.
      dx = 0.0
      dy = 0.0
      dz = (z11_sole(k)-z22_sole(k)-efblength_sole(k))/2.
C.
      shape( 1) = 2.*shape(2)
      shape( 2) = 2.*shape(2)
      shape( 3) = 2.*shape(3)
C.
CCC      CALL gsvolu (mname, 'BOX ', 2, shape,  3, ivol)
CCC      CALL gsatt(mname,'SEEN',0)
C.
CCC      CALL gspos(mname, k, kname, dx, dy, dz, 0, 'MANY')
C.
      dx_sole(1,k) = dx
      dx_sole(2,k) = dy
      dx_sole(3,k) = dz
C.
      CALL gspos(kname,k,'WRLD',pos(1),pos(2),pos(3),irot,'MANY')
C.
  999 RETURN
      END
C
      SUBROUTINE ugeo_col(pos,irot,data,rname)
C.
************************************************************************
*                                                                      *
*                        Define a REAL COLLIMATOR                      *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER i, jr, ivol, irot
C.
      REAL pos(3), data(6), shape(3)
      REAL d(3), rmat(10), posn(3), rmatn(10)
C.
      CHARACTER*4 kname,rname
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C.
      DATA i / 0 /
C.
      include 'gcbank.inc'          !geant
C.
      jr = lq(jrotm - irot)
      CALL ucopy(q(jr+1),rmat(1),10)
C.
      If(data(1).eq.0.0)then
C.
        i = i + 1
        If(i.lt.10)then
          write(wvol,10)i
   10     Format(I1)
          kname = 'C'//wvol//'  '
        Else
          write(vvol,20)i
   20     Format(I2)
          kname = 'C'//vvol//' '
        Endif
C.

        if (rname.eq.'QSLT') then
           shape(1) = 20.*data(4)
           shape(2) = 20.*data(5)
        else if (rname.eq.'MSLT') then
           shape(1) = 10.*data(4)
           shape(2) = 10.*data(5)
        else if (rname.eq.'FSLT') then
           shape(1) = 10.*data(4)
           shape(2) = 10.*data(5)
        else 
           shape(1) = 1.*data(4)
           shape(2) = 1.*data(5)
        end if        
        shape(3) = data(6)

C.
        CALL gsvolu (kname, 'BOX ', 5, shape, 3, ivol)
        CALL gsatt(kname,'SEEN',1)
C.
C. **** Do +x border
C.
        d(1) = data(2) + data(4) + shape(1)
        d(2) = data(3)
        d(3) = 0.0
C.
        CALL gtrmul(pos,rmat,d,0,posn,rmatn)
C.
        CALL gspos(kname,1,'WRLD',posn(1),posn(2),posn(3),irot,'ONLY') 
C.
C. **** Do -x border
C.
        d(1) = data(2) - data(4) - shape(1)
C.
        CALL gtrmul(pos,rmat,d,0,posn,rmatn)
C.
        CALL gspos(kname,2,'WRLD',posn(1),posn(2),posn(3),irot,'ONLY')
C.
        i = i + 1
        If(i.lt.10)then
          write(wvol,10)i
          kname = 'C'//wvol//'  '
        Else
          write(vvol,20)i
          kname = 'C'//vvol//' '
        Endif
C.
        if (rname.eq.'QSLT') then
           shape(1) = 20.*data(4)
           shape(2) = 20.*data(5)
        else if (rname.eq.'MSLT') then
           shape(1) = 10.*data(4)
           shape(2) = 10.*data(5)
        else if (rname.eq.'FSLT') then
           shape(1) = 10.*data(4)
           shape(2) = 10.*data(5)
        else 
           shape(1) = 1.*data(4)
           shape(2) = 1.*data(5)
        end if
        shape(3) = data(6)
C.
        CALL gsvolu (kname, 'BOX ', 5, shape, 3, ivol)
        CALL gsatt(kname,'SEEN',1)
C.
C. **** Do +y border
C.
        d(1) = data(2)
        d(2) = data(3) + data(5) + shape(2)
        d(3) = 0.0
C.
        CALL gtrmul(pos,rmat,d,0,posn,rmatn)
C.
        CALL gspos(kname,1,'WRLD',posn(1),posn(2),posn(3),irot,'ONLY') 
C.
C. **** Do -y border
C.
        d(2) = data(3) - data(5) - shape(2)
C.
        CALL gtrmul(pos,rmat,d,0,posn,rmatn)
C.
        CALL gspos(kname,2,'WRLD',posn(1),posn(2),posn(3),irot,'ONLY')
C.
      Else
C.
        i = i + 1
        If(i.lt.10)then
          write(wvol,10)i
          kname = 'C'//wvol//'  '
        Else
          write(vvol,20)i
          kname = 'C'//vvol//' '
        Endif
C.
        shape(1) =    data(4)
        shape(2) =    data(5)
        shape(3) =    data(6)
C.
        CALL gsvolu (kname, 'TUBE', 5, shape, 3, ivol)
        CALL gsatt(kname,'SEEN',1)
C.
        d(1) = data(2)
        d(2) = data(3)
        d(3) = 0.0
C.
        CALL gtrmul(pos,rmat,d,0,posn,rmatn)
C.
        CALL gspos(kname,1,'WRLD',posn(1),posn(2),posn(3),irot,'ONLY') 
C.
      Endif
C.
  999 RETURN
      END
C
      SUBROUTINE ugeo_end(pos,irot)
C.
************************************************************************
*                                                                      *
*                   Define a RAYTRACE final volume                     *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER ivol, irot, MCP
C.
      REAL shape(3), pos(3), dead, det
      include 'gcvolu.inc'      !geant 
 
C.
      shape( 1) = 2.4
      shape( 2) = 2.4
      shape( 3) =   0.1
      det = shape(3)
C.
      CALL gsvolu ('ENDV', 'BOX ', 19, shape, 3, ivol)
      CALL gsatt('ENDV','SEEN',1)
C.
      CALL gspos('ENDV',1,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY')

C     Position deadlayer at surface of volume.
C     Note: since GEANT cannot track very accurately through
C     thin layers, create a new medium which is modified silicon,
C     make it 1000 x thicker but proportionately less dense so that the 
C     energy loss calculated is correct.
      dead = 3.5E-05*100. !! 0.35 micrometres * 100.
      shape(3) = dead/2.
      CALL gsvolu('DEAD','BOX ',23,shape, 3, ivol)
      CALL gsatt('DEAD','SEEN',1)
      CALL gspos('DEAD',1,'WRLD',pos(1),pos(2),pos(3)+(det+shape(3)),
     +     irot,'ONLY')

C     Add in MCP foil 20 microgram/cm**2, 25.4 cm diameter, 50 cm from 
C     DSSSD
      MCP = 0
      if(MCP.eq.1) then
C     First make foil holder (Aluminium) 10cm x 10 cm x 0.1 cm
         shape(1) = 5.
         shape(2) = 5.
         shape(3) = 0.05
         CALL gsvolu('HOLD','BOX ',6,shape,3,ivol)
         CALL gsatt('HOLD','SEEN',1)
C     Position a 25.4 mm hole (vacuum) into the holder
         shape(1) = 0.
         shape(2) = 1.0 !!1.27
         shape(3) = 0.05
         CALL gsvolu('HOLE','TUBE',1,shape,3,ivol)
         CALL gsatt('HOLE','SEEN',1)
C         CALL gspos('HOLE',1,'HOLD',0.0,0.0,0.0,irot,'ONLY')
C     Position the MCP foil into the holder
         shape(1) = 0.
         shape(2) = 1.27
         shape(3) = 8.889E-03 !! 20 microgram/cm**2 / 2.25 g/cm**3 x 1000
         CALL gsvolu('MCP ','TUBE',24,shape,3,ivol)
         CALL gsatt('MCP ','SEEN',1)
C         CALL gspos('MCP ',1,'HOLE',0.0,0.0,0.0,irot,'ONLY')
C     Position MCP at -50cm from DSSSD
C         CALL gspos('HOLD',1,'WRLD',pos(1),pos(2),pos(3)+50.,irot,
C     *        'ONLY')
      endif
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_start(pos,irot)
C.
************************************************************************
*                                                                      *
*                   Define a RAYTRACE start volume                     *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER ivol, irot
C.
      REAL shape(3), pos(3) 
C.
      shape( 1) = 100.
      shape( 2) = 100.
      shape( 3) =   1.0
C.
      CALL gsvolu ('STRV', 'BOX ', 1, shape, 3, ivol)
      CALL gsatt('STRV','SEEN',1)
C.
      CALL gspos('STRV',1,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY') 
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_dssd(pos,irot)
C.
************************************************************************
*                                                                      *
*                   Define a DSSD detector                             *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER ivol, irot
C.
      REAL shape(3), pos(3) 
C.
      shape( 1) = 4.8
      shape( 2) = 4.8
      shape( 3) =   0.1
C.
      CALL gsvolu ('DSSD', 'BOX ', 19, shape, 3, ivol)
      CALL gsatt('DSSD','SEEN',1)
C.
      CALL gspos('DSSD',1,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY') 
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_test(k,pos,irot)
C.
************************************************************************
*                                                                      *
*                   Define a DSSD test volume (alpha acceptance tests) *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER ivol, irot, k
C.
      REAL shape(3), pos(3)
      CHARACTER*4 kname
      CHARACTER*1 wvol
      CHARACTER*2 vvol
C. 
C.
      If(k.lt.10)then
        write(wvol,10)k
   10   Format(I1)
        kname = 'TST'//wvol//'  '
      Else
        write(vvol,20)k
   20   Format(I2)
        kname = 'TS'//vvol//' '
      Endif 
C.
      shape( 1) = 4.8
      shape( 2) = 4.8
      shape( 3) =   0.1
C.
      CALL gsvolu (kname, 'BOX ', 1, shape, 3, ivol)
      CALL gsatt(kname,'SEEN',1)
C.
      CALL gspos(kname,1,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY') 
      print*, pos(1), pos(2), pos(3), 'TEST'
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_mcp(pos,irot,data,rname,nmcp)
C.
************************************************************************
*                                                                      *
*                   Define a MCP detector                              *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER i, jr, ivol, irot, nmcp
C.
      REAL data(2), shape(3)
      REAL d(3), rmat(10), pos(3), rmatn(10)
C.
      CHARACTER*4 kname, lname, rname
C.
      CHARACTER*1 wvol
C. 
C.
        write(wvol,10)nmcp
   10   Format(I1)
        kname = 'HLD'//wvol//'  '
        lname = 'HOL'//wvol//'  '

C        write (*,*) 'ugeo_mcp for MCP ', rname
C        write (*,*) 'diameter ', data(1)
C        write (*,*) 'thickness ug/cm^2 ', data(2)
C        write (*,*) 'thickness cm ', data(2)/2250.
C        write (*,*) 'holder name ', kname
C        write (*,*) 'hole name ', lname
C     First make foil holder (Aluminium) 10cm x 10 cm x 0.1 cm
         shape(1) = 5.
         shape(2) = 5.
         shape(3) = 0.05
         CALL gsvolu(kname,'BOX ',6,shape,3,ivol)
         CALL gsatt(kname,'SEEN',1)
C     Position a hole (vacuum) into the holder
         shape(1) = 0.
         shape(2) = data(1)                !! radius in cm   
         shape(3) = 0.05
         CALL gsvolu(lname,'TUBE',1,shape,3,ivol)
         CALL gsatt(lname,'SEEN',1)
         CALL gspos(lname,1,kname,0.0,0.0,0.0,irot,'ONLY')
C     Position the MCP foil into the holder
         shape(1) = 0.
         shape(2) = data(1)             !! radius in cm 
         shape(3) = data(2)/(2250.)      !!  microgram/cm**2 / 2.25 g/cm**3 x 1000
         CALL gsvolu(rname,'TUBE',24,shape,3,ivol)
         CALL gsatt(rname,'SEEN',1)
         CALL gspos(rname,1,lname,0.0,0.0,0.0,irot,'ONLY')
C     Position MCP at posn
         CALL gspos(kname,1,'WRLD',pos(1),pos(2),pos(3),irot,'ONLY')
C.
  999 RETURN
      END
C.
      SUBROUTINE ugeo_fcup(pos,irot)
C.
************************************************************************
*                                                                      *
*                   Define a Farraday Cup                              *
*                                                                      *
************************************************************************
C.
C.
      IMPLICIT none
C.
      INTEGER ivol, irot
C.
      REAL shape(3), pos(3), p1, p2, p3 
C.
      shape( 1) = 0.
      shape( 2) = 2.5
      shape( 3) =   2.
      p1 = pos(1) 
      p2 = pos(2) 
      p3 = pos(3) + shape(3)  
C.      print*, 'posns ', pos(1), pos(2), pos(3), p1, p2, p3
C.
      CALL gsvolu ('FCUP', 'TUBE', 19, shape, 3, ivol)
      CALL gsatt('FCUP','SEEN',1)
C.
      CALL gspos('FCUP',1,'WRLD',p1,p2,p3,irot,'ONLY') 
C.
  999 RETURN
      END
C.



      SUBROUTINE udetmitray
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     UDETMITRAY is a user routine which defines certain volumes to be C
C     detectors and defines what information each detector collects.   C
C     For convenience the detector type variable (IDTYPE) is used      C
C     for some volumes:                                                C
C                                                                      C
C                       1 - Detector Crystal                           C
C                       2 - PM Tube                                    C
C                       3 - Silicon                                    C
C                       4 - Slits/Collimators                          C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      include 'geometry.inc'		! local
C.
      INTEGER i, idtyp, iset, idet
C.
      INTEGER nbitsv
C.
      CHARACTER*4 name
C.
      INTEGER nhdim
      PARAMETER (nhdim=5)
C.
      CHARACTER*4 chnamh(nhdim)
C.
      INTEGER nbitsh_dedx, nbitsh_time, nbitsh_coor, nbitsh(nhdim)
C.
      REAL orig_dedx, orig_time, orig_coor, orig(nhdim)
      REAL fact_dedx, fact_time, fact_coor, fact(nhdim)
C.
      DATA nbitsh_dedx, nbitsh_time, nbitsh_coor / 32  ,    32,    32 /
      DATA orig_dedx,   orig_time,   orig_coor  / 0.  , 1000.,  500.  /
      DATA fact_dedx,   fact_time,   fact_coor  / 1.E6,  1.E5, 1000.  /
C.
************************************************************************
*                                                                      *
*                          Detector DSSD              		       *
*                                                                      *
************************************************************************
C.
      idtyp = 3
C.
      chnamh(1) = 'X   '
      chnamh(2) = 'Y   '
      chnamh(3) = 'Z   '
      chnamh(4) = 'TIME'
      chnamh(5) = 'EDEP'
C.
      CALL ufill(nbitsh,1,3,nbitsh_coor)
      nbitsh(4) = nbitsh_time
      nbitsh(5) = nbitsh_dedx
C.
      CALL ufill(orig,1,3,orig_coor)
      orig(4) = orig_time
      orig(5) = orig_dedx
      DO i = 1, 5
       orig(i) = 0.
      ENDDO
C.
      CALL ufill(fact,1,3,fact_coor)
      fact(4) = fact_time
      fact(5) = fact_dedx
      DO i = 1, 5
       fact(i) = 1.
      ENDDO
C.
      nbitsv = 10
C.
      name = 'ENDV'
      CALL gsdet('DSSD',name,1,name,nbitsv,idtyp,100,0,iset,idet)
      CALL gsdeth('DSSD',name,5,chnamh,nbitsh,orig,fact)

      RETURN
      END







