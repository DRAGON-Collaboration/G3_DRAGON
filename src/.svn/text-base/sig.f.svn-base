c----67---- (p,g) cross-section

      real function sig(e)
      implicit real (a-h,o-z)

      real m1,m2,mprod,z1,z2,zprod,er,gp,gg,omg,ell,e

      common /parameters/ m1,m2,mprod,z1,z2,zprod,
     &                  er, gp, gg, omg, ell


      

      cv = 931.494



      pi = 3.141592654

      

      rm = (m1/cv)*(m2/cv)/((m1/cv)+(m2/cv))

c     total width
      gt = gg + gp

c     sommerfeld parameter (on resonance)
      etar = 0.1575d0*z1*z2*sqrt(rm/er)

c     wavenumber (on resonance)
      wnr = 0.219d0*sqrt(rm*er)

c     BW cross-section (on resonance)
      sigr = (omg*pi/(wnr**2))*gp*gg/((0.5d0*gt)**2)

      sigr = sigr/100.d0 !! into barns

c     S-Factor (on resonance)
      srr = sigr*er*exp(2.d0*pi*etar) 

c----67---- energy dependent part

       eta = 0.1575d0*z1*z2*sqrt(rm/e)
       wn = 0.219d0*sqrt(rm*e)

!      Equations 10 P. Decrock et al, Phys. Rev. C 48 (1993), 2057
       gge = gg*(q+e)**(2.d0*ell+1.d0)/((q+er)**(2.d0*ell+1.d0))
c       gpe = gp*dexp(-dsqrt(eg/e))/(dexp(-dsqrt(eg/er)))
       gpe = gp*exp(-2.d0*pi*(eta-etar))
       
       gte = gge+gpe

       d1 = exp(2.d0*pi*eta)*gpe*gge*(0.5d0*gt)**2
       d2 = exp(2.d0*pi*etar)*gp*gg*((e-er)**2+(0.5d0*gte)**2)
       d3 = d1/d2
       sr = srr*d3

       sig = (omg*pi/(wn**2))*gpe*gge/((e-er)**2+(0.5d0*gte)**2)
       sig=sig/100.
       sn2 = sig*e*exp(2.d0*pi*eta)
       

!      Equation 15 P. Decrock et al, Phys. Rev. C 48 (1993), 2057
       
c       snr = c**2 * s * 3.44d-04 * exp( -0.605d0 * e )
       snr = 0.

c       print*, eta,wn,gge,gpe,gte,sr

!      convert into keV.barns

       snr = snr*1000.d0
       sr = sr*1000.d0

!      interference phase factor

       del = atan( gte/(2d0*(e-er)) )
       if (del.gt.0d0) del = pi-del

!      addition of resonant & non-resonant parts

       se = sr + snr + 2d0*sqrt(sr*snr)*cos(del) !! keV.b

       sig = se*exp(-2.d0*pi*eta)/(e*1000.) !! barns

       end
