C.
      SUBROUTINE guout
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine called at the end of each event             *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      CALL guout_mitray
C.
      CALL guout_gbox
C.
      CALL gfhsta
C.
      RETURN
      END
C.
      SUBROUTINE guout_mitray
C.
      IMPLICIT none
C.
      REAL theta, phi, pbeam, dir0(3)
C.
      INTEGER nout
      DATA nout / 0 /
C.
      CHARACTER*2 label(3)
      DATA label /'TA','RG','ET'/
C.
      INTEGER i, ntbeam, nttarg, nubuf
      INTEGER jv, nt, it, itr, ipart, ivert
      INTEGER nwbuf / 0 /
C.
      REAL vert(3), pvert(4), toff, amass, ubuf(6)
C.
      include 'gcbank.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'gcnum.inc'           !geant
C.
      include 'diagnostic.inc'      !local
C.
      Do i = 1, nvertx
C.
         CALL gfvert(i,vert,ntbeam,nttarg,toff,ubuf,nubuf)
C.
         jv = lq(jvertx-i)
         nt = q(jv+7)
C.
         If(nubuf.eq.0)then				! Start vertex
C.
           Do it = 1, nt
C.
              itr = q(jv+7+it)
              CALL gfkine(itr,vert,pvert,ipart,ivert,0,nwbuf)
C.
              If(ipart.eq.61)then                       ! it is an ion
                CALL ucopy(pvert(1),dir0(1),3)
                goto 100
              Endif
C.
           Enddo
C.
         Endif
C.
      Enddo
C.
  100 Continue
C.
      CALL vunit(dir0(1),dir0(1),3)
C.
      theta = 0.0
      If(dir0(1).ne.0.0.or.dir0(3).ne.0.0)then
         theta = 1000.*atan2(dir0(1),dir0(3))
      Endif
      phi = 1000.*asin(dir0(2))
C.
      If(istop.eq.100)then
C.
        nout  = nout  + 1
        idevt = idevt + 1
C.
        pbeam = sqrt(pvert(1)**2+pvert(2)**2+pvert(3)**2)
        amass = q(lq(jpart-ipart)+7)
C.
        WRITE(lout,111)
     &                vert(1),theta,vert(2),phi,pbeam/1.E3,
     &                amass,1.0,1.0,vert(3)/1.E2,0.0,0.0,
     &                ievent,(label(i),i=1,3)
  111   FORMAT (1X, F10.6,F10.4,F10.6,F10.4,F12.8,
     &              F11.6,2E11.3,F9.4,E12.4,F9.4 , I8, 1X, 3A2)
C.
      Endif
C.
      If(istop.eq.100.or.istop.eq.200)then
C.
        If(jslit.gt.0)write(lout,*)' SLIT! ',ievent,nrndm(1),nrndm(2)
C.
        CALL hfill( 5,vert(1),0.0,1.0)
        CALL hfill( 6,vert(2),0.0,1.0)
        CALL hfill( 7,theta  ,0.0,1.0)
        CALL hfill( 8,phi    ,0.0,1.0)
C.
        pbeam = sqrt(pvert(1)**2+pvert(2)**2+pvert(3)**2)
        pbeam = 100.*(pbeam/0.25855443-1.0)
C.
        CALL hfill(10,pbeam   ,0.0,1.0)
C.
        CALL hfill(103,vert(1),vert(2),1.0)
        CALL hfill(104,theta,phi,1.0)
        CALL hfill(107,vert(1),theta,1.0)
        CALL hfill(108,vert(2),phi  ,1.0)
C.
      Endif
C.
      RETURN
      END
C.
      SUBROUTINE guout_gbox
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gcnum.inc'           !geant
      include 'gconst.inc'          !geant
C.
      include 'geometry.inc'   	    !local
      include 'uenergy.inc'	    !local
      include 'ev_info.inc'	    !local
      include 'rescom.inc'      !local
C.
      INTEGER i, ntbeam, nttarg, nubuf
      INTEGER jv, nt, it, itr, ipart, ivert
      INTEGER nwbuf / 0 /
C.
      REAL vert(3), pvert(4), tofg, ubuf(6)
C.
      REAL egamma, gamma(3)
      REAL vmod, vdotn
C.
      REAL dist, theta
C.      
C.                Information on the photons
C.                --------------------------
C.
C.    true_e: The true energy of photon [MeV]
C.    true_d: The true polar angle of photon [deg]
C.
C.            Information on the photon conversion point (i)
C.            ----------------------------------------------
C.
      INTEGER n_gamma, nconv
C.
C.    Index array relating in the event ITRA -> n_gamma
C.    -------------------------------------------------
C.
C.    index_track_to_gamma: ITRA -> n_gamma
C.
      INTEGER imax, jmax, jord
C.
      n_gamma = 0
C.
      CALL vzero(index_track_to_gamma,max_itra)
C.
C.    vname(i)   : The full name of the conversion volume 
C.    ivcopy(2,i): The volume copy number of the conversion volume
C.    true_conv(3,i): The coordinates of the conversion piont [cm]
C.
      egamma = 0.0
C.
      CALL vfill(true_e,max_photon,-999.0)
      CALL vfill(true_d,3*max_photon,-999.0)
C.
      nconv = 0
      CALL ublank(vname,1,max_conv)
      CALL vfill(ivcopy,2*max_conv,-999)
      CALL vfill(true_conv,3*max_conv,-999.0)
C.
      Do i = 1, nvertx
C.
         CALL gfvert(i,vert,ntbeam,nttarg,tofg,ubuf,nubuf)
C.
         jv = lq(jvertx-i)
         nt = q(jv+7)
C.
         If(nubuf.eq.0)then				! Start vertex
C.
           Do it = 1, nt
C.
              itr = q(jv+7+it)
              CALL gfkine(itr,vert,pvert,ipart,ivert,0,nwbuf)
C.
!              write(6,*)"track",it,"part",ipart,"ngamma",n_gamma,"max_p",max_photon
              If(ipart.eq.1)then                         ! it is a gamma
                CALL ucopy(pvert(1),gamma(1),3)
                egamma = vmod(gamma,3)
                CALL vunit(gamma(1),gamma(1),3)
                If(n_gamma + 1 .le. max_photon) then
                  n_gamma = n_gamma + 1
                  true_e(n_gamma) = 1000. * egamma
                  true_d(3,n_gamma) = raddeg * acos(gamma(3))
                  If(itr.le.max_itra)index_track_to_gamma(itr) = n_gamma
                  CALL hfill(21,true_e(n_gamma)  ,0.0,1.0)
                  CALL hfill(22,true_d(3,n_gamma),0.0,1.0)
                  CALL hfill(24,tofg*1.e9, z_react ,1.0)
!                  write(6,*) tofg*1.e9, z_react   
                  CALL ucopy(gamma(1),true_d(1,n_gamma),3)
                Endif !n_gamma
              Endif  !ipart
C.
           Enddo
C.
         Endif
C.
         If(nubuf.eq.6.and.int(ubuf(1)).eq. 1)then	! photon conv. vertex
C.
           If(nconv + 1 .le. max_conv)then
             nconv = nconv + 1
             CALL uhtoc(iq(jvolum+int(ubuf(3))),4,vname(nconv),4)
             ivcopy(1,nconv) = int(ubuf(4))
             ivcopy(2,nconv) = int(ubuf(5))
             CALL ucopy(vert(1),true_conv(1,nconv),2)
             true_conv(3,nconv) = ubuf(6)
             CALL hfill(23,1.*ivcopy(2,nconv)+0.5,0.0,1.0)
           Endif
C.
           Do it = 1, nt
              itr = q(jv+7+it)
              If(itr.le.max_itra)then
                index_track_to_gamma(itr) = index_track_to_gamma(ntbeam)
              Endif
           Enddo
C.
         Endif
C.
      Enddo
C.
C      If(ntot.eq.0)goto 999
C      If(edetect.le.tot_thrshld)goto 999
C.
      z_conv = -999.0
      Do it = 1, nconv
         If(z_conv.eq.-999.0)then
           x_conv = true_conv(1,it)
           y_conv = true_conv(2,it)
           z_conv = true_conv(3,it)
         Endif
      Enddo
C.
      If(x_conv.ne.-999.0.and.
     &   y_conv.ne.-999.0.and.
     &   z_conv.ne.-999.0)then
           CALL hfill( 46,z_conv,0.0,1.0)
C.
           dist=sqrt((x_conv-x_max)**2+
     &               (y_conv-y_max)**2)
           CALL hfill( 48,dist,0.0,1.0)
           dist=sqrt((x_conv-x_max)**2+
     &               (y_conv-y_max)**2+
     &               (z_conv-z_max)**2)
           CALL hfill( 49,dist,0.0,1.0)
           CALL hfill(121,x_conv,y_conv,1.0)
      Endif
      CALL hfill( 47,z_max,0.0,1.0)
      dist=sqrt((x_mean-x_max)**2+(y_mean-y_max)**2)
      CALL hfill( 50,dist,0.0,1.0)
C.
      CALL hfill( 60,x_mean,0.0,1.0)
      CALL hfill( 61,y_mean,0.0,1.0)
      CALL hfill( 62,z_mean,0.0,1.0)
C.
      CALL hfill(122,x_max ,y_max ,1.0)
      CALL hfill(123,x_mean,y_mean,1.0)
      CALL hfill(124,x_fngr(ifngr_max),y_fngr(ifngr_max),1.0)
      CALL hfill(125,z_fngr(ifngr_max),x_fngr(ifngr_max),1.0)
      CALL hfill(126,z_fngr(ifngr_max),y_fngr(ifngr_max),1.0)
C.
      Do it = 1, n_gamma
         If(nclu.ge.it)then
           If(it.le.3)then
             CALL hfill(93+it,true_e(it)-eclu(it),0.0,1.0)
             theta = raddeg*acos(vdotn(true_d(1,it),dir_clu(1,it),3))
             CALL hfill(96+it,theta,0.0,1.0)
           Endif
         Endif
      Enddo
C.
  999 Continue
C.
      If (idebug .ne. 0 .and. iswit(1).eq. 1) then
         CALL gprint('VERT',0)
         CALL gprint('KINE',0)
      Endif
C.
      RETURN
      END
C.
