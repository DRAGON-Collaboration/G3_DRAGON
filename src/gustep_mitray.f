C.
      SUBROUTINE gustep_mitray
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gcking.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gctmed.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gccuts.inc'          !geant
      include 'gcnum.inc'           !geant
      include 'gcunit.inc'          !geant
      include 'gcsets.inc'          !geant
C.
      include 'geom_dipole.inc'     !local
      include 'geom_edipol.inc'     !local
      include 'geom_mpole.inc'      !local
      include 'geom_sole.inc'       !local
C.
      include 'mitray_diag.inc'     !local
      include 'diagnostic.inc'      !local
      include 'ukine.inc'           !local
      include 'beamcom.inc'
      include 'dsssd.inc'
      include 'rescom.inc'
      include 'uevent.inc'
      include 'cntrs.inc'
      include 'history.inc'
      include 'gammahit.inc'
C.
      INTEGER i, j, k, irot, kstop, ihit
      INTEGER JPAA,JPAB,JDKA,JDKB
      REAL radius, dr, theta, xlo, xhi, trec, hits(5)
C.
      CHARACTER*20 chtmed
      CHARACTER* 4 chname_nlevel
      CHARACTER* 4 chcase
      CHARACTER* 1 kdname
C
      INTEGER in_new_vol, name_old, number_old, ntmult_old
      DATA name_old / 0 /, number_old / 0 /, ntmult_old / 0 /
C.
      REAL xm(3), xd(3), xd_endv(3), xdd_endv(3)
C.
      REAL amugev, tlast, Zrec, a0, a1, aa, b0, b1, bb, phd, tphd
      REAL tof, x_mcp, y_mcp, z_mcp, disp, tflight

      amugev = 0.93149432E+00
      kstop = 0
C.
C *** Because INWVOL = 1 can mean either that a new volume has been 
C *** entered or that a new track has been started, define a new 
C *** variable IN_NEW_VOL which specifically indicates a new volume.
C.
      CALL uhtoc(natmed(1),4,chtmed,20)
      CALL uhtoc(names(nlevel),4,chname_nlevel,4)
      in_new_vol = 0
      If(inwvol.eq.1)then
         If(name_old  .ne.names(nlevel).or.
     &        number_old.ne.number(nlevel))then
            If(ntmult.eq.ntmult_old)then 
               in_new_vol = 1
cc mt            If(ntmult.eq.ntmult_old)in_new_vol = 1
               if( chname_nlevel.eq.'ENDV' .and.ipart .eq. irecoil )
     &              nend =nend+1
cc mt           If( chname_nlevel .eq. 'C7  '.and. ipart .eq.irecoil) 
               If( chname_nlevel .eq. 'C7  '.and. ipart .eq. 80) 
     &              nfcm2 = nfcm2 + 1
C  MT adds counters for the number of recoils that make it
C     to Q3 (Sext 1) and to Q8 (Quad 6).
               if( chname_nlevel .eq. 'Q3' .and. ipart .eq. irecoil )
     &              Num_Recoils_Q3 = Num_Recoils_Q3 + 1
               if( chname_nlevel .eq. 'Q8' .and. ipart .eq. irecoil )
     &              Num_Recoils_Q8 = Num_Recoils_Q8 + 1 
C  MT adds counter for the number of beam particles that reach
C     the end detector.
               if( chname_nlevel .eq. 'ENDV' .and. ipart .eq. 80 )
     &              Num_BeamPart_ENDV = Num_BeamPart_ENDV + 1

     &           
            Endif
         Endif
      Endif
C.

C.      CALL uhtoc(kcase,4,chcase,4)
C.
      If(sleng.gt.len_max)then
        istop = 6
        goto 999
      Endif
C
C *** Change beam particle charge state to the same as the recoil
C
      
c      If(ipart.eq.80) then
c       JPAA = LQ(JPART-IPART)  !! pointer to beam particle
c       print*, Q(JPAA+10), FKINE(2)
c       Q(JPAA+10) = FKINE(2)
c      Endif


C
C *** Calculate recoil (or beam) kinetic energy
C
      If(ipart.eq.irecoil) then
       If(in_new_vol.eq.1)then
        tlast = 1000.*(sqrt(prodm**2+vect(7)**2)-prodm)
       Endif
      trec = sqrt( prodm**2 + vect(7)**2 ) - prodm
      Else If(ipart.eq.80) then
      trec = sqrt( beammass**2 + vect(7)**2 ) - beammass
      Endif
      trec = trec*1000.

C
C *** MCP hit to determine start of TAC
C     If the current volume is 'MCP0' and we are leaving
C     that volume, then for the recoil and the beam both
C     calculate the time of flight using the particle's 
C     mass and energy. Record the current coordinates.
C     Note: trec [GeV], mass [GeV/c**2] -> units of 
C     1/c for time of flight - so have to correct by 'clight'. 
      If(chname_nlevel.eq.'MCP0'.and.inwvol.eq.2)then
         If(ipart.eq.irecoil)then
            tof = sqrt( 0.5*prodm/(trec/1000.) )
            tof = tof/clight
         ElseIf(ipart.eq.80)then
            tof = sqrt( 0.5*beammass/(trec/1000.) )
            tof = tof/clight
         EndIf
         McpHit = .TRUE.
         x_mcp = vect(1)
         y_mcp = vect(2)
         z_mcp = vect(3)
      EndIf
            
         
C *** If the current volume is 'MCP1' and MCP0 was hit, calculate distance travelled, time-of-flight
C     between MCPs. 
      If(chname_nlevel.eq.'MCP1'.and.inwvol.eq.2)then
        If(McpHit)then
           disp = sqrt( (vect(1)-x_mcp)**2 + (vect(2)-y_mcp)**2 +
     +          (vect(3)-z_mcp)**2 )
           If(tof.ne.0.0)then
              tflight = tof * disp
              tflight = tflight * 1.E+09 !! into nanoseconds
C              write (*,*) 'ToF ' , tflight
              CALL hfill(518,tflight,0.0,1.0)
           EndIf
        EndIf
      EndIf   


C          
C
C *** If particle is escaped beam from ED1
C
      If(ipart.eq.80 .and. chname_nlevel.eq.'D1  ' 
     + .and. inwvol.eq.2 ) then
       CALL hfill(503,1.,0.0,1.0)
      Endif


C
C *** If recoils are stopped
C
      If(ipart.eq.irecoil .and. istop.ne.0 .and.
     +   chname_nlevel.ne.'ENDV') then
       CALL hfill(504,vect(3),vect(1),1.0)
       xstop = vect(1)
       ystop = vect(2)
       zstop = vect(3)
C       print*, 'recoil disappeared!', vect(1), vect(2), vect(3)
        write(4,*) vect(1), vect(2), vect(3), tlast 
C       write(4,*) "recoil stopped mit",vect(1),vect(2),vect(3),tlast
C       write(4,*) sleng, "  volume  ", chname_nlevel
      Endif

C     alpha acceptance tests - plot x-y coords at Q1 fringe field start
      If(alpha .and. ipart.eq.irecoil .and. chname_nlevel.eq.'Q1' .and.
     &     in_new_vol.eq.1)then
         CALL hfill(522,vect(1),vect(2),1.0)
         dsssdpos = 1
         xtest(dsssdpos) = vect(1)
         ytest(dsssdpos) = vect(2)
         etest(dsssdpos) = trec
         write(20,*) vect(1), " ", vect(2), " ", vect(3), " ",  trec
      endif
      If(alpha .and. ipart.eq.irecoil .and. chname_nlevel.eq.'TST1'.and.
     &     in_new_vol.eq.1)then
         dsssdpos = 2
         xtest(dsssdpos) = (vect(1)+68.5685349)*cos(50.*3.1415926/180.)
     +        +            (vect(3)-359.253174)*sin(50.*3.1415926/180.)
         ytest(dsssdpos) = vect(2)
         etest(dsssdpos) = trec 
         write(21,*) (vect(1)+68.5685349)*cos(50.*3.1415926/180.)
     +    +(vect(3)-359.253174)*sin(50.*3.1415926/180.), " " , vect(2),
     +    " " , trec

      endif
      If(alpha .and. ipart.eq.irecoil .and. chname_nlevel.eq.'TST2'.and.
     &     in_new_vol.eq.1)then
         dsssdpos = 3
         xtest(dsssdpos) = (vect(1)-(-509.712341+8.1*cos(20*3.1415926/ 
     +   180.)))*cos(70.*3.1415926/180.) + (vect(3)-(661.428772-8.1* 
     +   sin(20.*3.145926/180.)))*sin(70.*3.1415926/180.)

         ytest(dsssdpos) = vect(2)
         etest(dsssdpos) = trec
         
         write(23,*) (vect(1)-(-509.712341+8.1*cos(20*3.1415926/ 
     +   180.)))*cos(70.*3.1415926/180.) + (vect(3)-(661.428772-8.1* 
     +   sin(20.*3.145926/180.)))*sin(70.*3.1415926/180.), " ", vect(2), 
     +    " ", trec

      endif
      If(alpha .and. ipart.eq.irecoil .and. chname_nlevel.eq.'TST3'.and.
     &     in_new_vol.eq.1)then
         dsssdpos = 4
         xtest(dsssdpos) = (vect(1)+1024.617632)
         ytest(dsssdpos) = vect(2)
         etest(dsssdpos) = trec

         write(24,*) (vect(1)+1024.617632), " ", vect(2), " ", trec

      endif
C.
C *** If particle is in ENDV volume
C.
      If(chname_nlevel.eq.'ENDV' .and. in_new_vol.eq.1 )then
C. JS adjusts flag recoil_hit_ENDV to 1 when recoil reaches end detector
         If(ipart.eq.irecoil) then
            recoil_hit_ENDV = 1
            recdet = 1
         
            CALL hfill(510, 0.5, 0.0, 1.0)
         EndIf
        
C.
        CALL ucopy(vect(1),xm(1),3)
        CALL gmtod(xm,xd_endv,1)
        CALL ucopy(vect(4),xm(1),3)
        CALL gmtod(xm,xdd_endv,2)
C.
        If(iswit(8).eq.1)nevent = ievent
C.
        If(xdd_endv(1).ne.0.0.or.xdd_endv(3).ne.0.0)then
          xdd_endv(1) = 1000.*atan2(xdd_endv(1),xdd_endv(3))
        Else
          xdd_endv(1) = 0.0
        Endif
        xdd_endv(2) = 1000.*asin(xdd_endv(2))
C.
        hits(1) = vect(1)
        hits(2) = vect(2)
        hits(3) = vect(3)
        hits(4) = 0.
        hits(5) = trec

C     Calculate polar angle of recoil in lab
C     note: vect(6) is equal to px/P, i.e is the UNIT vector component
C     of the momentum in the z-direction. In order to calculate the 
C     polar angle of the recoil in the plave of the end detector, we 
C     take into account the fact that the normal vector to the plane 
C     of the detector, in the direction of ions travelling along the 
C     optical axis, is exactly antiparallel to the z-axis (though 
C     obviously not collinear). Therefore, the polar angle is given 
C     by the inverse cosine of the negative unit z-momentum:
        theta = ACOS( -vect(6) )

C     Extract the charge (atomic number) of the incident ion
C     in order to calculate the Pulse Height Defect
        JPAA = LQ(JPART-IPART)
        Zrec = Q(JPAA+8)

C     Calculate the Pulse Height Defect
C     
        a0 = 0.804
        a1 = 1.13E-04
        b0 = -0.462
        b1 = -1.625
        aa = a0 + a1*Zrec**2
        bb = b0 + b1/Zrec
        phd = 10.**bb*trec**aa

        tphd = trec - phd
        
C.        CALL gsahit(iset,idet,itra,numbv,hits,ihit)
C.
        radius = sqrt(xd_endv(1)**2+xd_endv(2)**2)
C        dr = sqrt(1.-vect(6)**2)
C.
C        theta = 0.0
C        If(dr.ne.0.0.or.vect(6).ne.0.0)then
C          theta = 1000.*atan2(dr,vect(6))
C        Endif

C
C *** If MCP was hit, calculate distance travelled, time-of-flight
C     between MCP and DSSSD. 
        If(McpHit)then
           disp = sqrt( (vect(1)-x_mcp)**2 + (vect(2)-y_mcp)**2 +
     +          (vect(3)-z_mcp)**2 )
           If(tof.ne.0.0)then
              tflight = tof * disp
              tflight = tflight * 1.E+09 !! into nanoseconds
              CALL hfill(517,tflight,0.0,1.0)
           EndIf
        EndIf

C.
C.      DSSSD hit-pattern
        nstrip = 16
        pitch = 0.3
        do i = 1, nstrip
         xlo = -(float(nstrip)/2.)*pitch + float(i-1)*pitch
         xhi = -(float(nstrip)/2.)*pitch + float(i)*pitch
         if (xd_endv(1).ge.xlo.and.xd_endv(1).lt.xhi) then
          CALL hfill(401,float(i),0.0,1.0)
         endif
         if (xd_endv(2).ge.xlo.and.xd_endv(2).lt.xhi) then
          CALL hfill(402,float(i),0.0,1.0)
         endif
        enddo
C.
        CALL hfill(11, xd_endv(1),0.0,1.0)
        CALL hfill(12, xd_endv(2),0.0,1.0)
        CALL hfill(13,xdd_endv(1),0.0,1.0)
        CALL hfill(14,xdd_endv(2),0.0,1.0)
        CALL hfill(15,trec,0.0,1.0)
        CALL hfill(516,tphd,0.0,1.0)
        CALL hfill(19,gekin*1000.,0.0,1.0)
C.
c MT histograms
c
        CALL hfill(310, xd_endv(1), trec, 1.0)
        CALL hfill(311, trec, xd_endv(1), 1.0)


        CALL hfill(111, xd_endv(1), xd_endv(2),1.0)
        CALL hfill(112,xdd_endv(1),xdd_endv(2),1.0)
        CALL hfill(113, xd_endv(1),xdd_endv(1),1.0)
        CALL hfill(114, xd_endv(2),xdd_endv(2),1.0)
C.
        CALL hfill(115,radius,theta,1.0)

        If(iswit(7).eq.2)then
C.
          kstop = 1
          istop = 100
C.
        Elseif(iswit(7).eq.3)then
C.
          idevt = idevt + 1

          WRITE(lout,111)jevent,
     &                   xd_endv(1),xdd_endv(1),
     &                   xd_endv(2),xdd_endv(2),
     &                   100.*(vect(7)/recoilmom*1000.-1.0),xd(3)
  111     FORMAT(1X,I7,F10.6,F10.4,F10.6,F10.4,F12.8,F12.6)
C.
          kstop = 1
          istop = 200
C.
        Elseif(iswit(7).eq.1)then
C.
c$$$          write(lout,*)' x_final: ',xd_endv(1),
c$$$     &                 ' y_final: ',xd_endv(2)
c$$$          write(lout,*)' theta_final: ',xdd_endv(1),
c$$$     &                 ' phi_final: ',xdd_endv(2)
          kstop = 1
          istop = 200
C.
        Endif
C.
        goto 999
C.
      Elseif(chtmed.eq.'COPPER              ')then
C.
C *** If particle is in COPPER (jaws, slits)
C.
        jslit = 1
C.
        If(iswit(7).eq.1)then
          CALL hfill(16,sleng,0.0,1.0)
c
      if(ipart.eq.irecoil)then
         CALL hfill(300,sleng,0.0,1.0)
      endif
      if(ipart.eq.80)then
         CALL hfill(301,sleng,0.0,1.0)
      endif
c 
         kstop = 1
          istop = 5
          goto 999
        Endif
C.
      Endif 
C.
      If(chname_nlevel.eq.'ENDV')goto 1111
      If(chname_nlevel.eq.'STRV')goto 1111
      If(chname_nlevel.eq.'DEAD')goto 1111
c
c
c  MT adds histograms
c
c
c      if(ipart.eq.irecoil .and. istop.ne.0)then
c         CALL hfill(300,sleng,0.0,1.0)
c      endif
c      if(ipart.eq.80 .and. istop.ne.0)then
c         CALL hfill(301,sleng,0.0,1.0)
c      endif
c
c
c

C.
C *** Check collimators in all RAYTRACE elements
C.
      If(in_new_vol.eq.1.or.inwvol.eq.2)then
        If(in_new_vol.eq.1)j = 1
        If(  inwvol  .eq.2)j = 2
        k = number(nlevel)
        CALL uhtoc(names(nlevel),4,kdname,1)
        CALL ucopy(vect(1),xm(1),3)
        CALL gmtod(xm,xd,1)
        If(kdname.eq.'D')then
          irot = irot_dipole(k)
          CALL gitran(xd,dx_dipole(1,k),irot,xd)
          If(in_new_vol.eq.1)then
           If(jcol_dipole(j,k).eq.1)then
             If((xd(1)-xcol_dipole(j,k))**2/dxcol_dipole(j,k)**2 +
     &       (xd(2)-ycol_dipole(j,k))**2/dycol_dipole(j,k)**2.gt.1.0)
     &                                                      istop = 3
           Else
ccc            If(abs(xd(1)-xcol_dipole(j,k)).gt.dxcol_dipole(j,k))istop=3
            If(abs(xd(2)-ycol_dipole(j,k)).gt.dycol_dipole(j,k))istop=3
           Endif
          Else
            If(abs(xd(2)-ycol_dipole(j,k)).gt.dycol_dipole(j,k))istop=3
          Endif

        Elseif(kdname.eq.'Q')then
          irot = 0
          CALL gitran(xd,dx_mpole(1,k),0,xd)
          If(jcol_mpole(j,k).eq.1)then
            If((xd(1)-xcol_mpole(j,k))**2/dxcol_mpole(j,k)**2 +
     &         (xd(2)-ycol_mpole(j,k))**2/dycol_mpole(j,k)**2.gt.1.0)
     &                                                        istop = 3
          Else
            If(abs(xd(1)-xcol_mpole(j,k)).gt.dxcol_mpole(j,k))istop=3
            If(abs(xd(2)-ycol_mpole(j,k)).gt.dycol_mpole(j,k))istop=3
          Endif
        Elseif(kdname.eq.'S')then
          irot = 0
          CALL gitran(xd,dx_sole(1,k),0,xd)
          If(jcol_sole(j,k).eq.1)then
            If((xd(1)-xcol_sole(j,k))**2/dxcol_sole(j,k)**2 +
     &         (xd(2)-ycol_sole(j,k))**2/dycol_sole(j,k)**2.gt.1.0)
     &                                                        istop = 3
          Else
            If(abs(xd(1)-xcol_sole(j,k)).gt.dxcol_sole(j,k))istop=3
            If(abs(xd(2)-ycol_sole(j,k)).gt.dycol_sole(j,k))istop=3
          Endif
          CALL gmtod(xm,xd,1)
        Endif
      Endif

       If(in_new_vol.eq.1 .or. inwvol.eq.2)then
        If(kdname.eq.'E')then
          irot = irot_edipol(k)
          CALL gitran(xd,dx_edipol(1,k),irot,xd)
          If(in_new_vol.eq.1)then
           If(jcol_edipol(j,k).eq.1)then
             If((xd(1)-xcol_edipol(j,k))**2/dxcol_edipol(j,k)**2 +
     &       (xd(2)-ycol_edipol(j,k))**2/dycol_edipol(j,k)**2.gt.1.0)
     &                                                      istop = 3
           Else
            If(abs(xd(1)-xcol_edipol(j,k)).gt.dxcol_edipol(j,k))istop=3
            If(abs(xd(2)-ycol_edipol(j,k)).gt.dycol_edipol(j,k))istop=3
           Endif
          Else
            If(abs(xd(2)-ycol_edipol(j,k)).gt.dycol_edipol(j,k))istop=3
          Endif
        Endif
       Endif


      If(istop.eq.3)then
        kstop = 1
        CALL hfill(16,sleng,0.0,1.0)
c
      if(ipart.eq.irecoil)then
         CALL hfill(300,sleng,0.0,1.0)
      endif
      if(ipart.eq.80)then
         CALL hfill(301,sleng,0.0,1.0)
      endif
         write(4,*) vect(1),vect(2),vect(3),tlast
C        write(4,*) "stopped mit", vect(1),vect(2),vect(3),tlast
C        write(4,*) sleng, "  volume  ", chname_nlevel
        CALL hfill(17,sqrt(xd(1)**2+xd(2)**2),sleng,1.0)
      Endif
C
 1111 Continue
C.
C *** Daughter particles that were generated in the current step
C ***                  are put on the stack
C.
      If(ngkine.gt.0)then
C.
        CALL uhtoc(kcase,4,chcase,4)
C.
        Do i = 1, ngkine
C.
           iflgk(i) = 0
C.
           If(chcase.eq.'DCAY')then
               iflgk(i) = 1
           Endif
C.
           If(gkin(5,i).eq.4)iflgk(i) = -1
C.
        Enddo
C.
	CALL gsking(0)
C.
      Endif
C.
  999 Continue
C.
C *** Debug/plot event
C.
      CALL gdebug
C      If(itrtyp.eq.8)Call gdebug
C.
      If(jstop.ne.0)then
        istop = 1
        kstop = 1
        CALL hfill(16,sleng,0.0,1.0)
c
      if(ipart.eq.irecoil)then
         CALL hfill(300,sleng,0.0,1.0)
      endif
      if(ipart.eq.80)then
         CALL hfill(301,sleng,0.0,1.0)
      endif
c
        write(6,*)' *** Problem!!! *** '
      Endif
C      If(kstop.eq.0.and.istop.ne.0)then
      if(alpha)then 
         continue
      elseIf(istop.ne.0 .and. iswit(1) .eq. 1)then
        write(6,*)' Whats stopping me??? (in mit)'
        write(6,*)' istop: ',istop,' Volume: ',chname_nlevel,"  ",sleng,
     &            ' ipart: ',ipart
      Endif
      jstop = 0
C.
      ngkine = 0
C.
      name_old   = names (nlevel)
      number_old = number(nlevel)
      ntmult_old = ntmult
C.
      RETURN
      END











