C.
      SUBROUTINE gustep_trgt
C.
      IMPLICIT none
C.
      include 'gcbank.inc'
      include 'gckine.inc'          !geant
      include 'gcking.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gctmed.inc'          !geant
      include 'gcvolu.inc'          !geant
C.
      include 'uevent.inc'          !local
      include 'uggeom.inc'          !local
      include 'rescom.inc'          !local
      include 'beamcom.inc'         !local
      include 'geometry.inc'
      include 'history.inc'
      include 'cntrs.inc'
C.
      INTEGER i,lpart, JPA, JDK
C.
      REAL fstep, vectl(7), amugev, trec, tlast
      PARAMETER (amugev=0.93149432 E+00)
C.
      REAL  xtarg, ytarg, xprime, yprime, deltap, dummy, newm
C.
      CHARACTER* 4 chname_nlevel
      CHARACTER* 4 chcase
      INTEGER JDK1, JDK2, in_new_vol,name_old, number_old, ntmult_old,
     &        ntmult
      DATA name_old / 0 /, number_old / 0 /, ntmult_old / 0 /
C.
      DATA dummy / 0.0 /
C.
      If(itrtyp.ne.8)RETURN
C.
      CALL uhtoc(names(nlevel),4,chname_nlevel,4)
      
      in_new_vol = 0
      If(inwvol.eq.1)then
        If(name_old  .ne.names(nlevel).or.
     &     number_old.ne.number(nlevel))then
             If(ntmult.eq.ntmult_old)in_new_vol = 1
        Endif
C Increment counters
        If(chname_nlevel .eq.'EAPG' .and.ipart .eq. 80) 
cc mt        If(chname_nlevel .eq.'EAPG' .and.ipart .eq. irecoil) 
     &  ngascell = ngascell +1
      Endif
C.
C
C *** Change beam to recoil charge state
C
      tlast = 0.
      if (alpha) then
         continue
      elseIf(chname_nlevel.eq.'EX2G' .and. ipart.eq.80 )then
       Q(LQ(JPART-IPART)+10) = Q(LQ(JPART-IRECOIL)+10)
      Endif 


C.-------------------Check for resonance crossing-----------------------
C.
C. Check that it's in gas volume or in solid target
C      print *,'<<<', lkine,ipart,eres,gekin,destep
C     JS adds CMBG condition for solid target, as no CELG
      If((lkine.gt.0) .and. (ipart.eq.80) .and.
     + (    (targtype.eq.0.and.index(chname_nlevel,'P').eq.1) !pumping tubes
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'UHOL')       ! listing all other volumes containing gas
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'CMBG')
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'EAPG')
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'CELG')
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'XAPG')
     + .or. (targtype.eq.0.and.chname_nlevel.eq.'DHOL')
     + .or. (   targtype.eq.1.and.chname_nlevel.ne.'CMBG'   )   )  )then

        If(gekin.gt.eres )then
          CALL ucopy(vect,vectl,7)
C        Else if (gekin+destep .gt. eres) then
        else 
C           goto 888
          if (destep .gt. 0.0) then
             fstep = (eres-gekin)/destep
             Do i = 1, 7
               vect(i) = vect(i) - fstep * (vect(i)-vectl(i))
             Enddo
           endif
C.
C.
C.--> Set E_int ntuple
  
          E_int = eres*1000.

C.--> Reaction is occurring, so set the value of beamtof to TOFG
          beamtof = tofg

         
C.
C. ***    change mass of resonating particle
C.
          newm = resmass + (erescm-resenerg)/1000.
C          print*,'<<<<', erescm, resenerg, resmass, 
C     +         resmass+(erescm-resenerg)/1000.
          JPA = LQ(JPART-IRES)
          Q(JPA+7) = newm
C          print*, 'Eres (CM) and newmass', erescm, newm
         
         

      

          CALL gureact
          react = 1
          nreact = nreact +1

C.          if (vect(3) .lt. -5 .or. vect(3) .gt. 5) 
C.     +    write(6,*) 'Suspicious! ', vect, gekin,fstep,vectl
C.
          CALL hfill(205,vect(3),0.0,1.0)
          CALL hfill(520,vect(1),0.0,1.0)
          CALL hfill(521,vect(2),0.0,1.0)
          CALL hfill(213,vect(3),sqrt(vect(1)**2+vect(2)**2),1.0)
          xint = vect(1)
          yint = vect(2)
          zint = vect(3)
C.          if (zint.gt.10 .or. zint.lt.-10) print*,chname_nlevel
C.
          istop = 1
C.
          goto 999
C.
        Endif
        
      Endif
 888  continue
C.
C.-------------------Fill collimator histograms-----------------------
C.
C.--> If particle has stopped then...
C.
C.
C. Pumping tube volumes all end with C
      If(index(chname_nlevel,'C') .eq. 4 .and. ipart.eq.irecoil)then
       tlast=1000.*(sqrt(prodm**2+vect(7)**2)-prodm)
      Endif
       
      If(istop.eq.2)then
C.
C.--> If its a recoil....
C.
        IF(ipart.eq.irecoil)then
         CALL hfill(201,vect(3),0.0,1.0)
         write(4,*) vect(1),vect(2),vect(3),tlast
C         write(4,*) "recoil stopped trgt",vect(1),vect(2),vect(3),tlast
C         write(4,*) sleng,"  volume  ",chname_nlevel
         CALL hfill(211,vect(3),sqrt(vect(1)**2+vect(2)**2),1.0)
         CALL hfill(212,vect(3),sqrt(vect(1)**2+vect(2)**2),1.0)
C.
C.--> If its in either target collimator...
C.
C        JS puts in CMBG condition for slid target, as no CELL
         If((targtype.eq.0.and.chname_nlevel.eq.'CELL')
     + .or. (targtype.eq.1.and.chname_nlevel.eq.'CMBG') ) then
          If(vect(3).lt.0.0)then
            CALL hfill(202,sqrt(vect(1)**2+vect(2)**2),0.0,1.0)
          Else
            CALL hfill(203,sqrt(vect(1)**2+vect(2)**2),0.0,1.0)
          Endif
         Endif
C.        
        Endif
C.
C.
C.--> If particle is beam...
C.
        If(ipart.eq.80)then
C.
C.--> If it is either target collimator...
C.
C        JS puts in CMBG condition for solid target, as no CELL
         If((targtype.eq.0.and.chname_nlevel.eq.'CELL')
     + .or. (targtype.eq.1.and.chname_nlevel.eq.'CMBG') ) then
          CALL hfill(206,vect(3),0.0,1.0)
         Endif
C.
        Endif
      istop = 1
C.
      Endif
      If(inwvol.eq.1 .and. chname_nlevel.eq.'TEND') then
C.
      if (ipart .eq. irecoil) ntargexit = ntargexit +1
      if (ipart .eq. 80) nbeamout = nbeamout +1
      

        CALL hfill(204,1.E6*tofg,0.0,1.0)
        CALL hfill(207,1.E3*gekin,0.0,1.0)
        CALL hfill(211,vect(3),sqrt(vect(1)**2+vect(2)**2),1.0)
        CALL hfill(214,vect(1),vect(2),1.0)
        CALL hfill(215,vect(1),vect(4),1.0)
        CALL hfill(216,vect(2),vect(5),1.0)
C.
        xtarg = vect(1)-vect(3)/vect(6)*vect(4)
        ytarg = vect(2)-vect(3)/vect(6)*vect(5)
C.
        CALL hfill(217,xtarg,vect(4),1.0)
        CALL hfill(218,ytarg,vect(5),1.0)
C.
        xprime = 1000.*atan(vect(4)/vect(6))
        yprime = 1000.*asin(vect(5))
        deltap = 100.*(vect(7)/sqrt(e0recoil*(e0recoil+2*prodm))-1.)
C.
C.  old ray output
C        write(10,111)xtarg,xprime,ytarg,yprime,dummy,dummy,deltap
  111   format(7F10.4)
C.
CCC        istop = 1
C.
      Endif
C.
      if(ipart.eq.irecoil.and.lkine .gt. 0 .and.lpart.ne.irecoil)then
C.   Initial position and momentum of recoil part
          CALL hfill(5,vect(1),0.0,1.0)
          CALL hfill(6,vect(2),0.0,1.0)
          CALL hfill(7,vect(4)*1000.,0.0,1.0)
          CALL hfill(8,vect(5)*1000.,0.0,1.0)
          CALL hfill(10,deltap,0.0,1.0)
      endif
      lpart = ipart
C
  999 Continue
C.
C.      
      If(ngkine.gt.0)then
C.
        CALL uhtoc(kcase,4,chcase,4)
C.
        Do i = 1, ngkine
C.
           iflgk(i) = 0
C.
           If(chcase.eq.'DCAY'.or.chcase.eq.'RESR')then
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
C *** Debug/plot event
C.
      CALL gdebug
C.
      if(alpha)then
         continue
      elseIf(istop.ne.0)then
        write(6,*)' Whats stopping me??? (in trgt)'
        write(6,*)' istop: ',istop,' Volume: ',chname_nlevel,"  ",sleng,
     &            ' ipart: ',ipart
      Endif

      ngkine = 0
C.
      name_old   = names (nlevel)
      number_old = number(nlevel)
      ntmult_old = ntmult

      RETURN
      END
