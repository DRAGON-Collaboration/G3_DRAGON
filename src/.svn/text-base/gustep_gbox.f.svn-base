C.
      SUBROUTINE gustep_gbox
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
      include 'gcmulo.inc'          !geant
C.
      include 'geometry.inc'		!local
      include 'runstats.inc'		!local
      include 'uenergy.inc'		!local
      include 'gammahit.inc'
C.
      INTEGER i, iflag, ivt, iadr
C.
      INTEGER nubuf
C.
      REAL ubuf(6)
C.
      CHARACTER* 4 chname_nlevel
      CHARACTER* 4 chname_old
      CHARACTER* 4 chcase
      CHARACTER* 4 wname
      CHARACTER* 1 vname
      CHARACTER*20 chtmed
C.
      INTEGER in_new_vol, name_old, number_old, ntmult_old
      DATA name_old / 0 /, number_old / 0 /, ntmult_old / 0 /
C.
      REAL xm(3), xd(3)
C.
      CALL uhtoc(names(nlevel),4,chname_nlevel,4)
C     Stop particles, other than beam and recoils, that try enter spectrometer
C     Some charged particles (ex. positrons) in B fields confused GEANT
      if (ipart.lt.80 .and. chname_nlevel.eq.'WRLD')then
         istop = 2
      endif
C.
C.    Because INWVOL = 1 can mean either that a new volume has been entered
C.    or that a new track has been started, define a new variable IN_NEW_VOL
C.    which specifically indicates a new volume.
C.
      in_new_vol = 0
      If(inwvol.eq.1)then
        If(name_old  .ne.names (nlevel).or.
     &     number_old.ne.number(nlevel))then
             if(ntmult.eq.ntmult_old)in_new_vol = 1
        Endif
      Endif
C.
C.    Find if any of original photon hit the sleeve of photon detector module
C.
      If(in_new_vol.eq.1.and.ivert.eq.1)then
        If(chname_nlevel.eq.'FNGR'.or.chname_nlevel.eq.'SCNT')then
          If(ipart.eq.1)then
            If(n_flag.eq.0)n_detector = n_detector + 1
            n_flag = n_flag + 1
          Endif
        Endif
      Endif
C.
      If(itrtyp.eq.2.and.destep.gt.0.0
     &              .and.itckov.eq.2.and.istop.eq.0)then
C.
C ***   The charged particle has produced scintillation photons and it
C ***   is still alive; we put it on the stack and we let the photons be
C ***   tracked first
C.
        ngkine = ngkine + 1
        gkin(1,ngkine) = vect(4)*vect(7)
        gkin(2,ngkine) = vect(5)*vect(7)
        gkin(3,ngkine) = vect(6)*vect(7)
        gkin(4,ngkine) = getot
        gkin(5,ngkine) = ipart
        tofd(ngkine) = 0.0
        istop = 1
        gpos(1,ngkine) = vect(1)
        gpos(2,ngkine) = vect(2)
        gpos(3,ngkine) = vect(3)
C.
      Endif
C.
C.    Stop shower particles if they enter (not simulated) PMT volume
C.
      If(itrtyp.eq.1.or.itrtyp.eq.2)then
        If(chname_nlevel.eq.'PMT ')then
          istop = 2
          goto 200
        Endif
      Endif
C.
C.    Deal with scintillation photons when they are elsewhere
C.
      If(in_new_vol.eq.1.and.itrtyp.eq.7)then
        If(chname_nlevel.eq.'PMT ')then
          CALL ghipmt
          goto 200
        Elseif(chname_nlevel.ne.'SCNT'.and.
     &         chname_nlevel.ne.'MGOR')then
          istop = 3
          goto 200
        Endif
      Endif
C.
C *** Store hits
C.
      If(numed.eq.n_detmate.and.itrtyp.ne.7)CALL ghidet
C.
C *** Daughter particles that were generated in the current step
C ***                  are put on the stack
C.
      If(ngkine.gt.0)then
C.
        iflag = 0
        CALL uhtoc(kcase,4,chcase,4)
C.
        Do i = 1, ngkine
           iflgk(i) = 0
           If(ipart.eq.1)then
             if(chcase.eq.'PAIR')then
                pair_productions = pair_productions + 1
             endif
             If(chcase.eq.'PAIR'.or.
     &          chcase.eq.'COMP'.or.
     &          chcase.eq.'PHOT'    )then
                If(ivert.eq.1)then
                  iflag = 1
                  iflgk(i) =  1
                Endif
             Endif
           Endif
           If(gkin(5,i).eq.4)iflgk(i) = -1
        Enddo
C.
	CALL gsking(0)
C.
        If(iflag.gt.0)then
          iadr = 0
          nubuf = 6
          CALL vzero(ubuf,nubuf)
          ivt = iflgk(ngkine+1)
          If(ivt.gt.0)then
            If(nlevel.gt.2)then
              CALL uhtoc(iq(jvolum+lvolum(2)),4,wname,4)
              CALL uhtoc(iq(jvolum+lvolum(3)),4,vname,1)
              If(wname.eq.'DETE')then
                ubuf(1) = ipart
                ubuf(2) = numed
                ubuf(3) = lvolum(nlevel)
                If(vname.eq.'H')then
                  ubuf(4) = number(2)    ! Scintillator  Number
                  ubuf(5) = number(3)    ! Module  Number
                  CALL ucopy(vect(1),xm(1),3)
                  CALL gmtod(xm,xd,1)
                  ubuf(6) = xd(3)
                Else
                  ubuf(4) = 0.0          ! Scintillator  Number
                  ubuf(5) = 0.0          ! Vacuum Tube Wall
                  ubuf(6) = -999.0
                Endif
                CALL gsveru(ivt,nubuf,ubuf,iadr)
              Endif
            Endif
          Endif
        Endif
C.
      Endif
C.
C *** Scintillation photons generated?
C.
      if(ngphot.gt.0)CALL gskpho(0)
C.
  200 Continue
C.
C *** Debug/plot event
C.
      CALL gdebug
C      If(itrtyp.eq.8)CALL gdebug
C.      If(itrtyp.ne.7)Call gdebug
C.      If(itrtyp.eq.7)Call gdebug
C.
      ngkine = 0
C.
      name_old   = names (nlevel)
      number_old = number(nlevel)
      ntmult_old = ntmult
C.
      CALL uhtoc(name_old,4,chname_old,4)
C.
      RETURN
      END
