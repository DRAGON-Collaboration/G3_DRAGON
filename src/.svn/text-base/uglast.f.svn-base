C. 
      SUBROUTINE uglast
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C             UGLAST terminates the GEANT/USER  program                C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      INTEGER icycle, ier
      REAL tmx
      integer nbeam
C.
      include 'gclist.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gctime.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'cntrs.inc'           !local
C.
      CALL timel(tmx)
      write(lout,100)timint-tmx
  100 format(' Time elapsed after initialization = ',E15.5 )
      nbeam = ievent - nreact
C.
      idevt = idevt - ievent ! idevt = sum of all events + sum of good events
C.
      write(lout,*)'Total number of events generated',ievent
      write(lout,*)'Number of events that went to output',idevt
      write(lout,*)'Random Number at the beginning of last event'
      write(lout,*)'**** ',nrndm(1),nrndm(2),' **** '
      write(lout,*)'Beam particles entering gas cell ',ngascell
      if (ngascell .gt. 0)
     + write(lout,*)'Reactions occurring ',nreact,100.*nreact/ngascell,
     + '%  +- ',100.*sqrt(nreact*(1-nreact/real(ngascell)))/ngascell,'%'
      if (nreact .gt. 0) then
        write(lout,*)
     + '** Recoil efficiencies are with respect to the number of ',
     +    'Reactions **'
        write(lout,*)
     + 'Recoils exiting target',ntargexit,100.*ntargexit/nreact,
     + '%  +- ',100.*sqrt(ntargexit*(1.-ntargexit/real(nreact)))/nreact,
     +  '%'
        write(lout,*)
C
C MT adds counters for the number of recoils that make it
C   to Q3 (Sext 1) and to Q8 (Quad 6).
C
     + 'Recoils reaching Q3 (Sext 1) ', Num_Recoils_Q3, 
     +    100.*Num_Recoils_Q3/nreact,'%  +- ',
     +    100.*sqrt(Num_Recoils_Q3*(1.-Num_Recoils_Q3/real(nreact)))/
     +    nreact,'%'
        write(lout,*)
     + 'Recoils reaching Q8 (Quad 6) ', Num_Recoils_Q8, 
     +    100.*Num_Recoils_Q8/nreact,'%  +- ',
     +    100.*sqrt(Num_Recoils_Q8*(1.-Num_Recoils_Q8/real(nreact)))/
     +    nreact,'%'
        write(lout,*)
C
     + 'Recoils reaching end detector ', nend, 100.*nend/nreact,
     + '%  +- ',100.*sqrt(nend*(1.-nend/real(nreact)))/nreact,'%'
        write(lout,*)
     + 'Beam particles exiting target ', nbeamout,100.*nbeamout/nbeam, 
     + '%  +- ',100.*sqrt(nbeamout*(1.-nbeamout/real(nbeam)))/nbeam,'%'
        write(lout,*)
     + 'Beam particles reaching FCM2 ',nfcm2,100.*nfcm2/nbeam,
     + '%  +- ', 100.*sqrt(nfcm2*(1.-nfcm2/real(nbeam)))/nbeam,'%'
        write(lout,*)
C
C MT adds counter for the number of beam particles that reach
C   the end detector.
C
     + 'Beam particles reaching end detector ', Num_BeamPart_ENDV, 
     +    100.*Num_BeamPart_ENDV/nbeam,'%  +- ',
     +    100.*sqrt(Num_BeamPart_ENDV*(1.-Num_BeamPart_ENDV/
     +    real(nbeam)))/nbeam,'%'
      endif
C.
C.-->   Call standard GEANT termination routine
C.
      CALL glast
C.
C.-->   Save histograms
C.
      CALL hrout(0,icycle,' ')
      CALL hrend('HBOOK')
      CLOSE(lunits(4))
C.
      If(nget.ne.0.or.nsave.ne.0)CALL gclose(0,ier)
C.
C.-->   Print histograms
C.
      If(nhsta.gt.0)CALL histdo
C.
      RETURN
      END 
C.
