C.
      SUBROUTINE uginit
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     UGINIT initializes the GEANT/USER program and reads data cards   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'gconst.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gclist.inc'          !geant
      include 'gccuts.inc'          !geant
      include 'gcmulo.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gcrz.inc'            !geant
C.
      include 'ukine.inc'           !local
C.
      include 'uevent.inc'          !local
      include 'runstats.inc'        !local
      include 'geometry.inc'        !local
C.
      INTEGER ivalue, ilook
      CHARACTER*80 text
C.
      REAL FULL_MONTE_VRN
C.
C.-->   Initialise GEANT
C.
      CALL ginit
C.
C.-->   Set standard RUN defaults
C.
      CALL uvinit
C.
C.-->   Define user FFKEYs
C.
      CALL ugffgo
C.
C.-->   Read data records
C.
      CALL ffget('LINP',ivalue)
C.
      read(ivalue,111)text
      write(lout,111)text
  111 format(a80)
C.
      CALL gffgo
C.
      CLOSE(ivalue)
C.
      cutgam = ekmin   ! Photon energy tracking cut
      cutele = ekmin   ! Electron kin. energy tracking cut
C.
      bcute  = cutgam  ! Kinetic energy cut for electron bremsstr.
      bcutm  = cutgam  ! Kinetic energy cut for muon bremsstr.
      dcute  = cutele  ! Kinetic energy cut for electron delta-rays
      dcutm  = cutele  ! Kinetic energy cut for muon delta-rays
C.
      maxnst = max_step
C.
C.-->   Initialize data structures
C.
      CALL gzinit
C.
C.-->   Read permanent data structures from file
C.
      If(nrget.gt.0)then
C.
        CALL glook('INIT',lrget,nrget,ilook)
        If (ilook.ne.0) CALL grfile(1,'mygeom.dat','I')
C.
      Else
C.
C.-->   Particle table initialization
C.
        CALL gpart                     !Define standard particle data
C.
C.-->     Define Ne19++++ particle
C.
        CALL gspart(61,'Ne19++++   ',8,19.*0.93149432,4.,1000.,0,0)
C.
C.-->   Define radioactive ion reactions
C.
        CALL ureact  
C.
C.-->   Material table initialization
C.
        CALL gmate                     !Define standard material data
C.
C.-->   Define additional materials and traking medium parameters
C.
        CALL ugmate
        CALL ugstmed
C.
C.-->   Geometry description
C.
        CALL ugeom
C.
      Endif
C.
C.-->   Initialize MITRAY B-field routines by reading RAYTRACE file
C.
      CALL mitray_setup
C.
C.-->   Now define some of the previous volumes to be detectors
C.
      CALL udet
      CALL udetmitray
C.
C.-->   Perform requested geometry optimisation (GSORD)
C.
      CALL ggclos
C.
C.-->   Energy loss and cross-sections tables
C.
      CALL gphysi
C.
C.-->   Debug printing after initialization
C.
C.      Version number for FULL_MONTE
C.
      FULL_MONTE_VRN = 1.000
C.
      write(lout,*)' - This MC run has the following version numbers - '
      write(lout,*)' ------------------------------------------------- '
      write(lout,*)'        The GEANT version # is:   ',GVERSN
      write(lout,*)'     The FULL_MONTE version # is: ',FULL_MONTE_VRN
C.
      If (idebug.ne.0) then
C.
        CALL glook('PART',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('PART',0)
        CALL glook('MATE',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('MATE',0)
        CALL glook('TMED',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('TMED',0)
        CALL glook('VOLU',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('VOLU',0)
        CALL glook('ROTM',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('ROTM',0)
        CALL glook('SETS',lprin,nprin,ilook)
        If (ilook.ne.0) CALL gprint('SETS',0)
C.
      Endif
C.
C.-->   Save permanent data structures
C.
      If(nrsave.gt.0)then
        CALL glook('INIT',lrsave,nrsave,ilook)
        If (ilook.ne.0) CALL grfile(2,'mysave.dat','NO')
      Endif

C.  Determine the approriate beam energy for the required interactio
C.  and adjust the DRAGON tune appropriate to the recoils
      IF(nrget .le. 0) call beaminit
      print*, 'uginit.f'
C.
C.-->   Initialize histogram package
C.
      CALL uhinit
      print*, 'uginit.f'
C.
C.-->   Initialize standard histograms
C.
      If(nhsta.gt.0)CALL gbhsta
C.
C ---> Save the init phase in ntuple
C.
      If(iswit(9).eq.2)then
         CALL hfnt(998)
      Endif
C.

C.-->   Initialize run statistics variables
C.
      n_detector = 0
C.
      RETURN
      END
C.


















