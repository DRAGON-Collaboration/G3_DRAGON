C.
      SUBROUTINE uvinit
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C               Define defaults of run-time variables                C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
      
C.
      include 'gccuts.inc'          !geant
      include 'gcphys.inc'          !geant
      include 'gcmulo.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gckine.inc'          !geant
      include 'gctime.inc'          !geant
      include 'gcunit.inc'          !geant
C.
      include 'ukine.inc'           !local
C.
      include 'uevent.inc'          !local
      include 'geometry.inc'        !local
      include 'uggeom.inc'          !local
      include 'uenergy.inc'         !local
      include 'cntrs.inc'           !local
C.
C.    *************** RUN DEFAULTS ***************
C.
      lout = 6         ! GEANT output unit
      idrun = 1        ! Current user run number
      idevt = 0        ! Current user event number
C.
      timend = 10.     ! Time required for program termination phase
C.
      iloss  =  1      ! Continuous energy loss with delta-rays
      istra  =  0      ! Urban model for energy loss in thin layers
      ihadr  =  1      ! Hadronic interactions/generation of secondaries
      ipfis  =  2      ! Resonance photon absorption
C.
      cutgam = ekmin   ! Photon energy tracking cut
      cutele = ekmin   ! Electron kin. energy tracking cut
      cutmuo = 0.0001  ! Muon kin. energy tracking cut
      cuthad = 0.001   ! Charge hadron kin. energy tracking cut
      cutneu = 0.001   ! Neutral hadron kin. energy tracking cut
C.
      bcute  = cutgam  ! Kinetic energy cut for electron bremsstr.
      bcutm  = cutgam  ! Kinetic energy cut for muon bremsstr.
      dcute  = cutele  ! Kinetic energy cut for electron delta-rays
      dcutm  = cutele  ! Kinetic energy cut for muon delta-rays
C.
C.    *************** USER DEFAULTS ***************
C.
      nunits = 5
      lunits(1) =  6            ! standard output
      lunits(2) =  4            ! FFCARD input
      lunits(3) = 22            ! MITRAY, BFMAP files
      lunits(4) = 34            ! HBOOK histogram file
      lunits(5) = 23            ! RAYFILE file
C.
      ikine = 0                    ! No DRAGON test particle
C.
      pkine(1) =   0.0             ! Particle origin x [cm]
      pkine(2) =   0.0             ! Particle origin y [cm]
      pkine(3) =   0.0             ! Particle origin z [cm]
      pkine(4) = 258.55443         ! Particle momentum [MeV/c]
      pkine(5) =   0.0             ! horizontal emittance [mr]
      pkine(6) =   0.0             ! vertical   emittance [mr]
      pkine(7) =   0.0             ! RAYTRACE theta [mr]
      pkine(8) =   0.0             ! RAYTRACE phi   [mr]
      pkine(9) =   1.0             ! 1 +- deltaP/P
C.
      mkine     =   0           ! number of photons at initial vertex
      gkine( 1) =   0.0         ! x of photon origin distribution [cm]
      gkine( 2) =   0.0         ! y of   "      "         "       [cm]
      gkine( 3) =   0.0         ! z of   "      "         "       [cm]
      gkine( 4) =   0.0         ! length of photon origin x-dimension [cm]
      gkine( 5) =   0.0         ! length of photon origin y-dimension [cm]
      gkine( 6) =   0.0         ! length of photon origin z-dimension [cm]
      gkine( 7) =   4.03        ! photon energy [MeV]
      gkine( 8) =   0.0         ! theta [degree]
      gkine( 9) =   0.0         ! phi [degree]
      gkine(10) =   0.0         ! emittance [mrad]
C.
      lkine     =   2           ! full 15O(alpha,g)19Ne simulation
      fkine( 1) =   8.0         ! effective charge of the BEAM in vacuum
      fkine( 2) =   4.0         ! effective charge of the PRODUCT in vacuum
C.
      max_step = 100000            ! Default maximum number of steps
      len_max  =  10000.           ! Default is 100m track length
C.
      irevs = 0
C.
      CALL vzero(egamma,10)
C.
      iscnt = 1
      photon_yield = 10000.0
      resolution_scale = 1.0
C.
      tot_thrshld = 0.0
      pmt_thrshld = 0.0
C.
      n_detmate = 14
C.
      s_finger = 5.08
      z_finger = 7.62
C.
      d_air(1) = 0.01270
      d_air(2) = 0.01270
      d_mtl = 0.00254
C.
      wall(1) = 0.5
      wall(2) = 0.1
      wall(3) = 0.1
C.
C.      gap  = 4.0
C.      pinhe= 0.6
C.      pinhx = 0.8
C.
C.      shield_end(1) =  0.0
C.      shield_end(2) = 20.0
C.
      mtype_pmt  =  1
      pmt_size   =  2.5
      pmt_length = 10.0
C.
      bulk_absorption  = 10.0
      paint_absorption = 0.20
C.
C.      xtnd_block  = 1.0
      E_threshold = 3.0
C.
C-----------------------------------------------------------------------
C.
C.--> Define the geometrical dimensions of the different parts
C.                      of the apparatus.
C.
      Rrms    =  6.0               ! gas volume radius
      TLrms   = 88.0               ! DETE Volume
      targetl =  5.5               ! diameter of target vessel
C.
C.                          Apertures
C.
      rent = 0.8
      lent = 0.8
      len1 = 7.6
      riren1 = 0.4
      rilen1 = 0.5
c
      len2 = 8.7
      riren2 = 0.495
      rilen2 = 0.59
C.
      len3 = 10.
      riren3 = 0.65
      rilen3 =0.75
C.
C.    JS makes dimension changes to pumping tubes
C.    Old pumping tube setup is commented out
C      lex1   =  7.6
      lex1 = 0.34
C      rilex1 =  0.45
      rilex1 = 0.64135
C      rirex1 =  0.59
      rirex1 = 0.71755
C.
C      lex2   =  1.65
      lex2 = 7.15
C      rilex2 =  0.675
      rilex2 = 1.16713
C      rirex2 =  0.675
      rirex2 = 1.51003

C      lex3   =  3.9
      lex3 = 10.25
C      rilex3 =  0.895
      rilex3 = 1.83134
C      rirex3 =  1.03
      rirex3 = 2.36474
C.
C      lex4   = 15.25
      lex4 = 11.45
C      rilex4 =  1.25
      rilex4 = 2.72161
C      rirex4 =  1.80
      rirex4 = 3.26644
C
C.                              Z Positions
C.
      zent(1) = -15.6
      zent(2) =   0.0
      zent(3) = -36.05
      zent(4) =   0.0
      zent(5) = -63.75
      zent(6) =   0.0
C.
C      zex (1) =  15.6
      zex (1) = 10.25
      zex (2) =   0.0
C      zex (3) =  29.95
      zex (3) = 38.35
      zex (4) =   0.0
C      zex (5) =  39.7
      zex (5) = 67.95
      zex (6) =   0.0
C      zex (7) = 68.75
      zex(7) = 106.35
C.
C Init of counters
      nreact=0
      ntargexit=0
      nend=0
      nfcm2=0
      ngascell = 0
      nbeamout = 0
C MT adds counters for the number of recoils that make it
C   to Q3 (Sext 1) and to Q8 (Quad 6).
      Num_Recoils_Q3 = 0
      Num_Recoils_Q8 = 0
C MT adds counter for the number of beam particles that reach
C   the end detector.
      Num_BeamPart_ENDV = 0
C JS adds a counter to determine if the current recoil reach the end detector
C recoil_hit_ENDV = 0 <=> recoil didn't reach end detector
C recoil_hit_ENDV = 1 <=> recoil did reach end detector
      recoil_hit_ENDV = 0
      RETURN
      END
C.








