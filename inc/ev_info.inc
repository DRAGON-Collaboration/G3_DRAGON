C.
C.                Information on the photons
C.                --------------------------
C.
C.    true_e: The true energy of photon [MeV]
C.    true_d: The true photon unit vector [unit-vector-components]
C.
C.            Information on the photon conversion point (i)
C.           ------------------------------------------------
C.
C.    vname(i)   : The full name of the conversion volume 
C.    ivcopy(2,i): The volume copy number of the conversion volume
C.    true_conv(3,i): The coordinates of the conversion piont [cm]
C.
C.    Index array relating in the event ITRA -> n_gamma
C.    -------------------------------------------------
C.
C.    index_track_to_gamma: ITRA -> n_gamma
C.
C.    ntot: Total number of PMT hits 
C.
C.    ifngr_max: The coordinates of the maximum finger
C.
C.    x_conv, y_conv, z_conv: The coordinates of 'first' conversion [cm]
C.
C.    x_max, y_max, z_max:    The coordinates of max energy deposition [cm]
C.
C.    x_mean, y_mean, z_mean: The coordinates of mean energy deposition [cm]
C.
      REAL true_e, true_d
C.
      INTEGER     ivcopy
      REAL        true_conv
C.
      INTEGER max_photon
      PARAMETER (max_photon = 10)
C.
      INTEGER max_conv
      PARAMETER (max_conv = 10)
C.
      INTEGER max_itra
      PARAMETER (max_itra = 30)
C.
      INTEGER index_track_to_gamma
C.
      INTEGER ifngr_max
C.
      REAL x_conv, y_conv, z_conv
      REAL x_max, y_max, z_max
C.
      INTEGER ntot
C.
      REAL x_mean, y_mean, z_mean
C.
      INTEGER mclu
      PARAMETER ( mclu  = 10 )
      INTEGER mhits
      PARAMETER ( mhits = 10 )
C.
C.    nclu              Number of energy clusters found
C.
      INTEGER nclu
C.
C.    nhit(iclu):       Number of neighbours on hit list
C.    jhit(ihit,iclu):  Volume # of entry on hit list
C.
      INTEGER nhit(mclu), jhit(mhits,mclu)
C.
      REAL eclu(mclu), xclu(mclu), yclu(mclu), zclu(mclu)
C.
      REAL dir_clu(3,mclu)
C.
      COMMON /EV_INFO/
     *       index_track_to_gamma(max_itra)
     *      ,true_e(max_photon),true_d(3,max_photon)
     *      ,ivcopy(2,max_conv),true_conv(3,max_conv)
     *      ,x_conv,y_conv,z_conv
     *      ,ifngr_max,x_max,y_max,z_max
     *      ,ntot, x_mean,y_mean,z_mean
     *      ,nclu,nhit,jhit,eclu,xclu,yclu,zclu,dir_clu
C.
      CHARACTER*4 vname(max_conv)
C.
