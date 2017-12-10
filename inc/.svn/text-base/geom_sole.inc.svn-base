C.
C. *** Geometry COMMON BLOCK for SOLENOID magnets in the beam-line
C.
      INTEGER max_sole
      PARAMETER (max_sole = 9)
C.
      INTEGER nsole                    ! # of solenoids
C.
      REAL pos_sole(3,max_sole)        ! position of solenoid in beam-line
C.
      REAL  dx_sole(3,max_sole)        ! displacement of A-frame
C.
      REAL efblength_sole(max_sole)    ! solenoid length
      REAL         r_sole(max_sole)    ! solenoid radius
C.
      REAL z11_sole(max_sole)          ! RAYTRACE - z11
      REAL z22_sole(max_sole)          ! RAYTRACE - z22
C.
      INTEGER jcol_sole(2,max_sole)    ! collimator type
      REAL  xcol_sole(2,max_sole)      ! collimator x-position
      REAL  ycol_sole(2,max_sole)      ! collimator y-position
      REAL dxcol_sole(2,max_sole)      ! collimator x-size
      REAL dycol_sole(2,max_sole)      ! collimator y_size
C.
      COMMON / geom_sole / nsole, pos_sole, dx_sole,
     &                  efblength_sole, r_sole,
     &                  z11_sole, z22_sole,
     &                  jcol_sole, xcol_sole, ycol_sole,
     &                  dxcol_sole, dycol_sole
C.
