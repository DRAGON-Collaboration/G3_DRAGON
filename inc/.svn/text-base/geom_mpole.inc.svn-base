C.
C. *** Geometry COMMON BLOCK for all POLES magnets in the beam-line
C.
      INTEGER max_mpole
      PARAMETER (max_mpole = 20)
C.
      INTEGER nmpole                   ! # of mpoles
C.
      REAL pos_mpole(3,max_mpole)      ! position  of mpole in beam-line
C.
      REAL  dx_mpole(3,max_mpole)      ! displacement of A-frame
C.
      REAL efblength_mpole(max_mpole)  ! mpole gap width
      REAL         r_mpole(max_mpole)  ! mpole aperture radius
C.
      REAL z11_mpole(max_mpole)        ! RAYTRACE - z11
      REAL z22_mpole(max_mpole)        ! RAYTRACE - z22
C.
      INTEGER jcol_mpole(2,max_mpole)  ! collimator type
      REAL  xcol_mpole(2,max_mpole)    ! collimator x-position
      REAL  ycol_mpole(2,max_mpole)    ! collimator y-position
      REAL dxcol_mpole(2,max_mpole)    ! collimator x-size
      REAL dycol_mpole(2,max_mpole)    ! collimator y_size
C.
      COMMON / geom_mpole / nmpole, pos_mpole, dx_mpole,
     &                  efblength_mpole, r_mpole,
     &                  z11_mpole, z22_mpole,
     &                  jcol_mpole, xcol_mpole, ycol_mpole,
     &                  dxcol_mpole, dycol_mpole
C.
