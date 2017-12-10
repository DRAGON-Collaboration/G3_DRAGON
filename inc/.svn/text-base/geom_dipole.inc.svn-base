C.
C. *** Geometry COMMON BLOCK for all DIPOLE magnets in the beam-line
C.
C.    The radius of curvature is POSITIVE 
C.        if bend from entrance to exit is in a clockwise direction
C.    The radius of curvature is NEGATIVE
C.        if bend from entrance to exit is in a counter clockwise direction
C.
      INTEGER max_dipole
      PARAMETER (max_dipole = 9)
C.
      INTEGER ndipole                   ! # of dipoles
C.
      REAL pos_dipole(3,max_dipole)     ! position  of dipole in beam-line
C.
      INTEGER irot_dipole(max_dipole)   ! rotation matrix index of A-frame
      REAL    dx_dipole(3,max_dipole)   ! displacement of A-frame 
C.
      REAL gap_dipole(max_dipole)       ! dipole gap width
      REAL phi_dipole(max_dipole)       ! dipole bend angle
      REAL   r_dipole(max_dipole)       ! dipole radius of curvature
      REAL  dr_dipole(max_dipole)       ! dipole delta_r of radius
C.
      REAL alpha_dipole(max_dipole)     ! RAYTRACE - alpha
      REAL  beta_dipole(max_dipole)     ! RAYTRACE - beta
C.
      REAL z11_dipole(max_dipole)       ! RAYTRACE - z11
      REAL z22_dipole(max_dipole)       ! RAYTRACE - z22
C.
      INTEGER jcol_dipole(2,max_dipole) ! collimator type
      REAL  xcol_dipole(2,max_dipole)   ! collimator x-position
      REAL  ycol_dipole(2,max_dipole)   ! collimator y-position
      REAL dxcol_dipole(2,max_dipole)   ! collimator x-size
      REAL dycol_dipole(2,max_dipole)   ! collimator y_size
C.
      COMMON / geom_dipole / ndipole, pos_dipole, 
     &                  irot_dipole, dx_dipole,
     &                  gap_dipole, phi_dipole, r_dipole, dr_dipole,
     &                  alpha_dipole, beta_dipole,
     &                  z11_dipole, z22_dipole,
     &                  jcol_dipole, xcol_dipole, ycol_dipole,
     &                  dxcol_dipole, dycol_dipole
C.
