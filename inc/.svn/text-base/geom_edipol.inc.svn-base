C.
C. *** Geometry COMMON BLOCK for all electrostatic DEFLECTORS in the beam-line
C.
C.    The radius of curvature is POSITIVE 
C.        if bend from entrance to exit is in a clockwise direction
C.    The radius of curvature is NEGATIVE
C.        if bend from entrance to exit is in a counter clockwise direction
C.
      INTEGER max_edipol
      PARAMETER (max_edipol = 9)
C.
      INTEGER nedipol                   ! # of electrostatic deflectors
C.
      REAL pos_edipol(3,max_edipol)     ! position  of deflector in beam-line
C.
      INTEGER irot_edipol(max_edipol)   ! rotation matrix index of A-frame
      REAL    dx_edipol(3,max_edipol)   ! displacement of A-frame 
C.
      REAL gap_edipol(max_edipol)       ! deflector gap width
      REAL phi_edipol(max_edipol)       ! deflector bend angle
      REAL   r_edipol(max_edipol)       ! deflector radius of curvature
      REAL  dr_edipol(max_edipol)       ! deflector delta_r of radius
C.
      REAL z11_edipol(max_edipol)       ! RAYTRACE - z11
      REAL z22_edipol(max_edipol)       ! RAYTRACE - z22
C.
      INTEGER jcol_edipol(2,max_edipol) ! collimator type
      REAL  xcol_edipol(2,max_edipol)   ! collimator x-position
      REAL  ycol_edipol(2,max_edipol)   ! collimator y-position
      REAL dxcol_edipol(2,max_edipol)   ! collimator x-size
      REAL dycol_edipol(2,max_edipol)   ! collimator y_size
C.
      COMMON / geom_edipol / nedipol, pos_edipol, 
     &                  irot_edipol, dx_edipol,
     &                  gap_edipol, phi_edipol, r_edipol, dr_edipol,
     &                  z11_edipol, z22_edipol,
     &                  jcol_edipol, xcol_edipol, ycol_edipol,
     &                  dxcol_edipol, dycol_edipol
C.
