C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     File bint.inc  -  grid of solenoidal magnetic field values.      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INTEGER*4 n, max_r, max_z
      PARAMETER (n = 2)                        ! Dimension of field grid
      PARAMETER (max_r =  31)                  ! # of r points in grid
      PARAMETER (max_z = 301)                  ! # of z points in grid
      INTEGER*4 na(n)
      
      REAL*4 a(max_R + max_Z)
      REAL*4 f_br(max_R, max_Z)                ! Radial field comp.
      REAL*4 f_bz(max_R, max_Z)                ! Long. field comp.
      REAL*4 bz00                              ! Long. com. at (0,0)
      
      COMMON /b_int/ na, a, f_br, f_bz, bz00
C
