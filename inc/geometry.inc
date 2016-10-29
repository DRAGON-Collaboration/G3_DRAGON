C.
      INTEGER n_detmate, targtype, tubetype
      REAL s_finger, z_finger, air_gap, d_air(2), d_mtl
C.
      INTEGER max_hexagon
      PARAMETER (max_hexagon = 30)
C.
      REAL hexagon_small_width, hexagon_large_width, depth
      REAL wall(3), box_width, box_length, aprt, col_length,
     +     col_collar_length
C.
      INTEGER mtype_pmt
      REAL pmt_size, pmt_length
C.
      REAL bulk_absorption, paint_absorption
C.
      REAL x_fngr(max_hexagon), y_fngr(max_hexagon), z_fngr(max_hexagon)
C.
      INTEGER Nn
      PARAMETER ( Nn = 10 )
      INTEGER n_fngr(Nn,max_hexagon)
C.
      COMMON / geometry / n_detmate, s_finger, z_finger, 
     &                    air_gap, d_air, d_mtl,
     &                    mtype_pmt, pmt_size, pmt_length,
     &                    bulk_absorption, paint_absorption,
     &                    hexagon_small_width, hexagon_large_width,
     &                    depth, wall, box_width, box_length, aprt,
     &                    col_length, col_collar_length,
     &                    targtype, tubetype
C.
      COMMON / fngr_geo / x_fngr, y_fngr, z_fngr, n_fngr
C.







