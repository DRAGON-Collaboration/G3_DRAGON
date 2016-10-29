C.
      SUBROUTINE ugmate
C.
************************************************************************
*                                                                      *
*                 Routine to define tracking material                  *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      INTEGER i
C.
      INTEGER n_mat
      PARAMETER (n_mat = 13)  ! # of created new materials
C.
      INTEGER i_mat(n_mat)
      DATA i_mat/ 17, 18, 19, 20, 21, 22, 23, 24, 25, 50, 62, 51, 52/
C.
      CHARACTER*20 name_mat(n_mat)
C.
      DATA name_mat/                           ! materials created
     *            'SCINTILLATOR        ',
     *            'BARIUM FLORIDE BAF2 ',
     *            'CESIUM FLORIDE CSF  ',
     *            'SODIUM IODIDE NAI:TL',
     *            'CESIUM IODIDE CSI:TL',
     *            'BGO       BI4GE3O12 ',
     *            'LSO   LU2(SI04)O:CE ',
     *            'MGO (POWDER)        ',
     *            'GLASS               ',
     *            'SILICON             ',
     *            'TARGET CARBON       ',
     *            'Modified SILICON    ',
     *     'MCP Carbon'/
C.
      INTEGER nl_mat(n_mat)
      REAL a_mat(5,n_mat),z_mat(5,n_mat),w_mat(5,n_mat),dens_mat(n_mat)
      REAL radl_mat(n_mat),absl_mat(n_mat)
C.
      DATA a_mat/                       ! Atomic weights of constituents
     *     12.01,   1.01,  0.0 ,  0.0 ,  0.0,
     *     137.3,  19.0 ,  0.0 ,  0.0 ,  0.0,
     *     132.9,  19.0 ,  0.0 ,  0.0 ,  0.0,
     *      23.0, 126.9 ,  0.0 ,  0.0 ,  0.0,
     *     132.9, 126.9 ,  0.0 ,  0.0 ,  0.0, 
     *     209.0,  72.6 , 16.0 ,  0.0 ,  0.0,
     *     175.0,  28.1 , 16.0 ,  0.0 ,  0.0, 
     *      24.3,  16.0 ,  0.0 ,  0.0 ,  0.0,  
     *     12.01,   1.01,  0.0 ,  0.0 ,  0.0,
     *     28.08,    0.0,  0.0 ,  0.0 ,  0.0,
     *     12.00,    0.0,  0.0 ,  0.0 ,  0.0,
     *     28.08,    0.0,  0.0,   0.0,   0.0,
     *     12.00,    0.0,  0.0,   0.0,   0.0/
C.
      DATA z_mat/                       ! Atomic numbers of constituents
     *      6.0,  1.0,  0.0, 0.0, 0.0, 
     *     56.0,  9.0,  0.0, 0.0, 0.0,
     *     55.0,  9.0,  0.0, 0.0, 0.0, 
     *     11.0, 53.0,  0.0, 0.0, 0.0, 
     *     55.0, 53.0,  0.0, 0.0, 0.0,
     *     83.0, 32.0,  8.0, 0.0, 0.0, 
     *     71.0, 14.0,  8.0, 0.0, 0.0, 
     *     12.0,  8.0,  0.0, 0.0, 0.0, 
     *      6.0,  1.0,  0.0, 0.0, 0.0, 
     *     14.0,  0.0,  0.0, 0.0, 0.0,
     *      6.0,  0.0,  0.0, 0.0, 0.0,
     *     14.0,  0.0,  0.0, 0.0, 0.0,
     *      6.0,  0.0,  0.0, 0.0, 0.0/
C.
      DATA dens_mat/			! density
     *     1.032 ,
     *     4.890 ,
     *     4.640 ,
     *     3.670 ,
     *     4.510 ,
     *     7.130 ,
     *     7.400 ,
     *     1.870 ,
     *     1.032 ,
     *     2.330 ,
     *     0.0225,! TARGET CARBON density 100 x less dense than C.
     *     2.330E-02, !Modified SILICON 100 x less dense than Si.
     *     0.00225/ ! MCP Carbon density 1000 x less dense than C. 
C.
      DATA nl_mat/           ! >,< 0 => WMAT: proportions by mass, atoms
     *     -2 ,
     *     -2 ,
     *     -2 ,
     *     -2 ,
     *     -2 ,
     *     -3 ,
     *     -3 ,
     *     -2 ,
     *     -2 ,
     *      0 ,
     *      0 ,
     *      0 ,
     *      0/
C.
      DATA w_mat/	        ! proportions of elements in the mixture
     *     1.0  , 1.1  ,  0.0  , 0.0  , 0.0 ,
     *     1.0  , 2.0  ,  0.0  , 0.0  , 0.0 , 
     *     1.0  , 1.0  ,  0.0  , 0.0  , 0.0 ,
     *     1.0  , 1.0  ,  0.0  , 0.0  , 0.0 , 
     *     1.0  , 1.0  ,  0.0  , 0.0  , 0.0 , 
     *     4.0  , 3.0  , 12.0  , 0.0  , 0.0 , 
     *     2.0  , 1.0  ,  5.0  , 0.0  , 0.0 , 
     *     1.0  , 1.0  ,  0.0  , 0.0  , 0.0 , 
     *     1.0  , 1.1  ,  0.0  , 0.0  , 0.0 , 
     *     1.0  , 0.0  ,  0.0  , 0.0  , 0.0 ,
     *     1.0  , 0.0  ,  0.0  , 0.0  , 0.0 ,
     *     1.0  , 0.0  ,  0.0  , 0.0  , 0.0,
     *     1.0  , 0.0  ,  0.0  , 0.0  , 0.0/
C.
      DATA radl_mat/	        ! radiation length;  if 0 GEANT will calc.
     *     42.40 ,
     *      2.05 ,
     *      0.0  ,
     *      2.59 ,
     *      0.0  ,
     *      1.12 ,
     *      0.0  ,
     *      0.0  ,
     *     42.40 ,
     *     2.70  ,
     *      0.0  ,
     *      0.0  ,
     *      0.0  /
C.
      INTEGER n_matm1
      PARAMETER (n_matm1 = n_mat - 4)  
      DATA absl_mat/ n_matm1*0.0,8.9,0.0,0.0,0.0 / ! absorption length; if 0 GEANT will calc.
C.
      Do i = 1, n_mat
         If (nl_mat(i).eq.0)then
            CALL gsmate(i_mat(i), name_mat(i), a_mat(1,i), z_mat(1,i),
     *                  dens_mat(i), radl_mat(i), absl_mat(i), 0, 0)
         Else
            CALL gsmixt(i_mat(i), name_mat(i), a_mat(1,i), z_mat(1,i),
     *                  dens_mat(i), nl_mat(i), w_mat(1,i))
         Endif
      Enddo
C.
      CALL ugmate_trgt
C.
      RETURN
      END
C.













