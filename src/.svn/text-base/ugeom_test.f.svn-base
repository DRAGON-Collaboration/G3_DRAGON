      SUBROUTINE ugeom_test
*
* *** Define experimental setup
*
      COMMON/CUSER/IMAT1,THICK1,IMAT2,THICK2
*
      DIMENSION PAR( 3)
      CHARACTER text*41, unt*8
      CHARACTER*20 namat(15)
*
      CALL GIDROP
      CALL GPART
*
* *** Defines USER perticular materials
       namat(1) = 'air'
      CALL GSMATE( 1,namat(1), 14.61,7.3,0.001205,30423.,6750.,0,0)
       namat(2) = 'H2 liquid'
      CALL GSMATE( 2,namat(2),   1.01, 1.,0.0708,865.,790.,0,0)
       namat(3) = 'Ar liquid'
      CALL GSMATE( 3,namat(3),   40. ,18., 1.40 , 14., 84.,0,0)
       namat(4) = 'carbon'
      CALL GSMATE( 4,namat(4), 12.01, 6.,2.265 ,18.8,49.9,0,0)
       namat(5) = 'Aluminium'
      CALL GSMATE( 5,namat(5), 26.98,13.,2.7   , 8.9,37.2,0,0)
       namat(6) = 'Iron'
      CALL GSMATE( 6,namat(6), 55.85,26., 7.87 ,1.76,17.1,0,0)
       namat(7) = 'Copper'
      CALL GSMATE( 7,namat(7), 63.54,29.,8.96  ,1.43,14.8,0,0)
       namat(8) = 'Lead'
      CALL GSMATE( 8,namat(8),207.19,82.,11.35 ,0.56,18.5,0,0)
*
* *** Defines tracking media parameters
      ISVOL    =  0
      FIELDM  =   50.
      TMAXFD = 10.0
      STEMAX =  1000.
      DEEMAX =  0.200
      EPSIL    =  0.0001
      STMIN   =  0.0001
*
      IMAT1  = 1
      IFIELD = 0
      CALL GSTMED( 1,'Frame    ',IMAT1,ISVOL ,IFIELD,FIELDM,TMAXFD,
     *                STEMAX,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
      IMAT2  = 5
      IFIELD = 3
      CALL GSTMED( 2,'Tr. medium',IMAT2,ISVOL ,IFIELD,FIELDM,TMAXFD,
     *                STEMAX,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
* *** Geometry  description
      THICK1 = 24.
      THICK2 = 20.
      SIZE1  = 0.5*THICK1
      PAR(1) = SIZE1
      PAR(2) = SIZE1
      PAR(3) = SIZE1
      CALL GSVOLU( 'VOL1' , 'BOX ' , 1, PAR , 3 , IVOL )
*
      SIZE2  = 0.5*THICK2
      PAR(1) = SIZE2
      PAR(2) = SIZE1
      PAR(3) = SIZE1
      CALL GSVOLU ('VOL2' , 'BOX ' , 2, PAR , 3 , IVOL )
*
      OGX =  0.
      CALL GSPOS  ('VOL2',1,'VOL1',OGX, 0., 0., 0, 'ONLY')
*
* *** Close geometry banks.
      CALL GGCLOS
*
      CALL GPRINT('VOLU',0)
*
      END

