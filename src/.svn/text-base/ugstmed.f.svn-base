C.
      SUBROUTINE ugstmed
C.
************************************************************************
*                                                                      *
*                   Routine to define tracking media                   *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'geometry.inc'            !local
C.
      INTEGER i
C.
      INTEGER n_med
      PARAMETER (n_med = 24)  ! # of created tracking media
C.
      CHARACTER*20 name_med(n_med)
C.
      INTEGER nmed_mat(n_med), isvol_med(n_med), ifield(n_med)
      REAL fieldm(n_med), tmaxfd_med(n_med), dmaxms_med(n_med), 
     *     deemax_med(n_med), epsil_med(n_med), stmin_med(n_med)
C.
      REAL ubuf_med(n_med)
C.
      DATA name_med/			! names of materials
     *            'VACUUM ->  no field ', ! 1
     *            'VACUUM -> ifield = 1', ! 2 ! sensitive
     *            'VACUUM -> ifield = 2', ! 3 !sensitive
     *            'VACUUM -> ifield = 3', ! 4 !sensitive
     *            'COPPER              ', ! 5 !sensitive
     *            'ALUMINUM            ', ! 6
     *            'LEAD                ', ! 7
     *            'ATMOSPHERE (AIR)    ', ! 8
     *            'SCINTILLATOR        ', ! 9 !sensitive
     *            'BARIUM FLORIDE BAF2 ', ! 10 !sensitive
     *            'CESIUM FLORIDE CSF  ', ! 11 !sensitive
     *            'SODIUM IODIDE NAI:TL', ! 12 !sensitive
     *            'CESIUM IODIDE CSI:TL', ! 13 !sensitive
     *            'BGO       BI4GE3O12 ', ! 14 !sensitive
     *            'LSO   LU2(SI04)O:CE ', ! 15 !sensitive
     *            'MGO (POWDER)        ', ! 16
     *            'GLASS               ', ! 17 !sensitive
     *            'TUNGSTEN            ', ! 18 
     *            'SILICON             ', ! 19 !sensitive
     *            'STAINLESS STEEL     ', ! 20
     *            'TARGET CARBON       ', ! 21
     *            'central vacuum      ', ! 22
     *            'Modified SILICON    ', ! 23
     *            'MCP Carbon          '/ ! 24
C.
      DATA nmed_mat/                      ! index of these materials
     *     16, 16, 16, 16, 11,
     *     9, 13, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 12, 50,
     *     26, 62, 28, 51, 52/
C.
      DATA isvol_med/                     ! 0 if not a sensitive medium
     *     0,  1,  1,  1,  1,
     *     0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1/
C.
      DATA tmaxfd_med/ n_med*10.0  / ! max. angle due to field in one step
C.
C.    DATA dmaxms_med/ n_med*-1.0 / ! max displace for mult scat. in one step
      DATA dmaxms_med/ (n_med-4)*-1.0,4.0e-5,-1.0,4.0e-5,4.0e-5 / ! max displacement 
C     for mult scat. in one step
C.
      DATA deemax_med/ n_med*-1.0 / ! max. fractional energy loss in one step
C.
      DATA epsil_med/               ! tracking precision
     *     5*0.001, 2*0.001, 0.1, 12*0.001, 0.0001, 0.1, 0.00001,
     *0.00001/
C.
      DATA stmin_med/ n_med*-1.0  / ! min. step due to energy loss or m. s.
C.
      DATA  ifield   / 0, 1, 2, 3, 0, 
     *                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,
     *     0,0/
C.                                  ! magnetic field flag =1 GRKUTA
C.                                                        =2 GHELIX
C.                                                        =3 GHELX3
C.
      DATA  fieldm   / 0., 100., 100., 100., 0.,
     *               0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 
     *               0., 0., 0., 0., 0., 0., 0./
C.                                  ! max. field value [kGauss]
C.                                    or magn. field for GHELX3
C.
C.    ****************************************************
C.    *** GEANT will recalculate negative variables of ***
C.    *               dmaxms,deemax,stmin                *
C.    ***  as long as you don't run with IAUTO=0 card  ***
C.    ****************************************************
C.
C.
      INTEGER ipckov, npckov
      REAL ppckov, absco, effic, rindex
      REAL absco_scnt, effic_pmt, rindex_scnt
C.
      PARAMETER (npckov = 32)
C.
      DIMENSION ppckov(npckov), absco(npckov)
      DIMENSION effic(npckov), rindex(npckov)
      DIMENSION absco_scnt(npckov), effic_pmt(npckov)
      DIMENSION rindex_scnt(npckov)
C.
      DATA ppckov / 2.038E-9, 2.072E-9, 2.107E-9, 2.143E-9, 2.181E-9,
     &              2.220E-9, 2.260E-9, 2.302E-9, 2.346E-9, 2.391E-9,
     &              2.438E-9, 2.486E-9, 2.537E-9, 2.590E-9, 2.645E-9,
     &              2.702E-9, 2.763E-9, 2.825E-9, 2.891E-9, 2.960E-9,
     &              3.032E-9, 3.108E-9, 3.188E-9, 3.271E-9, 3.360E-9,
     &              3.453E-9, 3.552E-9, 3.656E-9, 3.767E-9, 3.884E-9,
     &              4.010E-9, 4.144E-9 /
C.
      DATA absco_scnt  /  344.8,  408.2,  632.9,  917.4, 1234.6, 1388.9,
     &                   1515.2, 1724.1, 1886.8, 2000.0, 2631.6, 3571.4,
     &                   4545.5, 4761.9, 5263.2, 5263.2, 5555.6, 5263.2,
     &                   5263.2, 4761.9, 4545.5, 4166.7, 3703.7, 3333.3,
     &                   3000.0, 2850.0, 2700.0, 2450.0, 2200.0, 1950.0,
     &                   1750.0, 1450.0 /
C.
      DATA rindex_scnt / 1.82, 1.82, 1.82, 1.82, 1.82, 1.82, 1.82,
     &                   1.82, 1.82, 1.82, 1.82, 1.82, 1.82, 1.82,
     &                   1.82, 1.82, 1.82, 1.82, 1.82, 1.82, 1.82,
     &                   1.82, 1.82, 1.82, 1.82, 1.82, 1.82, 1.82,
     &                   1.82, 1.82, 1.82, 1.82 /
C.
CCC      DATA effic_pmt  / 0.005,0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07,
CCC     &                  0.08, 0.09, 0.10, 0.115,0.13, 0.15, 0.16, 0.18,
CCC     &                  0.195,0.22, 0.23, 0.24, 0.25, 0.255,0.26, 0.265,
CCC     &                  0.26, 0.25, 0.24, 0.215,0.175,0.14, 0.085, 0.0 /
C.
      DATA effic_pmt  / 0.02, 0.025,0.03, 0.035,0.04, 0.05, 0.075,0.09,
     &                  0.12, 0.14, 0.15, 0.175,0.185,0.20, 0.21, 0.22,
     &                  0.25, 0.26, 0.27, 0.28, 0.30, 0.30, 0.295,0.29,
     &                  0.285,0.28, 0.26, 0.20, 0.175,0.10, 0.05, 0.0 /
C.

      Do i = 1, n_med
C.
         CALL gstmed(i, name_med(i), nmed_mat(i), isvol_med(i), 
     *                  ifield(i), fieldm(i), tmaxfd_med(i), 
     *                  dmaxms_med(i), deemax_med(i),
     *                  epsil_med(i), stmin_med(i), ubuf_med(i), 1)
C.
         If(i.ge.9.and.i.le.15)then	! dielectric - scintillator
C.           CALL ucopy(absco_scnt,absco,npckov)
           CALL vfill(absco,npckov,bulk_absorption)
           CALL ucopy(effic_pmt,effic,npckov)
           CALL ucopy(rindex_scnt,rindex,npckov)
           CALL gsckov(i,npckov,ppckov,absco,effic,rindex)
         Elseif(i.eq.8.or.i.eq.1)then	! dielectric - air and vacuum
           CALL vzero(effic,npckov)
           CALL vfill(absco,npckov,1.e10)
           CALL vfill(rindex,npckov,1.00)
           CALL gsckov(i,npckov,ppckov,absco,effic,rindex)
         Elseif(i.eq.17)then            ! dielectric - glass
           CALL vzero(effic,npckov)
           CALL vfill(absco,npckov,10.0)
           CALL vfill(rindex,npckov,1.50)
           CALL gsckov(i,npckov,ppckov,absco,effic,rindex)
        Elseif(i.eq.6.or.i.eq.16)then	! metal - Al and MgO
           CALL vzero(effic,npckov)
           CALL vzero(rindex,npckov)
           CALL vfill(absco,npckov,paint_absorption)
           CALL gsckov(i,npckov,ppckov,absco,effic,rindex)
         Endif
      Enddo
C.
      CALL ugstmed_trgt
C.
      RETURN
      END
C.













