C.
      SUBROUTINE ugeo_detector
C.
************************************************************************
*                                                                      *
*                          Define the Detector                         *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'geometry.inc'            !local
      include 'uggeom.inc'
      include 'rescom.inc'              !local
C.
C *** Local variables
C.
      INTEGER ivol, irot, nxtrotm, irot_side, irot_box, irot_col
C.
      REAL the1, phi1, the2, phi2, the3, phi3, col_collar_inner_radius
C.
      REAL x, y, z, shape(4)
      REAL pbpl_length, pblg_length, pb_thin, pbsm_length, pb_thick
      REAL box_height, beam_height
      REAL bcr1_length, bcr2_length, bcr3_length, bcr4_length
C      print*, 'ugeom_detector'

     
C.
      hexagon_small_width = s_finger + 2.*d_air(1) + 2.*d_mtl + air_gap
      hexagon_large_width = 2. * hexagon_small_width / sqrt(3.)
      depth = z_finger + d_air(2) + d_mtl
      col_length = 15.24 
C.
C.--->the x width has an offset of 3.35 to account for two detectors
C.--->being pulled back by 6.7cm to make room for lead collimators
      shape( 1) =          depth + 3.35     		 ! half x width
      shape( 2) =  5  * hexagon_large_width 		 ! half y width
      shape( 3) =  3. * hexagon_small_width + col_length/2. ! half z width
      shape( 3) = TLrms
C.
      shape( 1) = shape( 1) + box_width/2. + pmt_length
C.
C.      CALL gsvolu('DETE', 'BOX ', mcent, shape, 3, ivol)
      CALL gsvolu('DETE', 'BOX ', 22, shape, 3, ivol)
      CALL gsatt('DETE','SEEN',0)
C.
      CALL gspos('DETE', 1, 'WRLD', 0.0, 0.0, 0.0, 0, 'MANY')
C.
      box_length = 17.069
      box_height = 20.0
      beam_height = - box_height/2. + 3.171
C.
      shape( 1) = box_width/2.
      shape( 2) = box_height/2.
      shape( 3) = box_length/2.
C.
      CALL gsvolu('CMBR','BOX ', 20, shape, 3, ivol)
      CALL gsatt('CMBR','SEEN',1)
C.
      shape( 1) = shape( 1) - wall(2)
      shape( 2) = shape( 2) - wall(2)/2.
      shape( 3) = shape( 3) - wall(2)
C.
      CALL gsvolu('CMBG','BOX ', mcent, shape, 3, ivol)
      CALL gsatt('CMBG','SEEN',1)
C     JS change CMBG to ONLY, MANY didn't work with solid target, ONLY does.
      CALL gspos('CMBG', 1, 'CMBR', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      y = beam_height
C.
      CALL gspos('CMBR', 1, 'DETE', 0.0, y, 0.0, 0, 'MANY')
C.
C.
C.***************************************************************
C.
C.    Make the upstream collimator assembly for the gas cell
C.
C.***************************************************************
C.
      shape( 1) = 0.0
      shape( 2) = aprt + wall(3)
      shape( 3) = col_length/2.
C.      
      CALL gsvolu('CTUB','TUBE', 6, shape, 3, ivol)
      CALL gsatt('CTUB','SEEN',1)
      
C.
      shape( 2) = shape( 2) - 0.357
      shape( 3) = shape( 3)/3.
      CALL gsvolu('CL1G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CL1G','SEEN',1)
      z = 2.*shape(3)
      CALL gspos('CL1G', 1, 'CTUB', 0.0, 0.0, z, 0, 'ONLY')
C.
      shape( 2) = shape( 2) - 0.0711
      CALL gsvolu('CL2G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CL2G','SEEN',1)
      CALL gspos('CL2G', 1, 'CTUB', 0.0, 0.0, 0.0, 0, 'ONLY')
C.
      shape( 2) = shape( 2) - 0.0699
      CALL gsvolu('CL3G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CL3G','SEEN',1)
      z = -2.*shape(3)
      CALL gspos('CL3G', 1, 'CTUB', 0.0, 0.0, z, 0, 'ONLY')
C.
C. ---> collimator end collar detail inside and outside box
C.
      col_collar_length = 0.945
      col_collar_inner_radius = shape(2)
C.
      shape(1) = col_collar_inner_radius
      shape(2) = 1.905
      shape(3) = col_collar_length/2.	!end collar detail half thickness
      CALL gsvolu('COLR','TUBE', 7, shape, 3, ivol)
      CALL gsatt('COLR','SEEN',1)
      z = -col_length/2. + shape(3)
      CALL gspos('COLR',1,'CTUB',0.0,0.0,z,0,'ONLY')
C.      
      z = -col_length/2.+ col_collar_length + shape(3) 
     &    + wall(2)
      CALL gspos('COLR',2,'CTUB',0.0,0.0,z,0,'ONLY')
C.
C.
C. ---> Position upstream collimator
      z = box_length/2. + col_length/2.  - wall(2) 
     &    - col_collar_length
C.
      irot_side = nxtrotm()            ! opposite side rotating matrix
C.
      the1 =  270.
      phi1 =  180.
      the2 =   90.
      phi2 =   90.
      the3 =  180.
      phi3 =    0.
C.
C. ---> Position downstream collimator on other side of the box
      CALL gsrotm(irot_side,the1,phi1,the2,phi2,the3,phi3)
      CALL gspos('CTUB', 1, 'DETE', 0.0, 0.0, -z, irot_side, 'ONLY')
C.
C.
C.***************************************************************
C.
C.    Make the downstream collimator assembly for the gas cell
C.
C.***************************************************************
C.

      shape( 1) = 0.0
      shape( 2) = aprt + wall(3)
c        write(6,*)'***LOOK*** aprt: ',aprt,' wall(3): ',wall(3)
      shape( 3) = col_length/2.      
      CALL gsvolu('CTBD','TUBE', 6, shape, 3, ivol)
      CALL gsatt('CTBD','SEEN',1)      

cc MT makes new 2" collimator that replaces 6" collimator

      shape( 1) = 0.0
      shape( 2) = 0.64135
      shape( 3) = 1.275
      CALL gsvolu('CL4G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CL4G','SEEN',1)
      z = -1.0*(14.892-8.975)
      CALL gspos('CL4G', 1, 'CTBD', 0.0, 0.0, z, 0, 'ONLY')

      shape( 1) = 0.0
      shape( 2) = 0.71755
      shape( 3) = 1.275
      CALL gsvolu('CL5G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CL5G','SEEN',1)
      z = -1.0*(14.892-11.525)
      CALL gspos('CL5G', 1, 'CTBD', 0.0, 0.0, z, 0, 'ONLY')

cc fill rest of CTBD with gas volumes CT1G and CT2G...

      shape( 1) = 0.0
      shape( 2) = aprt + wall(3) - 0.1
      shape( 3) = (col_length/2.0 + (14.892-11.525-1.275))/2.0
      CALL gsvolu('CT2G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CT2G','SEEN',1)
      z = col_length/2.0 - shape( 3)
      CALL gspos('CT2G', 1, 'CTBD', 0.0, 0.0, z, 0, 'ONLY')

      shape( 1) = 0.0
      shape( 2) = aprt + wall(3) - 0.1
      shape( 3) = ((8.975-1.275)-(14.892-col_length/2.0))/2.0
      CALL gsvolu('CT1G','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('CT1G','SEEN',1)
      z = -1.0*(col_length/2.0) + shape( 3)
      CALL gspos('CT1G', 1, 'CTBD', 0.0, 0.0, z, 0, 'ONLY')


C. ---> collimator end collar detail inside and outside box

      col_collar_length = 0.945

      shape(1) = col_collar_inner_radius      
      shape(2) = 1.905
      shape(3) = col_collar_length/2.	!end collar detail half thickness
      CALL gsvolu('CLRD','TUBE', 7, shape, 3, ivol)
      CALL gsatt('CLRD','SEEN',1)
      z = -col_length/2. + shape(3)
      CALL gspos('CLRD',1,'CTBD',0.0,0.0,z,0,'ONLY')      
      z = -col_length/2.+ col_collar_length + shape(3) 
     &    + wall(2)
      CALL gspos('CLRD',2,'CTBD',0.0,0.0,z,0,'ONLY')

C. ---> Position downstream collimator
      z = box_length/2. + col_length/2.  - wall(2) 
     &    - col_collar_length
c        write(6,*)'***LOOK*** position: ',z

      CALL gspos('CTBD', 1, 'DETE', 0.0, 0.0, z, 0, 'ONLY')

C.      
C.
C.***************************************************************
C.
C.    Make the aluminum collars around the beam tube
C.
C.***************************************************************
C.
C.
      		
      bcr1_length = 1.96		! fifth Al section after box
      bcr2_length = 2.68		! fourth Al section after box
      bcr3_length = 3.55		! third Al section after box
      bcr4_length = 3.67		! second Al section after box
C.
      shape(1) = 2.53      
      shape(2) = 5.71
      shape(3) = bcr1_length/2.	
      CALL gsvolu('BCR1','TUBE', 6, shape, 3, ivol)
      CALL gsatt('BCR1','SEEN',1)
      z = -col_length/2.+ col_collar_length + shape(3) 
     &    + wall(1) + wall(2) + bcr4_length + bcr3_length 
     &    + bcr2_length + bcr1_length/2.
      CALL gspos('BCR1',1,'CTUB',0.0,0.0,z,0,'ONLY')
C. As collimators are not symmetric in new pumping tubes, need two BCR1s
      CALL gspos('BCR1',1,'CTBD',0.0,0.0,z,0,'ONLY')
C.
      shape(1) = 2.07      
      shape(2) = 2.53
      shape(3) = bcr2_length/2.
      CALL gsvolu('BCR2','TUBE', 6, shape, 3, ivol)
      CALL gsatt('BCR2','SEEN',1)
      z = -col_length/2.+ col_collar_length + shape(3) 
     &    + wall(1) + wall(2) + bcr4_length + bcr3_length 
     &    + bcr2_length/2.
      CALL gspos('BCR2',1,'CTUB',0.0,0.0,z,0,'ONLY')
C. As collimators are not symmetric in new pumping tubes, need two BCR2s
      CALL gspos('BCR2',1,'CTBD',0.0,0.0,z,0,'ONLY')
C.
      shape(1) = 1.30      
      shape(2) = 2.07
      shape(3) = bcr3_length/2.
      CALL gsvolu('BCR3','TUBE', 6, shape, 3, ivol)
      CALL gsatt('BCR3','SEEN',1)
      z = -col_length/2.+ col_collar_length + shape(3) 
     &    + wall(1) + wall(2) + bcr4_length + bcr3_length/2.
      CALL gspos('BCR3',1,'CTUB',0.0,0.0,z,0,'ONLY')
C. As collimators are not symmetric in new pumping tubes, need two BCR3s
      CALL gspos('BCR3',1,'CTBD',0.0,0.0,z,0,'ONLY')
C.      
C.
C.***************************************************************
C.
C.    Make the inner gas cell assembly
C.
C.***************************************************************
C.
C.---> Aluminum outer shell of the cell
C.
      shape(1) = 2.115		! bottom of trap auto calculate
      shape(2) = 6.759		! top of trap dimension
      shape(3) = 1.905		! width of trap
      shape(4) = 4.208		! height of trap
C.
      irot_box = nxtrotm()	! orient the trapezoid box correctly
C.
      the1 =  180.		! theta x
      phi1 =    0.		! phi x
      the2 =  270.		! theta y
      phi2 =  180.		! phi y
      the3 =   90.		! theta z
      phi3 =   90.		! phi z
C.
      CALL gsrotm(irot_box,the1,phi1,the2,phi2,the3,phi3)
C.
C. JS removed CELL from solid target simulations!
C.
C.    CALL gsvolu('CELL','TRD1', 6, shape, 4, ivol)
C.    CALL gsatt('CELL','SEEN',1)
C.--->move inner cell so that apertures are centred on beam line
C.--->0.976 offset is height from box top to inner cell top
      y = .5 * box_height - shape(4) - 0.976   
C.    CALL gspos('CELL',1,'CMBG',0.0, y ,0.0,irot_box,'ONLY')      
C.
C.---> Vacuum inner shell of the cell
C.
      shape(1) = 1.798
      shape(2) = 6.094
      shape(3) = 1.588
      shape(4) = 3.891
C.
C.    CALL gsvolu('CELG','TRD1', mcent, shape, 4, ivol)
C.    CALL gsatt('CELG','SEEN',1)
C.    CALL gspos('CELG',1,'CELL',0.0,0.0,0.0,0,'ONLY')      
C.
C.
C.---> Entrance collimator to the trapezoid target
C.
      shape(1) = 0.0
c old     shape(2) = 0.3
cc MT updates gas target entrance collimator 21 Oct 2003.
      shape(2) = 0.2
      shape(3) = 0.5
C.
C. JS removes EAPG from solid target simulations!
C.
C.    CALL gsvolu('EAPG','TUBE', mcent, shape, 3, ivol)
C.    CALL gsatt('EAPG', 'SEEN', 1)
C.
      irot_col = nxtrotm()	! orient the collimators correctly
C.
      the1 =  180.		! theta x
      phi1 =    0.		! phi x
      the2 =   90.		! theta y
      phi2 =   90.		! phi y
      the3 =   90.		! theta z
      phi3 =    0.		! phi z
C.
      CALL gsrotm(irot_col,the1,phi1,the2,phi2,the3,phi3)
      z = 2.008
      x = + 5.315
C.    CALL gspos('EAPG', 1, 'CELL', x, 0.0, z, irot_col, 'ONLY')
C.
C.
C.---> Exit collimator to the trapezoid target
C.
      shape(1) = 0.0
c old     shape(2) = 0.4
cc MT updates gas target exit collimator 21 Oct 2003.
      shape(2) = 0.5
      shape(3) = 0.5     
C.
C. JS removes XAPG from solid target simmulation!
C.
C.    CALL gsvolu('XAPG','TUBE', mcent, shape, 3, ivol)
C.    CALL gsatt('XAPG', 'SEEN', 1)
      z = 2.008
      x = -5.315
C.    CALL gspos('XAPG', 1, 'CELL', x, 0.0, z, irot_col, 'ONLY')
C.
C.
C.***************************************************************
C.
C.    Make the solid carbon target
C.
C.***************************************************************
C.
      y = -1.0 * beam_height
C     entdens is length from targ entrance to targ center
C     exitdens is length from targ exit to targ center
C     Rather than defining a new variable for solid target half lengths (and
C     be forced to change several other files) JS has used the same variables
C     despite the poor name for solid targets.
C
C     The width and density are exaggerated 100x
      entdens = 1.111e-3
      exitdens = 1.111e-3
      shape(1) = 0.000           ! inner disc radius
      shape(2) = 1.200           ! outer disc radius
      shape(3) = entdens         ! half width of disc
C.        create rotation matrix to aline z-axis of disk with x-axis of CELG
C.    CALL GSROTM(54, 180.0, 0.0, 90.0, 90.0, 90.0, 0.0)
C.
      CALL gsvolu('CTAR', 'TUBE', 21, shape, 3, ivol)
C     CALL gspos ('CTAR' 1, 'CELG' 0.0, 0.0, z, 54, 'ONLY')
      CALL gspos('CTAR', 1, 'CMBG', 0.0, y, 0.0, 0, 'ONLY')  
C.      
C.
C.
C.
C.***************************************************************
C.
C.    Make the lead collimator and plate around the entrance
C.    collimator/pumping tube
C.
C.***************************************************************
C.
C.
      pb_thick    = 0.635			! thick lead (1/4")
      pb_thin     = 0.3175			! thin lead (1/8")
      pbpl_length = pb_thick + pb_thin		! lead plate thickness
      pblg_length = bcr2_length + bcr3_length
     &            - pbpl_length - 0.5*pb_thin	! large lead collar length
      pbsm_length = bcr4_length - 0.5*pb_thin	! small lead collar length
C.
C.
      shape(1) = 7.75			! lead plate half width
      shape(2) = 11.75			! lead plate half height
      shape(3) = pbpl_length/2  	! lead plate half thickness
      CALL gsvolu('PBPL','BOX ', 7, shape, 3, ivol)
      CALL gsatt('PBPL','SEEN',1)
      z = box_length/2. + pbsm_length + col_collar_length + pb_thin 
     &    + pblg_length + pbpl_length/2.
      CALL gspos('PBPL',1,'DETE',0.0,0.0,-z,irot_side,'ONLY')
C.
      shape(1) = 3.2 - pb_thick
      shape(2) = 3.2
      shape(3) = pblg_length/2		! GEANT requires 1/2 length
      CALL gsvolu('PBLG','TUBE', 7, shape, 3, ivol)
      CALL gsatt('PBLG','SEEN',1)
      z = box_length/2. + pbsm_length+ col_collar_length + pb_thin
     &    + pblg_length/2.
      CALL gspos('PBLG',1,'DETE',0.0,0.0,-z,irot_side,'ONLY')
C.
      shape(1) = 1.25
      shape(2) = 2.88
      shape(3) = pb_thin/2		! GEANT requires 1/2 length
      CALL gsvolu('PBDK','TUBE', 7, shape, 3, ivol)
      CALL gsatt('PBDK','SEEN',1)
      z = box_length/2. + pbsm_length +col_collar_length+pb_thin/2.
      CALL gspos('PBDK',1,'DETE',0.0,0.0,-z,irot_side,'ONLY')
C.
      shape(1) = 1.25 - pb_thick
      shape(2) = 1.25
      shape(3) = pbsm_length/2		! GEANT requires 1/2 length
      CALL gsvolu('PBSM','TUBE', 7, shape, 3, ivol)
      CALL gsatt('PBSM','SEEN',1)
      z = box_length/2. + pbsm_length/2. + col_collar_length
      CALL gspos('PBSM',1,'DETE',0.0,0.0,-z,irot_side,'ONLY')
C.
      RETURN
      END
C.
      SUBROUTINE ugeo_finger
C.
************************************************************************
*                                                                      *
*                  Define the BSO finger modules                       *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gcunit.inc'              !geant
      include 'gcflag.inc'              !geant
C.
      include 'mask.inc'                !local
      include 'geometry.inc'            !local
      include 'uggeom.inc'              !local
      include 'uenergy.inc'             !local
C.
C *** Local variables
C.
      INTEGER i, j, k, jm, icopy, ivol
C.
      REAL x, y, z, shape(10)
C.
      INTEGER irot_front, irot_back, nxtrotm
C.
      REAL the1, phi1, the2, phi2, the3, phi3

C      print*, 'ugeo_finger'
C.
C.*** Make the housing for scintillator and PMT
C.
      shape( 1) =   0.0
      shape( 2) = 360.0
      shape( 3) =   6.0
      shape( 4) =   2.0
      shape( 5) = -(d_mtl+d_air(2)+z_finger+pmt_length)/2.
      shape( 6) =   0.0
      shape( 7) = hexagon_small_width/2.
      shape( 8) =  (d_mtl+d_air(2)+z_finger+pmt_length)/2.
      shape( 9) =   0.0
      shape(10) = shape(7)
C.
      CALL gsvolu('HSNG', 'PGON', 8, shape, 10, ivol)
      CALL gsatt('HSNG','SEEN',1)
C      print*, '1'
C.
C *** Make the fingers ***
C.
      shape( 5) = shape( 5) + pmt_length/2.
      shape( 7) = shape( 7) - air_gap/2.
      shape( 8) = shape( 8) - pmt_length/2.
      shape(10) = shape(10) - air_gap/2.
C.
      CALL gsvolu('FNGR', 'PGON',  6, shape, 10, ivol)
      CALL gsatt('FNGR','SEEN',1)
C.
      z = -pmt_length/2.
C.
      CALL gspos('FNGR', 1, 'HSNG', 0.0, 0.0, z, 0,'ONLY') 
C.
      shape( 7) = shape( 7) - d_mtl
      shape(10) = shape(10) - d_mtl
      shape( 5) = shape( 5) + d_mtl/2.
      shape( 8) = shape( 8) - d_mtl/2.
C.
      z = d_mtl/2.
C.
      CALL gsvolu('MGOR', 'PGON', 16, shape, 10, ivol)
      CALL gsatt('MGOR','SEEN',1)
C.
      CALL gspos('MGOR', 1, 'FNGR', 0.0, 0.0, z, 0,'ONLY')
C.
      shape( 7) = shape( 7) - d_air(1)
      shape(10) = shape(10) - d_air(1)
      shape( 5) = shape( 5) + d_air(2)/2.
      shape( 8) = shape( 8) - d_air(2)/2.
C.
      z = d_air(2)/2.
C.
      CALL gsvolu('SCNT', 'PGON', n_detmate, shape, 10, ivol)
      CALL gsatt('SCNT','SEEN',1)
C.
      CALL gspos('SCNT', 1, 'MGOR', 0.0, 0.0, z, 0,'ONLY')
C      print*, '2'
C.
C *** Make two position rotation matrices ***
C.
      irot_front = nxtrotm()            ! front
C.
      the1 =  90.
      phi1 =  90.
      the2 =   0.
      phi2 =   0.
      the3 =  90.
      phi3 =   0.
C.
      CALL gsrotm(irot_front,the1,phi1,the2,phi2,the3,phi3)
C.
      irot_back = nxtrotm()		! back
C.
      the1 =  90.
      phi1 =  90.
      the2 = 180.
      phi2 =   0.
      the3 =  90.
      phi3 = 180.
C.
      CALL gsrotm(irot_back,the1,phi1,the2,phi2,the3,phi3)
C      print*, '3'
C.
C.--> Fill square DETE with hexagonal housings HSNG
C.
      icopy = 0
C.
      k = 1
C.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2. -7.2
      y = - aprt - wall(3) - (1./2.) * hexagon_large_width
      z =   (5./2.)*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 2
C.
      x = d_mtl/2. + d_air(2)/2. - pmt_length/2.      
      y = - aprt - wall(3) - (5./4.) * hexagon_large_width
      z =   2.*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2. - 6.7
      y =   aprt + wall(3) + (1./2.)* hexagon_large_width
      z =   2.*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 3
C.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y =   aprt + wall(3) + (5./4.)* hexagon_large_width
      z =   (3./2.)*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 5
C.
      y =  (9./8.) * hexagon_large_width
      z =  hexagon_small_width/2.
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 7
C.
      y =  (9./8.) * hexagon_large_width
      z = -hexagon_small_width/2.
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 9
C.
      y =   aprt + wall(3) + (5./4.)* hexagon_large_width
      z = - (3./2.) * hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 10
C.
      y = - aprt - wall(3) - (5./4.) * hexagon_large_width
      z = - 2.* hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y =   aprt + wall(3) + (1./2.)* hexagon_large_width
      z = - 2.*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      k = 11
C.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y = - aprt - wall(3) - (1./2.) * hexagon_large_width
      z = - (5./2.)*hexagon_small_width
C.
      icopy = icopy + 1
C.
      x_fngr(icopy)  = 0.0
      y_fngr(icopy)  =  y
      z_fngr(icopy)  =  z 
C.
      If(mask(icopy).eq.1)then
        CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
      Endif
C.
      Do k = 9, 3, -1
C.
         z = -3./2.*hexagon_small_width + (k-3)*hexagon_small_width/2.
C.
         jm = 2
         if(k.eq.3.or.k.eq.5.or.k.eq.7.or.k.eq.9)jm = 1
C.
         Do j = 1, jm
C.
            x = + box_width/2. + (depth + pmt_length)/2.
C.
            If(jm.eq.1)then
              y = - (3./8.) * hexagon_large_width
            Else
              y = - (9./8.) * hexagon_large_width
            Endif
C.
            y = y + (j-1) * (3./2.) * hexagon_large_width
C.
            icopy = icopy + 1
C.
            x_fngr(icopy)  = x - pmt_length/2.
            y_fngr(icopy)  = y
            z_fngr(icopy)  = z 
C.
            If(mask(icopy).eq.1)then
              CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_back,'ONLY')
            Endif
C.
            x = -x
C.
            icopy = icopy + 1
C.
            x_fngr(icopy)  = x + pmt_length/2.
            y_fngr(icopy)  = y
            z_fngr(icopy)  = z 
C.
            If(mask(icopy).eq.1)then
              CALL gspos('HSNG',icopy,'DETE',-x,y,-z,irot_front,'ONLY')
            Endif
C.
         Enddo
C.
      Enddo
C      print*, '4'
C.
      If(iswit(4).eq.1)then
        Do i = 1, max_hexagon
           write(lunits(3),*)i,x_fngr(i),y_fngr(i),z_fngr(i)
        Enddo
      Endif
C      print*, '5'
      do i = 1, Nn
       do j = 1, max_hexagon
C       print*, n_fngr(i,j)
       enddo
      enddo
C.
      CALL neighborhood(n_fngr)
C      print*, 'end ugeo_finger'
C.
      RETURN
C.
      END
C.
      SUBROUTINE ugeo_pmt
C.
************************************************************************
*                                                                      *
*                     Define the PMT plate and PMTs                    *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'geometry.inc'            !local
C.
C *** Local variables
C.
      INTEGER ivol
C.
      REAL z, shape(3)
C.
      print*, 'ugeo_pmt'
      If(mtype_pmt.eq.1)then
        shape( 1) =  0.0
        shape( 2) =  pmt_size
        shape( 3) =  pmt_length/2.
        CALL gsvolu('PMT ', 'TUBE', 17, shape, 3, ivol)
        CALL gsatt('PMT ','SEEN',1)
      Else
        shape( 1) = pmt_size
        shape( 2) = pmt_size
        shape( 3) = pmt_length/2.
        CALL gsvolu('PMT ', 'BOX ', 17, shape, 3, ivol)
        CALL gsatt('PMT ','SEEN',1)
      Endif
C.
      z = depth/2.
C.
      CALL gspos('PMT ', 1, 'HSNG', 0.0, 0.0, z, 0, 'ONLY') 
C.
      RETURN
      END
C.
      SUBROUTINE udet
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     UDET is a user routine which defines certain volumes to be       C
C     detectors and defines what information each detector collects.   C
C     For convenience the detector type variable (IDTYPE) is used      C
C     for some volumes:                                                C
C                                                                      C
C                       1 - Detector Crystal                           C
C                       2 - PM Tube                                    C
C                       3 - Silicon                                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      include 'geometry.inc'		! local
C.
      INTEGER i, idtyp, iset, idet
C.
      INTEGER nbitsv
C.
      CHARACTER*4 name
C.
      INTEGER nhdim
      PARAMETER (nhdim=5)
C.
      CHARACTER*4 chnamh(nhdim)
C.
      INTEGER nbitsh_dedx, nbitsh_time, nbitsh_coor, nbitsh(nhdim)
C.
      REAL orig_dedx, orig_time, orig_coor, orig(nhdim)
      REAL fact_dedx, fact_time, fact_coor, fact(nhdim)
C.
      DATA nbitsh_dedx, nbitsh_time, nbitsh_coor / 32  ,    32,    32 /
      DATA orig_dedx,   orig_time,   orig_coor  / 0.  , 1000.,  500.  /
      DATA fact_dedx,   fact_time,   fact_coor  / 1.E6,  1.E5, 1000.  /
C.
************************************************************************
*                                                                      *
*                          Detector Scintillator		       *
*                                                                      *
************************************************************************
C.
      idtyp = 1
C.
      chnamh(1) = 'X   '
      chnamh(2) = 'Y   '
      chnamh(3) = 'Z   '
      chnamh(4) = 'TIME'
      chnamh(5) = 'EDEP'
C.
      CALL ufill(nbitsh,1,3,nbitsh_coor)
      nbitsh(4) = nbitsh_time
      nbitsh(5) = nbitsh_dedx
C.
      CALL ufill(orig,1,3,orig_coor)
      orig(4) = orig_time
      orig(5) = orig_dedx
C.
      CALL ufill(fact,1,3,fact_coor)
      fact(4) = fact_time
      fact(5) = fact_dedx
C.
      nbitsv = 10
C.
      name = 'HSNG'
      CALL gsdet('SCNT',name,1,name,nbitsv,idtyp,100,0,iset,idet)
      CALL gsdeth('SCNT',name,5,chnamh,nbitsh,orig,fact)
C.
************************************************************************
*                                                                      *
*                                 PM Tube                              *
*                                                                      *
************************************************************************
C.
      idtyp = 2
C.
      name = 'PMT '
C.
      CALL gsdet('PMT ',name,1,name,nbitsv,idtyp,100,0,iset,idet)
      CALL gsdeth('PMT ',name,5,chnamh,nbitsh,orig,fact)

***********************************************************************

      RETURN
      END

















































