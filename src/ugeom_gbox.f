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
      include 'rescom.inc'
C.
C *** Local variables
C.
      INTEGER ivol, irot, nxtrotm, irot_side, irot_box, irot_col
C.
      REAL the1, phi1, the2, phi2, the3, phi3, col_collar_inner_radius
C.
      REAL x, y, z, shape(4)
      REAL bpa_len, bpb_len, bpc_len, bpd_len, bpe_len,
     &     bpf_len, bpg_len, bph_len, bpi_len, bpj_len, bpa_in_len,
     &     bpj_len2
      REAL pbpl_length, pblg_length, pb_thin, pbsm_length, pb_thick
      REAL box_height, beam_height
      REAL bcr1_length, bcr2_length, bcr3_length, bcr4_length
C      print*, 'ugeom_detector'

C.    Define Geometry
      CALL ugeo_defin



      If(targtype.eq.1 .or. alpha)then
         mcent = 1
         mbox = 1
         ment(1) = 1
         ment(2) = 1
         ment(3) = 1
         mex(1) = 1
         mex(2) = 1
         mex(3) = 1
      endif
      if(alpha)mtarg=1


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
      CALL gsvolu('DETE', 'BOX ', 22, shape, 3, ivol)
      CALL gsatt('DETE','SEEN',0)
C.
      CALL gspos('DETE', 1, 'WRLD', 0.0, 0.0, 0.0, 0, 'ONLY')
      
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
      
      CALL gsvolu('CMBG','BOX ', mbox, shape, 3, ivol)
      
      CALL gsatt('CMBG','SEEN',1)
C.    CMBG needs to be ONLY for solid target sims
      if (targtype.eq.0)then
         CALL gspos('CMBG', 1, 'CMBR', 0.0, 0.0, 0.0, 0, 'MANY')
      elseif (targtype.eq.1)then
         CALL gspos('CMBG', 1, 'CMBR', 0.0, 0.0, 0.0, 0, 'ONLY')
      endif
C.
      y = beam_height
C.
c     CR changed to ONLY
      CALL gspos('CMBR', 1, 'DETE', 0.0, y, 0.0, 0, 'ONLY')

C
C.---> Collimator hole through outer aluminum  gas cell box
C. changed material & z -> -z from DG's sim - CR. Need irot?
      shape( 1) = 0.0
      shape( 2) = 0.4
      shape( 3) = wall(2)/2.
     
      CALL gsvolu('UHOL','TUBE', ment(1), shape, 3, ivol)
       
      CALL gsatt('UHOL','SEEN',1)
      y = -beam_height
      z = box_length/2. - wall(2)/2.
      CALL gspos('UHOL', 1, 'CMBR', 0.0, y, -z, 0, 'ONLY')
C.
C.---> Aluminum Collimator on the inside of the gas cell box
C.
      bpa_in_len = 0.472
      shape( 1) = 0.0                   
      shape( 2) = 1.905                   
      shape( 3) = bpa_in_len         
      CALL gsvolu('PUAI','TUBE', 6, shape, 3, ivol)
c changed to Aluminium
      CALL gsatt('PUAI','SEEN',1)
      y = -beam_height
      z = box_length/2. - wall(2) - bpa_in_len
      CALL gspos('PUAI', 1, 'CMBG', 0.0, y, -z, 0, 'ONLY')
C.
      shape( 1) = 0.0
      shape( 2) = 0.4
      shape( 3) = bpa_in_len

      CALL gsvolu('PUBI','TUBE', ment(1), shape, 3, ivol)
      CALL gsatt('PUBI','SEEN',1)
      CALL gspos('PUBI', 1, 'PUAI', 0.0,0.0, 0.0, 0, 'ONLY')
C.
C. ---> collimator end collar detail outside box
C.
      bpa_len = 0.472
      bpb_len = 1.437
      bpc_len = 0.321
      bpd_len = 0.159
      bpe_len = 2.060
      bpf_len = 0.499
      bpg_len = 0.476
      bph_len = 0.980
      bpi_len = 0.585
      bpj_len = 0.350 
c     bpj_len2 = 0.467
C.
      shape(1) = 0.0     
      shape(2) = 1.905
      shape(3) = bpa_len  !end collar detail half thickness
      CALL gsvolu('PUA1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUA1','SEEN',1)
      z =  box_length/2 + bpa_len
      CALL gspos('PUA1',1,'DETE',0.0,0.0,-z,0,'ONLY')

      shape(1) = 0.0      
      shape(2) = 0.4
      shape(3) = bpa_len	!end collar detail half thickness
      CALL gsvolu('PUA2','TUBE', ment(1), shape, 3, ivol)
c changed to vac.
      CALL gsatt('PUA2','SEEN',1)
      CALL gspos('PUA2',1,'PUA1',0.0,0.0,0.0,0,'ONLY')
C.  
C.----> section BPB next to the left of collimator detail outside box    
C.
      shape(1) = 0.0
      shape(2) = 1.353
      shape(3) = bpb_len  !end collar detail half thickness
      CALL gsvolu('PUB1','TUBE', 7, shape, 3, ivol)
c changed to Lead.
      CALL gsatt('PUB1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + bpb_len
      CALL gspos('PUB1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.035
      shape(3) = bpb_len  !end collar detail half thickness
      CALL gsvolu('PUB2','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUB2','SEEN',1)
      CALL gspos('PUB2',1,'PUB1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.4
      shape(3) = bpb_len  !end collar detail half thickness
      CALL gsvolu('PUB3','TUBE', ment(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUB3','SEEN',1)
      CALL gspos('PUB3',1,'PUB2',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPC next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 1.353
      shape(3) = bpc_len  !end collar detail half thickness
      CALL gsvolu('PUC1','TUBE', 7, shape, 3, ivol)
c changed to Lead
      CALL gsatt('PUC1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + bpc_len
      CALL gspos('PUC1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.035
      shape(3) = bpc_len  !end collar detail half thickness
      CALL gsvolu('PUC2','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUC2','SEEN',1)
      CALL gspos('PUC2',1,'PUC1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.45  
      shape(3) = bpc_len  !end collar detail half thickness
      CALL gsvolu('PUC3','TUBE', ment(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUC3','SEEN',1)
      CALL gspos('PUC3',1,'PUC2',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPD next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 2.88
      shape(3) = bpd_len  !end collar detail half thickness
      CALL gsvolu('PUD1','TUBE', 7, shape, 3, ivol)
c changed to Lead.
      CALL gsatt('PUD1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + bpd_len
      CALL gspos('PUD1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.035
      shape(3) = bpd_len  !end collar detail half thickness
      CALL gsvolu('PUD2','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUD2','SEEN',1)
      CALL gspos('PUD2',1,'PUD1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0 
      shape(2) = 0.45 
      shape(3) = bpd_len  !end collar detail half thickness
      CALL gsvolu('PUD3','TUBE', ment(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUD3','SEEN',1)
      CALL gspos('PUD3',1,'PUD2',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPE next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 3.20 
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PUE1','TUBE', 7, shape, 3, ivol)
c changed to Lead.
      CALL gsatt('PUE1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + bpe_len
      CALL gspos('PUE1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0 
      shape(2) = 2.53
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PUE2','TUBE', 8, shape, 3, ivol)
c changed to Atmosphere.
      CALL gsatt('PUE2','SEEN',1)
      CALL gspos('PUE2',1,'PUE1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 2.09
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PUE3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUE3','SEEN',1)
      CALL gspos('PUE3',1,'PUE2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.45
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PUE4','TUBE', ment(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUE4','SEEN',1)
      CALL gspos('PUE4',1,'PUE3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPF next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 3.2 
      shape(3) = bpf_len
      CALL gsvolu('PUF1','TUBE', 7, shape, 3, ivol)
c changed to Lead.
      CALL gsatt('PUF1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + bpf_len   
      CALL gspos('PUF1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 2.53
      shape(3) = bpf_len  
      CALL gsvolu('PUF2','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUF2','SEEN',1)
      CALL gspos('PUF2',1,'PUF1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 2.09   
      shape(3) = bpf_len  !end collar detail half thickness
      CALL gsvolu('PUF3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUF3','SEEN',1)
      CALL gspos('PUF3',1,'PUF2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bpf_len 
      CALL gsvolu('PUF4','TUBE', ment(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUF4','SEEN',1)
      CALL gspos('PUF4',1,'PUF3',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.04
      shape(3) = bpf_len
      CALL gsvolu('PUF5','TUBE', 6, shape, 3, ivol)
      CALL gsatt('PUF5','SEEN',1)
      CALL gspos('PUF5',1,'PUF4',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.5
      shape(3) = bpf_len
      CALL gsvolu('PUF6','TUBE', ment(3), shape, 3, ivol)
      CALL gsatt('PUF6','SEEN',1)
      CALL gspos('PUF6',1,'PUF5',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPG next in line to left of box
C.
      shape(1) = 7.75 
      shape(2) = 11.75 
      shape(3) = bpg_len                                   
      CALL gsvolu('PUG1','BOX ', 7, shape, 3, ivol)
c changed to Lead.
      CALL gsatt('PUG1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len      
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + bpg_len
      CALL gspos('PUG1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 2.53  
      shape(3) = bpg_len                                   
      CALL gsvolu('PUG2','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUG2','SEEN',1)
      CALL gspos('PUG2',1,'PUG1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bpg_len 
      CALL gsvolu('PUG3','TUBE', ment(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUG3','SEEN',1)
      CALL gspos('PUG3',1,'PUG2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.04
      shape(3) = bpg_len
      CALL gsvolu('PUG4','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUG4','SEEN',1)
      CALL gspos('PUG4',1,'PUG3',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.5
      shape(3) = bpg_len                                    
      CALL gsvolu('PUG5','TUBE', ment(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUG5','SEEN',1)
      CALL gspos('PUG5',1,'PUG4',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPH next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 5.71
      shape(3) = bph_len 
      CALL gsvolu('PUH1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUH1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
     &     + bph_len
      CALL gspos('PUH1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bph_len 
      CALL gsvolu('PUH2','TUBE', ment(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUH2','SEEN',1)
      CALL gspos('PUH2',1,'PUH1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.04
      shape(3) = bph_len
      CALL gsvolu('PUH3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUH3','SEEN',1)
      CALL gspos('PUH3',1,'PUH2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.5
      shape(3) = bph_len
      CALL gsvolu('PUH4','TUBE', ment(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUH4','SEEN',1)
      CALL gspos('PUH4',1,'PUH3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPI next in line to left of box
C.
      shape(1) = 0.0  
      shape(2) = 2.53   
      shape(3) = bpi_len
      CALL gsvolu('PUI1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUI1','SEEN',1)
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
     &     + 2.*bph_len + bpi_len
      CALL gspos('PUI1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bpi_len 
      CALL gsvolu('PUI2','TUBE', ment(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUI2','SEEN',1)
      CALL gspos('PUI2',1,'PUI1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.04
      shape(3) = bpi_len
      CALL gsvolu('PUI3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PUI3','SEEN',1)
      CALL gspos('PUI3',1,'PUI2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.5
      shape(3) = bpi_len
      CALL gsvolu('PUI4','TUBE', ment(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PUI4','SEEN',1)
      CALL gspos('PUI4',1,'PUI3',0.0,0.0,0.0,0,'ONLY')

C.
C.----> section BPJ next in line to left of box
C. to fill the gap between the end of the pumping tubes and the
      shape(1) = 0.0
      shape(2) = 1.04
      shape(3) = bpj_len
      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
     &     + 2.*bph_len + 2.*bpi_len + bpj_len 
      CALL gsvolu('PUJ1','TUBE',6,shape,3,ivol)
      CALL gsatt('PUJ1','SEEN',1)
      CALL gspos('PUJ1',1,'DETE',0.0,0.0,-z,0,'ONLY')
      shape(1) = 0.0
      shape(2) = 0.5
      shape(3) = bpj_len
      CALL gsvolu('PUJ2','TUBE',ment(3),shape,3,ivol)
      CALL gsatt('PUJ2','SEEN',1)
      CALL gspos('PUJ2',1,'PUJ1',0.0,0.0,0.0,0,'ONLY')

C.***************************************************************
C.
C.    Make the collimator assembly for the gas cell for DOWNSTREAM
C.
C.***************************************************************
C.
      irot_side = nxtrotm()            ! opposite side rotating matrix
C.
      the1 =  270.
      phi1 =  180.
      the2 =   90.
      phi2 =   90.
      the3 =  180.
      phi3 =    0.

C.      tubetype = 0

      IF(tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7))THEN
C.
C.---> Collimator hole through outer aluminum  gas cell box DOWNSTREAM
C.
      shape( 1) = 0.0
      shape( 2) = 0.450
      shape( 3) = wall(2)/2.
      CALL gsvolu('DHOL','TUBE', mex(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('DHOL','SEEN',1)
      y = -beam_height
      z = -(box_length/2. - wall(2)/2.)
      CALL gspos('DHOL', 1, 'CMBR', 0.0, y, -z,0, 'ONLY')
C.
C.---> Aluminum Collimator on the inside of the gas cell box
C.
      shape( 1) = 0.0                   
      shape( 2) = 1.905                   
      shape( 3) = bpa_in_len         
      CALL gsvolu('PDAI','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDAI','SEEN',1) 
      y = -beam_height
      z = -(box_length/2. - wall(2) - bpa_in_len)
      CALL gspos('PDAI', 1, 'CMBG', 0.0, y, -z, 0, 'ONLY')
C.
      shape( 1) = 0.0
      shape( 2) = 0.450
      shape( 3) = bpa_in_len
      CALL gsvolu('PDBI','TUBE', mex(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDBI','SEEN',1)
      CALL gspos('PDBI', 1, 'PDAI', 0.0,0.0,0.0, 0, 'ONLY')
C.
C. ---> collimator end collar detail outside box
C.
      shape(1) = 0.0     
      shape(2) = 1.905
      shape(3) = bpa_len  !end collar detail half thickness
      CALL gsvolu('PDA1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDA1','SEEN',1)
      z = -(box_length/2 + bpa_len)
      CALL gspos('PDA1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0      
      shape(2) = 0.450
      shape(3) = bpa_len	!end collar detail half thickness
      CALL gsvolu('PDA2','TUBE', mex(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDA2','SEEN',1)
      CALL gspos('PDA2',1,'PDA1',0.0,0.0,0.0,0,'ONLY')
C.  
C.----> section PDB next to the RIGHT of collimator detail outside box    
C.
C.
      shape(1) = 0.0
      shape(2) = 1.035
      shape(3) = bpb_len  !end collar detail half thickness
      CALL gsvolu('PDB1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDB1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + bpb_len)
      CALL gspos('PDB1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.450
      shape(3) = bpb_len  !end collar detail half thickness
      CALL gsvolu('PDB2','TUBE', mex(1), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDB2','SEEN',1)
      CALL gspos('PDB2',1,'PDB1',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPD next in line to left of box
C.
C.
      shape(1) = 0.0
      shape(2) = 1.035
      shape(3) = bpc_len + bpd_len  !end collar detail half thickness
      CALL gsvolu('PDD1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDD1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + bpc_len
     &     + bpd_len)
      CALL gspos('PDD1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0 
      shape(2) = 0.520
      shape(3) = bpc_len + bpd_len  !end collar detail half thickness
      CALL gsvolu('PDD2','TUBE', mex(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDD2','SEEN',1)
      CALL gspos('PDD2',1,'PDD1',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section PDE next in line to RIGHT of box
C.
C.
      shape(1) = 0.0  
      shape(2) = 2.09
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PDE1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDE1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + bpe_len)
      CALL gspos('PDE1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.520
      shape(3) = bpe_len  !end collar detail half thickness
      CALL gsvolu('PDE2','TUBE', mex(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDE2','SEEN',1)
      CALL gspos('PDE2',1,'PDE1',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section PDF next in line to RIGHT of box
C.
C.
      shape(1) = 0.0  
      shape(2) = 2.53
      shape(3) = bpf_len+bpg_len  
      CALL gsvolu('PDF1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDF1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + bpf_len + bpg_len)   
      CALL gspos('PDF1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.25
      shape(3) = bpf_len+bpg_len
      CALL gsvolu('PDF2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDF2','SEEN',1)
      CALL gspos('PDF2',1,'PDF1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.04
      shape(3) = bpf_len+bpg_len
      CALL gsvolu('PDF3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDF3','SEEN',1)
      CALL gspos('PDF3',1,'PDF2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.591
      shape(3) = bpf_len+bpg_len
      CALL gsvolu('PDF4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDF4','SEEN',1)
      CALL gspos('PDF4',1,'PDF3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPH next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 5.71
      shape(3) = bph_len 
      CALL gsvolu('PDH1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDH1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
     &     + bph_len)
      CALL gspos('PDH1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bph_len
      CALL gsvolu('PDH2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH2','SEEN',1)
      CALL gspos('PDH2',1,'PDH1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.04
      shape(3) = bph_len
      CALL gsvolu('PDH3','TUBE', 6, shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH3','SEEN',1) 
      CALL gspos('PDH3',1,'PDH2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.591
      shape(3) = bph_len
      CALL gsvolu('PDH4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH4','SEEN',1)
      CALL gspos('PDH4',1,'PDH3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPI next in line to left of box
C.
      shape(1) = 0.0  
      shape(2) = 2.53   
      shape(3) = bpi_len
      CALL gsvolu('PDI1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDI1','SEEN',1)
      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
     &     + 2.*bph_len + bpi_len)
      CALL gspos('PDI1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.25
      shape(3) = bpi_len
      CALL gsvolu('PDI2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDI2','SEEN',1)
      CALL gspos('PDI2',1,'PDI1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.04
      shape(3) = bpi_len 
      CALL gsvolu('PDI3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDI3','SEEN',1) 
      CALL gspos('PDI3',1,'PDI2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.591
      shape(3) = bpi_len
      CALL gsvolu('PDI4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDI4','SEEN',1)
      CALL gspos('PDI4',1,'PDI3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPJ next in line to left of box
C. to fill the gap between the end of the pumping tubes and the
c      shape(1) = 0.0
c      shape(2) = 1.04
c      shape(3) = bpj_len2
c      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
c     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
c     &     + 2.*bph_len + 2.*bpi_len + bpj_len2 
c      CALL gsvolu('PDJ1','TUBE',6,shape,3,ivol)
c      CALL gsatt('PDJ1','SEEN',1)
c      CALL gspos('PDJ1',1,'DETE',0.0,0.0,z,0,'ONLY')
c      shape(1) = 0.0
c      shape(2) = 0.591
c      shape(3) = bpj_len2
c      CALL gsvolu('PDJ2','TUBE',ment(1),shape,3,ivol)
c      CALL gsatt('PDJ2','SEEN',1)
c      CALL gspos('PDJ2',1,'PDJ1',0.0,0.0,0.0,0,'ONLY')

      ELSEIF(tubetype.eq.1)THEN


C. Collar on right side of box wall
c         col_collar_length = 0.41

         shape(1) = 0.0
         shape(2) = 1.91
         shape(3) = 0.41/2.
         CALL gsvolu('CLRD','TUBE',6,shape,3,ivol)
         CALL gsatt('CLRD','SEEN',1)
         y = -beam_height
         z = box_length/2. - wall(2) - 0.41/2.
         shape(2) = 0.642
         CALL gsvolu('CL1G','TUBE',mex(1),shape,3,ivol)
         CALL gsatt('CL1G','SEEN',1)
         CALL gspos('CL1G',1,'CLRD',0.0,0.0,0.0,0,'ONLY')
         CALL gspos('CLRD',1,'CMBG',0.0,y,z,0,'ONLY')
c         shape(2) = 1.905
c         shape(3) = 0.214
c         CALL gsvolu('CLRB','TUBE',6,shape,3,ivol)
c         CALL gsatt('CLRB','SEEN',1)
c         y = -beam_height
c         z = box_length/2. - wall(2) - 0.517/2.
c     +       - 0.517/2. - 0.214
         
c         shape(2) = 0.8474
c         CALL gsvolu('CT1G','TUBE',ment(1),shape,3,ivol)
c         CALL gsatt('CT1G','SEEN',1)
c         CALL gspos('CT1G',1,'CLRB',0.0,0.0,0.0,0,'ONLY')
c         CALL gspos('CLRB',1,'CMBG',0.0,y,z,0,'ONLY')
C. Cut through box wall
         shape(1) = 0.0
         shape(2) = 0.642
         shape(3) = wall(2)/2.
         CALL gsvolu('APTU','TUBE',mex(1),shape,3,ivol)
         CALL gsatt('APTU','SEEN',1)
         y = -beam_height
         z = box_length/2. - wall(2)/2.
         CALL gspos('APTU',1,'CMBR',0.0,y,z,0,'ONLY')
C. Collar on right side of box
         shape(1) = 0.0
         shape(2) = 1.91
         shape(3) = 0.945/2.
         CALL gsvolu('CLLD','TUBE',6,shape,3,ivol)
         CALL gsatt('CLLD','SEEN',1)
         z = box_length/2. + 0.945/2.
         
         shape(2) = 0.642
         CALL gsvolu('CL2G','TUBE',mex(1),shape,3,ivol)
         CALL gsatt('CL2G','SEEN',1)
         CALL gspos('CL2G',1,'CLLD',0.0,0.0,0.0,0,'ONLY')
         call gspos('CLLD',1,'DETE',0.0,0.0,z,0,'ONLY')
C. First part of collimator tube
         shape(1) = 0.0
         shape(2) = 1.27
         shape(3) = 1.026/2.
         CALL gsvolu('CM1T','TUBE',6,shape,3,ivol)
         CALL gsatt('CM1T','SEEN',1)
         z = box_length/2. + 0.945 + 1.026/2.
         
         shape(2) = 0.642
         CALL gsvolu('CM1G','TUBE',mex(1),shape,3,ivol)
         CALL gsatt('CM1G','SEEN',1)
         CALL gspos('CM1G',1,'CM1T',0.0,0.0,0.0,0,'ONLY')
         CALL gspos('CM1T',1,'DETE',0.0,0.0,z,0,'ONLY')
C. Second part of collimator tube
         shape(1) = 0.0
         shape(2) = 1.27
         shape(3) = 2.54/2.
         CALL gsvolu('CM2T','TUBE',6,shape,3,ivol)
         CALL gsatt('CM2T','SEEN',1)
         z = box_length/2. + 0.945 + 1.026 + 2.54/2.
         shape(2) = 0.715
         CALL gsvolu('CM2G','TUBE',mex(1),shape,3,ivol)
         CALL gsatt('CM2G','SEEN',1)
         CALL gspos('CM2G',1,'CM2T',0.0,0.0,0.0,0,'ONLY')
         CALL gspos('CM2T',1,'DETE',0.0,0.0,z,0,'ONLY')

C. Tube up to first part of sheilding assembly


         shape(1) = 0.0
         shape(2) = 1.27
         shape(3) = 0.624/2.
         CALL gsvolu('CM3T','TUBE',6,shape,3,ivol)
         CALL gsatt('CM3T','SEEN',1)
         z = box_length/2. + 0.945 + 1.026 + 2.54 + 0.624/2.

         shape(2) = 0.95
         CALL gsvolu('CM3G','TUBE',mex(2),shape,3,ivol)
         CALL gsatt('CM3G','SEEN',1)
         CALL gspos('CM3G',1,'CM3T',0.0,0.0,0.0,0,'ONLY')
         CALL gspos('CM3T',1,'DETE',0.0,0.0,z,0,'ONLY')  

      shape(1) = 0.0  
      shape(2) = 2.09
      shape(3) = 3.54/2.  !end collar detail half thickness
      CALL gsvolu('PDE1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDE1','SEEN',1)
c      z =  -(box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
c     &     + 2.*bpd_len + bpe_len)
      z = -(box_length/2. + 0.945 + 1.026 + 2.54 + 0.624 + 3.54/2.)
      CALL gspos('PDE1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.95
      shape(3) = 3.54/2.  !end collar detail half thickness
      CALL gsvolu('PDE2','TUBE', mex(2), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDE2','SEEN',1)
      CALL gspos('PDE2',1,'PDE1',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section PDF next in line to RIGHT of box
C.
C.
      shape(1) = 0.0  
      shape(2) = 2.53
      shape(3) = 2.88/2.  
      CALL gsvolu('PDF1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDF1','SEEN',1)
      z = -(box_length/2. + 0.945 + 1.026 + 2.54 + 0.624 
     &     + 3.54 + 2.88/2.)
      CALL gspos('PDF1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 1.48
      shape(3) = 2.88/2.
      CALL gsvolu('PDF2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDF2','SEEN',1)
      CALL gspos('PDF2',1,'PDF1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.27
      shape(3) = 2.88/2.
      CALL gsvolu('PDF3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDF3','SEEN',1)
      CALL gspos('PDF3',1,'PDF2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0  
      shape(2) = 0.95
      shape(3) = 2.88/2.
      CALL gsvolu('PDF4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDF4','SEEN',1)
      CALL gspos('PDF4',1,'PDF3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPH next in line to left of box
C.
      shape(1) = 0.0
      shape(2) = 5.71
      shape(3) = 2.02/2. 
      CALL gsvolu('PDH1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDH1','SEEN',1)
      z = -(box_length/2. + 0.945 + 1.026 + 2.54 + 0.624 
     &     + 3.54 + 2.88 + 2.02/2.)
      CALL gspos('PDH1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.48
      shape(3) = 2.02/2.
      CALL gsvolu('PDH2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH2','SEEN',1)
      CALL gspos('PDH2',1,'PDH1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.27
      shape(3) = 2.02/2.
      CALL gsvolu('PDH3','TUBE', 6, shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH3','SEEN',1) 
      CALL gspos('PDH3',1,'PDH2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.95
      shape(3) = 2.02/2.
      CALL gsvolu('PDH4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDH4','SEEN',1)
      CALL gspos('PDH4',1,'PDH3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPI next in line to left of box
C.
      shape(1) = 0.0  
      shape(2) = 2.53   
      shape(3) = bpi_len
      CALL gsvolu('PDI1','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDI1','SEEN',1)
      z = -(box_length/2. + 0.945 + 1.026 + 2.54 + 0.624 
     &     + 3.54 + 2.88 +
     &     2.02 + bpi_len)
      CALL gspos('PDI1',1,'DETE',0.0,0.0,-z,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.48
      shape(3) = bpi_len
      CALL gsvolu('PDI2','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDI2','SEEN',1)
      CALL gspos('PDI2',1,'PDI1',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 1.27
      shape(3) = bpi_len 
      CALL gsvolu('PDI3','TUBE', 6, shape, 3, ivol)
c changed to Alum.
      CALL gsatt('PDI3','SEEN',1) 
      CALL gspos('PDI3',1,'PDI2',0.0,0.0,0.0,0,'ONLY')
C.
      shape(1) = 0.0
      shape(2) = 0.95
      shape(3) = bpi_len
      CALL gsvolu('PDI4','TUBE', mex(3), shape, 3, ivol)
c changed to Vac.
      CALL gsatt('PDI4','SEEN',1)
      CALL gspos('PDI4',1,'PDI3',0.0,0.0,0.0,0,'ONLY')
C.
C.----> section BPJ next in line to left of box
C. to fill the gap between the end of the pumping tubes and the
c      shape(1) = 0.0
c      shape(2) = 1.04
c      shape(3) = bpj_len2
c      z =  box_length/2. + 2.*bpa_len + 2.*bpb_len + 2.*bpc_len
c     &     + 2.*bpd_len + 2.*bpe_len + 2.*bpf_len + 2.*bpg_len
c     &     + 2.*bph_len + 2.*bpi_len + bpj_len2 
c      CALL gsvolu('PDJ1','TUBE',6,shape,3,ivol)
c      CALL gsatt('PDJ1','SEEN',1)
c      CALL gspos('PDJ1',1,'DETE',0.0,0.0,z,0,'ONLY')
c      shape(1) = 0.0
c      shape(2) = 0.71755
c      shape(3) = bpj_len2
c      CALL gsvolu('PDJ2','TUBE',ment(1),shape,3,ivol)
c      CALL gsatt('PDJ2','SEEN',1)
c      CALL gspos('PDJ2',1,'PDJ1',0.0,0.0,0.0,0,'ONLY')


      ENDIF

C.
C.
C.

C.***************************************************************
C.
C.    Make the inner gas cell assembly
C.
C.***************************************************************
C.
C.    Inner gas cell assembly only needed for gas targets
      if (targtype.eq.0)then
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
      CALL gsvolu('CELL','TRD1', 6, shape, 4, ivol)
      CALL gsatt('CELL','SEEN',1)
C.--->move inner cell so that apertures are centred on beam line
C.--->0.976 offset is height from box top to inner cell top
      y = .5 * box_height - shape(4) - 0.976   
      CALL gspos('CELL',1,'CMBG',0.0, y ,0.0,irot_box,'ONLY')      
C.
C.---> Vacuum inner shell of the cell
C.
      shape(1) = 1.798
      shape(2) = 6.094
      shape(3) = 1.588
      shape(4) = 3.891
C.
      CALL gsvolu('CELG','TRD1', mtarg, shape, 4, ivol)
      CALL gsatt('CELG','SEEN',1)
      CALL gspos('CELG',1,'CELL',0.0,0.0,0.0,0,'ONLY')      
C.
C.
C.---> Entrance collimator to the trapezoid target
C.
      shape(1) = 0.0
      if (tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7))then
         shape(2) = 0.3
      elseif (tubetype.eq.1)then
cc       MT updates gas target entrance collimator 21 Oct 2003.
         shape(2) = 0.2
      endif
      shape(3) = 0.5
C.
      CALL gsvolu('EAPG','TUBE', mtarg, shape, 3, ivol)
      CALL gsatt('EAPG', 'SEEN', 1)
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
      CALL gspos('EAPG', 1, 'CELL', x, 0.0, z, irot_col, 'ONLY')
C.
C.
C.---> Exit collimator to the trapezoid target
C.
      shape(1) = 0.0
      if (tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7))then
         shape(2) = 0.4
      elseif (tubetype.eq.1)then
cc       MT updates gas target exit collimator 21 Oct 2003.
         shape(2) = 0.5
      endif
      shape(3) = 0.5     

C.
      CALL gsvolu('XAPG','TUBE', mtarg, shape, 3, ivol)
      CALL gsatt('XAPG', 'SEEN', 1)
      z = 2.008
      x = -5.315
      CALL gspos('XAPG', 1, 'CELL', x, 0.0, z, irot_col, 'ONLY')
      endif
C.
C.***************************************************************
C.
C.    Make the solid target disc
C.
C.***************************************************************
C.
C.    Only need solid target disc for, ah, solid target work
      if (targtype.eq.1)then
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
C.        If the inner gas cell assembly is ever used for solid targets
C.    CALL GSROTM(54, 180.0, 0.0, 90.0, 90.0, 90.0, 0.0)
C.
      CALL gsvolu('CTAR', 'TUBE', 21, shape, 3, ivol)
C     CALL gspos ('CTAR' 1, 'CELG' 0.0, 0.0, z, 54, 'ONLY')
      CALL gspos('CTAR', 1, 'CMBG', 0.0, y, 0.0, 0, 'ONLY')
      endif


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
      include 'gammahit.inc'
C.
C *** Local variables
C.
      INTEGER i, j, k, n, jm, icopy, ivol
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
C. Detector 1.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2. -7.2
      y = - aprt - wall(3) - (1./2.) * hexagon_large_width-0.6
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
C. Detector 2.
      x = d_mtl/2. + d_air(2)/2. - pmt_length/2.      
      y = - aprt - wall(3) - (5./4.) * hexagon_large_width-0.6
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
C. Detector 3.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2. - 6.7
      y =   aprt + wall(3) + (1./2.)* hexagon_large_width+0.6
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
C. Detector 4.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y =   aprt + wall(3) + (5./4.)* hexagon_large_width+0.6
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
C. Detector 5.
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
C. Detector 6.
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
C. Detector 7.
      y =   aprt + wall(3) + (5./4.)* hexagon_large_width+0.6
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
C. Detector 8.
      y = - aprt - wall(3) - (5./4.) * hexagon_large_width-0.6
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
C. Detector 9.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y =   aprt + wall(3) + (1./2.)* hexagon_large_width+0.6
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
C. Detector 10.
      x =   d_mtl/2. + d_air(2)/2. - pmt_length/2.
      y = - aprt - wall(3) - (1./2.) * hexagon_large_width-0.6
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
C. Detectors 11 through 30.
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
C. Filling nearest neighour adding back adjacency matrix
C.
      k = 0
      n = 0
      Do i = 1, 30
         k = k + 1
         if (k.eq.2) k = k - 2
         Do j = 1, 30
            n = n + 1
            if (n.eq.2) n = n - 2
               if (i.gt.10 .and. j.gt.10 .and. n.ne.k) then
               else
                  if ( sqrt((x_fngr(i)-x_fngr(j))**2 +
     &                      (y_fngr(i)-y_fngr(j))**2 +
     &                      (z_fngr(i)-z_fngr(j))**2).lt.10)then
                  adjacency_matrix(i,j) = 1
                  else
                  adjacency_matrix(i,j) = 0
                  endif
               endif
         enddo
      enddo
      adjacency_matrix(5,3)=1
      adjacency_matrix(3,5)=1
      adjacency_matrix(9,6)=1
      adjacency_matrix(6,9)=1
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

















































