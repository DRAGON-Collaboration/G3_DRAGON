C.
      SUBROUTINE ugffgo
C.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C                   Define all user FFKEY cards                      C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C.
      IMPLICIT none
C.
      include 'gcunit.inc'          !geant
C.
      include 'ukine.inc'           !local
C.
      include 'uevent.inc'          !local  lkine, fkine  
      include 'geometry.inc'        !local
      include 'uenergy.inc'         !local
      include 'beamcom.inc'          !local
C.
      CHARACTER*120 CARDNAME
      CHARACTER*6 FFCARDS
      INTEGER*4 ioffset
C.
      LOGICAL fexist
C.
C.-->   Set and retrieve FFCARD input logical unit number
C.
      CALL ffset('LINP',lunits(2))
C.
      FFCARDS = 'FFCARD'
      CALL getenv(FFCARDS,CARDNAME)
      IF(CARDNAME .eq. '  ') CARDNAME = 'dragon_2003.ffcards'
      INQUIRE (file=CARDNAME,exist=fexist)
      If(fexist)then
        OPEN(UNIT=lunits(2),FILE=CARDNAME,STATUS='OLD',
     +       FORM='FORMATTED',err=999)
        WRITE(LOUT,*)' ***** Using FFCARDS from file *****',CARDNAME
      Else
        goto 999
      Endif
C.
C.-->   Define *** USER FFCARDS ***
C.
C *** Full Monte Simulation
C.
c *** Define the reaction number(I), beam charge(real), recoil charge(real)
C *** Using a negative value of the reaction number loads that reaction and
C *** tune, but passes the beam rather than recoils.
 
      CALL ffkey('FKIN',LKINE ,11,'MIXED')
C.
C*** Define the reference tune specifying reference energy(MeV), atomic#,charge
      CALL ffkey('TUNE',refenerg,3,'REAL')
C*** OFFSETS for beam spot position and angle
      DO ioffset=1,4
         offset(ioffset) = 0.
      ENDDO
      energscale = 0
      CALL ffkey('MTUN',offset,5,'REAL')
C.
C *** Define the beam energy and tune scale for non-resonant reactions
      beamenerg = 0 !Default for resonance  case
      CALL ffkey('BEAM',beamenerg,1,'REAL')

      CALL ffkey('SCAL',bscale,2,'REAL')
C *** Dragon Spectrometer
C.
      CALL ffkey('MAXS', max_step, 2, 'MIXE')
C.
      CALL ffkey('REVS', irevs, 1, 'INTEGER')
C.
      CALL ffkey('EGAM',egamma,10,'REAL')
C.
C *** Gamma Detector
C.
      CALL ffkey('GKIN',MKINE ,11,'MIXED')
C.
      CALL ffkey('SCNT',iscnt,1,'INTEGER')
      CALL ffkey('YILD',photon_yield,2,'REAL')
C.
      CALL ffkey('THLD',tot_thrshld,2,'REAL')
C.
      CALL ffkey('DMAT',n_detmate,1,'INTEGER')
      CALL ffkey('TARG',targtype,1,'INTEGER')
      CALL ffkey('TUBE',tubetype,1,'INTEGER')
      CALL ffkey('FSID',s_finger,6,'REAL')
C.
      CALL ffkey('WALL',wall,3,'REAL')
      CALL ffkey('BGAP',box_width ,1,'REAL')
      CALL ffkey('HOLE',aprt,2,'REAL')
C.      CALL ffkey('SHLD',shield_end,2,'REAL')
C.
      CALL ffkey('MPMT',mtype_pmt,1,'INTEGER')
      CALL ffkey('PMTR',pmt_size,2,'REAL')
C.
      CALL ffkey('BLKA',bulk_absorption ,1,'REAL')
      CALL ffkey('REFL',paint_absorption,1,'REAL')
C. 
      CALL ffkey('ANAL',E_threshold,1,'REAL')
    
C.
      RETURN
C.
  999 WRITE(lout,*)' Error opening FFCARD file: ',CARDNAME
      STOP
C.
      END
C.













