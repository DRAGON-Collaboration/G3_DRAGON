C.
       SUBROUTINE ugeom
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine to define the geometry of the set-up        *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      include 'gcunit.inc'             !geant
C.
      include 'mask.inc'               !local
      include 'geometry.inc'           !local
      INTEGER i, j
C.
      CHARACTER*120 CARDNAME/'aaaa'/
      CHARACTER*4 MASKING
C.
      LOGICAL fexist
C.
      Do i = 1, max_hexagon
         mask(i) = 1
      Enddo
C.
      MASKING = 'MASK'
      CALL getenv(MASKING,CARDNAME)
      if (CARDNAME == '') go to 999
      INQUIRE (file=CARDNAME, exist=fexist)
      If(fexist)then
        OPEN(UNIT=lunits(5), FILE=CARDNAME, FORM='FORMATTED',
     &           ACCESS = 'SEQUENTIAL', STATUS = 'OLD', err=999)
      Else
        goto 999
      Endif
C.
      Do i = 1, max_hexagon
         READ(lunits(5),111)j,mask(i)
  111    FORMAT(2I3)
      Enddo
C.
      goto 998
C.
  999 Continue
C.
      write(lout,*)' No MASKING file opened for simulation! '
C.
  998 Continue
C.
C.======================================================================
C.
C.    Space 
C.
C.======================================================================
C.
      CALL ugeo_space
C.
C.======================================================================
C.
C.    Detector
C.
C.======================================================================
C.
      CALL ugeo_detector
C.
C.======================================================================
C.
C.    Target
C.
C.======================================================================
C.
      if(tubetype.eq.1)then
         CALL ugeo_trgt_large
      elseif(tubetype.eq.0)then
         CALL ugeo_trgt_small         
      elseif(tubetype.eq.2)then
         CALL ugeo_trgt_small_up 
      elseif(tubetype.eq.3)then
         CALL ugeo_trgt_small_down 
      elseif(tubetype.eq.4)then
         CALL ugeo_trgt_small_left 
      elseif(tubetype.eq.5)then
         CALL ugeo_trgt_small_right 
      elseif(tubetype.eq.6)then
         CALL ugeo_trgt_small_hole 
      endif

C.
C.======================================================================
C.
C.    BSO Fingers
C.
C.======================================================================
C.
      CALL ugeo_finger
C.
C.======================================================================
C.
C.    PMTs
C.
C.======================================================================
C.
      CALL ugeo_pmt
C.
C.======================================================================
C.
      RETURN
      END
C.
      SUBROUTINE ugeo_space
C.
************************************************************************
*                                                                      *
*                       Define the mother spaces                       *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      INTEGER ivol
C.
      REAL shape(3)
C.
C.                              VACUUM SPACE 
C.                            ****************
C.
      shape(1) =  1500.	   ! square box space
      shape(2) = shape(1)
      shape(3) = shape(1)
C.
      CALL gsvolu ('WRLD', 'BOX ', 1, shape, 3, ivol)
      CALL gsatt('WRLD','SEEN',0)
C.
      RETURN
      END
C.
      INTEGER FUNCTION nxtmate()
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
C.
      INTEGER imat
C.
      If (jmate.le.0) then
         nxtmate = 1
      Else
         Do imat = 1, IQ(jmate-2)
            If(LQ(jmate-imat).eq.0)goto 100
         Enddo
  100    nxtmate= imat
      Endif
C.
      RETURN
      END
C.
      INTEGER FUNCTION nxttmed()
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
C.
      INTEGER itm
C.
      If (jtmed.le.0) then
         nxttmed = 1
      Else
         Do itm = 1, IQ(jtmed-2)
            If(LQ(jtmed-itm).eq.0)goto 100
         Enddo
  100    nxttmed= itm
      Endif
C.
      RETURN
      END
C.
      INTEGER FUNCTION nxtrotm()
C.
      IMPLICIT none
C.
      include 'gcbank.inc'          !geant
C.
      INTEGER irot
C.
      If (jrotm.le.0) then
         nxtrotm = 1
      Else
         Do irot = 2, IQ(jrotm-2)
            If(LQ(jrotm-irot).eq.0)goto 100
         Enddo
  100    nxtrotm = irot
      Endif
C.
      RETURN
      END
C.



















