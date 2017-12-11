C.
         PROGRAM GUMAIN
C.
C.    ******************************************************************
C.    *                                                                *
C.    *              Main program for GEANT-shower studies             *
C.    *                   (Batch FULL_MONTE simulation)                *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT none
C.
      INTEGER NGBANK
      PARAMETER (NGBANK = 2000000)
      REAL Q
      COMMON/GCBANK/Q(NGBANK)
C.
      INTEGER NHBOOK
      PARAMETER (NHBOOK = 800000)
      REAL H
      COMMON/PAWC  /H(NHBOOK)
C.
      EXTERNAL UGINIT,UGLAST
C.
      EXTERNAL GUDCAY,GUDIGI,GUEFLD,GUHADR,GUKINE,GUNEAR,GUOUT,GUPHAD
      EXTERNAL GUSKIP,GUSTEP,GUSWIM,GUTRAK,GUTREV,GUPARA,GUFLD
C.
C.--> For interactive control:  EXTERNAL GUIGET,GUINME,GUINTI,GUVIEW
C.
      EXTERNAL GDCXYZ
C.
C.-->   Set the total maximum time for the job
C.
      CALL timest(1.E5)
C.
C.-->   Initialize GEANT and HBOOK memory
C.
      CALL gzebra( NGBANK)
      CALL hlimit(-NHBOOK)
C.
C.-->   GEANT initialization phase
C.
      CALL uginit
C.
C.-->   Start event processing
C.
      CALL grun
C.
C.-->   End of Run: Termination phase
C.
      CALL uglast
C.
      STOP
      END
C.
      SUBROUTINE gdcxyz
C.
      ENTRY igsa
      ENTRY gdtrak
C.
      RETURN
      END
C.
