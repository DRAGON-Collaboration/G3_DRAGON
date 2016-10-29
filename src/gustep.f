C.
      SUBROUTINE gustep
C.
************************************************************************
*                                                                      *
*     GEANT3 user routine called at the end of each tracking step      *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gckine.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gctmed.inc'          !geant
      include 'gctrak.inc'          !geant        ! istop
C.
      CHARACTER*4 chname_2level, chname_pres
C.
      CALL uhtoc(names(2),4,chname_2level,4)
      CALL uhtoc(names(3),4,chname_pres,4)

C        Store traking points for graphics
C
      CALL GSXYZ
c      CALL GPCXYZ
C
C.
      If(ipart.ge.80)then
        If(chname_2level.eq.'DETE' ) then
          CALL gustep_trgt
        Else
          CALL gustep_mitray
        Endif
      Else
CCC        istop = 1
        CALL gustep_gbox
      Endif
C.
      RETURN
      END



