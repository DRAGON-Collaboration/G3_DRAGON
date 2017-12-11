C.
      SUBROUTINE gukine
C.
************************************************************************
*                                                                      *
*             GEANT3 user routine to generate Kinematics               *
*                        for primary tracks                            *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gckine.inc'          !geant
      include 'uevent.inc'          !local
      include 'geometry.inc'        !local
C.
      If(ikine.ne.0)then
        CALL gukine_mitray
      Elseif(mkine.ne.0)then
        CALL gukine_gbox
      Elseif(lkine.ne.0)then
         if(tubetype.eq.0)then
            CALL gukine_full
	 elseif(tubetype.eq.1)then
	    CALL gukine_full         
         elseif(tubetype.eq.2)then
            CALL gukine_full_up 
         elseif(tubetype.eq.3)then
            CALL gukine_full_down 
         elseif(tubetype.eq.4)then
            CALL gukine_full_left 
         elseif(tubetype.eq.5)then
            CALL gukine_full_right 
         elseif(tubetype.eq.6)then
            CALL gukine_full_hole
         endif
      Endif
C.
      RETURN
      END
C.
