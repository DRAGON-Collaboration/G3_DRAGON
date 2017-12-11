C----67----
      SUBROUTINE chmass(newm)

      IMPLICIT NONE
      REAL newm
      INTEGER JPA
      include 'gcbank.inc'     !geant
      include 'gckine.inc'     !geant
  
C
C--   pointer to JPART bank, resonant particle (81)
C
      JPA = LQ(JPART-81)      
C
C
C--   corrupt ZEBRA bank
C
      Q(JPA+7) = newm
C
      RETURN
      END
