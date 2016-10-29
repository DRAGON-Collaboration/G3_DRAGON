C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     File: UKINE.INC                                                  C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   The maximum number of steps before bailing out! 
C                  Set to 10,000 in GINIT
C
      INTEGER max_step, irevs
C
      REAL len_max
C
      COMMON /ukine/ max_step, len_max, irevs
C 
