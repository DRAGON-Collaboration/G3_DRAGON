C.
      SUBROUTINE neighborhood(n_fngr)
C.
************************************************************************
*                                                                      *
*                     Define neighborhood of each module               *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      INTEGER max
      PARAMETER ( max = 30 )
C.
      INTEGER Nn
      PARAMETER ( Nn  = 10 )
C.
      INTEGER n_fngr(Nn,max)
C.
      CALL vzero(n_fngr,Nn*max)
C.
      n_fngr(1,1) =  1
      n_fngr(2,1) =  2
      n_fngr(3,1) = 11
      n_fngr(4,1) = 12
      n_fngr(5,1) =  3
C.
      n_fngr(1,2) =  2
      n_fngr(2,2) =  1
      n_fngr(3,2) = 13 
      n_fngr(4,2) = 14 
      n_fngr(5,2) = 11
      n_fngr(6,2) = 12 
C.
      n_fngr(1,3) =  3
      n_fngr(2,3) =  4
      n_fngr(3,3) = 15 
      n_fngr(4,3) = 16
      n_fngr(5,3) =  5
C.
      n_fngr(1,4) =  4
      n_fngr(2,4) =  5
      n_fngr(3,4) =  3
      n_fngr(4,4) = 15
      n_fngr(5,4) = 16 
C.
      n_fngr(1,5) =  5
      n_fngr(2,5) =  4
      n_fngr(3,5) =  6
      n_fngr(4,5) = 15
      n_fngr(5,5) = 16
      n_fngr(6,5) = 21
      n_fngr(7,5) = 22
      n_fngr(8,5) =  3
C.
      n_fngr(1,6) =  6
      n_fngr(2,6) =  5
      n_fngr(3,6) =  7
      n_fngr(4,6) = 21
      n_fngr(5,6) = 22 
      n_fngr(6,6) = 27
      n_fngr(7,6) = 28
      n_fngr(8,6) =  9
C.
      n_fngr(1,7) =  7
      n_fngr(2,7) =  6 
      n_fngr(3,7) =  9 
      n_fngr(4,7) = 27
      n_fngr(5,7) = 28 
C.
      n_fngr(1,8) =  8
      n_fngr(2,8) = 10  
      n_fngr(3,8) = 25 
      n_fngr(4,8) = 26 
      n_fngr(5,8) = 29 
      n_fngr(6,8) = 30
C.
      n_fngr(1,9) =  9
      n_fngr(2,9) =  7
      n_fngr(3,9) = 27
      n_fngr(4,9) = 28
      n_fngr(5,9) =  6
C.
      n_fngr(1,10) = 10
      n_fngr(2,10) =  8
      n_fngr(3,10) = 29
      n_fngr(4,10) = 30
      n_fngr(5,10) =  9
C.
      n_fngr(1,11) = 11
      n_fngr(2,11) = 17
      n_fngr(3,11) = 13 
      n_fngr(4,11) = 15
      n_fngr(5,11) =  1
      n_fngr(6,11) =  2
C.
      n_fngr(1,12) = 12
      n_fngr(2,12) = 18 
      n_fngr(3,12) = 14 
      n_fngr(4,12) = 16 
      n_fngr(5,12) =  1
      n_fngr(6,12) =  2
C.
      n_fngr(1,13) = 13
      n_fngr(2,13) = 17 
      n_fngr(3,13) = 19 
      n_fngr(4,13) = 11
      n_fngr(5,13) =  2
C.
      n_fngr(1,14) = 14
      n_fngr(2,14) = 18
      n_fngr(3,14) = 20
      n_fngr(4,14) = 12
      n_fngr(5,14) =  2
C.
      n_fngr(1,15) = 15
      n_fngr(2,15) = 21
      n_fngr(3,15) = 11 
      n_fngr(4,15) = 17
      n_fngr(5,15) =  3
      n_fngr(6,15) =  4 
      n_fngr(7,15) =  5
C.
      n_fngr(1,16) = 16
      n_fngr(2,16) = 22  
      n_fngr(3,16) = 12
      n_fngr(4,16) = 18
      n_fngr(5,16) =  3
      n_fngr(6,16) =  4
      n_fngr(7,16) =  5
C.
      n_fngr(1,17) = 17
      n_fngr(2,17) = 21
      n_fngr(3,17) = 23 
      n_fngr(4,17) = 19
      n_fngr(5,17) = 11
      n_fngr(6,17) = 13
      n_fngr(7,17) = 15
C.
      n_fngr(1,18) = 18
      n_fngr(2,18) = 22 
      n_fngr(3,18) = 24
      n_fngr(4,18) = 20
      n_fngr(5,18) = 12
      n_fngr(6,18) = 14
      n_fngr(7,18) = 16
C.
      n_fngr(1,19) = 19
      n_fngr(2,19) = 23 
      n_fngr(3,19) = 17
      n_fngr(4,19) = 13
      n_fngr(5,19) = 25
C.
      n_fngr(1,20) = 20
      n_fngr(2,20) = 24 
      n_fngr(3,20) = 18
      n_fngr(4,20) = 14
      n_fngr(5,20) = 26
C.
      n_fngr(1,21) = 21
      n_fngr(2,21) = 23 
      n_fngr(3,21) = 15
      n_fngr(4,21) = 17
      n_fngr(5,21) = 27
      n_fngr(6,21) =  5
      n_fngr(7,21) =  6
C.
      n_fngr(1,22) = 22
      n_fngr(2,22) = 24 
      n_fngr(3,22) = 16
      n_fngr(4,22) = 18
      n_fngr(5,22) = 28
      n_fngr(6,22) =  5
      n_fngr(7,22) =  6
C.
      n_fngr(1,23) = 23
      n_fngr(2,23) = 21
      n_fngr(3,23) = 25
      n_fngr(4,23) = 17
      n_fngr(5,23) = 27
      n_fngr(6,23) = 29
      n_fngr(7,23) = 19
C.
      n_fngr(1,24) = 24
      n_fngr(2,24) = 22 
      n_fngr(3,24) = 26
      n_fngr(4,24) = 18
      n_fngr(5,24) = 28
      n_fngr(6,24) = 30
      n_fngr(7,24) = 20
C.
      n_fngr(1,25) = 25
      n_fngr(2,25) = 23 
      n_fngr(3,25) = 19
      n_fngr(4,25) = 29
      n_fngr(5,25) =  8
C.
      n_fngr(1,26) = 26
      n_fngr(2,26) = 24 
      n_fngr(3,26) = 20
      n_fngr(4,26) = 30
      n_fngr(5,26) =  8
C.
      n_fngr(1,27) = 27
      n_fngr(2,27) = 21 
      n_fngr(3,27) = 23
      n_fngr(4,27) = 29
      n_fngr(5,27) =  9
      n_fngr(6,27) =  6
      n_fngr(7,27) =  7
C.
      n_fngr(1,28) = 28
      n_fngr(2,28) = 22 
      n_fngr(3,28) = 24
      n_fngr(4,28) = 30
      n_fngr(5,28) =  9
      n_fngr(6,28) =  6
      n_fngr(7,28) =  7
C.
      n_fngr(1,29) = 29
      n_fngr(2,29) = 23
      n_fngr(3,29) = 25
      n_fngr(4,29) = 27
      n_fngr(5,29) = 10
      n_fngr(6,29) =  8
C.
      n_fngr(1,30) = 30
      n_fngr(2,30) = 24
      n_fngr(3,30) = 26
      n_fngr(4,30) = 28
      n_fngr(5,30) = 10
      n_fngr(6,30) =  8
C.
      RETURN
      END
C.
