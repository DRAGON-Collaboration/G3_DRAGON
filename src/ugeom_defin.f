C.
      SUBROUTINE ugeo_defin
************************************************************************
*                                                                      *
*                          Define Geometry                             *
*                                                                      *
************************************************************************
C.
      IMPLICIT none
C.
      include 'gcflag.inc'
      include 'geometry.inc'            !local
      include 'uggeom.inc'
C.
C.
C.
C.--> Define the geometrical dimensions of the different parts
C.                      of the apparatus.
C.
      if (tubetype.eq.0.or.(tubetype.gt.1.and.tubetype.lt.7)) then
         Rrms    =  6.0         ! gas volume radius
         TLrms   = 88.0         ! DETE Volume
C.
C.                          Apertures
C.
         rent = 0.8
         lent = 0.8
         len1 = 7.6
         riren1 = 0.4
         rilen1 = 0.5
c
         len2 = 8.7
         riren2 = 0.495
        rilen2 = 0.59
C.
         len3 = 10.
         riren3 = 0.65
         rilen3 =0.75
C.
         lex1   =  0.34
         rilex1 =  0.45
         rirex1 =  0.59
C.
         lex2   =  1.65
         rilex2 =  0.675
         rirex2 =  0.675

         lex3   =  3.9
         rilex3 =  0.895
         rirex3 =  1.03
C.
         lex4   = 15.25
         rilex4 =  1.25
         rirex4 =  1.80
C
C.                              Z Positions
C.
         zent(1) = -15.6
         zent(2) =   0.0
         zent(3) = -36.05
         zent(4) =   0.0
         zent(5) = -63.75
         zent(6) =   0.0
C.
         zex (1) =  15.6
         zex (2) =   0.0
         zex (3) =  29.95
         zex (4) =   0.0
         zex (5) =  39.7
         zex (6) =   0.0
         zex (7) = 68.75
C.
C.
      elseif (tubetype.eq.1) then
         Rrms    =  6.0         ! gas volume radius
         TLrms   = 88.0         ! DETE Volume
C.
C.                          Apertures
C.
         rent = 0.8
         lent = 0.8
         len1 = 7.6
         riren1 = 0.4
         rilen1 = 0.5
c     
         len2 = 8.7
         riren2 = 0.495
         rilen2 = 0.59
C.
         len3 = 10.
         riren3 = 0.65
         rilen3 =0.75
C.
         lex1 = 0.34
         rilex1 = 0.64135
         rirex1 = 0.71755
C.
         lex2 = 7.15
         rilex2 = 1.16713
         rirex2 = 1.51003

         lex3 = 10.25
         rilex3 = 1.83134
         rirex3 = 2.36474
C.
         lex4 = 11.45
         rilex4 = 2.72161
         rirex4 = 3.26644
C
C.                              Z Positions
C.
         zent(1) = -15.6
         zent(2) =   0.0
         zent(3) = -36.05
         zent(4) =   0.0
         zent(5) = -63.75
         zent(6) =   0.0
C.
         zex (1) = 10.25
         zex (2) =   0.0
         zex (3) = 38.35
         zex (4) =   0.0
         zex (5) = 67.95
         zex (6) =   0.0
         zex(7) = 106.35
C.
C.
      else
         print*, "UNKNOWN GEOMETRY"
         ieorun = 1
      endif

      RETURN
      END
