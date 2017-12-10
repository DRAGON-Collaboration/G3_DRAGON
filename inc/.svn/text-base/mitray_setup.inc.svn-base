C
C *** MITRAY COMMON BLOCK
C
      INTEGER*4 nmax, mmax
C
      PARAMETER (mmax =  75)
      PARAMETER (nmax = 300)
C
C     MMAX is maximum number of parameters for element
C     NMAX is maximum number of elements in beam transport system
C
      INTEGER*4 no
      INTEGER*4 idata(nmax)
C
      CHARACTER*12 ititle(nmax) 
C
      REAL data(mmax,nmax)
C
      COMMON /mitray/ no, idata, ititle, data
C
C     NO        I*4               Number of elements in beam transport 
C                                 system 
C
C     IDATA(i)  I*4(nmax)         Type code for ith element  (2=DIPO, 
C                                  3=EINZ,  7=EDIP,  8=VELS,  9=POLE, 
C                                 10=MULT, 11=SHRT, 12=DRIF, 13=COLL,
C                                 14=SOLE, 15=LENS, 16=ACCE)
C
C     ITITLE(i) CHAR*12(nmax)     User-specified name for ith element
C                                 (e.g. B1, QUAD2, ARGUS, GEORGE)
C
C     DATA(j,i) REAL*8(mmax,nmax) Parameters for the ith element
C                                 (field strengths, radii, etc.)
C                                 Up to 75 parameters, for each of
C                                 200 elements.
C                                     
