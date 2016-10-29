C
      SUBROUTINE mitray_field( devname, xpos, bfld, efld )
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     This subroutine takes as input the name of the element and the   C
C     position of the particle, and returns the magnetic and electric  C
C     field components due to the specified element.                   C
C                                                                      C
C     INPUT:                                                           C
C     -------                                                          C
C     DEVNAME         CHAR*12   Name of element whose B/E field        C
C                               is to be evaluated;  must match one    C
C                               of the names in ITITLE(i),i=1,NO       C
C                                                                      C
C     XPOS(i)         REAL*8(3) Vector containing the coordinates      C
C                               of the point where the B/E field       C
C                               components must be evaluated, in       C
C                               the RAYTRACE A-coordinate system of    C
C                               the element. [cm]                      C
C                                                                      C
C     OUTPUT:                                                          C
C     -------                                                          C
C     BFLD(i)         REAL*8(3) B-field components Bx, By, Bz in       C
C                               BFLD(1), BFLD(2), BFLD(3). [Tesla]     C
C                                                                      C
C     EFLD(i)         REAL*8(3) E-field components Ex, Ey, Ez in       C
C                               EFLD(1), EFLD(2), EFLD(3).             C
C                                                                      C
C     S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA                          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT none
C
      include 'gcflag.inc'          !geant
      include 'gctrak.inc'          !geant
      include 'gcunit.inc'          !geant
C
      include 'mitray_setup.inc'        !local
      include 'mitray_diag.inc'		!local
C
      INTEGER*4 i, ino, itype
C
      REAL*8 xpos(3), bfld(3), efld(3), devdata(mmax)
C
      CHARACTER*12 devname
C
      Do i = 1, 3
         bfld(i) = 0.D0
         efld(i) = 0.D0
      Enddo
C
C *** Set flag for diagnostic mode
C
      ldiag = .false.
      If(iswit(6).eq.1)ldiag = .true.
      If(ldiag)Write(lout,*)'Entering subroutine MITRAY_FIELD'
C
C *** Now find which element number in the beamline this corresponds to.
C
      Do i = 1, no
         If(devname.eq.ititle(i))then
           ino = i
           goto 100
         Endif
      Enddo
C
      WRITE(lout,10) devname
   10 FORMAT('MITRAY_FIELD called with unknown device name ',A12/
     &       ' All B- and E-field components set to zero.')
      WRITE(lout,*)'!!! Abort current event !!!'
      istop  = 1
      ieotri = 1
C
      RETURN
C
C *** INO now contains the number code for the device, where INO=1
C     corresponds to the first device of the beam transport system, etc.
C
C *** IDATA(INO) is the type code for this device, e.g. 2=dipole, etc.
C
  100 itype = idata(ino)
C
      Do i = 1, mmax
         devdata(i) = data(i,ino)
      Enddo
C
      If(itype.eq.2)then
C
C        dipole magnet
C
         CALL mitray_dipole(devdata,xpos,bfld)
C
      Elseif(itype.eq.7)then
C
C        electrostatic deflector
C
         CALL mitray_edipol(devdata,xpos,efld)
C
      Elseif(itype.eq.14)then
C
C        solenoid magnet
C
         CALL mitray_solnd(devdata,xpos,bfld)
C
      Elseif(itype.eq.9)then
C
C        multipole magnet
C
         CALL mitray_poles(devdata,xpos,bfld)
C
      Elseif(itype.eq.20)then
C
C        SASP dipole magnet
C
         CALL mitray_sasp(devdata,xpos,bfld)
C
      Else
C
         Write(lout,20) itype
   20    Format(' **ERROR** IN MITRAY_FIELD:' /
     &          ' NO FIELD CALC DEFINED FOR ITYPE= ',I10)
C
      Endif
C
      RETURN
      END
C
