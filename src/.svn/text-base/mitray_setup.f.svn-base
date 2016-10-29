C
        SUBROUTINE mitray_setup
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C	This subroutine reads the input file for MIT-RAYTRACE          C
C	and sets up the array of parameters specifying the beam        C
C       transport system.                                              C
C                                                                      C
C       It was adapted from the Aug 1989 version of MIT-RAYTRACE by    C
C       S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA       and              C
C       P. Gumplinger (TRIUMF) e-mail GUM@TRIUMF.CA                    C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT none
C
      INTEGER*4 i, j, ielement
C
      CHARACTER* 4 nwd
      CHARACTER* 6 mitray
      CHARACTER*12 jtitle
      CHARACTER*120 infile

C
      LOGICAL fexist
    
C
      include 'gcunit.inc'              !geant
C
      include 'mitray_setup.inc'	!local
      include 'mitray_diag.inc'		!local
C
      CALL vzero(idata,nmax)
      Do i = 1, mmax
         Do j = 1, nmax
            data(i,j) = 0.D0
         Enddo
      Enddo 
C
C *** Now open the file containing the specification
C *** of all the beam transport elements
C
      mitray = 'MITRAY'
      CALL getenv(mitray,infile)
      If(infile.eq.'  ')infile = 'mitray.dat'


C Output data file for ascii data, EOC
      open(20,status='unknown',file='TP7.dat')
      open(21,status='unknown',file='chargeslit.dat')
      open(23,status='unknown',file='massslit.dat')
      open(24,status='unknown',file='finalslit.dat')
      
C
      INQUIRE (file=INFILE,exist=fexist)
C
      If(fexist)then
        OPEN(UNIT=lunits(3),STATUS='OLD',FILE=INFILE,err=999)
      Else
        goto 999
      Endif
C
      no = 0
C
      Do ielement = 1, nmax
C
        READ (lunits(3),*,end=998) nwd, jtitle
C
        If(nwd.eq.'COMM')then
C
C         This is a comment, so do nothing!
C
        Elseif(nwd.eq.'DIPO')then
C
C          DIPOLE  LENS           TYPE = 2
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 2
C
           READ(lunits(3),*) (DATA(J,NO),J= 1, 6)  ! RECORD 2
           READ(lunits(3),*) (DATA(J,NO),J=11,15)  ! RECORD 3
           READ(lunits(3),*) (DATA(J,NO),J=16,18)  ! RECORD 4
           READ(lunits(3),*) (DATA(J,NO),J=19,22)  ! RECORD 5
           READ(lunits(3),*) (DATA(J,NO),J=25,28)  ! RECORD 6
           READ(lunits(3),*) (DATA(J,NO),J=29,34)  ! RECORD 7
           READ(lunits(3),*) (DATA(J,NO),J=35,40)  ! RECORD 8
           READ(lunits(3),*) (DATA(J,NO),J=41,46)  ! RECORD 9
           READ(lunits(3),*) (DATA(J,NO),J=47,50)  ! RECORD 10
           READ(lunits(3),*) (DATA(J,NO),J=51,57)  ! RECORD 11
           READ(lunits(3),*) (DATA(J,NO),J=58,64)  ! RECORD 12
C
        Elseif(nwd.eq.'EINZ')then
C
C          EINZEL  LENS           TYPE = 3
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 3
C
           READ(lunits(3),*)(DATA(J,NO),J= 1, 2)   ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=10,14)   ! RECORD 3
           READ(lunits(3),*)(DATA(J,NO),J=15,16)   ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=17,22)   ! RECORD 5
C
        Elseif(nwd.eq.'EDIP')then
C
C          ELECTROSTATIC DEFLECTOR  TYPE = 7
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 7
C
           READ(lunits(3),*)(DATA(J,NO),J= 1, 4)   ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=11,15)   ! RECORD 3
           READ(lunits(3),*) DATA(16,NO)           ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=17,20)   ! RECORD 5
           READ(lunits(3),*)(DATA(J,NO),J=25,28)   ! RECORD 6
           READ(lunits(3),*)(DATA(J,NO),J=29,34)   ! RECORD 7
           READ(lunits(3),*)(DATA(J,NO),J=35,40)   ! RECORD 8
C
        Elseif(nwd.eq.'VELS')then
C
C          VELOCITY SELECTOR      TYPE = 8
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 8
C
           READ(lunits(3),*)(DATA(J,NO),J= 1, 4)   ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J= 7,11)   ! RECORD 3
           READ(lunits(3),*)(DATA(J,NO),J=12,13)   ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=16,19)   ! RECORD 5
           READ(lunits(3),*)(DATA(J,NO),J=20,23)   ! RECORD 6
           READ(lunits(3),*)(DATA(J,NO),J=24,27)   ! RECORD 7
           READ(lunits(3),*)(DATA(J,NO),J=28,33)   ! RECORD 8
           READ(lunits(3),*)(DATA(J,NO),J=34,39)   ! RECORD 9
           READ(lunits(3),*)(DATA(J,NO),J=40,45)   ! RECORD 10
           READ(lunits(3),*)(DATA(J,NO),J=46,51)   ! RECORD 11
C
        Elseif(nwd.eq.'POLE')then
C
C          MULTIPOLE (POLES)      TYPE =  9
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 9
C
           READ(lunits(3),*)(DATA(J,NO),J= 1, 3)   ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=10,13)   ! RECORD 3
           READ(lunits(3),*)(DATA(J,NO),J=14,18)   ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=19,22)   ! RECORD 5
           READ(lunits(3),*)(DATA(J,NO),J=23,28)   ! RECORD 6
           READ(lunits(3),*)(DATA(J,NO),J=29,34)   ! RECORD 7
           READ(lunits(3),*)(DATA(J,NO),J=35,42)   ! RECORD 8
C
        Elseif(nwd.eq.'MULT')then
C
C          MULTIPOLE LENS         TYPE = 10
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 10
C
           READ(lunits(3),*)(DATA(J,NO),J= 1, 2)   ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=10,15)   ! RECORD 3
           READ(lunits(3),*)(DATA(J,NO),J=16,17)   ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=20,25)   ! RECORD 5
           READ(lunits(3),*)(DATA(J,NO),J=26,28)   ! RECORD 6
C
        Elseif(nwd.eq.'SHRT')then
C
C          SHIFT AND ROTATE       TYPE = 11
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 11
C
           READ(lunits(3),*)(DATA(J,NO),J=1,6)     ! RECORD 2
C
        Elseif(nwd.eq.'DRIF')then
C
C          DRIFT                  TYPE = 12
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 12
C
           READ(lunits(3),*) DATA(1,NO)            ! RECORD 2
C
C
        Elseif(nwd.eq.'FCUP')then
C
C          FARRADAY CUP           TYPE = 21
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 21
C
           READ(lunits(3),*) (DATA(J,NO),J=1,3)
C
        Elseif(nwd.eq.'COLL')then
C
C          COLLIMATOR             TYPE = 13
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 13
C
           READ(lunits(3),*)(DATA(J,NO),J=1,5)     ! RECORD 2
C
        Elseif(nwd.eq.'RCOL')then
C
C          COLLIMATOR             TYPE = 17
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 17
C
           READ(lunits(3),*)(DATA(J,NO),J=1,6)     ! RECORD 2
C
        Elseif(nwd.eq.'SOLE')then
C
C          SOLENOID               TYPE = 14
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 14
C
           READ(lunits(3),*)  DATA(1,NO)           ! RECORD 2
           READ(lunits(3),*) (DATA(J,NO), J=10,14) ! RECORD 3
           READ(lunits(3),*) (DATA(J,NO), J=15,16) ! RECORD 4
C
        Elseif(nwd.eq.'LENS')then
C
C          LENS                   TYPE = 15
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 15
C
           READ(lunits(3),*)(DATA(J,NO),J=1,8)     ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=9,11)    ! RECORD 3
C
        Elseif(nwd.eq.'ACCE')then
C
C          ACCELERATOR            TYPE = 16
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 16
C
           READ(lunits(3),*)(DATA(J,NO),J=1,3)     ! RECORD 2
           READ(lunits(3),*)(DATA(J,NO),J=10,13)   ! RECORD 3
           READ(lunits(3),*) DATA(14,NO)           ! RECORD 4
           READ(lunits(3),*)(DATA(J,NO),J=15,18)   ! RECORD 5
           READ(lunits(3),*)(DATA(J,NO),J=19,24)   ! RECORD 6
           READ(lunits(3),*)(DATA(J,NO),J=25,30)   ! RECORD 7
C
        Elseif(nwd.eq.'SASP')then
C
C          Added by S.Yen - SASP clamshell dipole at TRIUMF
C          (same as normal DIPOLE except that sagging of the
C           field at the high field end is taken into account)
C
C          TRIUMF SASP DIPOLE     TYPE = 20
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 20
C
           READ(lunits(3),*) (DATA(J,NO),J= 1, 6)  ! RECORD 2
           READ(lunits(3),*) (DATA(J,NO),J=11,15)  ! RECORD 3
           READ(lunits(3),*) (DATA(J,NO),J=16,18)  ! RECORD 4
           READ(lunits(3),*) (DATA(J,NO),J=19,22)  ! RECORD 5
           READ(lunits(3),*) (DATA(J,NO),J=25,28)  ! RECORD 6
           READ(lunits(3),*) (DATA(J,NO),J=29,34)  ! RECORD 7
           READ(lunits(3),*) (DATA(J,NO),J=35,40)  ! RECORD 8
           READ(lunits(3),*) (DATA(J,NO),J=41,46)  ! RECORD 9
           READ(lunits(3),*) (DATA(J,NO),J=47,50)  ! RECORD 10
           READ(lunits(3),*) (DATA(J,NO),J=51,57)  ! RECORD 11
           READ(lunits(3),*) (DATA(J,NO),J=58,64)  ! RECORD 12
C
        Elseif(nwd.eq.'ENDV')then
C
C          END CONTROL VOLUME     TYPE = 18
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 18
        Elseif(nwd.eq.'TEST')then
C
C          TEST VOLUME            TYPE = 22
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 22
C
        Elseif(nwd.eq.'MCPF')then
C
C          MCP                   TYPE = 24
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 24
           READ(lunits(3),*) (DATA(J,NO),J=1,2)
C
        Elseif(nwd.eq.'TUBS') then

c          ANGLED TUBE            TYPE = 23
           

           no = no + 1
           ititle(no) = jtitle
           idata(no) = 23


           READ(lunits(3),*)(DATA(J,NO),J=1,5) ! RECORD 2

        Elseif(nwd.eq.'STRV')then
C
C          START CONTROL VOLUME   TYPE = 19
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 19
C
        Elseif(nwd.eq.'SENT')then
C
C          SYSTEM END             TYPE = 1
C
           no = no + 1
           ititle(no) = jtitle
           idata(no) = 1
C
C	   Close the input file that has been opened
C
           CLOSE(unit = lunits(3))
C
C          CALCULATE FIELD MAPS
C
           CALL mitray_fmap
C
C          INITIALIZE Geometry
C
           CALL ugeom_setup
C
           RETURN
C
        Else
C
C	   UNKNOWN ELEMENT NAME
C
           WRITE(lout,100) nwd
100        FORMAT(' UNKNOWN ELEMENT TYPE ',A4,' IGNORED')
C
           goto 998
C
        Endif
      Enddo
C
  998 Continue
C
      WRITE(lout,*)' Error reading MITRAY file: ',infile
      STOP
C
  999 Continue
C
      WRITE(lout,*)' Error opening MITRAY file: ',infile
      STOP
      END
C
C=======================================================================
C
      SUBROUTINE ugeom_setup
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine which initializes the necessary ugeom COMMON          C
C     blocks from the RAYTRACE input file                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT none
C
      INTEGER i, ielement, irot, jr, nxtrotm
C
      INTEGER ncol, mcol, jcol(2), last_type, ntest, nmcp
C
      REAL d(3), dd(3), p(3), pn(3), rmat(10), rmatn(10)
      REAL the1, phi1, the2, phi2, the3, phi3
      REAL costh, sinth, cosph, sinph
      REAL rowmat(3), theta(3), phi(3)
      REAL u(3), ut(3), ur(3), uh(3), uv(3), up(3), scale_1, scale_2
C
      REAL xcol(2), ycol(2), dxcol(2), dycol(2)

      REAL par(5)
C
      LOGICAL rotate
C
      include 'gcbank.inc'              !geant
      include 'gconst.inc'              !geant
C
      include 'mitray_setup.inc'	!local
      include 'geom_dipole.inc'		!local
      include 'geom_edipol.inc'		!local
      include 'geom_mpole.inc'		!local
      include 'geom_sole.inc'           !local
C
      include 'ukine.inc'               !local
C
      CALL vzero(p,3)
      CALL vzero(rmat,10)
C
      rmat(1) = 1.
      rmat(5) = 1.
      rmat(9) = 1.
C
      ndipole = 0
      nedipol = 0
      nmpole  = 0
      ntest = 0
      nmcp = 0
      nsole   = 0
C
      Do ielement = 1, no
C
         If(idata(ielement).eq.21)then
C
C          FCUP                    TYPE = 21
C
           d(1) = 12.0
           d(2) = 0.0
           d(3) = 1.0
C           print*, 'posn ', p(1), p(2), p(3)
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
C           print*, p(1), p(2), p(3), pn(1), pn(2), pn(3)
           CALL ucopy(pn,p,3)
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
           CALL ugeo_fcup(p,irot)
C
           d(1) = -12.0
           d(2) = 0.0
           d(3) = -1.0
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
           Endif
         If(idata(ielement).eq.23) then
            
C          TUBS                  TYPE = 23
            par(1) = DATA(1,23)
            par(2) = DATA(2,23)
            par(3) = DATA(3,23)
            par(4) = DATA(4,23)
            par(5) = DATA(5,23)
            
         Endif
         If(idata(ielement).eq.18)then
C
C          ENDV                   TYPE =  18
C
           d(1) =       0.0
           d(2) =       0.0
           If(irevs.ne.1)then
             d(3) =     0.05 !!1.0
           Else
             d(3) =    -0.05 !!-1.0
           Endif  
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_end(p,irot)
C
           d(1) =       0.0
           d(2) =       0.0
           If(irevs.ne.1)then
             d(3) =    -1.0
           Else
             d(3) =     1.0
           Endif
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         ElseIf(idata(ielement).eq.24)then
C
C          MCP                   TYPE =  24
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) =       0.0 
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           the1 = 90.
           phi1 =  0.    !! axis I parallel to 1
           the2 = 90.
           phi2 = 90.    !! axis II parallel to 2
           the3 =  0.
           phi3 =  0.    !! axis III parallel to 3
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_mcp(p,irot,data(1,ielement),ititle(ielement),nmcp)
           nmcp = nmcp + 1
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) =       0.0
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         ElseIf(idata(ielement).eq.22)then
C
C          TEST                   TYPE =  22
C
            ntest = ntest + 1
           d(1) =       0.0
           d(2) =       0.0
           If(irevs.ne.1)then
             d(3) =     0.00 !!1.0
           Else
             d(3) =    -0.00 !!-1.0
           Endif  
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_test(ntest,p,irot)
C
           d(1) =       0.0
           d(2) =       0.0
           If(irevs.ne.1)then
             d(3) =    0.0
           Else
             d(3) =     0.0
           Endif
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         Elseif(idata(ielement).eq.19)then
C
C          STRV                   TYPE =  19
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_start(p,irot)
C
         Elseif(idata(ielement).eq.12)then
C
C          DRIFT                  TYPE = 12
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(1,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)

         Elseif(idata(ielement).eq.11)then
C
C          SHIFT AND ROTATE       TYPE = 11
C
           d(1) = data(1,ielement)
           d(2) = data(2,ielement)
           d(3) = data(3,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           If(data(4,ielement).ne.0.0)then	! Rotation around x-axis
             the1 =  90.
             phi1 =   0.
             the2 =  90. - data(4,ielement)
             phi2 =  90.
             the3 =        data(4,ielement)
             phi3 = -90.
             irot = nxtrotm()
             CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
             CALL grmul(rmat,irot,rmatn)
             CALL ucopy(rmatn,rmat,10)
           Endif
C
           If(data(5,ielement).ne.0.0)then	! Rotation around y-axis
             the1 =  90. + data(5,ielement)
             phi1 =   0.
             the2 =  90.
             phi2 =  90.
             the3 =        data(5,ielement)
             phi3 =   0.
             irot = nxtrotm()
             CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
             CALL grmul(rmat,irot,rmatn)
             CALL ucopy(rmatn,rmat,10)
           Endif
C
           If(data(6,ielement).ne.0.0)then	! Rotation around z-axis
             the1 =  90.
             phi1 =        data(6,ielement)
             the2 =  90.
             phi2 =  90. + data(6,ielement)
             the3 =   0.
             phi3 =   0.
             irot = nxtrotm()
             CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
             CALL grmul(rmat,irot,rmatn)
             CALL ucopy(rmatn,rmat,10)
           Endif
C
         Elseif(idata(ielement).eq.13)then
C
C          COLLIMATOR             TYPE = 13
C
           ncol = ncol + 1
           mcol = mod(ncol,2)
           If(mcol.eq.0)mcol = 2
C
           jcol(mcol) = data(1,ielement)
           xcol(mcol) = data(2,ielement)
           ycol(mcol) = data(3,ielement)
           dxcol(mcol) = data(4,ielement)
           dycol(mcol) = data(5,ielement)
C
           If(mcol.eq.2)then
             If(last_type.eq.2)then
               jcol_dipole(2,ndipole) =  jcol(mcol)
               xcol_dipole(2,ndipole) =  xcol(mcol)
               ycol_dipole(2,ndipole) =  ycol(mcol)
              dxcol_dipole(2,ndipole) = dxcol(mcol)
              dycol_dipole(2,ndipole) = dycol(mcol)
             Elseif(last_type.eq.7)then
               jcol_edipol(2,nedipol) =  jcol(mcol)
               xcol_edipol(2,nedipol) =  xcol(mcol)
               ycol_edipol(2,nedipol) =  ycol(mcol)
              dxcol_edipol(2,nedipol) = dxcol(mcol)
              dycol_edipol(2,nedipol) = dycol(mcol)
             Elseif(last_type.eq.9)then
               jcol_mpole(2,nmpole) =  jcol(mcol)
               xcol_mpole(2,nmpole) =  xcol(mcol)
               ycol_mpole(2,nmpole) =  ycol(mcol)
              dxcol_mpole(2,nmpole) = dxcol(mcol)
              dycol_mpole(2,nmpole) = dycol(mcol)
             Elseif(last_type.eq.14)then
               jcol_sole(2,nsole) =  jcol(mcol)
               xcol_sole(2,nsole) =  xcol(mcol)
               ycol_sole(2,nsole) =  ycol(mcol)
              dxcol_sole(2,nsole) = dxcol(mcol)
              dycol_sole(2,nsole) = dycol(mcol)
             Endif
           Endif
C
         Elseif(idata(ielement).eq.17)then
C
C          REAL COLLIMATOR        TYPE = 17
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_col(p,irot,data(1,ielement),ititle(ielement))
C
         Elseif(idata(ielement).eq. 2.or.
     &          idata(ielement).eq.20)then
C
C          DIPOLE  LENS           TYPE =  2
C          TRIUMF SASP DIPOLE     TYPE = 20
C
           ndipole = ndipole + 1
           ndipole = min(ndipole,max_dipole)
C
           If(mcol.eq.1)then
             jcol_dipole(1,ndipole) =  jcol(mcol)
             xcol_dipole(1,ndipole) =  xcol(mcol)
             ycol_dipole(1,ndipole) =  ycol(mcol)
            dxcol_dipole(1,ndipole) = dxcol(mcol)
            dycol_dipole(1,ndipole) = dycol(mcol)
           Endif
           last_type = idata(ielement)
C
           gap_dipole(ndipole) = data(13,ielement)
           phi_dipole(ndipole) = data(16,ielement)
             r_dipole(ndipole) = data(14,ielement)
            dr_dipole(ndipole) = (data(49,ielement)+data(50,ielement))/2.
C
           alpha_dipole(ndipole) = data(17,ielement)
            beta_dipole(ndipole) = data(18,ielement)
C
           z11_dipole(ndipole) = data(25,ielement)
           z22_dipole(ndipole) = data(28,ielement)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(11,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           Do i = 1, 3
              rowmat(1) = rmat(  i)
              rowmat(2) = rmat(3+i)
              rowmat(3) = rmat(6+i)
              CALL gfang(rowmat,costh,sinth,cosph,sinph,rotate)
              theta(i) = 57.295779513082*atan2(sinth,costh)
              phi(i)   = 57.295779513082*atan2(sinph,cosph)
           Enddo
C
           ut(1) = 0.0
           ut(2) = 0.0
           ut(3) = 1.0
C
           CALL ginrot(ut,rmat,u)
           CALL ucopy(u,ut,3)
C
           ur(1) = -ut(3)
           ur(2) =  ut(2)
           ur(3) =  ut(1)
C
           If(data(1,ielement).lt.0.0)then
             ur(1) = -ur(1)
             ur(2) =  ur(2)
             ur(3) = -ur(3)
           Endif
C
           scale_1 = r_dipole(ndipole)
           CALL vscale(ur,scale_1,d,3)
           CALL vadd(p,d,p,3)
C
           scale_1 = cos(degrad*phi_dipole(ndipole)/2.)
           scale_1 = scale_1 * sin(degrad*phi_dipole(ndipole)/2.)
           scale_1 = 2. * scale_1 * r_dipole(ndipole)
C
           scale_2 = sin(degrad*phi_dipole(ndipole)/2.)
           scale_2 = 2. * scale_2**2 * r_dipole(ndipole)
C
           CALL vline(ut,scale_1,ur,scale_2,d,3)
           CALL vunit(d,uh,3)
C
           uv(1) =  uh(3)
           uv(2) =  uh(2)
           uv(3) = -uh(1)
C
           If(data(1,ielement).lt.0.0)then
             uv(1) = -uv(1)
             uv(2) =  uv(2)
             uv(3) = -uv(3)
           Endif
C
           scale_1 = (r_dipole(ndipole)+dr_dipole(ndipole))/2.
           CALL vscale(uv,scale_1,dd,3)
           CALL vadd(p,dd,p,3)
C
           CALL ucopy(p(1),pos_dipole(1,ndipole),3)
C
           CALL cross(uh,uv,up)
           CALL vunit(up,up,3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
C
           CALL ucopy(uh(1),q(jr+1),3)
           CALL ucopy(uv(1),q(jr+4),3)
           CALL ucopy(up(1),q(jr+7),3)
           q(jr+10) = 1.
C
           Do i = 1, 3
              rowmat(1) = q(jr+  i)
              rowmat(2) = q(jr+3+i)
              rowmat(3) = q(jr+6+i)
              CALL gfang(rowmat,costh,sinth,cosph,sinph,rotate)
              theta(i) = 57.295779513082*atan2(sinth,costh)
              phi(i)   = 57.295779513082*atan2(sinph,cosph)
              q(jr+10+2*(i-1)+1) = theta(i)
              q(jr+10+2*(i-1)+2) = phi(i)
           Enddo
C
           CALL ugeo_dipole(ndipole,uh,irot)
C
           CALL vadd(pn,d,p,3)
C
           If(data(1,ielement).gt.0.0)then
             the1 =  90. - phi_dipole(ndipole)
             phi1 =   0.
             the2 =  90.
             phi2 =  90.
             the3 =      - phi_dipole(ndipole)
             phi3 =   0.
           Else
             the1 =  90. + phi_dipole(ndipole)
             phi1 =   0.
             the2 =  90.
             phi2 =  90.
             the3 =        phi_dipole(ndipole)
             phi3 =   0.
           Endif
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
           CALL grmul(rmat,irot,rmatn)
           CALL ucopy(rmatn,rmat,10)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(12,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         Elseif(idata(ielement).eq.7)then
C
C          ELECTROSTATIC DEFLECTOR  TYPE = 7
C
           nedipol = nedipol + 1
           nedipol = min(nedipol,max_edipol)
C
           If(mcol.eq.1)then
             jcol_edipol(1,nedipol) =  jcol(mcol)
             xcol_edipol(1,nedipol) =  xcol(mcol)
             ycol_edipol(1,nedipol) =  ycol(mcol)
            dxcol_edipol(1,nedipol) = dxcol(mcol)
            dycol_edipol(1,nedipol) = dycol(mcol)
           Endif
           last_type = idata(ielement)
C
           gap_edipol(nedipol) = data(13,ielement)
           phi_edipol(nedipol) = data(16,ielement)
             r_edipol(nedipol) = data(14,ielement)
            dr_edipol(nedipol) = (data( 2,ielement)+data( 3,ielement))/2.
C
           z11_edipol(nedipol) = data(25,ielement)
           z22_edipol(nedipol) = data(28,ielement)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(11,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           Do i = 1, 3
              rowmat(1) = rmat(  i)
              rowmat(2) = rmat(3+i)
              rowmat(3) = rmat(6+i)
              CALL gfang(rowmat,costh,sinth,cosph,sinph,rotate)
              theta(i) = 57.295779513082*atan2(sinth,costh)
              phi(i)   = 57.295779513082*atan2(sinph,cosph)
           Enddo
C
           ut(1) = 0.0
           ut(2) = 0.0
           ut(3) = 1.0
C
           CALL ginrot(ut,rmat,u)
           CALL ucopy(u,ut,3)
C
           ur(1) = -ut(3)
           ur(2) =  ut(2)
           ur(3) =  ut(1)
C
           If(data(1,ielement).lt.0.0)then
             ur(1) = -ur(1)
             ur(2) =  ur(2)
             ur(3) = -ur(3)
           Endif
C
           scale_1 = r_edipol(nedipol)
           CALL vscale(ur,scale_1,d,3)
           CALL vadd(p,d,p,3)
C
           scale_1 = cos(degrad*phi_edipol(nedipol)/2.)
           scale_1 = scale_1 * sin(degrad*phi_edipol(nedipol)/2.)
           scale_1 = 2. * scale_1 * r_edipol(nedipol)
C
           scale_2 = sin(degrad*phi_edipol(nedipol)/2.)
           scale_2 = 2. * scale_2**2 * r_edipol(nedipol)
C
           CALL vline(ut,scale_1,ur,scale_2,d,3)
           CALL vunit(d,uh,3)
C
           uv(1) =  uh(3)
           uv(2) =  uh(2)
           uv(3) = -uh(1)
C
           If(data(1,ielement).lt.0.0)then
             uv(1) = -uv(1)
             uv(2) =  uv(2)
             uv(3) = -uv(3)
           Endif
C
           scale_1 = (r_edipol(nedipol)+dr_edipol(nedipol))/2.
           CALL vscale(uv,scale_1,dd,3)
           CALL vadd(p,dd,p,3)
C
           CALL ucopy(p(1),pos_edipol(1,nedipol),3)
C
           CALL cross(uh,uv,up)
           CALL vunit(up,up,3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
C
           CALL ucopy(uh(1),q(jr+1),3)
           CALL ucopy(uv(1),q(jr+4),3)
           CALL ucopy(up(1),q(jr+7),3)
           q(jr+10) = 1.
C
           Do i = 1, 3
              rowmat(1) = q(jr+  i)
              rowmat(2) = q(jr+3+i)
              rowmat(3) = q(jr+6+i)
              CALL gfang(rowmat,costh,sinth,cosph,sinph,rotate)
              theta(i) = 57.295779513082*atan2(sinth,costh)
              phi(i)   = 57.295779513082*atan2(sinph,cosph)
              q(jr+10+2*(i-1)+1) = theta(i)
              q(jr+10+2*(i-1)+2) = phi(i)
           Enddo
C
           CALL ugeo_edipol(nedipol,uh,irot)
C
           CALL vadd(pn,d,p,3)
C
           If(data(1,ielement).gt.0.0)then
             the1 =  90. - phi_edipol(nedipol)
             phi1 =   0.
             the2 =  90.
             phi2 =  90.
             the3 =      - phi_edipol(nedipol)
             phi3 =   0.
           Else
             the1 =  90. + phi_edipol(nedipol)
             phi1 =   0.
             the2 =  90.
             phi2 =  90.
             the3 =        phi_edipol(nedipol)
             phi3 =   0.
           Endif
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
           CALL grmul(rmat,irot,rmatn)
           CALL ucopy(rmatn,rmat,10)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(12,ielement)
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         Elseif(idata(ielement).eq.9)then
C
C          MULTIPOLE (POLES)      TYPE =  9
C
           nmpole = nmpole + 1
           nmpole = min(nmpole,max_mpole)
C
           If(mcol.eq.1)then
             jcol_mpole(1,nmpole) =  jcol(mcol)
             xcol_mpole(1,nmpole) =  xcol(mcol)
             ycol_mpole(1,nmpole) =  ycol(mcol)
            dxcol_mpole(1,nmpole) = dxcol(mcol)
            dycol_mpole(1,nmpole) = dycol(mcol)
           Endif
           last_type = idata(ielement)
C
           efblength_mpole(nmpole) = data(12,ielement)
                   r_mpole(nmpole) = data(13,ielement)
C
           z11_mpole(nmpole)       = data(19,ielement)
           z22_mpole(nmpole)       = data(22,ielement)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(10,ielement)
C
           d(3) = d(3) + (z22_mpole(nmpole)+efblength_mpole(nmpole)
     &                            -z11_mpole(nmpole))/2.
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           CALL ucopy(p(1),pos_mpole(1,nmpole),3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_mpole(nmpole,irot)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) =  data(11,ielement) - z22_mpole(nmpole)
           d(3) = d(3) + (z22_mpole(nmpole)+efblength_mpole(nmpole)
     &                            +z11_mpole(nmpole))/2.
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         Elseif(idata(ielement).eq.14)then
C
C          SOLENOID               TYPE = 14
C
           nsole = nsole + 1
           nsole = min(nsole,max_sole)
C
           If(mcol.eq.1)then
             jcol_sole(1,nsole) =  jcol(mcol)
             xcol_sole(1,nsole) =  xcol(mcol)
             ycol_sole(1,nsole) =  ycol(mcol)
            dxcol_sole(1,nsole) = dxcol(mcol)
            dycol_sole(1,nsole) = dycol(mcol)
           Endif
           last_type = idata(ielement)
C
           efblength_sole(nsole) = data(12,ielement)
                   r_sole(nsole) = data(13,ielement)/2.
C
           z11_sole(nsole)       = data(15,ielement)
           z22_sole(nsole)       = data(16,ielement)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) = data(10,ielement)
C
           d(3) = d(3) + (z22_sole(nsole)+efblength_sole(nsole)
     &                            -z11_sole(nsole))/2.
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
           CALL ucopy(p(1),pos_sole(1,nsole),3)
C
           the1 = 90.
           phi1 =  0.
           the2 = 90.
           phi2 = 90.
           the3 =  0.
           phi3 =  0.
C
           irot = nxtrotm()
           CALL gsrotm(irot,the1,phi1,the2,phi2,the3,phi3)
C
           jr = lq(jrotm - irot)
           CALL ucopy(rmat(1),q(jr+1),10)
C
           CALL ugeo_sole(nsole,irot)
C
           d(1) =       0.0
           d(2) =       0.0
           d(3) =  data(11,ielement) - z22_sole(nsole)
           d(3) = d(3) + (z22_sole(nsole)+efblength_sole(nsole)
     &                            +z11_sole(nsole))/2.
C
           CALL gtrmul(p,rmat,d,0,pn,rmatn)
           CALL ucopy(pn,p,3)
C
         Endif
C
      Enddo
C
      RETURN
      END
