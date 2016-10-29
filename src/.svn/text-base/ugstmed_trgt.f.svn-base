      SUBROUTINE ugstmed_trgt
      IMPLICIT NONE
      include 'ugctmed.inc'
      include 'uggeom.inc'
      include 'rescom.inc'
      INTEGER ifield, isvol
      REAL fieldm, tmaxfd, stemax, deemax, stmin, epsil
C
C     Define tracking media
C
C     Magnetic field 
      ifield = 0
      fieldm = 0.0
      tmaxfd = 10.0
C     Beam pipe
      isvol = 0
      stemax = 10.0
      deemax = 0.005
      epsil = 0.001
      stmin = 0.001
      CALL gstmed(mssteel,'STAINLESS STEEL',mssteel,isvol,ifield,
     &     fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
C
C     CELL gas
      isvol = 0
      stemax = 0.05
      deemax = 0.1
      epsil = 1.e-05
      stmin = 1.e-05
C
C     If Solid target no need to redefine Carbon
C
      if(atarg.ne.12)then
         CALL gstmed(mtarg,'Gas Target',mtarg,isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      endif
C
C     Box, entrance and exit mediums
C     
      epsil = 1.e-05
      stemax = 1.
C
      print*,'******************mbox',mbox 
      CALL gstmed(mbox,'baseline',mbox,isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
C     stemax = 10.
      CALL gstmed(ment(1),'entrance1',ment(1),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      CALL gstmed(ment(2),'entrance2',ment(2),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      CALL gstmed(ment(3),'entrance3',ment(3),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      CALL gstmed(mex(1),'exit1',mex(1),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      CALL gstmed(mex(2),'exit2',mex(2),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      CALL gstmed(mex(3),'exit3',mex(3),isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      stemax = 10.
      CALL gstmed(mcent,'centralvacuum',mcent,isvol,ifield,
     &        fieldm,tmaxfd,stemax,deemax,epsil,stmin,0,0)
      RETURN
      END
