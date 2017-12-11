c
      SUBROUTINE guefld(vector,time,bfield,efield)
c
c     GEANT mother volume:
c     --------------------
c
c     XM(3) is in the beam direction (down stream)
c     XM(2) is UP
c     XM(1) completes a right-handed coordinate system
c
      IMPLICIT none
c
c     Local variables
c
      INTEGER i, k, jvo, kvo, jr, irot
      INTEGER in, nin, jin, ivo, itmed, jtm, jpar, iyes, ifld
      INTEGER mlevel, lnam(15), lnum(15), ier, istep, iglvolu
c
      REAL vector(7), time, bfield(3), efield(3)
c
      REAL xm(3), xd(3), bfm(3), bfd(3), efm(3), efd(3)
      REAL B_field, E_field, vmod
c
      REAL*8 xdd(3), bfdd(3), efdd(3)
c
      CHARACTER*4 chname, new_chname
      CHARACTER*1 kdname 
c
      CHARACTER*12 devname
c
      INTEGER nlevel_old,names_old(15),number_old(15),lvolum_old(15)
      INTEGER lindex_old(15),infrom_old,nlevmx_old
      INTEGER nldev_old(15), linmx_old(15)
      REAL gtran_old(3,15),grmat_old(10,15),gonly_old(15),glx_old(3)
      INTEGER jgpar_old,iq_old(15),lq_old(15)
c
      include 'gcbank.inc'          !geant
      include 'gcflag.inc'          !geant
      include 'gcvolu.inc'          !geant
      include 'gctmed.inc'          !geant
      include 'gcunit.inc'          !geant
      include 'gctrak.inc'          !geant
c
      include 'geom_dipole.inc'         !local
      include 'geom_edipol.inc'         !local
      include 'geom_mpole.inc'          !local
      include 'geom_sole.inc'           !local
      include 'beamcom.inc'             !local
c
      DATA istep / 0 /
c
      iglvolu = 0
c
      istep = istep + 1
      If(istep .eq.1)then
        nlevel_old = nlevel
        CALL ucopy(names,names_old,nlevel)
        CALL ucopy(number,number_old,nlevel)
        CALL ucopy(lvolum,lvolum_old,nlevel)
        CALL ucopy(lindex,lindex_old,nlevel)
        infrom_old = infrom
        nlevmx_old = nlevmx
        CALL ucopy(nldev,nldev_old,nlevel)
        CALL ucopy(linmx,linmx_old,nlevel)
        jgpar_old = jgpar
        Do i = 1, nlevel
           CALL ucopy(gtran(1,i),gtran_old(1,i), 3)
           CALL ucopy(grmat(1,i),grmat_old(1,i),10)
           iq_old(jgpar+i) = iq(jgpar+i)
           lq_old(jgpar-i) = lq(jgpar-i)
        Enddo
        CALL ucopy(gonly,gonly_old,nlevel)
        CALL ucopy(glx,glx_old,3)
      Endif
c
      If(ifield.eq.1)then
        If(mod(istep,3).eq.0)istep = 0
      Else
                             istep = 0
      Endif
c
c     Initialize the field values
c
      CALL vzero(bfield,3)
      CALL vzero(efield,3)
c
c *** Obtain the field for the present volume
c
      k = number(nlevel)
c
      CALL uhtoc(names(nlevel),4,chname,4)
      CALL uhtoc(names(nlevel),4,kdname,1)
c
      CALL ucopy(vector(1),xm(1),3)
      CALL gmtod(xm,xd,1)
c
      If(kdname.eq.'D')then
        irot = irot_dipole(k)
        CALL gitran(xd,dx_dipole(1,k),irot,xd)
      Elseif(kdname.eq.'E')then
        irot = irot_edipol(k)
        CALL gitran(xd,dx_edipol(1,k),irot,xd)
      Elseif(kdname.eq.'Q')then
        irot = 0
        CALL gitran(xd,dx_mpole(1,k),0,xd)
      Elseif(kdname.eq.'S')then
        irot = 0
        CALL gitran(xd,dx_sole(1,k),0,xd)
      Endif
c
      xdd(1) = xd(1)
      xdd(2) = xd(2)
      xdd(3) = xd(3)
c
      devname = chname
      CALL mitray_field(devname, xdd, bfdd, efdd)
c
      bfd(1) = bfdd(1)
      bfd(2) = bfdd(2)
      bfd(3) = bfdd(3)
c
      efd(1) = efdd(1)
      efd(2) = efdd(2)
      efd(3) = efdd(3)
c
      B_field = 1.E+01*vmod(bfd,3)
      E_field = 1.E-06*vmod(efd,3)
c
      If(B_field.gt.0.0)then
        CALL vunit(bfd(1),bfd(1),3)
        If(irot.ne.0)then
          jr = lq(jrotm - irot)
          bfm(1) = bfd(1)*q(jr+1)+bfd(2)*q(jr+4)+
     &             bfd(3)*q(jr+7)
          bfm(2) = bfd(1)*q(jr+2)+bfd(2)*q(jr+5)+
     &             bfd(3)*q(jr+8)
          bfm(3) = bfd(1)*q(jr+3)+bfd(2)*q(jr+6)+
     &             bfd(3)*q(jr+9)
          CALL ucopy(bfm(1),bfd(1),3)
        Endif
        CALL gdtom(bfd,bfm,2)
        CALL vscale(bfm,B_field,bfield,3)
      Endif
c
      If(E_field.gt.0.0)then
        CALL vunit(efd(1),efd(1),3)
        If(irot.ne.0)then
          jr = lq(jrotm - irot)
          efm(1) = efd(1)*q(jr+1)+efd(2)*q(jr+4)+
     &             efd(3)*q(jr+7)
          efm(2) = efd(1)*q(jr+2)+efd(2)*q(jr+5)+
     &             efd(3)*q(jr+8)
          efm(3) = efd(1)*q(jr+3)+efd(2)*q(jr+6)+
     &             efd(3)*q(jr+9)
          CALL ucopy(efm(1),efd(1),3)
        Endif
        CALL gdtom(efd,efm,2)
        CALL vscale(efm,E_field,efield,3)
      Endif
c
c *** Add all overlapping fields
c
      If(gonly(nlevel).eq.0.)then		! MANY volume
c
        jvo = lq(jvolum-lvolum(nlevel-1))	! reached 'WRLD'
c
        nin = q(jvo+3)
c
        Do 100 in = 1, nin
c
           jin = lq(jvo-in)
c
           If(q(jin+8).eq.0.)then		! another MANY volume
c
             mlevel = nlevel
c
             ivo = ifix(q(jin+2))
             lnum(mlevel) = ifix(q(jin+3))
c
             jin = lq(jvolum-ivo)
             itmed = q(jin+4)
             jtm = lq(jtmed-itmed)
             ifld = q(jtm+8)
c
             CALL uhtoc(iq(jvolum+ivo),4,new_chname,4)
             If(new_chname.eq.chname)goto 100
c
             If(ifld.gt.0)then			! volume has a field
c
c     Temporarily corrupt the COMMON/GCVOLU/ to this WRLD daughter's level
c
               CALL ucopy(names (1),lnam(1),mlevel-1)
               CALL ucopy(number(1),lnum(1),mlevel-1)
c
               lnam(mlevel) = iq(jvolum+ivo)
c
               iglvolu = iglvolu + 1
               CALL glvolu(mlevel,lnam,lnum,ier)
c
               If(ier.ne.0)then
                 ieorun = 1
                 write(lout,*)'Fatal error in GLVOLU'
                 goto 999
               Endif
c
               CALL gmtod(xm,xd,1)
c
               kvo  = lq(jvolum-lvolum(mlevel))
c
               CALL ginme(xd,q(kvo+2),q(kvo+7),iyes)
c
               nlevel = mlevel
c
               If(iyes.gt.0)then
c
                 devname = new_chname
c
                 iglvolu = iglvolu + 1
                 CALL glvolu(nlevel,lnam,lnum,ier)
c
                 If(ier.ne.0)then
                   ieorun = 1
                   write(lout,*)'Fatal error in GLVOLU'
                   goto 999
                 Endif
c
                 k = lnum(nlevel)
                 CALL uhtoc(iq(jvolum+ivo),4,kdname,1)
c
                 CALL gmtod(xm,xd,1)
c
                 If(kdname.eq.'D')then
                   irot = irot_dipole(k)
                   CALL gitran(xd,dx_dipole(1,k),irot,xd)
                 Elseif(kdname.eq.'E')then
                   irot = irot_edipol(k)
                   CALL gitran(xd,dx_edipol(1,k),irot,xd)
                 Elseif(kdname.eq.'Q')then
                   irot = 0
                   CALL gitran(xd,dx_mpole(1,k),0,xd)
                 Elseif(kdname.eq.'S')then
                   irot = 0
                   CALL gitran(xd,dx_sole(1,k),0,xd)
                 Endif
c
                 xdd(1) = xd(1)
                 xdd(2) = xd(2)
                 xdd(3) = xd(3)
c
                 CALL mitray_field(devname, xdd, bfdd, efdd)
c
                 bfd(1) = bfdd(1)
                 bfd(2) = bfdd(2)
                 bfd(3) = bfdd(3)
c
                 efd(1) = efdd(1)
                 efd(2) = efdd(2)
                 efd(3) = efdd(3)
c
                 B_field = 1.E+01*vmod(bfd,3)
                 E_field = 1.E-06*vmod(efd,3)
c
                 If(B_field.gt.0.0)then
                   CALL vunit(bfd(1),bfd(1),3)
                   If(irot.ne.0)then
                     jr = lq(jrotm - irot)
                     bfm(1) = bfd(1)*q(jr+1)+bfd(2)*q(jr+4)+
     &                        bfd(3)*q(jr+7)
                     bfm(2) = bfd(1)*q(jr+2)+bfd(2)*q(jr+5)+
     &                        bfd(3)*q(jr+8)
                     bfm(3) = bfd(1)*q(jr+3)+bfd(2)*q(jr+6)+
     &                        bfd(3)*q(jr+9)
                     CALL ucopy(bfm(1),bfd(1),3)
                   Endif
                   CALL gdtom(bfd,bfm,2)
                   CALL vscale(bfm,B_field,bfm,3)
                   CALL vadd(bfield,bfm,bfield,3)
                 Endif
c
                 If(E_field.gt.0.0)then
                   CALL vunit(efd(1),efd(1),3)
                   If(irot.ne.0)then
                     jr = lq(jrotm - irot)
                     efm(1) = efd(1)*q(jr+1)+efd(2)*q(jr+4)+
     &                        efd(3)*q(jr+7)
                     efm(2) = efd(1)*q(jr+2)+efd(2)*q(jr+5)+
     &                        efd(3)*q(jr+8)
                     efm(3) = efd(1)*q(jr+3)+efd(2)*q(jr+6)+
     &                        efd(3)*q(jr+9)
                     CALL ucopy(efm(1),efd(1),3)
                   Endif
                   CALL gdtom(efd,efm,2)
                   CALL vscale(efm,E_field,efm,3)
                   CALL vadd(efield,efm,efield,3)
                 Endif
c
               Endif
             Endif
           Endif
  100   Continue
c
c       Restore original COMMON/GCVOLU
c
        If(iglvolu.gt.0)then
            nlevel = nlevel_old
            CALL ucopy(names_old,names,nlevel)
            CALL ucopy(number_old,number,nlevel)
            CALL ucopy(lvolum_old,lvolum,nlevel)
            CALL ucopy(lindex_old,lindex,nlevel)
            infrom = infrom_old
            nlevmx = nlevmx_old
            CALL ucopy(nldev_old,nldev,nlevel)
            CALL ucopy(linmx_old,linmx,nlevel)
            jgpar = jgpar_old
            Do i = 1, nlevel
              CALL ucopy(gtran_old(1,i),gtran(1,i), 3)
              CALL ucopy(grmat_old(1,i),grmat(1,i),10)
              iq(jgpar+i) = iq_old(jgpar+i)
              lq(jgpar-i) = lq_old(jgpar-i)
            Enddo
            CALL ucopy(gonly_old,gonly,nlevel)
            CALL ucopy(glx_old,glx,3)
        Endif
c
      Endif
!
!    Rescale the fields from the reference tune according to the reaction
      call vscale(bfield,bscale,bfield,3)
      call vscale(efield,escale,efield,3)
c
  999 RETURN
      END
