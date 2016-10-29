C
      SUBROUTINE ureact
C
      IMPLICIT none
C
      include 'gckine.inc'          !geant
C
      include 'uevent.inc'          !local
      include 'rescom.inc'          !local
      include 'beamcom.inc'
      include 'res.inc'             !local
      include 'params.inc'          !local
C
      INTEGER ireaction
C
      INTEGER nubuf, i, j
      PARAMETER ( nubuf = 1)
      REAL    ubuf(nubuf)
C
      REAL hbar
      REAL amugev, hmass, hemass, deutmass, c12mass
C
      PARAMETER ( hbar   = 6.582122   E-22 )
      PARAMETER ( amugev = 0.93149432 E+00 )
      PARAMETER ( hmass  = 1.007825032 * amugev)
      PARAMETER ( hemass = 4.002603250 * amugev)
      PARAMETER (deutmass = 2.0141018 * amugev )
      PARAMETER ( c12mass = 12.0 * amugev )
C
      REAL devmass,aamass,aamass1,zbeam, elevel, aprod,  tlif
C
      INTEGER mode(10)
      REAL    brat(10)
      CHARACTER*2 num(31)
      CHARACTER*120 CARDNAME
      CHARACTER*5 INPUT
      CHARACTER*3 targtyp
      LOGICAL fexist
      DATA num/ '1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','10', 
     &          '11','12','13','14','15','16','17','18','19','20',
     &          '21','22','23','24','25','26','27','28','29','30','31'/
C
C======================================================================C
C                                                                      C
C      Initial beam and reaction information passed via FKIN card      C
C                                                                      C
C      LKINE reaction number                                           C
C                                                                      C
C      ( 1) 13N(p,g)14O                                                C
C      ( 2) 15O(alpha,g)19Ne                                           C
C      ( 3) 25Al(p,g)26Si                                              C
C      ( 4) 17F(p,g)18Ne                                               C
C      ( 5) 18F(p,g)19Ne                                               C
C      ( 6) 19Ne(p,g)20Na                                              C
C      ( 7) 20Na(p,g)21Mg 532 3/2-                                     C
C      ( 8) 21Na(p,g)22Mg(220)                                             C
C      ( 9) 23Mg(p,g)24Al                                              C
C      (10) 26mAl(p,g)27Si                                             C
C      (11) 7Be(p,g)8B                                                 C
C      (12) 21Na(d,n)22Mg
C      (13) 23Na(d,n)24Mg 2+                                           C
C      (14) 23Na(p,g)24Mg 2+                                           C
C      (15) 20Ne(p,g)21Na nonres
C      (16) 20Na(p,g)21Mg131                                           C
C      (17) 22Ne(alpha,g)26Mg
C      (18) 21Na(p,g)22Mg(825)
C      (19) 12C(a,g)16O 
C======================================================================C
C
C-- MOD 10/06/03 C.Ruiz
C-- Gamma cascades (isotropic emission) are included for
C-- product nuclei through up to 32 states (including final 
C-- ground state).
C--
C-- Each product energy level is defined as a GEANT particle.
C-- Mass in GeV, lifetime and branching ratios to the lower states
C-- are specified for each level.
C--
C-- Within this routine the array  BRAT and MODE   specify
C-- the decay branches. MODE = part1 + 100*part2
C-- where part1 is the GEANT particle number correspnding to the
C-- energy level or gamma (=1).
C--
C--                   LABEL     IDPART
C--  resonance                   81
C--                              82
C--                              83
C--                              84
C--                              85
C--                              86
C--                              .
C--                              .
C--  ground state              irecoil < 100
C--
C=======================================================================
C
      ireaction = abs(lkine)
C
      Select case (ireaction)
C
      case(0)
C
        STOP
C
      case(1)
C
C       ' (1) 13N(p,g)14O '
C
        zbeam =  7.
        abeam = 13.
        atarg =  1.
        aprod = atarg + abeam
C
        resenerg = 0.526
        reswidth = 0.000037
C
        elevel = 0.0
C
        write(6,*)'|**** 13N(p,g)14O reaction NOT implemented yet ****|'
        write(6,*)'Resonance energy ', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(2)
C
C       ' (2) 15O(alpha,g)19Ne '
C
        zbeam =  8.
        abeam = 15.
        atarg =  4.
        zprod = 10.
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
        devmass = 2855.4E-6
        aamass  = abeam*amugev + devmass
        tlif    = 122.2
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'O15',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        resenerg = 0.5036
        reswidth = 1.E-11
        aamass   = aamass + resenerg/1000. + hemass
        tlif     = hbar/reswidth
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Ne19_3/2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Ne19 gamma decays from resonance
C
        devmass = 1751.1E-6
        prodm   = aprod*amugev + devmass
C
        elevel  = 1.536
        aamass  = prodm + elevel/1000.
        tlif    = 2.8E-11
C
        CALL gspart(82,'3_Ne19_3/2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = .275
        aamass = prodm + elevel/1000.
        tlif   = 6.3E-11
C
        CALL gspart(83,'2_Ne19_1/2',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel =.238
        aamass = prodm + elevel/1000.
        tlif   = 2.6E-8
C
        CALL gspart(84,'1_Ne19_5/2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 85
        irecoil = 85
C
        tlif = 1000.
C
        CALL gspart(85,'Ne19_1/2+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1) = 80.
        mode(1) = 1 + 100*85
        brat(2) = 15.
        mode(2) = 1 + 100*83
        brat(3) =  5.
        mode(3) = 1 + 100*82
C
        CALL uzero(brat,4,6)
        CALL uzero(mode,4,6)
C
        CALL gsdk(81,brat,mode)
C
C--     3/2+ state
C
        brat(1) = 95.
        mode(1) = 1 + 100*84
        brat(2) =  5.
        mode(2) = 1 + 100*83
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(82,brat,mode)
C
        brat(1) = 100.
        mode(1) = 1 + 100*95
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(83,brat,mode)
        CALL gsdk(84,brat,mode)
C
        write(6,*)'|**** 15O(alpha,gamma)19Ne reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
      case(3)
C
C       ' (3) 25Al(p,g)26Si '
C
        zbeam = 13.
        abeam = 25.
        atarg =  1.
        zprod = 14.
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
        devmass = -8915.7E-6
        aamass  = abeam*amugev + devmass
        tlif    = 7.18
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Al25',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        resenerg = 0.452   !calculated state...see C. Illiadis Phys
C                           Rev C53(1)(1995)475
        reswidth = 0.00006
        aamass   = aamass + resenerg/1000. + hmass
        tlif     = hbar/reswidth
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Si26_3+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Si26 gamma decays from resonance
C
        devmass = -7145.E-6
        prodm   = aprod*amugev + devmass
C
        elevel = 4.183
        aamass = prodm + elevel/1000.
        tlif    = 150.E-15
C
        CALL gspart(82,'4_Si26_3+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 3.756
        aamass = prodm + elevel/1000.
        tlif   = 700.E-15
C
        CALL gspart(83,'3_Si26_3+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 2.783
        aamass = prodm + elevel/1000.
        tlif   = 210.E-15
C
        CALL gspart(84,'2_Si26_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 1.796
        aamass = prodm + elevel/1000.
        tlif   = 620.E-15
C
        CALL gspart(85,'1_Si26_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 86
        irecoil=86
C
        tlif = 1000.
C
        CALL gspart(96,'Si26_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1) = 87.
        mode(1) = 1 + 100*82
        brat(2) = 8.
        mode(2) = 1 + 100*83
        brat(3) = 5.
        mode(3) = 1 + 100*85
C
        CALL uzero(brat,4,6)
        CALL uzero(mode,4,6)
C
        CALL gsdk(81,brat,mode)
C
        brat(1) = 47.
        mode(1) = 1 + 100*84
        brat(2) = 53.
        mode(2) = 1 + 100*85
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(82,brat,mode)
C
        brat(1) = 70.
        mode(1) = 1 + 100*85
        brat(2) = 30.
        mode(2) = 1 + 100*84
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(83,brat,mode)
C
        brat(1) = 69.
        mode(1) = 1 + 100*85
        brat(2) = 31.
        mode(2) = 1 + 100*86
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(84,brat,mode)
C
        brat(1) = 100.
        mode(1) = 1 + 100*86
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(85,brat,mode)
C
        write(6,*)'|**** 25Al(p,g)26Si  reaction ****|'
        write(6,*)'Resonance energy',resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
      case(4)
C
C       ' (4) 17F(p,g)18Ne '
C
        zbeam =  9.
        abeam = 17.
        atarg =  1.
        aprod = abeam +atarg
C
        resenerg = 0.64
        reswidth = 0.
C
        elevel = 2.67
C
        write(6,*)'|**** 17F(p,g)18Ne reaction NOT implemented ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(5)
C
C       ' (5) 18F(p,g)19Ne '
C
        zbeam =  9.
        abeam = 18.
        atarg =  1.
C
        resenerg = 0.3308
        reswidth = 0.0
C
        elevel = 6.742
C
        write(6,*)'|**** 18F(p,g)19Ne reaction NOT implemented ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(6)
C
C       ' (6) 19Ne(p,g)20Na '
C
        zbeam = 10.
        abeam = 19.
        atarg =  1.
        zprod = 11.
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
        devmass = 1751.E-6
        aamass  = abeam*amugev + devmass
        tlif    = 1.
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Ne19',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        resenerg = 0.451
        reswidth = 7.E-9       !w=1 don't know spins
        aamass   = aamass + resenerg/1000. + hmass
        tlif     = hbar/reswidth
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Na20',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Na20 gamma decays from resonance
C
        elevel = 2.660
	aamass = aamass - elevel/1000.
C
C--     ground state --> idpart = 82
C
        irecoil = 82
        tlif = 1.                        ! made up
C
        CALL gspart(82,'gs_Na20',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1) = 100.
	mode(1) = 1 + 100*82
C
        CALL uzero(brat,2,6)
	CALL uzero(mode,2,6)
C
	CALL gsdk(81,brat,mode)
C
        write(6,*)'|**** 19Ne(p,gamma)20Na reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
      case(7)
C
C       ' (7) 20Na(p,g)21Mg536 '
C
        zbeam = 11.
        abeam = 20.
        atarg =  1.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
       	devmass = 6845E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 0.03
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na20',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        resenerg = 0.536
        reswidth = hbar/tlif
        aamass   = aamass + resenerg/1000. + hmass
        ubuf(1)  = fkine(2)
        tlif   = 4.0E-14
C
        CALL gspart(81,'res_Mg21_536',8,aamass,zprod,tlif,ubuf,nubuf)
C    Define states for Mg21 gamma decays from resonance state
        devmass = 10912E-6
        prodm   = aprod*amugev + devmass

C
        elevel = 0.0
        amass = prodm +elevel/1000.
        tlif    = 1000.
        irecoil = 82
C
        CALL gspart(82,'Mg21_gs',8,aamass,zprod,tlif,ubuf,nubuf)

C--     branch info -- resonance decays
C
        brat(1) = 100.
        mode(1) = 1 + 100*82
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(81,brat,mode)
C
C
C
        write(6,*)'|**** 20Na(p,g)21Mg reaction  ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        
C
      case(8)
C
C       ' (8) 21Na(p,g)22Mg(220) '
C
        zbeam = 11.
        abeam = 21.
        atarg =  1.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
       	devmass = -2184.3E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 0.03
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na21',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        tlif     = 4.E-14
        resenerg = 0.2124
        reswidth = hbar/tlif
        aamass   = aamass + resenerg/1000. + hmass
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Mg22_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Mg22 gamma decays from resonance
C
        devmass = -396.8E-6
        prodm   = aprod*amugev + devmass
C
        elevel  = 1.246
        aamass  = prodm + elevel/1000.
        tlif    = 3.E-11
C
        CALL gspart(82,'Mg22_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 83
C
        irecoil=83
        tlif   = 1000.
C
        CALL gspart(83,'Mg22_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1) = 87.
        mode(1) = 1 + 100*82
        brat(2) = 13.
        mode(2) = 1 + 100*83
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(81,brat,mode)
C
        brat(1) = 100.
        mode(1) = 1 + 100*83
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(82,brat,mode)
C
        write(6,*)'|**** 21Na(p,g)22Mg reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
      case(9)
C
C       ' (9) 23Mg(p,g)24Al '
C
        zbeam = 12.
        abeam = 23.
        atarg =  1.
C
        resenerg = 0.51
        reswidth = 0.0
C
        elevel = 2.38
C
        write(6,*)'|**** 23Mg(p,g)24Al reaction NOT implemented ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(10)
C
C       ' (10) 26mAl(p,g)27Si '
C
        zbeam = 13.
        abeam = 26.
        atarg =  1.
C
        resenerg = 0.201
        reswidth = 0.
C
        elevel = 7.893
C
        write(6,*)'|**** 26mAl(p,g)27Si reaction NOT implemented ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(11)
C
C       ' (11) 7Be(p,g)8B '
C
        zbeam = 4.
        abeam = 7.
        atarg = 1.
C
        resenerg = 0.2
        reswidth = 0.
C
        elevel = 0.338
C
        write(6,*)'|**** 7Be(p,g)8B reaction NOT implemented ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
        STOP
C
      case(12)
C
C       ' (12) 21Na(d,n)22Mg '
C
        zbeam = 11.
        abeam = 21.
        atarg =  2.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
       	devmass = -2184.3E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 0.03
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na21',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     nonresonant level populated --> idpart = 81
C
        tlif     = 1e-20  !10 KeV width
        resenerg = 0.00
        reswidth = hbar/tlif
        aamass = sqrt((aamass+deutmass)**2 + 2.*deutmass*beamenerg*.001)
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'nonres_Mg23_',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Mg22  from neutron decays from resonance
C
        devmass = -396.8E-6
        prodm   = (aprod-1)*amugev + devmass !gs of 22Mg
C

        elevel  = 4.401

        aamass  = prodm + elevel/1000.
        tlif    = 3.E-11
C
        CALL gspart(82,'Mg22_2+_2',8,aamass,zprod,tlif,ubuf,nubuf)
        elevel  = 1.246

        aamass  = prodm + elevel/1000.
        tlif    = 3.E-11
C
        CALL gspart(83,'Mg22_2+_1',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 84
C
        irecoil=84
        tlif   = 1000.
C
        CALL gspart(84,'Mg22_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1)= 100.
        mode(1)= 13+100*82
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
        CALL gsdk(81,brat,mode)
C
C
        brat(1) = 87.
        mode(1) = 1 + 100*83
        brat(2) = 13.
        mode(2) = 1 + 100*84

C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(82,brat,mode)
C
        brat(1) = 100.
        mode(1) = 1 + 100*84
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(83,brat,mode)
C
        write(6,*)'|**** 21Na(d,ng)22Mg reaction ****|'
        write(6,*)' 100% to gs  + neutron'
      case(13)
C
C       ' (12) 23Na(d,n)24Mg 2+ '
C
        zbeam = 11.
        abeam = 23.
        atarg =  2.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
       	devmass = -9530.0E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 1000.
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na23',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     nonresonant level populated --> idpart = 81
C
        tlif     = 1e-20  !10 KeV width
        resenerg = 0.00
        reswidth = hbar/tlif
        aamass = sqrt((aamass+deutmass)**2 + 2.*deutmass*beamenerg*.001)
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'nonres_Mg25_',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Mg24  from neutron decays from resonance
C
        devmass = -13930.7E-6
        prodm   = (aprod-1)*amugev + devmass !gs of 24Mg
C

        elevel  = 1.368

        aamass1  = prodm + elevel/1000.
        if(aamass .lt. aamass1) 
     &  write(6,*) "Beam energy below 2+ threshold"
        tlif    = 3.E-11
C
        CALL gspart(82,'Mg24_2+_2',8,aamass1,zprod,tlif,ubuf,nubuf)
C--     ground state --> idpart = 93
C
        irecoil=83
        tlif   = 1000.
C
        CALL gspart(83,'Mg24_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1)= 100.
        mode(1)= 13+100*82
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
        CALL gsdk(81,brat,mode)
C
C
        brat(1) = 100.
        mode(1) = 1 + 100*83

C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(82,brat,mode)
C
C
        write(6,*)'|**** 23Na(d,ng)24Mg reaction ****|'
        write(6,*)' 100% to 2+   + neutron'
C
      case(14)
C
C       ' (14) 23Na(p,g)24Mg '
C
        zbeam = 11.
        abeam = 23.
        atarg =  1.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 90
C
       	devmass = -9530.0E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 0.03
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na23',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 91
C
        tlif     = 4.E-14
        elevel = 1.368
        devmass = -13930.7E-6
        prodm   = aprod*amugev + devmass
        resenerg = (prodm + elevel/1000. -aamass -hmass)*1000.
        if (resenerg .le.  0.) then
           resenerg = 0.0
           elevel = (aamass + hmass -prodm)*1000.
           write(6,*) " resonance energy negative - set to 0!"
        end if
        aamass = aamass+ hmass + resenerg/1000.
        reswidth = hbar/tlif
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Mg24_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Mg24 gamma decays from resonance
C
C
C
C--     ground state --> idpart = 82
C
        irecoil=82
        tlif   = 1000.
C
        CALL gspart(82,'Mg24_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
C
        brat(1) = 100.
        mode(1) = 1 + 100*82
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(81,brat,mode)
C
        write(6,*)'|**** 23Na(p,g)24Mg reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
      case(18)
C
C       ' (18) 21Na(p,g)22Mg(822) '
C
        zbeam = 11.
        abeam = 21.
        atarg =  1.
        zprod = 12
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
       	devmass = -2184.3E-6
       	aamass  = abeam*amugev + devmass
        tlif    = 0.03
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'Na21',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        tlif     = 84.0e-19
        resenerg = 0.8225
        reswidth = hbar/tlif/2 !This is gamma/2 =HWHM for BW
        aamass   = aamass + resenerg/1000. + hmass
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_Mg22_822',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     Define states in Mg22 gamma decays from resonance
C
        devmass = -396.8E-6
        prodm   = aprod*amugev + devmass
C
        elevel = (aamass -prodm)*1000.
        write(6,*)'|**** 21Na(p,g)22Mg(822) reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth*2,' MeV ', 'level ', elevel,' MeV'
C
        elevel  = 4.401
        aamass  = prodm + elevel/1000.
        tlif    = 3.E-14
C
        CALL gspart(82,'Mg22_4401+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C
        elevel  = 3.308
        aamass  = prodm + elevel/1000.
        tlif    = 3.E-14
C
        CALL gspart(83,'Mg22_3308',8,aamass,zprod,tlif,ubuf,nubuf)
C
C
        elevel  = 1.246
        aamass  = prodm + elevel/1000.
        tlif    = 3.E-11
C
        CALL gspart(84,'Mg22_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 85
C
        irecoil=85
        tlif   = 1000.
C
        CALL gspart(85,'Mg22_0+',8,prodm,zprod,tlif,ubuf,nubuf)
C
C--     branch info -- resonance decays
        brat(1) = 50.
        mode(1) = 1+100*84
        brat(2) = 50.
        mode(2) = 1 + 100*85
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(81,brat,mode)

C
        brat(1) = 87.
        mode(1) = 1 + 100*84
        brat(2) = 13.
        mode(2) = 1 + 100*85
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
C
        CALL gsdk(82,brat,mode)
C
        brat(1) = 100.
        mode(1) = 1 + 100*84
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(83,brat,mode)
C        brat(1) = 100.
        mode(1) = 1 + 100*85
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
C
        CALL gsdk(84,brat,mode)
C
C
      case(19)
C
C       ' (19) 12C(alpha,g)16O '
C
        zbeam =  6.
        abeam = 12.
        atarg =  4.
        zprod = 8.
        aprod = atarg + abeam
C
C--     create beam particle --> idpart = 80
C
        devmass = 0.0E-6
        aamass  = abeam*amugev + devmass
        tlif    = 1000.
        ubuf(1) = fkine(1)
C
        CALL gspart(80,'C12gs',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C--     resonant level populated --> idpart = 81
C
        resenerg = 4.358
        reswidth = 7.E-05
        aamass   = aamass + resenerg/1000. + hemass
        print*, aamass
        tlif     = hbar/reswidth
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_O16_2+',8,aamass,zprod,tlif,ubuf,nubuf)

        elevel = 11.520
        print*, 'Resonant mass: ', aamass
        rmass = aamass

        write(6,*)'|**** 12C(alpha,gamma)16O reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ', ' width ',
     &  reswidth,' MeV ', 'level ', elevel,' MeV'
C
C--     Set common block variables for cross-section
C
        m1 = abeam*amugev + devmass
        m2 = hemass
        z1 = zbeam
        z2 = 2.
        er = resenerg
        gp = 70./1000.
        gg = 0.007/1000.
        omg = 5.
        ell = 1.
        ires = 81
C
C--     Define states in O16 gamma decays from resonance
C
        devmass = -4736.998E-6
        prodm   = aprod*amugev + devmass
C
        elevel  = 11.260
        aamass  = prodm + elevel/1000.
        tlif    = hbar/2500.E-06
C
        CALL gspart(82,'12_O16_0+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 11.0967
        aamass = prodm + elevel/1000.
        tlif   = hbar/0.28E-06
C
        CALL gspart(83,'11_O16_4+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 11.080
        aamass = prodm + elevel/1000.
        tlif   = hbar/12.E-06
C
        CALL gspart(84,'10_O16_3+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 10.957
        aamass = prodm + elevel/1000.
        tlif = 5.5E-15
C
        CALL gspart(85,'9_O16_0-',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 10.356
        aamass = prodm + elevel/1000.
        tlif = hbar/26.E-06
C
        CALL gspart(86,'8_O16_4+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 9.8445
        aamass = prodm + elevel/1000.
        tlif = hbar/0.62E-06
C
        CALL gspart(87,'7_O16_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 9.585
        aamass = prodm + elevel/1000.
        tlif = hbar/420.E-06
C
        CALL gspart(88,'6_O16_1-',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 8.8719
        aamass = prodm + elevel/1000.
        tlif = 125.E-15
C
        CALL gspart(89,'5_O16_2-',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 7.11685
        aamass = prodm + elevel/1000.
        tlif = 8.3E-15
C
        CALL gspart(90,'4_O16_1-',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 6.9171
        aamass = prodm + elevel/1000.
        tlif = 4.7E-15
C
        CALL gspart(91,'3_O16_2+',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 6.12989
        aamass = prodm + elevel/1000.
        tlif = 18.4E-12
C
        CALL gspart(92,'2_O16_3-',8,aamass,zprod,tlif,ubuf,nubuf)
C
        elevel = 6.0494
        aamass = prodm + elevel/1000.
        tlif = 67.E-12
C
        CALL gspart(93,'1_O16_0+',8,aamass,zprod,tlif,ubuf,nubuf)
C
C--     ground state --> idpart = 104
        irecoil = 94
C
        tlif = 1000.
C
        CALL gspart(94,'gs_O16_0+',8,prodm,zprod,tlif,ubuf,nubuf)
        print*, 'Ground state mass: ', prodm
C
C--     branch info -- resonance decays
C

        brat(1) = 90.99
        mode(1) = 1 + 100*94
        brat(2) = 4.19
        mode(2) = 1 + 100*93
        brat(3) = 4.00
        mode(3) = 1 + 100*91
        brat(4) = 0.82
        mode(4) = 1 + 100*90
C
        CALL uzero(brat,5,6)
        CALL uzero(mode,5,6)
        CALL gsdk(81,brat,mode)
C
C--     12th ex. state
C
C
        CALL uzero(brat,1,6)
        CALL uzero(mode,1,6)
C
        CALL gsdk(82,brat,mode)
C
C--     11th ex. state
C
        brat(1) = 55.25
        mode(1) = 1 + 100*92
        brat(2) = 44.75
        mode(2) = 1 + 100*91
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
        CALL gsdk(83,brat,mode)
C
C--     10th ex. state
C
        CALL uzero(brat,1,6)
        CALL uzero(mode,1,6)
        CALL gsdk(84,brat,mode)
C
C--     9th ex. state
C
C
        brat(1) = 100.
        mode(1) = 1 + 100*90
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
        CALL gsdk(85,brat,mode)
C
C--     8th ex. state
C
        brat(1) = 1.57
        mode(1) = 1 + 100*92
        brat(2) = 98.43
        mode(2) = 1 + 100*91
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
        CALL gsdk(86,brat,mode)
C
C--     7th ex. state
C
        brat(1) = 60.98
        mode(1) = 1 + 100*94
        brat(2) = 18.29
        mode(2) = 1 + 100*93
        brat(3) = 20.73
        mode(3) = 1 + 100*91
C
        CALL uzero(brat,4,6)
        CALL uzero(mode,4,6)
        CALL gsdk(87,brat,mode)
C
C--     6th ex. state
C
        brat(1) = 89.29
        mode(1) = 1 + 100*94
        brat(2) = 10.71
        mode(2) = 1 + 100*91
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
        CALL gsdk(88,brat,mode)
C
C--     5th ex. state
C
        brat(1) = 7.22
        mode(1) = 1 + 100*94
        brat(2) = 0.12
        mode(2) = 1 + 100*93
        brat(3) = 77.67
        mode(3) = 1 + 100*92
        brat(4) = 3.57
        mode(4) = 1 + 100*91
        brat(5) = 11.42
        mode(5) = 1 + 100*90
C
        CALL uzero(brat,6,6)
        CALL uzero(mode,6,6)
        CALL gsdk(89,brat,mode)
C
C--     4th ex. state
C
        brat(1) = 99.93
        mode(1) = 1 + 100*94
        brat(2) = 0.07
        mode(2) = 1 + 100*92
C
        CALL uzero(brat,3,6)
        CALL uzero(mode,3,6)
        CALL gsdk(90,brat,mode)
C
C--     3rd ex. state
C
        brat(1) = 99.97
        mode(1) = 1 + 100*94
        brat(2) = 0.027
        mode(2) = 1 + 100*93
        brat(3) = 0.008
        mode(3) = 1 + 100*92
C
        CALL uzero(brat,4,6)
        CALL uzero(mode,4,6)
        CALL gsdk(91,brat,mode)
C
C--     2nd ex. state
C
        brat(1) = 100.
        mode(1) = 1 + 100*94
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
        CALL gsdk(92,brat,mode)
C
C--     1st ex. state
C  
        brat(1) = 100.
        mode(1) = 1 + 100*94
C
        CALL uzero(brat,2,6)
        CALL uzero(mode,2,6)
        CALL gsdk(93,brat,mode)



C
C--> Case 20: reaction drawn from user input card
C    only implemented for (a,g) or (p,g) reactions at present
C    CR: 22.07.2003
C    Now implemented for (c12,g) reactions.
C    JS: 15.02.2004
        case(20)
C
        INPUT = 'INPUT'
        CALL getenv(INPUT,CARDNAME)
        IF(CARDNAME.eq.'   ')CARDNAME='c12ag.dat'
        INQUIRE(file=CARDNAME,exist=fexist)
        IF(fexist)then
          open(20,file=CARDNAME,status='old')
        Else
          print*, 'No input reaction card! (LKINE set to 20)'
          stop
        Endif
        read(20,NML=params)
        close(20)

C    Setup beam particle
C
        aprod = atarg + abeam
        devmass = beam_mass_excess
        aamass  = abeam*amugev + devmass
        tlif    = beamlifetime
        ubuf(1) = fkine(1)
C
        CALL gspart(80,beamtyp//'gs',8,aamass,zbeam,tlif,ubuf,nubuf)
C
C    Create resonant particle
C
C    Note: Resonant particle energy based on beam mass, target mass and
C          variable resenerg.  Not based on energy levels specified in INPUT
C          file.  (However, energy of states below resonance are based on
C          eneryg levels specified in INPUT)
C
        If(atarg.eq.4.)then
        targtyp = 'a'
        targmass = hemass
        aamass   = aamass + resenerg/1000. + hemass
        print*, aamass, hemass, resenerg
        Elseif(atarg.eq.1)then
        targtyp = 'p'
        targmass = hmass
        aamass   = aamass + resenerg/1000. + hmass
        Elseif(atarg.eq.12)then
        targtyp = '12C'
        targmass = c12mass
        aamass   = aamass + resenerg/1000. + c12mass 
        Endif
        tlif     = hbar/((part_width+gam_width)/1000.)
        ubuf(1)  = fkine(2)
C
        CALL gspart(81,'res_'//rectyp//'_'//num(rstate)//'',8,
     &              aamass,zprod,tlif,ubuf,nubuf)
        ires = 81
        elevel = level(rstate)
        print*, 'Resonant mass: ', aamass
C        rmass = aamass
        resmass = aamass

        

        write(6,*)'|**** '//beamtyp//'('//targtyp//',gamma)'//rectyp//
     &           ' reaction ****|'
        write(6,*)'Resonance energy', resenerg, ' MeV ',
     &   'level ', elevel,' MeV'  

C
C    Setup excited states below resonance
C
        prodm   = aprod*amugev + recoil_mass_excess
        DO i = 0, rstate-1

        elevel = level(i)
        aamass = prodm + elevel/1000.
        tlif = life(i)
C
        CALL gspart(81+rstate-i,''//rectyp//'_'//num(i)//'',8,
     &              aamass,zprod,tlif,ubuf,nubuf)
        ENDDO
        irecoil = 81+rstate
C
C    Setup decay branching ratios and modes
C
        DO i = 1, rstate
        CALL uzero(brat,1,10)
        CALL uzero(mode,1,10)
         DO j = 1, 10
         if(br(i,j).ne.0)then
          brat(j) = br(i,j)
          mode(j) = 1 + 100*(irecoil-md(i,j))
         endif
         ENDDO
        CALL gsdk(81+rstate-i,brat,mode)
        ENDDO

C
C    Set cross-section variables
C
        gp = part_width
        gg = gam_width
        er = resenerg
        omg = spin_stat_fac
        ell = ell
        m1 = abeam*amugev + beam_mass_excess
        print*, m1
        If(atarg.eq.1.)then
        m2 = hmass
        print*, '++++++', m2
        Elseif(atarg.eq.4.)then
        m2 = hemass
        Elseif(atarg.eq.12.)then
        m2 = c12mass
        Endif
        z1 = zbeam
        z2 = ztarg
        
C     alpha source
        case(21)
        alpha = .true.
        atarg = 1
        aamass = hemass
        ubuf(1) = FKINE(1)
        ubuf(2) = FKINE(2)
        tlif = 1000.
        CALL gspart(80,'Alpha',8,aamass,2.0,tlif,ubuf,nubuf)





         End select
C
      RETURN
      END
C










