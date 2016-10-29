c----67----
      REAL beam_mass_excess, recoil_mass_excess
      REAL part_width, gam_width, spin_stat_fac
      REAL level(0:15), life(0:15), ztarg
      REAL br(15,10), beamlifetime
      CHARACTER*4 beamtyp, rectyp
      
      INTEGER rstate, md(15,10)

      NAMELIST /params/ zbeam,abeam,atarg,ztarg,zprod,beam_mass_excess,
     &          recoil_mass_excess,
     &          resenerg,part_width,gam_width,spin_stat_fac,ell,ires,
     &          rstate,
     &          level,life,br,md, beamtyp,rectyp,beamlifetime
