c---- 67---- gamma angular distribution

      REAL FUNCTION angdist(X)

      REAL pi
      parameter (pi = 3.14159265358979)

C  A uniform angular distribution for gammas
      angdist = 1
CC  A dipole angular distribution for gammas
C      angdist = (3./(8.*pi))*(1.-X**2)
CC  A quad. angular distribution for gammas
C      angdist = (15./(8.*pi))*(1.-X**2)*X**2

      END
