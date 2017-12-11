c---- 67---- gamma angular distribution

      REAL FUNCTION angdist(X)

      REAL pi
      parameter (pi = 3.14159265358979)

c$$$c$$$  A uniform angular distribution for gammas
c$$$      angdist = 1
c$$$  A dipole angular distribution for gammas
      angdist = (3./(8.*pi))*X**2
c$$$c$$$  A quad. angular distribution for gammas
c$$$      angdist = (15./(8.*pi))*(1.-X**2)*X**2

      END
