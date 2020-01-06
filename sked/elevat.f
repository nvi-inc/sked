      subroutine elevat(deap,hss,pih,elv,az)
CHS-----------------------------------------------------
CHS Elevat was created in order to compute the azimuth
CHS and elevation angle for any site.
C
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C     implicit real*8(a-h,o-z)
C   
C Input:
      real*8 deap,hss,pih

C Output:
      real*8 elv,az

C Local:
      real*8 sine,xb,yb

      sine=dsin(pih)*dsin(deap)+dcos(pih)*dcos(deap)*dcos(hss)
C
      elv=dasin(sine)
C
      xb=dsin(hss)
      yb=dsin(pih)*dcos(hss)-dcos(pih)*dtan(deap)
      az=datan2(xb,yb)
      az=az+pi
C
      return
      end
