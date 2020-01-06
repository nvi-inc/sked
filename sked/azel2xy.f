      subroutine azel2xy(az,el,x,y)

! AEM 20050217 add implicit none
      implicit none

C  AZEL2XY converts an az/el pair to an x/y pair for
C  plotting.
C  930430 nrv created
      include '../skdrincl/constants.ftni'

C Input:
      real*4 az,el !degrees

C Output:
      real*4 x,y ! degrees

C Local:
      real*4 r

      r = 90.0*(1.0 - el/90.0)
      x = r * sin(az*deg2rad)
      y = r * cos(az*deg2rad)

      return
      end

