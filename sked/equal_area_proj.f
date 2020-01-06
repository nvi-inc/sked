! *************************************************************
      subroutine equal_area_proj(rlon,rlat,x,y)
      implicit none
      include '../skdrincl/constants.ftni'
!  equal_area_proj converts a ha-dec or lat-lon pair into x-y.
!  uses equal area projection about unit vector.
!  History
!  2006Oct30  JMGipson

! functions
      real*8 dot8
! input
      real*8 rlon,rlat  !radians.

! output
      real*8 x,y
! Common area.
      real*8 xvec(3),yvec(3),zvec(3)
      common /equal_area_proj_cmn/xvec,yvec,zvec

C Output:
C Local:
      double precision sin_lon,cos_lon,sin_lat,cos_lat
      real*8 xin,yin,zin
      real*8 vec(3)

! Initialization
      sin_lon =sin(rlon)
      cos_lon =cos(rlon)
      sin_lat =sin(rlat)
      cos_lat =cos(rlat)
! Compute unit vector. These are XYZ coordinats on original unit sphere.
      vec(1)=cos_lat*cos_lon
      vec(2)=cos_lat*sin_lon
      vec(3)=sin_lat
! compute projections into other unit vectors.
! These give new XYZ coordinates.
      xin=dot8(vec,xvec)
      yin=dot8(vec,yvec)
      zin=dot8(vec,zvec)

      x=xin*50.d0*sqrt(2.d0/(1+zin))
      y=yin*50.d0*sqrt(2.d0/(1+zin))
      return
      end
! **********************************************************************
      subroutine init_equal_area_proj(rlon,rlat)
      implicit none
!  2006Nov13 JMGipson.  Added end statement.
! passed
      real*8 rlon,rlat  !origen of projection, i.e., north pole
! Common block which is initialized.
      real*8 xvec(3),yvec(3),zvec(3)

      common /equal_area_proj_cmn/xvec,yvec,zvec
! local
      double precision sin_lon,cos_lon,sin_lat,cos_lat

! Initialization
      sin_lon=sin(rlon)
      cos_lon=cos(rlon)
      sin_lat =sin(rlat)
      cos_lat =cos(rlat)
! make vectors in X-Y-Z directions
      zvec(1)=cos_lat*cos_lon
      zvec(2)=cos_lat*sin_lon
      zvec(3)=sin_lat

      yvec(1)=-sin_lon
      yvec(2)=cos_lon
      yvec(3)=0.

      xvec(1)=sin_lat*cos_lon
      xvec(2)=sin_lat*sin_lon
      xvec(3)=cos_lat
      return
      end
 
















