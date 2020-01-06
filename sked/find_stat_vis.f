      subroutine find_stat_vis(rlong,rlat, el_min,del_deg,rhadec)
! Find station visibility limits.
      implicit none
      include "../skdrincl/constants.ftni"


! on entry:
      double precision rlong     !longitude (radians)
      double precision rlat      !latitude
      double precision el_min    !minimum elevation
      double precision del_deg   !spacing in degrees
! on output
      double precision rhadec(2,*)  !contains unit vectorS along minimum elevation.
!                                !1st entry corresponds to az,el=(0, el_min)
!                                !2nd to                         (del_deg,el_min)
!                                ! etc.
! functons
      real*8 dot8
! local
      double precision sin_long,cos_long,sin_lat,cos_lat
      double precision xvec(3),yvec(3),zvec(3)
      double precision src_vec(3)
      double precision cos_elmin,sin_elmin
      double precision az
      double precision xyz(3)
      integer icnt

! Initialization
      sin_long=sin(rlong)
      cos_long=cos(rlong)
      sin_lat =sin(rlat)
      cos_lat =cos(rlat)

      xvec(1)=-sin_lat*cos_long
      xvec(2)=-sin_long
      xvec(3)=cos_lat*cos_long

      yvec(1)=-sin_lat*sin_long
      yvec(2)=cos_long
      yvec(3)=cos_lat*sin_long

      zvec(1)=cos_lat
      zvec(2)=0.
      zvec(3)=sin_lat

      cos_elmin=cos(el_min*deg2rad)
      sin_elmin=sin(el_min*deg2rad)


      icnt=0
      do az=0.0, twopi,del_deg*deg2rad
        src_vec(1)=cos_elmin*cos(az)
        src_vec(2)=cos_elmin*sin(az)
        src_vec(3)=sin_elmin
        icnt=icnt+1
        xyz(1)=dot8(src_vec,xvec)
        xyz(2)=dot8(src_vec,yvec)
        xyz(3)=dot8(src_vec,zvec)
        rhadec(2,icnt)=asin(xyz(3))
        rhadec(1,icnt)=atan2(xyz(2),xyz(1))
      end do
      return
      end






