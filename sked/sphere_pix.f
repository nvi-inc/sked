! The following routines break the sphere into equal size pixels.
! We start by specifying the maxium  angle from the north pole.
! How many bands, and how many pixels per band.
! From this we compute the angle at which we go from one band to another.
!************************************************************************
      subroutine init_sphere_pix_ang(dang_max, num_bands, ipix_bands,
     >  dang_pix_band)
! Calculate where the angular break_points need to be.
      implicit none
! Input
      double precision dang_max !max distance from the north pole (radians)
      integer num_bands         !number  of  bands
      integer ipix_bands(*)     !number of pixels per band
! output
      double precision dang_pix_band(*)   !place where we change from one band to another (radians)

! history
!  2007Oct17 JMGipson

! local
      integer i
      integer ipix_tot             !Total number of pixels
      double precision dAreaTot    !area of sphere we are interested.
      double precision dAreaPix
      double precision dAreaDisk

! calculate the total number of pixels
      ipix_tot=0
      do i=1,num_bands
        ipix_tot=ipix_tot+ipix_bands(i)
      end do

      dAreaTot=1.d0-dcos(dang_max)     ! Should really multiply by 2 pi, but this drops out below
      dAreaPix=dAreaTot/dble(ipix_tot)

      dAreaDisk=0.d0
      do i=1,num_bands
        dAreaDisk=DAreaDisk+dble(ipix_bands(i))*dAreaPix
        dang_pix_band(i) =dacos(1.d0 - dAreaDisk)
      end do

      return
      end
!***************************************************************************
      subroutine sphere_pix(az,el, num_bands, ipix_bands,dang_pix_band,
     >  isphere_pix,ierr)
      implicit none
! compute which pixel we are on
! input
      double precision az       !Azimuth in radians
      double precision el       !Elevation in radians
      integer num_bands         !how many  bands
      integer ipix_bands(*)
      double precision dang_pix_band(*)     !place where we change from one band to the next
! Output is
      integer isphere_pix, ierr
! History
! 2007Oct17 JMGipson

! local
      integer i
      double precision co_el
      double precision az_local
      double precision piov2, twopi
      double precision dtemp
      double precision rad2deg
 
!      write(*,*) az,el
      piov2=acos(0.d0)
      twopi=piov2*4.
      rad2deg=360.d0/twopi
      co_el =  piov2-el
      az_local=dmod(az+twopi,twopi)   !Make Az between 0 and two pi
      if(az_local .lt. 0.d0) then
         write(*,*) "error:! ", az_local*rad2deg,az*rad2deg
      else if(az_local .gt. twopi) then
         write(*,*) "And your wife!",az_local
      endif

      ierr=-2
      isphere_pix=0

      if(co_el .gt. dang_pix_band(num_bands)) then
         write(*,'(a, 2f8.2)')
     >    "Sphere_pix: Warning! Elevation lower than minimum band: ",
     >     el*rad2deg, dang_pix_band(num_bands)*rad2deg
!         write(*,'(i4,6f8.2)')  num_bands,
!     >    (dang_pix_band(i)*rad2deg,i=1,num_bands)
!         pause
         co_el=dang_pix_band(num_bands)
      endif

      do i=1,num_bands
        if(co_el .le. dang_pix_band(i)) then
           ierr=0
           dtemp=(az_local/twopi)*dble(ipix_bands(i))
           isphere_pix=int(dtemp)+1+isphere_pix
           return
        endif
        isphere_pix=isphere_pix+ipix_bands(i)
      end do
! Should never get here.
      write(*,*) "Probable position error!"
      write(*,'("AZEL ",2f8.2)') rad2deg*az, rad2deg*el
      return
      end

