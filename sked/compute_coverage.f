      subroutine compute_coverage
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'pixelation.ftni'
      include '../skdrincl/constants.ftni'

! Function
      real*8 dot8

! local variables.
      real*8 Ug(3,max_stn)      ! Up  unit vector in geocentric co-ordinants
      real*8 Eg(3,max_stn)      ! East
      real*8 Ng(3,max_stn)      ! North
      real*8 up_lcl(3)          ! local up of other station in local coordiantes.
      save ug, eg,ng

      integer ipix 
! Below used to calculate sky_coverage. 
      logical kpix_mut(max_pixel,max_stn) !True==>this pixel visible from this site.
      integer num_pix_mut(max_stn)       !number pixels mutually visible
      integer num_pix_vis(max_stn)       !number pixels visible at this site.
      integer num_pix_cov(max_stn)       !number of pixels covered by a source
      integer nscans_stat(max_stn) !
      real*8  dist_avg(max_stn)          !average distance of pixel to nearest source
      real*8  dist_max(max_stn)          !maximum distance of pixel to nearest source.
      real*8  eff(max_stn)
      real*8  mu_eff(max_stn)
      integer num_scans_tot              
   
! local 
      integer i,j               ! counter    
      real*8 sin_el_min   
 
      if(nobs .eq. 0) then
         write(ludsp,*) "Please schedule some observations first!"
      endif
      call opfill 

! If this is the first time this is called, then we need to do some setup.
      if(kinit_pixel) goto 100
      
      kinit_pixel=.true.
      sin_el_min=sin(5.d0*deg2rad)
! Initilize the pixels.
      call compute_matrices(R)
      call compute_corners(v)
      do ipix=0,max_pixel-1
        call pixel2vector(ipix,ires,R,v,pix_vec4)
        pix_vec8(1:3,ipix+1)=pix_vec4(1:3)        !Convert from real*4 to real*8 and save. 
      end do

      do i=1,nstatn
! Negative sign on longitude converts from sked conventions to normal. 
        call make_triad(stnpos(2,i), -stnpos(1,i),
     >      eg(1,i),ng(1,i),ug(1,i))
      end do
    
      do i=1,nstatn
        num_pix_vis(i)=0                  !all pixels visible at this station.
        kpix_mut(1:max_pixel,i)=.false.   !pixels mutually visible at another station.
        num_pix_mut(i)=0

        do ipix=1,max_pixel
          if(pix_vec8(3,ipix) .ge. sin_el_min) then
            num_pix_vis(i)=num_pix_vis(i)+1
            do j=1,nstatn
              if(j .ne. i) then
! convert Up in geocentric at station j to up in local coordinates.
                call geo_2_ENU(Eg(1,i),Ng(1,i),ug(1,i),ug(1,j),up_lcl)
! Test to see if this pixel is visible at jstat
                if(dot8(pix_vec8(1,ipix),up_lcl) .ge. sin_el_min) then
                   num_pix_mut(i)=num_pix_mut(i)+1
                   kpix_mut(ipix,i)=.true.
                   goto 15
                endif
              endif
            end do
          endif
15        continue
        end do
      end do

! End of initilization.
100   continue      
      num_scans_tot=0
      do i=1,nstatn
         call compute_visibility(azimu(1,i),eleva(1,i),
     >       nobs, kpix_mut(1,i),nscans_stat(i),
     >       num_pix_cov(i), dist_avg(i), dist_max(i))
         num_scans_tot=num_scans_tot+nscans_stat(i)
      end do
 
      eff=0.d0
      do i=1,nstatn
        if(nscans_stat(i) .gt. 0) then
          eff(i)=float(num_pix_cov(i))/float(nscans_stat(i))*100.
        endif
        if(num_pix_vis(i) .gt.0) then
         mu_eff(i)=float(num_pix_mut(i))/float(num_pix_vis(i))*100
       endif
      end do       
  
      write(ludsp,'(a)') " Coverage Summary: " 
      write(ludsp,'(11x,64(a8,1x))') cstnna(1:nstatn), "Average "
      writE(ludsp,'(" PixMut  ",64(i6,3x))') num_pix_Mut(1:nstatn),
     > sum(num_pix_mut(1:nstatn))/nstatn
      writE(ludsp,'(" PixVis  ",64(i6,3x))') num_pix_vis(1:nstatn),
     >  sum(num_pix_vis(1:nstatn))/nstatn 
      write(ludsp,'(" % MutVis ",64(f6.1,3x))') mu_eff(1:nstatn),
     >  sum(mu_eff(1:nstatn))/nstatn   
      write(ludsp,'(" PixCvred ",64(i6,3x))') num_pix_cov(1:nstatn),
     >  sum(num_pix_cov(1:nstatn))/nstatn
      writE(ludsp,'(" Scans    ",64(i6,3x))') nscans_stat(1:nstatn),
     >   sum(nscans_stat(1:nstatn))/nstatn
      write(ludsp,'(" % Eff    ",64(f6.1,3x))') eff(1:nstatn),
     >   sum(eff(1:nstatn))/nstatn
      write(ludsp,'(" DistAvg  ",64(f6.1,3x))')
     >   dist_avg(1:nstatn)/deg2rad, 
     >   sum(dist_avg(1:nstatn))/deg2rad/float(nstatn)
      write(ludsp,'(" DistMax  ",64(f6.1,3x))') 
     >   dist_max(1:nstatn)/deg2rad, 
     >   sum(dist_max(1:nstatn))/deg2rad/float(nstatn)
      writE(ludsp,'(" Number of pixels ", i6, 
     >    "      Average Pixel radius ", f8.1," deg")')
     >     max_pixel, sqrt(4.d0/max_pixel)/deg2rad
      return
      end
! *************************************************
      subroutine compute_visibility(az,el,num_azel,
     >      kpix_mut,num_scans,num_pix_cov,dist_avg,dist_max)
      implicit none
! functions
      real*8 dot8

      include 'pixelation.ftni'
! passed     
      integer num_Azel
      real*8 az(num_azel),el(num_azel)
      logical kpix_mut(*)             !True of pixel is mutually visible by 2 or more stations.
! returned
      integer num_pix_cov             !number of pixels that have 1 or more scans
      integer num_scans
      real*8  dist_avg, dist_max

! local
      logical kpix_cov(max_pixel)      ! true indicating have a source in this pixel

      integer iazel          !azel counter
      integer ipix           !pixel counter
      integer ipix_vec       !which pixel is a given unit vector on

!     character*20 lfilout   !fileaname
!     integer nch

      real*8 src8(3)         !unit vector to source in local uen coordinates.
      real*4 src4(3)         !same thing, but as real*4
      character*1 lchar

      real*8 az_vec,el_vec   !az,el of vector

      real*8  dist           !min angular distance from pixel to nearest source
      real*8  dist_sum       !sum of angular distances.
      real*8  cos_ang        !cosine between two unit vectors, typically source, pixel
      real*8  cos_ang_max
      integer imax_dist_pix  !pixel which is farthest from all sources
      integer num_pix_mut    !number of pixels mutally visible.
                             !   = # elements of kpix_mut=.true.
      real*8 deg2rad
      real*8 piov2
      piov2  = acos(0.d0)
      deg2rad=piov2/90.d0

      kpix_cov=.false.
      num_pix_cov=0

       num_scans=0 
       do iazel=1,num_azel
! Compute azel vector.
!      write(*,*) el(iazel)
!      pause
      if(el(iazel) .gt. 0) then 
        num_scans=num_scans+1
        call make_src_vector(az(iazel),el(iazel),src8)
        src4=src8                       !convert from real*8 to real*4
        call vector2pixel(src4,ires,R,v,ipix_vec)
        if(.not.kpix_cov(ipix_vec)) then
           kpix_cov(ipix_vec)=.true.
           num_pix_cov=num_pix_cov+1
        endif
      endif 
      end do
!      close(2)
      close(3)
!      write(*,*) "Num covered at ", lstat," ", Num_pix_cov

      dist_max=0.
      dist_sum=0.0
      num_pix_mut=0

      do ipix=1,max_pixel 
        if(kpix_mut(ipix)) then
           num_pix_mut=num_pix_mut+1
           cos_ang_max=-1.d0
           
           do iazel=1,num_azel           
           if(el(iazel) .gt. 0) then
             call make_src_vector(az(iazel),el(iazel),src8)
             cos_ang=dot8(src8,pix_vec8(1,ipix))
             cos_ang_max=max(cos_ang,cos_ang_max) !find max cosine=nearest distnace
             if(cos_ang .gt. 1) then
!                write(*,*) "VEC ", pix_vec8(1:3,ipix)
!                write(*,*) "SRC ", src8(1:3)
             endif
           end if  
           end do
           if(cos_ang_max .gt. 1) then
             dist=0.d0                  !sometimes cos_Ang>1 because of rounding.
           else
             dist=acos(cos_ang_max)
           endif

           if(dist .gt. dist_max) then
              dist_max = dist
              imax_dist_pix = ipix
           endif
           dist_sum=dist_sum+dist
        endif
      end do
      dist_avg=dist_sum/num_pix_mut

!      write(*, '(" ",a, " AZEL COV  Avg  Max: ",2i4, 2f8.1)')
!     >  lstat,  num_azel, num_pix_cov,
!     >   (dist_sum/num_pix_mut)/deg2rad,  dist_max/deg2rad

      close(4)
      return
      end

  ! ****************************************
      subroutine unitvec2azel(vec,az,el)
! convert a unit vector to Az-el coordinates.
      implicit none
!input
      real*8 vec(3)
! output
      real*8 az,el

      el=asin(vec(3))
      if(abs(vec(1)) .lt. 1.e-4 .and. abs(vec(2)) .lt. 1.e-4) then
         az=0.d0
      else
        az=atan2(vec(1),vec(2))
      endif
      return
      end
! ********************************************************
      subroutine make_src_vector(az,el,src)
      implicit none
      real*8 az, el
      real*8 src(3)
      Src(2)=cos(el)*cos(az)    !note reversal of 1 and 2. This is because Az measured from due north.
      src(1)=cos(el)*sin(az)
      src(3)=sin(el)
      return
      end
! ***************************************************************************
      subroutine convert_azel(az1,el1,az2,el2)
      real*8 az1,el1,az2,el2
! Given az el at 1, find it at 2.

      real*8 U1g(3), E1g(3), N1g(3)  !UEN triad at site 1 in geocentric
      real*8 U2g(3), E2g(3), N2g(3)  !UEN triad at site 2 in geocentric
      real*8 U21(3), E21(3), N21(3)  !UEN triad at site 2 expressed in site's 1 ENU
      common /azel_conv/ U1g,E1g,N1g, U2g,E2g,N2g,   U21,E21,N21
! local
      real*8 Src1(3)                !source vector expressed in site1 ENU
      real*8 src2(3)                !source vector expressed in site2 EN"U
      real*8 piov2
      piov2  = acos(0.d0)

      Src1(2)=cos(el1)*cos(az1)    !note reversal of 1 and 2. This is because Az measured from due north.
      src1(1)=cos(el1)*sin(az1)
      src1(3)=sin(el1)
!      write(*,'(1x,A, 1x, 3f8.2)') "src_k ", src1
      src2(1)=dot8(n21,src1)
      src2(2)=dot8(e21,src1)
      src2(3)=dot8(u21,src1)

!     write(*,'(1x,A, 1x, 3f8.2)') "src2 ", src2
      el2 =  piov2- acos(src2(3))
      az2 =atan2(src2(2), src2(1))

      return
      end
!*************************************************
      subroutine make_triad(rlat,rlong, e,n,u)
      implicit none
! given lat, long of a point, make the ortho-normal triad
      real*8 rlat,rlong           !lat, long on sphere
      real*8 U(3), e(3), N(3)

      u(1) = cos(rlat)*cos(rlong)
      U(2) = cos(rlat)*sin(rlong)
      U(3) = sin(rlat)

      E(1)= -sin(rlong)
      E(2)= cos(rlong)
      E(3)= 0.d0

      N(1)= -sin(rlat)*Cos(rlong)
      N(2)= -sin(rlat)*sin(rlong)
      N(3)=  cos(rlat)
      return
      end subroutine
! ***************************************************
      subroutine geo_2_ENU(E,N,U, A_geo,A_ENU)
      implicit none
! function
      real*8 dot8
! passed
      real*8 E(3),N(3),U(3)     !local E,N,U vectors
      real*8 A_geo(3)           !vector in geocentric
! return
      real*8 A_enu(3)           !vector in local enu
      A_enu(1)=dot8(a_geo,E)
      A_enu(2)=dot8(a_geo,N)
      A_enu(3)=dot8(a_geo,U)
      return
      end subroutine    
! compute and print out coverage by site
      
     
