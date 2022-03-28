      real function slew_ggao(az1,el1,az2,el2,
     &  az_off,az_vel,az_acc, el_off,el_vel,el_acc,slew0,lkind)
      implicit none            
! compute slew time accouting for radar mask at GGAO

! History
! 2021-02-22 Original version
! 2021-04-22 Modified to handle cass of az_off=0 
! 2021-11-10 Modified to have az_off, az_vel,az_acc passed from outside
      
! passed. 
      real az1,el1          !starting point   
      real az2,el2          !ending point
      real az_off           !az_off (seconds)  Settling time
      real az_vel           !max az velocity  (deg/sec)  maximum
      real az_acc           !az acceleration. 
      real el_off           !el_off (seconds)  Settling time
      real el_vel           !max velocity  (deg/sec)  maximum
      real el_acc           !acceleration. 
         
!returned      
      real slew0            !slew time w/o masks.  
      character*8 lkind     !Describes path taken       
! 
! Output
!     return value time to slew (seconds)
!     current and target positions are assumed to be outside of mask limits

! Function
      real slew_time 

! local   
! Internally make starting point be less than ending point. 
      real az_beg, el_beg          !beginning and ending az.   
      real az_end, el_end    
               
      real az_pk1, az_pk2         !location of peaks in az
      real el_pk                  !height of peaks   
      
      real az_pk1_lft, az_pk1_rt  !limits of peak
      real az_pk2_lft, az_pk2_rt  !limits of second peak 
      
! We break the motion into pieces     
      real az_slewt, el_slewt     !Time to slew in azimuth or elevation ignorning mask. 
      
      real az_slew1, el_slew1     !az and el slew time for first segment
      real az_slew2, el_slew2     !az and el slew time for second segment        
      real az_slew1p, az_slew2p   !Same  as above but don't account for deceleration
      real el_slew1p, el_slew2p           
                          
      real slewt                  !total slew time
      real az_mid1, az_mid2       !possible mid-points of az path. 
      real el_mid                 !one end point of path 

      real half_width             !Half width
      real fudge                  !extra amount to avoid mask.        
       
      data az_pk1/192./, az_pk2/552./,el_pk/42./
      fudge=1.d0                   !go above mask by 1 degree
      half_width= el_pk            !go to the right or the left by this factor.    
      
      az_pk1_lft=az_pk1-half_width
      az_pk1_rt =az_pk1+half_width
      az_pk2_lft=az_pk2-half_width
      az_pk2_rt =az_pk2+half_width
      
      el_mid = el_pk            
      
      if(az_off .gt. 0) then
         az_acc=az_vel/az_off
      else
         az_acc=100.d0               !very fast acceleration
      endif
      if(el_off .gt. 0) then
         el_acc=el_vel/el_off
      else
         el_acc=100.d0
      endif 
           
! Make sure we are always moving in one direction.
      if(az1 .le. az2) then
          az_beg=az1  
          el_beg=el1
          az_end=az2
          el_end=el2
      else
          az_beg=az2 
          el_beg=el2
          az_end=az1
          el_end=el1
      endif       
     
      az_slewt=slew_time(az1,az2,az_off,az_vel,az_acc)
      el_slewt=slew_time(el1,el2,el_off,el_vel,el_acc)      
     
      slew0=max(az_slewt,el_slewt)
      slewt=slew0 
      
      lkind="Normal"
!      goto 300
! Some simple cases:
    
      if(el_beg .ge. el_pk  .and. el_end .ge. el_pk)  goto 500            !Above the mask.
      If(az_beg .le. az_pk1_lft .and. az_end .le. az_pk1_lft) goto 500   !Both to the left of the first mask.
      If(az_beg .ge. az_pk2_rt .and. az_end .ge. az_pk2_rt)   goto 500   !Both to the right of the second mask.
      If((az_beg .ge. az_pk1_rt .and. az_beg .le. az_pk2_lft) .and.       !Both between the masks. 
     &   (az_end .ge. az_pk1_rt .and. az_end .le. az_pk2_lft)) goto 500   !

! This handles case where starting and ending below mask and both starting and ending points are in same valley.       
      if((el_beg .le. el_pk .and. el_end .le. el_pk)) then      !starting and ending below the peaks. 
        if((az_beg .le. az_pk1 .and. az_end .le. az_pk1) .or.   !Both to the left of the first mask.
     &     (az_beg .ge. az_pk2 .and. az_end .ge. az_pk2) .or.   !Both to the right of the second mask.
     &     (az_beg .ge. az_pk1 .and. az_beg .le. az_pk2) .and.  !Both between the masks. 
     &     (az_end .ge. az_pk1 .and. az_end .le. az_pk2)) then 
          goto 500 
        endif        
      endif              
       
! Handle some rare cases.  Both within LHS of mask or RHS of mask.   Assume normal slewing.
      if(az_beg .ge. az_pk1_lft .and. az_beg .le. az_pk1 .and. 
     &   az_end .ge. az_pk1_lft .and. az_end .le. az_pk1) goto 500
     
      if(az_beg .ge. az_pk2_lft .and. az_beg .le. az_pk2 .and. 
     &   az_end .ge. az_pk2_lft .and. az_end .le. az_pk2) goto 500
     
      if(az_beg .ge. az_pk1 .and. az_beg .le. az_pk1_rt  .and. 
     &   az_end .ge. az_pk1 .and. az_end .le. az_pk1_rt) goto 500
     
      if(az_beg .ge. az_pk2 .and. az_beg .le. az_pk2_rt  .and. 
     &   az_end .ge. az_pk2 .and. az_end .le. az_pk2_rt) goto 500        

! Below has little effect on WRMS
  
! In the region of a peak and going up. This is OK if going up from right side of peak. 
      lkind="NormUp"
      if(el_end .gt. el_beg .and. el_end .gt. el_pk) then
         if(az_beg .gt. az_pk1 .and. az_beg .lt. az_pk1_rt .or.
     &      az_beg .gt. az_pk2 .and. az_beg .lt. az_pk2_rt)  then
!            write(*,*) "Normup",az_beg,el_beg,az_end,el_end 
            goto 500
          endif
      endif
      lkind="NormDn"
! In the region of a peak and going done. This is OK if coming down from left side.
      if(el_beg .gt. el_end .and. el_beg .gt. el_pk) then
         if(az_end .gt. az_pk1_lft .and. az_end .lt. az_pk1 .or.
     &      az_end .gt. az_pk2_lft .and. az_end .lt. az_pk2) then
!         write(*,*) "NormDn", az_beg,el_Beg,az_end,el_end
         goto 500
        endif 
      endif                   
      
!For many of the remaining cases we split the motion into two or three line segments.
!Each line segment starts or ends at a peak. 
      el_mid = el_pk+fudge           !for many parts below assume that one line segment ends at a peak. 
      
! FIRST CASE.        
! The beginning and ending elevation are below the peak. 
! This means that we start in one valley and end in another. 
! (The case where we started and ended in the same valley are covered above.)      

! We split the calculation into several segments.
! 1. To the top of a peak.
! 2. Down from a peak.  (May not be the first peak as before. 
! 3. Optional:  travel time between the peaks.
! For segments 1&2:
!    For the elevation time we add in the full-offset since we come to a stop.
!    For the azimuth time we add in only 1/2 the offset since we only have to account for starting acceleration.
! For segment 3
!    We  don't have to account for azimuth acceleration since we are already at speed. 
 
100   continue 
      if(el_beg .le. el_mid .and. el_end .le. el_mid) then  !both starting and ending points below a peak.         
! Break the problem into pieces. 
! 1. What peak do we have to climb?
! 2. What peak do we descend.
! 3. Did we go over both peaks. 

! 1. Find which peak we are climbing   
        if(az_beg .le. az_pk1) then 
           az_mid1 =az_pk1_lft                    
        else
           az_mid1=az_pk2_lft      
        endif
! Find slew time for first segment.       
        az_mid1=max(az_beg,az_mid1)   !handles rare case when within rectangular mask
        az_slew1=slew_time(az_beg,az_mid1,az_off,az_vel,az_acc)
        el_slew1=slew_time(el_beg,el_mid, el_off,el_vel,el_acc)                     
        
! 2. Find which peak we are descending
        if(az_end .ge. az_pk2) then       
           az_mid2=az_pk2_rt
        else
           az_mid2=az_pk1_rt     
        endif     
! Find slew time for second segment        
        az_mid2=min(az_mid2,az_end)     !handles rare case when within rectangular mask
        az_slew2=slew_time(az_mid2,az_end,az_off,az_vel,az_acc)
        el_slew2=slew_time(el_mid,el_end, el_off,el_vel,el_acc) 

! Slew values used for comparison of time. 
! Subtract 1/2 offset because we don't worry about stopping/starting 
        az_slew1p=az_slew1-az_off/2
        az_slew2p=az_slew2-az_off/2
        el_slew1p=el_slew1-el_off/2
        el_slew2p=el_slew2-el_off/2
    
        if(az_slew1p .ge. el_slew1p .and. 
     &     az_slew2p .ge. el_slew2p) then
! One very long slew in azimuth        
           slewt=slew_time(az_beg,az_end,az_off,az_vel,az_acc)
        else if(az_slew1p .ge. el_slew1p .and. 
     &          az_slew2p .le. el_slew2p) then
! A long slew in Az followed by the descent in Elevation
! Subtract 1/2 of the offset because this coincides with el starting. 
           slewt=slew_time(az_beg,az_mid2,az_off,az_vel,az_acc)+el_slew2
     &            -az_off/2
        else if(az_slew1p .le. el_slew1p .and. 
     &          az_slew2p .ge. el_slew2p) then
           slewt=el_slew1+slew_time(az_mid1,az_end,az_off,az_vel,az_acc)
     &            -az_off/2
        else 
           slewt=el_slew1+ (az_mid2-az_mid1)/az_vel + el_slew2
        endif                     
        goto 500 
      endif
    
300   continue 
! SECOND CASE
! Start in a valley and and above a peak 
! --OR--
! Start above a peak and end in a valley.    
! In both cases ceck if we would hit a peak in the normal course of business.
! If we don't can use the normal slewing. 
      
! First case. Start low, come up high.
      if(el_beg .lt. el_end) then
         if(az_beg .lt. az_pk1) then
            az_mid1=az_pk1_lft
         else
            az_mid1=az_pk2_lft
         endif
      else
! Start high, come down low      
         if(az_beg .lt. az_pk1_rt) then 
            az_mid1=az_pk1_rt
          else
            az_mid1=az_pk2_rt
          endif 
      endif
      
      az_mid1=max(az_beg,az_mid1)  !middle can't be before beginning
      az_mid1=min(az_mid1,az_end)  !middle can't be after ending 
      
      az_slew1=slew_time(az_beg,az_mid1,az_off,az_vel,az_acc)
      el_slew1=slew_time(el_beg,el_mid, el_off,el_vel,el_acc) 
      
! This is slew time used for comparison. Don't worry about stopping      
      az_slew1p=abs(az_beg-az_mid1)/az_vel+az_off/2.
      el_slew1p=abs(el_beg-el_mid)/el_vel+az_off/2. 
      
      if(el_beg .lt. el_end) then     
         if(az_slew1p .ge. el_slew1p) goto 500  ! Don't hit side on the way up. Normal slew.
! Two possibilities.  
! 1. A long slew in elevation 
! 2. A slew in elevation followed by one in azimuth 
         az_slew2=slew_time(az_mid1,az_end,az_off,az_vel,az_acc)
         slewt=max(el_slewt,el_slew1p+az_slew2)
      else 
         if(el_slew1p .gt. az_slew1p) goto 500     !don't hit top on the way down. Normal slew
! Two possibilities.
! 1. A long slew in azimuth
! 2. A slew in azimuth followed by one in elevation.     
         el_slew2=slew_time(el_mid,el_end, el_off,el_vel,el_acc)
! Use az_slew1p because antenna is still moving. It will stop while el is moving.          
         slewt=max(az_slewt,az_slew1p+el_slew2)   
      endif 

500   continue
      slew_ggao=slewt 
      return   
      end
      

      
      
