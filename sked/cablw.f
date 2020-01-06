      real*4 FUNCTION CABLW(ISTN,az_cur,cwrap_cur,az_new,cwrap_new)
C
C  CABLW returns the azimuth difference between the NOW and the
C              NEW source positions, taking into account cable wraps.
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  INPUT VARIABLES:
      integer istn              !index for stations in array. 
      character*2 cwrap_cur     !current cable wrap. 
      character*2 cwrap_new     !desired cable wrap. Options are " " go fastest. "c" clockwise, "w" counterclockwise.                     
      real*4 az_cur              !original position
      real*4 az_new              !new position

C   COMMON BLOCKS USED
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'

!  functions
      real*4 azwrap
C
C     CALLING SUBROUTINES: SLEWT
C
C  LOCAL VARIABLES    
      real az_new1,az_new2 ! trial values for VLBA and Noto algorithms

      real az_cur0,az_new0 ! non-wrapped values
      logical kq31,kq24 ! for Noto logic
      character*2 cwrap_new_orig 
C
C HISTORY
C    LAST MODIFIED: created 780424
C    880315 NRV DE-COMPC'D
C    930225 nrv implicit none
C    930702 nrv Put back in check for pseudo-azel axis types
C    940513 nrv Special section added for VLBA slewing algorithm
C               which says: don't go through south unless necessary.
C               Invoke this algorithm with a "V" in the cable wrap
C               variable cwrap_new.
C 961016 nrv Add special section for Noto slewing algorithm. 1) If
C            going from one quadrant to the opposite one, must go
C            through south. 2) If going to az=270 to 290, antenna
C            thinks it can get there in CCW, but this range is
C            "prohibited" and operator must catch it and send it the
C            right way. So allow extra time for antenna to slew to
C            the wrong limit and then go around 360 degrees to the
C            other wrap.
C 970114 nrv Change amin0,amax0 to amin1,amax1 (found by Simone Magri, ATNF)

! 2015Nov13 JMGipson. Completely re-written and simplified. 
!
!     1. We skip out of this routine if the axis type is not az-el.
!        The first thing is to adjust the azimuths to put them into the
!        range as found in the data base.  Then if we are currently on
!        the outer overlapped portion, add another 2pi.
!    2.  Fixed a  small issue if we are near the border of W and neutral 
! 2017Feb14.  Added declaration of azwrap. Also fixed problem with going past line 72


! Check to see if AzEl.  If not, then quick exit. 
      IF (IAXIS(ISTN).EQ.3.or.iaxis(istn).eq.6.or.iaxis(istn).eq.7) 
     &      GOTO 100
C     If we don't have an azel mount, delta-az=0 and return.
      CABLW = 0.D0
      cwrap_new=" " 
      return 

! Have an AZ-EL antenna.  
100   continue
      cwrap_new_orig=cwrap_new
  
      az_cur0 = az_cur
      az_new0 = az_new
 
      if(.false.) then 
! Check to see if in wrap region.
      if(az_cur .gt. stnlim(1,1,istn) .and.
     &   az_cur .lt. stnlim(2,1,istn))  then
!By the azimuth we think we are in the wrap region...
!But the wrap says we are neutral. This means we wandered into this region from neutral.
!Adjust the az according (but don't change the wrap!  
        if(cwrap_cur .eq. '- ' .and.
     &    az_cur+twopi .lt. stnlim(2,1,istn)) then            
          az_cur=az_cur+twopi         
        endif
      endif 
      endif 

      az_cur=azwrap(az_cur,cwrap_cur,stnlim(1,1,istn))

!      IF (az_cur.LT.STNLIM(1,1,ISTN)) az_cur=az_cur+TwoPi
      if (az_new.lt.stnlim(1,1,istn)) az_new=az_new+TwoPi 

!      if(cwrap_cur .eq. "-") then


     
      if(cwrap_cur .eq.'C ' .and. 
     &   az_cur+twopi .lt. stnlim(2,1,istn)) az_cur=az_cur+TwoPi
      


      if(cstnna(istn) .eq. "SVETLOE") then
!          write(*,'("Input ",2f8.1)')  az_cur0*rad2deg, az_cur*rad2deg
      endif 

      cablw= ABS(az_cur-az_new)
      select case(cwrap_new)
        case(" ")        !go fastest
!  See if in overlap region. If so, then determine which wrap is least. 
          if(az_new+twopi .lt. stnlim(2,1,istn)) then 
             if(abs(az_cur-(az_new+twopi)) .lt. cablw) then
                az_new=az_new+twopi
                cwrap_new="C"
                cablw=az_cur-az_new
            else
                cwrap_new="W"
            endif
          else
! Not in overlap. Only one way to go.  
            cwrap_new="-"
          endif 
! Special logic for NOTO. 
          if(cstnna(istn).eq.'Noto' .or. cstnna(istn).eq.'NOTO') then
! Moving from quadrant 2 to 4. 
            kq24 = az_cur0.gt.0.5*pi.and.az_cur0.le.  pi.and.
     &             az_new0.gt.1.5*pi.and.az_new0.le.TwoPi
! Movign from quadrant 3 to 1
            kq31 = az_cur0.gt.    pi.and.az_cur0.le.1.5*pi.and.
     &            az_new0.gt.0.0   .and.az_new0.le.0.5*pi
            if(kq31.or.kq24) then
              az_new2 = -1.0
              az_new1 = az_new        
              if(az_new+TwoPi.lt. stnlim(2,1,istn)) then
                az_new2 = az_new1+TwoPi
              else if(az_new-TwoPi.gt. stnlim(1,1,istn)) then 
                az_new2 = az_new1-TwoPi
              endif 
              if (kq31) az_new=amin1(az_new1,az_new2)
              if (kq24) az_new=amax1(az_new1,az_new2)
              if (az_new .gt. 3.5*pi)  cwrap_new="C"
              if (az_new .lt. 2.5*pi)  cwrap_new="W"
           endif 
          endif
        case("W")   !Want to end up on counterclockwise wrap (if possible)
          if(az_new+twopi .lt. stnlim(2,1,istn)) then
            continue         !we are on the counterclockwisewrap. Don't need to update wrap. 
          else
! Acutally in neutral. Update wrap. 
            cwrap_new="-"
          endif
        case("C")
          if(az_new+twopi .lt. stnlim(2,1,istn)) then  
! We can go to the "C" region. Go there. Don't need to update the wrap. 
            az_new=az_new+twopi
          else
! Are actuallly on the neutral region. Update the wrap. 
            cwrap_new="-" 
          endif 
        case("V") 
          az_new1 = az_new ! initial trial value
          az_new2 = -1.0
         if (az_new+TwoPi .lt. stnlim(2,1,istn)) az_new2 = az_new1+TwoPi
         if (az_new-TwoPi .gt. stnlim(1,1,istn)) az_new2 = az_new1-TwoPi
         if (az_new2 .gt. 0.0) then
           if (az_cur .lt. 3.0*pi) then
             az_new = amin1(az_new1,az_new2)
           else
             az_new = amax1(az_new1,az_new2)
           endif
         endif     
         cwrap_new=" " 
         if (az_new .gt. 3.5*pi) cwrap_new="C "  
         if (az_new .lt. 2.5*pi) cwrap_new="W " 
      end select  
! Calculate this again in case az_new has changed. 
      cablw= ABS(az_cur-az_new)
! Debug stuff
      if(.false.) then
      write(*,'(a8,1x,2f8.1,3(1x," | ",a2)," | ")') cstnna(istn), 
     &    az_cur*rad2deg,az_new*rad2deg, cwrap_cur, cwrap_new, cwrap_new_orig 
      endif

C
      END
