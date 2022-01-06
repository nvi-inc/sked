      integer*4 function itime_to_down(mjdnow,utnow,istat)
      implicit none
      include 'downtime.ftni'
      
      integer mjdnow             !current time
      double precision utnow
      integer istat              !station
! Return the time to the beginning of the next downtime.       
! History
! 2022-01-05 First version.             
! functions
      integer*4 isecdif          !difference in time. 

! local
      integer itemp 
      integer idown  !counter 
            
      itime_to_down=86400    
      do idown=1,num_down
        if(istat .ne. idown_stat(idown)) cycle              !station is not in this idle time 
        itemp=
     &  isecdif(mjd_down_beg(idown),ut_down_beg(idown),mjdnow,utnow)    
        if(itemp .lt. 0) cycle                     !idle time started before the scan began. 
! Idle time started AFTER the current scan.  Find the one that is closest.
        itime_to_down =min(itime_to_down,itemp) 
      end do
      return
      end function 
