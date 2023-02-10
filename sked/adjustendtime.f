      subroutine AdjustEndTime(isked_start,isked_end ,iMin_sked_len)
      implicit none 
      integer isked_start(5),isked_end (5)
      integer iMin_sked_len     !minimum schedule in seconds 
      
!  history
!   2023-02-08. Changed variable names to make clearer. Modest cleanup. 
!   2004-04-18. First version 

! Local
      integer isked_len 
      integer julda
      integer jdaystart,jdayend
      double precision utstart,utend
      double precision hms2seconds

      double precision dSecPerDay
      parameter (dSecPerDay=86400.d0)
      integer ischedule_length

! if no ending time, set it to 1 day later.
      if(isked_end (1) .eq. 0) then
         call timeadd(isked_start,86400,isked_end )
         return
      endif

      jdaystart=JULDA(1,isked_start(2),isked_start(1)-1900)
      utstart= hms2seconds(isked_start(3),isked_start(4),isked_start(5))

      jdayend  =JULDA(1,isked_end (2),isked_end (1)-1900)
      utend  = hms2seconds(isked_end (3),isked_end (4),isked_end (5))

! Have leeway of 60 seconds.
      isked_len=dble(jdayend-jdaystart)*86400.d0+utend-utstart
      if(isked_len .lt. imin_sked_len) then
         writE(*,"(a,i4,a,i4)") "WARNING: Schedule length ", isked_len,
     &                         " less than minimum ", imin_sked_len
         write(*,*) "Adjusting schedule length to minimum "   
         call timeadd(isked_start,imin_sked_len,isked_end ) 
      endif
      return
      end

