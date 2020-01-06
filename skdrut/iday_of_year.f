      integer function iday_of_year(iyear,imonth,iday)
! Return the day of year.
      integer iyear,imonth,iday
! 2019May15 JMGipson. First version
! 2019Sep04 JMGipson. Second version.  Take into account that we only add leapday after February

! local

      integer imon_offset(12)
      data imon_offset/0,31,59,90,120,151,181,212,243,273,304,334/
      integer ileap_day

      if(mod(iyear,400) .eq. 0) then
         ileap_day=1
      else if(mod(iyear,100) .eq.0) then
         ileap_day=0
      else if(mod(iyear,4) .eq. 0) then
         ileap_day=1
      else 
         ileap_day=0
      endif
        
      iday_of_year=imon_offset(imonth)+iday
      if(imonth .gt. 2) then
        iday_of_year=iday_of_year+ileap_day
      endif 
         
      return
      end function 

        
