      subroutine AdjustEndTime(itimestart,itimeend,iMinDifSec)
      integer itimestart(5),itimeend(5)
      integer iMinDifSec

      integer julda
      integer jdaystart,jdayend
      double precision utstart,utend
      double precision hms2seconds

      double precision dSecPerDay
      parameter (dSecPerDay=86400.d0)

! if no ending time, set it to 1 day later.
      if(itimeend(1) .eq. 0) then
         call timeadd(itimestart,86400,itimeend)
         return
      endif

      jdaystart=JULDA(1,itimestart(2),itimestart(1)-1900)
      utstart= hms2seconds(itimestart(3),itimestart(4),itimestart(5))

      jdayend  =JULDA(1,itimeend(2),itimeend(1)-1900)
      utend  = hms2seconds(itimeend(3),itimeend(4),itimeend(5))

! Have leeway of 60 seconds.
      if(dble(jdayend-jdaystart)*86400.d0+(utend-utstart)+60. .lt.
     >   iMinDifSEc) then
           call timeadd(itimestart,86400,itimeend)
      endif
      return
      end

