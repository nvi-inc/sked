      subroutine addsec2ut(mjd_beg,ut_beg,isec,mjd_end,ut_end)
      implicit none
      double precision ut_beg,ut_end    !begiinning and end of ut time (in seconds)
      integer mjd_end,mjd_beg

      integer isec                      !seconds to add

      ut_end=ut_beg+isec
      mjd_end=mjd_beg
!     if(ut_end .gt. 86400.d0) then
!AEM20120621 bugfix, sometimes this cause unpak to drop scans with 24:00:00 start time
      if(ut_end .ge. 86400.d0) then
         ut_end=ut_end-86400.d0
         mjd_end=mjd_end+1
      endif
      return
      end


