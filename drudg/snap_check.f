      subroutine snap_check(lu_out,itime_buf_write_end,
     >   kdata_xfer_prev)
      implicit none
!Includes
      include 'hardware.ftni'           !This contains info only about the recorders.
      include 'drcom.ftni'
      include '../skdrincl/broadband.ftni'

      integer lu_out
      logical kdata_xfer_prev      
      integer itime_buf_write_end 
      character*40 ldum 
        
      if(km6disk) then
         call snap_wait_time(lu_out,itime_buf_write_end)            
         write(ldum,'("mk6=rtime?",i10,";")') idata_mbps(istn)
         call drudg_write(lu_out,ldum)
         write(lu_out,'("checkmk6")') 
       else if((km5disk.or.kk5.or.kflexbuff).and.
     >      .not. (krunning .or. kdata_xfer_prev)) then
         if(kk5) then
           write(lu_out,'("checkk5")')
         else if(kflexbuff) then
           write(lu_out,'("checkfb")')
         else if(km5disk) then          
           write(lu_out,'("checkmk5")')
         endif
       endif 
       return
       end 
