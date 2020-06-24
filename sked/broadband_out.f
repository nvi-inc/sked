      subroutine broadband_out(luout,kall,knumber,lkind)
! write station weights.

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'

!! passed
      integer luout
      logical kall      !list all or just ones set.
      logical knumber   !number them
      character*1 lkind 
!  2012Sep24  JMG.  Modified to output to VEX as well. 
!  2015Mar26  JMG. Added idata_mbps and isink_mbps
! local
      integer i
 
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$BROADBAND"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      do i=1,nstatn
        if(bb_bw(i) .ne. 0 .or. kall) then
          if(knumber) then
            write(cbuf,'(i4," ",a," ",f8.2,3(2x,i6,3x))') 
     >     i, cstnna(i),bb_bw(i),idata_mbps(i),isink_mbps(i),ibb_off(i)
          else
            write(cbuf,'(a,1x,f8.2,3(2x,i6,3x))') 
     >        cstnna(i),bb_bw(i),idata_mbps(i),isink_mbps(i),ibb_off(i)
          endif 
          call wrt_param_line(cbuf,luout,lkind) 
        endif
      end do
      return
      end
