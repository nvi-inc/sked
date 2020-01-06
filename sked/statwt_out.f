      subroutine statwt_out(luout,kall,knumber,lkind)
! write station weights.

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'statwt.ftni'    

!! passed
      integer luout
      logical kall      !list all or just ones set.
      logical knumber   !number them
      character*1 lkind 
!  2012Sep24  JMG.  Modified to output to VEX as well. 
! local
      integer i
 
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$STATWT"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      do i=1,nstatn
        if(statwt(i) .ne. 0 .or. kall) then
          if(knumber) then
            write(cbuf,'(i4," ",a," ",f8.2)') i, cstnna(i),statwt(i)
          else
            write(cbuf,'(a,1x,f8.2)') cstnna(i),statwt(i)
          endif 
          call wrt_param_line(cbuf,luout,lkind) 
        endif
      end do
      return
      end
