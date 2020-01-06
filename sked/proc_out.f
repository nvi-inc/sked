      subroutine proc_out(luout,lkind)
! Write out the catalogs in this schedule.
!       include '../skdrincl/skparm.ftni'
!       include 'skcom.ftni'
       include 'proc.ftni'

! 2013Sep18  JMG. Was not putting out Position.cat info
! 2014Jan16  JMG. Some of the 'luout' were 'lutmp'. This resulted in writing to a non-open file. 

       
! Passed
      integer luout
      character*1 lkind
      character*120 cbuf    
  
     
! local
      integer i
    
      if(lkind .eq. "v" .or. lkind .eq. "s" .and.
     >   num_proc_lines .gt. 0) then 
        cbuf="$PROCS" 
        write(*,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
        do i=1,num_proc_lines
          call wrt_param_line(cproc_lines(i),luout,lkind)
        end do
      endif
      return
      end 

 
