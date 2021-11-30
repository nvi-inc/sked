      subroutine wrt_param_line(cline,lutmp,ciin)
! Write out a line to sked or vex file. 
      implicit none
! History
!  2012Nov08. JMG. Changed from:
!     write(*, '(a)') to write(lutmp,'(a)') if called with option 'd'
! functions
      integer ptr_ch
      integer trimlen
! passed
      character*1 ciin          !either 's' or 'v'
      character*(*) cline       !line to print.
      integer lutmp             !lu for sked file.

      if (ciin.eq.'s') then
        write(lutmp,'(a)') cline(1:trimlen(cline))
      else if(ciin .eq. 'd') then
        write(lutmp,'(a)') cline(1:trimlen(cline))
      else if (ciin.eq.'v') then
        call null_term(cline)
        call fcreate_literal(ptr_ch(cline))
!        write(*,*) "VEX: ", trim(cline) 
      endif
      return
      end
