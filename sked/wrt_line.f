      subroutine wrt_line(cline,lutmp,ctype)
      implicit none
! passed
      character*1 ctype         !either 's' or 'v'
      character*(*) cline       !line to print.
      integer lutmp             !lu for sked file.
! if ctype="v" then 
!    write out to  vex file.
! else
!    write to lump
! endif 
 
! functions
      integer ptr_ch

      if(ctype .eq. "v") then
        call null_term(cline)
        call fcreate_literal(ptr_ch(cline))
      else   
        write(lutmp,'(a)') Trim(cline)
      endif 
        
      return
      end
