      subroutine add_file_to_vex(lsection,lfile,lutmp)
          
! Passed 
      character*(*) lsection
      character*(*) lfile
      integer lutmp

! Local
      logical kexist
      character*1 lkind
      character*256 cbuf 
      lkind="v"

      inquire(file=lfile,exist=kexist)
      if(.not. kexist) then
         write(*,*) "add_file_to_vex: File not found: "//trim(lfile)
         return
      endif

      cbuf=lsection
      call wrt_param_line(cbuf,lutmp,lkind) 

      open(lutmp,file=trim(lfile))
      do while(.true.)
         read(lutmp,'(a)',end=500) cbuf
         call wrt_param_line(cbuf,lutmp,lkind) 
      end do
      
500   continue
      close(lutmp)
      return
      end 
           



     

