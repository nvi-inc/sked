      subroutine read_cmdline(luscn,luusr,cmdline) 
      implicit none
! passed
      integer luscn,luusr     !Lu's for input and output
      character*(*) cmdline
      write(luscn, '("? ",$)')
       read(luusr,'(a)') cmdline

      return
      end
