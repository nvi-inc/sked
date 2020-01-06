      subroutine read_cmdline(luscn,luusr,cmdline) 
      implicit none
! passed
      integer luscn,luusr     !Lu's for input and output
      character*(*) cmdline

! function   
      integer*4, external :: readline
! local 
      integer*4 iadr
      integer ind
! History
! 2008Mar14  JMG   read a line from standard input or readline routine
! 2014Apr03  JMGipson.  replace with spaces when we find char(0) 


#ifdef READ_LINE     
      CALL SET_SIGNAL_CTRLC ( 1 )  
! --- Get the address of the line that user typed
      IADR = READLINE ( %REF('? '//CHAR(0)) )
! --- Copy the line to the destimation
      CALL STRNCPY ( %REF(CMDLINE), %VAL(IADR), %VAL(LEN(CMDLINE)) )
      ind=index(cmdline,char(0))            
      if(ind .ne. 0) cmdline(ind:)=" "
! --- Include the line into the history buffer
      CALL ADD_HISTORY ( %VAL(IADR) )
      return
#else
      write(luscn, '("? ",$)')
      read(luusr,'(a)') cmdline
#endif

      return
      end
