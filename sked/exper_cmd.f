      subroutine exper_cmd(cmdline)

      implicit none
! Set, display experiment code

! Common blocks
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
    
! History
! 2021-05-07  JMGipson use trim(cexper) on output
! 2019-03-14  JMGipson first version


! passed
      character*(*) cmdline
!
! Stuff dealing with finding which "broadband command" to do.
      integer icmdlen 
      character*12 cmd
   
      icmdlen=len_trim(cmdline)
  
      if(icmdlen .eq. 0) then
         write(luscn,'("Session name: ", a)') trim(cexper)
      else
         cexper=cmdline
         write(luscn,'("Setting session name to: ", a)')  trim(cexper)
      endif
      return

      end

