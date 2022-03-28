      SUBROUTINE delete_temp_files 
      use Obs_Scan_Counters
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'

! History
!   2022-01-10  JMGipson. Added clean_up_obs_scan_counters     
      
      call clean_up_obs_scan_counters()

      if(kkeep_log) then
         write(*,*) "Keeping ",trim(clgfil)
      else  
        call delete_file(clgfil,lutmp)    
      endif
      call delete_file(csofil,lutmp)
      call delete_file(cstfil,lutmp)
      call delete_file(cfrfil,lutmp)
      call delete_file(copfil,lutmp)
      call delete_file(cflfil,lutmp)
      call delete_file(chdfil,lutmp)
      call delete_file(ctmfil,lutmp)
      call delete_file(ctmfi2,lutmp)
      call delete_file(cprfil,lutmp)
      call delete_file(cplfil,lutmp)
      call delete_file(cskselect_file,lutmp)
      call delete_file(cskcontrol_file,lutmp)
      call delete_file(cskcat_file,lutmp)
      END
