      subroutine print_slew_info_warning(lu,lprefix,islew_info,istat)
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

      include 'skcom.ftni'
!      include 'maistator.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
!      include '../skdrincl/skobs.ftni' 
! Passed
      integer lu
      character*(*) lprefix
      integer islew_info
      integer istat 


! local
      character*80 lform
       
      lform=" " 
 
      if(islew_info .eq. 1) then            
              lform=
     >"(A,'Source ',A8,' near cable wrap limits at ',A)"   
      else if(islew_info .eq. 2) then
              lform=   
     >"(A,'Antenna slews 180 degrees for ',A8,' at ',A)"   
      else if(islew_info .eq. 3) then 
              lform=
     >"(A,'Indeterminate direction for ',A8,' at ',A)"    
       else if(islew_info .eq. -4) then
              lform=
     >"(A,'Source ', A8,' is not up ',A)"   
       else if(islew_info .eq. -5) then
              lform=
     >"(A,'Source ', A8,' is not continous at ',A)"   
       else if(islew_info .eq. 6 ) then
              lform=
     >"(A,'Source ', A8,' near upper wrap edge of CCW at ',A)"    
       else if( islew_info .eq. 7) then
              lform=     
     >"(A,'Source ', a8, ' near lower wrap edge of CW ',A)"      
       endif 

       if(lform .eq. " ") then           
           write(*,*) 'Unknown slew_info: ', islew_info 
        else if(islew_info .eq. 6 .or. islew_info .eq. 7) then
          write(lu,lform) lprefix,cSORNA(NSORcur(istat)),
     >   cstnna(istat)//"(="//cpocod(istat)//")"         
       else
              write(lu,lform) lprefix,cSORNA(NSORcur(istat)),
     >   cstnna(istat)//"(="//cpocod(istat)//")"     
       endif 
       return
       end
