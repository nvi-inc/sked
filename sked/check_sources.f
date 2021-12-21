      subroutine check_sources(ierr)
      integer ierr
 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
!      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'      
    
      integer isrc  !counter over sources
      integer ib    !counter over bands 
      
      integer ierr=0       
! Check the sources to make sure we have flux information.       
       do isrc=1,nceles
         do ib=1,2 
           if(nflux(ib,isrc) .le. 0) then 
               write(lu,
     >      "('check_sources: No flux information for ', 
     >        a,'-band for source ',a)")   cband(ib),csorna(isrc)
              ierr=-1 
            endif
          end do
      end do 
      return
      end 
     
  


