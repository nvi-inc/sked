      subroutine get_cat_version(cat_name,lversion,ierr)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
! Check to see if a catalog is there.
! If it is open it and read the first 5 line lines to see if we can find the version.
! If not, return with error message.
! We look for a line that looks like:
! * VERSION   24OCT12      
! The string 24OCT12 is the version.
! Histry
! 2012Oct10  JMGipson first version.
! 2025-03-19 JMG.  Fixed bug: Was trimming off the first two characters of the version 
!
      character*(*) cat_name
      integer ierr       
! Returned
      character*(*) lversion
! integer
      integer ifirst_non_blank

! local
      character*80 ldum_in,ldum_cap
      character*10 ltmp
      integer ind 
      integer i 
      logical kexist

! Start of code.
      lversion="unknown"    
      ierr=0
      inquire(file=cat_name,exist=kexist)
      if(.not. kexist) then
        write(luscn,"('get_cat_version: Catalog ',a,' does not exist')")
     >   trim(cat_name)      
        cat_name=trim(cat_name)//" does not exist"
        return
      endif 
      
      lversion="unknown"
      open(lucat,file=cat_name,status='old',iostat=ierr)  
      if (ierr.ne.0) then
         write(*,*) "Catalog not found!", trim(cat_name)   
         goto 100 
      endif   

! Search the first 10 lines for the VERSION
      do i=1,10      
        read(lucat,'(a)',end=100,err=100) ldum_in   
        ldum_cap=ldum_in
        call capitalizE(ldum_cap)
           
        ind=index(ldum_cap,"VERSION") 
        if(ind .ne. 0) then
           ind=ind+7        
           ldum_in(1:ind)=" "
           ind=ifirst_non_blank(ldum_in)            
           if(ldum_in(ind:) .ne. " ") then         
              read(ldum_in(ind:),'(a)',err=100,end=100) lversion
           endif
           goto 100
                 
        endif 
      end do 

100   continue     
      close(lucat)
      return
      end 









           
 

        
 






