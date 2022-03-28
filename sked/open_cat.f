      subroutine open_cat(cat_name,ierr)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
! Check to see if a catalog is there.
! If it is open it.
! If not, return with error message
! History
! 2005Nov21  JMGipson
! 2008Jun11  JMGipson.  Only write out if verbose is on.
!
      character*(*) cat_name
      integer ierr

! local
      ierr=0
      open(lucat,file=cat_name,status='old',iostat=ierr)

      if (ierr.ne.0) then
        write(luscn,"('open_cat: Error ',i5,' opening catalog ',a)")
     >    ierr,trim(cat_name)
        close(lucat)     
        return
      endif
      if(iverbose_level.ge.5)
     >   write(luscn,'("Opening catalog: ",a)') trim(cat_name)
      return
      end





