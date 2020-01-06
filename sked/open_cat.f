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
! function
      integer trimlen

! local
      integer nch
      ierr=0
      open(lucat,file=cat_name,status='old',iostat=ierr)
      nch = trimlen(cat_name)
      if (ierr.ne.0) then
        write(luscn,"('open_cat: Error ',i5,' opening catalog ',a)")
     >    ierr,cat_name(1:nch)
        close(lucat)     
        return
      endif
      if(kverbose)
     >   write(luscn,'("Opening catalog: ",a)') cat_name(1:nch)
      return
      end





