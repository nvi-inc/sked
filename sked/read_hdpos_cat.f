      subroutine read_hdpos_cat(ierr)
! Read in the tracks catalog, and save the different assignments.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_mode.ftni'
      include 'cat_stat.ftni'
      include 'cat_rec.ftni'
      include 'cat_track.ftni'
      include 'cat_hdpos.ftni'


! History.
!    2005Nov21 JMGipson.  First version.
! Passed
! Return
      integer ierr

! Local variables

!--------------Start of code-----------------------------------------------
!  1. Check that the catalog exists.
      ierr=0
      if(kcat_hdpos) return    !skip if already read in.
      call open_Cat(hdpos_cat,ierr)
      if(ierr .ne. 0) then
         return
      endif
      close(lucat)
      kcat_hdpos=.true.
      return
      end
