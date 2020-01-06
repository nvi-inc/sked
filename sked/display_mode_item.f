!****************************************************************************************
      subroutine display_mode_item(cname,iptr)
      include '../skdrincl/skparm.ftni'
      include 'cat_mode.ftni'
! 2009Apr02 JMGipson. Changed name: icat_mode_freq-->icat_mode_freq_ptr
!                                   icat_mode_rec -->icat_mode_rec_ptr 
! 2009Apr10 JMGipson.  Modified printing to avoid overflow

      character*(*) cname
      integer iptr

      if(iptr .eq. 0) then
        cname=
     >  'Mode name         Frq.Code   BW   Sample  Recorder Code'
      else
        write(cname,'(a16,2x,a8,2x,2(f5.1,2x),a16)')
     >   cat_mode(iptr),     cat_mode_freq(icat_mode_freq_ptr(iptr)),
     >   rcat_mode_bw(iptr), rcat_mode_samp(iptr),
     >   cat_mode_rec(icat_mode_rec_ptr(iptr))
      endif
      return
      end
