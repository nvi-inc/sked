      subroutine major_out(luout,lkind)
! 2008May22  Moved many parameters from $PARAM to $MAJOR
! 2009Mar03  Rerranged output, and MinSunDist
! 2009Oct12  JMG.  Allowed larger subnet. Upto 80 stations!
! 2011Apr25  Added MaxAngle
! 2012Nov08  JMG. Changed call to wrt_param_line 
! 2013Sep13  JMG. Made rMinAngle, rMaxAngle and rMinSunAngle real parameters.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'major.ftni'
     
! Passed
      integer luout
      character*1 lkind 
   
  
! function 
      integer ptr_ch
      character*3 lyesno
! local
      integer i
    
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$MAJOR"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 
     
! Subroutine to write out the major modes
      if(nsubst .gt. 80 ) then
          write(*,*) "Recompile major_out!"
      endif
      write(cbuf,'(a,80A2)') "Subnet ",
     >  (cpocod(isubst(i)),i=1,nsubst) 
      call wrt_param_line(cbuf,luout,lkind) 
      
      write(cbuf,'(a,a6)')  "SkyCov          ", lyesno(kOptBySky)
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(A,a6)')  "AllBlGood       ",lyesno(kallblgood)
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f6.2)')  "MaxAngle       ", rMaxAngle
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f6.2)')  "MinAngle       ", rMinAngle
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,i6)')  "MinBetween     ", iMinBetween/60
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f6.2)')  "MinSunDist     ", rSunMinAngle
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,i6)')  "MaxSlewTime    ", iMaxSlewTime
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f9.2)')"TimeWindow     ", rcovar_win
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a, i6)') "MinSubNetSize  ",MinSubNetSize
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a, i6)') "NumSubNet      ",NumSubNet
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,i6)')  "Best           ",nint(rBestPerCent*100.)
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(A,a6)')  "FillIn         ",lyesno(kfillin)
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(A,i6)')  "FillMinSub     ",ifillmin
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(A,i6)')  "FillMinTime    ",ifilltime
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(A,i6)')  "FillBest       ",ifillbest
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f8.1)')"Add_ps         ",radd_noise
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(a,f8.1)')"El_noise       ",rel_noise
      call wrt_param_line(cbuf,luout,lkind)

      write(cbuf,'(a,a6)')  "SNRWts         ",lyesno(kSNRwts)
      call wrt_param_line(cbuf,luout,lkind)
      write(cbuf,'(A,a6)')  "SplitTwins      ",lyesno(ksplittwins)
      call wrt_param_line(cbuf,luout,lkind)
   

      return
      end
