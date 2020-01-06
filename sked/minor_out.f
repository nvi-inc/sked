      subroutine minor_out(luout,kall,kheader,lkind)
! output the minor modes.
!  2004Mar08  JMGipson First version
!  2007Feb10  JMGipson. Modified so that can print 65(!) Values for statwt
!  2010Mar10  JMGipson. Removed obsolete srcfloor and TapeWaste
!  2010Apr13  JMGipson. Changed because of StatWt changes. 
!  2010Oct04  JMGipson. Fixed bug in format statement for SrcWt
!  2012Sep24  JMG.  Modified to output to VEX as well. 
!  2015Nov13  JMGipson. Added kCovar for covariance optimization. 

!   COMMON BLOCKS USED
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/constants.ftni'

      include 'skcom.ftni'
      include 'minor.ftni'

! input
      integer luout  !where to output the numbers.
      logical kall
      logical kheader
      character*1 lkind 

! functions
      character*3 lyesno
      character*5 labsrel

! local  
      integer i
      characteR*6 lEvnDist(4)
      data lEvnDist/"NONE","UPTIME","SQRT","EVEN"/

      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$MINOR"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 


      if(kall) then
        if(kheader)  then
           write(cbuf,'(a)')
     >    "Option       On/Off  Norm      Wt   Aux_Parm"
        endif 
          
        write(cbuf,100) "Astro         ",
     >    lyesno(kAstro),labsrel(kAstronorm),  rAstroWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "BegScan       ",
     >  lyesno(kBegScan),labsrel(kBegScannorm),rBegScanWt
        call wrt_param_line(cbuf,luout,lkind) 
   
        write(cbuf,100) "Covar         ",
     >  lyesno(kCovar),labsrel(kCovarNorm),rCovarWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "EndScan       ",
     >   lyesno(kEndScan),labsrel(kEndScannorm), rEndScanWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "LowDec        ",
     >   lyesno(kLowDec),labsrel(kLowDecnorm),   rLowDecWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "NumLoEl       ",
     >  lyesno(kNumLoEl),labsrel(kNumLoElnorm),rNumLoElWt,rLoEl*rad2deg
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "NumRiseSet    ",
     >   lyesno(kNumRiseSet),labsrel(kNumRiseSetNorm), rNumRiseSetWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "NumObs        ",
     >   lyesno(kNumObs),labsrel(kNumObsnorm), rNumObsWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "SkyCov        ",
     >   lyesno(kSkyCov),labsrel(kSkyCovnorm),rSkyCovWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,110) "SrcEvn        ",
     >   lyesno(kSrcEvn),labsrel(kSrcEvnnorm),rSrcEvnWt, 
     >      lEvnDist(iSrcEvnMode+1)
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "SrcWt         ",
     >   lyesno(kSrcWt),labsrel(kSrcWtnorm), rSrcWtWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,110) "StatEvn       ",
     >  lyesno(kStatEvn),labsrel(kStatEvnnorm),
     >     rStatEvnWt, lEvnDist(iStatEvnMode+1)
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "StatIdle      ",
     >   lyesno(kStatIdle),labsrel(kStatIdlenorm),rStatIdleWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "StatWt        ",
     >    lyesno(kStatWt),labsrel(kStatWtnorm), rStatWtWt
        call wrt_param_line(cbuf,luout,lkind) 

        write(cbuf,100) "TimeVar       ",
     >   lyesno(kTimeVar),labsrel(kTimeVarnorm), rTimeVarWt
         call wrt_param_line(cbuf,luout,lkind) 
       else
         if(kheader) then
           write(cbuf,'(a)')
     >    "Option         Norm      Wt   Aux_Parm"
         endif

        if(kAstro) then
          write(cbuf,200)"Astro         ",
     >      labsrel(kAstronorm),rAstroWt
           call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kBegScan) then
          write(cbuf,200) "BegScan       ",
     >      labsrel(kBegScannorm),rBegScanWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif 

        if(kCovar) then 
          write(cbuf,200) "Covar         ",
     >       labsrel(kCovarNorm),rCovarWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif        

        if(kEndScan) then
          write(cbuf,200)"EndScan       ",
     >      labsrel(kEndScannorm),rEndScanWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kLowDec) then
          write(cbuf,200)"LowDec        ",
     >      labsrel(kLowDecnorm),rLowDecWt  
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kNumLoEl) then
          write(cbuf,200) "NumLoEl       ",
     >      labsrel(kNumLoElnorm),rNumLoElWt,rLoEl*rad2deg
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kNumRiseSet) then 
         write(cbuf,200) "NumRiseSet    ",
     >      labsrel(kNumRiseSetNorm),rNumRiseSetWt
          call wrt_param_line(cbuf,luout,lkind)      
        endif 
        if(kNumObs) then
          write(cbuf,200) "NumObs        ",
     >      labsrel(kNumObsnorm),rNumObsWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kSkyCov) then
          write(cbuf,200) "SkyCov        ",
     >      labsrel(kSkyCovnorm),rSkyCovWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif
 
        if(kSrcEvn) then
          write(cbuf,210) "SrcEvn        ",
     >      labsrel(kSrcEvnnorm),rSrcEvnWt, lEvnDist(iSrcEvnMode+1)         
          call wrt_param_line(cbuf,luout,lkind) 
         endif
        if(kSrcwt) then
          write(cbuf,200) "SrcWt         ",
     >      labsrel(kSrcWtnorm), rSrcWtWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif
   
        if(kStatEvn) then 
          write(cbuf,210) "StatEvn       ",
     >      labsrel(kStatEvnnorm),rStatEvnWt,lEvnDist(iStatEvnMode+1)
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kStatIdle) then
          write(cbuf,200)"StatIdle      ",
     >      labsrel(kStatIdlenorm),rStatIdleWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif 
        if(kStatWt) then
          write(cbuf,200) "StatWt        ",
     >      labsrel(kStatWtnorm),rStatWtWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif
        if(ktimeVar) then
          write(cbuf,200) "TimeVar      ",
     >      labsrel(kTimeVarnorm),rTimeVarWt
          call wrt_param_line(cbuf,luout,lkind) 
        endif
      endif 

      return
!            Name   On/Off  Mode   Wt
100   format(a14,2x,a3,2x,  a5, 2(1x,f11.2))
105   format(a14,2x,a3,2x,  a5, 1x,f11.2,1x,i4,f11.2)
110   format(a14,2x,a3,2x,  a5, 1x,f11.2,1x,a)
115   format(a14,2x,a3,2x,  a5, 1x,f11.2,11(1x,5L1))

! Only put out things that are on.
200   format(a14,      2x,  a5, 2(1x,f11.2))
205   format(a14,      2x,  a5, 1x,f11.2,1x,i4,1x,f11.2)
210   format(a14,      2x,  a5, 1x,f11.2,1x,a)
215   format(a14,      2x,  a5, 1x,f11.2,11(1x,5L1))

      end
!*******************************************************************************
      character*3 function lyesno(kon)
      logical kon
      if(kon) then
         lyesno="Yes"
      else
         lyesno="No"
      endif
      return
      end
!******************************************************************************
      character*5 function labsrel(knorm)
      logical knorm
      if(knorm) then
         labsrel="Rel"
      else
         labsrel="Abs"
      endif
      return
      end

