      subroutine find_best_scan(TimeFree,TimeLast,TimeExpEnd,
     >  isrcvec,NSrcUse,
     >  iStnAll,NumAll, iStnVec,NstnUse,  NumTst,kfillobs,ibest)

      use Obs_Scan_Counters
      implicit none 
! subroutine to find the best scan in some set.
! Include files
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'minor_score.ftni'
      include 'major.ftni'
      include 'covar.ftni'
      include 'astro.ftni'
      include 'srcwt.ftni'
      include 'statwt.ftni'

! On entry
      double precision timeFree(*)  !Time station becomes free (in days)
      double precision TimeLast      !time last station becomes free
      double precision TimeExpEnd    !end of experiment
      integer isrcvec(*)        !vector of possible sources
      integer NsrcUse           !Number of sources
      integer iStnAll(*)        !vector for all stations
      integer NumAll            !Number of elements.
      integer iStnVec(*)        !vector of stations
      integer NstnUse           !numober of stations
      integer NumTst            !number to test
      logical kfillobs          !kfillin mode?

! On exit
      integer ibest     !best scan

! History
! 2010Sep16. Removed rewind calculation--not used since gone to tape. 
! 2019Mar16. Was calling dsecdif with UT and MJD reversed. Fixed 
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  
! 2019Jun24 JMG. Fixed bug in calculating Astro. Previously checked if %Obs > max_target, or %OB <min_target, but not if within target.
! 2019Sep09 JMG. Changed scoring of StatIdle.  Still does not work as expected. 


! functions
      logical kstatup
      real speed
      double precision dsecdif

! Local variables
      real az,el,ha,x30,y30,x85,y85,dec   !used in finding position & declination.
      logical kup

      integer ierr
      integer itst,isub         !loop counters
      integer i,j
      integer itrial            !index into trial scans.
      character*18 lreason

      integer isrc              !source
      integer istn              !station
      integer istn1             !first station
      integer itemp
      integer ift               !feet on tape
      double precision tapetime
      double precision dtemp
      double precision scan_wt   ! wt of this scan=(obs this scan)/(nstn*(nstn+1)/2)

      integer NumObsThisScan                     !number of obs this scan

      Double precision AstDel(Max_sor)       !Deviation of source from range in astrometric mode.
      Double precision SrcWant(max_sor)      !ideal distribtuion of sources
      Double Precision SrcDel(Max_Sor)       !deviation from ideal

      Double precision StatWant(max_stn)     !Ideal station distribution
      Double Precision StatDel(Max_stn)      !Deviation of station from desired 
     
      Double Precision rAvg,rMin,rMax,rStd
      Double precision TotObsPred(Max_Sor)


      double precision UtDelTime(Max_stn)    !Difference between previous end and current start

      double precision UtSum,UtSqSum         !used to compute variance
      double precision del_ut
      integer num_stat

      integer irelTimeEnd(NumTst)            !differene between begin and end time

      integer mjd_end(max_stn)               !end of scan
      double precision ut_end(max_stn)
      double precision utLast                !time when last station becomes free (in seconds including day!)

      logical ktoolate(NumTst)               !scan ends too late
      logical ktoobig(NumTst)                !overweighted obs
      logical ktoolong(NumTst)

      double precision Score, ScoreMax
      double precision dSecPerDay

      integer iplus,iminus              !1,-1

      dSecPerDay=86400.d0   
    
      TotObsPred = 0.d0
  
! Computer various things having to do with trying to even up # obs on sources, stations.
! Only do after we have done a certain number of obs to prime the pump.

      AstDel=0.d0             !these measure difference between what we have and what we want
      SrcDel=0.d0
      StatDel=0.d0
      if(nobs .lt. 4) then
        continue 
      else

! Compute for source Even or station even.
        if((kSrcEvn  .and. iSrcEvnMode .ne. 0) .or.
     >    (kStatEvn .and. iStatEvnMode .ne. 0) .or.
     >    kastro) then
          call tsincer(TimeLast,iSrcVec,NSrcUse,iStnAll,NumAll) ! calculate time since rise
!
         endif

! compute weighting vector for astrometric mode.s
 
        if(kastro) then
          do i=1,NSrcUse
            isrc=isrcvec(i)       
            if(kastro_src(i)) then             
              dtemp=dble(NumObsSource(isrc))/dble(NumObs)
               if(dtemp .lt. rmin_astro(isrc)) then
!               Astro(isrc)=rmin_astro(isrc)
                AstDel(Isrc)=rmin_astro(isrc)-dtemp  !positive-->prefer 
              else if(dtemp .gt. rmax_astro(isrc)) then
! soft constraint.  
                AstDel(isrc)=rmax_astro(isrc)-dtemp  !negative-->don't like
              else
                AstDel(isrc)=0.d0 
              endif 
            endif
          end do
        endif

! compute weigthing vector for SrcEvn Mode.
        if(kSrcEvn .and. isrcEvnMode .ne. 0) then
          call MakeSrcObsTarget(iSrcVec,NSrcUse,iStnAll,NumAll,
     >      SrcWant,iSrcEvnMode)
! SrcEvn is difference between % we want and % we have.
           SrcDel =SrcWant - dble(NumObsSource)/dble(NumObs)

!           TotObsPred=dble(NumObs)*
!     >          (TimeExpEnd-TimeExpBeg)/(TimeLast-TimeExpBeg)
        endif

! WRITE OUT DEBUGGIN INFO.
        if(.false.) then
          do i=1,NsrcUse,20
            itemp=min(i+19,NsrcUse)
            write(*,'("WNT  ",i4,20f6.1)')
     >       i, (SrcWant(iSrcVec(isrc))*100.d0,isrc=i,itemp)
          end do

          do i=1,NsrcUse,20
            itemp=min(i+19,NsrcUse)
            write(*,'("OBS  ",i4,20i6)')
     >      i,(NumObsSource(iSrcVec(isrc)),isrc=i,itemp)
           end do

           do i=1,NsrcUse,20
             itemp=min(i+19,NsrcUse)
             write(*,'("DEL_SRC  ",i4,20f6.1)')
     >         i, (SrcDel(iSrcVec(isrc))*100.d0,isrc=i,itemp)
           end do
           call ComputeStatsDoubleXref(iSrcVec,NsrcUse,SrcDel,
     >         rAvg,rMin,rMax,rStd)

           write(*,'("Source (nextc): Avg Max Min STD Obs",4f8.2,2i8)')
     >     ravg*100.d0,rmax*100.d0,rmin*100.d0,rStd*100.d0,
     >     NumObs, nint(TotObsPred)
        endif

!  Compute weigthing for StatEvn mode.
        if(kStatEvn .and. iStatEvnMode .ne. 0) then
          call MakeStatObsTarget(iSrcVec,NsrcUse,iStnAll,NumAll,
     >      StatWant,iStatEvnMode)
! is difference between % we want and % we have.
           StatDel=StatWant- Dble(NumObsStat)/Dble(NumObs)
        endif

! Write out debugging info.
        if(.false.) then
           write(*,*) " "
           do i=1,NstnUse,20
             itemp=min(i+19,NstnUse)
             write(*,'("Del_STAT  ",i4,20f6.1)') i,
     >       (StatDel(iStnVec(istn))*100.d0, istn=i,itemp)
           end do
           call ComputeStatsDoubleXref(iStnAll,NumAll,StatDel,
     >      rAvg,rMin,rMax,rStd)
        endif

        if(.false.) then
          write(*,'(20f6.1)') StatWant(1:NstnUse)*100.
          Write(*,'(20f6.1)') 0.5D0*Dble(NumObsStat(1:NstnUse))
     >             /Dble(NumObs)*100.d0

          write(*,'("Stat:  %>0.3%, Avg Max Min STD Obs",4f8.2,2i8)')
     >     ravg*100.d0,rmax*100.d0,rmin*100.d0,rStd*100.d0
        endif
      endif

! Cycle over all test scans, compute score for each minor option that is on.
100   continue

! Pre loop initialization
      ktoolate=.false.
      ktoobig=.false.
      ktooLong=.false.
      iRelTimeEnd=0

      TstAstro  =0d0
      TstBegScan=0d0
      TstCovar=0.d0 
      TstEndScan=0d0
      TstNumLoel=0d0
      TstNumObs =0d0
      TstNumRiseSet=0d0
      TstSkyCov =0d0
      TstSrcEvn =0d0
      TstStatEvn=0d0
      TstStatIdle=0d0
      TstStatWt =0d0
      tstSrcWt  =0d0
      TstTimeVar=0d0
      TstDurScan=0d0

      UtDelTime=0.
! AEM 20050221 init vars

      mjd_end = 0
      ut_end = 0d0

      UtLast=TimeLast*dSecPerDay        !time when last station becomes free.
    
      do itst=1,NumTst
        itrial=itrial_key(itst)
        mjdtst= 0   !set the time to zero in these arrays
        uttst = -1

        do isub=1,nsub_trial_vec(itrial)
          cbuf=ctrial_vec(isub,itrial)
!          write(*,*) isub, cbuf(1:60)
          call unpak(ierr,isub)
          scan_wt=dble(nstntst*(nstntst-1))/dble(NstnUse*(NstnUse-1))
 
          NumObsThisScan=nstntst*(nstntst-1)/2
          TstNumObs(itst)=TstNumObs(itst)+dble(NumObsThisScan)

          TstDurScan(itst)=max(TstDurScan(itst), utobss-utobs)  !maximum length of scan

!          write(*,*) utlast/86400., utobs/86400.,  utlast-utobs 
          TstBegScan(itst)=TstBegScan(itst)+ (UtLast-utobs)/60.  ! The smaller utobs is, the better  (normalized to minutes
          TstEndScan(itst)=TstEndScan(itst)+ (Utlast-utobss)/60. ! The smaller utobss is, the better (convert to minutes)
  
! Source stuff that is independent of the station.
          isrc=nsortst(isttst(1))
! 1.  Src even mode.
          if(kSrcEvn .and. iSrcEvnMode .ne. 0) then
            TstSrcEvn(itst)=TstSrcEvn(itst)+SrcDel(isrc)*scan_wt
          endif
          TstSrcWt(itst)=SrcWt(isrc)
! 2. Astrometric mode.
          if(kAstro .and. kastro_src(isrc)) then 
             TstAstro(itst) =TstAstro(itst) +AstDel(isrc)*scan_wt
          endif
! 3. Low dec sources
          if(kLowDec) then
!             DEC=SORPDA(2,NSOR)
             TstLowDec(itst)=TstLowDec(itst)-
     >                       (abs(sorpda(2,isrc)*rad2deg)/10.)*scan_wt
           endif
           if(KSkyCov) then
             TstSkyCov(itst)=TstSkyCov(itst)+sky_Trial_vec(itrial)
           endif 

           if(KCovar) then
            if(utobss-utlast .ne. 0) then 
               TstCovar(itst)=TstCovar(itst)+
     &              covar_Trial_vec(itrial) 
     &  /(utobss-utlast)     !divide by time required to complete scan. 
             else
                TstCovar(itst)=TstCovar(itst)+covar_trial_vec(itrial)*100
             endif 
   
!             write(*,*) ">>>", TstCovar(itst),
!     &            covar_trial_vec(itrial), utobss-utlast 
           endif 

! If fill in mode, don't want obs to end too long after previous obs.
          dtemp=utobss-utlast
          itemp=nint(dtemp)
          iRelTimeEnd(itst)= max(itemp,iRelTimeEnd(itst))

! Mark scan as bad if this pushes the end too late.
          if(kFillObs .and. iRelTimeEnd(itst) .gt. 60. ) then
               ktoolong(itst)=.true.
          endif

! Now loop over all stations in this scan
          do j=1,Nstntst
            istn=isttst(j)

            mjd_end(istn)=mjdtst(istn)
            ut_end(istn) =uttst(istn)+idurtst(istn)
            if(ut_end(istn) .gt. dsecperday) then
               ut_end(istn)=ut_end(istn)-dSecPerDay
               mjd_End(istn)=mjd_end(istn)+1
            endif

! Things that depend only on the station.
! 1. Station even mode.
            if(kStatEvn .and. iStatEvnMode .ne. 0) then
              TstStatEvn(itst)=TstStatEvn(itst)+StatDel(istn)*scan_wt
            endif
! 2. Station participation mode.
            TstStatWt(itst)=TstStatWt(itst)+statwt(istn)
 
            tapetime = idurtst(istn)
            if (kauto .and. iyr_end .ne. 0 .and.
     >          (utobs+tapetime .gt. TimeExpEnd*dSecPerDay)) then
                ktoolate(itst)=.true.
            endif

! Station dependendent source stuff
!  1. Source rising or setting.
            if (irs(isrc,istn).ne.0) then
              TstNumRiseSet(itst)=TstNumRiseSet(itst)+1
            endif
!  2. Low El source
            if(kNumLoEl .and. rloel .gt. 0) then
              CALL CVPOS(isrc,istn,mjdtst(istn),uttst(istn),
     >            az,el,ha,dec,x30,y30, x85,y85,kup)
              if (el.le.rloel) then
                  TstNumLoel(itst)=TstNumLoel(itst)+1
              endif
            endif
          end do  ! Nstttst
        enddo ! ik=1,NumSubntst

        TstBegScan(itst)=TstBegScan(itst)/dble(nsub_trial_vec(itst)) ! mean observation time beg
        TstEndScan(itst)=TstEndScan(itst)/dble(nsub_trial_vec(itst)) ! mean observation time end.

! Have done all subnets and stations. Compute variance in ending times.
        if(ktimeVar) then
          istn1=isttst(1)
          do i=1,NstnUse
            istn=istnvec(i)
            if(mjdtst(istn) .ne. 0) then  ! a station was in the scan.
              del_ut= uttst(istn)-uttst(istn1)
     >              + dble(idurtst(istn)-idurtst(istn1))
     >              + dble(mjdtst(istn)-mjdtst(istn1))*dSecPerDay
              num_stat=num_stat+1
              utSum  =UtSum+del_ut
              utSqSum=UtSqSum+del_ut*del_ut
            endif
          end do
          UtSqSum=UtSqSum/Num_Stat
          utSum=UtSum/Num_Stat
          TstTimeVar(itst)=dsqrt(abs(UtSqSum-UtSum*UtSum))
          TstTimeVar(itst)=TstTimeVar(itst)/60.    !convert to STDev in minutes
        endif

!  Check to see if this scan includes any idle stations that haven't been observed
!  If so, upweight it.
        if(kStatIdle) then
!          write(*,*) "Source ",csorna(isrc) 
! 2019Sep09.  Modified. 
! Make TstStatIdle be large if there are many antennas which have been idle for a while in this scan.
! Previous version added a penalty for stations that had been observed recently.     
          istn1=isttst(1) 
          do i=1,Nstntst
            istn=isttst(i)
! If the station had not been previously observed, give it a score of 10                 
            if(nsorcur(istn).eq. 0) then
               Del_ut=10
            else
! Else score is time in minutes since last observation.
               Del_ut= dsecdif(mjdtst(istn1),uttst(istn1),
     >                          mjdcur(istn),utcur(istn))/60.d0               
               if(del_ut .lt. 0) del_ut=0.d0           
            endif
            if(.false.) then
            write(*,*) cstnna(istn),  mjdtst(istn),
     >             dsecdif(mjdtst(istn1),uttst(istn1), 
     >                           mjdcur(istn),utcur(istn)),
     >             del_ut
            endif
            TstStatIdle(itst)=TstStatIdle(itst)+Del_ut         
          end do
!          write(*,*) "StatIdle: ", TstStatIdle(itst) 
        endif    !kstatIdle
      enddo ! itst=1,NumTst

      ibest=-1 ! initialize to -1 in case there are no possible configurations
      ScoreMax=-100000.

! Normalize minor options.
! If kOptNorm=true, we normalize by all of the scans that we considered.
!            =false, either no normalization, or some other kind.
!All of the normalizations are done so that
!  1. larger is better
!  2. all values are positive
      iplus  = 1         !if we pass +1 to Norm_by_range, larger are better.
      iminus =-1         !if we pass -1 to norm_by_range, smaller are better.

      if(KAstro) then
        if(kAstroNorm) then
          call Norm_By_Range(TstAstro,NumTst,iplus)
        Else
          TstAstro=TstAstro*100.  !converts to Percent
        ENDIF
        TstAstro=TstAstro*rAstroWt
      endif

      if(KBegScan) then
        if(kBegScanNorm) then
          call Norm_By_Range(TstBegScan,NumTst,iplus)
        ENDIF
        TstBegScan=TstBegScan*rBegScanWt
      endif

      if(KEndScan) then
        if(kEndScanNorm) then
          call Norm_By_Range(TstEndScan,NumTst,iplus)
        ENDIF
        TstEndScan=TstEndScan*rEndScanWt
      endif

      if(KNumLoEl) then
        if(kNumLoElNorm) then
          call Norm_By_Range(TstNumLoEl,NumTst,iplus)
!        Else
!          TstNumLoEl=TstNumLoEl
        ENDIF
        TstNumLoEl=TstNumLoEl*rNumLoElWt
      endif
      if(KLowDec) then
        if(kLowDecNorm) then
          call Norm_By_Range(TstLowDec,NumTst,iplus)
!       Else
!          TstLowDec=TstLowDec
        ENDIF
        TstLowDec=TstLowDec*rLowDecWt
      endif

      if(KNumObs) then
        if(.false.) then
          do itst=1,NumTst
            TstNumObs(itst)=TstNumObs(itst)/TstDurScan(itst)
          end do
        endif
        if(kNumObsNorm) then
          call Norm_By_Range(TstNumObs,NumTst,iplus)
        Else
!          TstNumObs=TstNumObs/(nstatn*(nstatn-1)/2)
        ENDIF
        TstNumObs=TstNumObs*rNumObsWt
      endif

      if(KNumRiseSet) then
        if(kNumRiseSetNorm) then
          call Norm_By_Range(TstNumRiseSet,NumTst,iplus)
!        Else
!          TstNumRiseSet=TstNumRiseSet
        ENDIF
        TstNumRiseSet=TstNumRiseSet*rNumRiseSetWt
      endif

      if(KSkyCov) then
        if(kSkyCovNorm) then
          call Norm_By_Range(TstSkyCov,NumTst,iplus)
 !       Else
!          TstSkyCov=TstSkyCov
        ENDIF
        TstSkyCov=TstSkyCov*rSkyCovWt
      endif



      if(KCovar) then
        if(kCovarNorm) then
          call Norm_By_Range(TstCovar,NumTst,iplus)
 !       Else
!          TstSkyCov=TstSkyCov
        ENDIF
        TstCovar=TstCovar*rCovarWt
      endif


      if(KSrcEvn) then
        if(kSrcEvnNorm) then
          call Norm_By_Range(TstSrcEvn,NumTst,iplus)
        Else
          TstSrcEvn=TstSrcEvn*100.  !converts to percent.
        ENDIF
        TstSrcEvn=TstSrcEvn*rSrcEvnWt
      endif

      if(KStatEvn) then
        if(kStatEvnNorm) then
          call Norm_By_Range(TstStatEvn,NumTst,iplus)
        Else
          TstStatEvn=TstStatEvn*100.
        ENDIF
        TstStatEvn=TstStatEvn*rStatEvnWt
      endif

      if(KStatIdle) then
        if(kStatIdleNorm) then
          call Norm_By_Range(TstStatIdle,NumTst,iplus)
        Else
          TstStatIdle=TstStatIdle
        ENDIF
        TstStatIdle=TstStatIdle*rStatIdleWt
      endif

      if(KSrcWt) then
        if(kSrcWtNorm) then
          call Norm_By_Range(TstSrcWt,NumTst,iplus)
         ENDIF
        TstSrcWt=TstSrcWt*rSrcWtWt
      endif

      if(KStatWt) then
        if(kStatWtNorm) then
          call Norm_By_Range(TstStatWt,NumTst,iplus)
          ENDIF
        TstStatWt=TstStatWt*rStatWtWt
      endif

      if(kTimeVar) then
        if(kTimeVarNorm) then
          call Norm_By_Range(TstTimeVar,NumTst,iminus)
        Else
          TstTimeVar=-TstTimeVar
        ENDIF
        TstTimeVar=TstTimeVar*rTimeVarWt
      endif
! Done normalization

! Write header info
      if(kdebug .or. iverbose_level.ge.5) then
        call write_minor_score_header(ludsp)
      endif 

      do itst=1,NumTst
        Score=0.d0
        if(kastro)       Score=Score+TstAstro(itst)
        if(kBegScan)     Score=Score+TstBegScan(itst)
        if(kDurScan)     Score=Score+TstDurScan(itst)
 
        if(kEndScan)     Score=Score+TstEndScan(itst)
        if(kLowDec)      Score=Score+TstLowDec(itst)
        if(kNumLoEl)     Score=Score+TstNumLoel(itst)
        if(KNumObs)      Score=Score+TstNumObs(itst)
        if(kNumRiseSet)  Score=Score+TstNumRiseSet(itst)
! Skycoverage.
        if(kSkyCov)      Score=Score+TstSkyCov(itst)
        if(kCovar)       Score=Score+TstCovar(itst) 
         if(kSrcEvn .and.iSrcEvnMode.ne. 0) Score=Score+TstSrcEvn(itst)
        if(ksrcWt)       Score=Score+TstSrcWt(itst)
        if(kStatEvn.and.iStatEvnMode.ne. 0)Score=Score+TstStatEvn(itst)
        if(kStatIdle)    Score=Score+TstStatIdle(itst)
        if(kStatWt)      Score=Score+TstStatWt(itst)  
        if(kTimeVar)     Score=Score+TstTimeVar(itst)

        if(ktoolong(itst)) then
          write(lreason,'("Too long: ",i8)') iRelTimeEnd(itst)
        else if(ktoolate(itst)) then
          lreason = "Too late: "
        else if(ktoobig(itst)) then
          lreason = "Overweighted obs: "
        else
          write(lreason,'(10x,i8)') iRelTimeEnd(itst)
        endif

       if(kdebug) then
         call write_minor_score_and_Scan(ludsp,lreason,itst,Score)
        endif

        if (ktoolate(itst)) then ! disqualified
           continue
        else if(ktoobig(itst)) then
           continue
        else if(ktoolong(itst)) then
           continue
        else ! check it out
          if(Score .gt.ScoreMax) then
            ScoreMax=Score
            ibest=itst
          endif
        endif ! disqualified/check it
      enddo ! i=1,NumTst
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CNV If there is no qualified configuration, then they have all
C   exceeded the stop time.
      if (ibest.eq.-1) then
         return
      ENDIF
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CHS Initialization
C
      if(iverbose_level.ge.5 .or. kdebug) then
        itst=ibest
        lreason="Final"
        call write_minor_score_and_Scan(ludsp,lreason,itst,Scoremax)
      endif 

      return
      end

