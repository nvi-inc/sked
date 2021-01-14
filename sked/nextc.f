      SUBROUTINE NEXTC(Linstq)
C
C    NEXTC computes and displays sources available for observation
C    ( WHATSUP command)
C
C   COMMON BLOCKS USED
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'covar.ftni'
      include 'astro.ftni'
C
C     CALLING ROUTINES: SKED
C     CALLED SUBROUTINES: CVPOS,SLEWT,SNROK,NEXTPR,STATn,UNPAK,
C                         SIMUL,INV,SUBCON,TSINCER,RSINI
C
C functions
      real sunarc       		!distance to sun.
      double precision hms2seconds    !convert hours,min,seconds to seconds
      integer julda             	!julian day
      logical kstatup  		!is a station observing
      double precision dot8           !dot product
      integer indx4
C  INPUT:
      integer*2 Linstq(*)

C  LOCAL VARIABLES
      double precision dSecPerDay
      parameter (dSecPerDay=86400.d0)
C      - holder for footage count at end of current obs
      integer iStnAll(max_stn)          !Stations used initially.
      integer iStnSub(MAX_STN)          !Subset of iStnAll
      integer istn                      !
      integer NumAll,  NumSub           !Number in each array.
      integer NsrcUse                   !number of sources we  use
      integer iZero
      real arcd                         !distance of sun from source

      logical kNumObsOld,kEndScanOld    !logical variables we want to preserve
      logical kStatWtOld,kSkyCovOld
      logical kOptBySkyOld

      logical kexist                    !Used to see if file exists
      logical koff

      integer iSrcVec(max_sor)          !Source array
      integer isrc

      integer*4 NumObsSrc(MAx_Sor)
      integer*4 NumOBsStat(MAx_stn)
      integer*4 NumObs

      LOGICAL KMIN

C      - time of rise,set during lookahead
      Double precision UtFree(Max_Stn)       !Time last obs was done. (seconds part)
      Double Precision UtEndTotPrev(Max_stn)    !=MJDCUr*dSecPERDAY+UTendTotPrev
      integer MjdFree(max_stn)

      integer ierr
      integer i,j                              !counters
      integer ielall(max_sor,max_stn)          !used for VLBA stations in special mode

      integer*2 job                            !flag to indicate what we do 10=compute det
      double precision rcond                   !matrix condition number.

! 040627  ZMM (see PTOBS) logical->integer
      integer iunerr

C added by JMGipson
      integer ibest                             !best station

! These are variables  used in gauging the goodness of an obsevation.
      integer itrial   !index for ctrial_vec

      integer NumTst                            !Number to test
      Integer itemp   !Short term temporary integer variable

      logical kFillObs                          !Fill in holes in schedule?
      real rBestPerCentOld                      !Maximum # to consider.

! all of the following are measured in units of Days.
      double precision TimeExpBeg               !Time of Experiment Beg in Days
      double precision TimeExpEnd               !Days
      double precision TimeEndCurObs            !Time end current obs in days
      double precision TimeFinish               !last time for Automatic scheudulind
      double precision TimeFree(max_stn)
      double precision TimeEarly                !Time of Earliest station.
      double precision TimeLast                 !And time of last station

      integer MJDExp

      character*1 cans

! Used for finding scan with maximum angular distance
      double precision dist                     !distance between two sources
      double precision dist_max                 !maximum distance
      double precision src_unit(3)              !unit vector to source
      double precision src0_unit(3)
      integer itst_max
      integer itst
      integer isrc_max

      character*80 ldum 

!  Date     Who   What
!  2007Sep22 JMG  Broken into smaller pieces. Got rid of obsolescent code.
!  2008Jun06 JMG  Got rid of some variables that were no longer used
!  2009Sep22 JMG. Changed format statement for Endtime later than 1 day.
!  2010Mar19 JMG. Minor formatting changes. 
!  2010Mar26 JMG. Changed stutcm ->utstcm, enutcm-utencm for consistency with jdstcm and jdencm

      IF  (NSOURC.EQ.0.OR.NStatn.EQ.0.or.ncodes.eq.0) THEN  !
        write(luscn,*)
     >      ' Select sources, stations, and frequencies first.'
        return
      endif 


!1. Get command parameters and determine KAUTO variable.
      call nextpr(linstq,NumAll,iStnAll,kmin,ierr)
      if (ierr.ne.0) return
      NumSub=NumAll
      iStnSub(1:NumSub)=iStnAll(1:NumAll)   !copy these   
      if(NumAll .lt. MinSubnetSize) then
         write(luscn,*) 
     >   "Number stations < minsubnet. No observations possible"
         return
      endif

! 2. Get rid of stopping file if it exists.
      inquire(file="sked.stop",exist=kexist)
      if(kexist) then
        write(*,*) "Removing sked stop file 'sked.stop'."
        open(99,file="sked.stop")
        close(99,status="delete")
      endif

!If doing covariance optimization do some setup. 
      if(kcovar) then
         call op_refresh
         if(num_est .eq. 0) then
            write(*,*) 
     &        "Covariance optimization with no parameters specified."
            write(*,*) "Set parameters using 'op set'"
            return
         endif
         if(nobs .eq. 0) then
            do i=1,num_est
               dnorm_tri(indx4(i,i),0)=small 
            end do 
          endif   
          num_tri_est=num_est*(num_est+1)/2
      
          dnorm_inv(1:num_tri_est)=dnorm_tri(1:num_tri_est,0)  !normal equations so far
          job=11         !compute condition number and inverse.              
          call invert_and_con_tri(dnorm_inv,rcond,num_est,job)
        endif

! Set some parameters for autosked.

! Save everything that is reset in this routine
      kNumObsOld   = KNumObs
      kEndScanOld  = kEndScan
      kStatWtOld   = kStatWt
      kSkyCovOld   = kSkyCov
      kOptBySkyOld = kOptBySky
      rBestPerCentOld=rBestPerCent

      TimeExpBeg = 0d0
      TimeExpEnd = 1d0

      TimeExpBeg=dble(JULDA(1,IDA_start,IYR_start-1900))+
     >      hms2seconds(ihr_start,imin_start,isc_start)/dSecPerDay

      if(iyr_end .ne. 0) then
        TimeExpEnd=dble(JULDA(1,IDA_end,IYR_end-1900))+
     >      hms2seconds(ihr_end,imin_end,isc_end)/dSecPerDay
      else
        TimeExpEnd=TimeExpBeg+1.
      endif

      if(TimeExpEnd .lt. TimeExpBeg) then
         write(*,*)  "NEXTC01: Nominal end time is BEFORE Beg time!"
         write(*,*)
     >     "   Assuming user error. Setting END_TIME=BEG_TIME+24Hr"
            TimeExpEnd=TimeExpBeg+1.
      else if(TimeExpEnd-TimeExpBeg .gt. 1.05) then
10       continue
         write(*,
     >  '("NEXTC02: Nominal end time is",f10.2, " days after start.")')
     >      TimeExpEnd-TimeExpBeg
         write(*,
     >  '(" Is this OK? Enter Y/N :")')
         read(*,'(a)') ldum
         call capitalize(ldum)
         if(ldum .eq. "N" .or. ldum .eq. "NO") then 
           write(*,*) "Please modify end time and try again."
           return
         else if(ldum .eq. "Y" .or. ldum .eq. "YES") then 
            continue
         else 
            write(*,*) "Invalid response: ",trim(ldum)
            write(*,*) "Try again!" 
            goto 10
         endif
      endif

C   Calculate all rising/setting times now.
      if (.not.krsini) call rsini
      
! Ignore sources too close to the sun.
      MJDExp=TimeExpBeg
      NsrcUse=0
      iZero=0
      DO I=1,NSOURC
        ARCD = SUNARC(I,mjdcur(1),utcur(1))
        IF (ARCD.NE.-1.0.AND.ARCD.LT.rSunMinAngle) THEN !too close
          continue
        else
          NsrcUse=NsrcUse+1
          iSrcVec(NsrcUse)=I
        ENDIF
      ENDDO

      if(.false.) then
        call WriteTotalUpTime(iSrcVec,NsrcUse,iStnAll,NumAll)
      endif

      TimeFinish = -1d0

      write(ludsp, '("Auto Mode:       ",L1)') ,kauto

      if (kauto) then !optimization
! 2. Ending time of optimization mode. Default is given by command line.
        TimeFinish=dble(jdstcm)+utstcm/dSecPerDay
        TimeFinish=min(TimeFinish,TimeExpEnd)
      endif !automatic / no autom. optimization

! Do the loop below until the end of the current obs is past
!  1.) End of experipent.
!  2.) End of time we set in the command.

!     kFillObs=kFillIn
      kFillOBs=.false.    !whenever we enter start with full subnet 
! this  ensures that we do at least one time through the loop.
      TimeEndCurObs=TimeFinish-1

      do while(TimeFinish.ge.TimeEndCurObs)  ! optimization loop
        inquire(file="sked.stop",exist=kexist)
        if(kexist) goto 1200

! Set default optimization values.
        KNumObs  =kNumObsOld
        kEndScan =kEndScanOld
        kStatWt  =kStatWtOld
        kSkyCov  =kSkyCovOld
        kOptBySKy=kOptbySkyOld

        if(koptBySky) then
          continue              !anything special for optimization by sky coverage.
        endif 


! Calculate some times that are used below.
! Calculate UtFree.  This is end of previous scan in seconds.
! AEM 20050221 additionally initialize UtFree
        UtFree = 0d0
        UTEndTotPrev=-1.         !-1 is also a flag for "not-used".
        TimeLast=TimeExpBeg      !Time latest staiton is free
        TimeEarly=TimeExpEnd     !Time earliest station is free.

        do i=1,NumAll
          istn=iStnAll(i)
          UtFree(istn)=utcur(istn)+dble(idurcur(istn)+idlcur(istn))
          MjdFree(istn)=Mjdcur(istn)
          if(UtFree(istn) .ge. dsecperDay) then
             UtFree(istn)=UtFree(istn)-dsecperDay
             MjdFree(istn)=MjdFree(istn)+1
          endif
          Timefree(istn)= dble(MjdFree(istn))+UtFree(istn)/dsecperDay
          if(kstatup(istn,mjdFree(istn),UtFree(istn),izero)) then
            TimeLast= Max(TimeLast,Timefree(istn))
            TimeEarly=Min(Timelast,Timefree(istn))
          endif
        end do

        do i=1,NumAll
          istn=iStnAll(i)
          if(.not. kstatup(istn,MjdFree(istn),Utfree(istn),izero)) then
!             write(*,*) "turning off ", cpocod(istn)
!             TimeFree(istn)=TimeLast
          endif
        end do
     
        if(kauto .and. kfillobs) then
          call MakeSubSet(iStnAll,NumAll,TimeFree,
     >      iFillTime, iFillMin, iStnSub,NumSub,
     >      ludsp, cpocod,kdebug)
          if(NumSub .eq. NumAll) then
              kFillObs=.false.
          else
             rBestPerCent=float(iFillBest)/100.d0
           endif
        else
          rBestPerCent=rBestPerCentOld
          iStnSub(1:NumAll)=iStnAll(1:NumAll)
          NumSub=NumAll
        endif

        write(ludsp, 
     >   '("Fill-In Mode:    ",L1,"  Subnet: ", a2,64("-",a2))') 
     >    kfillobs,(cpocod(istnsub(j)),j=1,NumSub)    

! Calculate what sources are up at which stations.
        call whatsup(IstnSub,NumSub,iSrcVec,NsrcUse,MjdFree,UtFree,
     >    ielall,kmin)
! If not in optimization mode, exit.
        if (.not.kopgo) then
          if(kauto) then
            call opfill     
          else  
            goto 1200
          endif
        endif 
     
      if(.true.) then
       call MakeObsPer(isrcvec,NsrcUse,iStnAll,NumAll,
     &     NumObsSrc,NumObsStat,NumObs) 

       koff=.false.
      
       itemp=0
       if(NumObs .ne. 0) then
! turn off astrometric sources if above targets ans astromode is on.
       do i=1,NsrcUse         
         isrc=isrcvec(i)
         if(kastro .and. kastro_src(i)) then 
             if(dble(NumObsSrc(isrc))/dble(NumObs) .ge. 
     &                         rmax_astro(isrc)) then
               kvs(isrc,1:Max_Stn)=.false.
               if(.not.koff) then
                  koff=.true.
                  write(*,
     &             "('Following astro sources meet targets:  ',$)") 
               endif
               write(*,'(" ",a8, $)') csorna(isrc)
               itemp=itemp+1
               if(mod(itemp,10) .eq. 0) write(*,*) " "
             endif
         endif
       end do
       if(koff) write(*,*) " "
!       pause
       endif
       endif

        call clear_close_sources(isrcvec,nsrcuse,istnsub,numsub)
!  write up a map showing the visibility

        if(kdebug) then
          call write_vs(isrcvec,NsrcUse,istnSub,numsub)
        endif

! Findpossible scans.
        NumTrial=0
        call make_scans(istnSub,NumSub,iSrcVec,NsrcUse,
     >     NumSubNet,MinSubNetSize)

        NumTst=max(nint(dble(numTrial)*(rBestPerCent)),1)
        NumTst=min(NumTst,max_trial,numTrial)
        write(ludsp, '("Total tested: ",i4, "  Tested for Minor:",i4)')
     >     numTrial,NumTst

! NumTst = number of configurations to test.
        if(NumTst.eq.0) then ! no configuration possible
          if(kFillObs) then
            write(ludsp,*)
     >      "NEXTC04: No oservations possible with current subnet."
            write(ludsp,*) "       Going to full network."
            kFillObs=.false.
            goto 1100
          else
            write(ludsp,*)
     >        "No more observations possible with this subnet"
            goto 1200
          endif
        endif
       

! get sort key.
!     Note that this sorts ascending.
        NumTrial=min(NumTrial,Max_trial)   !set to the number we actually kept.
        if(koptbysky) then 
          call indexx8(NumTrial,sky_trial_vec,itrial_key)
        else
          call indexx8(NumTrial,covar_trial_vec,itrial_key)
        end if   
! Change the order from ascending to descending
        do i=1,NumTrial/2
          itemp=itrial_key(i)
          itrial_key(i)=itrial_key(NumTrial+1-i)
          itrial_key(NumTrial+1-i)=itemp
        end do

        if(.not.kauto) then !no automatic optimization
          call subcon(NumSub,iStnSub,itrial_key)
          goto 1200
        endif

        call find_best_scan(TimeFree,TimeLast,TimeExpEnd,
     >  isrcvec,NSrcUse,
     >  iStnAll,NumAll, iStnSub,NumSub,  NumTst,kfillobs,ibest)

        if(ibest .eq. -1) then
          if(.Not.kFillObs) then
            write(*,*) "No more valid observations found within time."
            goto 1200
          else if(Kfillin .and. KfillObs) then
            write(*,*) "Turning off fillin flag!"
            kFillObs=.false.
            goto 1100
          endif
        endif

        if(kFillin) kFillObs=.true.
! Insert the scan into the schedule.
        itrial=itrial_key(ibest)
        do i=1,nsub_trial_vec(itrial) ! Put the observation in the schedule.
          cbuf=ctrial_vec(i,itrial)
          call unpak(iunerr,0)
!          if(i .eq. 1) then
!            isrc0=nsortst(isttst(1))
!            call make_unit_vector(
!     >      sorp50(1,isrc0),sorp50(2,isrc0),src0_unit)
!          endif

          call ptobs("AU",0,ierr)
          if(ierr .ne. 0) goto 1200
        enddo
!       pause 
       
! Here we put in the scan which is in the most opposite direction.
        if(.false.) then
          dist_max=0.d0
          itst_max=0
          do itst=1,NumTst
            if(itst .ne. ibest) then
              itrial=itrial_key(itst)
              cbuf=ctrial_vec(1,itrial)
              call unpak(ierr,1)
              isrc=nsortst(isttst(1))
              call make_unit_vector(
     >        sorp50(1,isrc),sorp50(2,isrc),src_unit)
              dist=acos(dot8(src_unit,src0_unit))
              if(dist .gt. dist_max) then
                dist_max=dist
                itst_max=itst
                isrc_max=isrc
              endif
            endif
          end do

          do i=1,nsub_trial_vec(itst_max) ! Put the observation in the schedule.
            cbuf=ctrial_vec(i,itrial)
            call unpak(iunerr,0)
            call ptobs("AU",0,ierr)
            if(ierr .ne. 0) goto 1200
          enddo
        endif


! Find end time of current observation. This is to check if we are done.
        TimeEndCurObs=0.d0
        do i=1,NumAll
          istn=iStnAll(i)
          TimeEndCurObs=max(TimeEndCurObs,   dble(mjdcur(istn))+
     >        (utcur(istn)+dble(idurcur(istn)))/dSecPerDay)
        enddo
 1100   continue
      enddo ! while(TimeFinish.ge.TimeEndCurObs)) optimization loop

! standard return
1200  continue
! restore everything that got changed.
      rBestPerCent=rBestPerCentold

      kOptBySKy=kOptBySkyOld
      KNumObs  =kNumObsOld
      kEndScan =kEndScanOld
      kStatWt  =kStatWtOld
      kSkyCov  =kSkyCovOld
      kauto=.false.
      RETURN
      END
