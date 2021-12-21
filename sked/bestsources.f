      subroutine bestsources(cmdline)
! Get the best source, based on current network

! History
!   2005Jan26 JMGipson Modified to turn on vscan if it was off.
!   2005Mar16 JMGipson Alex took out parsing because it didn't work
!             correctly under linux. I put it back in using "splitntokens"
!             Also check to see that irank_mode has valid values of 1,2,3
!   2006May31 Check that duration of experiment is not longer than 1 day.
!   2012Oct10 JMG. Modified to update version, catalog name info. 
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'   
C
C   COMMON BLOCKS USED
      include 'major.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'astro.ftni'
      include 'cat_name_version.ftni'


! function
      integer trimlen
      integer Julda

! passed
      character*(*) cmdline

! General scheme:
!  1. Rank all sources by some criteria.
!  2. Choose best source by rank.
!  3. Add to list of best sources.
!  4. See effect of adding a new source on sky coverage.
!  5. Look at NumCover of these,
!  6. Go to 2 and repeat until we have NumBest sources.

! local
      integer ierr
      integer NumBest           !number of "best sources"
      integer irank_mode        !criteria for ranking
      integer NumCover          !number of sources used from coverage.
! following equivalence makes parsing code slightly easier.
      Integer NumBestParm(3)
      equivalence (NumBestParm(1),NumBest),(NumBestParm(2),irank_mode)
      equivalence (NumBestParm(3),NumCover)

      integer MaxToken
      integer NumToken
      parameter(MaxToken=4)
      character*30 ltoken(MaxToken)

      integer i,iptr
      double precision srcrank(max_sor)
      integer ibestSrc(Max_sor)
      character*50 lfilnam
      logical kwrite

! used in saving astrometric settings.
      logical keep_astro
      integer NumAst
! Start and end date.
      integer MJD_START,MJD_END

      MJD_start=JULDA(1,IDA_start,IYR_start-1900)
      MJD_end=JULDA(1,IDA_end,IYR_end-1900)

      if(MJD_end-MJD_start .gt. 1) then
         write(*,*) "Bestsources:  Experiment duration too long."
         write(*,*) "              Max is 1 day."
         return
      endif

! need to do this, cause we may remove a source that was scheduled.
      call delete_all_obs()

      kwrite=.false.

      if(kwrite) then
        lfilnam=cexper(1:trimlen(cexper))//"src.out2"
        open(1,file=lfilnam)
        do iptr=1,Nsourc
          write(1,'(i4,1x,a8,1x,2f8.2)') iptr,csorna(iptr),
     >     sorp_now(1,iptr)*rad2deg,sorp_now(2,iptr)*rad2deg
        end do
        close(1)
      endif

! Parse the arguments: Have three--NumBest,Irank_mode,NumCover
! Default Values.
      NumBest=60
      NumCover=3
      irank_mode=3

      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      if(NumToken .eq. 0) then
        goto 20
      else if(NumToken .ge. 4) then
        write(luscn,*) "BestN: Too many parameters! Max=3"
        return
      else
        do i=1,NumToken
          read(ltoken(i),'(i4)',err=900) NumBestParm(i)
        end do
      endif
      if(irank_mode .lt. 1 .or. irank_mode .gt. 3) then
         write(luscn,'(a,i4)')
     >    "BestN Abort: Valid modes are 1-3. You entered ",irank_mode
         return
      endif
      if(NumCover .gt. NumBest) then
         writE(luscn,*) "BestN Error: NumCover is larger than NumBest"
         return
      else if(NumCover .gt. NumBest/3) then
         write(luscn,*) "BestN: Large relative value for NumCover"
         write(luscn,*) " Proceeding, but be ware of results."
      endif

!---End of getting the arguments.
20    continue
      write(luscn,
     > '("BestSource:  NumBest= ",i3," BestMode",i3," NumCover=",i3)')
     >    NumBest,irank_mode,NumCover

      if(NumCover .lt. 1) then
        write(*,"('BestSource: NumCover too low. Minimum is 1')")
        return
      else if(NumCover .gt. NumBest/2) then
        write(*,"('BestSource: NumCover too large. Maximum is ',i4)")
     >    NumBest/2
        return
      endif

      keep_astro=.true.

      call ReadSourceCatalog(source_cat,csofil,keep_astro,ierr)
      call rsini            !now calculate the rise set time of all sources
! Update catalog, version info.
      lsource_cat_use=source_cat
      call get_cat_version(lsource_cat_use,lsource_cat_version,ierr)
      
      if(ierr .ne. 0) then
        write(luscn,'(a)') "BestSources: Error in reading catalog!"
        return
      endif
      call ranksources(irank_mode,srcrank)

! Note:
!    After readSourcCatalog astrometric sources are in the top.
      NumASt=0
      do i=1,Nsourc
        if(kastro_src(i)) then
           ibestSrc(i)=i
           NumAst=NumAst+1
        else
           goto 100
        endif
      end do
100   continue

      if(NumBest+NumCover .gt. Nsourc) then
         write(*,*) "Warning! NumBest => Nsourc"
         write(luscn,*) "Using all available sources!"
         return
      endif

      if(.not.kvscan) then
        write(luscn,*) "Warning! Vscan was turned off. Turning it on!"
        kvscan=.true.
      endif

      call FindBestSources(sorp_now,NSourc,NumAst,srcrank,
     >    NumBest,NumCover,iBestSrc,luscn)

      if(NumBest .eq. 0 .and. kAllBLGood) then
           write(*,*) "    HINT: TRY TURNING OFF ALL_BL_GOOD option!"
           write(*,*) "         major ALL_BL_GOOD no"
      endif

      kwrite=.false.
      if(kwrite) then
        write(lfilnam,'(a,"N",i2.2,"M",i2.2,"C",i2.2,".out")')
     >  cexper(1:trimlen(cexper)),  NumBest,irank_mode,NumCover
        open(1,file=lfilnam)
        do i=1,numbest
          iptr=ibestsrc(i)
          write(1,'(i4,1x,a8,1x,2f8.2)') i,csorna(iptr),
     >     sorp_now(1,iptr)*rad2deg,sorp_now(2,iptr)*rad2deg
        end do
        close(1)
      endif
! Save only the best sources.
      call Keep_Some_Srcs(iBestSrc,NumBest,csofil,ierr)
      if(ierr .ne. 0) then
        write(luscn,*) "Bestsources: error in keep_some_src"
        return
      endif

! Recalculate rise set times, since we have different set of sources.
      call rsini
      krsini=.true.
      return

! Error exit
900   continue
      write(luscn,*) "Error in parsing NumBest Args: "
     > //cmdline(1:trimlen(cmdline))

      return


      end
! ************************************************************************
      subroutine FindBestSources(SrcRADec,NumSrc,NumInit,SrcRank,
     >   NumBest,NumCover,iBestSrc,luscn)
! Find the best sources for an experiment.
! ranked by sky coverage, then by "SrcRank"functions
      implicit none
    
! functions
      double precision dot8
! On entry
      integer NumSrc                    !Total Number of sources
      integer NumInit                   !Number of sources in initial set. Could be 0.
      double precision SrcRADec(2,NumSrc)  !RA and Dec of sources.
      double precision SrcRank(NumSrc)  !Goodness of source by some criteria
      integer NumBest                   !Number of sources we want (<NumSrc)
      integer NumCover                  !Number of sources we use from coverage.
      integer luscn                     !unit to write error messages to.
! On exit
      integer iBestSrc(*)        	!Best sources.

! Local variables
      double precision XYZ(3,NumSrc)    !Unit vector to sources
      logical kused(NumSrc)             !Is the source used
      double precision rMinAng(NumSrc) 	!Maximum cosine of source from colleciton of sources.
      double precision rAng
      double precision rCosAng
      integer ikey(NumSrc)              !auxiliary vector used to rank sources.
      integer ibest
      integer itest

      integer isrc                      !index
      integer iptr
      Double precision SrcRankBest      !best rank.
      double precision CosDec           !cosine dec
      double precision pi
      double precision rad2deg
      integer NumPossible
      integer iBestStart
      integer i

      pi=2.0d0*acos(0.0d0)
      rad2deg=180.d0/pi

! initialization.
! A.) Find best source. This is the first source we use.
! B.) Compute unit vectors to sources.

      NumPossible=0
      rMinAng=pi
      do isrc=1,NumSrc
        if(SrcRank(isrc) .gt. 0) then
          kused(isrc)=.false.
          NumPossible=NumPossible+1
          ibestSrc(NumPossible)=isrc !default is we take all the sources  that we can observe.
        else
          kused(isrc)=.true.         !exclude sources with ranks <0. These can't be observed.
        endif
        cosDec=Cos(SrcRADec(2,isrc))
        XYZ(1,isrc)=CosDec*Cos(SrcRaDec(1,isrc))
        XYZ(2,isrc)=CosDec*Sin(SrcRaDec(1,isrc))
        XYZ(3,isrc)=Sin(SrcRADec(2,isrc))
      end do

! Sometimes, have fewer possible sources than we want. Exit at this point.
! May happen for small network, restricted times, e.g., intensives.
      if(NumPossible .eq. 0) then
         write(luscn,*) "FindBestSources: ERROR! No possible sources"
          numbest=0
         return
       endif

      if(NumPossible .le. NumBest) then
         write(luscn,*) "BestSources: # of possible sources: ",
     >     NumPossible,  " Less than NumBest: ", NumBest
         write(luscn,*) " Adjusting NumBest to", NumPossible
         NumBest=NumPossible
         return
      endif

! Most of the time, we have to choose the best sources from larger list.
! Two possibilites:

      if(NumInit .eq. 0) then
! 1. No restriction on sources. Find the source that has the maximum rank. This seeds the list.
        SrcRankBest=-1.
        do isrc=1,NumSrc
          if(SrcRank(isrc) .gt. SrcRankBest) then
            SrcRankBest=SrcRank(isrc)
            ibestSrc(1)=isrc
          endif
        end do
        kused(ibestSrc(1))=.true.
        iBestStart=2    !pointer to next place to start.
      else
! 2. Have some sources in the list initially.
        iBestStart=NumInit+1
        do i=1,NumInit
           kused(ibestsrc(i))=.true.
        end do
        do isrc=1,NumSrc
          if(.not. kused(isrc)) then   !Only do for ranking >0, i.e., sources we will use.
            do ibest=1,NumInit         !Want to find distance of remaining sources to (Best-1) universe.
               rCosAng=dot8(XYZ(1,isrc),XYZ(1,iBestSrc(ibest)))
               if(rCosAng .gt. 1.d0) then
                 rAng=0.
               else if(rCosang. lt. -1.d0) then
                 rang=pi
               else
                 rAng=Acos(rCosAng)
               endif
               rMinAng(isrc)=min(rAng,rMinAng(isrc))
            end do
          endif
        end do
      endif

! In the following, we find the minimum angle of a source w/r/t the sources in the iBestSrc list.
! Note that this list starts with either
!   1. Source, found above, as source with higest rank.
!   2. NumInit sources
! In both cases, rminAng(isrc) is the minimum angular distance of current source to source already
! in the list.
!
! Since iBestSrc is only built up 1 src at a time, we only need to find the angle
! to the last source added to this.

! Now here is where we build up the rest.
      do ibest=iBestStart,NumBest
! find cosine of angle between all sources and the current list.
        do isrc=1,NumSrc
          if(.not. kused(isrc)) then   !Only do for sources not in "iBestSrc"
             rCosAng=dot8(XYZ(1,isrc),XYZ(1,iBestSrc(ibest-1)))   ! ibest-1 is last source added.
             if(rCosAng .gt. 1.d0) then
               rAng=0.
             else if(rCosang. lt. -1.d0) then
               rang=pi
             else
               rAng=Acos(rCosAng)
             endif
             rMinAng(isrc)=min(rAng,rMinAng(isrc))
          endif
        end do

! Rank the sources by angular distance.
        call indexx8(NumSrc,rMinAng,ikey)

        SrcRankBest=-1.  !Note-all the ranks are positive.
        itest=0
! Once we have the angles, we consider "NumCover", starting with the sources which are furthest,
! and pick the one which is ranked highest.
        do isrc=NumSrc,1,-1  !Start at the top of the list and work done.  (largest min. distance.
          iptr=ikey(isrc)
          if(.not.kused(iptr)) then
             itest=itest+1                   !itest is the number of sources tested this go round.
             if((SrcRank(iptr) .gt. SrcRankBest)) then
               SrcRankBest=SrcRank(iptr)
               iBestSrc(ibest)=iptr
             endif
          endif
          if(itest .ge. NumCover) goto 200  !tested Numcover sources, exit with best.
        end do
200     continue
        kused(iBestSrc(ibest))=.true.
      end do

       if(.false.) then
!      if(.true.) then
        do isrc=1,NumBest
          iptr=iBestSrc(isrc)
          write(*,'(1x,i4,1x,f8.2)') isrc,
     >      rMinAng(iptr)*rad2deg
        end do
      endif

      return
      end

