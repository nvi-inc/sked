      subroutine MakeSubSet(iStnAll,NumAll,TimeFreeOut,
     >  iFillTime,iFillSub,iStnSub,NumSub,
     >  ludsp, cpocod,kdebug)
      implicit none
! Basic idea is that we want to see if some subset has been idle for some time.
! If so, then we return a list of the idle stations.

! History
!  2009Jun29  JMGipson. Fixed bug in calculating TimeFree. Previously used outside time, which included stations not in list. 
!  

! Passed variables.
      integer NumAll
      integer iStnAll(NumAll)            !Array of stations we start with
      double precision TimeFreeOut(*)    !Time a station is free (in MJDs)
      integer iFillTime
      integer ifillSub                   !smallest size array
      character*2 cpocod(*)              !station array
      logical kdebug                     !display debugging info.
      integer ludsp

! Returned. Array of stations to participate.
      integer iStnSub(*)   !Array we end up with.
      integer NumSub       !number of elements

! Internal variables.
      integer ikey(NumAll)

      double precision TimeFree(NumAll) !Internal array
      double precision UtDel(NumAll)    !(TimeLast-TimeFree)*secperday
      integer i
      double Precision TotalObsTime(NumAll)  !#stations * UTdel

      logical kMaxOBs/.true./           !Maximize #of obs, or #amount of time we fillin.
      Double Precision TotalMax
      integer imaxPtr
      integer istn

! This extracts the part of the timeFree array which is relevant. 
      do i=1,NumAll
        TimeFree(i)=TimeFreeOut(iStnAll(i))
      end do 

! sort by ending time
      call indexx8(NumAll,TimeFree,ikey)
      do i=1,NumAll
        UTdel(i)=(TimeFree(ikey(NumAll))-TimeFree(i))*86400.d0   !This makes UTDel time to end of last obs.
      end do   
     
      iMaxPtr=0
      TotalMax=0.

! Go through and look at subsets of arrays that maximize amount of time we can fill in.
! This is a function like  NumObs*(Time all stations are free)
!      kMaxObs=.false.      !if this is false, try to maximize time we fill in.
      TotalObsTime(1)=0
      do NumSub=2,NumAll
       istn=ikey(NumSub)    
    
       if(KmaxOBs) then
         TotalObsTime(NumSub)=UTDel(istn)*NumSub*(NumSub-1)
       Else
         TotalObsTime(NumSub)=UTDel(istn)*NumSub
       endif
!       write(*,*) NumSub, istn, utdel(istn), TotalObsTime(Numsub)
!       pause

       if(TotalObsTime(NumSub) .ge. TotalMax.and. 
     >     Utdel(istn) .ge. IFillTime) then !try to maximize
          iMaxPtr=NumSub
          TotalMax=TotalObsTime(NumSub)
       endif
      end do

      if(kdebug) then
        write(ludsp,'("Stations   ",32(1x,a6))')
     >    (cpocod(istnall(ikey(i))),    i=1,NumAll)
     
        write(ludsp,'("IdleTime   ",32(1x,i6))')
     >   (int(UTDel(ikey(i))),i=1,NumAll)
        write(ludsp,'("TotObsTime ",32(1x,i6))')
     >     int(TotalObsTime(1:NumAll))
        write(ludsp,*) "NumSub: ",iMaxPtr
      endif

      if(iMaxPtr .lt. iFillSub) then
        iStnSub(1:NumAll)=iStnAll(1:NumAll)
        NumSub=NumAll
      Else
        do NumSub=1,iMaxPtr
           iStnSub(NumSub)=iStnAll(ikey(NumSub))
        end do
        NumSub=iMaxPtr
      endif

      return

      End

