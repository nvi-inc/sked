      subroutine tsincer(TimeNow,iSrcVec,NumSrc,iStnVec,NumStn)
C Calculate time since rise, itsincer, for each source on each baseline.
C 930722 nrv Created

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

C Input:
!      integer now ! time now, in minutes past 0 UT
      Double Precision TimeNow  !Time of obs. Includes
      integer iStnVec(max_stn)
      integer NumStn,NumSrc
      integer iSrcVec(max_sor)
C Returned:

C Called by: NEXTC

C functions:
      integer ibnum     		!compute baseline number
      integer julda

C Local:
      integer MJDBeg,MJDNow
      integer iTimeMiNumStart
      integer iTimeMinNow

      Integer iDaysSinceStart

      integer iBlSeg(2),iExpSeg(2,2),iSegOut(2)
      Integer NumExpSeg
      integer iRSCnt,iExpCnt
      integer ioverlap

      integer iSrcCnt,isrc
      integer istat1Cnt,istat1
      integer istat2Cnt,istat2
      Integer iBL

      MJDNow=int(TimeNow)
      iTimeMinNow=(TimeNow-MJDNow)*1440.        !Time in minutes

      MJDBeg = JULDA(1,IDA_start,IYR_start-1900)
      iTimeMiNumStart=ihr_start*60+imin_start               !Time in Minutes

      if(iTimeMinNow .gt. iTimeMiNumStart) then  	! case |       [----]  |
         NumExpSeg=1
         iExpSeg(1,1)=iTimeMiNumStart
         iExpSeg(2,1)=iTimeMinNow
      else                                	! case |--]    [-------|
         NumExpSeg=2
         iExpSeg(1,1)=iTimeMiNumStart             !segment1
         iExpSeg(2,1)=1440
         iExpSeg(1,2)=0      	         	!segment2
         iExpSeg(2,2)=iTimeMinNow
      endif

      iDaysSinceStart=MJDNow-MJDBeg+(iTimeMinNow-iTimeMiNumStart)/1440

      do iSrcCnt=1,NumSrc
        isrc=iSrcVec(iSrcCnt)
        do istat1Cnt=1,NumStn-1
          istat1=iStnVec(istat1Cnt)
          do istat2Cnt=istat1+1,NumStn
            istat2=iStnVec(istat2Cnt)
            iBL=ibnum(istat1,istat2)
            itsincer(isrc,ibl)=iDaysSinceStart*itimeup(isrc,ibl)

            do iRSCnt=1,Ntrisset(isrc,iBL)
              iBlSeg(1)=itris(isrc,iBL,iRSCnt)
              iBlSeg(2)=itset(isrc,iBL,iRSCnt)
              do iExpCnt=1,NumExpSeg
                call FindSegOverLap(iExpSeg(1,iExpCnt),iBlSeg,iSegout)
                ioverlap=Isegout(2)-isegout(1)
                if(ioverlap .gt. 0) then
                  itsincer(isrc,iBL)=itsincer(isrc,iBL)+ioverlap
                endif
              end do  !iExpCNT
            end do   !iRSCnt
          enddo  !iStat2Cnt
        enddo   !iStat1Cnt
      enddo  !iSrcCnt

      return
      end

