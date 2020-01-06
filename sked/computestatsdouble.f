      subroutine ComputeStatsDouble(Num,rvec,rAvg,rMin,rMax,rStd)
! compute statistics for some indexed vector.
!
! On entry
      implicit none
      integer num      	!number of references.
      Double Precision rvec(*)  !Data vector
! on Exit
      Double Precision rAvg,rMin,rMax,rSTD      !Avg,Min, Max and standard deviation.
! Local
      integer isum
      Double precision TmpSum,TmpSqSum
      Double precision rtemp
      Integer i

! Initialize.
      rtemp=rvec(1)
      rmin=rtemp
      rmax=rtemp
      isum=1
      TmpSum=rtemp
      TmpSqSum=rtemp*rtemp

      do i=2,Num
        rtemp=rvec(i)
        isum=isum+1
        rmin=min(rmin,rtemp)
        rmax=max(rmax,rtemp)
        TmpSum=TmpSum+rtemp
        TmpSqSum=TmpSqSum+rtemp*rtemp
      end do
      if(isum .ge. 1) then
        rAvg=TmpSum/dble(isum)
        TmpSqSum=TmpSqSum/dble(isum)
        rStd=sqrt(abs(TmpSqSum-rAvg*rAvg))
      else
        rAvg=0
        rstd=0
        rmin=0
        rmax=0
      endif
      return
      end

