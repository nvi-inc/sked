      subroutine ComputeStatsDoubleXref(Ivec,Num,rvec,rAvg,rMin,rMax,
     >  rStd)
! compute statistics for some indexed vector.
!
! On entry
      implicit none
      Integer ivec(*)   !vector containing references. ivec(i) is index into rvec.
                        ! if ivec(i) is negative, ignore it.
      integer num      	!number of references.
      Double Precision rvec(*)  !Data vector
! on Exit
      Double Precision rAvg,rMin,rMax,rSTD      !Avg,Min, Max and standard deviation.
! Local
      integer isum
      Double precision TmpSum,TmpSqSum
      Double precision rtemp
      Integer iptr,i

      isum=0
      tmpsum=0.
      tmpSqSum=0.

      do i=1,Num
        iptr=ivec(i)
        if(iptr .gt. 0) then
          rtemp=rvec(iptr)
          if(isum .eq. 0) then
            rmin=rtemp
            rmax=rtemp
          endif
          isum=isum+1
          rmin=min(rmin,rtemp)
          rmax=max(rmax,rtemp)
          TmpSum=TmpSum+rtemp
          TmpSqSum=TmpSqSum+rtemp*rtemp
         endif
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

