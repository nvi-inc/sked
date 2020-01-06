!***********************************************************************
      subroutine FindSegOverLap(iseg1,iseg2,ioverlap)
! find overlap between two segments.
!        |---Seg1---|
!              |-----Seg2-----|
      integer iseg1(2),iseg2(2),ioverlap(2)
!
      ioverlap(1)=max(iseg1(1),iseg2(1))  !bottom has to be max of the bottoms
      ioverlap(2)=min(iseg1(2),iseg2(2))  !top has to be min of the tops.
! iF we have this case:
!        |---Seg1---|
!                     |-----Seg2-----|
! Set both to 0.
      if(ioverlap(1) .ge. ioverlap(2)) then
         ioverlap(1)=0
         ioverlap(2)=0
      endif
      return
      end
