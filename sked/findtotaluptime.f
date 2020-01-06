      subroutine FindTotalUpTime(istnVec,NumStn,isrcvec,NumSrc,
     > itimeup,Max_sor, Uptime,UpSum)
! Compute the total uptime over some set of stations and sources.
! Passed
      integer isrcvec(*)
      Integer NumSrc
      integer istnVec(*)
      Integer NumStn
! returned
      Double Precision UpTime(Max_sor)
      Double Precision UpSum
      integer itimeup(Max_sor,*)

! local
      integer i
      integer isrc

      integer ktt,kt,kuu,ku,kb

      UpSum=0
      do i=1,Max_Sor
        uptime(i)=0.d0
      end do

      do i=1,NumSrc
        isrc=isrcvec(i)
        do ktt=1,NumStn-1
          kt=istnVec(ktt)
          do kuu=ktt+1,NumStn
            ku=istnVec(kuu)
            kb=ibnum(kt,ku)
            UpTime(isrc) =UpTime(isrc) +itimeup(isrc,kb)
          enddo
        enddo
        UpSum=UpSum+UpTime(isrc)
      enddo
      return
      end


