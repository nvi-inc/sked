      subroutine MakeObsPer(Isor,nsrc,iStnAll,NumAll,
     >     NumObsPerSrc, NumObsPerStat,NumObs)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

! Compute DelPerSrc, DelPerStat
! These are vectors which are proportional to:
!     Ideal # Obs - Current # Obs
! functions
      integer ibnum
! Passed
      Integer isor(*)           !Source vector
      Integer nsrc            !Number of sources
      Integer IstnAll(*)        !Station Vector
      Integer NumAll            !Number of stations
! Returned
      Integer*4 NumObsPerSrc(Max_sor)       !Number of Obs on this source.
      Integer*4 NumObsPerStat(Max_stn)      !Num of times a station was observed.
      Integer*4 NumObs

      integer i
! Local variables
      integer kr,krr,ku,kuu,kt,ktt,kb

!Start of code.
      NumObs=0
      do i=1,Max_sor
        NumObsPerSrc(i)=0
      end do
      do i=1,Max_stn
        NumObsPerStat(i)=0
      end do

      do krr=1,nsrc
        kr=isor(krr)
        do ktt=1,NumAll-1
          kt=iStnAll(ktt)
          do kuu=ktt+1,NumAll
            ku=iStnAll(kuu)
            kb=ibnum(kt,ku)
            NumObsPerSrc(kr)=NumObsPerSrc(kr)+nsorobs(kr,kb)    !sum obs up at this source.
            NumObsPerStat(kt)=NumObsPerStat(kt)+nsorobs(kr,kb)  !and at this end of the baseline
            NumObsPerStat(ku)=NumObsPerStat(ku)+nsorobs(kr,kb)  !...the other
          enddo
        enddo
        NumObs=NumObs+NumObsPerSrc(kr)
      enddo

      NumObsPerStat=NumObsPerStat/2         !this normalizes cause otherwise we count each observation twice.

!Normalize Source
      return
      end
