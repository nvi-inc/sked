      subroutine MakeStatObsTarget(iSrcVec,NumSrc,iStnVec,NumStn,
     >  PerWantStat,imode)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

! Compute Observation target for sources,
!    i.e., percent of observations we want per source.
! These are vectors which are proportional to:
! functions
      integer ibnum
! Passed
      Integer iSrcVec(*)           !Source vector
      Integer NumSrc            	!Number of sources
      Integer iStnVec(*)        !Station Vector
      Integer NumStn            !Number of stations
      integer imode             !imode=1        ===> PerCent ~ Uptime
                                !imode=2        ===> PerCent ~ Sqrt(Uptime)
                                !imode=3        ===> PerCent ~ Const
! Returned
      Double Precision PerWantStat(Max_Sor)

! Local variables
      integer kr,krr,ku,kuu,kt,ktt,kb

      double precision UpPerStat(max_stn)   !Percent of time a source is up.
      Double precision PerStatSum

!Start of code.
      UpPerStat=0

      do krr=1,NumSrc
        kr=iSrcVec(krr)
        do ktt=1,NumStn-1
          kt=iStnVec(ktt)
          do kuu=ktt+1,NumStn
            ku=iStnVec(kuu)
            kb=ibnum(kt,ku)
            UpPerStat(kt)=UpPerStat(kt)+itsincer(kr,kb)
            UpPerStat(ku)=UpPerStat(ku)+itsincer(kr,kb)
          enddo
        enddo
      enddo

      PerStatSum=0.
      do krr=1,NumStn
        kr=iStnVec(krr)
        if(kr .gt.0) then
          if(imode .eq. 1) then
            PerWantStat(kr)=UpPerStat(kr)
          else if(imode .eq. 2) then
            PerWantStat(kr)=sqrt(UpPerStat(kr))
          else if(imode .eq. 3) then
            PerWantStat(kr)=1.
          else
            stop
          endif
          PerStatSum=PerStatSum+PerWantStat(kr)
        endif
      end do

      PerWantStat=PerWantStat/PerStatSum

      return
      end
