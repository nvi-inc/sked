      subroutine MakeSrcObsTarget(iSrcVec,NumSrc,iStnVec,NumStn,
     >  PerWantSrc,imode)

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
      Double Precision PerWantSrc(Max_Sor)
! Local variables
      integer kr,krr,ku,kuu,kt,ktt,kb

      double precision UpPerSrc(max_sor)    !Percent of time a source is up from start to now.
      Double precision PerSrcSum

!Start of code.

      UpPerSrc=0

      do krr=1,NumSrc
        kr=iSrcVec(krr)
        do ktt=1,NumStn-1
          kt=iStnVec(ktt)
          do kuu=ktt+1,NumStn
            ku=iStnVec(kuu)
            kb=ibnum(kt,ku)
            UpPerSrc(kr) =UpPerSrc(kr) +itsincer(kr,kb)
          enddo
        enddo
      enddo

      PerSrcSum=0.
      do krr=1,NumSrc
        kr=iSrcVec(krr)
        if(kr .gt. 0) then
          if(imode .eq. 1) then
            PerWantSrc(kr)=UpPerSrc(kr)
          else if(imode .eq. 2) then
            PerWantSrc(kr)=sqrt(UpPerSrc(kr))
          else if(imode .eq. 3) then
            PerWantSrc(kr)=1.
          else
            write(*,*) "MakePerCentWant: invalid mode. Aborting"
            stop
          endif
          PerSrcSum=PerSrcSum+PerWantSrc(kr)
        endif
      end do
      PerWantSrc=PerWantSrc/PerSrcSum

      return
      end
