      subroutine find_obs_per_src(NumObsSrc,NumScansSrc,
     >     NumObsTot,NumScansTot)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

! History
! 2010Mar19. First version. Extracted from atro_obs.f 

! Passed
      Integer*4 NumObsSrc(Max_Sor)      
      integer*4 NumScansSrc(Max_sor)
      integer*4 NumObsTot, NumScansTot

! Find the number of observations and scans per source.

! local
      LOGICAL KSTART,KRWND,KGOT        !for kgot
      integer i
      integer isrc,istat 
      integer*4 NumObs

      nobscm=0
      jdencm=48070        !This is last day we want data for.
                          !Since this is beyond end of data, this means get all data.
! go through all of the obs, finding the number of obs/source
      do i=1,nsourc
        NumObsSrc(i)=0
        NumScansSrc(i)=0
      end do
      NumObsTot=0
      NumScansTot=0

      KSTART=.TRUE.
      KRWND=.FALSE.
      kgot=.true.
      ircur=0
      jdstcm=0            !this keeps from matching on current obs
      DO WHILE (KGOT) !main loop getting observations
        CALL GTOBS(KSTART,KRWND,KGOT,IERRCM)
        if(.not. kgot) return
        istat = ISTCUR(1)
        isrc = NSORcur(istat)
        NumObs=nstncur*(nstncur-1)/2
        NumScansSrc(isrc)=NumScansSrc(isrc)+1
        NumObsSrc(isrc)=NumObsSrc(isrc)+NumObs
        NumObsTot=NumObsTot+NumObs
      end do

      end
