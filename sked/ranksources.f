      subroutine ranksources(irank_mode,srcrank)
! Subroutine to rank sources by visibility and uptime.
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include 'major.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
        

! passed
      integer irank_mode
! returned variable
      double precision srcrank(Max_sor)

! functions
      double precision hms2seconds
      integer Julda

! local variables
      integer isrcvec(Max_sor)
      integer istnvec(max_stn)
      integer NumUp                             !number up for a given source.
      double precision uptime(max_sor)
      double precision UpSum

      integer MJD_start,MJD_END,MJD             !start of experiment, end, and current MJD
      double precision UT_start,UT_End,UT       !same for UT.

      double precision del_UT                   !step size

      integer iokst(max_stn)                    !returned from SNROK
      integer idur                              !duration of scan

      double precision  az,el,ha
      double precision  DEC,X30,Y30,X85,Y85     !used by cvpos
      logical kup                               !is a source up for a station?
      logical kok                               !Is psuedo scan ok?
      integer nscans_tot        !number of possible psuedo scans
      integer nscans_good       !number successful

      integer lu                !logical unit. set to 0 suppress writing errors to screen.
      integer i,j               !indices
      logical kdisplay          !passed to chksundist
      integer isrc,istat
      integer ierr
      integer icod              ! ????
      double precision del_rank
      logical kwrite

      kwrite=.false.  
      if(kwrite) then
        open(32,file="bestsrc.out")
      endif

      icod=1

! 1.) Calculate rise and set time of all sources. Exclude sources which are not up anywhere.

! Now everything in common.  Need to come up with a ranking of the sources.
! This will be based on uptime and flux density.
      do i=1,nsourc
        isrcvec(i)=i
      end do

      do i=1,nstatn
        istnvec(i)=i
      end do

! only use stations in current subnet.
      istnvec(1:nsubst)=isubst(1:nsubst)
      call FindTotalUpTime(istnVec,nsubst,isrcvec,Nsourc,
     > itimeup,Max_sor, Uptime,UpSum)

! Exclude sources too close to the sun.
      MJD_start=JULDA(1,IDA_start,IYR_start-1900)
      UT_start= hms2seconds(ihr_start,imin_start,isc_start)

      MJD_end=JULDA(1,IDA_end,IYR_end-1900)
      UT_end= hms2seconds(ihr_end,imin_end,isc_end)

      MJD=MJD_start
      UT=(UT_start+UT_END)/2.
      if(MJD_END .eq. MJD_START) then
        continue
      else
        UT=UT+86400.d0*dble(MJD_start-mjd_end)/2.d0
        do while(UT .gt. 86400.d0)
           UT=UT-86400.d0
           MJD=MJD+1
        end do
      endif

!      if(luscn.ne.0) write(luscn,*) "Checking angular distance from sun"
      lu=0         !this suppresses printing in ChkSundist
      do i=1,nsourc
       call ChkSunDist(i,csorna(i),mjd,ut,kdisplay,lu,rSunMinAngle,ierr)
       if(ierr .ne. 0) then
         Uptime(i)=0.
        endif
      end do

      del_ut=600.                           !how often we do psuedo-measurements in seconds.

! General scheme--
!  Try scheduling a series of scans for each source every del_ut.
!  For each scan, calculate   NumBaseLines/Idur    (NumBaselines=Number of observations for this scan).
!  Then ranksrc=Sum  (NumBaselines/idur)

! write something to keep people interested
      if(luscn.ne.0) write(luscn,'(" Ranking sources ")')

      lu=0
! Do sources that are good.
      do isrc=1,nsourc
!       if(luscn.ne.0) write(luscn,'(i4,$)') isrc
        if(luscn .ne. 0) write(luscn,'(1x,a8,$)') csorna(i)
        if(mod(isrc,20) .eq. 0) write(luscn, *) " "
        if(uptime(isrc) .eq. 0) then
          srcrank(isrc)=-1.
        else
          nscans_tot=0
          nscans_good=0
          srcrank(isrc)=0
          MJD=MJD_start
          UT=UT_start
          do while(MJD .ne. MJD_END .or. ut .lt. ut_end)
            NumUp=0
            Nscans_tot=Nscans_tot+1
            do istat=1,nsubst
              CALL CVPOS(isrc,isubst(istat),mjd,Ut,az,el,ha,
     >                       DEC,X30,Y30,X85,Y85,KUP)
              if(kup) then
                NumUp=NumUp+1
                istnvec(NumUp)=istat
               endif
            end do
            kok=.false.
            do while(.not.kok .and. Numup .ge. 2)
              call snrok(istnvec,NumUp,isrc,icod,lu,iokst,mjd,ut)                         
              kok=.true.
              do i=1,NumUp
                j=istnvec(I)
                if (iokst(i).lt.0) then ! some problem
                  istnvec(i)=-iabs(istnvec(i))
                  kok=.false.
                endif
              end do
              if(.not.kok) then
                call destn(NumUp,istnvec)
              endif
            end do   
            if(NumUp .ge.2) then
              Nscans_good=Nscans_good+1
              call snrsk(isscan(isrc),NumUp,istnVec,isrc,icod,ierr,0,
     >           mjd,UT)
              if (ierr.lt.0) then
                 write(*,*) "Hmmm strange error!"
              endif
              idur=0
              do i=1,NumUp
                j=istnvec(i)
                idur=max(idur,idurst(j))
              end do                           
              if(irank_mode .eq. 1) then
                 del_rank= float(NumUP*(NumUp-1))
              else if(irank_mode .eq. 2) then
                 del_rank= 1./float(idur)
              else if(irank_mode .eq. 3) then
                 del_rank= float(NumUP*(NumUp-1))/float(idur)
              endif
              srcrank(isrc)=srcrank(isrc)+del_rank        
            endif
            UT=UT+del_ut
            if(ut .gt. 86400) then
              ut=ut-86400.
              MJD=mjd+1
            endif
          end do
        endif
        if(kwrite) then
          write(32,'(i4,1x,a,1x,f8.2)') isrc,csorna(isrc),srcrank(isrc)
        endif
! if only 1% of the scans are good, mark the source as bad. Probably wouldn't pick up any scans.
!        if(dble(nscans_good)/dble(nscans_tot) .lt. 0.01) srcrank(isrc)=0.
      end do
      if(luscn.ne.0) write(luscn,*) "...done."
      if(kwrite) close(32)
      return
      end









