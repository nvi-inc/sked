      SUBROUTINE SUMCM(LINSTQ)
      implicit none 
C
C  SUMCM produces a summary of the schedule
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
C
C  INPUT VARIABLES:
      integer*2 LINSTQ(*) ! - input string with DTR, word 1=length

! functions
      integer ibnum ! functions
      real speed    ! function
      integer sonum ! function
      integer trimlen
      integer*4 isecdif

C
C  OUTPUT VARIABLES:
C
C Called by: FSKED
C Calls: GTRUN, GTOBS, GTPRE,
C
C LOCAL VARIABLES
      integer*4 npairs
      integer itime_rec    !recording time for a scan
      integer look ! set to 0 for slewt call
      real POBS(MAX_STN),PCAL(MAX_STN),PSLW(MAX_STN)
C               - percentages for time slewing, observing, cal.
      real apobs,apcal,apslw,apidle
      integer ianh,iastcnt,ibst,iadobs
  
      real SITOBS(MAX_STN),SITCAL(MAX_STN),TSLW(MAX_STN),dobs(max_stn)
      real sitobb(max_stn),tottapes
      double precision adgb,adgBM5
      double precision dmax_store_gBM5   !Maximum storage
      real dgBM5(max_stn)                  !Mark5 Gigabytes recorded
C               - total time observing, cal., slewing
      integer ioff
      integer NSPRE(MAX_STN)
      character*2 cwrap_pre(MAX_STN)
      character*2 ctemp
      real PIDLE(MAX_STN)
      integer iscan_per_stat(MAX_STN)
      integer iobs_per_Stat(Max_stn)    
        
   
      integer IDTCUR(MAX_STN)
C               - times by station
      integer ilin_wid
      integer iper_hour
C               - output time line for source observations
      integer IDIRPR(MAX_STN),IPASPR(MAX_STN),IFTPRE(MAX_STN)
C               - holds pre direction, pass & feet by stn for tape counting
      integer IDURPR(MAX_STN),ICODPR(MAX_STN),itupr(max_stn)
      integer iftend(max_stn)
      integer IUTRIS(MAX_STN),IUTSET(MAX_STN)
      double precision UT1,UT2,UTOFF,UTEF
C               - starting, stopping UTs
      real elhist(18,max_stn+1) ! el histogram
      integer nbins     ! dimension of this array
      integer ielhstx(10,max_stn+1) ! el histogram
      integer nbinsx
      real elhistlo(11) ! histogram for low elevations
      integer ibinlo,ik
      integer iklo ! number of stations with low-el observations
      real covhist(20,max_stn+1) ! coverage histogram
      integer ncobins     ! dimension of this array
      real skycov(max_obs,2,max_stn) ! all az-el for schedule
      real az1,az2,el1,el2,sin1,sin2,cos1,cos2,aa,dista,cosd
      integer idista(18),i1,i2
      real rdista(18)
      integer isnhist(18,max_band,max_baseline+1) ! SNR histogram
      integer nsnr(max_band)
      double precision sumsnr(max_band),avgsnr
      double precision sumsnr_by_bl(max_baseline,max_band)
      integer iband(max_band),nba,iba !band indices
      LOGICAL KSTART,KRWND,KGOT,KNEWT
C               - for GTOBS
      LOGICAL KBIT,KEX
      integer iscan_per_src(MAX_SOR),iobs_per_src(max_sor)

      integer ibcount(max_sor,max_baseline),ibsum(max_baseline)
C      - number of obs by source
      integer ibnumob(max_stn) ! number of n-station obs
! maximum is every 6 minutes=10*24=240.  240/16 bits/word=15
      integer iline_sor(15,MAX_SOR) ! one bit per source per time unit 16*6=96
      integer iline_stn(15,Max_stn) ! and one for station
      integer iline_stn2(240,Max_stn)  !one cell for each time unit
      character*2 ctype
      LOGICAL KSK
C      - true if one of the required subnet stations is in this obs
      LOGICAL KBASE
C      - true if 1st baseline station is in this obs.
      integer IBSELN(MAX_BASELINE)
C      - #obs per baseline counters
C     xmin,xmax,ymin,ymax - optional plotting limits
C     NST - number of stations requested
      integer IST(MAX_STN)
C      - list of stations requested
      logical kskp
C      - true if this station's observation is to be plotted
      logical kup
C      - true if source is up, from CVPOS
      integer nch  !variable and function for variable length
      integer i,j,k,ierr,j1,mjd1,mjd2,ij,is,js,ib,iut,ibl
      integer iut2(max_stn)
      integer ibin,ic,ii,jj,kk,nst,istim,iftold,iscan_tot,blnk
      integer iobs_time_tot           !integrated obs time
      integer nazseg,nelseg,iazbin,ielbin,iijj,iij,imaxrun
      integer ilin,inum,kj,iobs_tot,imoff,ks,ittime,issum,isssum
      integer mutris,mutset
      real rExpDur       !length of experiment
      real tslew,trise,eld,azd,uth,tot,tot3,tot5,tot7
      real totst,tots2,astrms,atotst
      integer iptot3,iptot5,iptot7,iptot
      real xmax,ymax,xmin,ymin,az,el,ha,dec,x30,y30,x85,y85,xd,yd
      integer luplt(max_stn) !one data file for each station
      character*128 cplnam
      character*2 cnum
      logical kvis ! false (true for muvis calls)
      integer nvarn,navg,nsdmax,njdmax,nkdmax,ndnob
      real coavg,corms,cotot,coto2,acotot,acoto2,acoavg,acorms
      integer totne,totse,totsw,totnw,totup
      double precision busum,bxsum,bysum,blsum
      double precision varn,dnob,vavg,davgr,davgs,dvar,dmaxnob,dminnob
      real*8 dnobs(max_sor),snrtot,snrtot2
      real uniform5(18),uniform10(18)
      integer islew_info        !info about slewing
      integer num_tracks(max_stn),num_mk5_tracks(max_stn)
      integer num_trk_code,num_mk5_trk_code
      integer ipow2
      integer icode
      integer iwid
      integer idurhist(-1:21), timeSpan(-1:21)
      integer curIndex, time, start, actMinScn, actMaxScn
      integer binsize, bins, evenBins
      real*8 iobs_avg
      real az_now,az_new 
      real el_now,el_new
      
      real*8 temp,temp5
            
      character*36 csymbol/"123456789abcdefghijklmnopqrstuvwxyz"/
      
C  INITIALIZED
      DATA ILIN/MAX_BASELINE/,istim/0/
      data uniform5 /1.6,4.5,6.8,8.6,9.8,10.4,10.5,10.1,9.2,8.1,6.7,
     >   5.3,3.8,2.5,1.4,0.6,0.1,0.0/
      data uniform10/1.7,4.9,7.4,9.3,10.5,11.0,10.9,10.3,9.2,7.9,6.3,
     >   4.7,3.2,1.8,0.8,0.1,0.0,0.0/
   
          
!      
C
! Updates, most recent first.
! 2021-02-19 JMG Slewt now returns az_now, az_new
! 2021-01-14 JMG Removed calculation of storage space left over from tape. (included parity bit.
! 2020-06-08 JMG  Include broadband.ftni 
! 2019-05-23 JMG  Was not printing out stuff for SNR. NBA=number of bands was being set to 0. 
! 2017-12-19 JMG  sumcm.f:  Do test on writing precision based on MAXimum amount per station rather than average. 
! 2017-12-05 JMG sumcm.f:  When priting out storage in TB, give 2 decimals.  
! 2017-02-14 JMG. For combatibility with gfortran, got rid of some tabs.
! 2015-03-12 JMG. Removed some stuff associated with tapes. 
! Previous updates 
C    811125  MAH    BASELINE SUMMARY ADDED AT THE END.
C    831117  WEH    FIXED SOURCE PLOT FOR UTCUR(J) NOT UTCUR
C    840813  MWH    Added printer LU lock, header for listing,and
C                   # of obs for indiv stations to matrix display
C    841111  MWH    Added expanded summary display
C    880314  NRV    DE-COMPC'D
C ** 880320  NRV    HARD-CODED FORMAT STATEMENTS WHICH FORMERLY
C                   HAD PARAMETERS E.G. MAX_STNA1.
C    880121  NRV    Moved EX parameter to IGTKY
C    890518  NRV    Added total number of obs at end
C    890531  NRV    Cleaned up format of code.
C                   Added section to calculate az,el for plots
C    890612  NRV    Removed command line parsing to SUMPR
C                   Added windows common statement and code to
C                   fill in values.
C    890711  NRV    Added total # obs by source to LINE display
C    890720  NRV    Check plot limits before sending to X
C    900327  NRV    Added lookah to SLEWT call
C    900425  NRV    Added BASELINE summary option
C    910619  NRV    Added trise to SLEWT call
C                   Changed to stats-only as default display
C                   Added # n-station obs display
C    910620  NRV    Added el histogram display
C    910712  NRV    Added SNR histogram
C    910924  NRV    Add mjd,ut to SNR call
C    930225  nrv    implicit none
C    930325  nrv    Fixed accumulation of observing time statistics to
C                   add in the correct station's time. Previously had
C                   always added the duration for station 1.
C    930429  nrv    Initialize UTOFF to 0. This is a non-implemented option
C                   to offset the LINE summary display.
C    930430  nrv    Write out data file for plotting, call pc8 to plot
C    930616  nrv    Modify sumpl call to add kvis argument
C    931005  nrv    Add coverage option
C    931020  nrv    Add a line to the elevation histogram with numbers
C                   of observations in each bin
C    931021  nrv    Add itsris to SLEWT call
C    931108  nrv    Corrected low-elevation observation histogram. Bin values
C                   by truncating, e.g. obs between 3.1 and 3.9 all below in
C                   the bin of el=3 to 4. Previously had been rounding.
C    931109  nrv    change itsris to tsris for double precision
C    931112  nrv    Add st0,frac to slewt call
C    940208  nrv    Set lookahead to 0 for slewt call
C    940209  nrv    Normalize sky coverage display by total number of pairs.
C    940216  nrv    Add random sky distribution values as a gauge.
C    940218  nrv    Add sky distribution histogram plots.
C    940513  nrv    Set cable to "V" for special VLBA slewing algorithm.
C    950404  nrv    Change to using position ID instead of 1-letter code.
C 951018 nrv Remove holleriths
C 951116 nrv Add station index to SPEED call
C 960715 nrv Add list of average baseline components at end
C 960723 nrv Change formatting to write out average number of scans/hour
C            with one decimal point. Fix calculation of fractional number
c            of tapes per station.
C 960923 nrv ITEARL array
C 970328 nrv Add running time for SNR calculations
C 990617 nrv Don't use incremented baseline number but calculate it.
C 000124 nrv Enlarge format 9390 to print a full line!
C 020103 nrv List tape change times
C 020620 nrv Accumulate tape change times for first max_change_list.
C 020815 nrv Calculate GBytes recorded. Re-arrange lines of summary.
C 020917 nrv Calculate number of 8-pk needed.
C 2004Feb2 JMG. Removed calculation of 8-packs, since this size is no longer uniform.
! 2004Nov2 JMGipson. RD0408, RD0409 etc used stations with different codes.
!            Modified to use the appropriate code for the appropriate stations.
! 2006Sep19  JMGipson. Modified so that can output more scans, more observations in summary statistics.
! 2007Jan29 JMG. calculation of amount of data recorded only used 1st recorded mode. Changed
!           so that it calculates correct amount if many kinds of recording modes.
! 2007Feb10 JMG. Fixed printout for big networks.
! 2007Mar07 JMg. Replaced all lpocod by cpocod.
! 2009Apr29 JMG. Fixed bug in skycoverage histograms. 
! 2009Nov05 JMG. changed size of character array
! 2009Dec29 JMG. Minor format change required because of change in compiler.
! 2015Mar12 JMG. Removed some stuff associated with tapes. 
! 2017Feb14 JMG. For combatibility with gfortran, got rid of some tabs.
! 2017Dec05 sumcm.f:  When priting out storage in TB, give 2 decimals.  
! 2017Dec19 sumcm.f:  Do test on writing precision based on MAXimum amount per station rather than average. 
! 2019May23 JMG.  Was not printing out stuff for SNR. NBA=number of bands was being set to 0. 
! 2020Jun08 JMG.  Include broadband.ftni 
C
C   1. Parse command line
C
      call sumpr(linstq,nst,ist,ctype,xmin,xmax,ymin,ymax,ierr)

! AEM comment: probably it is reasonable to do something with these conditions
      kex=.false.
      if (iwscn.gt.79) kex=.true.
      kex=.true.
      if(kex) then
        ilin_wid=96
      else
        ilin_wid=48
      endif
      iper_hour=4
      ilin_wid=iper_hour*24

      if (ierr.ne.0) return
      utoff = 0.d0

      call gtban(icodcur(istcur(1)),nba,iband)  
C
C   2. Initialize arrays, print header line, open plot file.
C
      nch = trimlen(cskfil)
      if (ctype.ne.'EL'.and.ctype.ne.'PO'.and.ctype.ne.'XY'
     .    .and.ctype.ne.'AZ') then
        write(ludsp,"(/3x,'SKED Summary from file ',a,
     >  ' for experiment ',a8)") CSKFIL(1:nch),cEXPER

        write(ludsp,"(a)") 
     >   '      (all scans with at least one subnet station)'      

      end if
      if (ctype.eq.'HI') then
        write(ludsp,'("Elevation histogram")')
      else if (ctype.eq.'CO') then
        write(ludsp,'("Sky coverage histogram")')
      else if (ctype.eq.'SN') then
        write(ludsp,'("SNR histogram")')
      endif
C
      if (ctype.eq.'LI') then
        CALL SUMHD('SOURCE','#SCANS #OBS #Obs/bl',iper_hour,ISTIM,LUDSP)
      else if (ctype.eq.'FI') then !plot file
        open(90,status='unknown',file=cplfil,iostat=ierr)
        if (ierr.ne.0) then
          write(luscn,"('SUMCM - Error ',i5,' opening ',a16,
     .    ' for plot data.')") ierr,cplfil
          return
        endif
      else if (ctype.eq.'BA') then !baseline header
        write(ludsp,"(9x,' ',$)")
          do i=1,nst-1
            is=ist(i)
            do j=i+1,nst
              js=ist(j)
              write(ludsp,"(1x,a2,'-',a2,$)") cpocod(is), cpocod(js)
            enddo
          enddo
          write(ludsp,'(" Total")')
      else if (ctype.eq.'XY'.or.ctype.eq.'EL'.or.ctype.eq.'PO'
     >     .or.ctype.eq.'AZ'.or.ctype.eq.'DI') then ! set up files for plots
        do i=1,nst
          j=ist(i)
          luplt(j) = 100+j
          write(cnum,'(a2)') cpocod(j)
          nch = trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cnum
          open(luplt(j),file=cplnam,status='unknown',iostat=ierr)
          if (ctype.eq.'XY'.or.ctype.eq.'PO')
     >       call horpl(j,xmin,xmax,ymin,ymax,ctype,luplt(j))
        enddo
      endif
C
      KSTART=.TRUE.
      KRWND=.FALSE.
      KBASE=.FALSE.
C
      DO  I=1,NSOURC
        iscan_per_src(I)=0
        iobs_per_src(i)=0
        do k=1,max_baseline
          ibcount(i,k)=0
        enddo
      END DO
      iline_sor=0
      iline_stn=0
      iline_stn2=0

      DO  I=1,MAX_BASELINE
        IBSELN(I)=0
        ibsum(i)=0       
      END DO
      do i=1,11
        elhistlo(i)=0.0
      enddo
      nbins = 18
      nbinsx = 10
      ncobins=20
      nazseg = 4
      nelseg = 5
      nvarn=0
      vavg=0.d0
      navg=0
      varn=0.d0
      DO  I = 1,MAX_STN
        SITOBS(I)=0
        SITOBB(I)=0
        SITCAL(I)=0
        dgbM5(i)=0.d0

! calculate number of tracks for first code
        num_tracks(i)=ntrkn(1,i,1)+ntrkn(2,i,1)
        ipow2=4
        do while(num_tracks(i) .gt. ipow2 .and. ipow2 .le. 64)
          ipow2=ipow2*2
        end do
        num_mk5_tracks(i)=ipow2

        TSLW(I)=0
    
        iscan_per_stat(I)=0
        iobs_per_Stat(i)=0
        IDIRPR(I)=-1
        IPASPR(I)=0
        IFTPRE(I)=0
        NSPRE(I)=0       
        cwrap_pre(i)=" " 
        IDURPR(I)=0
        ICODPR(I)=1
        ibnumob(i)=0
        do j=1,nbinsx
          ielhstx(j,i)=0
        enddo
        do j=1,nbins
          elhist(j,i)=0
        enddo
        do j=1,ncobins
          covhist(j,i)=0
        enddo
      END DO
      do j=1,nbinsx
        ielhstx(j,max_stn+1)=0
      enddo
      do j=1,nbins
        elhist(j,max_stn+1)=0
      enddo
      do j=1,ncobins
        covhist(j,max_stn+1)=0
      enddo
      busum=0.d0
      bxsum=0.d0
      bysum=0.d0
      blsum=0.d0
      do j=1,max_band
        sumsnr(j)=0.d0
        nsnr(j)=0
        do i=1,max_baseline
           sumsnr_by_bl(i,j)=0.d0
        end do
      enddo
      do j=1,nbins
        do i=1,max_baseline+1
          do k=1,max_band
            isnhist(j,k,i)=0
          enddo
        enddo
      enddo
      MJD1=-1
! AEM 20050218 additionally initialize mjd2,ut1,ut2
      MJD2 = 0
      UT1 = 0d0
      UT2 = 0d0
C     tproc= ITAPTM+ISORTM+ITAPTM+IMODTM
!ELW
      iobs_time_tot=0
      idurhist=0
      actMaxScn=maxscn
      actMinScn=minscn
!
      binsize=(maxscn-minscn)
      if (binsize<=20) then
         binsize=1
      else 
         binsize=(binsize/100+1)*5
      end if

      timeSpan=0
      time=minscn-mod(minscn,binsize)
      bins=(maxscn-minscn)/binsize
      do i=0,bins
         timeSpan(i)=time
         time=time+binsize
      end do

      if (maxscn>timeSpan(bins)) then
         bins=bins+1
	 timeSpan(bins)=time
      end if

!End ELW
C
C
C    4.  Get each scans and accumulate it if the source/station
C     list is satisfied.
C
      CALL GTOBS(KSTART,KRWND,KGOT,IERRCM)
      DO WHILE (KGOT) !main loop getting observations
        IF  (IERRCM.NE.0) THEN
          CALL WRERR(IERRCM,INUMCM)
        END IF
        J1 = ISTCUR(1)
        K = NSORcur(J1)
        KSK=.FALSE.  
 
        DO  I=1,NST !check stations
          DO  II=1,nstncur
            IF (IST(I).EQ.ISTCUR(II)) KSK=.TRUE.
          END DO
        END DO  !check stations       
        
        IF (ISORCM.NE.0.AND.K.NE.ISORCM) KSK=.FALSE.
C
C   5. This scan is to be included.
C      Count observations on each of the baselines.
C      Mark the scans in the source line display,
C   or write out the data to be plotted.
C   or fill in windows common block.
C
        IF  (KSK) THEN !include this one
          iklo=0 !initialize count for any stations with low elevations this obs
          ibinlo=0
          IC=0
          DO  I=1,NST-1 !count baselines
            DO  II=1,nstncur
              IF (IST(I).EQ.ISTCUR(II)) KBASE=.TRUE.
            END DO
            IF  (KBASE) THEN
              DO  IJ=I+1,NST
C               Don't just increment the count but get the baseline number
C               IC=IC+1
                ic=ibnum(ist(i),ist(ij))
                DO  II=1,nstncur
                  IF(IST(IJ).EQ.ISTCUR(II)) then !increment
                    IBSELN(IC)=IBSELN(IC)+1
C                   iobs_per_src(k)=iobs_per_src(k)+1
                  endif !increment
                END DO
              END DO
              KBASE=.FALSE.
            ELSE
              IC=IC+NST-I
            ENDIF
          END DO  !count baselines
          UTEF = UTCUR(J1)-UTOFF
          IF (UTEF.LT.0.D0) UTEF = UTEF+86400.D0
! AEM undo          iut=1+(UTEF/(3600/iper_hour))
          iut=1+(UTEF/(3600.d0/dble(iper_hour)))
          do i=1,nstncur
            is=istcur(i)
            UTEF = UTCUR(J1)-UTOFF+idurcur(is)
            IF (UTEF.LT.0.D0) UTEF = UTEF+86400.D0
! AEM undo            iut2(is)=1+(UTEF/(3600/iper_hour))
            iut2(is)=1+(UTEF/(3600.d0/dble(iper_hour)))
          end do
! EMMA: Here.
      do i=1,nstncur
         is=istcur(i)
         do j=i+1,nstncur
            js=istcur(j)
            iobs_time_tot = iobs_time_tot + min(idurcur(is),idurcur(js))
               if (min(idurcur(is), idurcur(js)).ne.0) then
                  curIndex=(min(idurcur(is), idurcur(js)))
		  curindex=(curindex-(minscn-mod(minscn,binsize))-1)/binsize
		  if(curindex<0) then
		     curIndex=-1
		     if(min(idurcur(is), idurcur(js))<actMinScn) then
		        actMinScn=min(idurcur(is), idurcur(js))
		     end if
		  end if
		  if(curindex>=bins) then
		     curIndex=bins
		     if(min(idurcur(is), idurcur(js))>actMaxScn) then
		        actMaxScn=min(idurcur(is), idurcur(js))
		     end if
		  end if
                  idurHist(curIndex) = idurHist(curIndex)+1
               end if
         end do
      end do
!End EMMA
!          IF  (kex) THEN
!            IUT = 1+UTEF/900.D0
!          ELSE
!            IUT = 1+UTEF/1800.D0
!          ENDIF
          iscan_per_src(K)=iscan_per_src(K)+1
          DO  I = 1,nstncur
            is=istcur(i)
            iscan_per_stat(is) = iscan_per_stat(IS)+1
            iobs_per_Stat(is)=iobs_per_stat(is)+(nstncur-1)
          END DO
C       Increment count of observations by baseline
          do i=1,nstncur-1
            do j=i+1,nstncur
              is=istcur(i)
              js=istcur(j)
              ib = ibnum(is,js)
              busum = busum + dsqrt(bx(ib)*bx(ib)+by(ib)*by(ib))
              bxsum = bxsum + dsqrt(bx(ib)*bx(ib)+bz(ib)*bz(ib))
              bysum = bysum + dsqrt(by(ib)*by(ib)+bz(ib)*bz(ib))
              blsum = blsum + baselen(ib)
              ibcount(k,ib) = ibcount(k,ib)+1
            enddo
          enddo
          iobs_per_src(k)=iobs_per_src(k)+(nstncur*(nstncur-1))/2
          ibnumob(nstncur)=ibnumob(nstncur)+1

C       Main if-test for summary options starts here:   
        if (ctype.eq.'LI') THEN !mark this scan on the appropriate source line
          CALL SBIT(iline_sor(1,K),IUT,1)
! set the bits for the station.
          do i=1,nstncur
            is=istcur(i)
            do j=iut,iut2(is)
              call sbit(iline_stn(1,is),j,1)
            end do
            iline_stn2(iut,is)=iline_stn2(iut,is)+1
          end do

        else if (ctype.eq.'BA') then
! AEM comment: should there be something?
        else if (ctype.eq.'SN') then
          if (.not.krsini) call rsini
          do i=1,nstncur
            j=istcur(i)
            idurst(j)=idurcur(j)
          enddo
          j=istcur(1)
          call snrac(nstncur,istcur,nsorcur(j),icodcur(j),-1,mjdcur(j),
     .    utcur(j),ierr)   
          do ib=1,nba !bands
            iba=iband(ib)
            do i=1,nstncur-1
              do j=i+1,nstncur
                is=istcur(i)
                js=istcur(j)
                ibl = ibnum(is,js)
                sumsnr(iba)=sumsnr(iba)+iactbl(iba,ibl)
                sumsnr_by_bl(ibl,iba)=sumsnr_by_bl(ibl,iba)+
     >                        iactbl(iba,ibl) 
                nsnr(iba)=nsnr(iba)+1
                ibin = min(nbins,iactbl(iba,ibl)/5 + 1) 
                isnhist(ibin,iba,ibl) = isnhist(ibin,iba,ibl)+1
              enddo
            enddo
          enddo !bands
        else if (ctype.eq.'AZ'.or.ctype.eq.'EL'.or.ctype.eq.'XY' .or.
     >           ctype.eq.'PO'.or.ctype.eq.'FI'.or.ctype.eq.'HI' .or.
     >           ctype.eq.'CO'.or.ctype.eq.'DI') then
C             write out a line for each station's scans to be plotted
          do i=1,nstncur
            j=istcur(i)
            kskp=.false.
            ij=1
            do while (ij.le.nst.and..not.kskp)
              if (j.eq.ist(ij)) kskp=.true.
              ij=ij+1
            enddo
            if (kskp) then !this station
              call cvpos(k,j,mjdcur(j),utcur(j),az,el,ha,
     .        dec,x30,y30,x85,y85,kup)
              azd=az*rad2deg
              eld=el*rad2deg

              if (ctype.eq.'FI') THEN !plot file
! write to plot file 
                write(90,"(a1,i4,2f7.1,f7.2)") cstcod(j),k,azd,eld,
     .              utcur(j)/3600.d0
              else if (ctype.eq.'HI'.or.ctype.eq.'CO'
     .             .or.ctype.eq.'DI') then !histogram accum.
               
                if (ctype.eq.'HI') then !el hist
                  ibin = eld/5.0 + 1
                  if (ibin.le.0) ibin=1
                  if (ibin.gt.nbins) ibin=nbins
                  elhist(ibin,j) = elhist(ibin,j) + 1
                  if (eld.lt.10.0) then !a low elevation observation
                    ibin = eld + 1
                    if (ibin.le.0) ibin=1
                    if (ibin.gt.nbinsx) ibin=nbinsx
                    ielhstx(ibin,j) = ielhstx(ibin,j) + 1
                    iklo=iklo+1
                    if (iklo.eq.1) then !first station
                      ibinlo=eld+1
                      if (ibinlo.le.0) ibinlo=1
                      if (ibinlo.gt.11) ibinlo=11
                    else !another was low too
                      ibinlo=min(ibinlo,int(eld+1))
                      if (ibinlo.le.0) ibinlo=1
                      if (ibinlo.gt.11) ibinlo=11
                    endif
                  endif
               else !coverage hist
                  iazbin = nint(nazseg*azd)/360.0 + 1
!                  write(*,*) azd, iazbin
!                pause 
                  ielbin = sin(el)*nelseg + 1
                  ibin = (iazbin-1)*nelseg + ielbin
                  if (ibin.le.0) ibin=1
                  if (ibin.gt.ncobins) ibin=ncobins
                  covhist(ibin,j)=covhist(ibin,j)+1
                  skycov(iscan_per_stat(j),1,j)=az
                  skycov(iscan_per_stat(j),2,j)=el
                endif
              else ! plots
                 if (eld.ge.ymin.and.eld.le.ymax) then !within el limits
                  if (ctype.eq.'XY'.or.ctype.eq.'PO') then
                    if (azd.ge.xmin.and.azd.le.xmax) then !within az limits
                      if (ctype.eq.'XY') then
                        write(luplt(j),9607) sonum(k),azd,eld
9607                    format(i3,2f8.3)
                      else !PO
                        call azel2xy(azd,eld,xd,yd)
                        write(luplt(j),9607) sonum(k),xd,yd
                      endif
                    endif !within az limits
                  else ! EL or AZ vs time
                    uth = utcur(j)/3600.d0
                    if (uth.ge.xmin.and.uth.le.xmax) then !within ut limits
                      if (ctype.eq.'EL') then
                        write(luplt(j),9607) sonum(k),uth,eld
                      else
                        write(luplt(j),9607) sonum(k),uth,azd
                      endif
                    endif !within ut limits
                  endif ! EL or AZ vs time
                endif !within el limits
              endif ! plots
            endif !this station
          enddo ! 1,nstncur
          if (ctype.eq.'HI'.and.iklo.gt.0) then !low-elevation obs
            do ik=1,iklo
              elhistlo(ibinlo)=elhistlo(ibinlo)+nstncur-iklo
            enddo
          endif
        endif
C
C
C    6. Accumulate statistics
C
        if (ctype.eq.'LI'.or.ctype.eq.'ST'.or.ctype.eq.'BA') THEN !stats
          if (.not.krsini) call rsini

          IF (MJD1.LE.0) THEN !first scans
            UT1=UTCUR(J1) - ICALcur(J1)
            MJD1=MJDCUR(J1)
          ENDIF !first scans
! Calculate SNR statistics.
          do i=1,nstncur
            j=istcur(i)
            idurst(j)=idurcur(j)
          enddo
          j=istcur(1)
          call snrac(nstncur,istcur,nsorcur(j),icodcur(j),-1,mjdcur(j),
     >      utcur(j),ierr)              
          do ib=1,nba !bands
            iba=iband(ib)
            do i=1,nstncur-1
              do j=i+1,nstncur
                is=istcur(i)
                js=istcur(j)
                ibl = ibnum(is,js)   
                sumsnr_by_bl(ibl,iba)=sumsnr_by_bl(ibl,iba)+
     >                        iactbl(iba,ibl) 
                nsnr(iba)=nsnr(iba)+1 
              enddo
            enddo
          enddo !bands

          UT2=UTCUR(J1) + IDURcur(J1)
          MJD2=MJDCUR(J1)
          DO  IJ = 1,nstncur !add times for this scan
            KJ = ISTCUR(IJ)
            IDTCUR(KJ) = (MJDCUR(KJ)-MJD1)*1440+(UTCUR(KJ)-ICALcur(KJ))/
     .         60.D0
            SITOBS(KJ)=SITOBS(KJ)+IDURcur(kj)

            itime_rec=idurcur(kj)+itearl(kj)*itucur(kj)
            sitobb(kj)=sitobb(kj)+itime_rec

            num_trk_code=ntrkn(1,kj,1)+ntrkn(2,kj,1)
            ipow2=4
            do while(num_trk_code .gt. ipow2 .and. ipow2 .le. 64)
              ipow2=ipow2*2
            end do
            num_mk5_trk_code=ipow2
! Calculate data recorded in Gbytes
            icode=icodcur(kj)
! Here temp is the number of bits/second. 
            if(idata_mbps(kj) .gt. 0) then
              temp=idata_mbps(kj)
            else             
              temp= samprate(kj,icode)*float(num_trk_code)
              temp5=samprate(kj,icode)*float(num_mk5_trk_code)          
            endif            

            dgBM5(kj)=dgBM5(kj)+itime_rec*temp*(1./8.d3)

! End of Gbyte
            if (idurxt(kj).gt.0) then ! more time
              imaxrun=-1
              do iij=1,nstncur
                iijj=istcur(iij)
                if (iijj.ne.kj.and.idurxt(iijj).gt.imaxrun)
     .            imaxrun=idurxt(iijj)
              enddo
              if (imaxrun.gt.0) then
                sitobs(kj)=sitobs(kj)+min(idurxt(kj),imaxrun)
                sitobb(kj)=sitobb(kj)+min(idurxt(kj),imaxrun)
              endif
            endif ! more time
            SITCAL(KJ)=SITCAL(KJ)+ICALcur(kj)
            IF (NSPRE(KJ).EQ.0) NSPRE(KJ) = NSORcur(KJ)
           
            look=0
            ctemp=" " 
            CALL SLEWT(NSPRE(KJ),MJDCUR(KJ),UTCUR(KJ),NSORcur(KJ),KJ,
     >         cwrap_pre(KJ),ctemp,TSLEW,look,trise,tsris,st0cur,frac,
     >         knov,islew_info,az_now,el_now,az_new,el_new)
            if (tslew.gt.0.0) TSLW(KJ) = TSLW(KJ)+TSLEW

C           IDTPRE(KJ) = (MJDCUR(J1)-MJD1)*1440+(UTCUR(J1)+IDURcur(J1))/60.D0       
            IFTPRE(KJ) = IFTCUR(KJ)
            NSPRE(KJ) = NSORcur(KJ)
            cwrap_pre(KJ) = cwrap_cur(KJ)
            IDURPR(KJ) = IDURcur(KJ)
          END DO  !add times for this scan
        endif !stats
C
        END IF  !include this one
C       Save the ending time of this scan
        call GTPRE(nspre,cwrap_pre,icodpr)
        call GTOBS(KSTART,KRWND,KGOT,IERRCM)
      END DO ! main loop getting observations

      iscan_tot = 0
      iobs_tot = 0
      do i=1,nsourc !final accum. for stats
        iscan_tot = iscan_tot + iscan_per_src(I)
        iobs_tot = iobs_tot + iobs_per_src(i)
      enddo !final accum. for stats

C     Calculate ratios of number of observations per hour "up"
        ittime = nint(dble(isecdif(MJD2,UT2,MJD1,UT1))/60.d0)
C                   Compute total time in minutes
! AEM 20050218 init counters njdmax,nkdmax,nsdmax - cause sked crash
        njdmax = 1
      	nkdmax = 1
      	nsdmax = 1
        davgr=0.d0
        davgs=0.d0
        nvarn = 0
        dmaxnob = 0.d0
        dminnob = 9999.d0
        DO  I=1,NSOURC !source loop
          dnobs(i)=0.d0
          ndnob=0
          IF (ISORCM.EQ.0.OR.(ISORCM.GT.0.AND.I.EQ.ISORCM)) THEN !include this source
            do j=1,nst-1
              js=ist(j)
              do k=j+1,nst
                ks=ist(k)
                ib=ibnum(js,ks)
                dnob=0.d0
                if (itimeup(i,ib).gt.0) then
                  dnob=1440.d0*dble(ibcount(i,ib))/dble(itimeup(i,ib))
                  if (dnob.gt.dmaxnob) then
                    dmaxnob = dnob
                    nsdmax = i
                    njdmax = js
                    nkdmax = ks
                  endif
                  if (dnob.lt.dminnob) dminnob = dnob
                  davgr = davgr + dnob
                  davgs = davgs + dnob*dnob
                  nvarn = nvarn+1
                  dnobs(i)=dnobs(i)+dnob
                  ndnob=ndnob+1
                endif
              enddo !k=j+1,nst
            enddo !j=1,nst-1
          endif !include this source
          if (ndnob.gt.0) dnobs(i)=dnobs(i)/dble(ndnob)
        enddo !source loop
C
C
C     5. Now write out each line by source.
C     If one source was mentioned, list that one only.
C
      if (ctype.eq.'LI') then !display
        DO  I=1,NSOURC !display
          IF  (ISORCM.EQ.0.OR.(ISORCM.GT.0.AND.I.EQ.ISORCM)) THEN !show this one
            cline=' '
            DO  J=1,ilin_wid
              IF (KBIT(iline_sor(1,I),J)) cline(j:j)='x'
            END DO
            IF  (kex) THEN !mutual rise/set
              call VISSS(I,NST,IST,IUTRIS,IUTSET,MUTRIS,MUTSET)
              IMOFF = ISTIM*60
              MUTRIS = MUTRIS-IMOFF
              MUTSET = MUTSET-IMOFF
              IF (MUTRIS.LT.0) MUTRIS = MUTRIS+1440
              IF (MUTSET.LT.0) MUTSET = MUTSET+1440
              if(MUTRIS .ne. MUTSET) then
                IUT = 1+MUTRIS/15
                cline(iut:iut)='R'
                IUT = 1+MUTSET/15
                cline(iut:iut)='S'
              endif
!              WRITE(LUDSP,9281) cSORNA(I),LINE,iscan_per_src(I),
!     >          iobs_per_src(i),dnobs(i)
            ELSE !plain
!              WRITE(LUDSP,9280) cSORNA(I),
!     .        (LINE(II),II=1,24),iscan_per_src(I),iobs_per_src(i),dnobs(i)
            END IF  !plain
            write(ludsp,"(1x,a8,'|',a,'| ',i5,1x,i5,1x,f5.1)")cSORNA(I),
     .      cLINE(1:ilin_wid),iscan_per_src(I),iobs_per_src(i),dnobs(i)
          END IF  !show this one
        END DO  !display
        if (kex) then
          write(ludsp,"(' Total scans, obs:',90x,i5,1x,i7,f10.4)")
     .    iscan_tot,iobs_tot
        else
          write(ludsp,"(' Total scans, obs:',42x,i5,1x,i7)")
     .    iscan_tot,iobs_tot
        endif
! Write out station scan info
        write(ludsp, *) ' '
        CALL SUMHD("STATN","#SCANS #OBS %OBS", iper_hour,ISTIM,LUDSP)
        do i=1,nst
          cline=' '
          is=ist(i)
          do j=1,ilin_wid
             IF (KBIT(iline_stn(1,Is),J)) cline(j:j)='x'
          END DO
          if(.false.) then
            do j=1,ilin_wid
             inum=iline_stn2(j,is)
             if(inum .le. 0) then
               cline(j:j)=' '
             else if(inum .ge. 26) then
               cline(j:j)='*'
             else
               cline(j:j)=csymbol(inum:inum)
             endif
             end do
           endif
          write(ludsp,"(1x,a8,'|',a,'| ',i5,1x,i5,1x,f5.1)") cstnna(Is),
     .    cLINE(1:ilin_wid),iscan_per_stat(Is), iobs_per_stat(is),
     .    float(iobs_per_stat(is))/float(iobs_tot)*100.d0
        end do

C
        IF  (ISORCM.GT.0) RETURN
C
      else if (ctype.eq.'PL') then !for plots
        close (90,status='keep')
C
      else if (ctype.eq.'HI') then !histogram
        write(ludsp,"(' Distribution of elevations, for each station'/
     .  4x,'Elev:  0   5  10  15  20  25  30  35  40  45  ',
     .  '50  55  60  65  70  75  80  85  90')")

        do i=1,nst
          j=ist(i)
          write(ludsp,"(a8,': ',18i4)") cstnna(j),
     .	               (nint(elhist(k,j)),k=1,nbins)
          do k=1,nbins
            elhist(k,max_stn+1)=elhist(k,max_stn+1)+elhist(k,j)
          enddo
        enddo
        write(ludsp,"(3x,'Total: ',18i4)")
     .  (nint(elhist(k,max_stn+1)),k=1,nbins)
        tot = 0
        do i=1,nbins
          tot=tot+elhist(i,max_stn+1)
        enddo
        write(ludsp,"('Total number of station scans: ',i5)") nint(tot)

        write(ludsp,"(/4x,'Elev:  0   1   2   3   4   5   6   7   8',
     .  '   9  10')")
        do i=1,nst
          j=ist(i)
          write(ludsp,"(a8,': ',18i4)") cstnna(j),
     >      (ielhstx(k,j),k=1,nbinsx)
          do k=1,nbinsx
            ielhstx(k,max_stn+1)=ielhstx(k,max_stn+1)+ielhstx(k,j)
          enddo
        enddo
        write(ludsp,"(3x,'Total: ',18i4)") 
     >     (ielhstx(k,max_stn+1),k=1,nbinsx)
        tot = 0
        do i=1,nbinsx
          tot=tot+ielhstx(i,max_stn+1)
        enddo
        write(ludsp,"('Total number of station scans: ',i5)") nint(tot)

C       Distribution of low-elevation observations
        tot=0
        do i=1,10
          tot=tot+elhistlo(i)
        enddo
        write(ludsp,"(/'Distribution of observations (one or both ',
     .  'stations are observing at low elevation)'/'Elev:  0    1    2',
     .  '    3    4    5    6    7    8    9   10   Total'/
     .  6x,10i5,4x,i5)") (nint(elhistlo(k)),k=1,10),nint(tot)
        tot3=0
        tot5=0
        tot7=0
        do i=1,7
          tot7=tot7+elhistlo(i)
        enddo
        do i=1,5
          tot5=tot5+elhistlo(i)
        enddo
        do i=1,3
          tot3=tot3+elhistlo(i)
        enddo
        iptot3=100.0*tot3/float(iobs_tot)
        iptot5=100.0*tot5/float(iobs_tot)
        iptot7=100.0*tot7/float(iobs_tot)
        iptot =100.0*tot /float(iobs_tot)
        write(ludsp,"(14x,3(i5,'>>|',2x)/17x,3(i3,'%',6x),14x,i3,'%')")
     >       nint(tot3),nint(tot5),nint(tot7),iptot3,iptot5,iptot7,iptot
C
      else if (ctype.eq.'CO') then !sky coverage
        write(ludsp,"(12x,'NE  SE  SW  NW  UP   Total  Avg   Rms')")
        acotot=0.0
        acoto2=0.0
        do i=1,nst
          j=ist(i)
          totne = 0
          do k=1,4
            totne=totne+covhist(k,j)
          enddo
          totse = 0
          do k=6,9
            totse=totse+covhist(k,j)
          enddo
          totsw = 0
          do k=11,14
            totsw=totsw+covhist(k,j)
          enddo
          totnw = 0
          do k=16,19
            totnw=totnw+covhist(k,j)
          enddo
          totup = 0
          do k=5,20,5
            totup=totup+covhist(k,j)
          enddo
          cotot=totne+totse+totsw+totnw+totup
          acotot=acotot+cotot
          coavg=cotot/5.0
          coto2=totne*totne+totse*totse+totsw*totsw+totnw*totnw+
     .    totup*totup
          acoto2=acoto2+coto2
          corms=sqrt(coto2/5.0-coavg*coavg)
          write(ludsp,"(a8,': ',5i4,2x,3f6.0)") cstnna(j),totne,totse,
     .    totsw,totnw,totup,cotot,coavg,corms
        enddo !station loop
        acoavg=acotot/(nst*5.0)
        acorms=sqrt(acoto2/(nst*5.0)-acoavg*acoavg)
        write(ludsp,"(' Overall',5x,'Avg=',f6.0,'  Rms=',f6.0)") acoavg,
     .  acorms
        write(ludsp,"(/20x,'NE',19x,'SE',19x,'SW',19x,'NW',9x,'Avg Rms'/
     .  '  El bin: ',4(' 0  11  23  36  53  |'))")
        cotot=0.0
        coto2=0.0
        do i=1,nst
          j=ist(i)
          totst=0.0
          tots2=0.0
          do k=1,ncobins
            covhist(k,max_stn+1)=covhist(k,max_stn+1)+covhist(k,j)
            cotot=cotot+covhist(k,j)
            totst=totst+covhist(k,j)
            coto2=coto2+covhist(k,j)*covhist(k,j)
            tots2=tots2+covhist(k,j)*covhist(k,j)
          enddo
          atotst=totst/ncobins
          astrms=sqrt(tots2/ncobins-atotst*atotst)
          write(ludsp,"(a8,': ',4(5i4,'|'),2f4.0)") cstnna(j),
     .    (nint(covhist(k,j)),k=1,ncobins),atotst,astrms
        enddo !station loop

        write(ludsp,"(5x,'Avg: ',4(5i4,'|'))")
     .  (nint(covhist(k,max_stn+1))/nst,k=1,ncobins)
        acoavg=cotot/(ncobins*nst)
        acorms=sqrt(coto2/(nst*ncobins)-acoavg*acoavg)
        write(ludsp,"(' Overall',5x,'Avg=',f6.0,'  Rms=',f6.0)") acoavg,
     .  acorms

      else if (ctype.eq.'DI') then
        write(ludsp,9414)
9414    format(/'Histogram of distances between pairs of observations',
     .  /'Values are percentages of the total number of pairs')
        write(ludsp,9412)
9412    format(/' Degrees:  0   10   20   30   40   50   60   70   80',
     .  '   90  100  110  120  130  140  150  160  170  180  #pairs')
      
        do j=1,nst !station loop
          js=ist(j)
          do i=1,18
            idista(i)=0
          enddo
          do i1=1,iscan_per_stat(js)-1 ! first observation loop
            do i2=i1+1,iscan_per_stat(js) ! second observation loop
              az1=skycov(i1,1,js)
              el1=skycov(i1,2,js)
              az2=skycov(i2,1,js)
              el2=skycov(i2,2,js)
              sin1=sin(el1)
              cos1=cos(el1)
              cos2=cos(el2)
              sin2=sin(el2)
              cosd=cos(az1-az2)
              aa=sin1*sin2+cos1*cos2*cosd
              if(abs(aa).ge.1.0) then
                dista=0.0
              else
                dista=acos(sin1*sin2+cos1*cos2*cosd)*rad2deg
              endif
              ibin=1.5+dista/10.0
              if (ibin.le.0) ibin=1
              if (ibin.gt.18) ibin=18
              idista(ibin)=idista(ibin)+1
            enddo ! second observation loop
          enddo ! first observation loop
          npairs = (iscan_per_stat(js)*(iscan_per_stat(js)-1))/2
          do i=1,18
            rdista(i)=0.5 + 100.0*idista(i)/npairs
             write(luplt(js),'("  1",i8,f10.2)') i*10,rdista(i)
          enddo
          do i=1,18
            write(luplt(js),'("  1",i8,f10.2)') i*10,uniform10(i)
          enddo
          do i=1,18
              write(luplt(js),'("  1",i8,f10.2)') i*10,uniform5(i)
          enddo
          write(ludsp,'(a8,": ",18f5.1,i8)') cstnna(js),
     .	       (rdista(i),i=1,18), npairs
        enddo !station loop
        write(ludsp,'(105("-"))')
        write(ludsp,'("Random5 : ",18f5.1," (5d min. el)")')
     .	     (uniform5(i),i=1,18)
        write(ludsp,'("Random10: ",18f5.1," (10d min. el)")')
     .       (uniform10(i),i=1,18)

      else if (ctype.eq.'SN') then !histogram    
! New 2013Apr12 JMGipson         
        iwid=6
        write(*,*) "Line 11117 ", nba 
        do ib=1,nba
          iba=iband(ib)
          writE(ludsp,'("Average ",A1,"-band SNRs by baseline")') 
     >    lband(iba)      
           write(cbuf,"('  | ',40(' ',a3,'  '))") 
     >            (cpocod(ist(i)),i=1,nst),"AVG"
            
          write(ludsp,'(a)') trim(cbuf)   
        
          do i=1,nst*iwid+9
            cbuf(i:i)="-"
          end do
          write(ludsp,'(a)') cbuf(1:i-1)
          do i=1,nst
            write(cbuf,'(a2,"| ",$)') cpocod(ist(i))
            do j=i+1,nst
              ioff=j*iwid-2
              ibl = ibnum(ist(i),ist(j)) 
              if(ibseln(ibl) .eq. 0) then
                cbuf(ioff:ioff+iwid-1)="   -  "
              else
                write(cbuf(ioff:ioff+iwid-1),'(f6.1)') 
     >             sumsnr_by_bl(ibl,iba)/ibseln(ibl)
              endif                    
            end do
            ibst=0
            avgsnr=0.d0
            do j=1,nst
              if (i.ne.j) then
                ibl=ibnum(ist(i),ist(j))
                ibst=ibst+ibseln(ibl)
                avgsnr=avgsnr+sumsnr_by_bl(ibl,iba)            
              endif 
            enddo
            ioff=(nst+1)*iwid-2
            if(ibst .eq. 0) then 
                cbuf(ioff:ioff+iwid-1)="  -   "
            else
               write(cbuf(ioff:ioff+iwid-1),'(f6.1)') avgsnr/ibst
            endif  
            write(ludsp,'(a)') cbuf(1:ioff+10)
          end do
        end do   
! End new 2013Apr12

        write(*,*) " "

! First off print out the average SNR for each baseline.   
        do ib=1,nba !bands
          iba=iband(ib)
          write(ludsp,'("Distribution of ",A1,"-band SNRs")') lband(iba)
          writE(ludsp,'(" SNR:  ",17i6,"    >>")') (5*j,j=0,16)

          do i=1,18*6+10
            cbuf(i:i)="-"
          end do
          write(ludsp,'(a)') trim(cbuf) 

          do i=1,nst-1
            is=ist(i)
            do j=i+1,nst
              js=ist(j)
              ibl=ibnum(is,js)
              write(ludsp,9503) cpocod(is)//"-"//cpocod(js),
     >           (isnhist(k,iba,ibl),k=1,nbins)
              do k=1,nbins
                isnhist(k,iba,max_baseline+1)=
     >            isnhist(k,iba,max_baseline+1)+isnhist(k,iba,ibl)
              enddo
            enddo
          enddo
          write(ludsp,9503) "Total",
     >         (isnhist(k,iba,max_baseline+1),k=1,nbins)
9503      format(1x,a,":",18i6)

          snrtot = 0.0
          do i=1,nbins
            snrtot=snrtot+isnhist(i,iba,max_baseline+1)
          enddo
          avgsnr=sumsnr(iba)/nsnr(iba)
          i=1
          snrtot2=0.0
          do while (i.le.nbins.and.snrtot2.lt.snrtot/2.0)
            snrtot2=snrtot2+isnhist(i,iba,max_baseline+1)
            i=i+1
          enddo
          write(ludsp,9504) nint(snrtot),avgsnr,(i-1)*5
9504      format(1x,'Total number of obs: 'i5,'  Average SNR: ',f6.1,
     .          '   Median SNR bin: ',i4)
        enddo
      else if (ctype.eq.'BA') then !baseline summary
        isssum=0
        DO  I=1,NSOURC !display
          IF (ISORCM.EQ.0.OR.(ISORCM.GT.0.AND.I.EQ.ISORCM)) THEN !show this one

            write(LUDSP,'(2x,a8,$)') cSORNA(I)
            issum=0
            do j=1,nst-1
              js=ist(j)
              do k=j+1,nst
                ks=ist(k)
                ib=ibnum(js,ks)

                write(ludsp,'(i6,$)') ibcount(i,ib)
                ibsum(ib)=ibsum(ib)+ibcount(i,ib)
                issum=issum+ibcount(i,ib)
              enddo
            enddo
          write(ludsp,'(i6)') issum
          isssum=isssum+issum
          endif
        enddo
        write(ludsp,'("  Total   ",$)')
        do j=1,nst-1
          js=ist(j)
          do k=j+1,nst
            ks=ist(k)
            ib=ibnum(js,ks)
            write(ludsp,'(i6,$)') ibsum(ib)
          enddo
        enddo
        write(ludsp,'(i6)') isssum
      endif !display
C
      if (ctype.eq.'AZ'.or.ctype.eq.'EL'.or.ctype.eq.'XY'.
     .  or.ctype.eq.'PO'.or.ctype.eq.'DI') then !finish the plot
        kvis = .false.
        call sumpl(ctype,nst,ist,xmin,xmax,ymin,ymax,luplt,
     >   iscan_per_stat,kvis)
      endif
C
C   7. Write out statistics.
C
      if (ctype.eq.'LI'.or.ctype.eq.'BA'.or.ctype.eq.'ST') then !statistics display
        davgr = davgr/dble(nvarn)
        dvar = (davgs/dble(nvarn)) - davgr*davgr
C       write(ludsp,9405) davgr,dminnob,dmaxnob,lstcod(njdmax),
        if (njdmax.gt.0) write(ludsp,
     >   "(/' Average number of obs. per baseline per source',
     >    '(normalized by up-time) = ',f5.1,/' Min = ',f6.1,'  Max = ',
     >      f6.1,' (Baseline ',a2,'-',a2,' on ',a8,')   RMS = ', f5.1)")
     >     davgr,dminnob,dmaxnob,cpocod(njdmax),cpocod(nkdmax),
     >      csorna(nsdmax),dsqrt(dvar)
        rExpDur=float(ittime)/60.0

        write(ludsp,"(/' Total time: ',i10,' minutes (',f6.1,
     >  ' hours).'/)") ITTIME,rexpDur
        write(ludsp,'(" Key:  ",$)')
        DO  I = 1,NST
          J = IST(I)
          write(ludsp,"('   ',a,'=',a,$)") cpocod(j),cantna(j)
          if (mod(i,5).eq.0) write(ludsp,'(/,6x," ",$)')
          POBS(J)=100.0*(SITOBS(J)/60.0/float(ITTIME))
          dobs(j)=0
          if (iscan_per_stat(j).gt.0)
     >        dobs(j)=sitobs(j)/iscan_per_stat(j)
          PCAL(J)=100.0*(SITCAL(J)/60.0/float(ITTIME))
          PSLW(J)=100.0*(TSLW(J)/60.0/float(ITTIME))
          PIDLE(J)=100.0-POBS(J)-PCAL(J)-PSLW(J)
        END DO
        write(ludsp,"(/18x,64(a3,'  '))") (cpocod(IST(I)),I=1,NST),
     >       "Avg"
!
        apobs=0.0
        apcal=0.0
        apslw=0.0
        apidle=0.0
        iastcnt=0
        ianh=0
        iadobs=0
        adgB=0.d0
        adgbM5=0.0d0
        tottapes=0.0
        dmax_store_gBM5 =0.d0

        do i=1,nst ! calculate fractions
          j=ist(i)
          apobs=apobs+pobs(j)
          apcal=apcal+pcal(j)
          apslw=apslw+pslw(j)
          apidle=apidle+pidle(j)
          iastcnt=iastcnt+iscan_per_stat(j)
          ianh=ianh+iscan_per_stat(j)*rExpDur
          iadobs=iadobs+dobs(j)
    
          adgBM5=adgBM5+dgBM5(j)
          dmax_store_gBM5=max(dmax_store_gbM5,dgBM5(j))    
        enddo ! calculate fractions

        WRITE(LUDSP,"(' % obs. time:   ',41(1X,I4))")
     >    (nint(POBS(IST(I))),I=1,NST), nint(apobs/float(nst))
        WRITE(LUDSP,"(' % cal. time:   ',41(1X,I4))")
     >    (nint(PCAL(IST(I))),I=1,NST), nint(apcal/float(nst))
        WRITE(LUDSP,"(' % slew time:   ',41(1X,I4))")
     >    (int(PSLW(IST(I))),I=1,NST),  nint(apslw/float(nst))
        WRITE(LUDSP,"(' % idle time:   ',41(1X,I4))")
     >    (int(PIDLE(IST(I))),I=1,NST), nint(apidle/float(nst))

        WRITE(LUDSP,"(' total # scans: ',41(1X,I4))")
     >    (iscan_per_stat(IST(I)),I=1,NST),iastcnt/nst
        write(ludsp,"(' # scans/hour : ',41(1x,i4))")
     >    (nint(float(iscan_per_stat(IST(I)))/rexpDur),I=1,NST),
     >     nint(float(iastcnt)/(float(nst)*rexpDur))

        write(ludsp,"(' Avg scan (sec):',41(1x,i4))")
     >    (nint(dobs(ist(i))),i=1,nst),iadobs/nst


        WRITE(LUDSP,"(' # data tracks: ',41(1X,I4))")
     >    (num_tracks(IST(I)),I=1,NST)
        WRITE(LUDSP,"(' # Mk5 tracks:  ',41(1X,I4))")
     >    (num_mk5_tracks(IST(I)),I=1,NST)

        adgbm5=adgbm5/dble(nst)
        if(dmax_store_gbM5 .ge. 9.95d3) then     !> 10 TB
          write(ludsp,"(' Total TB(M5):  ',41(1x,f4.1))")
     >    (dgBM5(ist(i))/1.d3,i=1,nst), adgBM5/1.d3   
        else if (dmax_store_gbM5 .gt. 9.95d2) then  !>1 TB
          write(ludsp,"(' Total TB(M5):  ',41(1x,f4.2))")
     >    (dgBM5(ist(i))/1.d3,i=1,nst), adgBM5/1.d3       
        else         
         write(ludsp,"(' Total GB(M5):  ',41(1x,i4))")
     >    (nint(dgBM5(ist(i))),i=1,nst), nint(adgBM5)
        endif      

        WRITE(LUDSP,"(/,a)") '      # OF OBSERVATIONS BY BASELINE '
        write(cbuf,"('  | ',40(' ',a2,'  '))") (cpocod(ist(i)),i=1,nst)
        ioff=5+nst*5
        cbuf(ioff:ioff+7)="Total"
        write(ludsp,'(a)') cbuf(1:ioff+7)
        
        do i=1,nst*5+10
          cbuf(i:i)="-"
        end do
        write(ludsp,'(a)') cbuf(1:i-1)
        do i=1,nst
          write(cbuf,'(a2,"| ",$)') cpocod(ist(i))
          do j=i+1,nst
            ioff=j*5-2
            write(cbuf(ioff:ioff+4),'(i5)') ibseln(ibnum(ist(i),ist(j)))
          end do
          ibst=0
          do j=1,nst
            if (i.ne.j) ibst=ibst+ibseln(ibnum(ist(i),ist(j)))
          enddo
          ioff=6+nst*5
          write(cbuf(ioff:ioff+4),'(i5)') ibst
          write(ludsp,'(a)') cbuf(1:ioff+10)
        end do

        write(ludsp,'(/)')
        do i=2,nstatn
          write(ludsp,9396) i,ibnumob(i)
9396      format(' Number of ',i2,'-station scans: ',i4)
        enddo
!ELW
        iobs_avg=1.0*iobs_time_tot/iobs_tot	
	
	if(actMaxScn>timeSpan(bins)) then
	   evenBins=(binsize-mod(actMaxScn,binsize))
	   if(evenBins.ne.binsize) then		
	      actMaxScn=actMaxScn+evenBins
	   end if
	   timeSpan(bins+1)=actMaxScn
	   bins=bins+1
	end if

	if(actMinScn<timeSpan(0)) then
	   if(mod(actMinScn,binsize)>=0) then
	      actMinScn=actMinScn-mod(actMinScn,binsize)			
	   else
	      actMinScn=0
	   end if
	   timeSpan(-1)=actMinScn
	   start=-1
	else
	   start=0
	end if	

        write(ludsp,'("Total number of scans:     ", i8)') iscan_tot
        write(ludsp,'("Total number of obs:       ", i8)') iobs_tot
        write(ludsp,'("Total integrated obs-time: ", i8)') iobs_time_tot
        write(ludsp,'("Average obs-time:          ", f8.1)') iobs_avg
        write(*,*) ""
        write(ludsp,'("Histogram of observation durations")') 
	write(ludsp,'(A$)') "Dur:  "
	do i=start,bins
           write(ludsp,'(" ", i5$)') timeSpan(i)
	end do
        write(ludsp,*) ""
	write(ludsp,'(A$)') "#Obs:     "
	do i=start,bins-1
           write(ludsp,'("|", i5$)') idurhist(i)
	end do
	write(ludsp,'(A)') "|"
	write(ludsp,*) ""

! New 2013Apr12 JMGipson      
       write(*,*) icodcur(1), nba, iband 
        call gtban(icodcur(1),nba,iband) 
        iwid=5
        do ib=1,nba
          iba=iband(ib)
          writE(ludsp,'("Average ",A1,"-band SNRs by baseline")') 
     >    lband(iba)      
           write(cbuf,"('  | ',40(' ',a3,' '))") 
     >            (cpocod(ist(i)),i=1,nst),"AVG"
            
          write(ludsp,'(a)') trim(cbuf)   
        
          do i=1,nst*iwid+9
            cbuf(i:i)="-"
          end do
          write(ludsp,'(a)') cbuf(1:i-1)
          do i=1,nst
            write(cbuf,'(a2,"| ",$)') cpocod(ist(i))
            do j=i+1,nst
              ioff=j*iwid-2
              ibl = ibnum(ist(i),ist(j)) 
              if(ibseln(ibl) .eq. 0) then
                cbuf(ioff:ioff+iwid-1)="   -  "
              else
!              write(cbuf(ioff:ioff+iwid-1),'(f6.1)') 
               write(cbuf(ioff:ioff+iwid-1),'(i5)') 
     >             nint(sumsnr_by_bl(ibl,iba)/ibseln(ibl))
              endif                    
            end do
            ibst=0
            avgsnr=0.d0
            do j=1,nst
              if (i.ne.j) then
                ibl=ibnum(ist(i),ist(j))
                ibst=ibst+ibseln(ibl)
                avgsnr=avgsnr+sumsnr_by_bl(ibl,iba)            
              endif 
            enddo
            ioff=(nst+1)*iwid-2
            if(ibst .eq. 0) then 
                cbuf(ioff:ioff+iwid-1)="  -   "
            else
               write(cbuf(ioff:ioff+iwid-1),'(i5)')nint(avgsnr/ibst)
            endif  
            write(ludsp,'(a)') cbuf(1:ioff+10)
          end do
        end do   
! End new 2013Apr12






C       write(ludsp,9397)
C9397    format(/' Average baseline components for all observations')
C       write(ludsp,9398) busum/(1000.d0*float(iobs_tot)),
C     .  bxsum/(1000.d0*float(iobs_tot)),
C     .  bysum/(1000.d0*float(iobs_tot)),blsum/(float(iobs_tot))
C9398    format('  Average XY     = ',f5.0/
C     .         '  Average XZ     = ',f5.0/
C     .         '  Average YZ     = ',f5.0/
C     .         '  Average length = ',f5.0/)
      endif !statistics display

      RETURN
      END
