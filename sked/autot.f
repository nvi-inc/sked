      SUBROUTINE AUTOT(cmdcod, MinIdle, MJD,UT,nsor,IDUR,idle,ical,
     >  ISTN,NSTN,cwrap,nsornew, 
     >  mjd_beg,ut_beg,mjd_at,ut_at,cwrap_new, istbad)
C     AUTOT automatically calculates a start time for SKED scan. 
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni'

C
C  INPUT:
      
      character*2 cmdcod             !Command code. Indicates who is calling
      integer MinIdle                 !Minimum idle time before printing a message.    
      integer MJD(max_stn)            !time on entry. 
      double precision UT(*)          !UTC part of time.    
      integer NSOR(max_stn)
      integer IDUR(max_stn)           !duration of scan
      integer idle(max_stn)           !idle time after scan
      integer ical                    !calibration time.   Same for all stations.
      integer istn(*)                 !stations in scan 
      integer nstn                    !number of stations.
      character*2  cwrap(*)           !cable wrap of stations, 
      integer nsornew                 !source to go to. 

 ! Output 
      integer mjd_beg                  !starting time of new scan.
      double precision ut_beg          !ending time.      
  
      integer mjd_at(max_stn)          !Time when station arrives on source
      double precision ut_at(max_stn)  !
      character*2 cwrap_new(*)
      integer istbad(max_stn)            !array holding information about bad station.      

! functions
      integer*4 isecdif

C  LOCAL:
      integer*2 IBUF1(60),IBUF2(60)
      character*120 cbuf1,cbuf2
      equivalence (ibuf1,cbuf1),(ibuf2,cbuf2)
      integer itmin
      integer ih1,im1,is1,ih2,im2,is2,nch1,nch2,isp
      integer itdiff,itsec
   

      integer itidl(max_stn) !idle time by station
      integer ib2as,ichmv_ch,i,j,ipk
      logical  kwrite_header 

      integer  isrc_time       !extra time to allow antenna to settle
      integer  ibuf_time       !extra buffer time. 
      integer  isetup_time     !setup time
      real     tslew           !time to slew
      real     aznow, aznew 
      real     elnow, elnew   
      logical kdisplay 
      integer luout           !Argument
      integer ierr 
   
C
C  MODIFICATIONS
C      ** 880310 NRV UN-COMPC'D
C         880404 NRV ADDED CHECK FOR IDLE TIME
C    881227 GAG ADDED NRV's TIME LINE DISPLAY
C         890428 NRV Added mjd_at,ut_at to returned parameters
C    900116 NRV Changed MXFEET to MAXTAP(J)
C    910224 nrv changed calc of "maxslew" to include ITEARL
C               and removed IPKTM (never used)
C    910619 nrv add trise to SLEWT call
C    930219 nrv merge sked/autosked: add ITEARL and new calculations
C    930526 nrv Add +5s if spinning tape and early start, to allow tape
C               to settle.
C    931021 nrv Add itsris to SLEWT call
C    931109 nrv Change itsris to tsris for double precision
C    931112 nrv Add st0, frac to slewt call
C    950405 nrv Change to use 2-letter station codes
C    950523 nrv Add ISTBAD to call.
C 960510 nrv Change ICBL to cwrap and make it i*2!!
C 960923 nrv ITEARL array
C 970221 nrv Move the calculation of setup time to after parity time is set.
C 970224 nrv Add back in the 5 seconds of tape settling time after a spin.
C 970226 nrv Allow 10 sec extra in checking the time
C 970317 nrv For continuous recording, allow time for parity check at the
C            end of a pass, i.e. when this scan starts a new pass.
C 980625 nrv For DYNAMIC or auto-alloc set all parameters to 0
C 000605 nrv Use tape_allocation=AUTO instead of tape_motion_type=DYNAMIC.
C 000611 nrv No setup or parity checks for AUTO allocation.
! 2006Aug08 JMG. Set parity check to 0 for Mark5.
! 2006Sep21 JMG. a) Corrected rounding to modtm.  b) corrected calculation of
!                 next starting time, which was erroneously adding in some
!                 contributions twice. c) Unified calculation of AMAXSL
!                 between continuous, adaptative and stop&start. Only 2 cases:
!                 tape is moving or stationary
! 2006Nov13 JMG. Round up slewt=int(slewt+0.9), amaxsl=int(amaxsl+0.9)
! 2006Nov30 JMG. Changed MARK5A to Mark5A
! 2007Jan24 JMG. Modified so that if slewtime is 0, then time to settle on source is set to 0.
! 2007Feb12 JMG. Removed parameter amaxmax  (not used). Fixed overflow problem in printing amaxsl
! 2008Jun06 JMG. Changed cmdcod "AU" to "SH"  (shift)
! 2008Jun18 JMG. Better timing. In particular, don't allow extra time for first source of an antenna.
! 2008Jun20 JMG. Use isecdif instead of computing difference here
! 2012Oct11 JMG. If previous source is same as current source, skip slew calculation
! 2015Mar18 JMG. Added support for buffer time for Mark6
! 2015Apr23 JMG. Buffer time added to setup. Previously thought this could occur simultaneously with it. 

C
C     1. Step through participating stations and calculate slew time from
C     the previous source.  Add slew to previous stop time to get
C     start time at this station.  Compare start times to get the latest one
C     First find the minimum time between the old and new observations
C     based on tape change and rewind time.
C
!      IF (cmdcod.eq.'CH') IDUR(1) = 0     
      kwrite_header=.true.    
      ut_beg = 0.D0
      mjd_beg = 0
      kdisplay=.false.         !don't output stuff in when_at_next_source
      luout = 0 
      DO  I=1,NSTN !get latest start time
        J = ISTN(I)      
        call when_at_next_source(kdisplay,luout,
     >    j,nsor(j),nsornew,mjd(j),ut(j),
     >    idur(j),idle(j),ical,cwrap(j),cwrap_new(j),
     >    mjd_at(j),ut_at(j),
     >    aznow,elnow,aznew,elnew,tslew, 
     >    isetup_time,isrc_time,ibuf_time,ierr)          
 
        if(i .eq. 1) then
           mjd_beg=mjd_at(j)
           ut_beg=ut_at(j)
        else if(isecdif(mjd_at(j),ut_at(j),mjd_beg,ut_beg).gt.0) then 
           mjd_beg=mjd_at(j)
           ut_beg =ut_at(j)      
        endif     
C
C     Display times (only non-zero ones) added to get new start time
C       KTMLIN = .TRUE.
        IF (KTMLIN) THEN !display time line
          call seconds2hms(ut(j),ih1,im1,is1)
          call seconds2hms(ut_at(j),ih2,im2,is2)

          cbuf1=
     >  ' STN  PREV   Wraps  AzBeg  AzEnd  DUR  TAPE SRC  SLEW '
     >  //'CAL   START   | '
          nch1 = 69        
          if(cwrap(j) .eq. " ") cwrap(j)="- "
          if(cwrap_new(j) .eq. " ") cwrap_new(j)="- " 
 
          WRITE(cbuf2,9121) cpoCOD(J),IH1,IM1,IS1,
     >       cwrap(j),cwrap_new(j),  aznow*rad2deg,aznew*rad2deg,
     >       idur(j),ITAPTM,isrc_time,int(tslew+.99),ICAL,IH2,IM2,IS2 
9121    FORMAT(1X,A2,1x,  2(I2.2,':'),I2.2,2(1x,a2), 2f7.1, 5I5,
     >     1X,2(I2.2,':'),I2.2,1x,"|")
          nch2 = 68
       
          nch1=ichmv_ch(ibuf1,nch1," SLEW -or- ")      
          nch2=nch2+ib2as(int(tslew+0.99),ibuf2,nch2+1,8)   
           
          IF (ISETup_time.NE.0) THEN !setup procedure
            NCH1=ichmv_ch(IBUF1,NCH1,'SETUP')
            NCH2=NCH2+IB2AS(ISETup_time,IBUF2,NCH2+1,6)
          ENDIF !setup procedure      
          if(cstrec(j,1) .eq. "Mark6".or.idata_mbps(j) .gt. 0) then
            NCH1=ichmv_ch(IBUF1,NCH1,'+BUFF_TIME')
            NCH2=NCH2+IB2AS(ibuf_time,IBUF2,NCH2+1,12)
          endif             
              
          IF (ituse(j)*ITEARL(j).NE.0) THEN !early parameter
            NCH1=ichmv_ch(IBUF1,NCH1,'+(EARLY-CAL)')
            NCH2=NCH2+IB2AS(ITEARL(j)-ICAL,IBUF2,NCH2+1,12)
          ENDIF !early parameter

          if(kwrite_header) then
             write(ludsp,'(a)') cbuf1(1:nch1)
             kwrite_header=.false.
          endif 
          write(ludsp,'(a)') cbuf2(1:nch2)
        ENDIF !display time line
      END DO  !get latest start time

      IF (cmdcod.eq.'CH'.or.cmdcod.eq.'UT')  THEN !check
        DO  I = 1,NSTN !check all stns
          J = ISTN(I)
          if(nsor(j) .gt. 0) then        !observerd a source before.
            istbad(j)=0
C         Allow 3-sec slack in checking the time
            itdiff=isecdif(mjd_at(j),ut_at(j),mjdcur(j),utcur(j))
            IF  (itdiff .ge. 3) then
              call sec2minsec(itdiff,itmin,itsec)
              istbad(j)=1
              WRITE(LUDSP,"('ERROR! (autot): Following obs occurs ',
     >            I3,'mins ',I2,'secs too early for station ',A)")
     >           ITMIN,ITSEC,  cstnna(j)//"(="//cpocod(j)//")"   
            ELSE IF (MinIdle.GT.0) THEN  !check for idle time
              itdiff=abs(itdiff)
              IF (itdiff .ge. MinIdle) then
                call sec2minsec(itdiff,itmin,itsec)
                itidl(j)=itdiff
                WRITE(LUDSP,"('WARNING! (autot): Station ',A,
     >          ' idle for ',I3,'mins ',I2,'secs before this obs')")
     >           cstnna(j)//"(="//cpocod(j)//")", ITMIN,ITSEC
              ENDIF !this stn early
            END IF
          endif
        END DO  !check all stns
      END IF  !check
C
      RETURN
      END
C
