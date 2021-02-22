       SUBROUTINE NEWOB(cINSTQ,ISTN,NSTN,IERR,nsubc)

!  NEWOB decodes an observation command line, checks to see ifthe observation is feasible.
!  If it is not feasible with original station list, tries removing some of them. 
!  Beginning at 920 we start with original list of stations.
!  If the source is not up at a station, then it is removed.

      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
 

! functions
      logical kstatup                   !is station up for scan?
      logical kcont
      real speed                        ! tape speed
      logical kyes_to_prompt            ! returns true if anser is "Y" or "Yes"
      integer  trimlen
      integer isecdif                   !difference in time between two scans.
 
C  INPUT:
      character*(*) cinstq              ! Input character string
      integer nsubc                     ! 0 means insert.   1-4 mean trial subnet used in autosked mode. 
C
C  OUTPUT VARIABLES:
      integer ISTN(max_stn)             !Final stations in scan.
      integer nstn                      !number of stations
      integer ierr                      ! <>0 some error. 

C  Other commmon blocks used.
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom_vec.ftni'
      include 'major.ftni'
C
C     CALLING SUBROUTINES: NEWCM
C     CALLED SUBROUTINES: ,YMDHM,SIDTM,CVPOS,DESTN,
C                         SNRAC,SNRDS,AUTOT,MUTAP,NEWPR,SLEWT
C                         RSPYN,SNROK,FLUXDS,BASEDS,NEWTRUN
C
C  LOCAL VARIABLES  
      real TSLEW(MAX_STN)            !time allowed for slewing, by station    
      real trise(max_stn) !time till source rises

      LOGICAL KVIS                   !True of mutual visibility is required for subnet
      logical kfirstobs
      logical keep_index             !keep index in copying to scan

      double precision UTT(MAX_STN)  !Trial time (seconds)
      integer MJDT(MAX_STN)           !Trial date 

      integer mjdbeg                ! MJD beginning of new scan
      double precision utbeg       ! UT  beginning of scan
      integer mjdtmp                ! MJD temporary holder
      double precision uttmp       ! UT temporary holder

      character*20 lproblem         ! Description of potentional problem


C     iStat1 - first station in this subnet
C     J - index of station in selected list
C        ICH  - character counter
C        IDURSO,IDLE,ICAL,NSOR,IYR,IDA,IHR,iMIN,ISC
C               - Holders for decoded values

      double precision UT,utcmd      !Holder for decoded UT.
      LOGICAL KOK                     !All stations pass KUP test

      LOGICAL KNENEW(MAX_STN),KRWNEW(MAX_STN) ! true when new tape or rewind is needed, by station

      integer*2 LPRE(3),LMID(3),LPST(3)    !Procedures 
      character*6 cpre,cmid,cpst
      equivalence (cpre,lpre), (cmid,lmid),(cpst,lpst)
      character*2 cmdcod

      integer*2 LCABLE(MAX_STN)
      character*2 cwarp(max_stn)
      equivalence (cwarp,lcable)
      character*2 cwarp_temp  

      CHARACTER*1 CANS           ! NC and CANS are used for user response
      INTEGER NC

      integer iokst(max_stn)     ! returned from SNROK
      integer itersnr            ! 1 for first iteration, add 1 for subsequent      
      integer itemp              ! temporary variablel            
        
      character*80 lerr_msg 
      character*20 lfrnt_msg
      integer istat1             ! first station in subnet.
     
      integer i,nsor,iyrcmd,idaycmd,ihr,imin,isc,mjdcmd
      integer j        !station index (replace istat)
      integer idur,idurso,ical,idle,icod,mjd
      integer lu
      
      integer istbad(max_stn) ! not used here
      logical kdisplay
      integer islew_info         !info about slew
      real  dur_temp
      integer itdif              !timedifference
      real az_now,az_new 
C
C  History
! 2021-02-19 JMG  slew now returns az_now,az_new 
C   811125  MAH    CHECK FOR CONTINUITY OF OBS
C                  NEW MSG FOR TSLEW=-2.0
C   811130  MAH    CALCULATE SLEW TIMES WHEN ST TIME IS SPECIFIED
C   830423  NRV    ADD X,Y TO CVPOS CALLS
C   835024  WEH    ADD DEC TO CVPOS CALLS, LOOP AROUND SUN DISTANCE
C                  CALCULATION WHEN THE SOURCE IS A SATELLITE
C   841009  MWH    INCLUDE ARC DISTANCE TO SUN MESSAGE
C   880310  NRV    DE-COMPC'D
C   880329  NRV    ADDED BLINK/INVERSE TO NEW TAPE MESSAGE
C   890427  NRV  Remove parameter checks to subroutine NEWPR
C   890428  NRV  Calculate SNR, drop stations from subnet if errors,
C                ask for approval before finalizing
C   890505  NRV  Write out time of the new observation
C   890508  NRV  Don't re-set specified start time!
C   890526  NRV  If DUR is specified, use it exactly for all stations
C   891127  NRV  Changed calling of SNRSK, SNRDS
C   891201  gag  added call to rspyn for user response
C   891205  NRV  Added check after SNRSK to drop station if flux was low.
C   891207  NRV  Removed SNR/dur checks to subroutine
C   900118  NRV  Changed check for TSLEW=-3 (rising in an hour)
C   900327  NRV  Changed TSLEW again to allow scheduling a rising source.
C   910224  NRV  Added ITEARL to calculation of ending footage count
C                before the call to MUTAP
C   910227  NRV  Set IRECCU with other CUR variables
C   910619  NRV  Added trise to SLEWT call, display and ask user
C   910924  NRV  Add mjd,ut to SNR call
C   910925  NRV  Add call to FLUXDS,BASEDS
C   920522  NRV  Add call to SEFDDS
C   920615  NRV  NEW VERSION FOR AUTOSKED, COPIED FROM SKED and RE-MODIFIED
C                for AUTOSKED !!!!!!!!!!!!!!!!!!!!!
C   930225  nrv  implicit none
C   930312  nrv  Add nsubc parameter to MUTAP
C   930521  nrv  Add check for auto-select before asking question about
C                waiting for source to rise.
C   931013  nrv  Re-set mjd/ut when iterating SNR calculations
C   931021  nrv  Add itsris to SLEWT call
C   931105  nrv  Use time+dur+slew as starting point for SNR calculations
C   931109  nrv  Change itsris to tsris for double precision,
C                add nrs to ISUP call
C   931110  nrv  Change SIDTM call to use st0cur and st0tst
C   931112  nrv  Add st0,frac to slewt call
C   940112  nrv  Restore CVPOS calls for satellites
C   940224  nrv  If a duration is specified, don't calculate SNRs unless
C                vscan is on. This is the fourth pair of the vscan on/off
C                and duration specified/auto choices.
C   950405  nrv  Use 2-letter station codes
C 951116 nrv Add station index to SPEED call
C 960328 nrv Initialize mjdsnr and utsnr for call to SNRAC
C 960618 nrv Re-initialize iStat1 after deleting stations
C 960923 nrv ITEARL array
C 970317 nrv Changes for continuous tape
C 970401 nrv Initialize idurxt to 0.
C 970408 nrv Move ITERSNR initialization to just before loop.
C 970408 nrv Check if one station is causing problems with cascading
C            waits, elminate it, and start over.
C 970506 nrv Don't calculate utsnr (because it's already done) for
C            the case when duration is specified.
C 970507 nrv Print out which station has a new pass starting, which
C            may account for a long wait with continuous recording.
C 021009 nrv Reject a station's participation in an observation if
C            there is not enough time to SFF and SRW the tape to
C            achieve a complete postpass (=non-stop reverse pass).
C            *** was not implemented **
C 27Aug2003 JMG  If a source is near cable wrap limits, reject it.
! 2004Oct12 JMG  changed input to a string, not a character.
!    2007Feb23  JMGipson.  Replaced call to RSPYN by call to read_cap_char
! AEM 20070320 fix bug (see below in source) - possible confusion with
!              counters 'istat' and 'j' which are identical, therefore 
!              all 'istat' instances were replaced with 'j' (+little cleanup)
! 2008Nov11 JMGipson.  Fixed problem with "START" parameter. Was not using it.
! 2008Nov12 JMGipson.  Fixed bug in format statement when source was about to rise.
! 2008Nov12 JMGipson.  Moved lines at end of routine which write info about scan to separate routine.
! 2008Dec02 JMGipson.  Somewhere in November changes, broke effect of down-time. Restored it. 
! 2009Mar03 JMGipson.  HPUX doesn't know function trim.
! 2009Jun20 JMGipson.  Issue error message if station can't participate because of downtime.
! 2011Feb08 JMGipon.  If previous scan was further than 10 minutes away for an antenna, don't do slewing
!                     calculation.  Assume the antenna can do it!
! 2014May02 JMG. Removed ipas,idir, ift from call to set_scan_param. No longer used. 

      kdisplay=nsubc .eq. 0 .or. kdebug 

C
C   1. Check for enough info to start.  Call NEWPR to parse
C   the input line and return all parameters.
C
      IF  ((NSTATN.EQ.0).OR.(NSOURC.EQ.0).or.ncodes.eq.0) THEN
        IERR = 13
        RETURN
      END IF  !not enough info yet
C  
       CALL NEWPR(cINSTQ,NSOR,iyrcmd,idaycmd,IHR,iMIN,ISC,
     >  utcmd,mjdcmd,IDUR,
     >  IDURSO,ICAL,IDLE,cwarp,ICOD,cPRE,cMID,cPST,NSTN,ISTN,KVIS,IERR)

      IF (IERR.NE.0) RETURN
      
!      write(*,"('NEWOB: ',i3,'-',2(i2.2,':'),i2.2)") idaycmd,ihr,imin,isc
!      write(*,*) mjdcmd

C    Initialize rise/set times if needed.
      if (.not.krsini) call rsini
C    Initialize extra durations to zero for scheduling.
      do i=1,nstatn
        idurxt(i)=0
      enddo

!
! Iterate. First we start with all stations in the command list.
!   We try to schedule them. If there is a problem with some station it is marked as bad. (KOK = .false.)
!   and removed from station list.  
 
      KOK = .TRUE.     
920   Continue  
      if(.not.Kok) then
        if(kdebug) then 
          write(luscn,'(a," problem with following: ",$)') 
     >      lproblem(1:trimlen(lproblem))                          
          do i=1,nstn               
            if(istn(i) .lt. 0) then
               write(luscn,'(" ",i2," ", a, $)') i,cstnna(-istn(i))
            endif
          end do 
          write(luscn,'(a)') 
        endif       
!        write(*,'("Newob 242  ",i4," | ",12i4)')  nstn,istn(1:nstn)
        CALL DESTN(NSTN,ISTN)
    
!        if(nstn .gt. 0) then
!          write(*,'("Newob 246  ",i4," | ",12i4)') nstn,istn(1:nstn)
!        else
!          write(*,*) "Newob 248: All deleted"
!        endif        
        Kok=.true.
      ENDIF  

! We calculate the nominal "begining" of the scan several times below with different
! levels of precision until we converge on the final time.
      iStat1 = ISTN(1) ! first station in requested subnet
      IF (NSORcur(iStat1).GT.0.AND.mjdcmd.NE.-1.AND.
     >    isecdif(mjdcmd,utcmd,mjdcur(istat1),utcur(istat1)).lt.0) then     
        if (kdisplay) WRITE(LUSCN,'(a)')
     >   "WARNING! (newob): Start time is earlier than current time..."        
      endif
        
      IF (NSTN.LT.2) THEN !too few left
        if (kdisplay)
     >     WRITE(LUSCN,'(a)') 
     >     "ERROR! (newob): Less than two stations left. Can't observe."       
        IERR=1
        RETURN
      ENDIF !too few left

      MJD=mjdcmd  !reset MJD to the date specified in the command
      UT=utcmd    !reset UT to the time specified in the command
      
      if (kdisplay) then
        write(luscn,'("Checking new obs on ",a," with ",30(A2,1x))')
     >    cSORNA(NSOR),  (cpoCOD(ISTN(j)),j=1,NSTN)   
      endif

! 2.1 See if source is visible at station at the start of the scan. 
      lproblem="ChkSrcUp4Scan:  start"
! Get first guess for time of scan start. 
      if(mjd .ne. -1) then    
! observation time specified.
         mjdbeg=mjd
         utbeg =ut
      else
! no time specified.  Initialize to current time of first station in scan. 
         mjdbeg=mjdcur(istat1)
         utbeg= utcur(istat1)
         do i=1,nstn
           j=istn(i)
! Calculate when a station becomes free at end of current scan. 
           call addsec2ut(mjdcur(j),utcur(j),
     >         idlcur(j)+idurcur(j),mjdtmp,uttmp)
! Make the time the latest of all times. This is earliest time trial scan can occur. 
           if(isecdif(mjdtmp,uttmp,mjdbeg,utbeg) .gt. 0) then
              mjdbeg=mjdtmp
              utbeg =uttmp
            endif  
         enddo     
     
        DO  I = 1,NSTN !Check source is up at start of scan. If not, kick station out.
            j=istn(i)
            call ChkSrcUp4Scan(j,Nsor,nceles,
     >        csorna(nsor),cstnna(j),mjdbeg,utbeg,
     >        Idurst(j),lcable(j),luscn,kdisplay,ierr)
            if(ierr .ne. 0) then
              Kok=.false.
              istn(i)=-iabs(istn(i))
            endif
         END DO  !make sure we can observe at this time
         if(.not.kok) goto 920            
      endif

   
! OK, all sources are up at the end of the previous scan. Now take into account slewing.      
      lproblem="Slewing "
      DO I=1,NSTN
        j = ISTN(I)
        call addsec2ut(mjdcur(j),utcur(j),
     >         idurcur(j)+idlcur(j),mjdt(j),utt(j))
 
! If the time between 'now' and the end of the last scan is large enough, skip slew calcuation.
! 2012May
         itdif=isecdif(mjdbeg, utbeg,mjdcur(j),utcur(j))  
!        IF  (NSORcur(j).GT.0 .and. itdif .lt. 600) then 
!
!         write(*,*) nsorcur(j)
        if(nsorcur(j) .gt. 0) then       
          CALL SLEWT(NSORcur(j),mjdt(j),utt(j),
     >      NSOR,j,LCBLcur(j),LCABLE(j),TSLEW(j),lookah,trise(j),
     >      tsris,st0cur,frac,knov,islew_info,az_now,az_new) 
!             write(*,*) "JMG!!",  cstnna(j), islew_info, tslew(j) 
          if(islew_info .eq. 0) then
            UTT(j)=UTT(j)+Tslew(j)
          else 
            KOK =.FALSE.
            ISTN(I)=-iabs(ISTN(I))
            if(kdisplay) then
              if(islew_info .lt. 0) then 
                lfrnt_msg='ERROR! (newob): '
              else
                lfrnt_msg='WARNING! (newob): '
              endif  
              call print_slew_info_warning(ludsp,lfrnt_msg,islew_info,j)                  
            endif        
          endif 
        ELSE
          TSLEW(j) = 0.0
          trise(j) = -1.0
          call cabl1(j,nsor,mjdt(j),utt(j),lcable(j))
        ENDIF
      ENDDO
C
      IF (.NOT.KOK) THEN
        IF (KVIS) THEN !mutual vis required
          IERR=1
          RETURN
        ELSE !remove bad stations           
           goto 920         
        ENDIF
      endif
C
C     If the source is about to rise, ask whether to wait for it or just
C     drop the station from the list. If in autosked mode, then "kask"
C     will be true, so drop the station automatically.

      lproblem = "Rising"
      do i=1,nstn
        j=istn(i)
        if (trise(j).gt.0) then !ask if wait for rise
          if (kask.and.kdisplay) then
            nc=-1
            do while (nc.lt.0)
              write(luscn,9412) cstnna(j),trise(j)/60.0
9412          format('NEWOB91 - Source will rise at ',a,' in ',f4.1,
     >         ' minutes.  Do you want to ',/,
     >         '(D)elay the start time until it rises',/,
     >         '(S)kip this station ',/, 
     >         '(A)bandon the observation? ',$)              
              CALL read_cap_char(cans)            
              if (CANS.eq.'A') then
                ierr = 1
                return
              else IF (CANS.EQ.'S') then !remove
                istn(i)=-iabs(istn(i))
                kok=.false.
                nc=0
              else if (cans.ne.'D') then
                nc=0
              endif
            enddo
          else !in auto mode, remove station
            istn(i)=-iabs(istn(i))
            kok=.false.
          endif !auto or not
        endif !ask if wait for rise
      enddo     
        if(.not.kok) then          
          goto 920
        endif 

C   2.2. Calculate dur/SNR for this source and subnet.
C        Check that each station is OK and print messages if not.
C********Return here to re-calculate SNRs using the calculated start time.
C
       itersnr = 1
930   continue 
      lproblem="Duration" 
! Find time scan start. 
! Note:  MJD is either mjdcmd (set above), or is set below as a result of calculation.
      if(mjd .ne. -1) then    
! observation time specified, or from previous iteration.
        mjdbeg=mjd
        utbeg= ut
      else
!no time specified.  Initialize to current time of first station in scan. 
        mjdbeg=mjdcur(istat1)
        utbeg=utcur(istat1)
        do i=1,nstn
          j=istn(i)
          call addsec2ut(mjdcur(j),utcur(j),
     >     idlcur(j)+idurcur(j)+isortm+ical+nint(tslew(j)),mjdtmp,uttmp)
! Make the time the latest of all times. This is when scan occurs.
          if(isecdif(mjdtmp,uttmp,mjdbeg,utbeg) .gt. 0) then
             mjdbeg=mjdtmp
             utbeg =utbeg
          endif  
        enddo
      endif   

! Now calculate the durations.
      IF (IDUR.EQ.0) THEN !calculate durations
        if (kvscan) then !calculate
          lu=luscn
          if (nsubc.gt.0) lu=-1       
          call snrok(istn,nstn,nsor,icod,lu,iokst,mjdbeg,utbeg)             
          do i=1,nstn
            j=istn(i)
            if(iokst(i).lt.0) then ! some problem
              kok=.false. 
              if(kdisplay) then !messages                      
                if(iokst(i).eq.-1) then
                  lerr_msg='Source flux too low on all baselines to'
                else if(iokst(i) .eq. -2) then
                  lerr_msg='SNR too low on baselines to'
                else if(iokst(i) .eq. -3) then
                  lerr_msg='No flux or SEFDs available for'
                else if(iokst(i) .eq. -4) then 
                  lerr_msg='No good baselines to'
                endif               
                writE(luscn,"(' ERROR! (newob): ',a, ' ',a)" ) 
     >               trim(lerr_msg), cstnna(j)

                if(.not.kasnr.and.iokst(i).eq.-2) then !ask anyway
                  if(.not. kyes_to_prompt( 
     >                  "Schedule (Y) or drop this station (N)?")) then
                    istn(i)=-iabs(istn(i))          !response no --> Drop 
                    kok=.false.
                  endif
                else !reject automatically
                  istn(i)=-iabs(istn(i))
                  kok=.false.
                endif !ask/reject
              else !reject automatically
                istn(i)=-iabs(istn(i))
                kok=.false.
              endif !ask/reject
            endif ! some problem
          enddo     
!          write(*,*) "Newob 469: ", kok 
          if (.not.kok) goto 920            !remove bad station
! Don't have to do this because already done in "snrok"
          if(.false.) then 
          call snrsk(isscan(nsor),nstn,istn,nsor,icod,ierr,0,
     >        mjdbeg,utbeg)
          endif 
          if (ierr.lt.0) return         
        else !use scan times
          do i=1,nstn
            idurst(istn(i))=isscan(nsor)
          enddo
        endif !calculate/use scan times
      ELSE !duration specified
        DO I=1,NSTN
          IDURST(ISTN(I))=IDUR
        ENDDO
        lu=luscn
        if (nsubc.gt.0) lu=-1
        if (kvscan) then ! calculate SNRs anyway
          call snrac(nstn,istn,nsor,icod,lu,mjdbeg,utbeg,ierr)
          if (ierr.ne.0) return
        endif
      ENDIF !calc/spec durations
C     3. Determine the mutual tape location for this observation.
!     

C    5. Calculate start time.
C     Reset these variables when iterating so that start time is re-calculated
! 
      MJD=mjdcmd  !reset MJD to the date specified in the command
      UT=utcmd    !reset UT to the time specified in the command
      IF (MJD.EQ.-1) THEN !no start time specified
        kfirstobs=.true.
        do i=1,nstn
           if(nsorcur(istn(i)).ne.0) kfirstobs=.false.
        end do
!        IF (NSORcur(iStat1).NE.0) THEN !we're initialized, use AUTOT
        if(.not. kfirstobs) then !initialized. Use Autot.
          cmdcod=" "   
          itemp=0     !minimum idle time.        
 
          CALL AUTOT(cmdcod,itemp,MJDCUR,UTCUR,NSORcur,IDURcur,IDLCUR,
     >      ical,istn,nstn, cwrap_cur,nsor,
     >       MJD,UT, MJDT,UTT,cwarp,istbad)    
 
        ELSE !not initialized, use starting day
          UT = UTCUR(iStat1)
          MJD  = MJDCUR(iStat1)
          do i=1,nstn
            utt(istn(i))=ut
            mjdt(istn(i))=mjd
          enddo
        END IF !initialized/not initialized        
C
C    Check sun distance and quit if it's too close.
C
        call ChkSunDist(nsor,csorna(nsor),mjd,ut,
     >     kdisplay,luscn,rSunMinAngle,ierr)
        if(ierr .ne. 0) return
     
      END IF !no start time specified
      lproblem="Source_not_up"     
      DO  I = 1,NSTN !Check source is visible for all observations.
        j=istn(i)
        call ChkSrcUp4Scan(j,Nsor,nceles,
     >      csorna(nsor),cstnna(j),MJD,UT,
     >      Idurst(j),lcable(j),luscn,kdisplay,ierr)

        if(ierr .ne. 0) then
          Kok=.false.           
          istn(i)=-iabs(istn(i))
        endif 
      END DO  !make sure we can observe at this time
      if(.not.kok) goto 920

 ! Now we also check to see if we have problems with cable wrap.  
        lproblem="Cable wrap"     
        DO  I=1,NSTN
! calculate when antenna is on source at new source. This is mjdtmp, uttmp.
          call addsec2ut(mjdcur(j),utcur(j),
     >     idlcur(j)+idurcur(j)+isortm+ical+int(tslew(j)+0.9),
     >      mjdtmp,uttmp)
! dur_temp is time from on source (after previous scan) to end of scan.
          dur_temp=isecdif(mjd,ut,mjdtmp,uttmp)+idurst(j)
          cwarp_Temp=cwarp(j)
          IF (.NOT.kcont(mjdtmp,uttmp,dur_temp,nsor,j,cwarp(j),ierr))
     >     THEN
            WRITE(LUDSP,
     >        "('ERROR! (newob):  Cable wrap problem! ',
     >        'Scan not continous for ', A8,' at ',A8)") 
     >         cSORNA(NSOR),cstnna(j)
            kok=.false.
            istn(i)=-iabs(istn(i))
          endif

          if(cwarp_temp .ne. cwarp(j)) then   
          if(cwarp_temp .eq. " ") cwarp_temp="-"
            write(ludsp,
     >	    "('ERROR! (newob): Cable wrap changed at ',a ' from ',
     >         a,' to ',a)")      cstnna(j),cwarp_temp,cwarp(j)
            kok=.false.
            istn(i)=-iabs(istn(i))
          endif
        end do
        if(.not.kok) goto 920 
       
 
      lproblem="Misc"
      IF (.NOT.KOK) THEN
        IF (KVIS) THEN !mutual vis required
          IERR=1
          RETURN
        ELSE !remove bad stations
          goto 920
        ENDIF
      ENDIF
C
C    Iterate until the SNR calculations are being done at the same time as the
C    calculated start time, or max 3 times.

! Note. utbeg effects not only snr, but tape footages as well, through itrun above.
      if (mjd.ne.mjdbeg.or.(int(dabs(utbeg-ut)).ge.imodtm)) then !iterate     
         itersnr = itersnr + 1
!        write(*,*) "Iterating !", itersnr,isecdif(mjd,ut,mjdbeg,utbeg)
        if (itersnr.le.3) goto 930
      endif !iterate

! Now check for downtime. 
       lproblem= "DOWN" 
       DO  I = 1,NSTN !Check source is visible for all observations.
         j=istn(i)
!  Now turn off station if station is not up for scan. (Becase of downtime.
         if(.not.(kstatup(j,mjdbeg,utbeg,idurst(j)))) then 
           write(*,
     >    '("Station ",a, " can not participate because of downtime")')
     >             cstnna(j)                
             kok=.false.
             istn(i)=-iabs(istn(i))
          endif   
        end do   
        if(.not.kok) goto 920

!     7.  Everything looks OK.  Write out info on screen.
      if(kdisplay) then       
        call display_scan_info(mjd,ut,mjdt,utt,icod,
     >     nsor,nstn,istn, tslew)
      endif

!     8.  Ask if this is OK before finalizing.
      if(nsubc .eq. 0 .and. kask) then
        if(.not.kyes_to_prompt("Accept observation (Y/N) ?")) then
           ierr=1
           return          
        endif
      ENDIF

!     9. Everything is OK.  Put all of the stuff we've just calculated into cur or tst variables.

      
      keep_index=.true.  
      if (nsubc. eq. 0) then
! A real scan
       call set_scan_param(
     >  nstn,   istn,    mjd,    ut,
     >  nsor,   ical,    idle,   icod, ircur,   idurst,  lcable,
     >  cpre,cmid,cpst, keep_index,
     >  nstncur,istcur,  mjdcur, utcur,gstcur,st0cur,iyrcur, idacur,
     >  nsorcur,icalcur, idlcur, icodcur,ireccur,idurcur,lcblcur,
     >  cprecur,cmidcur,cpstcur)
        DO  I=1,NSTN !set CUR variables
          j=istn(i)
          itucur(j)=ituse(j)
        end do
      else
! A test scan
       call set_scan_param(
     >  nstn,   istn,    mjd,    ut,
     >  nsor,   ical,    idle,   icod, ircur,   idurst,  lcable,
     >  cpre,cmid,cpst,keep_index,
     >  nstntst,isttst,  mjdtst, uttst,gsttst,st0tst,iyrtst, idatst,
     >  nsortst,icaltst, idltst, icodtst,irectst,idurtst,lcbltst,
     >  cpretst,cmidtst,cpsttst)
      endif

      IERR = 0
      RETURN
      END
