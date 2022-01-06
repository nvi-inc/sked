       SUBROUTINE NEWOB(cINSTQ,ISTN,NSTN,IERR,nsubc)

!  NEWOB decodes an observation command line, checks to see ifthe observation is feasible.
!  If it is not feasible with original station list, tries removing some of them. 
!  Beginning at 920 we start with original list of stations.
!  If the source is not up at a station, then it is removed.f

      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
 

! functions
      logical kstatup                   !is station up for scan?
      logical kcont
      logical kyes_to_prompt            ! returns true if anser is "Y" or "Yes"
      integer  trimlen
      integer*4 isecdif                   !difference in time between two scans.
      real*4  sefdel                    !elevation dependent SEFD
 
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

      integer mjd_beg                ! MJD beginning of new scan
      double precision ut_beg        ! UT  beginning of scan
      integer mjdtmp                 ! MJD temporary holder
      double precision uttmp         ! UT temporary holder
      integer mjd_at(max_stn)
      double precision ut_at(max_stn) 
      
      integer idur_tmp(max_stn)      ! holds duration
            
      character*20 lproblem         ! Description of potentional problem

C     iStat1 - first station in this subnet
C     J - index of station in selected list
C        ICH  - character counter
C        IDURSO,IDLE,ICAL,NSOR,IYR,IDA,IHR,iMIN,ISC
C               - Holders for decoded values

      double precision UT,ut_cmd      !Holder for decoded UT.
      LOGICAL KOK                     !All stations pass KUP test
    
      integer*2 LPRE(3),LMID(3),LPST(3)    !Procedures 
      character*6 cpre,cmid,cpst
      equivalence (cpre,lpre), (cmid,lmid),(cpst,lpst)
      character*2 cmdcod

      integer*2 LCABLE(MAX_STN)
      character*2 cwrap(max_stn)
      equivalence (cwrap,lcable)     

      CHARACTER*1 CANS           ! NC and CANS are used for user response
      INTEGER NC

      integer iokst(max_stn)     ! returned from SNROK
      integer itersnr            ! 1 for first iteration, add 1 for subsequent      
      integer itemp              ! temporary variablel            
        
      character*80 lerr_msg 
      character*20 lfrnt_msg
      integer istat1             ! first station in subnet.
     
      integer i,nsor,iyrcmd,idaycmd,ihr,imin,isc,mjd_cmd
      integer j        !station index (replace istat)
      integer idur,idurso,ical,idle,icod,mjd
      integer lu
      integer iband              !iband 
      
      integer istbad(max_stn)    ! not used here
      logical kdisplay
      integer islew_info         !info about slew
      real  dur_temp
      integer itdif              !timedifference
      real az_now,az_new 
      real el_now,el_new
      integer isetup_time
      integer isrc_time
      integer ibuf_time 
 
      character*2 cwrap_new,cwrap_now
      integer*2 iwrap_now
      equivalence (iwrap_now,cwrap_now)

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
!      kdisplay = kdebug
C
C   1. Check for enough info to start.  Call NEWPR to parse
C   the input line and return all parameters.
C
      IF  ((NSTATN.EQ.0).OR.(NSOURC.EQ.0).or.ncodes.eq.0) THEN
        IERR = 13
        RETURN
      END IF  !not enough info yet

! Parse command line. 
       CALL NEWPR(cINSTQ,NSOR,iyrcmd,idaycmd,IHR,iMIN,ISC,
     >  ut_cmd,mjd_cmd,IDUR,
     >  IDURSO,ICAL,IDLE,cwrap,ICOD,cPRE,cMID,cPST,NSTN,ISTN,KVIS,IERR)

      IF (IERR.NE.0) RETURN
      
      if (kdisplay) then
        write(luscn,'("Checking new obs on ",a," with ",30(A2,1x))')
     >    cSORNA(NSOR),  (cpoCOD(ISTN(j)),j=1,NSTN)   
      endif  

C    Initialize rise/set times if needed.
      if (.not.krsini) call rsini
C    Initialize extra durations to zero for scheduling.
      do i=1,nstatn
        idurxt(i)=0
      enddo
      
! Find how long it would take the stations to get to the specified source. 
      DO  I=1,NSTN !get latest start time
         J = ISTN(I)   
         iwrap_now=lcblcur(j)          
         call when_at_next_source(kdisplay,luscn,
     >     j,nsorcur(j),nsor,mjdcur(j),utcur(j),
     >     idurcur(j),idle,ical, cwrap_now,cwrap(j),mjd_at(j),ut_at(j),
     >     az_now,el_now,az_new,el_new,tslew(j),
     >     isetup_time,isrc_time,ibuf_time, ierr)         
     
        if(ierr .ne. 0) then
           istn(i)=-istn(i)         !Remove stations that cannot participate. 
         endif  !got a later time           
      end do 
! Remove stations that can't observe      
      CALL DESTN(NSTN,ISTN)  
      if(nstn .lt. 2) goto 900   
      
200   continue   
! Some stations might have been removed. 
! Recalculate start time.          
! update start of scan time. This is latest of all the times.  
      do i=1,nstn
        j=istn(i)        
        if(i .eq. 1) then
           mjd_beg=mjd_at(j)
           ut_beg=ut_at(j)
        else if(isecdif(mjd_at(j),ut_at(j),mjd_beg,ut_beg).gt.0) then 
           mjd_beg=mjd_at(j)
           ut_beg =ut_at(j)      
        endif   
      end do       
   
! If this is a manual scheduled scan AND we specified the time
!    then check to see if scheduled time  is AFTER begin time
      if(mjd_cmd.ne.-1 .and. 
     &   isecdif(mjd_cmd,ut_cmd, mjd_beg,ut_beg) .lt. 0) then     
        do i=1,nstn
          j=istn(i) 
          IF (isecdif(mjd_cmd,ut_cmd,mjd_at(j),ut_at(j)).lt.0) then      
            WRITE(LUSCN,
     >  '("ERROR (newob): At Station ",a," start time ",i6,1x,f8.1 )')
     >        cstnna(j), mjd_cmd, ut_cmd
            write(luscn, '(29x,"before free time time ",i6,1x,f8.1)')
     >       mjdcur(j),utcur(j)
             ierr=-1
          endif                             
        end do 
        if(ierr .ne. 0) return 
        mjd_beg=mjd_cmd
        ut_beg =ut_cmd 
      endif     
        
!     Check sun distance and quit if it's too close.
      call ChkSunDist(nsor,csorna(nsor),mjd_beg,ut_beg,
     >     kdisplay,luscn,rSunMinAngle,ierr)
      if(ierr .ne. 0) return 
         
! Now calculate the SEFD at each station, taking into account the elevation.
      if(.false.) then
      do iband=1,2 
        do i =1, nstn 
           j=istn(i)
           sefdstel(iband,j) =sefdel(iband,nsor,j,mjd_beg,ut_beg)
!           write(*,*) sefdstel(iband,j) 
        end do
      end do          
      endif  

! Duration.   
! Now calculate the durations if not set in the command. 
      KOK=.true. 
      IF (IDUR.ne.0) then
! Duration set in the command use it.       
        do i=1,nstn
           j=istn(i)
           idurst(i)=idur
        end do  
        lu=luscn
        if (nsubc.gt.0) lu=-1
        if (kvscan) then ! calculate SNRs anyway
          call snrac(nstn,istn,nsor,icod,lu,mjd_beg,ut_beg,ierr)
          if (ierr.ne.0) return
        endif
      else 
        if(.not. kvscan) then   !don't calculate duration, yes defualts. 
          do i=1,nstn
            idurst(istn(i))=isscan(nsor)
          enddo
        else                   !heere we calculate duration to meet SNR targets.        
          lu=luscn
          if (nsubc.gt.0) lu=-1       
          call snrok(istn,nstn,nsor,icod,lu,iokst,mjd_beg,ut_beg)             
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
        endif 
      endif
! At this stage have the duration for all of the stations. (Either given or calculated.)
      if(.not. KOK) then 
        call destn(nstn,istn) 
        if(nstn .lt. 2) goto 900    !Common exit on two few stations.   
        goto 200
      endif 
     
! Now check for downtime. 
      lproblem= "DOWN" 
      DO  I = 1,NSTN !Check source is visible for all observations.
        j=istn(i)
!  Now turn off station if station is not up for scan. (Becase of downtime.
        if(.not.(kstatup(j,mjd_beg,ut_beg,idurst(j)))) then 
          if(iverbose_level .ge. 1) then 
             write(luscn,
     >     '("Station ",a, " can not participate because of downtime")')
     >             cstnna(j)                
          endif 
          kok=.false.
          istn(i)=-iabs(istn(i))
        endif   
      end do  
      if(.not. KOK) then 
        call destn(nstn,istn) 
        if(nstn .lt. 2) goto 900    !Common exit on two few stations.   
        goto 200
      endif 
         
! Check to make sure that the source will be up for the entire scan.    
      KOK =.true. 
      DO I = 1,NSTN !Check source is up at start of scan. If not, kick station out.
         j=istn(i)
         call ChkSrcUp4Scan(j,Nsor,mjd_beg,ut_beg,
     >        Idurst(j),lcable(j),luscn,kdisplay,ierr)  
         if(ierr .ne. 0) then
            Kok=.false.
            istn(i)=-iabs(istn(i))
         endif
      END DO  !make sure we can observe at this time
 ! Remove stations where we had problems.     
      if(.not. KOK) then 
        call destn(nstn,istn) 
        if(nstn .lt. 2) goto 900    !Common exit on too few stations.   
        goto 200
      endif 
  
!     7.  Everything looks OK.  Write out info on screen.
      if(kdisplay) then       
        call display_scan_info(mjd_beg,ut_beg,mjd_at,ut_at,icod,
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
     >  nstn,   istn,    mjd_beg,    ut_beg,
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
     >  nstn,   istn,    mjd_beg,    ut_beg,
     >  nsor,   ical,    idle,   icod, ircur,   idurst,  lcable,
     >  cpre,cmid,cpst,keep_index,
     >  nstntst,isttst,  mjdtst, uttst,gsttst,st0tst,iyrtst, idatst,
     >  nsortst,icaltst, idltst, icodtst,irectst,idurtst,lcbltst,
     >  cpretst,cmidtst,cpsttst)
      endif

      IERR = 0
      return 
! Common exit on two few stations      
900   continue   
      ierr=-1
      if (kdisplay) then
        WRITE(LUSCN,'(a)') 
     >     "ERROR! (newob): Less than two stations left. Can't observe."                   
      ENDIF !too few left            
      
      RETURN
      END
