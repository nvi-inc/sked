      SUBROUTINE CHCMD(LINSTQ,cmdcod)  !CHECK/SHIFT COMMAND
C
C  CHCMD checks a schedule for adequate slewing time, and
C  can automatically shift the start time.
C  SKED commands: AUTOSHIFT, TAGALONG, REMOVE, CHECK, ADD, UNTAG
C
      implicit none 
      include "../skdrincl/skparm.ftni"
      include "skcom.ftni"
      include "../skdrincl/sourc.ftni"
      include "../skdrincl/statn.ftni"
      include "../skdrincl/freqs.ftni"
      include "../skdrincl/skobs.ftni"
      include "major.ftni"
C
C  INPUT VARIABLES:

      integer*2 LINSTQ(*)    ! input string, contains DT, first word=length 
      character*2 cmdcod     ! SH=shift;  CH=check; TA=tagalong; RM=remove; UT=Untag;

! Functions
      logical kstatup        !is a station up?
      real speed             ! Speed of tape drive
      integer isecdif        !Difference between two times in seconds
      integer maxdu 

C
C  CALLING SUBROUTINES: SKED
C  CALLED SUBROUTINES: SLEWT,GTOBS,LSCUR,PTOBS,CHPAR,SNRSK,TAPEF,AUCHK
C                      GTRUN, GTPRE
C
C  LOCAL VARIABLES
      integer jt             !JT - index into station arrays of new station for tagging
      integer jnet(max_stn)  !indices of tag-to stations
      integer njnet          !number of tag-to-stations 
      integer ii
      integer iftend(max_stn)

      LOGICAL KAUTIM        !If true shift the time. 
C     - true for autoshifting time or tape only
   
C     The following PREvious variables are dimensioned by station.
      integer NSPRE(MAX_STN),MJDPRE(MAX_STN),IDURPR(MAX_STN)
      double precision UTPRE(MAX_STN)    ! UT at end of previous observation
      integer icod_pre(MAX_STN)    
      integer itupr(max_stn)
      character*2 cwrap_pre(max_stn)
      integer idurpre(max_stn)
      integer idlpre(max_stn) 
     
C       - previous values for source number, MJD, duration, cable wrap,
C         footage count, direction, code, and pass.
 

      double precision utend
      integer mjdend
      integer ISTN(MAX_STN)               ! list of stations, dummy for LSCUR
      LOGICAL KSTART,KGOT,KRWND           ! for GTOBS call 
      LOGICAL KHEAD                       ! for LSCUR call                                            
      integer minidl,ifirst,i,kerr
      integer ierr_snr,ierr_auchk,ierr_slew
      integer j1,ijt,j,nstn
      logical kok
      integer idursav,nsorsav,mjdsav,icalsav,idlsav 
      integer icodsav,iyrsav,idasav
      character*2 cwrap_sav
      double precision utsav,gstsav
      real tslew,trise
      logical knewtp,krw
      character*2 cwrap_new
      integer mjd_tmp
      real*8  ut_tmp
      real*8  rec_factor
     
      integer MaxDuraInScan           ! result of maxdu function
 
      integer istbad(max_stn)  ! =1 for source not up there
      integer ithres           ! Threshold 
      integer itdif            !difference in time between two epochs

      
! JMG variables.
      logical kdisplay         !print out the warning messages
      integer ierr             !some kind of error.
      integer istat            !station index
      integer isource          !source index
      integer islew_info       !info about slew.
C
C History
C     820522  MAH  TAgalong added
C     840816  MWH  Fixed incorrect conditional statement
C     840928  MWH  Added cable wrap indicator on TAgalong
C     880310  NRV  DE-COMPC'D
C     880404  NRV  ADDED CHECK FOR IDLE TIME
C     890515  NRV  Added SNRSK calculations for tag-along durations.
C                  Added option to tag only to observations having a
C                  specified station.
C     890516  NRV  Removed command parsing to CHPAR
C     890519  NRV  Added KGINOR check for TAGALONG
C     890523  NRV  Added ADD command
C     890531  NRV  Removed KIGNORE (use ADD instead)
C     891118  NRV  Changed calling sequence for SNRSK
C     900116  NRV  Added MAXTAP to TAPEF calling sequence
C     900327  NRV  Added lookah to SLEWT call
C     910224  NRV  Add ITEARL to calculations of previous footages
C                  and to calculation of UTPRE -
C     910225  NRV  Added initialization of SNR-check variables,
C                  added display of SNR histograms.
C     910305  NRV  Removed ITEARL from UTPRE calculation (??)
C     910619  NRV  Added trise to SLEWT call
C     910924  NRV  Add mjd,ut to SNRSK call
C     930224  nrv  implicit none
C    931021 nrv Add itsris to SLEWT call
C    931109 nrv Change itsris to tsris for double precision
C    931112 nrv Add st0,frac to slewt call
C    940124 nrv Restore full CUR variables after failed TAG
C    940513 nrv Set cable to "V" for VLBA special slewing algorithm
C    940705 nrv Check for a subnet for the "tag-to" 
C    950509 nrv Do not return on errors if we are only shifting tapes.
C    950519 nrv Add "untag" mode to remove bad stations
C 951018 nrv Change PTOBS first argument to character
C 951116 nrv Add station index to SPEED call, and to TAPEF
C 960628 nrv Uncomment the special option to use the MAXIMUM duration
C            for tagalong, instead of the minimum.
C 960708 nrv Re-comment the above.
C 960923 nrv ITEARL array
C 970329 nrv Continuous recording.
C 970406 nrv Add icod_pre to GTPRE/GTRUN calls
C 970715 nrv Use ITUCUR in calculation of IFTPRE on first check
C 001011 nrv Check first source up or not.
! 2005Apr29 JMGipson.  Added final argument to tapef. If 0, just checking.
!                                                     if >0, trial obs.
! 2005Oct06 JMGipson.  initialized utpre=utcur, mjdpre=mjdcur
! 2006Sep22 JMGipson.  A.) Cleaned up tag-along section to make it easier to folow.
!             B.) Indicate why a tag-along failed. Only if kdebug=.true.
!             C.) Removed calculation of tproc which was never used.
!             D.) Ignore tape positioning errors for first source in tagalong mode.f
!             E.) If tagalong can't meet SNR requirements, see if can meet
!                 SNR-margin. If so, use.
! 2006Oct25 JMGipson.  Fixed bug in tagalong. WAsn't setting
!
! 2007Jan11 JMGipson.  Initialized ierr_auchk, ierr_snr, ierr_tapef.
! 2008Jun06 JMG. Changed cmdcod "AU" to "SH"  (shift)
! 2010Mar26 JMG. Now check the first scan for SNR targets. PReviosly assumed OK.
!                Also if UNTAG remove scans with bad SNR.
! 2010Apr29 JMG. Wasn't correctly putting out header info for listing.
! 2010Aug23 JMG. Removed call to tapef.  (Don't need to do tape stuff anymore.) 
! 2010Nov23 JMG. Tagalong didn't work correctly if the station was down.
! 2011Feb08 JMG. Fixed bug with tag-along. If the previous time the tagged station was used was
!                a long time ago, the slew-time calculation might not work because sked will think
!                the source is down. So instead set the previous time to 300 seconds ago. 
! 2015Mar18 JMG. Removed variable 'ja' which is no longer used. 
! 2015Nov20 JMG. Simplified tag-along logic, particularly if tagging  along to a subnet. 
! 2016Jul14 JMG. TAGALONG: Modified so that takes into account time to write data if Mark6 station.
! 2016Aug16 JMG. Now correctly does wrap in tag-along mode.  

C
C    1. Parse command and parameters.
C
      kdisplay=.true.

      CALL CHPAR(LINSTQ,cmdcod,JT,JNET,njnet,KAUTIM,MINIDL,IERR)
      IF (IERR.NE.0) RETURN
C
C   Pseudo-code for this routine:
C  get CUR variables, initialize PRE variables
C  do while xerr=0
C    if (tagalong)
C      add JT station to CUR
C      check PRE --> CUR with AUCHK
C      if (ok) then
C        put new CUR with PTOBS
C      else
C        remove JT station from CUR
C      endif
C    endif
C    if (remove)
C      if station is in this obs then
C        remove station from CUR
C        shift up remaining stations in ISTCUR
C        put new CUR with PTOBS
C      endif
C    endif 
C    if (add and station is in this obs) then
C      add station to CUR (no checking)
C      put new CUR with PTOBS
C    endif
C    if (rewrite)
C      put with PTOBS
C    endif
C    list CUR with LSCUR
C    save CUR in PRE
C    get next CUR
C    check PRE --> CUR with AUCHK (before tagging)
C    if (untag)
C      remove bad stations as found by AUCHK
C    endif
C    write error/info messages
C    put new CUR with PTOBS
C  enddo
C
C
C  2. Read first obs to get CUR variables set.  Initialize PRE.
C     Begin loop over observations.
C
      IFIRST = 1
      KSTART=.TRUE.
      ISORCM=0
      KRWND=.FALSE.
      do i=1,max_stn
        nspre(i)=-1   
!        idurcur(i)=0
!        idlcur(i)=0     
      end do   
    
! Get an observation. If a problem, exit. 
      CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)  
  
      IF (IERRCM.ne.0) then
        CALL WRERR(IERRCM,INUMCM)
        GOTO 990
      endif 
      IF (.NOT.KGOT) GOTO 990
C   
      DO  I=1,NSTATN 
!        NSPRE(I) = nsorcur(i) 
        MJDPRE(I) = mjdcur(i)
        UTPRE(I) = utcur(i)
        idurpre(i) =idurcur(i) 
        idlpre(i)  =idlcur(i) 
        cwrap_pre(i)=cwrap_cur(i)  
        icod_pre(I) = 1     
      END DO
C
      ISORCM = 0
      KHEAD=.TRUE.
      KERR=0
C
      kdisplay=.true.

! Check the first scan.
      if(cmdcod .eq. "CH" .or. cmdcod .eq. "UT") then      
        CALL LSHED(LUDSP,nsubst,isubst)
        khead=.false. 
        istat=istcur(1)
        isource=nsorcur(istat)
  
        call ChkSrcUp4Scan(istat,isource,nceles,
     >    csorna(isource),cstnna(istat), MJDCUR(istat),
     >    UTCUR(istat), Idurcur(istat),cwrap_new,ludsp,kdisplay,ierr)
 
        do i=1,nstncur
          idurst(istcur(i))=idurcur(istcur(i))
        enddo
 
        call snrac(nstncur,istcur,isource,icodcur(istat),ludsp,
     >    mjdcur(istat),utcur(istat),ierr)
        if(cmdcod .eq. "UT") then
           write(*,*) "Removing"
           call remove_Bad_snr(isource,icodcur(istat))
        endif 
      endif
    
 
! -----------------Start of loop over all observations-----------------------
      DO WHILE (.TRUE.)  

C  3. Add station to CUR variables for tagalong.  Check slewing
C  and time with SLEWT and AUCHK.  If OK, replace observation with PTOBS.
C
      IF (cmdcod.eq."AD".OR.(cmdcod.eq."TA".AND.(KERR.EQ.0))) THEN !try to add stn
! Initialize to no errror
        ierr_snr=0
     
        ierr_auchk=0 
        ierr_slew=0 
 
! Check if station is "up" 
        if(.not. kstatup(istat,mjdcur(j1),utcur(j1),idurcur(j1))) then
          goto 110                          !this is fast exit to end of tag-along
        endif
        
! Check to see if station is in the scan.                                                         
        DO  I = 1,NSTNCUr
          IF (JT.EQ.ISTCUR(I)) goto 110    !this is fast exit to end of tag-lalong. 
        END DO

        if(njnet .ne. 0) then   ! Only tag along to some subnet. 
          do i=1,nstncur
            do ii=1,njnet
               if(jnet(ii) .eq. istcur(i)) goto 50   !found a match. 
            end do
          end do
          write(*,*) "No match from subnet!" 
          goto 110    !no match. quick exit to end of tag-along. 
        ENDIF

50    continue    
        istat=jT
        j1=istcur(1)
        isource=nsorcur(j1) 
        call ChkSrcUp4Scan(istat,isource,nceles,
     >      csorna(isource),cstnna(istat), MJDCUR(j1),
     >      UTCUR(j1), Idurcur(j1),cwrap_new,ludsp,kdisplay,ierr)
        if(ierr .ne. 0) goto 110            !Source not up so we can't tag it.        

        MaxDuraInScan=MAXDU(idurcur,nstncur,istcur)  !find maximum scan length of other stations.
        nstncur = NSTNCUr+1                    !put tagged station at end of list.
        ISTCUR(NSTNcur) = JT
C         Save the cur(jt) variables in case we can't tag along JT
C         to the current obs and have to restore its variables to the
C         previous scan that it could make.
        idursav=idurcur(jt)
        idlsav= idlcur(jt)
      
        nsorsav=nsorcur(jt)
        utsav=utcur(jt)
        mjdsav=mjdcur(jt)
        icalsav=icalcur(jt)
        cwrap_sav=cwrap_cur(jt)
        icodsav=icodcur(jt)
        iyrsav=iyrcur(jt)
        idasav=idacur(jt)
        gstsav=gstcur(jt)

        if (kvscan) then !compute duration for the new station JT
C         Compute scan lengths for subnet, including JT.
C         Use as default duration the duration of this scan.
          CALL SNRSK(MaxDuraInScan,NSTNcur,ISTCUR,nsorcur(j1),
     >      icodcur(j1),IERR_snr,ludsp,mjdcur(j1),utcur(j1))

! Note:  Ierr_snr only means not enough info to compute SNR for some station.

!          write(*,*) "NumBadSNR: ", numBadSNR(nstncur) 
!          write(*,*) "Max, dur: ", MaxDuraInScan, idurst(jt)
! Check for various errors in tag along mode. If so, skip rest of checking.
! If not found, set to the computed duration.
          if (cmdcod.eq."TA") then
            if(ierr_snr .ne. 0) then
              if(kdebug)
     >          write(ludsp,*)"TAG ERROR!  SNRSK: ",ierr_snr
              goto 100
            else if(idurst(jt) .lt. 0) then
              if(kdebug)
     >          write(ludsp,*)"TAG ERROR!  durst <0 ",idurst(jt)
                ierr_snr=idurst(jt)
                goto 100
            else if(IDURST(JT).le.MaxDuraInScan) THEN 
               idurcur(jt)=idurst(jt)  !use computed value. 
            else
! To meet target would need to increase Duration of current scan. This is a no-no.
              if(kdebug) then
                write(ludsp,*) "TAG ERROR! duration: (max, want) ",
     >                 MaxDuraInScan,idurst(jt)
              endif
! See if would work with "margin".
              idurst(jt)=MaxDuraInScan
              call snrac(nstncur,istcur,nsorcur(j1),icodcur(j1),ludsp,
     >               mjdcur(j1),utcur(j1),ierr_snr)
              if(kdebug .and. numbadsnr(nstncur) .eq. 0) write(ludsp,*)
     >              "Meets relaxed SNR-margin requirements"
              if(ierr_snr .ne. 0) goto 100
              idurcur(jt)=MaxDuraInScan          
            endif
          else ! for ADD use the calculated duration if it's smaller
C                  than the max, else use the max. If the calculated
C                  duration is -1 then use the max.
              if (idurst(jt).le.MaxDuraInScan.and.idurst(jt).gt.0) then
                idurcur(jt)=idurst(jt)
              else
                idurcur(jt)=MaxDuraInScan
              endif
            endif 
          else !use current duration for the new station JT
!            idurcur(jt) = idurcur(j1)
C*************************************************************
C         Special option to always use the maximum duration
C         instead of the duration calculated by SNR. Used for
C         polarization experiments. And for tagging along GGAO.
C         Uncomment this line and set parameter VSCAN to no.
            idurcur(jt) = MaxDuraInScan
C*************************************************************
          endif! compute duration/use current for the new station JT  

          NSORcur(JT) = NSORcur(J1)
         
! The tag along station should already have all of this info...
           if(.false.) then 
           UTCUR(JT)   = UTCUR(J1)
           MJDCUR(JT)  = MJDCUR(J1)
           ICALcur(JT) = ICALcur(J1)
           ICODcur(JT) = ICODcur(J1)
           IYRCUR(JT)  = IYRCUR(J1)
           IDACUR(JT)  = IDACUR(J1)
           GSTCUR(JT)  = GSTCUR(J1)
           endif 
 
C         Calculate slewing just to get the cable wrap         
          IF (NSPRE(JT).eq. -1) THEN
! No prior source. Use time of first station. 
            call cabl1(jt,nsorcur(j1),mjdcur(j1),utcur(j1),
     >            cwrap_cur(jt))
! Prior Obs
          ELSE     
! Used to be  
!           cwrap_new=" "
            cwrap_cur(jt)=" "       
            CALL SLEWT(NSPRE(JT),MJDPRe(jt),UTpre(jt),NSORcur(J1),JT,
     >         cwrap_pre(JT),cwrap_cur(jt),tslew,lookah,trise,tsris,
     >         st0cur,  frac,knov,islew_info)
!             write(*,*) "cwrap_new ", cwrap_cur(jt) 
!             write(*,*) idurcur(jt), idlcur(jt) 
           
! mjd_tmp is when antenna becomes free
            call addsec2ut(mjdpre(jt),utpre(jt),
     >         idurpre(jt)+isortm+icalcur(jt),mjd_tmp,ut_tmp)
      
             if(cstrec(jt,1) .eq."Mark6") then
                if(isink_mbps(jt) .gt. 0 .and.
     >            isink_mbps(jt) .lt. idata_mbps(jt)) then
                  rec_factor= idata_mbps(jt)/isink_mbps(jt)     
                  ut_tmp=ut_tmp+idurpre(jt)*(rec_factor-1)               
                endif
             endif
                 
             itdif=isecdif(mjdcur(j1),utcur(j1),mjd_tmp,ut_tmp)     
   
             if(itdif .lt. tslew) then         
                ierr_slew=1
                write(*,*) "INFO: (tag): NOT ENOUGH time between obs"
                goto 100 
             endif        
          ENDIF     

!         if(.false.) then 
           UTCUR(JT)   = UTCUR(J1)
           MJDCUR(JT)  = MJDCUR(J1)
           ICALcur(JT) = ICALcur(J1)   
           ICODcur(JT) = ICODcur(J1)
           IYRCUR(JT)  = IYRCUR(J1)
           IDACUR(JT)  = IDACUR(J1)
           GSTCUR(JT)  = GSTCUR(J1)
!          endif 
         
          itdif=isecdif(mjdcur(jt),utcur(jt),mjdpre(jt),utpre(jt))
          if(itdif .gt. 300) then
            nspre(jt)=-1           !set it as if the antenna has not been used. 
          endif 
    

          CALL AUCHK(cmdcod,MINIDL,NSPRE,MJDPRE,UTPRE,idurpre,idlpre,
     >       cwrap_pre,KAUTIM,ierr_auchk,istbad)
! Ignore errors in tape positioning on the first source.
          if(nsorsav .eq. 0) ierr_auchk=0
          if(kdebug.and. Cmdcod.eq."TA" .and. ierr_Auchk .ne. 0)
     >          write(ludsp,*)"TAG ERROR! auchk ",ierr_auchk

100       continue     
          kok=ierr_snr.eq.0 .and. NumBadSNR(nstncur) .eq. 0  .and. 
     >        ierr_auchk.eq.0 .and. ierr_slew .eq. 0
          IF (cmdcod.eq."AD".OR.Kok) THEN !replace in file             
            CALL PTOBS("RE",1,IERRCM)
            IF  (IERRCM.NE.0) THEN  !
              CALL WRERR(IERRCM,INUMCM)
              GOTO 990
            END IF  !

            call GTPRE(nspre,cwrap_pre,icod_pre) 
          ELSE !readjust cur vars
            nstncur = nstncur-1
            idurcur(jt)=idursav
            idlcur(jt)=idlsav          
            nsorcur(jt)=nsorsav
            utcur(jt)=utsav
            mjdcur(jt)=mjdsav
            icalcur(jt)=icalsav
            cwrap_cur(jt)=cwrap_sav  
            icodcur(jt)=icodsav
            iyrcur(jt)=iyrsav
            idacur(jt)=idasav
            gstcur(jt)=gstsav
C           KADDOK = .FALSE.
          ENDIF  !replace/readjust cur vars      
110   continue                   !exit of tag along. 
      ENDIF !try to add stn 
! ************************************************************************************************
! ---------------End TA .or. AD------------------------------------------

C   3.5  Remove bad stations   
      if (cmdcod.eq."UT".and.ifirst.gt.1) then 
        do i=1,nstatn
          istbad(i)=0
        enddo
! Check timing, source up for scan.
        CALL AUCHK(cmdcod,MINIDL,NSPRE,MJDPRE,UTPRE,idurpre,idlpre,
     >       cwrap_pre,KAUTIM,ierr_auchk,istbad)     
! Find bad stations. 
        do i=1,nstncur
          j=istcur(i)         
          if(istbad(j) .gt. 0) then       
             write(luscn,
     >         "('CHCMD05 - ',a8,' removed from this scan.')") cstnna(j)
             istcur(i)=-istcur(i)  !mark as bad.
          endif
        end do
        call destn(nstncur,istcur)  
        if(nstncur .ge. 2) then
          CALL PTOBS("RE",1,IERRCM) ! replace scan
        else
          call ptobs("DE",1,IERRCM) !remove scan
          writE(luscn,
     >      '("CHCMD06 - less than 1 station. Scan removed.")') 
          goto 390
        endif
        IF  (IERRCM.NE.0) THEN
          CALL WRERR(IERRCM,INUMCM)
          GOTO 990
        ENDIF 
        
      endif
390   continue
C
C   4. Remove a station
C
      IF  (cmdcod.eq."RM") THEN  !remove a station 
C       Find the location of the station to remove
        IJT = 0                                                           
        DO  I=1,nstncur                                                    
          IF (ISTCUR(I).EQ.JT) IJT=I
        END DO                                                            
        IF  (IJT.GT.0) THEN  !this stn actually here
C         Shift remaining stations up

          nstncur = nstncur-1
          if(nstncur .ge. 2) then
            DO  I=IJT,NSTNCUr
              ISTCUR(I) = ISTCUR(I+1)
            END DO
C         Also should restore this stn to have parameters
C         valid for its most recent obs. This is hard to
C         do -- probably have to do the same logic as delete.
C         Re-store the obs without this station
            CALL PTOBS("RE",1,IERRCM)
          else
!  Delete it!   
            write(*,*) "Deleting ", cbuf(1:60)
            call ptobs("DE",1,ierrcm)
          endif
          IF  (IERRCM.NE.0) THEN  !
            CALL WRERR(IERRCM,INUMCM)
            GOTO 990
          END IF  !
        END IF  !this stn actually here
      END IF  !remove a station
C
C
C  5. Re-write each observation (can be used to convert
C     from old to new format).
C
      IF (cmdcod.eq."RW") THEN !write out each obs
        CALL PTOBS("RE",1,IERRCM)
        IF  (IERRCM.NE.0) THEN  !
          CALL WRERR(IERRCM,INUMCM)
          GOTO 990
        END IF  !
      ENDIF !write out each obs
C
C
C   5.5  List current observation
C     This will have the tag-along station in it if it checked ok.
C     This will have the bad stations removed.
C
      NSTN = 0
      CALL LSCUR(KHEAD,isubst,Nsubst,90.0)
      IF (KERR.GE.2.AND.cmdcod.eq."SH") goto 990
     
C     if (KERR.GT.0.AND.cmdcod.eq."TA") GOTO 990
C                   We must quit if we're autoshifting and we can't
C                   go on due to an error.                                  
C                   Also quit if tagging and the original obs. has error    
C                    ...no, don't quit during tag!
C
C  6. Save CUR variables in PRE, since the observation was OK if we
C     got to this point.
C
      DO  I=1,nstncur  !save CUR status in PRE variables
        J = ISTCUR(I)
        NSPRE(J)=NSORcur(J)
!       call addsec2ut(mjdcur(j),utcur(j),idurcur(j)+idlcur(j),
!     >     mjdpre(j),utpre(j))
        cwrap_pre(J)=cwrap_cur(J)     
        icod_pre(J)=ICODcur(J)
        mjdpre(j)  =mjdcur(j)
        utpre(j)   =utcur(j)
        idlpre(j) = idlcur(j) 
        IDURPRe(J) = IDURcur(J)   
      END DO  !save CUR status in PRE variables
C
C  7. 
C
      IF  (IFIRST.EQ.1) THEN  !initialize 1st time thru


      END IF  !initialize 1st time thru
      IFIRST = 2

C
C  8. Get next observation and check transition PRE --> CUR.
C
400   continue 
      
      call GTPRE(nspre,cwrap_pre,icod_pre)  ! save pre variables and calculate utstart
      CALL GTOBS (KSTART,KRWND,KGOT,IERRCM) ! Get next observation into CUR variables
      IF (.NOT.KGOT) GOTO 990
!      call gtrun(idirpr,nspre,cwrap_pre,iftpre,icod_pre,itupr) ! calculate running time
      KERR=0
    
      if (cmdcod.eq."CH".or.cmdcod.eq."SH") Then      
          CALL AUCHK(cmdcod,MINIDL,NSPRE,MJDPRE,UTPRE,idurpre,idlpre,
     >       cwrap_pre,KAUTIM,ierr_auchk,istbad)      
      endif 
  
C                     Check transition from PRE to CUR
C
C  9. Error messages.
C     And end of loop.
C
      IF (KERR.EQ.1.AND.(cmdcod.eq."CH")) Then
          WRITE(LUDSP,'(a)')
     >   " CHECK99 - Not enough time between these observations"
      Else IF (KERR.EQ.2.AND.(cmdcod.eq."CH")) then
         write(ludsp,'(a)')
     >  " CHECK89 - Not enough tape between these observations"
      Else If (KERR.EQ.3) then
         WRITE(LUDSP,'(a)')
     >   " CHECK98 - Following source is outside limits:"
      Else IF (KERR.EQ.4) then
         WRITE(LUDSP,'(a)')
     >  " CHECK97 - Following observation has error in tape usage."
      endif
C
C
C
C  10.  End of it all.
C
      ENDDO !major loop over observations
C
990   WRITE(LUDSP,"(' END OF AUTOCHECKING')")
C
C Write summary of autoshift here

      RETURN
      END
