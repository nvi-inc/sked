       SUBROUTINE AUCHK(cmdcod,MINIDL,NSPRE,MJDPRE,UTPRE,idurpre,
     >  idlpre,cwrap_pre, KAUTIM,KERR,istbad)
C
C     AUCHK checks for slewing times and limits and SNR levels.
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

      include 'skcom.ftni'
      include 'major.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
C
C  MODIFICATIONS:
! 2021-11-19 JMG Got rid of calculation of itol which was never  used. 
! 2021-02-19 JMG Now slew returns aznow, aznew, 
! 2019-05-15 JMG If doing 'ch' don't return on slew error messages. Just mush on. 
! 2017-11-14 JMG  Fixed problem in writing out cable-wrap problems. Indicated wrong station. 

C  DATE    WHO    CHANGES
C  810720  NRV    CHECK FOR SOURCE UP AFTER NEW START TIME CALCULATION
C  810817  NRV    MODS FOR FREQ. CODE
C  810817  NRV    ADD AUTO-SHIFT BY TAPE OR TIME ONLY
C  811125  MAH    CHECK OBS CONTINUITY WITH FUNCTION KCONT
C                 NEW ERROR MSG FOR TSLEW=-2.0
C  830423  NRV    CHANGE CVPOS CALLS TO INCLUDE X,Y
C  830524  WEH    ADD DEC TO CVPOS CALL
C  841009  MWH    RESTORE CURRENT VALUES WHEN AUTO QUITS
C                 CORRECT "TIME SHIFTED" MESSAGE FOR LARGE SHIFTS
C  851108  MWH    ALLOW TAPE AUTOSHIFTING FOR A SINGLE STATION
C  851114  MWH    ADD "CHECK" FLAG TO CALL TO MUTAP
C  880310  NRV    DE-COMPC'D
C  880404  NRV    ADDED CHECK FOR IDLE TIME
C  890428  NRV Changed call to AUTOT to include UTT,MJDT
C  890516  NRV Added check for sun distance
C  900118  NRV Modified check for TSLEW=-3 (rising in an hour)
C  900327  NRV Changed to allow scheduling rising sources
C  900413  NRV Added check of SNRs
C  900425  NRV Corrected call to SNRAC
C  910619  NRV Added trise to SLEWT call
C  910924  NRV Add MJD,UT to SNR calls
C  930219  nrv merge of sked/autosked
C  930224  nrv  added implicit none
C  930211  nrv Add nsubc=0 to MUTAP call
C  930609  nrv Add check for initialized station in the final check loop
C    931021 nrv Add itsris to SLEWT call
C  931109  nrv Change to tsris for double precision
C  931110  nrv Change call to SIDTM to use stm0cur
C  931112  nrv Add st0, frac to slewt call
C  940513 nrv Initialize VLBA cable to "V" for special slewing algorithm
C  950405 nrv Use 2-letter station codes
C  950505 nrv Do not return upon error if using tape shifting only.
C  950515 nrv List full station name in error messages.
C  950519 nrv Add untag checking mode, add ISTBAD to autot call
C 951116 nrv Add station index to SPEED call
C 960923 nrv ITEARL array
C 970329 nrv Continuous recording
C 970331 nrv Allow 10 seconds tolerance in footage checks and timing.
C 970406 nrv Add ITUPR to call

! 2005Apr27 JMGipson. Check for continuity from on source after previous scan
!                     to end of current scan.
! 2005Sep25 JMGipson. cwrap_new initialized to cwrap_cur, which is current wrap from schedule.
! 2007May10 JMGipson. Some cleanup.
! 2008Jun06 JMG. Changed cmdcod "AU" to "SH"  (shift)
! 2008Jun20 JMG. Use isecdif instead of computing difference here
! 2009Jan09 JMG. Some reason "Shift" was not working.  Added "call PTOBS(RE,0,kerr)" to get it to work.
! 2010Aug23 JMG. Removed some stuff that checked tape footage, etc. 
! 2012Oct11 JMG. Don't check slew stuff if previous source is the same as the current source. 
! 2014Apr08 JMG. Got rid of  some used variables in the process of debugging. 
! 2015Oct22 JMG. Slight change in writing data. Added some debug options ('kwrite') that I commented out but kept in code because they may be useful. 


! functions
      logical kcont
      integer*4 isecdif                   !difference in time 

C  INPUT VARIABLES:
      character*2 cmdcod                  !  Two letter command code.
      integer minidl
      character*2 cwrap_pre(max_stn)       !previous cable wrap      
      character*2 ctemp
    
      integer NSPRE(*)                     !previous source
      integer MJDPRE(*)                    !previous MJD
      double precision UTPRE(*)           !  UT at start of previous observation. 
      integer idurpre(*)                   !duration of previous scan
      integer idlpre(*)
      LOGICAL KAUTIM                       !  true if shifting for time and/or tape
      character*2 lq

! returned 
      integer kerr  
      integer istbad(*)
C      - previous values for source, MJD, cable, footage count,

C
C  OUTPUT VARIABLES:
C        KERR   - Error return and code:
!                 0=OK-do nothing,
!                 1=no good.
C                 2=quitting.
C
C    SUBROUTINE INTERFACE:
C     CALLING SUBROUTINES: CHCMD
C     CALLED SUBROUTINES: SLEWT, CVPOS, MUTAP, AUTOT, SNRAC
C

C        TSLEW,TSLEW0
C               - Slewing times, from SLEWT
          
C      - new footage counter, direction and pass, by station.               
                           
      double precision UTNEW,GSTNEW                                   
C      - calculated UT for next observation                                 
      double precision UTT(MAX_STN)
      DIMENSION MJDT(MAX_STN)
C     - trial values for each station from AUTOT
      integer j,j2,i,ierr,n1,ichk
      integer mjdnew,mjdt,iyrnew,idanew,itsec,itmin
      integer*4 itdiff
      integer idum,itdif
   
      real toltime ! time tolerance for checking

      character*2 cwrap_new(max_stn)
      character*2 cwrap_tmp
      logical kfirst
      character*1 csign
C     character*128 tape_motion_new(max_stn)
      real tslew0,tslew,trise,speed
      character*80 lform
      character*20 lprefix  !Either 'WARNING! (auchk): ', or 'ERROR! (auchk)'
! JMG
      logical kdisplay
      integer istat     !station
      integer ist
      integer isource   !source
      integer islew_info !near edge of cable wrap
      logical kfirst_obs       !true if first observation. Then we don't have to do all the checking. 
      real az_now,az_new       !azimuth current and in the future 
      real el_now,el_new
   
      real  dur_temp   
      logical kwrite
      real*8 ut_test
C
C  INITIALIZED:                                                               
      data toltime/5.0/ ! allow 10 seconds tolerance in timing and footages

C     0. Check sun distance for this source.
C
     
      lq='"'
      j2=0
      kdisplay=.true.
      j=istcur(1)
      call ChkSunDist(nsorcur(j),csorna(nsorcur(j)),mjdcur(j),
     >    utcur(j), kdisplay,ludsp,rSunMinAngle,ierr)
      if(ierr .ne. 0 .and. kautim) then
        kerr=5
        RETURN
      endif
!    
C     1. First calculate slewing time to the new (=CUR) source.
C
!      kwrite = csorna(nsorcur(istcur(1))) .eq. "0308-611"
       kwrite=.false. 
!      write(ludsp,*) "Source: ", csorna(nsorcur(istcur(1)))
      
      TSLEW0=0.0
      KERR = 0
      kfirst_obs=.true. 
      DO  I=1,NSTNCUr !calculate slewing
        J = ISTCUR(I)
        cwrap_new(j)=cwrap_cur(j)
        if(nspre(j) .gt. 0) kfirst_obs =.false.     
        IF  (NSPRE(J).GT.0 .and. nspre(j) .ne. nsorcur(j)) then 
      
          CALL SLEWT(NSPRE(J),MJDPRE(J),UTPRE(J)+idurpre(j)+idlpre(j),
     >     NSORcur(J),J, cwrap_pre(J),cwrap_new(J),TSLEW,lookah,
     >     trise,tsris,st0cur,frac, knov, islew_info,
     >     az_now,el_now,az_new,el_new)

        if(kwrite) then
           writE(ludsp,*) i, cstnna(j)," ", lq//cwrap_pre(j)//lq, 
     >                 lq//cwrap_new(j)//lq, lq//cwrap_cur(j)//lq
        endif     
         if(tslew .lt. 0) then
             writE(*,*) "auchk: Inform J. Gipson about this!"
         else if(islew_info .ne. 0) THEN !error messages
             kerr=6      
! All of the messages have similar formats. Generate the format.
            if(islew_info .lt. 0) then 
                lprefix='ERROR! (auchk): '
            else
                lprefix='WARNING! (auchk): '
            endif  
            call print_slew_info_warning(ludsp,lprefix,islew_info,j)                       

          END IF  !error messages
        ELSE
          TSLEW=0.0
        ENDIF     
        TSLEW0=AMAX1(TSLEW0,TSLEW)
      END DO !calculate slewing


      if(cmdcod .eq. "TA" .and. kfirst_obs) return 
       
C
      IF  (KERR.NE.0.and.kautim.and.
     >   (cmdcod.ne.'UT'.and. cmdcod .ne. "CH")) then
        return
     
      endif 
C
C     1.3 Compute SNRs achieved.

      if (kvscan) then
        j=istcur(1)
        do i=1,nstncur
! Changed 2006Sep22.  idurxt is taken care of in snrac.
           idurst(istcur(i))=idurcur(istcur(i))
        enddo
        call snrac(nstncur,istcur,nsorcur(j),icodcur(j),ludsp,
     .  mjdcur(j),utcur(j),ierr)
      endif

!     
C     1.5 Calculate tape usage and a new start time.
C
      ICHK = 0
      IF (cmdcod.eq.'CH') ICHK = 1
      IF  (KERR.NE.0.and.kautim.and. cmdcod .ne. "CH") RETURN   
         
      N1 = ISTCUR(1)
      CALL AUTOT(cmdcod,MINIDL,MJDPRE,UTPRE,NSPRE,idurpre,idlpre,
     >    icalcur(n1), ISTCUR,  NSTNCUR,cwrap_pre, nsorcur(n1),
     >    MJDNEW, UTNEW,MJDT,UTT, cwrap_new, istbad)  !                  
C
C      2. Here we determine whether the observation needs a warning or
C         editing.  Return code KERR=0 for do nothing, KERR=1 for edit
C         or warning needed.
C      
      KERR=0
      J=ISTCUR(1)
      IYRNEW = IYRCUR(J)
      IDANEW = IDACUR(J)
      GSTNEW = GSTCUR(J)
      kfirst=.true.
      DO  I=1,NSTNCUr !check and/or adjust CUR variables
        J = ISTCUR(I)
       if(kwrite) then
           writE(ludsp,*) i, cstnna(j)," ", lq//cwrap_pre(j)//lq, 
     >                 lq//cwrap_new(j)//lq,  lq//cwrap_cur(j)//lq
        endif 

        itdiff=isecdif(mjdnew,utnew,mjdcur(j),utcur(j))
        if (nspre(j).gt.0) then
          IF(cmdcod.eq.'SH'.AND.  itdiff .ne. 0) then 
      
            IF(KAUTIM) THEN  !modify time variables
              IF (kfirst) THEN  !same time for all stns
                kfirst=.false.      
                  IF  (ITDIFF.NE.0) THEN  !time diff
                  if(itdiff .lt. 0) then
                     csign="-"
                  else
                     csign="+"
                  endif
                  ITDIFF = IABS(ITDIFF)
                  call sec2minsec(itdiff,itmin,itsec)
                  IF  (ITDIFF.GE.21600) THEN
                    WRITE(LUDSP,'(a)')
     >              ' Following observation shifted more than 6 hours'
                  ELSE
                    WRITE(LUDSP,9125) cSIGN,ITMIN,ITSEC
9125                FORMAT(' Following observation shifted ',
     >                  A1,1x,I3.3,'m',I2.2,'s')
                  ENDIF
                END IF  !time diff
              END IF  !same time for all stns          
              MJDCUR(J)=MJDNEW
              CALL YDJUL(IYRCUR(J),IDACUR(J),MJDNEW+2440000.0D0)         
              UTCUR(J)=UTNEW
              CALL SIDTM(MJDCUR(J),ST0cur(j),FRAC)
              GSTCUR(J) = ST0cur(j) + UTCUR(J)*FRAC
              IF (GSTCUR(J).GE.twoPI) GSTCUR(J)=GSTCUR(J)-twoPI

              ctemp = cwrap_cur(J)
              cwrap_cur(J)=cwrap_new(J)
              cwrap_new(J) = ctemp
              call ptobs("RE", 0, kerr)
            END IF  !modify time variables
         END IF  !modify CUR
        endif !initialized
        
                                                                           
        IF (cmdcod.eq.'CH'.OR.cmdcod.eq.'TA'.OR.cmdcod.eq.'RM' .or.
     >      cmdcod.eq.'UT') THEN  ! check CUR
          IF (MJDNEW.GT.MJDCUR(J).OR.((MJDNEW.EQ.MJDCUR(J)).AND.
     >        (UTNEW.GT.UTCUR(J)+toltime))) then ! check timing
!            write(*,"('auchk:',A,1x,2i6,3f10.1)")
!     >         cpocod(j), MJDNEW,MJDCUR(j), UTNEW, UTCUR(j),toltime
            KERR = 1
            itdif=idint(utnew-utcur(j))
          endif ! check timing
  
         END IF  !check CUR
C
      END DO  !check and/or adjust CUR variables
C
C        3. Check for source up at start and end of observation.
C        If there is no source to slew from then don't compute anything.
C

      DO  I=1,NSTNCUr !check source within limits
        istat=istcur(i)
        isource=nsorcur(istat)
        call ChkSrcUp4Scan(istat,isource,MJDcur(istat),UTcur(istat), 
     >    idurcur(istat),cwrap_new(istat),ludsp,kdisplay,ierr)
        if(ierr .ne. 0) then
          kerr=3
          istbad(istat)=1
        endif       
      END DO  !check source within limits
    
!
! Also check to see if source is up from the end of the slew to the end of the scan.
!
      DO  I=1,NSTNCUr !check source within limits
        ist=istcur(i)
        isource=nsorcur(ist)
        dur_temp=float(idurcur(ist))+
     >    isecdif(mjdcur(ist),utcur(ist),mjdpre(ist),utpre(ist))   
        IF (.NOT.KCONT(mjdpre(ist),UTpre(ist),dur_temp,isource,
     >       ist,cwrap_new(ist),ierr)) THEN
          WRITE(LUDSP,
     >  "('ERROR! (auchk): Cable wrap problem. Scan not continous for ',
     >      A,' at ',A)")   cSORNA(NSORcur(ist)),
     >   cstnna(ist)//"(="//cpocod(ist)//")"   
          kerr=6
          istbad(ist)=1
        endif  

        if(cwrap_new(ist) .eq. " ") cwrap_new(ist)="-"
        if(cwrap_cur(ist) .ne. cwrap_new(ist)) then  
           write(LUDSP,
     >  '("WARNING! (auchk): Wrong cable wrap for ",A8," at ",A,
     >        ".  Should be: ", a, " was: ",a)')
     >      cSORNA(NSORcur(ist)),
     >      cstnna(ist)//"(="//cpocod(ist)//")",
     >        lq//cwrap_new(ist)//lq,lq//cwrap_cur(ist)//lq
         endif
         cwrap_cur(ist)=cwrap_new(ist)
      end do
C
      if (cmdcod.eq.'SH'.and..not.kautim) return !leave alone if only shifting tape

      IF  (cmdcod.eq.'SH'.AND.KERR.EQ.3) THEN  !restore current variables
        write(ludsp,'(a)')
     > "WARNING! (auchk):  One or more sources not visible after shift."
        pause
    
      END IF  !restore current variables
C
C
990   RETURN
      END

