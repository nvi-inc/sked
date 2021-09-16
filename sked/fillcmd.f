      SUBROUTINE fillCMD(cmdline)  !FILL COMMAND

! History
! 2016Dec09. First production version.
! 2017Mar22. Set knewsk=.true. indicating schedule has been changed. This insures we capture changes in writing. 
! 2018Feb13. KLB Take into account downtime
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  
! 2021-04-27 JMG replace (integer durscan) by (integer idurscan) and introduced (real*4 durscan)
C
C  FILLCMD checks a schedule for iddle time that can be used
C    to observe longer the previous scan, and
C    can automatically change the end time of the previous scan.
C  SKED commands: FILL 
C
      implicit none 
      include "../skdrincl/skparm.ftni"
      include "skcom.ftni"
      include "../skdrincl/sourc.ftni"
      include "../skdrincl/statn.ftni"
      include "../skdrincl/freqs.ftni"
      include "../skdrincl/skobs.ftni"
      include "major.ftni"
      include "../skdrincl/constants.ftni"
      include 'astro.ftni'
      include 'downtime.ftni'
C
C  INPUT VARIABLES:
C

! Input
      character*(*) cmdline

! Functions
      integer isecdif        !Difference between two times in seconds
      integer trimlen        !non-blank length of string
      integer igetsrcnum     !get the source number 
      integer iwhere_in_int_list
      logical kcont          !check to see if source is continous 
      logical kstatup        !check if station in downtime
C
C  CALLING SUBROUTINES: SKED
C  CALLED SUBROUTINES: splitntokens, gtdtr, wrerr, UNPAK, 
C                      when_at_next_source, cvpos, seconds2hms,
C                      pakup, indexxint,isecdif
C
C  LOCAL VARIABLES

!     Variables used to calculate idle time
      integer mjdtmp1,mjdtmp2,mjdtest,mjdtmp       ! MJD
      double precision uttmp1,uttmp2,uttest,uttmp  ! UT
      real tslew1,tslew2                           ! slewing times
      integer idleTIME                             ! idle time between 

      double precision RitimeDIFF                  ! time difference in double precision
      integer ikey(max_stn)
      logical found_idle                           ! loop to find appropriate idle time to add
      integer idurscan                             ! new duration of the scan
      integer idurscan_orig                        ! original scan duration 
      real*4 durscan 

      integer iset,imaxsl,isrc_time                ! local variables for when_at_next_source
      real buf_time                                ! local variable for when_at_next_source
      integer ih1,im1,is1

      logical found_stat                           ! logical to see if a station is from the selected list
      logical found_src                            ! logical to check if the source was selected
      logical found_time                           ! logical to check if selected time range

      integer istn(max_stn)                        ! list of stations
      integer i,ij,j                               ! indices for loops
      integer kerr                                 ! variable for unpak errors
      character*2 cwrap_new

! Managing downtime
      integer idown
      integer idur2down,idur2down_temp

! Managing end of session
      integer idurlast

! Optimization: we stop looping through the observations at end time range + MAXSCN+iMaxSlewTime
      integer jdenFI                               ! time (JD) when we finish to apply FILL
      double precision utenFI                      ! time (UT) when we finish to apply FILL
      integer diff_end                             ! time diff (sec) between current and
                                                   !    end of time range to study + MAXSCN+iMaxSlewTime
      logical found_end                            ! logical to check if we are at end time range+MAXSCN

      logical ksource_do(1:max_sor)                ! logical on all sources
      logical kstat_do(1:max_stn)                  ! logical on all stations FALSE if station is all done
                                                   !                         TRUE if station still has to be processed
      integer num_stat_to_do                       ! to keep track of the number of stations that need to be processed
      logical time_test                            ! logical to compare current time with beg/end selected
 
! Local variables to get info from the command ( from licmd.f )
      integer ich
      integer*2 lkeywd(12)
      character*22 ckeywd
      equivalence (lkeywd(2),ckeywd) 
      
      logical kdisplay         !print out the warning messages
      integer ierr             !some kind of error.
      integer istat            !station index
      integer iobs             !obs counter
      integer obsFIN           !last observation processed in the big while loop
      integer*4 itemp          !temporary variable 

! Variables for functions when_at_next_source and cvpos
      real*4 azbeg,elbeg,azend,elend,ha,dc,x30,y30,x85,y85 
      logical kup

! Variable dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=4)
      character*(2*max_stn) ltoken(MaxToken)

! ASTRO sources
      integer iastro_save(max_sor)                 ! indices of the ASTRO sources of the schedule
      integer num_save

! Command list
      integer icmd_list_len,icmd
      parameter (icmd_list_len=5)
      character*45 lcmd_list(icmd_list_len)
      character*20 lcmd_caps(icmd_list_len)
      character*65 lhelp(icmd_list_len)

      data (lcmd_list(i), lhelp(i), i=1,icmd_list_len)/
!1
     >"Fill [ TimeRange [ SourceList [ Subnet ]]]",
     >"                                          ",
!2
     >"TimeRange                                 ",
     >"Time range on which the command is applied",
!3
     >"SourceList           [All|One|Astro]      ",
     >"Sources on which the command is applied   ",
!4
     >"Subnet               <string>             ",
     >"Observing subnet, e.g.: KkWzHo or Kk-Wz-Ho",
!5
     >"Omitted arguments assume _ = ``all''      ",
     >"                                          "/

! Used to store token information
      integer*2 itemp_vec(10) 
      character*30 ltemp
      equivalence(ltemp,itemp_vec)     
      integer nst    !number of stations 
      integer iwhere   !number

C
C    0. Parse command and parameters.
C

      knewsk=.true.
      kdisplay=.true.

! Some initialization
      isorcm=0     ! all sources
      nst=0        ! all stations 

! Initialization of variables to process sources and stations
      ksource_do=.true.      ! we process all sources
      kstat_do=.false.       ! we process all stations in the session 
      do i=1,nstatn          ! nstatn:common variable with stat nb in the schedule
         kstat_do(i)=.true.
!         write(*,*) cstnna(i)
      enddo

      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)

! If the command is called with no argument: FILL
! decode the beg/end time of the session and put them in jdstcm / jdencm
      if (NumToken.eq.0) then
         ltoken(1)="BEG-END"
         lkeywd(1)=trimlen(ltoken(1))
         ltemp=ltoken(1)
         do i=1,(lkeywd(1)+1)/2
            lkeywd(1+i)=itemp_vec(i)
         enddo
         call gtdtr(lkeywd,ierrcm)
         if (ierrcm.ne.0) then
           call wrerr(ierrcm,inumcm)
           ierr=1
           return
         endif
      endif

! Help documentation for the command : FILL ?
      if (NumToken.eq.1 .and. ltoken(1).eq."?") then
         do icmd=1,icmd_list_len
            write(ludsp,'(a,1x,a)') lcmd_list(icmd),lhelp(icmd)
         enddo
         return
      endif

! Decode the time: FILL TimeRange ...
      if(NumToken.ge.1) then
         lkeywd(1)=trimlen(ltoken(1))
         ltemp=ltoken(1)
         do i=1,(lkeywd(1)+1)/2
            lkeywd(1+i)=itemp_vec(i)
         enddo
         call gtdtr(lkeywd,ierrcm)
         if (ierrcm.ne.0) then
           call wrerr(ierrcm,inumcm)
           ierr=1
           return
         endif
      endif

! Get the source number: FILL TimeRange Source ...
      if(NumToken .ge. 2) then
        ! unnecessary 3 lines
        if(ltoken(2) .eq. "_") then
           isorcm = 0         ! all sources
           ksource_do=.true.
        else
           ksource_do=.false.
           if ((ltoken(2).eq."ASTRO").or.(ltoken(2).eq."astro")
     >                               .or.(ltoken(2).eq."Astro")) then
             num_save=0
             do i=1,nsourc
                 if(kastro_src(i)) then 
  !                      write(*,*) i,csorna(i)
                      ksource_do(i)=.true.
                      num_save=num_save+1
                      iastro_save(num_save)=i
                 endif
             enddo
             if (num_save.ge.1) then
                 isorcm=-99
             endif
           else
             isorcm=igetsrcnum(ltoken(2))
             if(isorcm .eq. 0) then
               write(ludsp,*) "fillcmd: Source name not found"
               return
             else if(isorcm .eq. -1) then
               write(ludsp,*) "fillcmd: Ambiguous source name"
               return
             endif
             ksource_do(isorcm)=.true.
!          write(*,*) isorcm                ! source index
!          write(*,*) csorna(isorcm)        ! source name
           endif
        endif 
      endif

! Get the station list.
      if((NumToken .ge. 3).and.(ltoken(3).ne."_")) then
        kstat_do=.false.
        lkeywd(1)=trimlen(ltoken(3))
        ltemp=ltoken(3)
        do i=1,(lkeywd(1)+1)/2
          lkeywd(1+i)=itemp_vec(i)
        enddo 
        ich=1    !start at the first character
        CALL gtsti(lkeywd,ich,nst,istn,ierr,ludsp)
        if (ierr.ne.0) then
           write(ludsp,*) "fillcmd: Revise station names of the subnet"
           return
        endif
        do i=1,nst                            ! number of stations in the selected subnet
!           write(*,*) istn(i), cstnna(istn(i))   ! station index, station name
           kstat_do(istn(i))=.true.
        end do
      endif

      num_stat_to_do=count(kstat_do.eqv..true.)
!      write(ludsp,*) "Start time: ",jdstcm, utstcm
!      write(*,*) "End time:   ",jdencm, utencm   
!      write(*,*) "Num src :   ",count(ksource_do.eqv..true.),csorna(14)
!      write(ludsp,*) "Sources: ",ksource_do 
!      write(ludsp,*) "Num_stat_to_do= ",num_stat_to_do
!      write(ludsp,*) "Stations: ",kstat_do 

C Debugging purposes
!      do iobs=1,4 
C      read observation iobs and put it in cbuf
!       cbuf=cskobs(iskrec(iobs))
!       write(*,*) iskrec(iobs)
!       write(*,'(a90)') cbuf(1:90)
C      unpak the record found in ibuf and puts data into the
C      CUR variables
!       call unpak(kerr, 0) 
C      nstncur = number of stations
!       write(*,*) "POF",iobs,nstncur
C      istcur(1:nstncur) = station indices
!       write(*,*) istcur(1:nstncur)
C      cstnna = station names stored in station_state.ftni
!       write(*,'(32(a,1x))') (cstnna(istcur(i)),i=1,nstncur)
!       write(*,*) tim(iobs)
!       write(*,*) (mjdcur(istcur(i)),i=1,nstncur) 
!       write(*,*) (utcur(istcur(i)),i=1,nstncur) 
!       write(*,*) (idurcur(istcur(i)),i=1,nstncur) 
!       write(*,*) (idlcur(istcur(i)),i=1,nstncur)   
!      end do 

C
C  1. Initialize all stations to no observations in previous scan
C

      do i=1,max_stn
        iprevscan(i)=-1
      end do

C
C  2. Read first obs 
C

      cbuf=cskobs(iskrec(1))
      call unpak(kerr, 0)
      do ij=1,nstncur
            iprevscan(istcur(ij))=1
      end do

C
C  3. Pre-loop to begin processing observations only if we passed the date of the
C     "mother" scan which is later than (jdstcm,utstcm)
C
      iobs=2
      time_test=.false.
      do while ((iobs.le.nobs).and.(.not.time_test))
        cbuf=cskobs(iskrec(iobs))
        call unpak(kerr,0)
        if (((mjdcur(istcur(1))-jdstcm)*86400+
     >       (utcur(istcur(1))-utstcm)).gt.0) then
            time_test=.true.
        else
            iobs=iobs+1
        endif
!        write(*,*)" test ",iobs,time_test
      enddo
!     At the end of this loop, the variable iobs has become the first observation in the
!     selected time range that has to be processed
      if (iobs.gt.nobs) then
        write(ludsp,*) "fillcmd: The time range you entered is not valid 
     > (not in session time range)"
        return
      endif
C
C  4. Loop on all observations
C   
      found_end=.false.
! The loop is done until it is not needed : end of time range + MAXSCAN+iMaxSlewTime
! However, it will loop a last observation for nothing to avoid another if / then / else 
      do while (.not.found_end .and. iobs.le.nobs 
     >             .and. num_stat_to_do.gt.0)
        cbuf=cskobs(iskrec(iobs)) 
!       put the info of iobs in TST variables
!       => The TST variables correspond to the end of the scan we will study
        call unpak(kerr,1)
 
        ! check if the observation investigated is after the selected end time + MAXSCN+iMaxSlewTime
        call addsec2ut(jdencm,utencm,MAXSCN+iMaxSlewTime,
     >                 mjdtest,uttest)
        if (((mjdtst(isttst(1))-mjdtest)*86400+
     >                       (uttst(isttst(1))-uttest)).gt.0) then
            found_end=.true.
        endif
        
        do i=1,nstntst
            istat=isttst(i)
!           Here, a station will observe
!           a source even if it is alone --- it is corrected in phase 4
            if (kstat_do(istat) .and. iprevscan(istat).ne.-1) then             
            ! the station observed in a previous scan PLUS it is in the set of selected stations
                cbuf=cskobs(iskrec(iprevscan(istat))) ! latest scan of the station
                call unpak(kerr,0) ! unpak latest obs of the station in CUR variables
    
                ! check if observation is in window time selected
                found_time=.false.
                if (((mjdcur(istat).eq.jdstcm) .and. 
     >                               (utcur(istat).ge.utstcm)) .or. 
     >                               (mjdcur(istat).gt.jdstcm)) then
                   if (((mjdcur(istat).eq.jdencm) .and. 
     >                               (utcur(istat).le.utencm)) .or. 
     >                               (mjdcur(istat).lt.jdencm)) then
                     found_time=.true.
                   endif
                endif

!               give time tmp1 = time after observations + other time
!                                constraints + slewing
!                             => time when at next source
                call when_at_next_source(istat,nsorcur(istat),
     >               nsortst(istat),mjdcur(istat),utcur(istat),
     >               idurcur(istat),idlcur(istat),icalcur(istat),iset,
     >               cwrap_cur(istat),cwrap_tst(istat),tslew1,imaxsl,
     >               mjdtmp1,uttmp1,azbeg,azend,isrc_time,buf_time)
!               need information on the date at the end of the scan to check if we
!               are still in the time window
                mjdtmp=mjdtmp1
                uttmp=uttmp1

                if (ksource_do(nsorcur(istat)) .and. found_time) then 
                ! we continue if the source is the one selected or if no source was selected
                ! AND if we are in the selected time range
!   SEARCHING FOR THE IDLE TIME

! This is for debugging purposes to see positions of initial, final source. 
!               CALL CVPOS(NSORcur(istat),istat,mjdtmp1,uttmp1,
!     >                azbeg,elbeg,ha,  dc,x30,y30,x85,y85,kup)
!               CALL CVPOS(NSORtst(istat),istat,mjdtmp1,uttmp1,
!     >                azend,elend,ha,  dc,x30,y30,x85,y85,kup)

!              IDLE TIME:
! ifill_off is substracted from idleTIME to absorb differences between the slewing time model and the real slewing speed
                  idleTIME=isecdif(mjdtst(istat),uttst(istat),
     >                    mjdtmp1,uttmp1)-ifill_off

!               => we take the "floor" of the idle time, meaning the lowest integer part
!                  of the idle TIME
!   ADDING THE IDLE TIME TO THE DURATION OF THE OBSERVATION, UNLESS THE TIME AT THE END
!   OF THE SLEWING TIME no 2 IS LATER THAN THE TIME OF THE NEXT OBSERVATION
!   ATTENTION: LENGTH OF SCAN MUST BE LOWER OR EQUAL TO MAXSCAN

! Only do the loop if idletime >0. 
                  if(idleTIME.gt.0) then
! Set the new scan duration...
                    idurscan_orig=idurcur(istat)
! ATTENTION! If station is on downtime during the scan, idurscan should not exceed
! the time of beginning downtime
                ! check if station is in downtime during the window time selected
                    idur2down=0
                    do idown=1,num_down
                      if (istat.eq.idown_stat(idown)) then
                        idur2down_temp=isecdif(mjd_down_beg(idown),
     >                     ut_down_beg(idown),mjdcur(istat),
     >                     utcur(istat))
                        ! if idur2down is negative, that means we passed the downtime for the station
                        ! if idur2down is positive, that means we still have to manage the downtime
                        ! ATTENTION! It can be several downtimes for the same station! we need to check the closest
                        if (idur2down_temp.gt.0) then
                          ! this downtime happens before the previous one we detected
                          if ((idur2down.gt.0 
     >                     .and. idur2down_temp.lt.idur2down)
     >                     .or. (idur2down.eq.0)) then
                              idur2down=idur2down_temp
                          endif
                        endif
                      endif
                    enddo
                    ! we need to check the very last scans that are less than MASCAN seconds from the end of the session
                    idurlast=isecdif(jdencm,utencm,
     >                               mjdcur(istat),utcur(istat))
                    if (idurlast.lt.MAXSCN) then
                      if (idur2down.gt.0) then
                        idurscan=min(idurlast,(min(idur2down,
     >                            idurcur(istat)+idletime)))
                      else
                        idurscan=min(idurlast,idurcur(istat)+idletime)
                      endif
                    else
                      if (idur2down.gt.0) then
                        idurscan=min(min(MAXSCN,idur2down),
     >                              idurcur(istat)+idletime)
                      else
                        idurscan=min(MAXSCN,idurcur(istat)+idletime)   
                      endif
                    endif
                    found_idle=.false. 
                    do while(idurscan .gt. idurscan_orig .and. 
     >                                 .not. found_idle)  
                    call when_at_next_source(istat,nsorcur(istat),
     >                    nsortst(istat),mjdcur(istat),utcur(istat),
     >                    idurscan,idlcur(istat),
     >                    icalcur(istat),iset,
     >                    cwrap_cur(istat),cwrap_tst(istat),tslew2,
     >                    imaxsl,
     >                    mjdtmp2,uttmp2,azbeg,azend,isrc_time,buf_time)

!                 call allday(mjdcur(istat),nsorcur(istat),istcur(istat))
!!     >              tsris,tsset 

! Check if source is still visible at the time + idle time (TMP4)
                    call cvpos(nsorcur(istat),istat,mjdtmp2,uttmp2,
     >                    azbeg,elbeg,ha,dc,x30,y30,x85,y85,kup)
           
                    RitimeDIFF=(mjdtst(istat)-mjdtmp2)*86400.+
     >                                          (uttst(istat)-uttmp2)
 
!               We consider the "real" time difference
! Added in test for continuity of scan.   
                    durscan=idurscan
                    if (Ritimediff.ge.0 .and. kup   .and.
     >                 kcont(MJDcur(istat),UTcur(istat),durscan,
     >                 nsorcur(istat),istat,cwrap_cur(istat),ierr)) THEN
                        idurcur(istat)=idurscan
                        found_idle=.true.
                        mjdtmp=mjdtmp2
                        uttmp=uttmp2
                     else
                        idurscan=idurscan-1   
                     endif  
                   end do                  
                  endif

! DEBUGGING/PRINTING
!               if(.true.) then   
                 if(.false.) then   
!                if (idleTIME.lt.0) then
                  write(*,*) "Idle time negative: ",idleTIME
                  write(*,"('Az El beg', 2f9.2)")
     >             rad2deg*azbeg,rad2deg*elbeg
                  write(*,"('Az El end', 2f9.2)")
     >             rad2deg*azend,rad2deg*elend
                  write(*,*) "WRAPS ", cwrap_cur(istat), cwrap_new
!                if (cstnna(istat).eq.'NYALES20') then
                  write(*,*) "Observation: ",iskrec(iobs)
                  write(*,*) cstnna(istat)
                  call seconds2hms(utcur(istat),ih1,im1,is1)
                  write(*,*) "Beginning:  ",mjdcur(istat),
     >                    utcur(istat), csorna(nsorcur(istat)),
     >                    ih1,im1,is1
                  write(*,*) "durcur:    ",idurcur(istat),idlcur(istat),
     >                    icalcur(istat),iset
                  write(*,*) " Slewing: ",tslew1!,itslew1
                  write(*,*) "Beg+dur+idle+sle1:",mjdtmp1,uttmp1
                  write(*,*) "Idle time:  ",idleTIME 
                  write(*,*) "Slewing2: ",tslew2!,itslew2
                  write(*,*) "+slewing(2):",mjdtmp2,uttmp2
                  write(*,*) "End:        ",mjdtst(istat),
     >                    uttst(istat),csorna(nsortst(istat))
                  write(*,*) "Diff of time between calculated and new: "
     >                    ,RitimeDIFF!,itimeDIFF  
                 end if
! END DEBUGGING

                  call pakup(kerr,0)
                  cskobs(iskrec(iprevscan(istat)))=cbuf
                endif

! Check if the station previous scan will end before the end of the selected time
! If not, that means the station does not need to be processed anymore
                if (((mjdtmp-jdencm)*86400+(uttmp-utencm)).gt.0) then 
                      kstat_do(istat)=.false.
                      num_stat_to_do=num_stat_to_do-1
                endif
            endif 
            iprevscan(istat)=iobs
        enddo
        iobs=iobs+1
      enddo
!      write(ludsp,*)"Nb stations left to do: ",num_stat_to_do
!      write(ludsp,*)"IOBS: ",iobs,nobs  
!      write(*,'(20(a8,1x))') cstnna(1:nstatn)
!      write(*,'(20(l8,1x))') kstat_do(1:nstatn)
!      write(*,'(20(i8,1x))') iprevscan(1:nstatn) 
 
C  5. Processing of the stations that are not done yet
C
!       if(.true.) then 
   
       do i=1,nstatn
!         ! we search for the stations that have not been processed totally
        if(.false.) then   !debugging. 
         write(*,*) i, cstnna(i),  kstat_do(i)
         write(*,'(20(a8,1x))') cstnna(1:nstatn)
         write(*,'(20(l8,1x))') kstat_do(1:nstatn)
         write(*,'(20(i8,1x))') iprevscan(1:nstatn) 
        endif 
         if (kstat_do(i)) then
! Found a station that has not been processed. Unpack the previous scan it was in. 
            cbuf=cskobs(iskrec(iprevscan(i)))    
            call unpak(kerr,1) ! observations in CUR variables
!               write(*,*) "nstntst"
! Now loop through all stations in this scan and pick out the ones that have not been done yet.
            do j=1,nstntst
               istat=isttst(j)   
               if(kstat_do(istat) .and. 
     >             iprevscan(i) .eq. iprevscan(istat)) then
   
! calculate time from start of scan to end time. This is one option for the duration. 
                 itemp=(jdencm-mjdtst(istat))*86400 +utencm-uttst(istat)  
! The actual duration is the minimum of itemp and MAXSCN
! ifill_off is a safety feature to absorb possible differences between slewing time models and real values
                 idurtst(istat)=min(MAXSCN,itemp)-ifill_off 
! Set the station to found.                    
                  kstat_do(istat)=.false.
               endif 
            end do 
! Packup the scan and store it.     
            call pakup(kerr,1)
            cskobs(iskrec(iprevscan(i)))=cbuf       
            num_stat_to_do=num_stat_to_do-1    
         endif
      enddo
!      stop
!      endif 
!      stop 

!      write(ludsp,*) "Num stations left: ",num_stat_to_do
C
C  6. Loop on all stations/observations to remove extra time (2 stations at least observing)
C   
      do iobs=1,nobs
         cbuf=cskobs(iskrec(iobs))
         call unpak(kerr, 0)
!        order the duration times from smallest to biggest
         call indexxint(nstncur,idurcur(istcur),ikey)
!        give to the longest duration the second longest duration
         if ((idurcur(istcur(ikey(nstncur)))) .ne.
     >            (idurcur(istcur(ikey(nstncur-1))))) then
             idurcur(istcur(ikey(nstncur)))=
     >                        idurcur(istcur(ikey(nstncur-1)))
         endif
         call pakup(kerr, 0)
         cskobs(iskrec(iobs))=cbuf
         write(ludsp,*) cbuf(1:34) 
      end do
      write(ludsp,*) "End of FILL command."

      return
      end
