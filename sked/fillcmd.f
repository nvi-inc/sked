      SUBROUTINE fillCMD(cmdline)  !FILL COMMAND

! History
! 2016Dec09. First production version.
! 2017Mar22. Set knewsk=.true. indicating schedule has been changed. This insures we capture changes in writing. 
! 2018Feb13. KLB Take into account downtime
! 2019Mar16  JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  
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
      integer*4 isecdif        !Difference between two times in seconds
      integer trimlen          !non-blank length of string
      integer igetsrcnum       !get the source number     

      real*4 azwrap            !az position including wrap 
      integer*4 itime_to_down    !How long to next down time 
     
C
C  CALLING SUBROUTINES: SKED
C  CALLED SUBROUTINES: splitntokens, gtdtr, wrerr, UNPAK, 
C                      when_at_next_source, cvpos, seconds2hms,
C                      pakup, indexxint,isecdif
C
C  LOCAL VARIABLES

!     Variables used to calculate idle time
! Note: Would like to say mjdcur etc but these are already used. 
      integer mjdnow                               ! Current scan time
      double precision utnow                       !
      integer mjdprv                               ! Time of previous scan a station was in. 
      double precision utprv                       !      
      integer isrc_now, isrc_prv                   ! Current and previous source number. 
      integer iprv_scan(max_stn)                   ! array containing previous scans. 
      character*2 cwrap_now, cwrap_prv             ! Cable wrap current, previous 
      integer idur_prv                             ! iduration of previous scan for some station.
      integer idl_prv,ical_prv                     ! Idle and cal time previous 
      integer iprv_scan_ptr                        ! Pointer to previous scan
      
      integer mjdtmp                               ! Temporary times
      double precision uttmp       
      
      integer itime_dif                            ! difference between two epochs in seconds                              
      integer idleTIME                             !idle time between end of prev scan after slewing and start of next.
      integer idur2down                            !time from start of prev scan to next downtime.          

      integer ikey(max_stn)                        ! used in cleanup at the end to order durations within a scan. 
      integer idurscan                             ! new duration of the scan
   
      integer idur_max                             ! maximum duration we can do. 
      integer idur_2max                            
      integer istat_max_dur                        ! station which has the longest duration
      integer istat_2max_dur                       ! station with second longest duration
     
      integer ih1,im1,is1
      
      integer i,ij,j                               ! indices for loops
      integer kerr                                 ! variable for unpak errors
      
      integer iobs_beg_fill                         ! first scan that was modified
      integer iobs_end_fill                         ! last scan that was modified 
      
      logical kdebug_fill
      
   
! This is used to unpack stations in argument list to fillcmd. 
      integer istn(max_stn)                        ! list of stations  in a scan.
      integer nst                                  ! number of stations in a scan.

! these are the stations and sources that we consider for fillcmd. If not in this list don't try to fill the time. 
      logical ksource_do(1:max_sor)                ! logical on all sources
      logical kstat_do(1:max_stn)                  ! logical on all stations FALSE if station is all done
                                                   !                         TRUE if station still has to be processed
      
! Local variables to get info from the command ( from licmd.f )     
      logical kdisplay         !print out the warning messages
      integer ierr             !some kind of error.
      integer istat            !station index
      integer iobs             !obs counter     

! Variables for use with cvpos    
      real*4 ha,dec,x30,y30,x85,y85   
      logical kup
      real*4  AZ1,EL1
      real*4  AZ2,EL2
! some variables used by when_at_next_source     
      real tslew                                   ! slewing times        
      integer isetup_time                           
      integer isrc_time                             
      integer ibuf_time                             
      real*4  azbeg,elbeg,azend,elend              !starting az (at current source), ending az (at next source) 

! Variable dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=4)
      character*(2*max_stn) ltoken(MaxToken)
! Variables dealing with keywords. 
! Note: lkeywd(1) contains length of keyword.
!       lkeywd(2:) contians the rest of the keyword.      
      integer ich
      integer*2 lkeywd(max_stn+1)
      character*(2*max_stn) ckeywd
      equivalence (lkeywd(2),ckeywd)      

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

! Beginning of code. 

! Some initializaition
      knewsk=.true.                  
      kdisplay=.true.
      kdebug_fill=.false. 

! Parse command line.  See above for valid arguments. 

! "Fill ?"  means list the options  
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
! Help documentation for the command : FILL ?
      if (NumToken.eq.1 .and. ltoken(1).eq."?") then
         do icmd=1,icmd_list_len
            write(ludsp,'(a,1x,a)') lcmd_list(icmd),lhelp(icmd)
         enddo
         return
      endif       
      
      if(nobs .eq. 0) then
        write(*,*) "fillcmd: Can't run on an empty schedule."
        return
      endif     
      if(kdebug_fill)  write(*,*) "NOBS ", nobs 
    
! Default is process all stations, all sources for entire span of schedule.
! This may be changed below. 
      ksource_do=.true.      ! we process all sources
      kstat_do  =.true.      ! we process all stations in the session 
      iprv_scan = 0     
       
! Decode the time: FILL TimeRange ...
! if the command is called with no argument, then do the entire time span.'
! If called with 1 argument which is not "?" then it is a time argument. 

! decode the beg/end time of the session and put them in jdstcm / jdencm
      if(NumToken.le.1) then
         if(NumToken .eq. 0) ltoken(1)="BEG-END"     !set up default argument. 
!stuff the first argument into ckeywd and decode it.          
         ckeywd=ltoken(1)
         lkeywd(1)=trimlen(ckeywd) 
         call gtdtr(lkeywd,ierrcm)
         if (ierrcm.ne.0) then
           call wrerr(ierrcm,inumcm)
           ierr=1
           return
         endif
      endif            
      
! Here we check that the fill window overlaps the observation window.
      if(kdebug_fill) then 
        write(*,*) "FILL_BEG ",jdstcm,utstcm
        write(*,*) "FILL_END ",jdencm,utencm
      endif   

! Unpak the first observation and see if it is after fill window.
! Unpak the last  observatoin and see if it is before fill window. 
      do i=1,2
        if(i .eq. 1) then
          iobs=1
        else
          iobs=nobs
        endif 
        cbuf=cskobs(iskrec(iobs))
        call unpak(kerr,0)

        istat=istcur(1)
        mjdtmp=mjdcur(istat)
        uttmp=utcur(istat) 
        if(i .eq. 1) then
         if(kdebug_fill) write(*,*) "SKED_BEG ", MJdtmp,UTtmp
          if(isecdif(mjdtmp,UTtmp,jdencm,utencm).gt.0) then
            write(*,*) "fillcmd:  Fill window after last observation "
            return
          endif
        else
          if(kdebug_fill) write(*,*) "SKED_END ", MJDtmp,uttmp
          if(isecdif(mjdtmp,uttmp,jdstcm,utstcm).lt.0) then
            write(*,*) "fillcmd:  Fill window before last observation "
            return
         endif
        endif 
      end do     
       
! Get the source number: FILL TimeRange Source ...
! ltoken(2) .eq. "_" means all 
      if(NumToken .ge. 2 .and. ltoken(2) .ne. "_") then 
        ksource_do=.false.
        call capitalize(ltoken(2)) 
        if(ltoken(2) .eq. "ASTRO") then 
          ksource_do=kastro_src
          if(count(kstat_do.eqv..true.) .eq. 0) then
            write(*,*) 
     &        "fillcmd: Specified ASTRO but no astro sources. Returning"
            return
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

! Get the station list.
      if((NumToken .ge. 3).and.(ltoken(3).ne."_")) then
        kstat_do=.false.
        ckeywd=ltoken(3) 
        lkeywd(1)=trimlen(ckeywd)  
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

! Below we loop through the observations putting.
! The CUR scan is the current scan we are considering. 
! The TST scan is the previous scan for a given station in the CUR scan.  
 
! initialize the first and last scan modified. 
! These will be updated below. 
      iobs_beg_fill=nobs   
      iobs_end_fill=1   
      kdebug_fill=.false.
! Default is to loop over all scans, but have an alternative exit if past filltime.      
      write(ludsp,'(a,$)') "Starting fill_in: "  
      do iobs=1, nobs               !loop over all the observations.     
        write(ludsp,'(a,$)') "."    
! put the current observation in the CUR variables.  
        idurcur=0                   ! This makes debugging easier.     
        cbuf=cskobs(iskrec(iobs)) 
        call unpak(kerr,0)        
        if(kdebug_fill) then      
          write(*,*) "-----New Scan ------------------"    
          write(*,*) "CUR --> ", trim(cbuf)       
        endif         
          
        istat=istcur(1)                !This is used just to get time and source. 
        mjdnow=mjdcur(istat)
        utnow =utcur(istat) 
        isrc_now=nsorcur(istat) 
        cwrap_now=cwrap_cur(istat) 

        if(kdebug_fill) then 
          write(*,*) "CUR_TIME ", mjdnow,utnow               
          write(*,'(15i4)') idurcur(1:11)
         endif  
        
! Check to see if past where we should start. 
        if(isecdif(mjdnow,utnow,jdstcm,utstcm) .lt. 0) cycle                         
        
! One consequence of cycling now is that iprv_scan does not get updated until AFTER we are in the time window. (which is what we want.)         

! Loop through all the stations in the current scan.
        do i=1,nstncur
          istat=istcur(i)         
          iprv_scan_ptr=iprv_scan(istat)        ! do this way because we want to keep a copy before we update below
          iprv_scan(istat)=iobs                 ! Update the previous scan to point to this one 
          if(kdebug_fill) then 
             write(*,*) ">>>>>>>Station ", cstnna(istat),iprv_scan_ptr                                                
          endif 
          if(iprv_scan_ptr .eq. 0) cycle         !CYCLE: Didn't observe previously (no previous scan.)                
          
          idurtst=0                             !Initialize. Don't really need to do this, but makes debugging easier.                 
          cbuf=cskobs(iskrec(iprv_scan_ptr))    ! previous scan for this station.                                                 
          call unpak(kerr,1)                    ! unpak latest obs of the station in TST variables     
          if(kdebug_fill)  write(*,*) "PRV --> ", trim(cbuf)                   
          
          mjdprv    =mjdtst(istat)              !extract some variables 
          utprv     =uttst(istat) 
          isrc_prv  =nsortst(istat) 
          cwrap_prv =cwrap_tst(istat)   
          idur_prv  =idurtst(istat)
          idl_prv   =idltst(istat)
          ical_prv  =icaltst(istat) 
          
          if(kdebug_fill) then
             write(*,*) "Doit ",ksource_do(isrc_prv),kstat_do(istat) 
          endif 
          
          if(.not.ksource_do(isrc_prv)) cycle                          !CYCLE: Not a source that we do fill for. 
          if(.not.kstat_do(istat))    cycle                            !CYCLE: not doing fill for this station. 
           
          if(kdebug_fill)  write(*,*) "PRV_TIME ", mjdprv,utprv            
 
!  See how long it takes to get from previous source to current source. 
          call when_at_next_source(kdisplay,ludsp,
     >         istat, isrc_prv,isrc_now,mjdprv,utprv, 
     >         idur_prv, idl_prv,ical_prv,
     >         cwrap_prv,cwrap_now, 
     >         mjdtmp,uttmp,azbeg,azend,elbeg,elend,tslew,
     >         isetup_time,isrc_time,ibuf_time,ierr)           
           if(kdebug_fill)   write(*,*) "FRE_TIME ", mjdtmp,uttmp       
                
  
! The idle time is difference between when we arrive at current source (mjdtmp) and when the current scan starts. 
          itime_dif=isecdif(mjdnow, utnow,mjdtmp,uttmp)      
          if(kdebug_fill) then
             write(*,'(a,2i5)') "idurtst, IDLETIME", idur_prv,itime_dif 
          endif 
 ! ifill_off is substracted from idleTIME to absorb differences between the slewing time model and the real slewing speed         
          idleTime=itime_dif-ifill_off 
          if(idleTime .le. 0) cycle                                      !CYCLE:  No time to add for this staiton. 
 
! Find time to next downtime 
          idur2down=itime_to_down(mjdprv,utprv,istat)
! Find the maximum duration for this station in this scan.

          idur_max=min(idur2down,Maxscn,idur_prv+idleTime)
          if(kdebug_fill) then 
            writE(*,'(a,4i8)') "idown,Maxscan,idurnew ",
     &       idur2down,maxscn,idur_prv+idleTime,idur_max
          endif 
          if(idur_max .eq. idur_prv) cycle                         !CYCLE: No room to increase the scan length. 

! Find position at the start of the previous scan 
          CALL CVPOS(isrc_prv, istat,MJDprv,UTprv,AZ1,EL1,
     &       HA,DEC,X30,Y30,X85,Y85,KUP)

! Az-el type antennas. Correct az1 for cable wrap. 
          IF (IAXIS(ISTat).EQ.3.or.iaxis(istat).eq.7.or.
     &      iaxis(istat).eq.6) then
            Az1=azwrap(az1,cwrap_prv,stnlim(1,1,istat))    
          endif  

! Find maximum duration by starting at current and increaseing to idur_max.
! If we encounter problems along the way stop. 
! Increase scan length by 1 second.  See if any problems.
!    No problems, continue.     
!    Yes problems, stop.   
          do idurscan = idur_prv+1, idur_max 
!            write(*,*) "Idurscan ", idurscan 
            CALL CVPOS(isrc_prv,IStat,MJDNow,UTprv+idurscan,
     &                 AZ2,EL2,HA,DEC,X30,Y30,X85,Y85,KUP)
            if(.not.kup) then
               if(kdebug_fill) write(*,*) "Not up at end "
                exit                               !EXIT source is not up at the end of the scan.
            endif 
  
! now check to see if there are problems with cable wrap.            
            IF (IAXIS(ISTat).EQ.3.or.iaxis(istat).eq.7.or.
     &         iaxis(istat).eq.6) then    
               az2=azwrap(az2,cwrap_prv,stnlim(1,1,istat)) !Put this on the correct wrap.                                   
! now both az1 and az2 are corrected  for cable wrap. 
! Want the distance between them to be small. If not then either.
!   1. Start at upper end of C-wrap and are trying to go to neutral.
!   2. Start at lower end of W-wrap and are trying to go to neutral. 
               if(abs(az2-az1) .gt. .1) then
                   if(kdebug_fill) then
                     write(*,*) "Cable wrap problem ",
     &               azbeg*rad2deg,az1*rad2deg,az2*rad2deg, 
     &               stnlim(1:2,1,istat)*rad2deg
                   endif 
                   exit              !Exit because a large difference. Indicates cable-wrap problem. (.1 ~ 5.7 deg)                
                endif 
            Endif   
! Make sure we can still get to the next source in time from this position. 
          call when_at_next_source(kdisplay,ludsp,
     >         istat, isrc_prv,isrc_now,mjdprv,utprv, 
     >         idurscan, idl_prv,ical_prv,
     >         cwrap_prv,cwrap_now, 
     >         mjdtmp,uttmp,azbeg,azend,elbeg,elend,tslew,
     >         isetup_time,isrc_time,ibuf_time,ierr)           
       
            idleTIME=isecdif(mjdnow,utnow,mjdtmp,uttmp)-ifill_off         
            if(idleTime .lt. 0) then
               if(kdebug_fill) write(*,*) "idletime EXIT ", idletime
               exit            
             endif 
          end do 
! At this point idurscan is 1 larger than maximum good scan lenghth.
          idurscan=idurscan-1
          idurtst(istat)=idurscan
          call pakup(kerr,1)
          cskobs(iskrec(iprv_scan_ptr))=cbuf
          if(kdebug_fill) then 
            write(*,*) "Final duration ",idurscan      
            write(*,'(15i4)') idurtst(1:11)         
          endif 
          iobs_beg_fill = min(iobs_beg_fill,iprv_scan_ptr) 
          iobs_end_fill = max(iobs_beg_fill,iprv_scan_ptr) 
! If mjdtmp,uttmp (time arrive at next source) after FILL window, then turn off 
          if(isecdif(jdencm,utencm,mjdtmp,uttmp).le. 0) then 
            if(kdebug_fill) then
              write(*,*) "TMP_TIME ",mjdtmp,  uttmp 
              write(*,*) "Turning off ",cstnna(istat)
              write(*,*) "Secdif ", isecdif(jdencm,utencm,mjdtmp,uttmp)
            endif 
            kstat_do(istat)=.false.                                !Past the end of the time window. Turn off this station
          endif 
!          pause 
        enddo       !loop over stations in scan.         
!  check if the observation investigated is after the selected end time + MAXSCN+iMaxSlewTime 
        if(isecdif(mjdnow,utnow,jdencm,utencm).gt.0) exit      !past the end of the time window.
!     &       .ge. Maxscn+imaxslewTime     
      enddo         !loop over observations. 
     
      iobs_end_fill=iobs 
! 
!  Processing of the stations that are not done yet (kstat_do(i) is TRUE) 
!  this could happen because the previous scan it was in was a long time before the end of the FILL window. 

      if(kdebug_fill) then 
        write(*,*) "Fixing up"       
        do i=1,nstatn
         write(*,*) i, cstnna(i),kstat_do(i), iprv_scan(i)
        end do
      endif 
         
      do i=1,nstatn
        if(.not. kstat_do(i)) cycle          !CYCLE:  Previosly done. Don't need to do anything. 
        iprv_scan_ptr=iprv_scan(i)
! Found a station that has not been processed. Unpack the previous scan it was in. 
        if(iprv_scan_ptr.eq. 0) then
          kstat_do(istat)=.false.
          cycle
        endif
         
        idurtst=0           !Not necessary but makes debugging easier.          
        cbuf=cskobs(iskrec(iprv_scan_ptr))   
        call unpak(kerr,1) ! observations in TST variables
        if(kdebug_fill) then
          write(*,*) cstnna(i), iprv_scan_ptr
          write(*,*) "----> ", trim(cbuf) 
          write(*,'("Idur before ", 11i4)') idurtst(1:11) 
        endif 
     
! Now loop through all stations in this scan and pick out the ones that have not been done yet.
        do j=1,nstntst
           istat=isttst(j)   
           if(.not.kstat_do(istat)) cycle                  !CYCLE: This station has been processed.   
           if(iprv_scan(istat) .ne. iprv_scan_ptr) cycle    !
           if(kdebug_fill) write(*,*) cstnna(istat), iprv_scan(istat)  
! calculate time from start of scan to end of fill window. This is one option for the duration. 
           itime_dif=isecdif(jdencm,utencm,mjdtst(istat),uttst(istat))   
! another option is until next downtime.                     
           idur2down=itime_to_down(mjdtst(istat),uttst(istat),istat)           
           idurscan=min(itime_dif,idur2down,Maxscn) 
! NEVER reduce a scan.             
           idurtst(istat)=max(idurscan,idurtst(istat))
! Set the station to found.                    
            kstat_do(istat)=.false.  
         end do
! Packup the scan and store it.     
         call pakup(kerr,1)
         cskobs(iskrec(iprv_scan_ptr))=cbuf      
         iobs_beg_fill = min(iobs_beg_fill,iprv_scan_ptr) 
         iobs_end_fill = max(iobs_beg_fill,iprv_scan_ptr) 
         if(kdebug_fill) then
           write(*,*) cstnna(i), iprv_scan_ptr
           write(*,*) "----> ", trim(cbuf) 
           write(*,'("Idur after ", 11i4)') idurtst(1:11) 
         endif 
      enddo
      
! Make sure no station observes alone          

! Loop on all observations modified  to do some cleanup.
!   Above we increased the duration so that each station arrived on source for next scan just in time.
!   But this means that we can have the situation where one station is obsrerving alone. 
!   In this case change the duration of the longest station to that of the second longest 
C   
      ikey=0
      do iobs=iobs_beg_fill, iobs_end_fill
        cbuf=cskobs(iskrec(iobs))   
!        write(*,*) "BEF ",trim(cbuf)                    
        idurtst=0                    !This initializes and sets all values to 0. Need to do this for sorting.                                   
        call unpak(kerr, 1)          !Unpak will update the durations of the stations in the scans, but other stations values are leftover. 
!        write(*,'(11i4)') idurtst(1:11) 
        
! find the  station which has maximum and 2nd maximum duration         
        istat_max_dur  =0
        istat_2max_dur =0
        idur_max       =0
        idur_2max      =0   
! First off find the longest duration              
        do j=1,nstntst
          istat=isttst(j)   
! This duration is candidate for being longest          
          if(idurtst(istat) .gt. idur_max) then
            idur_2max=idur_max                       !set previous longest to second longest
            istat_2max_dur=istat_max_Dur              
            idur_max=idurtst(istat)                  !update the longest    
            istat_max_dur =istat         
          else if(idurtst(istat) .gt. idur_2max) then
! This duration is not the longest, but it might be the second 
            idur_2max=idurtst(istat)        
            istat_2max_dur =istat
          endif               
        end do          
        idurtst(istat_max_dur)=idurtst(istat_2max_dur)        
        call pakup(kerr, 1)
        cskobs(iskrec(iobs))=cbuf

      end do
      write(ludsp,*) " " 
      write(ludsp,*) "End of FILL command."

      return
      end
