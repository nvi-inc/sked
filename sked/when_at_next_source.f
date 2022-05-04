      subroutine when_at_next_source(kdisplay,luout,
     >  istat,nsor,nsornew,mjd,ut,
     >  idur,idle,ical,cwrap,cwrap_new,mjd_out,ut_out,
     >  aznow,elnow,aznew,elnew,tslew, 
     >  isetup_time,isrc_time,ibuf_time,ierr) 

! 2022-05-05 JMG. Calculate appropriate for first scan for az-el antennas. 
! 2021-02-19 JMG slewt2 replaced by slew
! 2020Jun08 JMG. include broadband.ftni. New parameter ibb_off 


      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni'

! Passed variables 
      logical  kdisplay      !display results
      integer  luout 
      integer istat          !station
      integer nsor, nsornew  !source, source to slew to
  
      integer mjd            !start MJD
      double precision ut    !start UT
      integer idur           !duration of scan
      integer idle
      integer ical     
      character*2 cwrap      !current wrap
! Returned variables
      character*2   cwrap_new  !new cablewrap  
      integer mjd_out          !time when at source
      double precision ut_out  !UT time at source.     
      
      real     aznew, aznow    !starting ending positions.
      real     elnew, elnow   
      real     tslew           !time to slew
      real     azwrap          !aznew including wrap. 
      
      integer isetup_time      !setup time
      integer isrc_time        !source time
      integer ibuf_time        !extra buffer time 
   
      integer ierr 
      
! local   
      real     trise
      integer islew_info       !info on kind of slew.
      integer iUT_out          !integer part of UT_out
      integer irem             !remainder

      double precision ut_scan_end     
      integer ihr, imin,isec 
 
      real  elend,ha,  dc,x30,y30,x85,y85
      real amaxsl            !Maximum slewing time
      logical kup
      ierr =0 
      aznow=0.d0
      aznew=0.d0
      elnow=0.d0
      elnew=0.d0
      mjd_out=0
      ut_out=0 
      islew_info =0 
      kup=.true. 
      ut_scan_end=ut+idur+idle
      call seconds2hms(ut_scan_end,ihr,imin,isec)
      if(cwrap(1:1) .eq. char(0)) cwrap="-"
      cwrap_new=cwrap                          !This is default. No change. 
      IF(nsor.GT.0 .and. nsor .ne. nsornew) then  
         CALL SLEWT(nsor,mjd,ut_scan_end,
     >     nsornew,istat,cwrap,cwrap_new,tslew,
     >     lookah,trise,tsris,st0cur,frac,knov,islew_info,
     >     aznow,elnow,aznew,elnew)
           ierr=islew_info      
           if(islew_info .eq. -4) then     
              kup=.false.
           endif      
      ELSE  
        tslew = 0.0 
        aznow=0.d0     
        CALL CVPOS(NSORnew,istat,mjd,ut_scan_end,
     >      aznew,elnew,ha,  dc,x30,y30,x85,y85,kup)                  
        if(.not.kup) ierr=-1     
! If AZ-EL and first source, may need to adjust wrap.   
        IF ((IAXIS(ISTat).EQ.3 .or. iaxis(istat).eq. 6 .or. 
     &      iaxis(istat).eq.7) .and. nsor .eq. 0) then           
! 1. calculate the Az including the wrap. 
          azwrap=aznew
          if(aznew .lt. stnlim(1,1,istat))  azwrap=azwrap+360.d0
!2. If we are in the wrap reason, set the wrap to "W")          
          if(azwrap .lt. stnlim(2,1,istat)) cwrap_new="W"                           
        endif 
      ENDIF
  
      if(ierr .ne. 0) then
        if(.not.kup .or. islew_info .eq. -4) then 
          if(kdisplay) then                              
             write(luout,
     >      '("ERROR! when_at_next_source: At ", 2(i2.2,":"),i2.2, $)') 
     >        ihr,imin,isec
            write(luout,'(" source ",a8," not visible at ", a, 2f8.2 )') 
     >       csorna(nsornew), cstnna(istat), aznew*rad2deg,elnew*rad2deg
          endif 
        endif     
        return
      endif         
C     Determine procedure times to be added between runs
 
C     Move setup procedure calculation after parity, so that ipar is defined.
C     Setup procedure time:
        ISETUP_Time = 0
        if (tape_motion_type(istat).eq.'CONTINUOUS'.or. 
     &      tape_motion_type(istat).eq.'ADAPTIVE'  .or.
     &      tape_motion_type(istat).eq.'AUTO') then 
          continue 
        else
C       For start&stop, do setup on all scans if flag is set.
          IF (KFLG(1))  ISETup_time = ISETTM
        endif
  
        ibuf_time=0.0    !extra time to record the data. 
        if(cstrec(istat,1) .eq. "Mark6" .or.idata_mbps(istat).gt.0) then
           if(isink_mbps(istat) .eq. 0) then 
             write(*,*) "isink_mbps is 0 for station ", cstnna(istat)
             stop
           endif                 
           ibuf_time=iMark6_off+ibb_off(istat)
! Note: Recording data during taking. 
           if(idata_mbps(istat) .gt. isink_mbps(istat)) then
             ibuf_time=ibuf_time+idur*
     &         (idata_mbps(istat)-isink_mbps(istat))/isink_mbps(istat)
           endif                           
        endif     
    
! Default ending time is + duration.
        UT_out=ut+idur+idle 

! If already on source, don't need extra source time. 
        if(tslew .lt. 0.5) then  
           isrc_time=0
        else
           isrc_time=isortm
        endif
        if(nsor .le. 0) then
           amaxsl=0.d0
        else 
          AMAXSL = AMAX1(tslew, float(isetup_time+ibuf_time))   
          UT_out = UT_out+ICAL+isrc_time+ITAPTM
        endif ! continuous or not
            
        Amaxsl = amax1(amaxsl,float(imintm))             
        UT_out=UT_out+amaxsl

! Round up to modular unit of time
        iUT_out=int(UT_out+0.9)
        UT_out=iUT_out
        if(imodtm .gt. 1) then
          irem=mod(iUT_out,imodtm)
          if(irem .ne. 0) then
             irem=(imodtm-irem)
          endif
        else
          irem=0
        endif

        call addsec2ut(mjd,UT_out,irem,MJD_out,UT_out)
        return
        
! common error exit.         
500     continue        
        
        end 




