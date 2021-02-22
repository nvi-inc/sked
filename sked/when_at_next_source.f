      subroutine when_at_next_source(istat,nsor,nsornew,mjd,ut,
     >  idur,idle,ical,iset,
     >   cwrap,cwrap_new,tslew,imaxsl,mjd_out,ut_out,
     >   aznow,aznew,isrc_time, buf_time) 

! 2021-02-19 JMG slewt2 replaced by slew
! 2020Jun08 JMG. include broadband.ftni. New parameter ibb_off 


      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni'

      integer istat          !station
      integer nsor, nsornew  !source, source to slew to
  
      real     aznew, aznow 
      integer mjd            !start MJD
      double precision ut   !start UT
      integer idur           !duration of scan
      integer idle
      integer ical
      integer iset           !returned...

      character*2   cwrap, cwrap_new    !start and end cablewrap
      real     tslew         !time to slew
      integer mjd_out        !time when at source
      double precision ut_out  !UT time at source.      

! local 
      real     trise
      integer islew_info     !info on kind of slew.
      integer iUT_out        !integer part of UT_out
      integer irem           !remainder
      integer isrc_time      !extra time to allow antenna to settle
      real     buf_time      !extra time to record scan.
      real     rec_factor  !ratio of data_taking/data_recording

      real     amaxsl
      integer imaxsl
      integer iq

      real  elend,ha,  dc,x30,y30,x85,y85
      logical kup

       if(cwrap(1:1) .eq. char(0)) cwrap="-"
       IF  (nsor.GT.0 .and. nsor .ne. nsornew) then  
          CALL SLEWT(nsor,mjd,ut+IDUR+Idle,
     >     nsornew,istat,cwrap,cwrap_new,tslew,
     >     lookah,trise,tsris,st0cur,frac,knov,islew_info,
     >     aznow,aznew)
        ELSE  
          tslew = 0.0 
          aznow=0.d0
          CALL CVPOS(NSORnew,istat,mjd,ut+idur+idle,
     >      aznew,elend,ha,  dc,x30,y30,x85,y85,kup)
 
        ENDIF
C     Determine procedure times to be added between runs

 
C     Move setup procedure calculation after parity, so that ipar is defined.
C     Setup procedure time:
        ISET = 0
        if (tape_motion_type(istat).eq.'CONTINUOUS'.or. 
     &      tape_motion_type(istat).eq.'ADAPTIVE') then
        else
C       For start&stop, do setup on all scans if flag is set.
          IF (KFLG(1))  ISET = ISETTM
        endif
        if (tape_allocation(istat).eq.'AUTO') iset = 0

        buf_time=0.0    !extra time to record the data. 
        if(cstrec(istat,1) .eq. "Mark6" .or.idata_mbps(istat).gt.0) then
           if(isink_mbps(istat) .eq. 0) then 
             write(*,*) "isink_mbps is 0 for station ", cstnna(istat)
             stop
           endif      
           rec_factor=float(idata_mbps(istat))/float(isink_mbps(istat))
           buf_time=iMark6_off+ibb_off(istat)+
     >           float(idur)*max(0.d0,rec_factor-1.d0)                 
        endif     

        IQ = MAX(ituse(istat)*ITEARL(istat) - ICAL,0)
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
        else if (tape_motion_type(istat).eq.'CONTINUOUS') then
           amaxsl = amax1(tslew+isrc_time+ICAL,          
     >                    float(iset+ itearl(istat)*ituse(istat)))
! Tape is stationary. Either ADAPTIVE or stop and start.
        else 
          AMAXSL = AMAX1(tslew, float(iset+IQ)+buf_time)   
          UT_out = UT_out+ICAL+isrc_time+ITAPTM
        endif ! continuous or not
            
        Amaxsl = amax1(amaxsl,float(imintm))    
        imaxsl=int(amaxsl+0.9)  !round this up to nearest integer.
     
        UT_out=UT_out+imaxsl

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
        end 




