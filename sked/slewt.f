      SUBROUTINE SLEWT(NSNOW,MJD,UT,NSNEW,ISTN,cwrap_cur,cwrap_new,
     > TSLEW, lookah,trise,tsris,st0cur,frac,knov,islew_info,
     > aznow,aznew)
C
C   SLEWT calculates the slew time and the cable wrap
C
      implicit none
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
     
C
! functions
C     INPUT VARIABLES:
       integer nsnow       !Current source #
       integer mjd         !MJD now
       real*8  UT           !current UT
       integer nsnew       !New source #
       integer istn        !Station #
       integer lookah      !Lookahead value (Seconds).  
       character*2  cwrap_cur  !Current cable wrap
       real*8  tsris(Max_sor,max_stn,*)    !Rise set times
       real*8  st0cur(max_stn)             !Mean sideral time 0hrs UT. 
       logical knov        !special VLBA mode. 
     
! Output variables
       real tslew        !slew time
       real trise        !Time until source rises. <0 means source is up. >0 includes slew time.
       character*2 cwrap_new      !New cable wrap.
       real*8 frac 
       integer islew_info !0  -- no problem
                           !1  -- source near cable wrap and can't determine which direction antenna will go. 
                           !2  -- change in Az near 180 degrees (sked can't predict direction)
                           !3  -- changes cable wrap during iteration. 
                           !-4  -- source is not up 
                           !-5  -- source is not continuous
                           ! 6 
      
      integer ierr 
! Functions used.
      real cablw          !compute required az move.
      real ggao_slew      !ggao slew time

C   COMMON BLOCKS USED
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLING SUBROUTINES: OBSCM,CHCMD, and others
C     CALLED SUBROUTINES: CVPOS,CABLW
C
C   LOCAL VARIABLES

      LOGICAL kcont
      REAL RSTCON(2),tslewp,tslewc,delaz,delel,deldc,delha
      real delx30,dely30,delx85,dely85,aznow,aznew,elnow,elnew
      real hanow,hanew,decnow,decnew,x30now,x30new,y30now,y30new
      real x85now,x85new,y85now,y85new,az1,az2,elrate
      real tslew1,tslew2
      integer nloops
      integer nrs        !which rise-set interval. Some sources can rise a few times (e.g., go behind a mountain.)  
      character*2 cwrap1,cwrap2,cwrap2_pre
      double precision gst                       !current gst
      real rme
      double precision edge_tol     !How close can we be to the edge    
      double precision tol_180      !How close can we be to 180 degrees move?
      double precision move_test    !Test cablewrap within this distance
      character*8 lkind 
      real az_off, el_off, az_rate,el_rate
      real slew0, temp
      real el1,el2

      
      logical kfirst
      character*2 cwrap_first
      LOGICAL KUP                    ! Returned from CVPOS, TRUE if source within limits
      logical kdebug                 !If true, print out debugging

C        TSLEWP,TSLEWC - previous, current slew times.  For iterating.
C        DELAZ,DELEL,DELDC,DELHA,DELX30,DELY30,DELX85,DELY85
C        AZNOW,AZNEW,ELNOW,ELNEW,HANOW,HANEW,DECNOW,DECNEW
C        X30NOW,X30NEW,Y30NOW,Y30NEW,X85NOW,X85NEW,Y85NOW,Y85NEW
C               - Increments, current, next values of az,el,ha,x,y

C        NLOOPS - Number of iterations on slewing time
C        AZ1,AZ2,cwrap1,cwrap2
C               - current,new values of az,wrap

C
C  History
C      DATE   WHO    CHANGES
C     811125  MAH    CHECK THAT SLEWING DOES CONVERGE FOR AZ-EL ANTENNAS
C     830423  NRV    ADD X,Y CALCULATIONS
C     830523  WEH    SATELLITES ADDED, DEC ADDED TO CVPOS CALL, SLEWING
C                    NOW USES RETURNED DECs NOT VALUES FROM COMMOM
C     880315  NRV    DE-COMPC'D
C     900425  NRV    Added check for axis type 6 (SEST)
C     900511  NRV         "       "      "     7 (ALGO)
C     930308  nrv    implicit none
C     931012  nrv    Add in the constants when calculating slew times for
C                    type 7 (ALGO)
C     931021  nrv    Remove iterative loop for rising sources and replace
C                    with reference to the "rise/set" arrays
C                    Added common blocks at this time.
C     931109  nrv    Change from itsris to tsris for real*8
C     931112  nrv    tsris is in GST
C     931123  nrv    corrected calculation of srise, had an extra 86400!!
C     931124  nrv    Don't calculate time at lookahead if the value is 0
C     950519  nrv    Add knov for special all-observe VLBA mode
C 970120 nrv change variable RME to single precision for AMAX1
! 2005Mar14  JMGipson.  Return error if direction of cable wrap changes during slew calculation,
!                       Reason is that direction antenna moves is ambiguous.
! 2008Jun20  JMGipson changed call to kcont
! 2009Apr13  JMGipson. Include bug fix found by A. Melnikov
! 2012May07  JMGipson.  Another fix to cable wrap. Now check to see if going near border. 
!                       Also clean up the code. 
! 2016Sep26  JMGipson. Modified calculation of trise to be consistent with lookahead. 
C
C
C     1. First we find the position of the telescope at the end of
C        the current observation and the position of the new source
C        at that time also. Then we go into
C        a loop which calculates the required telescope move, the
C        time to get there, and the source position at the end of
C        the move.  The loop is terminated when the slewing time
C        does not change by more than 30sec, or when 5 tries have
C        been made.
C
      islew_info=0
      kfirst=.true.
      edge_tol=2.d0*deg2rad    !edge tolerance is .5degrees
      tol_180 =2.d0*deg2rad     !2 degree.
      move_test=185.d0*deg2rad
      kdebug=.false.
      tslew=0.d0    !initilize to no slew 

      CALL CVPOS(NSNOW,ISTN,MJD,UT,
     >  AZNOW,ELNOW,HANOW,DECNOW,X30NOW,Y30NOW,X85NOW,Y85NOW,KUP)
      if(kdebug) then
         write(*,'("slewt 123: stat=",a, " AZNOW ",f8.2,1x,a2)')
     >      cstnna(istn), aznow*rad2deg,cwrap_cur
      endif 
C                    this calculates the current telescope position
      if (knov) then
        kup=.true.
        if (elnow.lt.0.0) elnow=1.0*deg2rad   ! 1 degree
      endif
      trise=-1.0
      TSLEWC = 0.0
      NLOOPS = 0
100   NLOOPS = NLOOPS + 1
      TSLEWP = TSLEWC
      cwrap2_pre = cwrap2
C     This calculates the new source location:
      CALL CVPOS(NSNEW,ISTN,MJD,UT+TSLEWC,
     >  AZNEW,ELNEW,HANEW,DECNEW,X30NEW,Y30NEW,X85NEW,Y85NEW,KUP)
      if(kdebug) then
         write(*,'("slewt 141: stat=",a, " aznew ",f8.2)')
     >      cstnna(istn), aznew*rad2deg
      endif 
      if (knov) then
        kup=.true.
        if (elnew.lt.0.0) elnew=1.0*deg2rad   ! 1 degree
      endif
      if (aznew.gt.100.or.aznew.lt.-100.d0) then
        write(7,*) 'SLEWT 167:  bad aznew ',aznew*rad2deg
        stop
      endif
C
C     If the source is not up now but will be up within the lookahead
C     time, find out when it rises, then calculate its position at that time.
      if (.not.kup.and.lookah.gt.0) then ! check for source being up within lookahead
        CALL isup(NSNEW,ISTN,UT+lookah,kup,nrs)
        if (kup) then !rising within lookahead time
! Compute the local GST 
           gst=st0cur(istn)+UT*frac
           if(gst .gt. twopi) gst=gst-twopi                
! this is the difference in times in GST. between (Source_rises--GST_now)
           trise=tsris(nsnew,istn,nrs)-gst
           if(trise .lt. 0) trise=trise+twopi
! convert from radians to seconds.           
            trise=trise/frac 
          CALL CVPOS(NSNEW,ISTN,MJD,UT+trise,
     >    AZNEW,ELNEW,HANEW,DECNEW,X30NEW,Y30NEW,X85NEW,Y85NEW,KUP)
C       Now we have the time to rising and position at rise (xxNEW).
        endif
      endif

      IF (.NOT.KUP) then
       islew_info=-4
       return
      endif
C
      AZ1=AZNOW
      cwrap1=cwrap_cur
      AZ2=AZNEW
      cwrap2=cwrap_new

      if(kdebug) then
         write(*,'("slewt 179: stat=",a, " AZ1   ",f8.2,1x,a2)')
     >      cstnna(istn), az1*rad2deg, cwrap1
         write(*,'("slewt 179: stat=",a, " aznew ",f8.2,1x, a2)')
     >      cstnna(istn), az2*rad2deg, cwrap2
      endif 
      DELAZ = CABLW(ISTN,AZ1,cwrap1,AZ2,cwrap2)   !On exit AZ1, AZ2 are beg, ending AZ including cablewrap.       
      if(kdebug) then
         write(*,'("slewt 186: stat=",a, " AZ1   ",f8.2,1x,a2)')
     >      cstnna(istn), az1*rad2deg, cwrap1
         write(*,'("slewt 186: stat=",a, " aznew ",f8.2,1x, a2)')
     >      cstnna(istn), az2*rad2deg, cwrap2
      endif 
      if(kfirst) then
        kfirst=.false.
        cwrap_first=cwrap2
      else
        if(cwrap2 .ne. cwrap_first) then
          islew_info=3
          return
        endif
      endif
 
C                   Function to compute az move including cable wrap
! See if at edge. Only do for Az-el mounts.
      IF (IAXIS(ISTN).EQ.3.or.iaxis(istn).eq.6.or.iaxis(istn).eq.7) then
         if(abs(abs(delaz)-pi) .lt. tol_180) then
           islew_info=2
         else if((az1-az2 .lt. move_test .and.
     &              abs(az2-stnlim(1,1,istn)) .lt. edge_tol) .or.
     &            (az2-az1 .lt. move_test .and.
     &              abs(az2-stnlim(2,1,istn)) .lt. edge_tol) ) then     
           islew_info=1  
! Check if in Counterclokwise and move to area which is near top CCW limit or neutral. (ambiguous)
         else if(abs(az2+twopi-stnlim(2,1,istn)) .lt. edge_tol .and.  
     &               stnlim(2,1,istn)-az1 .lt. pi) then
           islew_info=6
         else if(abs(az2-twopi-stnlim(1,1,istn)) .lt. edge_tol .and.
     &               az1-stnlim(1,1,istn) .lt. pi) then
           islew_info=7
         endif
      endif

      DELEL = ABS(ELNEW-ELNOW)
      DELHA = ABS(HANEW-HANOW)
      DELDC = DABS(0.D0+DECNEW-DECNOW)
      DELX30 = ABS(X30NEW-X30NOW)
      DELX85 = ABS(X85NEW-X85NOW)
      DELY30 = ABS(Y30NEW-Y30NOW)
      DELY85 = ABS(Y85NEW-Y85NOW)
C
      IF (IAXIS(ISTN).EQ.1.OR.IAXIS(ISTN).EQ.5)
     .  TSLEWC = AMAX1(Slew_off(1,ISTN)+DELHA/Slew_rate(1,ISTN),
     .  Slew_off(2,ISTN)+DELDC/Slew_rate(2,ISTN))
      IF (IAXIS(ISTN).EQ.2)
     .  TSLEWC = AMAX1(Slew_off(1,ISTN)+DELX30/Slew_rate(1,ISTN),
     .  Slew_off(2,ISTN)+DELY30/Slew_rate(2,ISTN))
      IF (IAXIS(ISTN).EQ.3.or.IAXIS(ISTN).eq.6)
     .  TSLEWC = AMAX1(Slew_off(1,ISTN)+DELAZ/Slew_rate(1,ISTN),
     .  Slew_off(2,ISTN)+DELEL/Slew_rate(2,ISTN))
      IF (IAXIS(ISTN).EQ.7) then
        elrate = Slew_rate(2,istn)
C       The Algonquin antenna is faster going down.
C       if (elnew.lt.elnow) elrate=elrate*1.333
C       First compute the az/el slewing rate
        TSLEW1 = AMAX1(Slew_off(1,istn)+DELAZ/Slew_rate(1,ISTN),
     .                 Slew_off(1,istn)+DELEL/elrate)
C       Compute the ha/dec slewing rate for the master equatorial
C       NOTE: Rates are hard-coded here because they are not available
C             in the normal antenna info.  Rates are 24 deg/min.
        rme = 24.d0*deg2rad/60.d0
        TSLEW2 = AMAX1(Slew_off(1,istn)+DELHA/rme,
     .                 Slew_off(1,istn)+DELDC/rme)
        tslewc=amax1(tslew1,tslew2)
      endif
      IF (IAXIS(ISTN).EQ.4)
     .  TSLEWc = AMAX1(Slew_off(1,ISTN)+DELX85/Slew_rate(1,ISTN),
     .  Slew_off(2,ISTN)+DELY85/Slew_rate(2,ISTN))
C
      IF ((ABS(TSLEWC-TSLEWP).LT.10).OR.(NLOOPS.GE.5)) GOTO 110
      GOTO 100
C     We get here if the slew has converged OR we iterated 5 times.
110   IF  (kcont(MJD,UT+TSLEWC,TSLEWP-TSLEWC,NSNEW,ISTN,cwrap_cur,ierr))
     .  THEN  !continuity OK
        TSLEW = TSLEWC
! AEM20081128.  The followng lines set slew time to 0 for short slews.
! Led to erorrs
!        RSTCON(1) = FLOAT(Slew_off(1,ISTN))
!        RSTCON(2) = FLOAT(Slew_off(2,ISTN))
!        IF(TSLEW.LE.(AMAX1(RSTCON(1),RSTCON(2))+5.))  TSLEW=0.0
        cwrap_new = cwrap2
C       Final slewing time is the larger of 
C       "time to rise" (trise) and "slew to risen position" (tslew
C       calculated using az,el at UT+trise).
        if (trise.gt.0..and.tslew.gt.0.) tslew = amax1(tslew,trise)       
      ELSE  !
        islew_info=-5
        trise=-1.0    
      END IF  !
      aznow=az1
      aznew=az2
! Special  fix becauses of GGAO mask
      if(cstnna(istn) .eq. "GGAO12M") then
         az1=az1*rad2deg
         az2=az2*rad2deg
         az_rate=Slew_rate(1,istn)*rad2deg
         el_rate=Slew_rate(2,istn)*rad2deg
         az_off=Slew_off(1,istn)
         el_off=Slew_off(2,istn)
         el1=elnow*rad2deg
         el2=elnew*rad2deg

         tslew=ggao_slew(az1,el1,az2,el2,
     &      az_off,az_rate,el_off,el_rate,slew0,lkind)
!          if(abs(tslew-slew0) .ge. 4) then 
!             write(*,*) "Difference at GGAO ",tslew-slew0
!             write(*,'("At ",4f8.2)') az1,el1,az2,el2
!          endif
       endif
         
      
      
      

990   RETURN
      END
