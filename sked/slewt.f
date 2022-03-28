      SUBROUTINE SLEWT(NSNOW,MJD,UT,NSNEW,ISTN,cwrap_cur,cwrap_new,
     > TSLEW, lookah,trise,tsris,st0cur,frac,knov,islew_info,
     > aznow,elnow,aznew,elnew)
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
      real slew_ggao      !ggao slew time
      double precision slew_oe_ow     !Slew time for Onsala twins . In double precision 

C   COMMON BLOCKS USED
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLING SUBROUTINES: OBSCM,CHCMD, and others
C     CALLED SUBROUTINES: CVPOS,CABLW
C
C   LOCAL VARIABLES

! functions
      LOGICAL kcont
      real    slew_time
! Local       
      
      REAL tslewp,tslewc
      real delaz,delel
      real aznow,aznew,elnow,elnew
      real hanow,hanew,decnow,decnew,x30now,x30new,y30now,y30new
      real x85now,x85new,y85now,y85new
      real x1,x2,y1,y2
      real tslew1,tslew2                       !slewing times for two axis 
      real slew_tol 
      integer iter
      integer nrs        !which rise-set interval. Some sources can rise a few times (e.g., go behind a mountain.)  
      character*2 cwrap1,cwrap2
      double precision gst                       !current gst
      real rme
      double precision edge_tol     !How close can we be to the edge    
      double precision tol_180      !How close can we be to 180 degrees move?
      double precision move_test    !Test cablewrap within this distance
      character*8 lkind 
      real az_off, az_vel,az_acc    !For use by GGAO. Radians converted  to degrees
      real el_off, el_vel,el_acc      
      real temp
      real el1,el2
      real az1,az2
      real slew0_ggao               !slewing at GGAO in absence of mask
! Double precision       
      double precision daz_beg, daz_end     !The onsala twins uses double precision 
      double precision del_beg, del_end 
      double precision daz_slew,del_slew    ! Individual slew times (w/o offset) 

      
      logical kfirst
      character*2 cwrap_first
      LOGICAL KUP                    ! Returned from CVPOS, TRUE if source within limits
      logical kdebug                 !If true, print out debugging
      logical kdebug_slew 

C        TSLEWP,TSLEWC - previous, current slew times.  For iterating.
C        DELAZ,DELEL,DELDC,DELHA,DELX30,DELY30,DELX85,DELY85
C        AZNOW,AZNEW,ELNOW,ELNEW,HANOW,HANEW,DECNOW,DECNEW
C        X30NOW,X30NEW,Y30NOW,Y30NEW,X85NOW,X85NEW,Y85NOW,Y85NEW
C               - Increments, current, next values of az,el,ha,x,y

C        iter - Number of iterations on slewing time
C        AZ1,AZ2,cwrap1,cwrap2
C               - current,new values of az,wrap

C
C  History
! 2022-01-06 JMGipson. Removed unused cwrap2_pre. Made do loop more standard. 
!                      Reduced convergence to .5 secionds.  Write message if fails. 
! 2021-11-10 JMGipson  Modified slewing algorithms. 
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
!
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
      edge_tol=1.d0*deg2rad    !edge tolerance is .5degrees
      tol_180 =1.d0*deg2rad     !2 degree.
      move_test=185.d0*deg2rad
      kdebug=.false.
      kdebug_slew=.false. 
      tslew=0.d0    !initilize to no slew 
      slew_tol=0.25   !convergence criteria 

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
 
      
! This is passed to cablw below. If set to " ", then tries to use the quickest path      
      cwrap_new=" " 

100    continue 
      do iter=1,5  
      TSLEWP = TSLEWC  
      if(kdebug_slew .and. iter .eq. 1) then    
         write(*,'("NOW:  stat=",a," ",a, 2f8.2,1x,a2)')
     >   cstnna(istn), csorna(nsnow), aznow*rad2deg,
     >   elnow*rad2deg,cwrap_cur
      endif 
      
C     This calculates the new source location:
      CALL CVPOS(NSNEW,ISTN,MJD,UT+TSLEWC,
     >  AZNEW,ELNEW,HANEW,DECNEW,X30NEW,Y30NEW,X85NEW,Y85NEW,KUP)
!      write(*,*) csorna(nsnew), rad2deg*aznew,rad2deg*elnew, kup 
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
      
      if(kdebug_slew) then    
        write(*,'("NEW:  stat=",a," ",a, 2f8.2,1x,a2)')
     >     cstnna(istn), csorna(nsnew), aznew*rad2deg,
     >     elnew*rad2deg, cwrap_cur
      endif 
          
C     If the source is not up now but will be up within the lookahead
C     time, find out when it rises, then calculate its position at that time.
      if(.false.) then 
!code below does not work.        
!      if (.not.kup.and.lookah.gt.0) then ! check for source being up within lookahead
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
          if(kdebug_slew) then
             write(*,*) "TRISE ", trise,lookah  
          endif 
          CALL CVPOS(NSNEW,ISTN,MJD,UT+trise,
     >    AZNEW,ELNEW,HANEW,DECNEW,X30NEW,Y30NEW,X85NEW,Y85NEW,KUP)
C       Now we have the time to rising and position at rise (xxNEW).
        endif
      endif

      IF (.NOT.KUP) then   
       islew_info=-4
       return
      endif

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
      
      if(kdebug_slew) then
        write(*,'("AZ1  AZ2 ", 2f8.2,2x,"wrap= ",a2)') 
     &    az1*rad2deg, az2*rad2deg, cwrap2
      endif        
      
      DELAZ = CABLW(ISTN,AZ1,cwrap1,AZ2,cwrap2)   !On exit AZ1, AZ2 are beg, ending AZ including cablewrap.             
      if(kdebug_slew) then
          write(*,'("AZ1  AZ2 ", 2f8.2,2x,"wrap= ",a2, " del ", f8.2)') 
     &    az1*rad2deg, az2*rad2deg, cwrap2, delaz*rad2deg       
      endif     
      
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
       
C   Function to compute az move including cable wrap
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
      if(islew_info .ne. 0) return 
      
      select case (iaxis(istn))
        case(1,5)     
          x1=HaNew
          X2=HaNow
          Y1=DecNew
          Y2=DecNow
        case(2)     
          X1=X30new
          X2=X30Now
          Y1=DecNew
          Y2=Decnow
        case(3,6)
          X1=Az1
          X2=Az2
          Y1=ElNew
          Y2=ElNow
        case(4)
          X1=x85New
          X2=X85Now
          Y1=Y85New
          Y2=Y85Now
        case default  
! This is algonquin.  Should never hit
          write(*,*) "Slewt:  unknown axis offset ", iaxis(istn)
          stop
        end select
      tslew1=slew_time(x1,X2,
     &             slew_off(1,istn),slew_vel(1,istn),slew_acc(1,istn))
      tslew2=slew_time(Y1,y2,
     &             slew_off(2,istn),slew_vel(2,istn),slew_acc(2,istn))
    
      tslewc=max(tslew1,tslew2) 
      if(kdebug_slew) then
         write(*,*) "Slew old, slew new ", tslewc, tslewp
      endif 
C
!      write(*,*) "iter ",iter , tslewc-tslewp 
      IF (ABS(TSLEWC-TSLEWP).LT. slew_tol) GOTO 110        !jump out if converged 
      end do 
      
      write(*,*) " "
      write(*,'("Slewt: Failed to converge at station ",a )') 
     &  cstnna(istn)
      write(*,'("Slewing ", a, " to ", a)') csorna(nsnow), csorna(nsnew)
      write(*,'(2f8.2,   " to ", 2f8.2)') 
     &   aznow*rad2deg, elnow*rad2deg, aznew*rad2deg,elnew*rad2deg
      write(*,'("T_old ", f8.2, " T_new", f8.2)') tslewp, tslewc     
      
       if(kdebug_slew) then
          write(*,*) "Sleeping bug "
          write(*,*) "Please save the schedule that casued this and"
          write(*,*) "And send to John gipson"
          write(*,*) "Together with what caused it to crash"
          stop
       endif 
       kdebug_slew=.true.
       tslewc = 0.0
       tslewp = 0.0 
       goto 100 
   
C     We get here if the slew has converged OR we iterated 5 times.
110   continue 
!      write(*,*) "slew_info 298 ",kup, islew_info

      IF  (kcont(MJD,UT+TSLEWC,TSLEWP-TSLEWC,NSNEW,ISTN,cwrap_cur,ierr))
     .  THEN  !continuity OK
        TSLEW = TSLEWC

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
! Some special cases
      select case(cstnna(istn))
         case("GGAO12M")        
! Special  fix becauses of GGAO mask   
! convert from radians to degrees because this is what ggao_slew expects.  
         az1=az1*rad2deg     
         az2=az2*rad2deg
         el1=elnow*rad2deg
         el2=elnew*rad2deg
         
         az_off =Slew_off(1,istn) 
         az_vel =Slew_vel(1,istn)*rad2deg
         az_acc =slew_acc(1,istn)*rad2deg
         el_off =Slew_off(2,istn) 
         el_vel =Slew_vel(2,istn)*rad2deg
         el_acc =slew_acc(2,istn)*rad2deg          

         tslew=slew_ggao(az1,el1,az2,el2,
     &     az_off,az_vel,az_acc,el_off,el_vel,el_acc,slew0_ggao,lkind)
!          if(abs(tslew-slew0) .ge. 4) then 
!             write(*,*) "Difference at GGAO",tslew-slew0
!             write(*,'("At ",4f8.2)') az1,el1,az2,el2
!          endif
         case("ONSA13NE", "ONSA13SW") 
! special fix because Onsala twins have different speeds in different regions.          
           daz_beg=az1*rad2deg
           daz_end=az2*rad2deg
           del_beg=elnow*rad2deg
           del_end=elnew*rad2deg
           tslew=slew_oe_ow(daz_beg,del_beg,daz_end,del_end,
     &                         daz_slew,del_slew) 
       end select          
     

990   RETURN
      END
