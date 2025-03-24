      subroutine ChkSrcUp4Scan(istat,isource,
     >  MJD,UT,idur,cwrap, ludsp,kdisplay,ierr)
      implicit none
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni' 

! functions
      real*4 azwrap 
      
! Passed
      integer istat     !station
      integer isource   !source#
      
      integer MJD
      Double precision UT       !Time in seconds
      integer idur
      character*2 cwrap !cable wrap   
      integer ludsp     !lu to print
      logical kdisplay
! returned
      integer ierr      !0 if NO error.
                        !1 not up at start
                        !2 not up at end
                        !3 not continuous
      character*24 lmessage
! local
      logical kup
      real az(2),el(2),ha,dec,x30,y30,x85,y85   !All returned by CVPOS
      integer ihr, imin,isec
      real*8 ut_test                      !time to test if source up.
      integer i                           !counter 
      real*8 delaz                        !Az distance traveled.
      real*8 AZ2C                         !Ending az taking into account wrap. 
      character*4 lbeg_end(2)
      data lbeg_end/"beg", "end"/
      
! History
!    2003Mar25 JMGipson. First version. Originally tired to use isup, but this
!                           does not correctly handle case el~90.
!    2008Nov05 JMGipson. Better error messages
!    2008Nov12 JMgipson. Reorganized slightly. Replaced two calls (start, end) with loop.  
!    2009Jan09 JMGipson.  Changed error message from "too low" to "not visible"
!    2021-12-06 JMGipson. Removed call to kup which reproduces many of the calculations here.
!    2023-04-27 JMGipson. Initialize ierr. 
!    2025-03-21 JMGipson.  Only set error code on first loop if actually an error. 
!         
!      write(*,'(2(a,1x),2(i2.2, ":"),i2.2)')csorna,cstnna,ihr,imin,isec
      ierr=0 
      az=0.d0
      el=0.d0    
      do i=1,2  
        if(i.eq.1) then
          ut_test=ut   
        else
          ut_test=ut+idur    
        endif        
        CALL CVPOS(isource,istat,MJD,UT_test,
     >       AZ(i),EL(i),HA,DEC,X30,Y30,X85,Y85,KUP) ! start of obs
        if(.not.kup)  then
          ierr=i          
          goto 500  ! Exit with an error.                       
        endif          
      end do  
   
! at this point know that source was visible both at start and end. 
! Now make sure that it doesn't cross wrap limit boundaries. 
! The bottom checks if  AZ-el antennas. If not then we can skip. 
      IF(IAXIS(ISTAT).ne.3 .and. iaxis(istat).ne.7 .and. 
     &  iaxis(istat).ne.6) return

! Below is adapted from kcont.f 
      DELAZ = AZ(2)-AZ(1)
      IF (DELAZ.GT.PI) then
          DELAZ = -(TWOPI-DELAZ)
      else IF (DELAZ.LT.-PI) then
          DELAZ = TWOPI+DELAZ
      endif
!        write(*,'(a,1x, 2f8.2)') cstnna(ist), az1*rad2deg, az2*rad2deg

      Az(1)=azwrap(az(1),cwrap,stnlim(1,1,istat))

      AZ2C = AZ(1)+DELAZ
C  Check whether we cross into ambiguous section during observation
      IF (AZ2C.LT.STNLIM(1,1,ISTat) .or. AZ2C.GT.STNLIM(2,1,ISTat)) then      
        ierr=3
        lmessage="is not continuous" 
        goto 500
      endif      
      ierr=0 
      return       !return w/o an error.
             
! Common error exit.       
 500  continue          
! Write error message. Something like
! ERROR! ChkSrcUpForScan: At 11:12:34 source 3C84 at HOBART 314.4 9.8 is down at start of scan. 
      if(kdisplay) then           
         call seconds2hms(ut_test,ihr,imin,isec)
         write(ludsp,
     >       '("Error! ChksrcUpforScan:  At ", 2(i2.2,":"),i2.2, $)') 
     >       ihr,imin,isec
         write(ludsp,'("source ",a," at ", a, 2f8.2,$ )') 
     >       csorna(isource), cstnna(istat), az(1)*rad2deg,el(1)*rad2deg
         if(ierr .le. 2) then     
             write(ludsp, '(" is down at ", a)') lbeg_end(i)
         else
            write(ludsp, '(a)') "is not continous"
         endif
      endif  
      return    
      end







