      subroutine ChkSrcUp4Scan(istat,isource,nceles,csorna,cstnna,
     >  MJD,UT,idur,cwrap_new, ludsp,kdisplay,ierr)
      implicit none
      include '../skdrincl/constants.ftni'

! functions
      logical kcont
      integer trimlen
      
! Passed
      integer istat     !station
      integer isource   !source#
      integer nceles    !number of celestial sources.
      character*8 csorna
      character*8 cstnna
      integer MJD
      Double precision UT       !Time in seconds
      integer idur
      character*2 cwrap_new    !cable wrap   
      integer ludsp     !lu to print
      logical kdisplay
! returned
      integer ierr      !0 if an error.
                        !1 not up at start
                        !2 not up at end
                        !3 not continuous
      character*50 lmessage
! local
      logical kup
      real az,el,ha,dec,x30,y30,x85,y85   !All returned by CVPOS
      integer ihr, imin,isec
      real*8 ut_test                      !time to test if source up.
      real   dur_tmp
      integer i
      
! History
!    2003Mar25 JMGipson. First version. Originally tired to use isup, but this
!                           does not correctly handle case el~90.
!    2008Nov05 JMGipson. Better error messages
!    2008Nov12 JMgipson. Reorganized slightly. Replaced two calls (start, end) with loop.  
!    2009Jan09 JMGipson.  Changed error message from "too low" to "not visible"

         
      call seconds2hms(ut,ihr,imin,isec)
!      write(*,'(2(a,1x),2(i2.2, ":"),i2.2)')csorna,cstnna,ihr,imin,isec
      
      do i=1,2
        if(i.eq.1) then
          ut_test=ut
          ierr=1
          lmessage="ERROR! (chksrcup4scan): At scan start time "
        else
          ut_test=ut+idur
          ierr=2
          lmessage="ERROR! (chksrcup4scan): At scan end time "
        endif

!        if(isource .gt. nceles) then
          CALL CVPOS(isource,istat,MJD,UT_test,
     >       AZ,EL,HA,DEC,X30,Y30,X85,Y85,KUP) ! start of obs
!        else
!           CALL isup(Isource,istat,UT,KUP,nrs)
!        endif
         if(.not.kup) then   
           if(kdisplay) then           
             write(ludsp,
     >    '(a,1x, 2(i2.2,":"),i2.2," source ",a," not visible at ",
     >      a,": az, el= ",2f6.1)') trim(lmessage), ihr,imin,isec, 
     >      csorna, cstnna, az*rad2deg,el*rad2deg         
          endif
          return
         endif
      end do
  
! Continuity
      dur_tmp=float(idur)
      IF (.NOT.kcont(MJD,UT,dur_tmp,isource,istat,cwrap_new,ierr)) THEN
        ierr=3
        if(kdisplay) then 
          write(ludsp,
     >   "('ERROR! (chksrcup4scan): Source track ', a8, 
     >      'not continous at ',a8)")    csorna, cstnna       
        endif
      endif
      ierr=0
      return
      end







