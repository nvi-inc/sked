      subroutine downtime(cmdline)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'downtime.ftni'
      character*(*) cmdline
! Parse a line of the form:
!  1.   downtime ss-cc-aa beg_time end_time
!  2.   downtime wz  off
!  3.   downtime wz  REM
!  4.   downtime wz  beg_time end_time OFF
!  5.   downtime wz  beg_time end_time REM
!  6.   downtime OFF
!  7.   downtime LIST
!  8.   downtime 
!  9.   downtime ?
! Commands do the following:
!  1.  Sets downtime for some subnet.
!  2&3 Removes ALL instances of WZ from down time.
!  4.&5 Removes this instance of WZ from down time.
!  6.   Turn downtime off.
!  7-8  List stations.
!  9    Gives help. 

! History:
!  2005Feb01 JMGipson.  First version.
!  2005Mar30 JMGipson.  Set default time arguments to current time.
!  2006May11 JMGipson.  Changed format so that can have multiple downtimes per station.
!                       Introduced hms2seconds function to do time conversion.
!  2007Mar07 JMGipson.  Modified how to remove stations.
!  2010Jan06
!  2014Apr18 JMGipson.  Increased size of ltoken so taht we could handle more stations
!
!
! functions
      integer trimlen
      integer julda
      double precision hms2seconds

! local
! Stuff to do with parsing command line.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=4)         !expecting maximum of 4 tokens.

      character*128 ltoken(MaxToken)
!
      integer i,j
      integer istn_tmp(max_stn)

      character*20 ctime(2)

      integer num_sub
      integer mjd_down_beg_tmp,mjd_down_end_tmp
      double precision ut_down_beg_tmp,ut_down_end_tmp

      integer iyear,iday,ihour,imin,isec,ierr

      integer num_down0     !initial value for number of downs.

      logical kremove_one
      logical kremove_all



      call capitalize(cmdline)
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)     

!      write(*,*) trim(cmdline) 
      if(trimlen(cmdline) .eq. 0) then
! write it out.
         if(num_down .eq. 0) then
             write(luscn,'("No stations have down time")')
         else
            call downtime_out(luscn,'d') 
         endif
         return
      endif

      if(ltoken(1) .eq. "?") then
         write(*,*)"DOWNTIME: Set, remove or list downtimes"
         write(*,*)"DOWNTIME ?                              "//
     >   "| This information"   
         write(*,*)"DOWNTIME                                "//
     >   "| (No argument) list current downtimes"
         write(*,*)"DOWNTIME OFF                            "//
     >   "| Remove all downtimes"
         write(*,*)"DOWNTIME Subnet OFF                     "//
     >   "| Remove all downtimes for subnet"
         write(*,*)"DOWNTIME Subnet Start_time End_time     "//
     >   "| Insert downtime for subnet"
         write(*,*) "        REM and OFF are synonomous"
       endif   

      if(NumToken .eq. 1 .and.
     > (ltoken(1).eq. "OFF" .or.ltoken(1)(1:3) .eq. "REM")) then
        kremove_all =.true.
        goto 150
      endif

      if(NumToken .gt. 4) goto 900   !Need 2,3 or 4 tokens.    
! 1. extract stations in stationlist.
      call extract_station_list(luscn,ltoken(1), istn_tmp,num_sub)
      if(num_sub .eq. 0) then
        write(*,*) "Downtime: Invalid station_list ",ltoken(1)
        return
      endif

      kremove_one=.false.
      kremove_all=.false.
! 2. If have 2 args, check to see if 2nd is OFF or "0"
      if(NumToken .eq. 2) then
        if(.not.(ltoken(2).eq."OFF" .or. ltoken(2).eq."REM")) goto 900     !remove these stations.
        kremove_all = .true.
      else if(NumToken .ge. 3) then
        ctime(1)=ltoken(2)                                     !Assume 2nd and 3rd are times.
        ctime(2)=ltoken(3)
      else
        goto 900
      endif

   

      if(NumToken .eq. 4)  then
         if(ltoken(4)(1:3).eq. "OFF" .or.ltoken(4)(1:3) .eq. "REM") then
            kremove_one = .true.
         else
            goto 900
         endif
      endif

150   continue
      if(kremove_all) then
! remove station(s) from list. Easiest way is just to set index to 0.
        do i=1,num_down
           do j=1,num_sub
             if(istn_tmp(j) .eq. idown_stat(i)) then
               idown_stat(i)=0
             endif
           end do
        end do
        call clean_up_down_list
        return
      endif

! For adding or removing 1 interval, need to parse the times.

100   continue
! set the default time.
      J = ISTCUR(1)
      IF (NSTNCUr.EQ.0) J=1
      IYear=IYRCUR(J) ! full 4-digit year
      IDAY=IDACUR(J)
      call seconds2hms(utcur(j),ihour,imin,isec)
! find start and ending time.
      do i=1,2
        call ctime2YDHMS(ctime(i),iyear,iday,ihour,imin,isec,ierr)
        if(ierr .ne. 0) goto 900
        if(i .eq. 1) then
          mjd_down_beg_tmp=julda(1,iday,iyear-1900)
          ut_down_beg_tmp = hms2seconds(ihour,imin,isec)
        else
          mjd_down_end_tmp=julda(1,iday,iyear-1900)
          ut_down_end_tmp = hms2seconds(ihour,imin,isec)
        endif
      end do

! First do removal.

      if(kremove_one) then
! Now go through list of stations, and list of down structure.
        do j=1,num_sub
! search for match with existing stations.
          do i=1,num_down
! Found a match for stations. Check if a match on times.
            if(istn_tmp(j) .eq. idown_stat(i) .and.
     >        mjd_down_beg(i) .eq. mjd_down_beg_tmp .and.
     >        ut_down_beg(i)  .eq. ut_down_beg_tmp  .and.
     >        mjd_down_end(i) .eq. mjd_down_end_tmp .and.
     >        ut_down_end(i)  .eq. ut_down_end_tmp) then
              idown_stat(i) =0
            endif
          end do
        end do
        call clean_up_down_list
        return
      endif

! Must be adding.
      num_down0=num_down
      do j=1,num_sub
! Loop over down list.
        do i=1,num_down0
! Found a match for stations.
          if(istn_tmp(j) .eq. idown_stat(i)) then
! Check for a match on times.
! 1.) Exact match. Don't do anything.
! 2.) End times match begining.  Append at beggining.
! 3.) Begin times match end.     Append at end.
            if(mjd_down_beg(i) .eq. mjd_down_beg_tmp .and.
     >         ut_down_beg(i)  .eq. ut_down_beg_tmp  .and.
     >         mjd_down_end(i) .eq. mjd_down_end_tmp .and.
     >         ut_down_end(i)  .eq. ut_down_end_tmp) then
               goto 200
            else if(mjd_down_beg(i) .eq. mjd_down_end_tmp .and.
     >              ut_down_beg(i)  .eq. ut_down_end_tmp) then
               mjd_down_beg(i)=mjd_down_beg_tmp              !append time to the beginning
               ut_down_beg(i)=ut_down_beg_tmp
               goto 200
            else if(mjd_down_end(i) .eq. mjd_down_beg_tmp .and.
     >              ut_down_end(i)  .eq. ut_down_beg_tmp) then
              mjd_down_end(i)=mjd_down_end_tmp              !append time to end of interval.
              ut_down_end(i)=ut_down_end_tmp
              goto 200
            endif
          endif
        end do
! Went through loop without a match. Add at end of list.
        num_down=num_down+1
        idown_stat(num_down)=istn_tmp(j)
        mjd_down_beg(num_down) = mjd_down_beg_tmp
        ut_down_beg(num_down)  = ut_down_beg_tmp
        mjd_down_end(num_down) = mjd_down_end_tmp
        ut_down_end(num_down)  = ut_down_end_tmp
200     continue
      end do
      return

900   continue
      write(luscn,'(a)')
     >  "Down_time: Error in command line. Proper form: "
      write(luscn,'(a)') "Down_time subnet begtime endtime"
      return
      end

!******************************************************************
      subroutine clean_up_down_list
      implicit none
      include 'downtime.ftni'

      integer i,j

      j=0
! clean up by removing stations set to 0.
      do i=1,num_down
        if(idown_stat(i) .ne. 0) then
          j=j+1
          idown_stat(j)=idown_stat(i)
          mjd_down_beg(j)=mjd_down_beg(i)
          ut_down_beg(j)=ut_down_beg(i)
          mjd_down_end(j)=mjd_down_end(i)
          ut_down_end(j)=ut_down_end(i)
         endif
      end do
      num_down=j
      return
      end

