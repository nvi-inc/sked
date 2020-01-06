      SUBROUTINE now_cmd(cmdline)
! Set the current time in the schedule.  Observations that we schedule start from here.

!
C   COMMON BLOCKS USED

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/statn.ftni'   !contains nstatn.
      include '../skdrincl/skobs.ftni'   !contains nstatn.

      include 'skcom.ftni'
!      include 'cat_stat.ftni'

C Input
      character*(*) cmdline

! functions
      double precision hms2seconds
      integer julda

! local
! Stuff dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=2)
      character*(Max_stn*2) ltoken(MaxToken)

      integer ierr                    !some error
      integer iyr,iday,ihr,imin,isec  !time
      integer isub_tmp(max_stn),nsub_tmp   !subnet,  number of stations in subnet.
      integer istn                    !station.
! Counter 
      integer j


! History
! 2011August11. First version. Globally sets  time.
! 2011August12. 2nd version.  Set subnet. 


      call capitalize(cmdline)
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)

      if(NumToken .eq. 0) then
! Write current time 
        do j=1, nstatn
          call seconds2hms(utcur(j)+idurcur(j),ihr,imin,isec)
          write(*,'(a8,1x,i4,"/",i3,"-",i2.2,2(":",i2.2))')   
     &    cstnna(j),iyrcur(j), idacur(j), ihr, imin,isec
        end do
        return
      else if(NumToken .ne. 2 .or. ltoken(1) .eq. "?") then
! Write out syntax
         writE(luscn,'(a)') 'Now [? | <subnet> <time>]'
         return
      endif  
  
! Have two tokens. The first is subnet, second is time.    
      call extract_station_list(luscn,ltoken(1),isub_Tmp,nsub_tmp)
      call YDHMS(ltoken(2), ierr, IYR,IDAY,IHR,IMIN,ISEC)
      if(ierr .eq. 0) then
         write(*,'("For subnet ",a, " setting current time to ",
     &    i4,"/",i3,"-",i2.2,2(":",i2.2))')  
     &    trim(ltoken(1)), iyr, iday, ihr, imin,isec     
        do j=1,nsub_tmp
          istn=isub_tmp(j)
          mjdcur(istn)=JULDA(1, IDAY, IYR-1900)
          utcur(istn) =hms2secondS(ihr,imin,isec)
          idurcur(istn)=0
        end do
      endif 

      return
      end 



