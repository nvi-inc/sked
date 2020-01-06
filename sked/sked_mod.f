      module sked_obs
      implicit none
! This file contains definitions of the sked 'scan' datatype and associated routine

      type sked_scan
        logical kused           !is this scan used or not. 
        integer isrc_num        !source number
        integer iymdhm(5)       !Year,month,day,hour,minute
        real*8  second
        integer mjd             !Day part of time tag
        real*8  UT               !Seconds part of time tag--upto 86400 seconds =24*60*60
        character*6 cPreOB      !procedures. Usually constant throughout the session.
        character*6 cPostOB
        character*6 cMidOb
        character*4 cPrflag
        character*2 lcode
        integer ical       
        integer idelay
        integer idurx 

! Some stuff dealing with stations
        integer num_stat     !num stations in scan.
        integer, allocatable :: istn(:)
        real*8, allocatable ::   az(:), el(:)  !Azimuth and elevation of station.  
        integer*2, allocatable :: lcbl(:)      !cable wrap
        character, allocatable :: wrap(:)      !Cable wrap= "C", "W","-"        
        integer, allocatable :: idur(:)        !duration (seconds) for each station

                          !duration of scan
! Stuff having to do with footage,etc.        
        integer, allocatable :: ifeet(:)       !footage at start of scan
        integer, allocatable :: ifeet_end(:)   !At end of scan
        integer, allocatable :: ipass(:)       !which pass
        integer, allocatable :: idir(:)        !idirection
        integer, allocatable :: itu(:)         !tu???
        integer, allocatable :: icode(:)       !Code?
      end type

      type (sked_scan) scan_now                  !Current scan
      type (sked_scan) scan_test                 !test_scan

      integer, parameter :: Max_scan=2000       !maximum number of scans
      integer Num_scan                           !Number of scans.

      type (sked_scan) scan_vec(max_scan)        !Maximum number of scans
      integer iscan_key(Max_Scan)                !key into scans. Assume sorted by order.

      contains
! Some subroutines.
! ****************************************************
      subroutine init_sked_scans()
      Num_Scan=0
      scan_vec(1:Max_scan)%Kused=.false.         !Mark all scans as free.
      end subroutine

!************************************************************************************
      subroutine cobs2scan(cobs,scan_out,ierr)
!      
      character*(*) cobs               !observation in old sked format
      type(sked_scan) scan_out          !observation in new internal format.
! out     
      integer ierr
! Convert a scan in the old sked format into the new format.
! The old format is a line like:
! Source    ical Code preob   time           Dur midob     idl postob Stat    Feet                      PRFLG  durations  
!   1        2     3   4       5              6   7         8    9      10       11     ......
! 0133+476  10    SX  PREOB  10077183000     370 MIDOB      0  POSTOB BWCWD- 1F000000 1F000000 1F000000 YYNN   370   370   155

! Functions
      integer igetsrcnum
      integer igetstatnum
      integer julda 
      real*8   hms2seconds

! Local
      character*80 lerr_msg
      integer istart                   !Starting point to parse line
      integer inext                    !start of next token
! Stuff dealing with tokens.
      character*10 lkind                !kind of token
      character*10 lkind_vec(10)/"Source ","CAL","Code","Preob","Time",
     &    "Durx","Midob","Idle","Postob","Stations"/
      character*128 ltoken             !token  must be as long as largest token=2*NumStat    
      logical ktoken            !returned token?
      logical knospace          !no space left.
      logical keol              !end of line
! 
      integer ilen              !length of string
      integer iyr,idoy,ihr,imin,isc    !time
      integer iyr2              !temporary variable that holds year
      character*63 cpass
     &/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'/
      integer i,j                 !counter 
           

! Process the first 10 tokens.
! The order and number of the first 10 tokens is set. Read them in.

      ierr=-99
      inext=1
      do i=1,10
        istart=inext
        call ExtractNextToken(cobs,istart,inext,ltoken,ktoken,
     >              knospace, keol)

        lkind=lkind_vec(i)     
        if(.not.ktoken) goto 900      
        select case(i)
          case(1)           !source 
            scan_out%isrc_num=igetsrcnum(ltoken)
            if(scan_out%isrc_num .le. 0) goto 900
          case(2)           !caltime
            read(ltoken,*,err=900) scan_out%ical
          case(3)           !Code (e.g., SX)
            scan_out%lcode=ltoken(1:2)
          case(4)           !preob
            scan_out%cPreob=ltoken(1:6)
          case(5)           ! Epoch
! extract the time.
            read(ltoken,'(i2,i3,i2,i2,i2)') iyr,idoy,ihr,imin,isc
            if (iyr.lt.70) then
              iyr= iyr + 2000
            else
              iyr=iyr +1900         
            endif
            iyr2=iyr-1900                !  Note that Julda arg is years since 1900.
            scan_out%MJD = JULDA(1,Idoy,IYR2)          
            scan_out%ut  = hms2seconds(ihr,imin,isc)

            scan_out%iYMDHM(1)=iyr
            call ymday(iyr,idoy,scan_out%iymdhm(2),scan_out%iymdhm(3))
            scan_out%iymdhm(4)=ihr
            scan_out%iymdhm(5)=imin
            scan_out%second=isc
       
          case(6)           ! Duration
            read(ltoken,*,err=900) scan_out%idurx
          case(7)           !midob 
            scan_out%cMidob=ltoken(1:6)    
          case(8)           !idelay
            read(ltoken,*,err=900) scan_out%idelay
          case(9)           !postob
            scan_out%cPostob=ltoken(1:6)
          case(10)
        end select
      end do 

! At this point "Token" has the string containing stations.
      ilen=len_trim(ltoken)            !length of string
      scan_out%num_stat=ilen/2
      if(ilen*2 .ne. scan_out%num_stat) goto 900     

      do i=1,scan_out%num_stat
        j=2*i
        scan_out%istn(i)=igetstatnum(ltoken(j-1:j-1))
        if(scan_out%istn(i)  .le. 0) goto 900
        scan_out%wrap(i)=ltoken(j:j)        
      end do
    
! Extract tokens that deal with tape footage. 
      lkind="Footage"
      do i=1,scan_out%num_stat
        istart=inext
        call ExtractNextToken(cobs,istart,inext,ltoken,ktoken,
     >              knospace, keol)
        if(.not.ktoken) goto 900
! Parse something that looks like this...        
!     1F000000
        scan_out%ipass(i)=  index(cpass,ltoken(1:1))
        if(ltoken(2:2) .eq. "F") then
          scan_out%idir(i)=1
        else if(ltoken(2:2) .eq. "R") then
          scan_out%idir(i)=-1
        else
          write(*,*) "Unknown direction!"
        endif
        read(ltoken(3:len_trim(ltoken)),*, err=900) scan_out%ifeet
      end do 

      istart=inext
      call ExtractNextToken(cobs,istart,inext,ltoken,ktoken,
     >              knospace, keol)
      lkind="PRFLAG"
      if(.not.ktoken) goto 900
      scan_out%cPrflag=ltoken(1:4)
  
      lkind="Durations"
      do i=1,scan_out%num_stat
        istart=inext
        call ExtractNextToken(cobs,istart,inext,ltoken,ktoken,
     >              knospace, keol)
        if(.not.ktoken) goto 900
        read(ltoken,*,end=900) scan_out%idur(i)
      end do
      return

      ierr=0
      return


900   continue
      if(.not. ktoken) then
         writE(*,*) "cobs2scan: Did not find token for"//lkind
         write(*,*) "  ...in string: "//trim(cobs)
      else
         write(*,*) "cobs2scan: While trying to read token"//lkind
         write(*,*) "      problems parsing: "//trim(ltoken)
      endif
      return       
      end subroutine 
!********************************************************************
      subroutine scan2_cur(scan_in,ierr)
      type(sked_scan) scan_in
      integer ierr
! put the the scan to the current variables
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
! local



! just copy everything.
      isorcur  =scan_in%isrc_num
      nstncur  =scan_in%Num_Stat
      idurx =scan_in%idurx
      istcur(1:nstn)  =Scan_in%istn(1:Nstn)
      idur(1:nstn)  =Scan_in%idur(1:nstn) 
      ipass(1:nstn) =Scan_in%ipass(1:nstn)
      idir(1:nstn)  =Scan_in%idir(1:nstn) 
      ift(1:nstn)   =scan_in%ift(1:nstn) 

      mjdcur  =scan_in%mjd
      utcur   =scan_in%ut
      CALL SIDTM(MJD,ST0,FRAC)
      GST = DMOD(ST0 + UT*FRAC, 2.D0*PI)
      return
      end subroutine     

!*******************************************
      subroutine insert_scan(scan_in,ierr)
      type(sked_scan) scan_in
      integer ierr
      if(Num_scan .eq. Max_scan) then
        write(*,*) 
      endif    



      end subroutine

! **********************************************
      subroutine insert_scan2(scan_in)
      type(sked_scan) scan_in
      integer ierr
      end subroutine




      end module







       
   
      

