      SUBROUTINE YDHMS(cdt, ierr, IYR,IDAY,IHR,IMIN,ISEC)
C
C   This routine decodes the interactive entry of dates into sked.
C   YDHMS decodes the input string into integers which give the
C              year, day, hour, minute, second.
C   COMMON BLOCKS USED
      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE 'skcom.ftni'
      include '../skdrincl/skobs.ftni'
! functions
      integer trimlen
      integer iday0
      integer iStringMinMatch
C
C  INPUT VARIABLES:
      character*30 cdt
!  Valid values are:  ^=first obs
!                     .=current obs
!                     *=last obs
!                     First
!                     Now
!                     Current
!                     Beg or Start
!                     End
!
! Or a string:
!               YYYYDDDHHMMSS
! String can be separated by : or /, which will be removed.
! After removal, valid formats are:
!             YYYYDDDHHMMSS   13digits
!               YYDDDHHMMSS   11digits
!                 DDDHHMMSS    9digits
!                 DDDHHMM      7digits
!                    HHMMSS    6digits
!                    HHMM      4digits.
! Completely re-written 2006Nov15 JMGipson
! 2007Feb23.  Added "-" to list of characters allowed for readability.
! 2008Jun06   Added other special times.
!             Removed "-" because of conflict in giving range.
! 2010Apr26  JMG Better error message. 
! 2018Feb15  KOL Added seconds when reading the end time

! Local
      integer maxlist
      parameter (maxlist=10)
      character*10 list(maxlist)
      data list/"^","FIRST",            !first observation
     >          ".","NOW","CURRENT",    !current
     >          "*","LAST",             !last obs
     >         "BEGIN","START",         !start of experiment
     >         "END"/                   !end of experiment

      integer ierr                    !some error
      integer iyr,iday,ihr,imin,isec  !time
      integer ikey

      character*30 cin
      integer ilen
      integer j
      integer im

! Check special characters.
! special characters

      ierr = 0

! Initialize year and day to current value.
      J = ISTCUR(1)
      IF (NSTNCUr.EQ.0) J=1
      IYR=IYRCUR(J) ! full 4-digit year
      IDAY=IDACUR(J)
      ihr=0
      imin=0
      isec=0

! See if one of the special times specified as
      call capitalize(cdt)
      ikey=iStringMinMatch(list,Maxlist,cdt)

      select case(ikey)
!! This used to be case 1 or 2, case(-2) means not selected
!!    case(1,2)
      case(-2)
        IYR=1900
        IDAY=01
        return
      case(3,5)
        call seconds2hms(utcur(j),ihr,imin,isec)  !Current obs
        return
!! This used to be case 6 or 7, case(-1) means not selected
!!    case(6,7)
      case(-1)
! Star indicates last point--set time to way in the future.
        IYR=2099
        IDAY=366
        return
!! This used to be case 8 or 9
!!    case(8,9) ! one minute before start of session
      case(1,2,8,9)
        iyr  = iyr_start
        iday = ida_start
        ihr  = ihr_start
        imin = imin_start-1
        if(imin .lt. 0) then
           ihr=ihr-1
           imin=59
           if(ihr.lt. 0) then
              ihr=23
              iday=iday-1
           endif
        endif

        return
!! This used to be case(10)
!!    case(10)
      case(6,7,10)
        iyr  = iyr_end
        iday = ida_end
        ihr  = ihr_end
        imin = imin_end
        isec = isc_end
        return
      endselect

! not a special character.
! Remove characters such as ":" and "/"

      ilen=0
      cin=" "
      do j=1,trimlen(cdt)
        if(cdt(j:j) .ne. ":" .and. cdt(j:j) .ne. "/") then
           ilen=ilen+1
           cin(ilen:ilen)=cdt(j:j)
        endif
      end do

! Valid strings at this point:
!             YYYYMMDDHHMMSS  14digit
!             YYYYDDDHHMMSS   13digits
!               YYDDDHHMMSS   11digits
!                 DDDHHMMSS    9digits
!                 DDDHHMM      7digits
!                    HHMMSS    6digits
!                    HHMM      4digits.

      if(ilen .eq. 14) then
        read(cin,'(i4,i2,i2,i2,i2,i2)',err=990)
     >      iyr,im,iday,ihr,imin,isec
            iday=iday0(iyr,im)+iday
      else if(ilen .eq. 13) then
        read(cin,'(i4,i3,i2,i2,i2)',err=990) iyr,iday,ihr,imin,isec
      else if(ilen .eq. 11) then
        read(cin,'(i2,i3,i2,i2,i2)',err=990) iyr,iday,ihr,imin,isec
        if(iyr .ge. 70) then
           iyr=iyr+1900
        else
           iyr=iyr+2000
        endif
      else if(ilen .eq. 9) then
        read(cin,'(i3,i2,i2,i2)',err=990) iday,ihr,imin,isec
      else if(ilen .eq. 7) then
        read(cin,'(i3,i2,i2,i2)',err=990) iday,ihr,imin,isec
      else if(ilen .eq.  6) then
        read(cin,'(i2,i2,i2)',err=990) ihr,imin,isec
      else if(ilen .eq. 4) then
        read(cin,'(i2,i2,i2)',err=990) ihr,imin
      else
        write(luscn,'(a)')
     > 'YDHMS03: Incorrect format in time field:'//cin 
        goto 990
      endif

! Check that the time is valid 
      if(iyr .lt. 1979 .or. iyr .gt. 2050) then
         write(*,*) "Invalid year ", iyr
         goto 990
      else if(iday .lt. 0 .or. iday .gt. 366) then
         write(*,*) "Invalid day ",iday
         goto 990
      else if(ihr .lt. 0 .or. ihr .gt. 24) then
         write(*,*) "Invalid hour ", ihr
         goto 990
      else if(imin .lt. 0 .or. imin .gt. 59) then
         write(*,*) "Invalid minute ", imin
         goto 990
      else if(isec .lt. 0 .or. isec .gt. 59) then
         write(*,*) "Invalid second ", isec
         goto 990
      endif           
      RETURN

  990 ierr = 1
      RETURN
      END
