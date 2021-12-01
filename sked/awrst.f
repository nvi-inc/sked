      SUBROUTINE awrst(cname,cterm,cstnid,num_sel,ierr)
C
C  Subroutine awrst gets the antenna information from the
C  SKED temporary antenna file and/or the catalog file and
C  writes to a temporary working file. 
C
C   HISTORY:
C     WHO   WHEN   WHAT
C     gag   900108 created 
C     gag   900130 added logical lor for where to get information
C  nrv 930224 added implicit none
C  nrv 940519 Use ID from selection page, in case user changed it
C  nrv 950410 Remove above feature, using 2-letter codes always
C 951018 nrv Remove 'lor' option
C 960206 nrv Add back in using ID from selection page
C
C   PARAMETER FILE 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
!      include 'cat_stat.ftni'

C  INPUT:
      integer ierr
      character*8 cname(max_stn)
      character*4 cterm(max_stn)
      character cstnid(max_stn)
      integer num_sel

! functions
      integer iwhere_in_string_list
      integer trimlen
! local
      logical kdone(max_stn)
      integer num_done

      character*125 ldum
! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*20 ltoken(MaxToken)
      integer istart_vec(MaxToken)

! Used to hold data from catalog
      character*8 cat_name
      integer iwhere
      integer i
      integer nch

! 1. try opening the catalog file.
      call open_cat(antenna_cat, ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

      kdone=.false.
      num_done=0
! 2. Gothrough catalog until all of information is obtained  or reach end of file.
100   continue
      read(lucat,'(a125)',end=190) ldum
      if(ldum(1:1) .eq. "*" .or. ldum .eq. " ") goto 100
      call splitNtokens2(ldum,ltoken,Maxtoken,NumToken,istart_vec)
      cat_name=ltoken(2)
      iwhere=iwhere_in_string_list(cname,num_sel,cat_name)
      if(iwhere .eq. 0) then
        goto 100               !not selected
      endif
      
      if(.not. kdone(iwhere)) then
        num_done=num_done+1
        kdone(iwhere)=.true.
        if(iverbose_level.ge.5)  write(luscn,'(A," ",$)')
     >    cname(iwhere)(1:trimlen(cname(iwhere)))
        i=istart_vec(1)
        ldum(i:i)=cstnid(iwhere)
        i=istart_vec(15)
        ldum(i:125)=" "   !clear the rest of the line.
        ldum(i:i+3)=cterm(iwhere)
        if(numtoken .eq. 16) then
           ldum(i+5:i+6)=ltoken(16)(1:2)
        endif
!       nch=trimlen(ldum)
        nch=i+6
        write(lutmp,'(a)') "A "//ldum(1:nch)
      endif

      if(num_done .ne. num_sel) goto 100      !more to do?
190   continue
      close(lucat)
      if(iverbose_level.ge.5) write(luscn,'(a)')  !write the end of the line.

200   continue
      if(num_done .ne. num_sel) then
        do i=1,num_sel
          if(.not.kdone(i)) then
            write(luscn,9600) cname(i)
9600        format('AWRST: Error! - Antenna entry not found for: ',A)
          endif
        end do
      endif

      return
      END
