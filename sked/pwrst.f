      SUBROUTINE pwrst(cpos,num_sel,ierr)
C
C  Subroutine pwrst gets the position information from the 
C  position catalog and writes to the temeporary working SKED 
C  file SKW*.  
C
C   HISTORY:
C     WHO   WHEN    WHAT
C     gag   900108  created 
C     gag   900130  added logic to get original sked information
C     nrv   930225  implicit none
C 951018 nrv Removed lor option
C
C   parameter file
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C  INPUT:
      integer num_sel
      character*2 cpos(num_sel)
C
C  OUTPUT:
      integer ierr
! function
      integer iwhere_in_string_list
      integer trimlen  !variable and function from var length

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)

      integer i
      character*125 ldum
      integer iwhere

      integer num_done   ! counter
      logical kdone(max_stn)

      call open_cat(position_cat,ierr)
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
      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)

      iwhere=iwhere_in_string_list(cpos,num_sel,ltoken(1)(1:2))

      if(iwhere .eq. 0) goto 100
      if(.not. kdone(iwhere)) then
        if(kverbose) write(luscn,'(A,1x,$)') cpos(iwhere)
        write(lutmp,'(a)')"P "//ldum(1:trimlen(ldum))
        num_done=num_done+1
        kdone(iwhere)=.true.
      endif
      if(num_done .ne. num_sel) goto 100      !more to do?

190   continue
      close(lucat)
      if(kverbose) write(luscn,'(a)') ! write the EOL

200   continue
      if(num_done .ne. num_sel) then
        do i=1,num_sel
          if(.not.kdone(i)) then
            write(luscn,9600) cpos(i)
9600        format('PWRST: ERROR - Position entry not found for: ',A)
          endif
        end do
      endif

      RETURN
      END
