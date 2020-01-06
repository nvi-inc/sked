C
      SUBROUTINE MODCU
C
C     MODCU prompts user for editing the current observation line
C
      include '../skdrincl/skparm.ftni'
C
C     HISTORY:
C      NRV  ??????  CREATED
C      MWH  841018  ECHO CHANGED LINE FOR VERIFICATION
C      NRV  880310  DE-COMPC'D
C      GAG  890118  MODIFIED TO RECOGNIZE UC OR LC AND ADDED ABORT
C      NRV  890502  Changed LSHED call to include KDUR
C      nrv  890711  changed LSHED call to remove parameters
!    2010Mar25 JMGipson changed call to lshed to remove max_stn)
C
C  COMMON:
      include 'skcom.ftni'
! functions
      integer trimlen
      logical kyes_to_prompt

C
C  SUBROUTINES CALLED: PAKUP, UNPAK, LNFCH
C  CALLING SUBROUTINES: SKED
C
C  LOCAL:
C     KERR - error return from UNPAK, etc.
      integer i,j,ilen,kerr
      logical kxl_save
     
C
C  INITIALIZED:
C
C     1. First get the current observation packed up into the buffer.
C     Write out the observation and prompt the user for input.
      KERR = 1
      DO WHILE (KERR.NE.0) ! get edited line
        j=istcur(1)
        i=nsorcur(j)
        if (j.le.0.or.i.le.0) then
          write(luscn,*)"Current observation is not valid."
          return
        endif
        CALL PAKUP(ILEN,0)
C
        kxl_save=kxlist
        kxlist=.false.
        CALL LSHED(LUSCN,nstncur,ISTCUR)
        kxlist=kxl_save
C
        cbuftmp=cbuf
        ilen=trimlen(cbuftmp)
        write(luscn,'(1x,a)') cbuftmp(1:ilen)
        write(luscn,'(">",$)')
C
C     2. Fill in the reading buffer with blanks, then read what the
C     user typed.  Fill in edited string with original.
C     Check for correctness.
C
        read(luusr,'(a)') cbuftmp
        do i=1,iblen*2
          if(cbuftmp(i:i) .ne. " ") cbuf(i:I)=cbuftmp(i:i)
        end do
        ilen=trimlen(cbuf)
C
        WRITE(LUSCN,'(/1x,a)') cbuf(1:ilen)
        if(kyes_to_prompt("OK (Y/N)?")) then 
          CALL UNPAK(KERR,0)
        ELSE 
          return
        END IF
      END DO  !get edited line
C
      CALL PTOBS('RE',1,IERRCM)
      IF (IERRCM.NE.0) CALL WRERR(IERRCM,INUMCM)
C
990   RETURN
      END
