      SUBROUTINE PARCMD(cfunc)
C
C   PARCMD starts the parameters program if it's not already running
C   and sends the parameter values.
C   PARCMD retrieves parameters from the program if it's running.

C  HISTORY
C 000317 nrv New. Copied from catcmd.
C 000404 nrv Write out station names, terminal names.
C 000405 nrv Read opt file returned. Check for already running to restart.
C 000420 nrv Correct the logic so the opt file is read.
C 020930 nrv Correct the call to prread to include kvex and ivexnum.

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/statn.ftni'
      include 'skcom.ftni'
C
C Input
      character*2 cfunc   ! "JA" to start, "GT" to get parameters

C     CALLING SUBROUTINES: PRSET
C
C   LOCAL VARIABLES
      integer ilen,ierr,j,il
      integer trimlen
      integer ivexnum
      logical kvex ! set to false
C

C 1. If parameter program is not running then start it.
C      - create the parameter file
C      - start parameter program
      write(*,*) "PARCMD is obsolete"
      pause 
      return 
   

      IF  (CFUNC.EQ.'JA') THEN  ! Start java program

C 1.1 Create select file.

        OPEN(lutmp,file=cparam_file,iostat=IERR,status='new')
        if (ierr.lt.0) then
          il = trimlen(cparam_file)
          write(luscn,
     >   '("PARCMD00 - Error ",i6," opening parameter temp file ",a)')
     >   ierr,cparam_file(1:il)
        endif
        call exout ! write $EXPER line
        write(lutmp,'("$STATIONS")')
        if (nstatn.gt.0) then ! stations
          do j=1,nstatn-1 ! 2-letter IDs
            write(lutmp,'(a2,"-",$)') cpocod(j)
          enddo
          write(lutmp,'(a2)') cpocod(nstatn)
          do j=1,nstatn-1 ! 1-letter IDs
            write(lutmp,'(a1,"-",$)') cstcod(j)
          enddo
          write(lutmp,'(a2)') cstcod(nstatn)
          do j=1,nstatn ! station names
            write(lutmp,'(a," ",$)') cstnna(j)
          enddo
          write(lutmp,'()')
          do j=1,nstatn ! terminal names
            write(lutmp,'(a," ",$)') cterna(j)
          enddo
          write(lutmp,'()')
        endif ! stations
        call prout('s')
        call opout
        close(lutmp)

C 1.3 Start parameter program.

        if (.not.kparam) then ! not running
          call null_term(cparam_file)
          call null_term(par_program_path)
          call skpar(cparam_file,par_program_path)
          write(luscn, *) "PARCMD -- Starting Java and Parameters. "//
     >            "This will take a few minutes ...."
          write(luscn,*)
     >   "Use the command PARAMETERS GET to retrieve parametervalues."
          kparam=.true.
        else ! already running
          write(luscn,*) "PARCMD02 -- Parameters is already running. "//
     >     "Use the Restart button on the Parameters screen. "
          write(luscn,*) "Use the command PARAMETER GET to retrieve "//
     >     "parameter values."
        endif
C 2. Retrieve the returned parameter values.
      else if (cfunc.eq.'GT') then ! retrieve parameter values
        OPEN(luskd,file=cparam_file,iostat=IERR,status='old')
        il=trimlen(cparam_file)
        if (ierr.ne.0) then 
          write(luscn,'("PARCMD01 - Error ",i5," opening ",a)') 
     .    ierr, cparam_file(1:il)
          return 
        endif
        CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,1) ! read first line
        call exread ! read $EXPER line
        kvex = .false.
        ivexnum = 0
        call prread(kvex,ivexnum) ! read parameter lines
        rewind(luskd)
        do while(cbuf(1:3) .ne. "$OP")
          read(luskd,'(a)',end=500) cbuf
        end do
        call opread(luskd)
        close(luskd)
        return
      endif ! start/retrieve
      return

500   continue
      write(luscn, '("PARCMD: EOF reached before finding $OP")')

      RETURN
      END
