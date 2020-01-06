      SUBROUTINE PRNCM(LINSTQ,cmdcod)

C     Print out the specified file

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'

! functions
      integer istringminmatch

      integer*2 linstq(*)
      character*2 cmdcod
      integer*2 LKEYWD(12)
C
C LOCAL VARIABLES
      integer ikey,i2long
      integer ierr,printer,ic1,ic2,ich,nc,idum,ichmv
      character*128 fname ! file name
      logical*4     kex
      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=4)
      character*10 list(ilist_len)
      data list/"PRINT","SCREEN","APPEND","OVERWRITE"/

C HISTORY
C   890522
C     to
C   890505  GAG  added <file> or printfile and 'laser' only 
C   900221  gag changed call to printer adding var. cprtpor cprtlan
C   900413  NRV  Changed call to "printer" to remove printer
C                name.
C   900516  gag changed ctpfil to cprfil.
C 951017 nrv Fixed gtfld call to remove linstq
C 960226 nrv Add 't' to printer call


C  Default is currently open file, temp or filename. 

      if (linstq(1).eq.0) then
        fname = ctpfil
C  Check the parameter for file or PRINT
      else
        ich = 1
        fname=" "
        call gtfld(linstq(2),ich,i2long(linstq(1)),ic1,ic2)
        nc = ic2 - ic1 + 1
        ckeywd=" "
        idum= ichmv(lkeywd,3,linstq(2),ic1,nc)
        ikey = iStringMinMatch(list,ilist_len,ckeywd)

!        if(ikey .eq. 0) then
        if (list(ikey) .eq. "PRINT") then
          call hol2char(linstq(2),ic1,ic2,fname)
        else
          fname = cprfil
        end if
      end if

C  Check if the designated print file exists.

      inquire(file=fname,exist=kex)
      if (.not.kex) then
        write(luscn,9000)
9000    format('PRNCM - Error: No file has been created;',
     .         ' Save with UNIT PRINT or UNIT <file>.')
        return
      end if

C  Close the print file and change display unit to screen. 

      write(luscn,9020) fname
9020  format('Closing file 'A32)
      close(lufil,status='keep',iostat=ierr)
      ludsp = luscn
      if (ierr .ne. 0) then
        write(luscn,9022)
9022    format(' PRNCM - Error closing file 'A32)
        return
      end if

C  Print the file with the default print device laser. Delete the
C  temporary print file.

      call null_term(fname)
      if (cmdcod .eq. "PT") THEN
        ierr = printer(fname,'t',cprtlan)
      else
        ierr = printer(fname,'t',cprtpor)
      end if
      if (ierr .ne. 0) then
        write(luscn,9024)
9024    format('PRNCM - Error calling printer routine')
        return
      end if
      if (fname.eq.cprfil) then
        open (lufil,status='old',file=fname)
        close (lufil,status='delete')
        write(luscn,9030) fname
9030    format('Deleting file 'A32)
      end if

      return
      end


