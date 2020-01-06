      SUBROUTINE chout(lhead,tfile) 
C
C  This routine writes out the station, source, lhead, codes,
C   flux, and exper $ sections for sked.
C
C
C   HISTORY:
C
C     WHO   WHEN   WHAT
C     GAG   900206 CREATED
C 990915 nrv Replace REIO with WRITE
C
C   parameter file
      include '../skdrincl/skparm.ftni'

      integer trimlen
C
C   SUBROUTINES
C     CALLED BY:   SKCLS
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
C
C   INPUT:
      character*8 lhead
      character*128 tfile

C  LOCAL:
      integer ierr
C
C   1. Open station select file, read each line, write into schedule file.
C
      write(lutmp,'(a)') "$"//lhead
      write(luscn,'(a)') "$"//lhead

      OPEN(lusel,file=tfile,iostat=IERR)
      IF (IERR.NE.0) THEN
        WRITE(LUSCN,9100) IERR,CSTFIL
9100    FORMAT('CHOUT01 - Error',i5,' opening station file 'A32)
        RETURN
      ENDIF

100   continue
      read(lusel,'(a512)',end=200) cbuf
      write(lutmp,'(a)') cbuf(1:trimlen(cbuf))
      goto 100

200   continue
      CLOSE(lusel)
C
      RETURN
      END
