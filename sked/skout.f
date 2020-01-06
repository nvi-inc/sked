      SUBROUTINE skout(ierr)      !write out $SKED section
C
C  This routine writes out the $SKED section for SKCLS. 
C
C   HISTORY:
C     GAG   891114 CREATED
C     GAG   891121 Changed ISCUN(2) to lutmp
C 990915 nrv Replace REIO with WRITE.


C   parameter file
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
C
C   SUBROUTINES
C     CALLED BY:  SKCLS
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
! functions
      integer trimlen
C
C   OUTPUT:
      integer ierr
! local
      integer irec

C   LOCAL

      ierr=0
      write(lutmp,'(A)',err=100) "$SKED"
      IREC = 0
      DO WHILE (IREC.LT.NOBS) !read and write an observation
        IREC = IREC+1
C     Fill up IBUF from memory array, using index array 
        cbuf=cskobs(iskrec(irec))
        write(lutmp,'(a)') cbuf(1:trimlen(cbuf))
      END DO  !read and write an entry
      RETURN

100   continue
      ierr=1
      WRITE(luscn,'(a)')
     >  "SKOUT01: Error writing schedule: re-issue EC,ER,WC, or WR"
      CLOSE(lutmp)
      RETURN
      END
