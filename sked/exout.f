      SUBROUTINE exout      !write out $EXPER section
C
C  This routine writes out the $EXPER section for SKCLS. 
C
C   HISTORY:
C     GAG   891114 CREATED
C     GAG   891121 Changed ISCUN(2) to lutmp
C 990915 nrv Replace REIO with WRITE
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
C
C     CALLED BY:   SKCLS
C
! funciton
      integer trimlen
C  LOCAL
      integer nch
      nch=trimlen(cexper)
      write(lutmp,'(a)') "$EXPER "//cexper(1:nch)
      write(luscn,'(a)') "$EXPER "//cexper(1:nch)
C
      RETURN
      END
