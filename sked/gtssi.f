C@GTSSI
      SUBROUTINE GTSSI(LINSTQ,ICH,NST,ISTN,IERR,lu)
C
C     GTSSI decodes user input fields which are source name and
C     station ID lists, by calling GTSOI and GTSTI respectively.
C
      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE 'skcom.ftni'
C
C  INPUT:
      integer*2 LINSTQ(*)
C          - string holding input, first word=length
      integer ich,lu
C     ICH - first char to scan in LINSTQ
C
C  OUTPUT:
      integer nst,ierr
C     NST - number of stations found
      integer ISTN(max_stn)
C     - list of station indices, one per word
C     IERR - error, 0=OK, nn=error message for WRERR
C
C
      CALL GTSOI(LINSTQ,ICH,IERR,lu)
      IF (IERR.NE.0) RETURN
C
      CALL GTSTI(LINSTQ,ICH,NST,ISTN,IERR,lu)
      IF (IERR.NE.0) RETURN
C
      RETURN
      END
