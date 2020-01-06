C@INCUR
C
      SUBROUTINE INCUR  ! INITIALIZE TAPE VARIABLES C#861203:12:12#
C
      include '../skdrincl/skparm.ftni'
C
      include 'skcom.ftni'
C
C  DATE  WHO  CHANGE
C 841018 MWH  CREATED
C 880310 NRV  DE-COMPC'D
C
      integer i,ierr
C
      IF  (NSTNCUr.GT.0) THEN !station loop
        DO  I=1,NSTNCUr !initialize footage, dir, pass
          IFTCUR(ISTCUR(I)) = 0
          IDIRcur(ISTCUR(I)) = 1
          IPAScur(ISTCUR(I)) = 1
        END DO  !initialize
        CALL PTOBS('RE',1,IERR)
        IF (IERR.NE.0) CALL WRERR(IERR,INUMCM)
      END IF  !station loop
      RETURN
      END
