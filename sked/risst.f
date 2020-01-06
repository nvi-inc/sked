      SUBROUTINE RISST(ISOR,JSTN,NSTN,ISTN,I1,I2,I3,IMIN,CRS)
C
C   RISST finds the time at which a source's mutual visibiliy changes.
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT VARIABLES:
      integer isor,jstn,nstn,istn(*),i1,i2,i3
C        ISOR   - source index number
C     JSTN - station index number
C     NSTN - number of stations
C        I1,I2,I3 - indices for DO loop: start,stop,increment (minutes)
C
C  OUTPUT VARIABLES:
      integer imin
      character*2 crs
C        IMIN   - loop index at which mutual visibility changes
C        CRS    - code returned RI(rising), SE(setting), NR(never will
C                 rise, NS(never will set).
C
C COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLING SUBROUTINES: MUVIS
C     CALLED SUBROUTINES: CVPOS
C
C  LOCAL VARIABLES
      LOGICAL KUPPRE ! previous KUP
      real*4 az,el,ha,dec,x30,y30,x85,y85
C               - for CVPOS, not used.
      integer i,j,nup,jj
      LOGICAL KUP ! TRUE if source is up, from CVPOS
C
C     HISTORY:
C     WHO  WHEN    WHAT
C     WEH  830523  ADD DEC TO CVPOS CALLS
C     MWH  840827  USE ELEV PARM IN DETERMINING RISE/SET TIMES
C     NRV  880311  DE-COMPC'D
C     nrv  900425  Added check for SEST type axis
C     NRV  900511  Added check for ALGO type axis
C     nrv  930225  implicit none
C
C
C     1. One loop using the indices sent to us by MUVIS.  The first time
C        we are called, it will be incrementing the minutes by 60 to
C        check out the hours.  Then we will be called incrementing by
C        smaller amounts, down to 1 minute.
C     When KUP changes, then we have either a rising
C        time or a setting time.  Set the code for the proper event,
C        and return.
C
      DO 200 I=I1,I2+I3-1,I3
C                   Do the loop up to the limit PLUS one less than the
C                   increment to insure that the last value is actually
C                   done.
       IF  (JSTN.EQ.NSTATN+1) THEN  !mutual vis, all stations
           NUP=0
           DO  J=1,NSTN
C
               JJ = ISTN(J)
               CALL CVPOS(ISOR,JJ,MJDCUR(1),I*60.D0,AZ,EL,HA,DEC,
     .         X30,Y30,X85,Y85,KUP)
C              IF (IAXIS(JJ).EQ.3.or.iaxis(jj).eq.6.or.iaxis(jj).eq.7) 
C    .         KUP=EL.GE.STNLIM(1,2,JJ)
C    .             .AND.EL.GE.STNELV(JJ)
               IF ((IAXIS(JJ).EQ.3.or.iaxis(jj).eq.6.or.iaxis(jj).eq.7)
     .             .AND..NOT.KUP.AND.EL.GT.STNLIM(2,2,JJ)) KUP=.TRUE.
C                   For AZEL, check only the horizon limit and avoid
C                   the zenith hole
               IF (KUP) NUP=NUP+1
               END DO  !
           KUP = NUP.EQ.NSTN
         ELSE  !single station
           CALL CVPOS(ISOR,JSTN,MJDCUR(1),I*60.D0,AZ,EL,HA,DEC,
     .     X30,Y30,X85,Y85,KUP)
C          IF (IAXIS(JSTN).EQ.3.or.iaxis(jstn).eq.6.or.iaxis(jstn).eq.7) 
C    .     KUP=EL.GE.STNLIM(1,2,JSTN).AND.EL.GE.STNELV(JSTN)
           IF ((IAXIS(JSTN).EQ.3.or.iaxis(jstn).eq.6.
     .     or.iaxis(jstn).eq.7)
     .     .AND..NOT.KUP.AND.EL.GT.STNLIM(2,2,JSTN)) KUP=.TRUE.
C                   Avoid the hole at the zenith
           END IF  !single station
C
        IF (I.EQ.I1) GOTO 190
        IF (.NOT.KUP.AND.KUPPRE) THEN
          GOTO 110
        ELSE
          GOTO 120
        END IF
C     The source has lost mutual visibility (setting)
110     IMIN=I
        crs='SE'
        GOTO 900
120     IF (KUP.AND..NOT.KUPPRE) THEN
          GOTO 130
        ELSE
          GOTO 190
        END IF
C     The source has become mutually visible (rising)
130     IMIN=I
        crs='RI'
        GOTO 900
190     KUPPRE=KUP
C                   Save the current value of number of stations UP
200   CONTINUE
C     If we got here, we went through the entire loop with no
C     change in mutual visibility.  Either never rise or never set.
      IF (.NOT.KUP) crs='NR'
      IF (KUP) crs='NS'
900   RETURN
      END
C
