C@VISSS
      SUBROUTINE VISSS(ISOR,NSTN,ISTN,IUTRIS,IUTSET,MUTRIS,MUTSET)
C
C  VISSS computes the rise and set times for a source at a station
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
      integer isor,nstn,istn(*)
C     ISOR - source index
C     NSTN - number of stations
C     istn - station indices
C
C  OUTPUT:
      integer IUTRIS(*),IUTSET(*),mutris,mutset
C     IUTRIS - UT of rising, minutes 0-1440
C     IUTSET - UT of setting, minutes 0-1440
C
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C  LOCAL:
      integer is,i1,jstn,jmin,jjmin,j3min,iutr,iuts
      character*2 crs
C        CRS    - code returned from RISST, either RI(rising),
C                 SE(setting), NR(never rise), NS(never set)
C        JMIN   - time at which rise,set occurred, from RISST
      LOGICAL KRISE,KSET
C               - flags set to TRUE when we have got the rise,set times
C
C  Called by: MUVIS
C
C     LAST MODIFIED:  created 800930
C     880314 NRV DE-COMPC'D
C     930225 nrv implicit none
C
C
C        We call RISST which returns a time (number of minutes) and a code
C        for the meaning of the time (rise,set,never rise or set).
C
        DO  IS=1,NSTN+1 !station loop
        I1=1
        JSTN = ISTN(IS)
        IF (IS.EQ.NSTN+1) JSTN=NSTATN+1
C                   This is the cue to RISST to compute mutual vis
      KSET=.FALSE.
      KRISE=.FALSE.
C                   Start each source at 0 UT
105     CALL RISST(ISOR,JSTN,NSTN,ISTN,I1,1440,60,JMIN,CRS)
C                   Loop is DO I=1,1440,60.  Returned is
C                   JMIN (index at which it stopped) and
C                   CRS (code for rising or setting at JMIN).
        IF ((CRS.NE.'RI').AND.(CRS.NE.'SE')) GOTO 130
C                   Skip to "never" conditions
        CALL RISST(ISOR,JSTN,NSTN,ISTN,JMIN-60,JMIN,10,JJMIN,CRS)
        CALL RISST(ISOR,JSTN,NSTN,ISTN,JJMIN-10,JJMIN,1,J3MIN,CRS)
C                   Loop is over tens of minutes, then minutes to check
C                   out the minutes in the hour we just found.
C                   Returned is J3MIN, the actual minute
C                   of rising or setting (according to CRS).
        I1=J3MIN
        IF (CRS.EQ.'RI') THEN
          GOTO 110
        ELSE
          GOTO 120
        END IF
110     KRISE=.TRUE.
        IUTR=J3MIN
        IF (.NOT.KSET) GOTO 105
C                   Get setting time next
        GOTO 160
120     IUTS=J3MIN
        KSET=.TRUE.
        IF (.NOT.KRISE) GOTO 105
C                   Get rising time next
        GOTO 160
130     IUTR=0
        IF (CRS.EQ.'NR') IUTS=00
        IF (CRS.EQ.'NS') IUTS=1440
C
160   IF (IS  .EQ.NSTN+1) GOTO 165
      IUTRIS(JSTN) = IUTR
      IUTSET(JSTN) = IUTS
      GOTO 170
165   MUTRIS = IUTR
      MUTSET = IUTS
170     CONTINUE
        END DO  !station loop
C
      RETURN
      END
