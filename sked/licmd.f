      SUBROUTINE LICMD(LINSTQ,cmdcod)
C
C   LICMD lists observations in the date/time range.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C  INPUT VARIABLES:
C     cmdcod - type of command
C             LI - list
C             NE - next
C             PR - previous
C             CU - current
      integer*2 LINSTQ(*)
      character*2 cmdcod
C               - input string containing date/time range, word 1=length

C
C     CALLING SUBROUTINES: SKED
C     CALLED SUBROUTINES: GTDTR (to decode date/time range)
C                         GTOBS (to retrieve records)
C                         GTSSI (to decode source/station selection)
C
C  LOCAL VARIABLES
      LOGICAL KSTART
C               - for GTOBS to get going
      integer IST(MAX_STN),nst
C      - indices of requested stations, sent to LSCUR
      LOGICAL KGOT
C               - returned by GTOBS, TRUE if we have a valid record in
C                 the CUR variables, else FALSE.
      LOGICAL KRWND
C               - Set to TRUE to rewind schedule before listing
      LOGICAL KHEAD
C               - set to TRUE to get LSCUR to print a header
      integer*2 LKEYWD(12)
      character*22 ckeywd
      equivalence (lkeywd(2),ckeywd)
C               - Holds parsed portions of input string
      integer*2 lcbpre(max_stn)
      integer idirpr(max_stn),nspre(max_stn)
      double precision DAS2B
      real val,ellim
      integer ichmv_ch,ichmv,i2long
      integer i,ich,ifc,iec,idummy,nch,ierr
C
C  History
C  NRV  810116  ????????
C  MWH  840813  Added printer LU lock
C  MWH  841018  Implemented pointer array for work file
C  MWH  841018  Add BACK command, have PREVIOUS list thru current obs
C  NRV 880310   DE-COMPC'D
C  NRV 890516   Added elevation limit to LIST
C  nrv 930225   implicit none
C 951017 nrv Fixed gtfld call to remove linstq
C
C     1. First we call GTDTR to get the requested date/time range into
C        CM variables in COMMON.
C
     
      IRECGO = 0
      ICH=1
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IFC,IEC)
      LKEYWD(1) = IEC-IFC+1
      nch = lkeywd(1)
      ckeywd=" "
      if (ifc.gt.0) IDUMMY = ICHMV(LKEYWD(2),1,LINSTQ(2),IFC,nch)
      IF (IFC.EQ.0) LKEYWD(1)=0
C
C     1.1 Now set up any peculiarities for the type of command we have.
C
      IF (cmdcod.eq.'CU') LKEYWD(1)=ichmv_ch(LKEYWD(2),1,'.')-1

! See if there is a number. If so, get it.
      IF (cmdcod.eq.'NE'.OR.cmdcod.eq.'PR'.OR.cmdcod.eq.'BK'.or.
     >    cmdcod.EQ.'^ ') THEN  !nnn -> #nnn
        nobscm=1
        if(ckeywd .ne. " ") then
          read(ckeywd,*,err=10) nobscm
          goto 11
10        write(luscn,'(a)') "LICMD:  Not a valid number. "
          return
        endif
11      continue
        IF  (cmdcod.eq.'PR'.OR.cmdcod.eq.'BK'.OR.cmdcod.eq.'^ ') THEN  !
          NOBSCM = MIN0(IRCUR,NOBSCM)
          IRECGO = MAX0(IRCUR-NOBSCM,1)
          NOBSCM = NOBSCM+1
          IF(cmdcod.eq.'BK'.OR.cmdcod.eq.'^ ') NOBSCM=1
        endif
C
C     1.2 Decode the date/time range now.
C
      else
        CALL GTDTR (LKEYWD,IERRCM)
      END IF  !
      IF (cmdcod.eq.'NE') IRECGO = IRCUR+1
      IF  (IERRCM.NE.0) THEN
        CALL WRERR(IERRCM,INUMCM)
        RETURN
      END IF  !
C
C     2. Get the source name and/or station specifications.
C
      CALL GTSSI(LINSTQ,ICH,NST,IST,IERRCM,luscn)
      IF  (IERRCM.NE.0) then
        RETURN
      endif
C
C     3. Decode elevation limit.
C
      ELLIM = 90.0
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IFC,IEC)
      IF (IFC.NE.0) THEN !el specified
        NCH = IEC-IFC+1
        VAL = DAS2B(LINSTQ(2),IFC,NCH,IERR)
        IF (IERR.LT.0.OR.VAL.LT.-90.0.OR.VAL.GT.90.0) THEN !invalid
          WRITE(LUSCN,'(a)')
     >      'Invalid value for elevation limit. Must be -90 to 90.'
          RETURN
        ENDIF !invalid
        ELLIM = VAL
      ENDIF !el specified
C
C
C     4. Now get observations and list them.
C
      KSTART = .TRUE.
      KRWND = .FALSE.
      KHEAD = .TRUE.
      do i=1,nstatn
        nspre(i)=-1
        idirpr(i)=0
        lcbpre(i)=0
      enddo
      IF  (cmdcod.eq.'PR') THEN !back up
        IRCNT = 0
        IRCUR = IRECGO-1
        IRECGO = 0
        KSTART = .FALSE.
      END IF  !

      CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)
  
      DO WHILE (KGOT)
        IF  (IERRCM.NE.0) THEN
          CALL WRERR(IERRCM,INUMCM)
          RETURN
        END IF  !
        CALL LSCUR (KHEAD,IST,NST,ELLIM)
        KHEAD = .FALSE.
        CALL GTOBS(KSTART,KRWND,KGOT,IERRCM)
      END DO  !

      WRITE(LUDSP,'(a)') 'End of listing.'
      RETURN
      END
C
