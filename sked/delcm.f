      SUBROUTINE DELCM(LINSTQ,CFUNC)
C
C   DELCM deletes and copies groups of observations.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
C
C     INPUT VARIABLES:
      integer*2 LINSTQ(*)
C               - input string containing date/time range, word 1=length
      character*2 cfunc ! function code.  CO=copy, DE=delete.ng units
C
      include 'skcom.ftni'
      include 'covar.ftni'
! functions
      integer ICHMV

C     CALLING SUBROUTINES: SKED
C
C     CALLED SUBROUTINES: GTDTR, LOCF, GTOBS, APOSN, PTOBS                  
C                                                                           
C   LOCAL VARIABLES                                                         
      LOGICAL KHEAD
      integer ISTN(MAX_STN)
      LOGICAL KSTART,KRWND,KGOT
C               - for GTOBS
      integer irsave,nrec,nstn
      integer nch   !number of characters.
      integer idummy,ilen
C
C   PROGRAMMER: nrv
C     LAST MODIFIED: 810114
C     880310 NRV DECOMPC'D
C     930224 nrv implicit none
C     930623 nrv Remove some GOTO's and replace with if/then/else
C                Delete one fewer observation, i.e. do not delete the
C                last observation gotten in the counting loop because
C                that is the one that is beyond the end of the stopping
C                time.
C 000807 nrv Re-initialize NSORCU=0 if NOBS=0. This forces the
C            first obs of a scan to start at zero footage.
! 2009Apr09  JMGipson. Added "del all" option
C
C     1. First get the date\time range with GTDTR.
C
      IF  (LINSTQ(1).EQ.0) THEN  !
        IERRCM = 4
        CALL WRERR(IERRCM,INUMCM)
        RETURN
      END IF  !
! check for deleting all obs.
      cbuf=" "
      ilen=linstq(1)
      IDUMMY = ICHMV(ibuf,1,LINSTQ(2),1,ilen)   !put into cbuf.
      call squeezeleft(cbuf,nch)
      call capitalize(cbuf)
      if(cbuf(1:3) .eq. "^-*" .or. cbuf(1:3) .eq. "ALL") then
        call delete_all_obs
        dnorm_tri=0.
        return
      endif

      CALL GTDTR (LINSTQ,IERRCM)
      IF (IERRCM.NE.0) then
        CALL WRERR(IERRCM,INUMCM)
        return
      endif
C
C     2. Get the first observation which is to be deleted.
C     Save the current record number so we can return to it.
C
100   KSTART = .TRUE.
      ISORCM=0
      KRWND = .FALSE.
      CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)
      IF (IERRCM.NE.0) Then
        CALL WRERR(IERRCM,INUMCM)
        return
      endif
      IF (.not.KGOT) then
        IERRCM = 27
        CALL WRERR(IERRCM,INUMCM)
        return
      endif
C
C     3. First save the record we're at so we can return to it.
C     If a time range was specified, count the total number
C     of observations in the range.  If a number of observations
C     was specified, we don't need to do this.
C
      IRSAVE = IRCUR-1
      IF (IRSAVE.LE.0) IRSAVE=1
      IF (NOBSCM.GT.0) THEN 
        nrec = nobscm
      else ! count records in range
        CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)
        do while (kgot) !get records in time range
          CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)
          IF (IERRCM.NE.0) then
            CALL WRERR(IERRCM,INUMCM)
            return
          endif
        enddo !get records in time range
C       The number of records gotten in the time range is IRCNT.
C       Do not include the last one in the list to delete because
C       its time will be the first one beyond the ending time.
C       IRCNT is the number of records that were found in the range.
C       NREC = IRCNT-1
C       if (nrec.le.0) nrec=1
        nrec = ircnt
        KSTART = .TRUE.
        KRWND = .TRUE.
C       This is a call to GTOBS to reposition at start
        CALL GTOBS (KSTART,KRWND,KGOT,IERRCM)
        if(ircur .eq. 0) ircur=1
      endif
C
C     4. Now tell PTOBS to do the actual deleting.
C
      CALL PTOBS (CFUNC,NREC,IERRCM)
      IF (IERRCM.NE.0) then
        CALL WRERR(IERRCM,INUMCM)
        return
      endif
C
C     5. Re-position at the record BEFORE the first one deleted
C     by setting IRECGO.
C
      KSTART = .TRUE.
      KRWND = .TRUE.
      IRECGO = IRSAVE
      CALL GTOBS(KSTART,KRWND,KGOT,IERRCM)
      NSTN = 0
      KHEAD=.TRUE.
      IF (KGOT) CALL LSCUR(KHEAD,ISTN,NSTN,90.0)
      IERRCM = 22
      CALL WRERR(IERRCM,INUMCM)
      if (nobs.eq.0) then !
        call delete_all_obs
        dnorm_tri=0.
      endif ! re-initialize NSTNCU
C
      RETURN                                                                
      END

