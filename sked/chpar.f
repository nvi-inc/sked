      SUBROUTINE CHPAR(LINSTQ,cFUNC,JT,JNET,njnet,KAUTIM,MINIDL,IERR)
C
C     CHPAR parses the command line input for CHCMD.
C
      include '../skdrincl/skparm.ftni'
C
C  COMMON
      include 'skcom.ftni'
! functions
      integer istringminmatch
      integer trimlen 
C
C  INPUT
      integer*2 LINSTQ(*)
      character*2 cfunc
C     - input command string
C     LFUNC - function of the command AU,TA,RM,CH
C
C  OUTPUT
      integer jt,jnet(*),njnet,minidl,ierr
C     JT, JNET - station indices for tape shift, tagging, tagging-to
      LOGICAL KAUTAP,KAUTIM
C     - true for autoshifting tape or time only
C     MINIDL - idle time to be flagged
C     IERR - if non-zero, then CHCMD should just return
C
C  LOCAL
      integer*2 LKEYWD(12)
      character*12 ckeywd
      equivalence (lkeywd(2),ckeywd)
! stores time range.
      integer*2 lrange(12)
      character*22 crange
      equivalence (lrange(2),crange)

C     - for calls to check key words
      integer ISTN(MAX_STN)
C     - station ID array for GTSTI
      integer icx,nch,iec,ifc,nst,ikey,ich,ias2b,idum,ichmv

      integer list_len
      parameter (list_len=2)
      character*4 list(list_len)

      data list/"IDLE","TIME"/

C
C  HISTORY
C  890516 NRV created by pulling out of CHCMD
C  890519 NRV Added check for IGNORE in tag-along
C  890531 NRV Removed IGNORE (use ADD command instead)
C  930702 nrv Add LU argument to GTSTI
C  940705 nrv Let JNET be a subnet
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed call to GTFLD to remove linstq
C 951018 nrv Test on cfunc instead of lfunc
! 2009Apr09 Added new range option ALL= ^-*"
! 2010Apr27 Made it so that Shift got correct arguments
! 2015Mar19 Got rid of some obsolete stuff having to do with tapes. 
C
C
C     1. First call GTDTR to decode the date time range.
C
      IERR=0  
      JT = 0
      NJNET = 0
      ICH = 1
      nch = linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,nch,IFC,IEC)
      nch = IEC-IFC+1
      if (ifc.gt.0) idum=ICHMV(Lrange(2),1,LINSTQ(2),IFC,nch)
      lrange(1) = nch
      IF (IFC.EQ.0) Lrange(1) = 0     
      
      if(nch .ne. 0) then
        call capitalize(crange)       
        if(crange(1:nch) .eq. "ALL") crange="^-*"
         endif 
      CALL GTDTR (Lrange,IERRCM)
      IF (IERRCM.NE.0) THEN !error in time range
        CALL WRERR(IERRCM,INUMCM)
        IERR=1
        RETURN
      ENDIF !error in time range
C
C     2. IF AUTOSHIFT, get the following word "TIME" or "TAPE" 
C        IF CHECK, get IDLE and time
C
      nch = linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,nch,IFC,IEC)
      NCH = IEC-IFC+1

      KAUTIM = .TRUE.
      MINIDL = 0
      IF  (cfunc.eq.'SH'.OR.cfunc.eq.'CH') THEN !autoshift or check
        IF  (IFC.NE.0) THEN !tape/time/idle
          ckeywd=" "
          idum= ICHMV(LKEYWD,ich,LINSTQ(2),IFC,NCH)
      
          ikey=istringminmatch(list,2,ckeywd)
          IF (IKEY.Eq.0) THEN !invalid option
            write(luscn,"('CHCMD02 - Invalid type of shift or check.')")
            ierr=1
            return
          else if(ikey .eq. -1) then
            write(luscn,"('CHCMD01 - Ambiguous specification')")
            IERR=1
            RETURN
          END IF  !invalid
          IF (list(ikey) .eq. "IDLE") then
            nch = linstq(1)
            CALL GTFLD(LINSTQ(2),ICH,nch,IFC,IEC)
            NCH = IEC-IFC+1
            MINIDL = 1
            IF (IFC.NE.0) THEN !decode idle time
              MINIDL = IAS2B(LINSTQ(2),IFC,NCH)
              IF (MINIDL.EQ.-32768) THEN
                WRITE(LUSCN,9117)
9117            FORMAT('Invalid minimum idle time for checking.')
                IERR=1
                RETURN
              ENDIF
            ENDIF !decode idle time
          else if(list(ikey) .eq. "TIME") then
             continue 
          endif 
   
          
        END IF  !tape/time
C
C   3. If TAGALONG, get station to tag and tag-to station
C
      ELSE IF (cfunc.eq.'RM'.OR.cfunc.eq.'AD'.OR.cfunc.eq.'TA') THEN
     . ! remove or tagalong, get station
        if (ifc.eq.0) then
          write(luscn,9300)
9300      format(' CHPAR01 - no parameters given.')
          ierr = -1
          return
        end if
        idum= ICHMV(LKEYWD(2),1,LINSTQ(2),IFC,NCH)
        LKEYWD(1) = NCH
        ICX = 1
        CALL GTSTI(LKEYWD,ICX,NST,ISTN,IERR,luscn)
        IF  (NST.NE.1) THEN !error
        IF (NST.GT.1) WRITE(LUSCN,9510)
9510      FORMAT('Only 1 station allowed at present')
          IF (NST.EQ.0) WRITE(LUSCN,9501)
9501      FORMAT('CHCMD01 - Invalid/unselected station ID')
          IERR=1
          RETURN
        END IF  !error
        JT = ISTN(1)
      END IF  ! remove or tagalong, get station
      IF (cfunc.eq.'TA') THEN !taglong, get tag-to station
        nch = linstq(1)
        CALL GTFLD(LINSTQ(2),ICH,nch,IFC,IEC)
        IF (IFC.NE.0) THEN !tag-to station(s)
          NCH=IEC-IFC+1
          idum= ICHMV(LKEYWD(2),1,LINSTQ(2),IFC,NCH)
          LKEYWD(1) = NCH
          ICX = 1
          CALL GTSTI(LKEYWD,ICX,njnet,JNET,IERR,luscn)
C         IF  (NST.GT.1) THEN !error
C           WRITE(LUSCN,9510)
C           IERR=1
C           RETURN
C         END IF  !error
C         IF (NST.EQ.1) JNET=ISTN(1)
        ENDIF !tag-to station
      ENDIF !taglong, get tag-to station
C
      RETURN
      END
