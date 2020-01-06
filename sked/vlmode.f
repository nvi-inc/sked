      SUBROUTINE vlmode(LINSTR)
C
C     VLMODE processes the VLMODE=ON/OFF command
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
      integer*2 LINSTR(*)
C         - input string
C
C  Common blocks
      include 'skcom.ftni'
      integer iStringMinMatch
C
C  Local variables
      integer*2 LKEYWD(12)
      integer ikey,ich,ic1,ic2,idum
      integer i2long,ichmv

      character*12 ckeywd
      equivalence (lkeywd,ckeywd)

      integer ilist_len
      parameter (ilist_len=2)
      character*5 list(ilist_len)
      data list/"OFF","ON"/

C
C History
C 950519 nrv Copied TMLIN
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed gtfld call to remove linstq
C
C
C     1. Check for ON/OFF in input string.
C
      ICH = 1
      CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)
C
      IF  (IC1.EQ.0) THEN  !toggle knov
          knov = .NOT.knov
          IF (knov) WRITE(LUSCN,9001)
          IF (.NOT.knov) WRITE(LUSCN,9002)
9001      FORMAT(/'VLBA full-observe mode is being turned ON'/)
9002      FORMAT(/'VLBA full-observe mode is being turned OFF'/)
          RETURN
        END IF  !toggle knov
C
C     Check for on/off in command
      ckeywd=" "
      IDUM = ICHMV(LKEYWD,3,LINSTR(2),IC1,IC2-IC1+1)
      ikey = istringminmatch(list,ilist_len,ckeywd)
      IF (IKEY.LE.0) THEN !error
        IERRCM = 17
        CALL WRERR(IERRCM,INUMCM)
        RETURN
      ENDIF !error
      knov=list(ikey) .eq. "ON"
C
      RETURN
      END
