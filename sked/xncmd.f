      SUBROUTINE XNCMD(LINSTR)
C
C     XNCMD processes the XNEW=ON/OFF command
C
      include '../skdrincl/skparm.ftni'
C
! functions
      integer istringminmatch
C  INPUT:
      integer*2 LINSTR(*)
C
      include 'skcom.ftni'
C
C  LOCAL
      integer*2 LKEYWD(12)
      character*2 ckey
      integer ikey,ich,ic1,ic2,idum
      integer i2long,ichmv ! function
!
      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=5)
      character*6 list(ilist_len),listshort(ilist_len)
      data list/"BASE","FLUX","OFF","ON","SEFD"/

      data listshort/"BA","FL","OF","ON","SE"/

C
C HISTORY
C 970326 nrv New. Copied from XLCMD.
C
C
C     Set local variables until end of command is reached,
C     then set variables in common.
C     1. Check for ON/OFF in input string.
C
      ICH = 1
      CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)
      IF  (IC1.EQ.0) THEN  !toggle XNEW
        IF  (kxnewflux.or.kxnewsnr.or.kxnewsefd.or.kxnewbase) THEN  !
          kxnewflux = .FALSE.
          kxnewsnr = .FALSE.
          kxnewsefd = .FALSE.
          kxnewbase = .FALSE.
          WRITE(LUSCN,9001)
9001      FORMAT(/'XNEW is being turned OFF'/)
        ELSE  !
          kxnewflux = .TRUE.
          kxnewsnr = .TRUE.
          kxnewsefd = .TRUE.
          kxnewbase = .TRUE.
          WRITE(LUSCN,9002)
9002      FORMAT(/'XNEW is being turned ON'/)
        END IF  !
        RETURN
      END IF  !toggle XNEW
C
C     check key words on/off and feet/azel
C
      kxnewflux = .FALSE.
      kxnewsnr = .FALSE.
      kxnewsefd = .FALSE.
      kxnewbase = .FALSE.
      DO WHILE (IC1.GT.0)
        CALL IFILL(LKEYWD,1,12,oblank)
        ckeywd=" "
        IDUM = ICHMV(LKEYWD,1,LINSTR(2),IC1,IC2-IC1+1)
        ikey = istringminmatch(list,ilist_len,ckeywd)
        IF (IKEY.LE.0) THEN !error
          WRITE(LUSCN,8900)
8900      FORMAT('XNEW must be ON or OFF or FLUX, SNR, SEFD, or BASE.')
          RETURN
        ENDIF !error
        ckey=listshort(ikey)
        IF (CKEY.EQ.'ON') then
          kxnewflux = .TRUE.
          kxnewsnr = .TRUE.
          kxnewsefd = .TRUE.
          kxnewbase = .TRUE.
        endif
        IF (CKEY.EQ.'OF') then
          kxnewflux = .FALSE.
          kxnewsnr = .FALSE.
          kxnewsefd = .FALSE.
          kxnewbase = .FALSE.
        endif
        IF (CKEY.EQ.'FL') kxnewflux = .true.
        IF (CKEY.EQ.'SN') kxnewsnr = .true.
        IF (CKEY.EQ.'SE') kxnewsefd = .true.
        IF (CKEY.EQ.'BA') kxnewbase = .true.
C
        CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)
      ENDDO
C
      RETURN
      END
