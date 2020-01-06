C@SUBDIS
      SUBROUTINE SUBDIS(LINSTR)
C
C     SUBDIS processes the SUBCON=ON/OFF command
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
C
C
      include 'skcom.ftni'
! function
      integer iStringMinMatch
      integer*2 LINSTR(*)
C         - input string
C
C  LOCAL
      integer*2 LKEYWD(12)
      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer i2long,ich,ic1,ic2,idum,ikey,ichmv

      integer ilist_len
      parameter (ilist_len=2)
      character*5 list(ilist_len)
      data list/"OFF","ON"/

C
C  DATE    WHO   CHANGE
C 930323   nrv   Copied from BSELN
C 951017 nrv Fixed gtfld call to remove linstq
! 2010Mar20.  Removed dependence on KPART (part of obsolete baseline command) which was removed. 
C
C
C     1. Check for ON/OFF in input string.
C
      ICH = 1
      CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)

      IF  (IC1.EQ.0) THEN  !toggle 
        KDISSUB = .NOT.KDISSUB                                              
        IF (KDISSUB) WRITE(LUSCN,9001)                                     
        IF (.NOT.KDISSUB) WRITE(LUSCN,9002)                                
9001    FORMAT(/'Subconfiguration display is being turned ON'/)                           
9002    FORMAT(/'Subconfiguration display is being turned OFF'/)                          
        RETURN                                                            
      END IF  !toggle 
C
C     Check for ON/OFF key words
      ckeywd=" "
      IDUM = ICHMV(LKEYWD,3,LINSTR(2),IC1,IC2-IC1+1)
      ikey = istringminmatch(list,ilist_len,ckeywd)
      IF (IKEY.LE.0) THEN
        WRITE(LUSCN,'(a)') "Invalid or ambiguous:"//
     >   "Subconfiguration display must be turned ON or OFF."
        RETURN
      ENDIF
      kdissub=list(ikey) .eq. "ON"
      RETURN
      END
C
