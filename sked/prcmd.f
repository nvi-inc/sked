C@PRCMD
C
      SUBROUTINE PRCMD(LINSTR)
C
C   PRCMD gets the function to be performed for the PARAMETERS
C              command and calls the appropriate subroutine to do the
C              work.
C

      include '../skdrincl/skparm.ftni'
C
C     INPUT VARIABLES:
        INTEGER*2 LINSTR(*)
C        LINSTR  - Remainder of input string, after the command
C                 has been stripped off.  This array is interpreted
C                 using the QSUBS routines.
C
C COMMON BLOCKS USED
      include 'skcom.ftni'
! functions
      integer istringminmatch
C
C     CALLING SUBROUTINES: SKED
C     CALLED SUBROUTINES: IGTKY (to interpret the function requested)
C                         PRSET (to set the parameters)
C                         PRLIS (to list the parameters)
C
C  LOCAL VARIABLES
       integer*2 lkeywd(12) ! key word
       integer nc,ifunc,ich,ic1,ic2,idummy
       integer i2long,ichmv

! AEM 20050204 char*12->char*24 to fit 'lkeywd'
      character*24 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=2)
      character*6 list(ilist_len)
      data list/"LIST","SELECT"/

C
C  HISTORY:
C     880310 NRV DE-COMPC'D
C     880819 PMR added CEQ
C     890815 GAG added parameter to PRLIS call
C     891114 GAG added KNEWPA logical
C 951017 nrv Fixed gtfld call to remove linstq
C 000926 nrv Don't move more characters into LKEYWD than will fit.
C
C
C     1.  Get the function from IGTFN. If unrecognized, write error
C         message and return.
C
      IF (LINSTR(1).gt.0) THEN ! either set or list or get or start
        ich=1
        ckeywd=" "
        call gtfld(linstr(2),ich,i2long(linstr(1)),ic1,ic2)
! AEM 20050204 min->min0
        nc = min0(ic2-ic1+1,24)
        idummy = ichmv(lkeywd,3,linstr(2),ic1,nc)
        ifunc = istringminmatch(list,ilist_len,ckeywd)
        if(ifunc .gt. 0 .and. list(ifunc) .eq. "LIST") then
          CALL PRLIS(linstr,ich)
        ELSE
          CALL PRSET(LINSTR)
          KNEWPA = .TRUE.
        END IF
      ELSE
        CALL PRLIS(linstr,ich)
      END IF

      RETURN
      END

