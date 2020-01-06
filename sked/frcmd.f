      SUBROUTINE FRCMD(LINSTR)
C
C   FRCMD determines the function requested in the FREQUENCIES command,
C              and then calls the appropriate subroutine to do it.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
! functions
      integer istringminmatch

C INPUT VARIABLES:
      INTEGER*2 LINSTR(*)
C        LINSTR - input string from user, word 1=length
C
C CALLING SUBROUTINES: SKED (main program)
C CALLED SUBROUTINES: IGTKY,FRLIS,FRSEL,GTSTI

C  LOCAL
      integer i,ich,ilen,ic1,ic2,ikey,istn(max_stn),nst
      integer idum,ichmv
      integer*2 lkeywd(12)
      character*1 cme
      integer ierr
! AEM 20050207 fit to lkeywd (12->24)
      character*24 ckeywd
      equivalence (lkeywd,ckeywd)

      integer ilist_len
      parameter (ilist_len=2)
      character*6 list(ilist_len)
      data list/"LIST","SELECT"/

C
C     880310 NRV DE-COMPC'D
C     900126 gag added the if for station select first
C 951017 nrv Change igtky call to use lkey
C 000106 nrv Add 's' to frsel call.
! 2018Jul10  JMG got rid of announcing obsolete option
C
C     1. First call the function IGTKY to decode the input string which
C        the user typed.  The codes returned are:
C                  SE = select
C                  LI = list
C

      IF (LINSTR(1) .LE. 0) THEN
        IKEY = -1
      ELSE
        ich=1
        ilen=linstr(1)
        call gtfld(linstr(2),ich,ilen,ic1,ic2)
        ckeywd=" "
        idum = ichmv(lkeywd(2),1,linstr(2),ic1,ic2-ic1+1)
        ikey=istringMinMatch(list,ilist_len,ckeywd)

      END IF

      IF (IKEY .LE. 0) THEN
        IERRCM = 14
        CALL WRERR(IERRCM,INUMCM)
        RETURN
      ELSE IF (list(ikey) .eq. "SELECT") then
        if (nstatn.le.0) then
          write(luscn,'("You must select stations first!")')
          return
         else
!          write(luscn,'(a)')
!     >    'FRCMD - You may also use CAT START to select modes.'
          cme='s'
          CALL FRSEL(cme,ierr)
          RETURN
         end if
      ELSE ! default is list.
        call gtfld(linstr(2),ich,ilen,ic1,ic2)
        if (ic1.gt.0) then ! list
          CALL GTSTI(LINSTR,IC1,NST,ISTN,IERRCM,luscn)
          if (ierrcm.ne.0) return
          if (nst.eq.0) then ! all
            nst=nstatn
            do i=1,nstatn
              istn(i)=i
            enddo
          endif ! all
        else ! default
          nst=0
        endif ! list/default
        CALL FRLIS(istn,nst)
        RETURN
      END IF  !list
C
      RETURN
      END
