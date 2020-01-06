C@NEWCM
C
      SUBROUTINE NEWCM(cmdline,nsubc)
CHS------------------------------------------------------------------------
CHS Newcm was extended for the parameter nsubc in the parameter list. This 
CHS parameter can either be 0 (insert/delete mode) or 1,2,3 or 4 (optimization
CHS mode). NEWCM is called by STATn once for each scan in a subconfiguration.
C
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  NEWCM calls the subroutines for the new observation command.
C
      include '../skdrincl/skparm.ftni'
C
! functions
      integer trimlen
C  INPUT VARIABLES:
      character*(*) cmdline
      integer nsubc
C               - input string with parameters, word 1=length
C
C  COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'covar.ftni'
C
C  SUBROUTINE INTERFACE:
C     CALLING SUBROUTINES: SKED, STATn
C     CALLED SUBROUTINES: NEWOB,PTOBS,LSCUR
C
C  LOCAL VARIABLES
      integer ISTN(MAX_STN),nstn,ilen
C      - array of stations in new observation
C        (from NEWOB, to LSCUR)
      LOGICAL KHEAD
C               - set to FALSE for no heading from LSCUR

C
C  HISTORY
C     880310 NRV DE-COMPC'D
C     921005 NRV Added last parameter to SIMUL call
C     930219 nrv sked/autosked merge
C     931014 nrv Do not call simul if coverage only
!   2004Oct29  JMGipson.  Changed input variable linstq (integer array)
!               to cmdline (ascii string).
C
C
C     1. We call NEWOB first to parse the input string and set
C        appropriate CUR values.
C        If all went OK, next call PTOBS to write out the CUR
C        variables into a new record.
C        Lastly, call LSCUR to list the observation.
C
      if(trimlen(cmdline) .eq. 0) then
        write(luscn,9100)
9100    format('NO SOURCE GIVEN, CAN''T SCHEDULE AN OBSERVATION')
        return
      end if
      CALL NEWOB (cmdline,ISTN,NSTN,IERRCM,nsubc)
      IF  (IERRCM.NE.0) THEN  !
        IF (IERRCM.GT.1) CALL WRERR(IERRCM,INUMCM)
        RETURN
      END IF  !
C
CHS---------------------------------------------------------------
CHS Insert/delete mode
C
 110  if(nsubc.eq.0) then  ! insert mode
      CALL PTOBS ('IN',1,IERRCM)
C                   Call with function INsert, 1 record.
      IF  (IERRCM.NE.0) THEN  !
        CALL WRERR(IERRCM,INUMCM)
        RETURN
      END IF  !
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CHS---------------------------------------------------------------
CHS Optimization mode
C
      else ! optimization algorithm(nsubc.gt.0)
        if(nobs.ge.max_obs) then
          ierrcm=30
        endif
        call pakup(ilen,nsubc)
        ctrial_scan(nsubc)=cbuf
        if (ircur.le.0) then
          write(ludsp,
     >   '("NEWCM01 - You must insert an initial observation!")')
          ierrcm=1
          return
        endif
C  Do not call SIMUL if only sky coverage is being done. But,
C  elevations are needed by the COVERAGE routine. Calculate
C  them here in preparation.
        call simul(nsubc,ircur,1,.true.,.false.)
C       if (.not.kOptBySky) then
C         call simul(nsubc,ircur,1,.true.,.false.)
C       else !calculate elevations only
C         j=istcurs(1)
C         do i=1,nstncus
C           call cvpos(nsorcus(j),istcurs(i),mjdcurs(j),
C    .      utcurs(j)+idurcus(j)/2,az,el,ha,dc,x30,y30,x85,y85,kup)
C           elev(istcurs(i))=el
C           azim(istcurs(i))=az
C         enddo
C       endif   
      endif
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
120   KHEAD=.FALSE.
      ISORCM = 0
      if(nsubc.eq.0) then
      CALL LSCUR (KHEAD,ISTN,NSTN,90.0)
      endif
C
 990  RETURN
      END
C
