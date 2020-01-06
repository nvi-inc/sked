      SUBROUTINE DSPOP(cstpna,cerpna)
C
C     DSPOP displays the page full of optimization parameters
C
C  History:
C  NRV 910905 Created
C  NRV 911026 Modified to include new major/minor options
C  nrv 920706 added 2nd line of parameters, for noise floor
C  nrv 930210 changed to curses
C  nrv 930225 implicit none
C  nrv 930315 shorten words to fit on 80-char screen
C  nrv 930720 Add even # sources
C  nrv 930930 Add low el option
C  nrv 931013 Add expand option
C  nrv 931028 Add rise/set option
C  nrv 931029 Add min slew and min between
!  2010Apr15 JMG.  Removed parameters set by major or minor options. 

C  COMMON:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'covar.ftni'
      
C  CALLED BY: seop
C  CALLS: cursor control routines

C  INPUT:
      character*8 cstpna(8),cerpna(8)

C  LOCAL:
! AEM 20041217 int->int*4 passed to C-module
      integer*4 izero
      integer i,j,k,itype

      izero=0
      CALL SETCR_mn(izero,izero)
      CALL clear_mn
C
C Optimize  /  estimate sections
      do itype=1,2 !optimize, estimate
        if (itype.eq.1) then
          cbuftmp='  Optimize       '
        else
          cbuftmp='  Estimate       '
        endif
        call addstr_f(cbuftmp(1:17)) !AEM 1:18 -> 1:17
        do i=1,5
          if (lpara(i,itype)) call reverse_on_mn
          write(cbuftmp,'(a8)') cerpna(i)
          call addstr_f(cbuftmp(1:7))
          if (lpara(i,itype)) call reverse_off_mn
          call addstr_f(' ') !AEM uncommented
        enddo

C End of third line
        call nl_mn

        do i=1,nstatn
          write(cbuftmp,'(a8)') cstnna(i)
          call addstr_f(cbuftmp(1:9))
          do j=1,8
            if (j.eq.1) k=5+2*i-1
            if (j.eq.2) k=5+2*i
            if (j.eq.3) k=5+2*nstatn+(3*i)-2
            if (j.eq.4) k=5+2*nstatn+(3*i)-1
            if (j.eq.5) k=5+2*nstatn+(3*i)
            if (j.eq.6) k=5+5*nstatn+(3*i)-2
            if (j.eq.7) k=5+5*nstatn+(3*i)-1
            if (j.eq.8) k=5+5*nstatn+(3*i)
            if (lpara(k,itype)) call reverse_on_mn
            write(cbuftmp,'(a8)') cstpna(j)
            call addstr_f(cbuftmp(1:7))
            if (lpara(k,itype)) call reverse_off_mn
            if (j.ne.8) call addstr_f(' ')
            if (j.eq.8) call nl_mn
          enddo
        enddo
      enddo !optimize, estimate

      RETURN
      END
