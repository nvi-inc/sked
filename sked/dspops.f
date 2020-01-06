      SUBROUTINE DSPOPS
C
C     DSPOPS displays the page full of source names for parameter selection
C
C  History:
C  NRV 910910 Created
C  nrv 930210 Change to curses

C  COMMON:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'covar.ftni'

C  LOCAL:
! AEM 20041217 int->int*4 passed to C-module
      integer*4 izero,ione

      integer i,k,itype,ii
C
C
      izero=0
      ione=1
      CALL SETCR_mn(izero,izero)
      CALL clear_mn
C
      do itype=1,2 !optimize, estimate
        if (itype.eq.1) then
          cbuf='  Optimize        '
        else
          cbuf='  Estimate        '
        endif
! AEM undo       call addstr_f(cbuf)
        call addstr_f(cbuf(1:16))
        call nl_mn
        do i=1,nsourc,8
          do ii=i,min0(nsourc,i+7)
            k=5+nstatn*8+(ii*2)-1
            if (lpara(k,itype)) call reverse_on_mn
            write(cbuf,'(a)') csorna(ii)
            call addstr_f(cbuf(1:8))
            if (lpara(k,itype)) call reverse_off_mn
            call addstr_f(' ')
          enddo
          call nl_mn
        enddo
      enddo !optimize, estimate

C
      RETURN
      END
