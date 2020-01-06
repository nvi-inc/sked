      SUBROUTINE FLOUT
C
C FLOUT writes the $FLUX section of the SKED output file, based on
C   information contained in Common.  It is assumed that the output
C   file is already opened with lutmp.
C
      include '../skdrincl/skparm.ftni'
C
C  COMMMON BLOCKS USED:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
C
C  LOCAL VARIALBES:
C     NCH - position marker for output buffer
      integer trimlen,is,ib,i,j,ic,ic1
C
C  HISTORY:
C 891114 NRV Created, modeled after WHEAD
C 891121 GAG Changed ISCUN(2) to lutmp.
C 910924 NRV New format for new flux information
C 930706 nrv Second model component was overwriting the first
C            one before the buffer was written to the file.
C 940202 nrv Make output buffer longer so it doesn't overflow.
C 940216 nrv Add one more digit to model component output.
C 970224 nrv Change 4 to max_sorlen/2, write out name as char
C 970224 nrv Add one more digit to baseline component output.
C 970325 nrv Write out source name only to its length, not max_sorlen.
C 990915 nrv Replace REIO with WRITE
! 2007May03  increase number of flux steps to 30
! 2007Jul02  JMG  Added flux.ftni

C
C
C   1. Loop over sources and bands.
C
C     CALL REIO(2,LUSCN,IBUF,-NCH)
      write(lutmp,'(a)') "$FLUX"
C
      do is=1,nsourc
        i=trimlen(csorna(is))
        do ib=1,nband
          if (nflux(ib,is).gt.0) then
            if (cfltype(ib,is).eq.'B') then !baseline steps
              write(lutmp,9210) csorna(is)(1:i),lband(ib),
     >         cfltype(ib,is),(flux(j,ib,is),j=1,2*nflux(ib,is)+1)
!                  Name   band  type   0.0      flux
9210          format(a,2x,a2,1x,a1,1x,f4.2,1x,30(f6.2,1x,f7.1))
            else !model components
              do ic=1,nflux(ib,is)
                ic1 = 1 + (ic-1)*6
                write(lutmp,9211) csorna(is)(1:i),lband(ib),
     >           cfltype(ib,is),(flux(j,ib,is),j=ic1,ic1+5)
C                      name XS    M     flux    MajAx   Ratio    PA
9211            format(a,2x,a2,1x,a1,1x,f6.2,1x,f5.2,1x,f4.2,1x,f6.1,
     >              2(1x,f6.2))
C                  off1    off2
              enddo
            endif
          endif
        enddo
      enddo
C
      RETURN
      END

