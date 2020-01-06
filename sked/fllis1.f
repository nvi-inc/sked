      subroutine fllis1(is,imaxl)

C FLLIS1 lists the flux info for one source

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
! function
      character*1 sochr


C Called by: FLLIS, VSCAN

C History
C 960729 nrv New, taken from FLLIS so that the VSCAN routine can
C            also list the source model.
C 970224 nrv Write out full source name. Write out 1 more digit for flux.
C 970224 nrv Add imaxl to call, for formatting
! 2007Jul02  JMG  Added flux.ftni which was separated from sourc.ftni
! 2009Sep29  JMG. Cleaned up printing. Write out "****MISSING****" if a flux is missing.
!

C Input
      integer is ! source index 
      integer imaxl ! longest source name, for formatting

C Local
      integer j,ib,ic,ic1

      write(ludsp,9209) is,sochr(is),csorna(is)(1:imaxl)
9209  format(I4,1x,a1,2x,a,$)
      if(nflux(1,is) .eq. 0 .and. nflux(2,is) .eq. 0) then
        write(ludsp,'(" *****MISSING******* ")') 
        return
      endif 
      do ib=1,nband
        if (ib.eq.2) write(ludsp,'(8x,a,$)') csorna(is)(1:imaxl) !blanks
        if(nflux(ib,is) .eq. 0) then
            write(ludsp,'(2x,a2," *****MISSING***** ")') lband(ib)
        endif

        if (nflux(ib,is).gt.0) then !flux for this source
          write(ludsp,9208) lband(ib)
9208      format(2x,a2,$)
          if (cfltype(ib,is).eq.'B') then !baseline steps
            write(ludsp,9210) cfltype(ib,is),(flux(j,ib,is),
     .      j=1,2*nflux(ib,is)+1)
9210        format(1x,a1,1x,f4.1,15(f6.2,1x,f7.0))
          else !model components
            do ic=1,nflux(ib,is)
              ic1 = 1 + (ic-1)*6
              if (ic.gt.1) write(ludsp,'(16x,2x,a2,$)') lband(ib)
              write(ludsp,9212) cfltype(ib,is), 
     .             (flux(j,ib,is),j=ic1,ic1+5)
C                        M    flux    MajAx   Ratio    PA
9212          format(1x,a1,1x,f6.2,1x,f5.2,2x,f4.2,1x,f6.1, 
     .        1x,f6.2,1x,f6.2)
C                off1    off2
            enddo
          endif
        endif
      enddo
      return
      end
