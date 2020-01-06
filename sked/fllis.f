      SUBROUTINE FLLIS
C
C     FLLIS lists the selected source fluxes
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'

      integer trimlen
C
C     CALLING SUBROUTINES: FLCMD,SKOPN
C     CALLED SUBROUTINES: RADED
C
C   LOCAL VARIABLES
      integer imaxl
      integer is,ib
      integer ifluxfound(Max_band)
      integer ifluxtot
C
C  History
C  NRV 891114 Created, modeled after SOLIS
C  NRV 910924 Changed output to include model components
C  nrv 930708 Fix output format only
C  nrv 940225 Change output format to write more digits in the
C             flux and axis fields.
C 970224 nrv Find max source name, add IMAXL to fllis1 call
!  2007Jul02 JMG. Added flux.ftni (which was separated from sourc.ftni)
C
C
C     1. Check that we have something to list.
C
      IF (NSOURC .EQ. 0) THEN
        WRITE(LUSCN,"('FLLIS - No sources selected.')")
        RETURN
      endif
      if (nband.eq.0) then
        write(luscn,"('FLLIS - No frequency bands selected.')")
        return
      endif
C
C   2. Now check the fluxes.
C
      ifluxtot=0
      do ib=1,nband
        ifluxfound(ib)=0
        do is=1,nsourc
          ifluxfound(ib)=ifluxfound(ib)+nflux(ib,is)
        end do
        if(ifluxfound(ib) .eq. 0) then
          write(luscn,9111) lband(ib)
9111      format('FLLIS - No fluxes at all for band ',a2)
        endif
        ifluxtot=ifluxtot+ifluxfound(ib)
      enddo
      if (ifluxtot .eq. 0) return          !no fluxes to write!
C
C
C  3. Now list what fluxes we have.
C
      write(ludsp,9211)
9211  format(' #   Source     Band Type Base Flux',3('  Base   Flux')/
     . 26x,  'Flux MajAx Ratio  PA    Off1    Off2')
C
      imaxl=-1
      do is=1,nsourc
        imaxl=max(trimlen(csorna(is)),imaxl)
      enddo
      do is=1,nsourc
        call fllis1(is,imaxl)
      enddo
C
      RETURN
      END
