      SUBROUTINE SEFDDS(icod,NSTN,ISTN,nsor,mjd,ut)
C
C   SEFDDS displays elevation-adjusted SEFDs
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  INPUT:
C     NSTN - number of stations
      integer ISTN(MAX_STN),nstn,icod,nsor,mjd
C      - station indices
C     icod - frequency code index
      real*8 ut
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C  Called by: NEWOB
C
C  LOCAL VARIABLES
      integer iband(max_band) ! from GTBAN call
      real*4 az,el,ha,dc,x,y
      logical kup
      integer nba,iba,k,j,i,is,ladj,idum,ichmv_ch
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 920527 Created, copied from FLUXDS
C     nrv 950404 Write 2-letter codes
C
C
C  1. Top line with title
C
      WRITE(LUDSP,'("SEFDs (* = adjusted for elevation): ")')
C
C  2. Second line with station names across.
C
! AEM undo      write(ludsp,'(12x,$)')
! AEM 20050119 'Nx'->" "
      write(ludsp,'(11x," ",$)')
      DO I=1,NSTN
        J=ISTN(I)
C       WRITE(LUDSP,'(A2,7X,$)') LSTCOD(J)
! AEM undo        WRITE(LUDSP,'(A2,7X,$)') LpoCOD(J)
! AEM 20050119 'Nx'->Nspaces
        WRITE(LUDSP,'(A2,6x," ",$)') cpoCOD(J)
      ENDDO
      WRITE(LUDSP,'()')
C

C   2.5 Next line with elevations

      write(ludsp,'("Elevation  ",$)')
      do i=1,nstn
        is=istn(i)
        call cvpos(nsor,is,mjd,ut,az,el,ha,dc,x,y,x,y,kup)
! AEM undo        write(ludsp,'(f4.1,5x,$)') el*180.0/PI
! AEM 20050119 'Nx'->Nspaces
        write(ludsp,'(f4.1,4x," ",$)') el*180.0/PI
      enddo
      write(ludsp,'()')

C  3. One line of SEFDs for each band
C
      call gtban(icod,nba,iband)
      do k=1,nba
        iba=iband(k)
        write(ludsp,'(A1,"-band ",$)') lband(iba)
        do i=1,nstn
          j=istn(i)
          idum= ichmv_ch(ladj,1,'  ')
          if (nsefdpar(iba,j).gt.0) idum= ichmv_ch(ladj,1,'* ')
          write(ludsp,'(f8.1,a1,$)') sefdstel(iba,j),ladj
        enddo
        write(ludsp,'()')
      enddo
C
      return
      END

