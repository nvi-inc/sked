      SUBROUTINE FLUXDS(NSTN,ISTN,ICOD)
C
C   FLUXDS displays observed FLUXs in matrix format.
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
C     NSTN - number of stations
      integer ISTN(MAX_STN),nstn,icod
C      - station indices
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C  Called by: VSCAN, NEWOB
C
C  LOCAL VARIABLES
      integer iband(max_band) ! from GTBAN call
! AEM 20050204 char*8->char*12
      character*12 cnnx ! format 'nnX'
      integer nba,k,iba,ib,i,j,is,js
      integer ibnum ! function
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 910925 Created, copied from SNRDS
C
C
C  1. Top line with title and band names
C
      WRITE(LUDSP,'(" Observed flux by baseline: ")')
      call gtban(icod,nba,iband)
      do k=1,nba
        iba=iband(k)
! AEM undo        write(ludsp,'(10X,A1,"-band",10x,$)') lband(iba)
! AEM 20050119 'Nx'->Nspaces
        write(ludsp,'(10x,a1,"-band",9x," ",$)') lband(iba)
      enddo
      WRITE(LUDSP,'()')
C
C  2. Second line with station names across.
C
      do k=1,nba
! AEM undo       if (k.eq.1) write(ludsp,'(6X,$)')
!       if (k.eq.2) write(ludsp,'(9X,$)')
! AEM 20050119 'Nx'->Nspaces
        if (k.eq.1) write(ludsp,'(5x," ",$)')
        if (k.eq.2) write(ludsp,'(8x," ",$)')
        DO I=1,NSTN-1
          J=ISTN(I)
C         WRITE(LUDSP,'(1x,A2,2X,$)') LSTCOD(J)
! AEM undo          WRITE(LUDSP,'(1x,A2,2X,$)') LpoCOD(J)
! AEM 20050120 'Nx'->Nspaces
          WRITE(LUDSP,'(1x,a2,"  ",$)') cpoCOD(J)
        ENDDO
      enddo
      WRITE(LUDSP,'()')
C
C  3. Matrix lines, one for each station.
C
      DO I=2,NSTN ! each station down
        IS=ISTN(I)
! AEM undo       write(cnnx,9200) 5+(nstn-i)*5
!9200    format('(',i3,'X,$)')
! AEM 20050204 replace with simple
      	cnnx = '(   x," ",$)'
	write(cnnx(2:4),'(i3)') 4+(nstn-i)*5
        do k=1,nba ! each band
          iba=iband(k)
          if (iba.gt.1) write(ludsp,cnnx)
C         WRITE(LUDSP,9103)LSTCOD(IS)
! AEM undo          WRITE(LUDSP,9103)LpoCOD(IS)
!9103      FORMAT(2X,A2,$)
! AEM 20050120 avoid 'format' statement
          write(LUDSP,'(2x,a2,$)') cpoCOD(IS)

          DO J=1,I-1 ! each station across
            JS = ISTN(J)
            ib = ibnum(is,js)
! AEM undo            WRITE(LUDSP,9104) factbl(iba,IB)
!9104        FORMAT(1X,f4.1,$)
! AEM 20050120 avoid 'format' statement
          write(LUDSP,'(1x,f4.1,$)') factbl(iba,IB)
          ENDDO ! each station across
        enddo ! each band
        WRITE(LUDSP,'()')
      ENDDO ! each station down
C
      return
      END

