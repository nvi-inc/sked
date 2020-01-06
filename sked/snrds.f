      SUBROUTINE SNRDS(NSTN,ISTN,ICOD)
C
C   SNRDS displays variable scan lengths in matrix format.
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
      integer i,j,k,is,js,ib,iba,nba
      integer ibnum
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 890425 CREATED
C     NRV 891127 Display matrix for each band
C     nrv 950404 Write 2-letter codes
C
C
C  1. Top line with title and band names
C
      WRITE(LUDSP,'(" SNR by baseline: ")')
      call gtban(icod,nba,iband)
      do k=1,nba
        iba=iband(k)
! AEM undo        write(ludsp,'(10x,A1,"-band",5x,$)') lband(iba)
! AEM 20050120 'Nx'->Nspaces
        write(ludsp,'(10x,A1,"-band",4x," ",$)') lband(iba)
      enddo
      WRITE(LUDSP,'()')
C
C  2. Second line with station names across.
C
      do k=1,nba
! AEM undo       if (k.eq.1) write(ludsp,'(5X,$)')
!       if (k.eq.2) write(ludsp,'(8X,$)')
! AEM 20050119 'Nx'->Nspaces
        if (k.eq.1) write(ludsp,'(4x," ",$)')
        if (k.eq.2) write(ludsp,'(7x," ",$)')
        DO I=1,NSTN-1
          J=ISTN(I)
C         WRITE(LUDSP,'(A2,2X,$)') LSTCOD(J)
! AEM undo          WRITE(LUDSP,'(A2,2X,$)') LpoCOD(J)
! AEM 20050119 'Nx'->Nspaces
          WRITE(LUDSP,'(A2,1x," ",$)') cpoCOD(J)
        ENDDO
      enddo
      WRITE(LUDSP,'()')
C
C  3. Matrix lines, one for each station.
C
      DO I=2,NSTN ! each station down
        IS=ISTN(I)
! AEM undo        write(cnnx,9200) 5+(nstn-i)*4
!9200    format('(',i3,'X,$)')
! AEM 20050204 replace with simple
        cnnx = '(   x," ",$)'
	write(cnnx(2:4),'(i3)') 4+(nstn-i)*4
        do k=1,nba ! each band
          iba=iband(k)
          if (iba.gt.1) write(ludsp,cnnx)
C         WRITE(LUDSP,9103)LSTCOD(IS)
! AEM undo          WRITE(LUDSP,9103)LpoCOD(IS)
!9103      FORMAT(1X,A2,$)
! AEM 20050204 avoid format expression
          WRITE(LUDSP,'(1x,a2,$)') cpoCOD(IS)
          DO J=1,I-1 ! each station across
            JS = ISTN(J)
            ib = ibnum(is,js)
! AEM undo           WRITE(LUDSP,9104) IACTBL(iba,IB)
!9104        FORMAT(1X,I3,$)
          WRITE(LUDSP,'(1x,i3,$)') IACTBL(iba,IB)
          ENDDO ! each station across
        enddo ! each band
        WRITE(LUDSP,'()')
      ENDDO ! each station down
C
      return
      END
