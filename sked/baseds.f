      SUBROUTINE BASEDS(NSTN,ISTN)
C
C   BASEDS displays projected baselines in matrix format.
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
C     NSTN - number of stations
      integer ISTN(MAX_STN),nstn
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
      integer i,j,is,js,ib
      integer ibnum ! function
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 910926 Created, copied from FLUXDS
C     nrv 950404 Write 2-letter codes
C
C
C  1. Top line with title and band names
C
      WRITE(LUDSP,'(" Projected baseline lengths (km): ")')
C
C  2. Second line with station names across.
C
! AEM undo        write(ludsp,'(6X,$)')
! AEM 20050120 'Nx'->Nspaces
        write(ludsp,'("      ",$)')
        DO I=1,NSTN-1
          J=ISTN(I)
C         WRITE(LUDSP,'(2x,A2,2X,$)') LSTCOD(J)
          WRITE(LUDSP,'(2x,A2,"  ",$)') cpoCOD(J)
        ENDDO
      WRITE(LUDSP,'()')
C
C  3. Matrix lines, one for each station.
C
      DO I=2,NSTN ! each station down
        IS=ISTN(I)
C       WRITE(LUDSP,9103)LSTCOD(IS)
! AEM undo        WRITE(LUDSP,9103)LpoCOD(IS)
! AEM undo 9103    FORMAT(2X,A2,$)
! AEM 20050120 avoid 'format' statement
        WRITE(LUDSP,'(2x,a2,$)') cpoCOD(IS)
        DO J=1,I-1 ! each station across
          JS = ISTN(J)
          ib = ibnum(is,js)
! AEM undo           WRITE(LUDSP,9104) projbase(IB)
! AEM undo 9104      FORMAT(1X,f6.0,$)
! AEM 20050120 avoid 'format' statement
          WRITE(LUDSP,'(1x,f6.0,$)') projbase(IB)
        ENDDO ! each station across
        WRITE(LUDSP,'()')
      ENDDO ! each station down
C
      return
      END

