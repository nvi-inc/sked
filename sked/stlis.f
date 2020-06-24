C
      SUBROUTINE STLIS
C
C  STLIS lists the selected stations
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C     INPUT VARIABLES: NONE
C     OUTPUT VARIABLES: NONE
C
C  COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni' 
      include '../skdrincl/freqs.ftni'
! functions
      integer ibnum

C
C     CALLING SUBROUTINES: STCMD
C     CALLED SUBROUTINES: none
C
       character*4 laxis
       integer i,j,k,ib
       integer nbase,ipos
       real*8 bsum ! sum of baseline lengths
       real*8 bxysum,bxzsum,byzsum ! sum of components
       integer ikey(max_stn*(max_stn-1)/2)
       integer ibl_key(2,max_stn*(max_stn-1)/2)
       integer iptr
       logical ksome_mark6
C
C HISTORY
C   880314 NRV DE-COMPC'D
C   881228 GAG ADDED NRV's HORIZON AND COORDINATE MASK DISPLAY'S
C   900118 NRV Added listing SEFDs, etc. after baselines
C   900124 NRV Added call to AXTYP to get axis type code name
C   900301 NRV Allow ITERID to be hollerith or integer
C   920520 NRV Added listing of SEFD parameters
C   930225 nrv implicit none
C   940629 nrv Add sorted order and average baseline length
C 960220 nrv Write out bit density for first code only
C 960227 nrv Write out horizon mask to the end, including last el
C 960227 nrv Change iterid to lterid
C 960409 nrv Print number of headstacks
C 980625 nrv Print "auto" for automatic tape allocation, instead of length
C 990412 nrv Remove MAXSCN since it's a single parameter now.
C 990607 nrv Remove density and add rack and recorder types.
C 991119 nrv Remove SEFD parameter listings.
C 2003Dec10 JMGipson get rid of holleriths
! 2005Apr18  Got rid of lmaxtap which was written to but never used.
!            Got rid of associated functions.
!            Write out station number.
! 2006Nov30  Use cstrec(istn,irec)
! 2008Jun05  Rewritten and simplifed.
! 2016Dec12  JMG. Minor formatting changes. 
! 2020Jun08 JMG. New broadband.ftni 
C
C
C     1. Simply list the stations selected by the user, getting the
C        names from COMMON.  First check that there are some to list.
C
      IF (NSTATN .EQ. 0) THEN
        write(luscn,9110)
 9110   FORMAT('STLIS00 - No stations selected.')
        RETURN
      endif
C
      WRITE(LUDSP,'(a)') 
     >  '     STATION     AXIS  SLEW RATES   SLEW CONST  LIMIT STOPS'
      ksome_mark6=.false. 
      DO I=1,NSTATN
        if(cstrec(i,1) .eq. "Mark6") ksome_mark6=.true. 
        call axtyp(laxis,iaxis(i),2)
        WRITE(LUDSP,9130) I,cstcod(i),cpocod(I),cSTNNA(I),LAXIS,
     >   STNRAT(1,I)*rad2deg*60,STNRAT(2,I)*rad2deg*60,
     >   istcon(1,i),istcon(2,i),
     >   STNLIM(1,1,I)*rad2deg,STNLIM(2,1,I)*rad2deg,
     >   STNLIM(1,2,I)*rad2deg,STNLIM(2,2,I)*rad2deg

9130   FORMAT(I3,1X,4(a,1x),2(F5.1,1x),2I5,4F9.1)
        WRITE(LUDSP,9140) STNPOS(1,I)*rad2deg, STNPOS(2,I)*rad2deg,
     >     coccup(i)
9140    FORMAT('     Position ',F10.2,' WEST    ',F10.2,' NORTH',
     .  '   Occupation code: ',a)
        IF (NHORZ(I).GT.0) THEN !write horizon mask
          WRITE(LUDSP, "('     Horizon ',$)") 
          DO J=1,NHORZ(I)
            WRITE(LUDSP,9151) AZHORZ(J,I)*rad2deg,ELHORZ(J,I)*rad2deg

9151        FORMAT(F5.0,f5.1,$)
            IF (MOD(J,7).EQ.0) THEN
              WRITE(LUDSP,'()')
              WRITE(LUDSP,'("             ",$)')
            END IF
          ENDDO
          write(ludsp,'()')
        ENDIF
        IF (NCORD(I).GT.0) THEN !write coordinate mask
          WRITE(LUDSP,9154)
9154      FORMAT('             COORD. MASK: ',$)
          DO J=1,NCORD(I)-1
            WRITE(LUDSP,9151) CO1MASK(J,I)*rad2deg,CO2MASK(J,I)*rad2deg
            IF (MOD(J,5).EQ.0) THEN
              WRITE(LUDSP,'("    ",a2,$)') 
              WRITE(LUDSP,'("                   ",$)')
            END IF
          ENDDO
          WRITE(LUDSP,'(f5.1)')  CO1MASK(NCORD(I),I)*rad2deg
        ENDIF
      enddo
C
C  List baselines. Sort as you go for the next output.
C
      WRITE(LUDSP,'(/" Baseline lengths (km): ")')
      write(ludsp,'("          ",$)') 
      DO I=1,nstatn-1
        WRITE(LUDSP,'(A2,"     ",$)') cpocod(I)
      ENDDO
      WRITE(LUDSP,'()')
      ipos=0
      bsum = 0.d0
      bxysum = 0.d0
      bxzsum = 0.d0
      byzsum = 0.d0
      DO I=2,nstatn
        WRITE(LUDSP,'("    ",a," ",$)')cpocod(I)
        DO J=1,I-1
          ib = ibnum(i,j)
          ibl_key(1,ib)=i
          ibl_key(2,ib)=j
          WRITE(LUDSP,9161) baselen(ib)
9161      FORMAT(1X,F6.0,$)
          bxysum = bxysum + dsqrt((stnxyz(1,i)-stnxyz(1,j))**2 +
     .    (stnxyz(2,i)-stnxyz(2,j))**2) / 1000.d0
          bxzsum = bxzsum + dsqrt((stnxyz(1,i)-stnxyz(1,j))**2 +
     .    (stnxyz(3,i)-stnxyz(3,j))**2) / 1000.d0
          byzsum = byzsum + dsqrt((stnxyz(2,i)-stnxyz(2,j))**2 +
     .    (stnxyz(3,i)-stnxyz(3,j))**2) / 1000.d0
          bsum = bsum + baselen(ib)
1000      continue
        ENDDO
        WRITE(LUDSP,'()')
      ENDDO
      nbase = (nstatn*(nstatn-1))/2
      write(ludsp,'()')
      write(ludsp,9162) "total baeline ", bsum/nbase
      write(ludsp,9162) "X-Y component ", bxysum/nbase
      write(ludsp,9162) "X-Z component ", bxzsum/nbase
      write(ludsp,9162) "Y-Z component ", byzsum/nbase
9162  format(' Average ',a, 'length = ',f6.0,' km')

C  Write out baseline lengths in order
      call indexx8(nbase,baselen,ikey)

      write(ludsp,"('Baselines sorted by length:')")
      do k=1,nbase
        iptr=ikey(k)
        i=ibl_key(1,iptr)
        j=ibl_key(2,iptr)
        ib = ibnum(i,j)
        write(ludsp,9163) cstnna(i),cstnna(j), baselen(ib)
9163    format(1x,a,' - ',a,f10.0)
      enddo
C
C  Write SEFDs, terminal IDs, number of passes
C  

! Only write out data_mbs and sink_mbs if we have a Mark6 recorder
      write(ludsp,'(/,a,$)') 
     > ' #   ID  STATION   Band SEFD   Band SEFD  '//
     > ' DAT_name ID   Rack                 Recorder    '
      if(ksome_mark6) then
        write(ludsp,'(a)') 'Data_mbs Sink_mbs'
      else
        write(ludsp,'(a)') " " 
      endif 

      DO  I=1,NSTATN
        WRITE(LUDSP,
     >  '(i2,1X,a1,1x,A2,2X,A,3X,2(a1,2x,i5,3x),1x,4(a,1x),$)')
     >  i,cstcod(i),cpocod(I),cSTNNA(I),
     >  lband(1), int(SEFDST(1,I)),lband(2), int(sefdst(2,i)),
     >  cterna(i),cterid(i),cstrack(i),cstrec(i,1)
       if(cstrec(i,1) .eq. "Mark6") then
         write(ludsp,'(2(i8,1x))')  idata_mbps(i),isink_mbps(i)
       else
         write(ludsp,'(a)') " "
       endif 
       

      END DO
C
      RETURN
      END
