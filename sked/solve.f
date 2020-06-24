C
      SUBROUTINE SOLVE(lfilnam)
C
C     SOLVE generates an output file for SOLVE
C
      implicit none 
      include '../skdrincl/skparm.ftni'
C
C  COMMON BLOCKS
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'major.ftni'
C
C     CALLING SUBROUTINES: SKED
C
C Input:
      character*(*) lfilnam   !filename to use if specified. 

C LOCAL VARIABLES
      character*128 lfilout   !output filename 
      integer ierr,i,j,ldum,irh1,irm1,l1,idd1,idm1
      integer idum
      integer ich
      integer iy1,im1,id1,ih1,mi1,is1,iy2,im2,id2,ih2,mi2,is2
      integer ic1,ic2,ida
      real*4 ds1,rs1,dum
      integer nch,nc
      logical*4 kex
      character cans
      integer itmp
      integer ind 
C
C      WHO  WHEN    WHAT
C      NRV  921005  First edition
C      nrv  930412  Add number of obs, start/end times
C      nrv  930429  Add user-specified file name
!   2008Mar22 JMGipson Changed iobswt->ksnrwts
!   2009Jul08 JMGipson
!   2020Jun10. JMG. Added implict none. Base filename on skedfile name, not cexper. 
C

C  1. Make sure there is enough information.

      if (nsourc.eq.0.or.nstatn.eq.0.or.nobs.eq.0) then
        write(luscn,9102)
9102    format('Select sources and stations first, and'
     .  ' generate the observations.')
        goto 900
      endif

      if (.not.ksnrwts) then
        write(ludsp,9801)
9801    format('Solve: Equal weights were selected. Can''t ',
     .  'compute sigmas for solve unless SNR weights are selected.')
        return
      endif

! 1.5 Get file name or generate a default file name.

      if(lfilnam .ne. ' ') then
        lfilout=lfilnam
      else
!  use name of schedule file, replacing '.skd' or '.vex' with '.solve'
        lfilout=cskfil
        ind=index(cskfil,".skd")
        if(ind .eq. 0) then
          ind=index(cskfil,".vex")
          if(ind .eq. 0) then
             if(ind .eq. 0) ind=len_trim(cskfil)+1
          endif
        endif
        lfilout(ind:)=".solve"  
      endif 
           
C  2. Create the output file. 

      call purge_file(lfilout,luscn,luusr,.false.,ierr)
    
      open(lutmp,file=lfilout,status='unknown',iostat=ierr)
      CLOSE(lutmp,status='delete')
      OPEN (lutmp,file=lfilout,status='NEW',iostat=IERR)
      if (ierr.ne.0) then
        write(luscn,9201) ierr,lfilout(1:nch)
9201    format('SOLVE01: Error ',i5', trying to create SOLVE output',
     .  ' file ',a)
        goto 900
      else
        write(luscn,9291) lfilout(1:nch)
9291    format('SOLVE02: Opened output file ',a)
      endif

C  2.5 Write experiment name on first line

      write(lutmp,9202) cexper
9202  format('$EXPER ',a)

C  3. Write out the source list.

      write(lutmp,9301) nsourc
9301  format('$SOURCES',i6)
      do i=1,nsourc
        CALL RADED(SORP50(1,I),SORP50(2,I),0.D0,IRH1,IRM1,RS1,
     .  L1,IDD1,IDM1,DS1,LDUM,IDUM,IDUM,DUM)
        WRITE(LUTMP,9302) cSORNA(I), IRH1,IRM1,RS1,L1,IDD1,IDM1,DS1
9302    FORMAT(A8,3X,I2,1X,I2,1X,F4.1,2X, A1,I2.2,1X,I2,1X,F3.0)
      ENDDO

C  4. Write out the station list

      write(lutmp,'("$SITES",i6)') nstatn
      do i=1,nstatn
        write(lutmp,'(a8,2x,3f15.4)') cstnna(i),(stnxyz(j,i),j=1,3)
      enddo

C  5. Write out each observation and its partials
C     Get the first and last observation and write their start
C     times on the first line.

! Get the first and last observations.
C     First observation
      do itmp=1,2
       if(itmp .eq. 1) then
         cbuf=cskobs(iskrec(1))
       else
         cbuf=cskobs(iskrec(nobs))
       endif

        ich=1
        do i=1,5
          CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2) ! skip to start time
        enddo

        if(itmp .eq. 1) then
          read(cbuf(ic1:ic1+10),'(i2,i3,i2,i2,i2)') iy1,ida,ih1,mi1,is1
          call ymday(iy1,ida,im1,id1)
        else
          read(cbuf(ic1:ic1+10),'(i2,i3,i2,i2,i2)') iy2,ida,ih2,mi2,is2
          call ymday(iy2,ida,im2,id2)
        endif
      end do

      write(lutmp,9501) nobs,iy1,im1,id1,ih1,mi1,is1,
     .                       iy2,im2,id2,ih2,mi2,is2
9501  format('$OBS',i6,1x,6(i2,1x),6(i2,1x))
      do i=1,nobs
        cbuf=cskobs(iskrec(i))
        call simul(0,iskrec(i),1,.false.,.true.)
      enddo
      close(lutmp)
 
900   RETURN
      END
