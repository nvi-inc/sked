      SUBROUTINE SOLIS(kvlba_out)
C
C     SOLIS lists the selected sources, celestial and satellite
C
      include '../skdrincl/skparm.ftni'
C
C  COMMON BLOCKS
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'major.ftni'
! passed variables
      logical kvlba_out
C
C  CALLING SUBROUTINES: SOCMD,SKOPN
C  CALLED SUBROUTINES: RADED
C
! functions
      character*1 sochr
      integer trimlen
C  LOCAL VARIABLES
      integer i,j
      integer irh1,irm1,l1,idd1,idm1,irh2,irm2,l2,idd2,idm2
      integer irh3,irm3,l3,idd3,idm3
      integer imaxl,is
      integer nsat,ldum,idum
      double precision raout,decout
      real rs1,rs2,rs3,ds1,ds2,ds3,arcd,dum
      real sunarc !function
      character*16 lra      !used to hold RA
      character*18 ldec     !used to hold Dec strings.
      character*1 lsq, ldq  !used to hold single and double quotes.
      character*2 csign
      equivalence(l1,csign)
      integer ierr
      logical kdisplay/.true./
C
C   WHO  WHEN    WHAT
C      NRV  800816  ???????????????????
C      WEH  830523  ADD SATELLITES
C      NRV  880314  DE-COMPC'D
C      NRV  890501  Added check for sun distance
C 970307 nrv Re-format output depending on source name length.
C
C
C     1. Simply list the sources selected by the user, getting the
C        names from COMMON.  First check that there are some to list.
!     2006Apr24 JMGipson.  Added mode to print in VLBA mode.
!     2006Sep05 JMGipson. VLBA sources with negative declinations were
!               not being correctly written out because I did not print out the sign.
!               Also, was putting 1950, not 2000  positions.  Fixed both.
!     2006Sep08 Further changes to VLBA parser happy.
! 2013Sep13 Made minium sundistance a real parameter. 

      lsq="'"   !single quote
      ldq='"'   !double quote

      IF (NSOURC .EQ. 0) THEN
        WRITE(LUSCN, 9110)
 9110   FORMAT('SOLIS01 - No sources selected.')
        RETURN
      ENDIF
C
      IF(NCELES.GT.0) THEN
        if(kvlba_out) then
           writE(ludsp,'(a)') "Sources in J2000 for VLBA"
        else
           write(ludsp,'(a)')
     >      "   #    SOURCE     RA(hms) 2000 DEC(dms)     RA(hms) "//
     >      "DATE DEC(dms)    RA(hms) 1950 DEC(dms)"
        endif
C       Find the longest source name.
        imaxl=-1
        do is=1,nceles
          imaxl=max(trimlen(csorna(is)),imaxl)
        enddo

        DO I=1,NCELES
          CALL RADED(sorp2000(1,I),sorp2000(2,I),0.D0,IRH1,IRM1,RS1,
     .    L1,IDD1,IDM1,DS1,LDUM,IDUM,IDUM,DUM)
          CALL RADED(sorp_now(1,I),sorp_now(2,I),0.D0,IRH2,IRM2,RS2,
     .    L2,IDD2,IDM2,DS2,LDUM,IDUM,IDUM,DUM)
          call prefr(sorp2000(1,i),sorp2000(2,i),2000,raout,decout)
          CALL RADED(raout,decout,0.D0,IRH3,IRM3,RS3,
     .    L3,IDD3,IDM3,DS3,LDUM,IDUM,IDUM,DUM)
!          j=istcur(1)
!          if(J.lt. 0)
          call ChkSunDist(i,csorna,mjdcur(1),utcur(1),
     >             kdisplay,ludsp,rSunMinAngle,ierr)
      
          if(kvlba_out) then
            write(lra,'(i2.2,"h",i2.2,"m",f9.6,"s")') irh1,irm1,rs1
            if(lra(7:7)  .eq. " ") lra(7:7)="0"
            if(lra(8:8)  .eq. " ") lra(8:8)="0"
            write(ldec,'(a1,i2.2,"d",i2.2,a,f10.7,a)')
     >                               l1, idd1,idm1,lsq,ds1,ldq
            if(ldec(1:1)  .eq. "+") ldec(1:1)=" "
            if(ldec(8:8)  .eq. " ") ldec(8:8)="0"
            if(ldec(9:9)  .eq. " ") ldec(9:9)="0"
            write(ludsp,'("name = ",a12," ra = ",a," dec = ",a)')
     >       lsq//csorna(i)(1:trimlen(csorna(i)))//lsq, lra,ldec
          else
            WRITE(LUDSP,9130) I,sochr(i),csorna(i)(1:imaxl),
     .        IRH1,IRM1,RS1,L1,IDD1,IDM1,DS1,
     .        IRH2,IRM2,RS2,L2,IDD2,IDM2,DS2,
     .        IRH3,IRM3,RS3,L3,IDD3,IDM3,DS3
9130       FORMAT(I4,1x,a1,2X,A,3(3X,2(I2,1X),F4.1,2X,A1,2(I2,1X),F3.0))
          endif
        ENDDO
      ENDIF
C
      IF(NSATEL.GT.0) THEN
        WRITE(LUDSP,9141)
9141    FORMAT(/'  #  SATELLITE INC    ECC    PERIG  NODE   ANOM   ',
     .       'AXIS -or-  MOTION  YEAR DAY'/
     .        '               (deg)         (deg)  (deg)  (deg)  ',
     .       '(km)       (rv/dy)         ')
        DO NSAT=1,NSATEL
          I=NCELES+NSAT
          WRITE(LUDSP,9150) I,cSORNA(i)(1:8),
     .                  (satpos(J,NSAT),J=1,7),ISATY(NSAT),SATDY(NSAT)
9150      FORMAT(I4,1X,A8,1X,F7.2,F7.5,3F7.2,F11.1,F8.3,I5,F7.2)
        ENDDO
      ENDIF
C
      RETURN
      END
