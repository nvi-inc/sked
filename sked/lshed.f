      SUBROUTINE LSHED(LU,NSTN,ISTN)
C
C     LSHED prints out the header for observations
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
C
C  INPUT:
      integer lu,nstn,nprint
C     LU - unit for writing
C     NSTN - number of stations in this list
      integer ISTN(max_stn)
C
C  Local
      integer i,j,iba,is,js
      integer CLEN,TRIMLEN
      character*12 cnnx ! format nnX
      integer temp_i
C
C   INITIALIZED VARIABLES
C
C  History
C  DATE  WHO  CHANGE
C 841017 MWH  ADD HEADER FOR PRINTER OUTPUT
C 880310 NRV  DE-COMPC'D
C 880614 PMR  removed carriage control commands
C 890502 NRV Added listing of durations
C 890503 NRV Write headers using formatter
C 890711 NRV Changed call to remove parameters
C 891010 NRV Add option to print repeated fields (cal,dur,procs)
C 891208 NRV Add headers for multiple bands with SNRs
C 911112 NRV Added observed flux
C 970224 nrv New option to print az,el and NOT ha
C
! 2005Sep26 JMGipson. Modified to add kxwrap (AZ--with wrap taken into account)
! 2007Jan17 JMGipson. Stations were not being printed correctly if kxwrap was on.
! 2007Jan24 JMGipson.  Changed all xlist logical flags to be kx---, ie, kwrap-->kxwrap.
!           Added kxfreq option to display frequency band.
! 2007Nov02 JMG. Added skycoverage
! 2010Jan05 JMG. Formatting changes to get lines to line up.
! 2017Feb14 JMG. For combatibility with gfortran, got rid of some tabs. 



      IF (kxazel.or. kxwrap) then
         if(kxLong) then
          nprint=(ibuf_len*2-60)/12
         else
          NPRINT = (IBUF_LEN*2-60)/6
         endif
      endif
      IF (kxazel2) NPRINT = (IBUF_LEN*2-60)/11
      IF (kxfeet)  NPRINT = NSTN
      IF (kxobsf)  NPRINT = NSTN
      IF (kxdur)   NPRINT = NSTN
      if (kxsky)   nprint = nstn
      if (kxsnr)   nprint = nstn
      if (kxmaxl)  nprint = nstn
      IF ((kxazel .or. kxwrap).AND.kxfeet.AND.kxdur)
     >     NPRINT = (IBUF_LEN*2-60)/17
      IF (kxazel2.AND.kxfeet.AND.kxdur) NPRINT = (IBUF_LEN*2-60)/22



      clen=trimlen(cskfil)
      IF  (LU.NE.LUSCN) THEN  !print header
        write(LU,'("Observation listing from file ",a,
     >  "  for experiment ",a)') CSKFIL(1:CLEN),cEXPER
      END IF  !print header

      if (kxmaxl) then !full information
        write(LU,'("SOURCE   CAL CODE PREOB    START      DUR MIDOB",
     .  " IDLE POSTOB ",$)')
      else
        write(lu,'("Source      Start     ",$)')
      endif
      IF  (KXLIST) THEN  !first line extended headers
        if(kxfreq) then
           write(lu, '("FR  ",$)')    !space for frequency flag.
        endif
        IF  (kxazel2) THEN  ! az,el,ha
          DO  I=1,min0(NSTN,NPRINT)
            write(LU,'(" HA   AZ EL ",$)')
          END DO
        END IF  !
        IF  (kxazel.or.kxwrap) THEN  ! az,el only
          if(kxLong) then
            DO  I=1,min0(NSTN,NPRINT)
              write(LU,'("   AZ    EL   ",$)')
            END DO
          else
            DO  I=1,min0(NSTN,NPRINT)
              write(LU,'(" AZ  EL ",$)')
            END DO
          endif
        END IF  !
        if(kxsky) then
          DO  I=1,min0(NSTN,NPRINT)
            write(LU,'("PIX COV ",$)')
          END DO
        endif

        IF (kxfeet) THEN
          write(LU,'("TAPE FOOTAGE COUNTERS",$)')
! AEM 20050204 replace with simple for cnnx	  
          cnnx = '(     " ",$)'
	  temp_i = max(1,nstn*9-21) - 1
	  if ( temp_i.gt.0 ) write(cnnx(2:6),'(i3,"x,")') temp_i
c          write(cnnx,9200) imax0(1,nstn*7-21)
          write(lu,cnnx)
        ENDIF

        IF (kxdur) THEN
          write(LU,'(" DURATIONS",$)')
! AEM 20050204 replace with simple for cnnx	  
          cnnx = '(     " ",$)'
	  temp_i = max(1,nstn*4-9) - 1
	  if ( temp_i.gt.0 ) write(cnnx(2:6),'(i3,"x,")') temp_i
          write(lu,cnnx)
        ENDIF
        if (kxsnr) write(lu,'("SNR by baseline for ",a2,", ",a2,$)')
     .	(lband(i),i=1,nband)
        if (kxobsf) write(lu,'("Observed flux by baseline for ",
     >    a2,", ",a2,$)') (lband(i),i=1,nband)
      END IF  !first line extended headers
      write(LU,'()')
C
      if (kxmaxl) then !full headers
        write(LU,'("NAME    (SEC) FR PROC   YYDDD-HHMMSS (SEC)PROC ",
     .  "(SEC) PROC   ",$)')
      else
        write(lu,'("name     yyddd-hhmmss  ",$)')
      endif
      if(kxfreq) write(lu,'("    ",$)')  !leave space for freq

      IF  (KXLIST) THEN  !second line extended headers
        IF  (kxazel2) THEN  !azel + ha
          DO  I=1,min0(NSTN,NPRINT)
            J = ISTN(I)
            write(LU,'(A2,"(",A8,")",$)') cpocod(J),cSTNNA(J)
          END DO  !
          write(lu,'(" ",$)')  !leave a space at the end
        END IF  !azel + ha
        IF  (kxazel .or. kxwrap) THEN  !azel
          DO  I=1,min0(NSTN,NPRINT)
            J = ISTN(I)
            if(kxLong) then
               write(LU,'(6x,a2,5x," ",$)') cpocod(J)
            else
               write(LU,'(3x,a2,2x," ",$)') cpocod(J)
            endif
          END DO  !
!          write(lu,'(" ",$)')  !leave a space at the end
        END IF  !azel

        IF  (kxsky) THEN  !sky
          DO  I=1,min0(NSTN,NPRINT)
            J = ISTN(I)
            write(LU,'(3x,a2,2x," ",$)') cpocod(J)
          END DO  !
          write(lu,'(" ",$)')  !leave a space at the end
        END IF  !sky


        IF  (kxfeet)  THEN  !feet
          DO  I = 1,min0(NSTN,NPRINT)
            J = ISTN(I)
            write(LU,'(3x,a2,3x," ",$)') cpocod(J)
          END DO  !
          write(lu,'(" ",$)')  !leave a space at the end of each band
        ENDIF !feet


        IF  (kxdur)  THEN  !durations
          DO  I = 1,min0(NSTN,NPRINT)
            J = ISTN(I)
            write(LU,'(" ",A2," ",$)') cpocod(J)
          END DO  !
          write(lu,'(" ",$)')  !leave a space at the end
        END IF  !durations
! AEM 20051205 fix header due to lscur.f:226	
        if (kxsnr.or.kxobsf) then !SNRs      
          if(kxsnr .and. kxobsf) write(lu,'("   ",$)')   
!          write(lu,'(" ",$)')
          do iba=1,nband
            do i=1,min0(nstn,nprint)-1
             is=istn(i)
              do j=i+1,min0(nstn,nprint)
                js=istn(j)
                write(lu,'(a,"-",a," ",$)') cpocod(is),cpocod(js)
                if(kxsnr .and. kxobsf) then 
               
                else if(kxsnr) then
!                   write(lu,'(a,"-",a," ",$)') cpocod(is),cpocod(js)
                else
!                   write(lu,'(a,"-",a," ",$)') cpocod(is),cpocod(js)
                endif 
! AEM comment: add spaces here
!                                                 123456 
      		if(kxsnr.and.kxobsf) write(lu,'("      ",$)')
              enddo
            enddo
            write(lu,'(" ",$)')  !leave a space at the end of each band
          enddo
        endif !SNRs
      ELSE
        write(LU,'("STATIONS",$)')
      ENDIF !second line extended headers
C
      write(LU,'()')
C
      RETURN
      END
