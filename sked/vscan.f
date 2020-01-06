      SUBROUTINE VSCAN(LINSTQ)
C
C   VSCAN calculates and displays variable scan lengths.
C
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'major.ftni'
! functions
      integer ibnum ! function
      integer trimlen
C
C  INPUT:
      integer*2 LINSTQ(*)
C      - input string, word 1=length
C
C
C  Called by: SKEd
C  Calls: SNRDS, SNROK, FLUXDS, BASEDS
C
C  LOCAL VARIABLES
      integer ISTN(MAX_STN),nstn
C      - station indices for which SNR is to be calculated
C     NSTN - number of IDs in ISTN
      integer ic1,i,ib,is,iba,j,icod,nba,iso,ia,ierr
      integer iband(max_band) ! band indices from GTBAN
      integer iokst(max_stn) ! returned by SNROK
      integer nblanks
      integer mjdnow
      real*8  utnow
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 890420 CREATED
C     NRV 890425 Moved to Unix and pulled out subroutines
C                SNRSK and SNRDS
C     NRV 890430 Check error return from SNRSK
C     NRV 890526 Add source name to display
C     nrv 890913 Add display for all sources at once
C     NRV 891118 Modified for SEFD by band
C     NRV 891207 Removed SNSRK and added SNROK to calc and check SNRs
C     NRV 910924 Add mjd,ut to SNR call
C     NRV 910925 Add call to FLUXDS, BASEDS
C     nrv 950404 Write 2-letter codes
C 970224 nrv Find source name length, add imaxl to fllis1 call
C
! 2004Feb04  JMGipson.  If SNR is too low, print out " ", previously did -1
! 2008Jun05  JMGipson.  Ok, changed to "-"
! 2010Apr07  JMGipson. Call to snrok had wrong arguments for time. Fixed. 
!               Now use time of first station in most recent scan. 
C
C     1. First make sure we have enough information
C
      IF  ((NSOURC.EQ.0).OR.(NSTATN.EQ.0)) THEN  !not enough info yet
        write(luscn,9110)
9110    format(' VSCAN01 - Select sources and stations first.')
        RETURN
      END IF  !not enough info yet
C
      IC1 = 1
C  This subroutine decodes user input fields with gtsoi (sources)
C   and gtsti (stations).
      CALL GTSSI(LINSTQ,IC1,NSTN,ISTN,IERRCM,luscn)
      IF  (IERRCM.NE.0) RETURN
      IF  (NSTN.EQ.0) THEN  !all stations
        NSTN = NSUBST
        DO  I=1,NSTN
          ISTN(I) = ISUBST(I)
        END DO
      END IF  !all stations
      if (nstn.eq.1) then !takes two
        write(luscn,9120)
9120    format('VSCAN02 - It takes two to observe!')
        return
      endif
C
      ierr=0
      icod=1 ! *** only one code for now
C  This subroutine finds the number of bands and their indices
C   given the frequency code.
      call gtban(icod,nba,iband)
      do i=1,nba !check for SEFDs
        ib=iband(i)
        IS=1
        DO WHILE (is.le.nstn.and.SEFDST(ib,ISTN(IS)).GT.0.0)
          IS=IS+1
        ENDDO
        IF (IS.LT.NSTN) THEN !SEFDs not specified
          WRITE(LUSCN,9101) lband(ib),cstnna(istn(is))
9101      FORMAT('VSCAN02 - SEFDs not present for band ',a2,' at ',a8)
          RETURN
        ENDIF !SEFDs not specified
      enddo !check for SEFDs
C
      if (.not.kvscan) then
        write(luscn,9109)
9109    format('VSCAN03 - Scan calculations not set.')
        return
      endif
C
C  2. Calculate SNRs and WRITE OUT THE SCAN TIMES AND SNR MATRIX
C
      if (isorcm.eq.0) then !calculate snr
        WRITE(LUDSP,9201) lcode(icod),cnafrq(icod)
9201    FORMAT(' Variable scan info for ',a2,' (',a8,') ')
9200    format('(',i3,'x,$)')
        do iba=1,nba ! each band
          write(ludsp, '(" Durations & SNRs for ",a1,"-band")')
     >      lband(iba)
          write(ludsp, '("   Source   | Durations",$)')
          nblanks=5*nstn-9
          do i=1,nblanks
            write(ludsp,'(" ",$)')
          end do
          write(ludsp,'("| Baseline SNRS ")')
          write(ludsp,'(a,$)') "            |"
          do i=1,nstn
            write(ludsp,9202) cpocod(istn(i))
9202        format(3x,a2,$)
          enddo
          write(ludsp,'(" | ",$)') 
          do i=1,nstn-1
            do j=i+1,nstn
              write(ludsp,9203) cpocod(istn(i)),cpocod(istn(j))
9203          format(2x,a1,'-',a1,$)
            enddo
          enddo
          write(ludsp,'()')
C
          mjdnow=mjdcur(istcur(1))  !use time of first station in the most recent scan.
          utnow=utcur(istcur(1))
          do iso=1,nsourc !all sources    
            call snrok(istn,nstn,iso,icod,-1,iokst,mjdnow,utnow)    
            write(ludsp,9211) iso,csorna(iso)
9211        format(i2,1x,a8," |",$)
            do i=1,nstn !write durations by station
              ia=idurst(istn(i))
              if (iokst(i).lt.0.or.iokst(j).lt.0) then
                   write(ludsp,'("   - ",$)')
               else
                  write(ludsp,'(i5,$)') ia
              endif
            enddo
            write(ludsp,'(" | ",$)') 
            do i=1,nstn-1
              do j=i+1,nstn !write SNRs by baseline
                ia=iactbl(iba,ibnum(istn(i),istn(j)))
                if (iokst(i).lt.0.or.iokst(j).lt.0) then
                   write(ludsp,'("   - ",$)')
                else
                   write(ludsp,'(i5,$)') ia
                endif
              enddo
            enddo
            write(ludsp,'()')
          enddo !all sources
        enddo ! each station
C
      else if (isorcm.gt.0) then !single source display
        CALL SNRSK(ISSCAN(ISORCM),NSTN,ISTN,isorcm,icod,IERR,
     >       ludsp,-1,0.d0)
        IF (IERR.LT.0) RETURN
        WRITE(LUDSP,9301) csorna(isorcm)
9301    FORMAT(' Variable scan info for ',a8)
        call fllis1(isorcm,trimlen(csorna(isorcm)))
        WRITE(LUDSP,9300) (cpoCOD(ISTN(I)),I=1,NSTN)
9300    FORMAT(' Station:',5X,10(A2,3X))
        WRITE(LUDSP,'(" Duration: ",$)')
        DO I=1,NSTN
          J=ISTN(I)
          WRITE(LUDSP,'(I5,$)') IDURST(J)
        ENDDO
        WRITE(LUDSP,'()')
        CALL SNRDS(NSTN,ISTN,ICOD)
        CALL FLUXDS(NSTN,ISTN,ICOD)
        CALL BASEDS(NSTN,ISTN)
      endif !calculate snr
C
      return
      END
