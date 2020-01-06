      SUBROUTINE LSCUR(KHEAD,ISTN,NSTN,ELLIM)  !LIST CURRENT OBS
C
C   LSCUR lists CUR variables (current observation)
C
C  COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'major.ftni'

C
C  INPUT VARIABLES:
      LOGICAL KHEAD
C             - TRUE if we should put out a header
      integer ISTN(max_stn),nstn
C      - station indices for listing
C     NSTN - number of stations in ISTN
      real*4 ELLIM ! elevation limit, list obs above only

! functions
      real azwrap         !return az including wrap
C
C
C     CALLING SUBROUTINES: LICMD,NEWOB
C     CALLED SUBROUTINES: SNRAC
C
C    LOCAL VARIABLES
      integer ibnum !functions
      LOGICAL KSK   !print something
      logical kstat(max_stn)   !this station is in this obs
C      - true if station is schedule for this observation
      integer ihr,imin,isc,j,k,i,ic,ierr,iba,ib,is,js,ibl
      integer iaz,iel
      real*4 ha,elx
      real*4 az,el,har,dec,x30,y30,x85,y85
      LOGICAL KUP ! true if source is up at station
      logical kis,kjs
      integer isphere_pix
C     - true if requested station is in current obs
      integer iband(max_band) ,nba
      integer iyr_temp
      character*1 lchar
      character*2 cwrap
      integer*2   iwrap
      equivalence (iwrap,cwrap)
      character*6 lsnr
      character*6 lflux
 
C
C     LAST MODIFIED: 810817
C     DATE   WHO    CHANGES
C    811125  MAH    OBS LISTED BY BASELINE IF KBSELN=.TRUE.
C                   ALL STNS IN OBS LISTED IF KXLIST=.FALSE.
C    820323  MAH    PDFEET TO END OF XLIST DISPLAY
C    830423  NRV    ADD X,Y TO CVPOS CALL
C    841017  MWH    ADD PARAMS TO LSHED CALLS
C    880310  NRV    DE-COMPC'D
C    890502  NRV    Added option to list durations
C    890503  NRV    Changed to using formatter for output
C    890515  NRV  Added ELLIM parameter for min el. listings
C    890711  NRV  Added XLIST option for SNR
C                 Changed LSHED call to remove parameters
C    891010  NRV  Added KMAXL option for all information in listing
C    891127  NRV  Changed for multiple bands and SNRs
C    910924  NRV  Add mjd,ut to SNR call
C    911112  NRV  Add observed flux option
C 970224 nrv New option kxazel2 to print az,el,ha and use kxazel for
C            az,el ONLY and not HA
C 981113 nrv Output only the last 2 digits of the stored 4-digit year.
! 2004JAN13  JMGipson. Modified to get rid of call to ib2as
! 2005Sep26  JMGipson. Added "kxwrap" option to display total AZ
! 2007Jan24 JMGipson.  Changed all xlist logical flags to be kx---, ie, kwrap-->kxwrap.
!           Added kxfreq option to display frequency band.
! 2007Nov02 JMGipson. Added skycoverage. A little cleannup.
! 2007Dec01 JMGipson. klong
! 2008Jun18 JMG. Moved klong -->kxlong, set by xlcmd
! 2010Jan05 JMG. Formatting changes to get lines to line up.\
! 2010Jan25 JMG. Changed index order of azimu, eleva 
! 2010Mar20 JMG. Removed obsolete dependence on KBSELN, KPART. 
C
C
C     1. First set up the correct carriage control character.
C     If no stations were specified, set up for all.
C     Then print headers as requested.
C
      IF  (NSTN.EQ.0) THEN  !default is SU default stations
        NSTN=NSUBST
        DO  I=1,NSTN
          ISTN(I) = ISUBST(I)
        END DO
      END IF  !default is SU default stations

C
      IF (.NOT.KHEAD) GOTO 200
      CALL LSHED(LUDSP,NSTN,ISTN)
      KHEAD = .FALSE.
C
C     2. Fill up a buffer with the output.
C
200   CONTINUE
C  If any of the requested subnet stations are in the
C  current observation, then list it.
      kstat=.false.
      IC=0
      DO  I=1,NSTN
        J=ISTN(I)
        DO  K=1,nstncur
          IF (J.EQ.ISTCUR(K)) then
            IC=IC+1
            kstat(j)=.true.
          endif
        END DO
      END DO  !

      if(ic .lt. 1) return     
  
C
      IF  (ISORCM.GT.0.AND.NSORcur(ISTCUR(1)).NE.ISORCM) RETURN
C
C  If any elevation of the subnet stations of the current obs
C  is below the limit, list it
      KSK = .FALSE.
      I=1
      DO WHILE (I.LE.nstncur.AND..NOT.KSK) !loop on current obs stns
        J=ISTCUR(I)
        DO K=1,NSTN !loop on requested stations
          IF (J.EQ.ISTN(K)) THEN !this stn requested
            CALL CVPOS(NSORcur(J),J,MJDCUR(J),UTCUR(J),
     .                 AZ,EL,HAR,DEC,X30,Y30,X85,Y85,KUP)
            ELX=EL*rad2deg
            IF (ELX.LT.ELLIM) KSK=.TRUE.
          ENDIF !this stn requested
        ENDDO! loop on requested stations
        I=I+1
      ENDDO !loop on current obs stations
      IF (.NOT.KSK) RETURN
      
C
      J = ISTCUR(1)
      call gtban(icodcur(j),nba,iband)
      call seconds2hms(utcur(j),ihr,imin,isc)

      iyr_temp=mod(iyrcur(j),100)
      if (kxmaxl) then !maximum listing of all info
        WRITE(LUDSP,9001,ERR=999) cSORNA(NSORcur(J)), ICALcur(J),
     .  LCODE(ICODcur(J)),cPREcur(J),iyr_temp,IDACUR(J),
     .  IHR,iMIN,ISC,IDURcur(J),cMIDcur(J),IDLCUR(J),cPSTcur(J)
9001    FORMAT(A8,I5,1X,A2,1X,A6,1X,i2.2,I3.3,'-',3I2.2,I5,1X,
     >     A6,I4,1X,A6,$)
      else !Alternate shortened listing output without duration or procedures
        write(ludsp,9011) csorna(nsorcur(j)),
     >   iyr_temp,idacur(j),ihr,imin,isc
9011    format(a8,1x,i2.2,i3.3,'-',3i2.2,$)
      endif

      if(kxfreq) write(ludsp,'(" ",a2," ",$)') ccode(icodcur(j))

      if (.not.(kxazel.or.kxwrap.or.kxazel2))
     >    WRITE(LUDSP,'("|",$)',ERR=999)
C
      IF  (KXLIST) THEN  !xlist
        IF  (kxazel.or.kxwrap.or.kxazel2) THEN  !positions
          DO  I=1,NSTN !list requested stations
            J = ISTN(I)
            WRITE(LUDSP,'("|",$)',ERR=999)
            IF(KStat(j)) THEN  !calculate
               CALL CVPOS(NSORcur(J),J,MJDCUR(J),UTCUR(J),
     .                 AZ,EL,HAR,DEC,X30,Y30,X85,Y85,KUP)
                
!               write(*,'(4f10.1)') az*rad2deg, azimu(ircur,j)*rad2deg,
!     >                            el*rad2deg, eleva(ircur,j)*rad2deg
                if(kxwrap) then
                  az=azwrap(az,cwrap_cur(j),stnlim(1,1,j))                 
                endif
                IAZ=nint(AZ*rad2deg)
                IEL=nint(EL*rad2deg)
                HA=HAR*rad2ha
                if (kxazel2) then
                  IF (HA.LT.-9.99) THEN
                    write(LUDSP,'(F4.0,I4,I3,$)',ERR=999) HA,IAZ,IEL
                  ELSE
                    write(LUDSP,'(F4.1,I4,I3,$)',ERR=999) HA,IAZ,IEL
                  ENDIF
                else
                  if(kxLong) then
                    write(ludsp,'(f7.2,1x,f5.2,$)')
     >                         az*rad2deg,el*rad2deg
                  else
                    write(LUDSP,'(i4,1x,i2,$)',ERR=999) IAZ,IEL
                  endif
                endif
              ELSE  !blanks
                if (kxazel.or.kxwrap) then
                  if(kxLong) then
                    write(ludsp,'(12x," ",$)',ERR=999)
                  else
                    write(LUDSP,'(6x," ",$)',ERR=999)
                  endif
                endif
                if (kxazel2) write(LUDSP,'(10x," ",$)',ERR=999)
              END IF  !blanks
            END DO  !list requested stations
          WRITE(LUDSP,'("|",$)',ERR=999)
        END IF  !positions
      if(kxsky) then
        DO  I = 1,NSTN !list footage counters
          J = ISTN(I)
          IF (KStat(J)) THEN  !this one
!             writE(*,*) eleva(ircur,j), azimu(ircur,j)
             call sphere_pix(azimu(ircur,j),eleva(ircur,j),
     >            num_pix_bands, ipix_bands,dang_pix_band,
     >            isphere_pix,ierr)
             write(ludsp,'(2(1x,i2)," |",$)',err=999) isphere_pix,
     >           inum_pix_obs(ircur,j)
          else
             write(ludsp,'(6x," |",$)',err=999)
          endif
        end do
      endif


      IF  (kxfeet) THEN  !counters
        DO  I = 1,NSTN !list footage counters
          J = ISTN(I)
          IF (KStat(J)) THEN  !this one
             write(LUDSP,'(A1,A1,I6.6," ",$)',ERR=999)
     >        cpass(ipascur(j):ipascur(j)),cdir((idircur(j)+3/2)),
     >       IFTCUR(J)
C            endif ! new tape/not
          ELSE  !not this one
            write(LUDSP,'(8x," ",$)',ERR=999)
          END IF  !not this one
        END DO  !list footage counters
          WRITE(LUDSP,'("|",$)',ERR=999)
      END IF  !counters
      IF  (kxdur) THEN  !durations
        DO  I = 1,NSTN !list durations
          J = ISTN(I)
          IF  (KStat(j)) THEN  !this one
            write(LUDSP,'(i4,$)',ERR=999) IDURcur(J)
          ELSE  !not this one
            write(LUDSP,'(3x," ",$)',ERR=999)
          END IF  !not this one
        END DO  !list durations
        WRITE(LUDSP,'("|",$)',ERR=999)
      END IF  !durations
      
      IF  (kxsnr.or.kxobsf) THEN  !SNRs or flux
        do i=1,nstncur
          idurst(istcur(i))=idurcur(istcur(i))      
        enddo    
        j=istcur(1)
!        write(*,*) "BEFORE"
        call snrac(nstncur,istcur,nsorcur(j),icodcur(j),-1,mjdcur(j),
     >            utcur(j),ierr)
!        writE(*,*) "AFER"
        call gtban(icodcur(j),nba,iband)
        do ib=1,nba !bands
          iba=iband(ib)
          do i=1,nstn-1
            is=istn(i)
            kis=.false.
            ic=1
            do while (.not.kis.and.ic.le.nstncur)
              if (istcur(ic).eq.is) kis=.true.
              ic=ic+1
            enddo
            do j=i+1,nstn
              js=istn(j)
              kjs=.false.
              ic=1
              do while (.not.kjs.and.ic.le.nstncur)
                if (istcur(ic).eq.js) kjs=.true.
                ic=ic+1
              enddo
              ibl=ibnum(is,js)
! initialize holders for the information.
              lsnr=" "
              lflux=" "
           
              if (kis.and.kjs) then
                if (kxsnr) write(Lsnr,'(I5," ")') iactbl(iba,ibl)
              
                if (kxobsf) then
                  if (factbl(iba,ibl).lt.10.0d0) then
                    write(lflux,'(" ",f4.2," ")') factbl(iba,ibl)
                  else if(factbl(iba,ibl) .lt. 100.0d0) then
                    write(lflux,'(" ",f4.1," ")') factbl(iba,ibl)
                  else
                    write(lflux,'(" ",i4, " ")') int(factbl(iba,ibl))
                  endif
                endif
              endif
              if (kxsnr)  write(ludsp,'(a,$)') lsnr
              if (kxobsf) write(ludsp,'(a,$)') lflux
            enddo
          enddo
          WRITE(LUDSP,'("|",$)',ERR=999)
        enddo !bands
!       WRITE(LUDSP,'("|",$)',ERR=999)
      END IF  !SNRs or flux
      ELSE  !ids only
        WRITE(LUDSP,'(" ",$)',ERR=999)
        DO  K=1,nstncur ! all current stations
          J=ISTCUR(K)
          write(LUDSP,'(A2,$)',ERR=999) cpoCOD(J)
          lchar=cwrap_cur(j)(1:1)
          if(lchar .ne. "C" .and. lchar .ne. "W") lchar="-"
          write(ludsp,'(A1,$)') lchar
        END DO  !all current stations
      END IF  !ids only
      write(ludsp,*) " "
C
990   RETURN
999   WRITE(LUDSP,'(/"FORMAT ERROR")',ERR=990)
      RETURN
      END
