      SUBROUTINE SKOPN(cname,ierr)
C
C     This routine reads the schedule file.
C     Information in the various sections is handled appropriately.
C     Sources and stations are read into common blocks.
C     The schedule entries are copied into a buffer in memory.
C     The original schedule file is left intact.
C
      use group_mod          ! module containing GROUP definitions and routines
      use twin_mod           ! module containing TWIN_TELESCOPES definitions and routines

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

! function
      double precision hms2seconds
      integer julda             !julian day
      integer iwhere_in_string_list
C
C  INPUT:
      character*(*) cname
C
C  COMMON BLOCKS:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'astro.ftni'
      include 'proc.ftni' 

C
C  CALLING SUBROUTINES: SKED
C  CALLED  SUBROUTINES: FMP PACKAGE, LNFCH package
C       READS,GNPAS,SETBA,SOINP,STINP,PRSET,FRINP,FLINP,
C       SSCAN,SELEV,SNRCM,LICMD,SUNPO,APSTAR
C
C  LOCAL VARIABLES:
C     double precision DRA,DDEC,DC ! for MOVE
      logical ks2,kmv,kk4
      double precision ST0,GST,UT !for GST and SIDTM calculations
      integer mjd
C     The following are section names expected in the schedule file
      INTEGER I,J,ich,nr
C             - indexing variables
      integer iret,ierr,nsor,nsat,i1,ib,ilen
      character*2 cfunc
C      logical kflux ! true if there's a $FLUX section
      logical kcode     ! true if there's a $CODES section
      logical kop       ! true if there's a $OP section
      logical khead     ! true if there's a $HEAD section
      logical ksource   !true if a source section
      logical kexist
      integer nch,nch2  !array size
      integer trimlen   !string length function
      integer i2long,ibnum
      integer ivexnum,ncout,iserr(max_stn)
! This is used to set range limits for observations.
      character*256 ctemp
      integer iyr,ihr,ida,imin,isc   !time

      double precision tjd ! for APSTAR

      integer NumWant, NumGot    !for reading tokens
      character*30 ltoken(30) 
      integer istat 
      character*2 cstcod_sav(64)

C
C  History
C  WHEN   WHO CHANGES
!  2021-05-07  JMG Exper is now upto 128 characters
C  830425 NRV ADDED ELEVATION COMMAND
C  830524 WEH CHANGE LOOP LIMIT FOR PRECESSION FROM NSOUCR TO NCELES
C  830818 NRV MOVED SOURCE-SATELLITE NAME CONCAT. EARLIER SO THAT
C             LISTING FIRST OBSERVATION IS DONE AFTER THAT.
C  840824 MWH CREATE EMPTY SCRATCH FILE IF NECESSARY, ALLOW FILE
C               NAME IN RUN STRING.
C  850605 MWH CALL SSCAN WITH LINCA
C  880314 NRV DE-COMPC'D
C  880525 PMR adapted for workstation; replaced APOSN, POSNT, LOCF
C             with arrays to record information
C  890425 NRV Added sections for SEFD and SNR (copied from ELEV)
C  890531 NRV Changed temp file name to include PID
C  891115 NRV Changed ISCUN(7) to luskd
C  891118 NRV Added reading $FLUX section
C  891128 NRV Changed dimension of ISNRBL.
C  891129 NRV Removed variables with section names and coded
C             directly inline. General cleaning up.
C  891129 gag removed basescan
C  891227 NRV Changed MOVE to APSTAR for precession
C  900206 NRV Added reading $HEAD section and writing scratch file
C  900302 gag moved call to setsc from sked.f to this subroutine.
C  900305 gag added prepend of filename to path
C  900406 gag removed reading of SEFD's
C  920628 nrv added OP section
C  930119 nrv added VLBA section
C  930225 nrv implicit none
C  930708 nrv added HEAD section
C  931110 nrv Add st0cur to initialization
C 951017 nrv Change READS calls to change ibufq(1) to long
C 951017 nrv Read $OP last
C 951018 nrv Remove SEFD from this routine, re-read $OP at end
C 951019 nrv Remove reading $VLBA section
C 951116 nrv Add reading $NSKED section
C 960209 nrv Add error by station to GNPAS
C 960529 nrv Re-arrange sections to include VREAD call.
C 960604 nrv Add call to VGLINP.
C 970114 nrv Add CBUF to VREAD call so it can check the version.
C 970115 nrv Set all stations to itearl_save
C 970121 nrv Add iret to vob1inp call
C 970314 nrv Add EARLY lines. Change VEX reading to be like drudg.
C 970314 nrv Don't read $PARAM at start but at end when stations and
C            sources are known.
C 970319 nrv Read the WEIGHT lines
C 970515 nrv Initialize NSUBST and ISUBST in case it is not in $PARAM.
C 990524 nrv Add call to TTAPE.
C 991020 nrv Add call to SKORDER.
C 991118 nrv Initialize station times to nominal start, if no obs present.
C 991119 nrv Handle SNR_1 command.
C 000326 nrv Remove reading $EXPER, $PARAM section to routine EXREAD, PRREAD.
C 000404 nrv Remove reading $OP to opread.
C 000619 nrv Add call to OBS_SORT.
C 001005 nrv Set nominal start time to the time of the first observation,
C            and use time of the first station in the first scan. Formerly
C            UTCUR(1) was used but it should be UTCUR(ISTCUR(1)) to get the
C            correct time.
C 010102 nrv Add LUSCN to obs_sort call.
C 020713 nrv Use VOBINP (not VOB1INP).
C 020713 nrv Call PRREAD for both vex and sk, pass it kVexIn.
C 020713 nrv Copy calls to SETBA and GNPAS to VEX section.
C
C 2004Jan27  JMG Got rid of filling with blanks before "Call READS" since
!                READS blank fills already
!
! 2004Mar01  JMG added support for "ASTROMETRIC" Mode.
!   2006Aug03  JMGipson.  Got rid of call to trans. Not needed
!
! 2006Oct22. Changed argument from hollerith to ASCII
! 2007Jul03. Changed call to SOINP to make it ASCII.
! 2010Apr12. Added reading of SRcwt, StatWT sections 
! 2010.06.15 JMG.  Set flag kVexIn (true if input file is vex) which is now in common. 
! 2013Apr23  JMG.  Modified for broadband command. 
! 2018Oct10.  Set the flag "kvlba_corr" 

      ICH = 1
      NR=1
      kcode=.false.
      kop = .false.
      khead=.false.
      ksource=.false.
      ierr=0
      i1=-1 ! initialize first station to illegal value
      num_proc_lines = 0 
C
C  1. Open the schedule file.  Quit if there is any error.
C
      if(cname .eq. " ") then
        OPEN (luskd,file=CSKFIL,iostat=IERRCM,status='UNKNOWN')
      ELSE         ! open existing file
        nch = trimlen(csked)
        nch2 = trimlen(cname)
        if (cname(1:1).eq.'/'.or. cname(1:1) .eq. '.') then
          cskfil=cname(1:nch2)
        else
          cskfil = csked(:nch)//cname(1:nch2)
        endif
        inquire(file=cskfil,exist=kexist) 
        if(.not. kexist) then
          write(luscn,'(a)') "SKOPN00:  Did not find sked file "//
     >     cskfil(1:trimlen(cskfil))
          ierr=5
          return
        endif
        OPEN (luskd,file=CSKFIL,iostat=IERRCM,status='OLD')
      END IF
C
      IF (IERRCM.NE.0) THEN ! can't open schedule file
        nch = trimlen(cskfil)
        write(luscn,9010) ierrcm,cskfil(1:nch)
9010    format('SKOPN02 - Error ',I3,' opening schedule file ',A)
        ierr=5
        RETURN
      END IF  !can't open schedule file

C  2. Set up sked's scratch files
C
      CALL SETSC(IERRCM)
      IF  (IERRCM.LT.0) THEN
        WRITE(LUSCN,9100) IERRCM
9100    FORMAT('SKED - Error ',I5,' from SETSC.  Can''t continue.')
        STOP
      END IF  !

C  If this is an empty file, return now. Everything was initialized
C  in skini.

      IF(cname .eq. " ") return
C
C  1.5 Read the first line to find out if it's a VEX file.

      kVexIn=.false.
      read(luskd,'(a)') cbuf
      if (cbuf(1:3).eq.'VEX') then ! read VEX file
        kVexIn=.true.
        close(luskd)
C       read stations, codes, sources
        i=index(cbuf,';')
        call VREAD(cbuf(1:i),CSKFIL,luscn,iret,ivexnum,ierr)
        kvlba_corr=ccorname .eq. "VLBA"    

        if (iret.ne.0.or.ierr.ne.0) then
          write(luscn,9009) iret,ierr
9009      format(' from VREAD iret=',i5,' ierr=',i5)
        endif
C       Write out experiment information now.
        write(luscn,'(/"Experiment name: ",a)') trim(cexper)
        i=trimlen(cexperdes)
        if (i.gt.0) write(luscn,'("Experiment description: ",a)')
     .  cexperdes(1:i)
        i=trimlen(cpiname)
        if (i.gt.0) write(luscn,'("PI name: ",a)') cpiname(1:i)
        i=trimlen(ccorname)
        if (i.gt.0) write(luscn,'("Correlator: ",a)') ccorname(1:i)
        IF (NSTATN.GT.0) THEN ! get obs
          call VOBINP(ivexnum,LUscn,iret,IERR)
          if (ierr.ne.0.or.iret.ne.0) then
            write(luscn,
     >     '("SKOPNxx - Error from vobinp=",i5,", iret=",i5)') ierr,iret
            call errormsg(iret,ierr,'SCHED',luscn)
          endif
          write(luscn,
     >     '("  Total number of scans in this schedule: ",i5)') nobs
        endif ! get obs
        nsubst=nstatn
        do i=1,nstatn
          isubst(i)=i
        enddo
!        CALL GNPAS(luscn,ierr,iserr)
        if (ierr.ne.0) then
          write(luscn,'(a)') "SKOPN03 - Warning: error in GNPAS. "//
     >    "Inconsistent or erroneous track assignments."
           ierr=0
        endif
        call setba
      else ! skd file
        rewind(luskd)
C     endif ! vex/sked
C     endif is at the end of this routine.

C  3. Establish the outer loop for handling various sections
C     of the schedule file.  Get the first  $  entry, find out
C     what type it is, and set up the CASE statement.
C
C     Get the first entry which has $ in column 1
      CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,1)
      ibufq(1) = ilen
C
      DO WHILE (IBUFQ(1).NE.-1) !Process sections until EOF
C
        IF  (IERRCM.NE.0) THEN
          INUMCM = 42
          CALL WRERR(INUMCM,IERRCM)
          CLOSE(luskd)
          RETURN
        END IF
C
C         Set up variable to tell which section we have.
        cfunc = '  '
        IF (cbuf(1:6) .eq.'$EXPER') THEN
          cfunc='EX'
        ELSE IF (cbuf(1:3) .eq.'$OP') THEN
          cfunc='OP'
          kop = .true.    
        ELSE IF (cbuf(1:7) .eq.'$SOURCE') then
          cfunc='SO'
          ksource=.true.
        ELSE IF (cbuf(1:8) .eq.'$STATION') then
          cfunc='ST'
        ELSE IF (cbuf(1:6) .eq.'$CODES') THEN
          cfunc='CO'
          kcode = .true.
        ELSE IF (cbuf(1:5) .eq.'$SKED') THEN
          cfunc='SK'
        ELSE IF (cbuf(1:5) .eq.'$FLUX') THEN
          cfunc='FL'
          kflux = .true.
        ELSE IF (cbuf(1:5) .eq.'$HEAD') THEN
          cfunc='HD'
          khead = .true.
        END IF
C
        IF (cfunc.EQ.'  ') THEN  !unrecognized section name
          CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,1)
          ibufq(1) = ilen
        ELSE IF (cfunc.EQ.'EX') THEN  !experiment name
          call exread()
          CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,1)
          ibufq(1) = ilen          
        ELSE IF (cfunc .EQ. 'SO' .OR. cfunc .EQ. 'ST' .OR.
     &           cfunc .EQ. 'FL' .OR. cfunc .EQ. 'HD' .OR.
     &           cfunc .EQ. 'CO' .OR. cfunc .EQ. 'OP')
     &  THEN  !source, station, fluxes, head, freqs,  op

! This will read these sections of the files, and then write them out. 
          ncout = trimlen(cbuf)
C        Open the select file
9901      format(a,'  ',$)
          IF (cfunc.EQ.'SO') THEN
            OPEN(lusel,file=CSOFIL,iostat=IERR,status='old')
            write(luscn,9901) cbuf(1:ncout)
          ELSE IF (cfunc.EQ.'ST') THEN
            OPEN(lusel,file=CSTFIL,iostat=IERR,status='old')
            write(luscn,9901) cbuf(1:ncout)
          ELSE IF (cfunc.EQ.'CO') THEN
            OPEN(lusel,file=CFRFIL,iostat=IERR,status='old')
            write(luscn,'(a)') cbuf(1:ncout)
          ELSE IF (cfunc.EQ.'FL') THEN
            OPEN(lusel,file=CFLFIL,iostat=IERR,status='old')        
            write(luscn,'(a)') cbuf(1:ncout)
          ELSE IF (cfunc.EQ.'HD') THEN
            OPEN(lusel,file=CHDFIL,iostat=IERR,status='old')
            write(luscn,'(a)') cbuf(1:ncout)
          ELSE IF (cfunc.EQ.'OP') THEN
            OPEN(lusel,file=COPFIL,iostat=IERR,status='old')
            write(luscn,'(a)') cbuf(1:ncout)
          ENDIF
C
C           Get the first line of this section
          CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,2)
          ibufq(1) = ilen
C
          DO WHILE (cbuf(1:1) .ne. "$".AND.IBUFQ(1).NE.-1)
C     decode an entry
C
            if(cbuf(1:1) .eq. "*") cycle
            INUMCM = 0
            IERRCM = 0
            IBUFQ(1) = (IBUFQ(1)+1)/2
            IF (cfunc.EQ.'SO') THEN
              CALL SOINP(cbuf,luscn,IERRCM)
            ELSE IF (cfunc.EQ.'ST') THEN
              CALL STINP(IBUF,i2long(IBUFQ(1)),luscn,IERR)
            END IF
C
            IF  (INUMCM.NE.0) THEN
              CALL WRERR(INUMCM,IERRCM)
C             Write out the offending line
              ncout = trimlen(cbuf)
              write(luscn,9901) cbuf
            END IF
C
            write(lusel,'(a)') cbuf(1:2*ibufq(1))
            CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,2)
            ibufq(1) = ilen
          END DO  !decode an entry
          CLOSE(lusel)
          IF (cfunc.EQ.'SO') THEN
            write(luscn, "(3x,i5,' sources')") nsourc
          ELSE IF (cfunc.EQ.'ST') THEN
            write(luscn, "(2x,i5,' stations')") nstatn          
          ENDIF
C
        ELSE IF (cfunc .EQ. 'SK') THEN  !schedule
          ncout = trimlen(cbuf)
          write(luscn,9901) cbuf(1:ncout)
C           Read the first line of the schedule
          CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,2)
          ibufq(1) = ilen
C
          DO WHILE (cbuf(1:1) .ne. "$".AND.IBUFQ(1).NE.-1.AND.
     .            NOBS.LT.MAX_OBS) ! read schedule, store in ISKREC
            NOBS = NOBS + 1
            cskobs(nxtrec)=cbuf
            ISKREC(NOBS) = NXTREC
            NXTREC = NXTREC+1
            IF (IERRCM.NE.0) THEN
              INUMCM = 43
              CALL WRERR(INUMCM,IERRCM)
              RETURN
            END IF
            CALL READS(luskd,IERRCM,IBUF,IBLEN,ilen,2)
            ibufq(1) = ilen
          END DO  !read and write an entry
C
          write(luscn, "(5x,i6,' scans')") nobs
          IF (NOBS.EQ.MAX_OBS) CALL WRERR(32,MAX_OBS)
C
        END IF  !schedule
      END DO  !Process sections until EOF
C
C    Now we can read the CODES section because we have the
C    stations selected (maybe).

      if (kcode) then !read CODES
        write(luscn,'("Re-reading CODES.",$)')
        call frinit(nstatn,max_frq)
        OPEN(lusel,file=CFRFIL,iostat=IERR,status='old')
        call reads(lusel,ierrcm,ibuf,iblen,ilen,2)
        ibufq(1) = ilen
        do while (ibufq(1).ne.-1)
          call frinp(ibuf,i2long(ibufq(1)),luscn,ierrcm)
          call reads(lusel,ierrcm,ibuf,iblen,ilen,2)
          ibufq(1) = ilen
        enddo
        write(luscn,9906) ncodes
9906    format(5x,'(',i1,' frequency codes)')
        close(lusel)
      endif

C    Now we can read the HEAD section.
      if (khead) then
      endif
C
C    Move the satellite source names so that they are contiguous with
C      the celestial soure names. This is pretty inelegant and will
C      cause even greater ugliness if there is ever a way to add a
C      celestial source without going through SOCAT, since the satellite
C      names will have to be moved to make room. However this technique
C      causes the least disturbance to other parts of SKED. Note in
C      particular that the rest of the code assumes that names are
C      contigous at the start of the array, and that satellites follow
C      celestial sources. See SOSEL where this code is also used.
C   This must be done before fluxes are read so that all the source
C      names are there to be checked for.
C
      IF (NSATEL.GT.0.AND.NCELES.LT.MAX_CEL) THEN  !
        DO  I=1,NSATEL
          NSOR=NCELES+I
          NSAT=MAX_CEL+I
          csorna(nsor)=csorna(nsat)
        END DO  !
      END IF  !

C     Derive the number of passes in each frequency code

      if (ncodes.gt.0) then
!        CALL GNPAS(luscn,ierr,iserr)
        if (ierr.ne.0) then
          write(luscn,'(a)')"SKOPN03 - Warning: error in GNPAS. "//
     >    "Inconsistent or erroneous track assignments."
        endif
C       Figure out the frequency bands in use now.
        call setba
      endif !

C
C    Now we can read the $OP section because stations and
C    sources have been selected.
      endif ! read vex/skd file

C     FINALLY Close the original schedule file
      CLOSE(luskd)

     
100   continue
      write(luscn,'("Processing ",$)') 
  
      rmax_astro=1.               !initialize to no astrometric mode.
      rmin_astro=0.
! Now re-read some sections. These must be read after stations, sources.
      OPEN (luskd,file=CSKFIL,iostat=IERRCM,status='OLD')

      write(luscn,'(a," ",$)') "$PARAM" 
      call param_read(luskd)    !Read in the $PARAM section
      if(minsubnetsize .gt. nstatn) then 
         write(luscn, '("Warning! Minsubnetsize=",i3,a,i3)') 
     >      minsubnetsize, " > num_stations= ",nstatn
         write(luscn,'(a)') "Changing minsubnet to num_stations!"
         minsubnetsize=nstatn
      endif



160   continue 
      write(luscn,'(a," ",$)') "$OP" 
      call op_read(luskd)       !Read in the $OP  section
      rewind(luskd) 


! find a line that begins with a $ 
200   continue
      read(luskd,'(a)',err=300, end=300) cbuf
      if(cbuf(1:1) .ne. "$") goto 200
210   continue
      if(cbuf .eq. "$MAJOR" .or. cbuf .eq. "$MINOR" .or. 
     >   cbuf .eq. "$ASTROMETRIC" .or. cbuf .eq. "$SRCWT" .or.
     >   cbuf .eq. "$STATWT" .or. cbuf .eq. "$STAT_SEFD" .or.
     >   cbuf .eq. "$TWIN_TELESCOPES" .or.
     >   cbuf .eq. "$BROADBAND" .or. cbuf .eq. "$GROUP") then  
         write(luscn,'(a," ",$)') trim(cbuf) 
      endif 
   
! Process other commands that determine how the schedule is setup. 
      if(cbuf .eq. "$MAJOR") then
        kopgo =.false.    !set to true by "Hour command"
        do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210       
          if(cbuf(1:1) .ne. "*") call major_cmd(cbuf) 
        end do
      else if(cbuf .eq. "$MINOR") then
        do while(.true.)        
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          if(cbuf(1:1) .ne. "*")  call minor_cmd(cbuf) 
        end do
      else if (cbuf.eq. '$ASTROMETRIC') then
         do while(.true.)             
            read(luskd,'(a)',err=300,end=300) cbuf            
            if(cbuf(1:1) .eq. "$") goto 210
            if(cbuf(1:1) .ne. "*") then 
              ctemp="SET "//cbuf(1:trimlen(cbuf))
              call astro_cmd(ctemp)
            endif         
        end do
! The read command is only valid during input
      else if (cbuf.eq. '$TWIN_TELESCOPES') then
         ktwin_read_valid=.true.
         do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") then
             ktwin_read_valid=.false.
             goto 210
          endif
          ctemp="READ "//cbuf(1:trimlen(cbuf))
          call twin_cmd(ctemp)
        end do
      else if (cbuf.eq. '$GROUP') then
         kgroup_read_valid=.true.
         do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") then
             kgroup_read_valid=.false.
             goto 210
          endif
          ctemp="READ "//cbuf(1:trimlen(cbuf))
          call group_cmd(ctemp)
        end do
      else if(cbuf .eq. "$STAT_SEFD") then
        do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          call stat_sefd_in(cbuf,ierr)
        end do 
      else if(cbuf .eq. "$CATALOGS_USED") then
        do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210         
          call cat_cmd(cbuf) 
        end do 

      else if (cbuf.eq. '$FLUX') then
! If our input file is VEX then we need to make a copy fo the $FLUX section.
! This is used when we write out the file. 
        if(kvexin) then
          open(lutmp,file=cflfil)
        endif
        do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210 
          if(kvexin) write(lutmp,'(a)') trim(cbuf)
          if(cbuf(1:1) .ne. "*") call flinp(cbuf,luscn,ierrcm)          
        end do     
        if(kvexin) close(lutmp) 

      else IF (cbuf.eq. '$SRCWT') then
         do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          if(cbuf(1:1) .ne. "*") then 
            ctemp="SET "//cbuf(1:trimlen(cbuf))
            call srcwt_cmd(ctemp)
          endif 
        end do
      else IF (cbuf.eq. '$STATWT') then
         do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          if(cbuf(1:1) .ne. "*") then 
            ctemp="SET "//cbuf(1:trimlen(cbuf))
            call statwt_cmd(ctemp)
          endif 
        end do
      else IF (cbuf.eq. '$BROADBAND') then
         do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          if(cbuf(1:1) .ne. "*") then 
            ctemp="SET "//cbuf(1:trimlen(cbuf))
            call broadband_cmd(ctemp)
          endif 
        end do

      else if(cbuf .eq. '$DOWNTIME') then
        do while(.true.)
          read(luskd,'(a)',err=300,end=300) cbuf
          if(cbuf(1:1) .eq. "$") goto 210
          if(cbuf(1:1) .ne. "*") call downtime(cbuf)          
        end do
      else if(cbuf .eq. '$PROCS') then
         num_proc_lines=0
         do while(.true.)
           read(luskd,'(a)',err=300,end=300) cbuf
           if(cbuf(1:1) .eq. "$") goto 210
           if(cbuf .ne. " ")  then       !skip blanks
             num_proc_lines=num_proc_lines+1
             cproc_lines(num_proc_lines)=cbuf(1:trimlen(cbuf))
           endif           
        end do

      else if(Cbuf .eq. '$END') then
         goto 300 
      endif
      goto 200

300   continue
! must include this here in case $GROUP is the last section of the sked file.
! In that case exit from reading $GROUP without turning off the 'read' command.
      kgroup_read_valid=.false.
      close(luskd)
      if(rcovar_win .eq. -1.) rcovar_win = 1

!----------------------------------------------------

C    Check and fix time ordering of observations
C      write(luscn,'("SKOPN00 - Calling skorder.")')
C     call skorder
      call obs_sort(luscn,nobs)

C    Compute baseline lengths
C
      do i=1,nstatn-1
        do j=i+1,nstatn
          ib=ibnum(i,j)
          bx(ib)=stnxyz(1,j)-stnxyz(1,i)
          by(ib)=stnxyz(2,j)-stnxyz(2,i)
          bz(ib)=stnxyz(3,j)-stnxyz(3,i)
          baselen(ib)=dsqrt((stnxyz(1,i)-stnxyz(1,j))**2 +
     .    (stnxyz(2,i)-stnxyz(2,j))**2 +
     .    (stnxyz(3,i)-stnxyz(3,j))**2)/1.d3
        enddo
      enddo
C

C     Check consistency of recording types.
      ks2=.false.
      kk4=.false.
      kmv=.false.
      do i=1,nstatn
        ks2=cterna(i)(1:2) .eq. 'S2'
        kk4=cterna(i)(1:2) .eq. 'K4'
        kmv=cterna(i)(1:1) .eq. 'M' .or. cterna(i)(1:1) .eq. 'V'
      enddo
      if ((ks2.and.kk4).or.(ks2.and.kmv).or.(kk4.and.kmv)) then
        write(luscn,'(a,$)') "SKOPN04 - Warning: Mixed recorder "//
     >    " types found in station equipment: "
      endif
      if (ks2) write(luscn,'("S2 ",$)')
      if (kk4) write(luscn,'("K4 ",$)')
      if (kmv) write(luscn,'("Mk3/4/VLBA ",$)')
      write(luscn,'()')

C     Make the first observation current by listing it
C     This also gets the current date set.
      if (nsubst.eq.0) then
        nsubst=nstatn
        do i=1,nstatn
          isubst(i)=i
        enddo
      endif
      IF (NOBS.GT.0.AND.NSOURC.GT.0.AND.NSTATN.GT.0) THEN  !
        cbuf="."
        ibufq(1)=1
        CALL LICMD(IBUFQ,"LI")
      END IF  !
C
C     Initialize all stations not participating in the first
C     observation to the date of the first observation.
C
      IF  (nobs.gt.0.and.NSTATN.GT.0) THEN
        i1 = istcur(1) ! first station in the first scan
        DO  I=1,MAX_STN
          IYRCUR(I)=IYRCUR(I1)
          IDACUR(I)=IDACUR(I1)
          MJDCUR(I)=MJDCUR(I1)
          GSTCUR(I)=GSTCUR(I1)
          st0CUR(I)=st0CUR(I1)
          UTCUR(I)= UTCUR(I1)
        END DO  !
      END IF

C     If there was no initial observation, and the nominal start
C     was set, use the nominal start time to initialize the stations.

      if (iyr_start.ne.0.and.nobs.eq.0.and.nstatn.gt.0) then ! use nominal time
        DO  I=1,MAX_STN
          ut = hms2seconds(ihr_start,imin_start,isc_start)
          MJD = JULDA(1,IDA_start,IYR_start-1900)
          CALL SIDTM(MJD,ST0,FRAC)
          GST = DMOD(ST0 + UT*FRAC, 2.D0*PI)
          IYRCUR(I)=iyr_start
          IDACUR(I)=ida_start
          MJDCUR(I)=MJD
          GSTCUR(I)=GST
          st0CUR(I)=st0
          UTCUR(I)= UT
        END DO  !
      endif ! use nominal time

C     If there was no nominal start time, but there was an initial
C     observation, set the nominal start to that.
C  NO, change to: if there was an initial observation, always REset
C  the nominal start to that time.

      if (nobs.gt.0) i1=istcur(1) ! first station in first scan
      if (i1.gt.0.and.iyrcur(i1).ne.0) then ! change nominal start
        iyr_start = iyrcur(1)
        ida_start = idacur(1)
C       Figure out hms from ut
        call seconds2hms(utcur(1),ihr_start,imin_start,isc_start)
      endif ! change nominal start
! Make sure end is at least 1 hour later. If less than 1 hour, set to 1 day.
      call AdjustEndTime(itimestart,itimeend,3600)

C     Now that we have a date, precess the source positions.
      do while(iyr_start .le. 0) ! need to set starting time.
        WRITE(LUSCN,'(a)')
     >  "Enter start date/time in form yyyydddhhmmss or yyyymmddhhmmss:"
        read(luusr,'(a)') ctemp
        call ydhms(ctemp,ierr,iyr,ida,ihr,imin,isc)
        IF (iERR.NE.0) THEN
          WRITE(LUSCN,'("SKOPN - Error in specified date or time.")')
        else
          iyr_start = iyr
          ida_start = ida
          ihr_start = ihr
          imin_start = imin
          isc_start = isc
        endif
      end do
      call init_time_arrays(iyr_start,ida_start,ihr_start,
     >      imin_start,isc_start)
      CALL SUNPO(MJDCUR(1),UTCUR(1),PI,RASUN,DECSUN)
      tjd=mjdcur(1)+2440000.d0
     
      DO  I=1,NCELES
        call apstar_Rad(tjd,sorp50(1,i),sorp50(2,i),
     >         sorpda(1,i),sorpda(2,i))
      END DO  !
C
C       Now check that we have gotten complete station information.
C
      call chstn
      call op_refresh 
C
      IERRCM = 0
      INUMCM = 0
      RETURN
      END
