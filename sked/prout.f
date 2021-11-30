      SUBROUTINE PROUT(ciin)
C
C     This routine writes out the $PARAM section for SKCLS,
C     including SUBNET, ELEVATION, SCAN, SNR, 
C     and EARLY lines.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  COMMON BLOCKS USED:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'

! Recent history (in reverse order)
! 2016Jan04  JMG Changed MINIMUM to MINSLEW
! 2017Oct06 KOL. Added kconf_equip
! 2020Oct02 JMG. Removed all references to S2


! functions
      integer trimlen,ibnum
      character*1 lyn
    
C INPUT
      character*1 ciin ! v=vex output, s=standard,d=terminal
C

C  LOCAL VARIABLES:
      logical ksnrw,ksnrw_1
      INTEGER nch,i1,j1,i,j,iba,ib,iloop
      integer ilen,ival
      integer iend

      double precision k4sp
         integer i2
      character*5 cTapeDens

      character*5 cbaseline
      character*2 cpotmp

      character*4 lsnr

      character*4 lprflag
      integer*4 itime_vec(6)
      integer itemp

      logical ksame
C
C  HISTORY:
C  GAG  890428  REMOVED FROM SKCLS INTO ITS OWN FILE
C  GAG  890517  ADDED CORSYNCH
C  NRV  891116  Added BAND to output of SEFD lines
C  GAG  891121  Changed ISCUN(2) to lutmp
C  NRV  891128  Changed dimension of ISNRBL and wrote out BAND
C  gag  891129  removed BASESCAN,SNRSCAN,BWSCAN, and CHANSCAN
C  NRV  891205  Added KASNR
C  NRV  891207  Added writing out MARGIN on SNR lines
C  gag  900206  removed LENGTH(mxfeet)
C  gag  900406  removed writing out of SEFD
C  gag  900531  added check for snr's before writing out.
C  gag  900724  removed an extra ',1' from the parameter call to ib2as
C  NRV  910224  Added EARLY, removed ELEVATION, PEAK
C  nrv  930408  Put minbetween back in
C  nrv  931029  Remove minbetween, now in SEOP
C  nrv  950412  Write out ELEVATION with 2-letter codes
C  nrv  950412  Write out SUBNET with 2-letter codes
C  nrv  950505  Add MINSUBNET
C 951214 nrv Add BARREL
C 960123 nrv Move BARREL down one line
C 960709 nrv Remove BARREL, moved to freqs.ftni
C 960923 nrv Write ITEARL for station 1
C 970314 nrv Remove writing EARLY in parameters and write a separate line.
C 970314 nrv Clear buffer out to ibuf_len instead of just 80. Shorten SNR
C            maximum length so it doesn't overflow a nice 80 columns.
C 970314 nrv Write out default size of window width
C 970317 nrv Have to put EARLY into $PARAM for backwards compatibility
C            and for the Mark III correlators.
C 970319 nrv Write out WEIGHT commands.
C 970402 nrv Write out TAPE_MOTION commands and gap for adaptive.
C 970613 nrv Don't write out the EARLY_START line if all are the same
C            because the value is already in the EARLY line
C 990520 nrv Write out description, scheduler, and correlator.
C 990621 nrv Write out TAPE_TYPE commands.
C 990624 nrv Write MAXSCAN.
C 990915 nrv Replace REIO with WRITE.
C 991006 nrv Add ciin calling parameter. Add fcreate calls.
C 991118 nrv Add START, END nominal times.
C 991119 nrv Write SNR_1 output lines.
C 000110 nrv Remove ISHFT(ONE).
C 000121 nrv Don't write START, END if they have not been set.
C 000126 nrv Add S2 and K4 tape length output on TAPE_TYPE line.
C 000605 nrv Write TAPE_ALLOCATION lines
C 001005 nrv Write TAPE_TYPE using SHORT option.
C 020713 nrv Write K4 TAPE_TYPE in minutes not meters.
C 021011 nrv Write POSTPASS parameter.

! 2003Sep12 JMG  Set kvisde=true to be "ALL". Previously was "SUB"
!                inconsistent with prlis and prset command.
! 2004Nov03 JMG  Got rid of ls2speed, replaced cs2speed.
! 2005Apr29 JMG  Put out version.
! 2005Apr29 JMG  Also put out when scheduled.
! 2005Jul06 JMG  Don't put out SUBNET if nsubst<1
! 2006Jul27 JMG  Changed check for S2 systems to see if tape_type is all the same.
! AEM 20070319 fix parameter END output if no date specified (now full 9 zeros)
! 2007Oct04 JMG  Got rid of residual hollerith.
!                Added Fillbest,  filltime,fillsub
!                Changed logic of writing out tape_***
! 2008May22  Moved many parameters from $PARAM to $MAJOR
! 2009Jan20  Fixed bug in writing out long lines for Tape_allocation, tape_motion, etc. 
! 2009Apr02  Fixed bug in writing out tape type if we had K5 stations. 
! 2009Oct30  Got rid of unused vector array lsrcdist
! 2010Jan27  JMG. Wasn't outputting Mark5B Tape type correctly. 
! 2010Mar24  JMG. Above buf fix made things worse. Just output Mark5, didn't indicate type
! 2010Apr15 JMG. Remove WEIGHT output. This info is not used anywhere. 
! 2010Sep16 Got rid tape parameters knorewind, kpostpass, ksynch
! 2011May24 JMG. Better handling of TAPE_TYPE. Checks first to see if Mark5 or K5
! 2012Jul06 JMG. Was not writing out TAPETM although sked initialized this to 1.                 
! 2012Sep25 JMG. Moved writing of $PARAM (for VEX files) from skcls to here.
! 2015Mar17 JMG. Added Mark6_off
! 2016Dec08 KOL. Added Fill_off
! 2016Dec12 JMG. Got rid of TAPE_TYPE parameter. This is obsolete since everything is now disk. 

      cbuf="$PARAM"
      write(luscn,'(a)') trim(cbuf) 
      call wrt_param_line(cbuf, lutmp,ciin) 
   
!      ciin="d"
C Line 1.
      i1=trimlen(cexperdes)
      if (i1.gt.0) then
        write(cline,"('DESCRIPTION ',a)") cexperdes(1:i1)
      else
        write(cline,"('DESCRIPTION tbd')")
      endif
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,"('SCHEDULING_SOFTWARE SKED')") 
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,"('SOFTWARE_VERSION ',a)") skversion
      call wrt_param_line(cline,lutmp,ciin)

      call date_and_time_sked(itime_vec)
        write(cline,
     >  '("SCHEDULE_CREATE_DATE ",i4,2("/",i2.2),1x,2(i2.2,":"),i2.2)')
     >   itime_vec
      call wrt_param_line(cline,lutmp,ciin)

! next line. Scheduler, correlator, start and stop times.
      i1=trimlen(cpiname)
      if (i1.eq.0) then
        cpiname="tbd"
        i1=3
      endif
      i2=trimlen(ccorname)
      if(i2 .eq. 0) then
        ccorname="tbd"
        i2=3
      endif
      if(i1 .lt. 6) i1=6

        write(cline,'(2(a,1x,a,1x),2(a,1x,i4.4,i3.3,3i2.2,1x))')
     >  "SCHEDULER ",cpiname(1:i1), "CORRELATOR",ccorname(1:i2),
     >  "START",iyr_start,ida_start,ihr_start,imin_start,isc_start,
     >  "END  ",iyr_end,ida_end,ihr_end,imin_end,isc_end
      call wrt_param_line(cline,lutmp,ciin)

C Output the lines which have numbers in them.
! Roughly alphabetical order.
      write(cline,'(4(a11,1x,i5,1x))')
     > "CALIBRATION",icalde,
     > "CORSYNCH   ",itsync,"DURATION   ",idurde
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,'(4(a11,1x,i5,1x))')
     > "EARLY      ",itearl(1), 
     > "IDLE       ",idldef,    "LOOKAHEAD  ",lookah/60
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,'(4(a11,1x,i5,1x))')
     > "MAXSCAN    ",maxscn,    "MINSCAN    ",minscn,
     > "MINSLEW    ",imintm,    "MARK6_OFF  ",imark6_off
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,'(1(a11,1x,i5,1x))')
     > "FILL_OFF   ",ifill_off
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,'(4(a11,1x,i5,1x))')
     > "MIDTP      ",imtptm, "MODULAR    ",imodtm,
     > "MODSCAN    ",modscn,  "PARITY     ",ipartm
      call wrt_param_line(cline,lutmp,ciin)

      write(cline,'(4(a11,1x,i5,1x))')
     > "SETUP      ",isettm,"SOURCE     ",isortm, 
     > "TAPETM     ",itaptm, "WIDTH      ",iwdef
      call wrt_param_line(cline,lutmp,ciin)

   
! Output line with yes/no.
! order is confirm,fillobs, postpass,vscan
      write(cline,'(4(a11,5x,a1,1x))')
     > "CONFIRM    ",lyn(kask), "VSCAN      ",lyn(kvscan)
      call wrt_param_line(cline,lutmp,ciin)


!      write(cline,'(4(a11,5x,a1,1x,a1,1x,a1,1x))')
      write(cline,'(4(a11,5x,a1,1x))')
     >"DEBUG      ",lyn(kdebug),  "KEEP_LOG   ",lyn(kkeep_log),
     >"VERBOSE    ",lyn(kverbose),"CONF_EQUIP ",lyn(kconf_equip)

      call wrt_param_line(cline,lutmp,ciin)

! Output line with various ascii parameters.
      if(kasnr) then
        lsnr="AUTO"
      else
        lsnr=" MAN"
      endif
  
      lprflag="NNNN"
      DO  I=1,4
        IF(KFLG(I)) lprflag(i:i)="Y"
      END DO
      write(cline,
     >'("PRFLAG",7x,a," SNR",10x,a)') lprflag,lsnr
      call wrt_param_line(cline,lutmp,ciin)

C Line 4
      write(cline,'(4(a,1x,a,1x))')"FREQUENCY  ",ccode(icode_set_last),
     > "PREOB     ",cprede,"MIDOB    ",cmidde,"POSTOB    ",cpstde
      call wrt_param_line(cline,lutmp,ciin)

C SUBNET line
      IF(NSTATN.GT.0) THEN  !SUBNET,ELEVATION,SNR,SNR_1 commands

C ELEVATION line
        ksame=.true.
        do i=2,nstatn
          if(stnelv(i) .ne. stnelv(1)) ksame=.false.
        end do

        if(ksame) then
          iend=1  	       !Output 1 value for all stations
        else
          iend=nstatn         !Output 1 value per station
        endif
        nch=0

        do i=1,iend
          if(nch .eq. 0) then
            cbuf="ELEVATION "
            nch=11
          endif
          if(ksame) then
            cpotmp="_"
          else
            cpotmp=cpocod(i)
          endif

          write(cbuf(nch:nch+7),'(A2,1x,f4.1)') cpotmp,
     >        stnelv(i)*rad2deg
          nch=nch+8
          if(nch .gt. 68 .or. i .eq. iend) then
            call wrt_param_line(cbuf,lutmp,ciin)
            nch=0
          endif
        END DO  !elevation

C EARLY_START line
        ksame=.true.
        do i=2,nstatn
          if (itearl(i).ne.itearl(1)) ksame=.false.
        enddo
        if (ksame) then ! all have same value
! Don't need to specify here if all the same.
        else ! individual values
          nch=0
          do i=1,nstatn
            if(nch .eq. 0) then
              cbuf="EARLY_START"
              nch=13
            endif
            write(cbuf(nch:nch+6),'(a2,1x,i3)') cpocod(i),itearl(i)
            nch=nch+7
            if(nch .gt. 68 .or. i .eq. nstatn) then
              call wrt_param_line(cbuf,lutmp,ciin)
              nch=0
            endif
          END DO  !early
        endif ! single/multiple values
        if(nch .ne. 0) call wrt_param_line(cbuf,lutmp,ciin) 

C TAPE_MOTION line 
        ksame=.true.
        do i=2,nstatn
          if((tape_motion_type(i).ne.tape_motion_type(1)) .or.
     >       (tape_motion_type(i).eq.'ADAPTIVE'.and.itgap(i).ne.
     .            itgap(1))) ksame=.false.
        enddo

        if(ksame) then
          iend=1  	       !Output 1 value for all stations
        else
          iend=nstatn         !Output 1 value per station
        endif
        nch=0
        do i=1,iend
          if(nch.eq.0) then
            cline="TAPE_MOTION"
            nch=13
          endif
          if(ksame) then
            cpotmp="_"
          else
            cpotmp=cpocod(i)
          endif

          if(tape_motion_type(i) .eq. 'ADAPTIVE') then
              write(cline(nch:),'(a2," ADAPTIVE",i6)')
     >              cpotmp,itgap(i)
          else
                write(cline(nch:),'(a2,1x,a)')
     >              cpotmp,tape_motion_type(i)
          endif
          nch=trimlen(cline)+2
          if(nch .gt. 68 .or. i.eq. iend) then
            call wrt_param_line(cline,lutmp,ciin)
            nch=0
          endif
        END DO  !tape_motion
        if(nch .ne. 0) call wrt_param_line(cbuf,lutmp,ciin) 

C TAPE_TYPE line
! NOTE!!!! No longer do this because everything is DISK. 
        if(.false.) then                 
        ksame=.true.
        write(*,*) "cstrec ", cstrec(1:2,1) 
        if(cstrec(1,1) .eq. "Mark5A".or. cstrec(1,1) .eq. "K5".or.
     >     cstrec(1,1) .eq. "Mark5B" .or. cstrec(1,1) .eq."MARK6") then
          do i=2,nstatn
            if(cstrec(i,1) .ne. cstrec(1,1)) ksame=.false.
          end do
        endif             

        if(ksame) then
          iend=1            !Output 1 value for all stations
        else
          iend=nstatn       !Output 1 value per station
        endif
        nch=0

        do i=1,iend
*          write(*,"(i4,5(1x,a))")i, cpocod(i), cterna(i), cterid(i),
*     &                      cstrec(i,1)
          if(nch .eq. 0) then
            cbuf="TAPE_TYPE "
            nch=11
          endif
          if(ksame) then
            cbuf(nch:nch)="_ "
          else
            cbuf(nch:nch+1)=cpocod(i)
          endif
          nch=nch+3
          if(cstrec(i,1)(1:5) .eq. "Mark5") then        
            cbuf(nch:nch+6)=cstrec(i,1)(1:6)
            nch=nch+7
          else if(cstrec(i,1) .eq. "Mark6") then
            cbuf(nch:nch+9)="THIN HIGH"
            nch=nch+10
          else if(cstrec(i,1) .eq. "K5") then
            cbuf(nch:nch+1)="K5"
            nch=nch+3
          else if (cterid(i)(1:2) .eq. "K4") then
!            k4sp = speed(1,i) ! speed for code 1 in m/s
!            ival = idint(0.1 + maxtap(i)/(60.d0*k4sp)) ! min=m/(60*m/s)
            write(cbuf(nch:),'(i4)') 0
            nch=nch+5
          else  
            if (maxtap(i).gt.5000.and.maxtap(i).lt.10000) then
              cbuf(nch:nch+4)="THICK"
              nch=nch+6
            else
              if (maxtap(i).lt.5000) then
                cbuf(nch:nch+5)="SHORT"
              else
                cbuf(nch:nch+5)="THIN "
              endif
              nch=nch+7
              if(bitDens(i,1)      .gt.5600000.0) then
                cTapeDens="DUPER"
              else if(bitDens(i,1) .gt.560000.0) then
                cTapeDens="SUPER"
              else if(bitDens(i,1) .gt.56000.0) then
                cTapeDens="HIGH"
              else
                cTapeDens="Low"
              endif
              cbuf(nch:nch+5)=CtapeDens
              nch=nch+7
            endif
          endif
          if(nch .gt. 68 .or. i .eq. iend) then
            call wrt_param_line(cbuf,lutmp,ciin)
            nch=0
          endif
        END DO  !tape_type
        ENDIF                      !end tape type. 

        if(nch .ne. 0) call wrt_param_line(cbuf,lutmp,ciin) 
C TAPE_ALLOCATION line
        ksame =.true.
        do i=2,nstatn
          if (tape_allocation(i).ne.tape_allocation(1)) ksame=.false.
        enddo

        if(ksame) then
          iend=1            !Output 1 value for all stations
        else
          iend=nstatn       !Output 1 value per station
        endif
        nch=0

        do i=1,iend
          if(nch .eq. 0) then
            cbuf="TAPE_ALLOCATION"
            nch=17
          endif

          if(ksame) then
            cpotmp="_"
          else
            cpotmp=cpocod(i)
          endif
          write(cbuf(nch:),'(a2,1x,a)') cpotmp,tape_allocation(i)
          nch=trimlen(cbuf)+2
          if(nch .ge. 68) then
            call wrt_param_line(cbuf,lutmp,ciin)
            nch=0
          endif
        end do
        if(nch .ne. 0)  call wrt_param_line(cbuf,lutmp,ciin)

C SNR and SNR_1 lines
        ksnrw = .false.
        ksnrw_1 = .false.
        do i=1,max_band
          do j=1,max_baseline
            if (isnrbl(i,j).ne.0) then
              ksnrw = .true.
            end if
            if (isnrbl_1(i,j).ne.-1) then
              ksnrw_1 = .true.
            end if
          end do 
        end do

        do iloop=1,2 ! SNR and SNR_1 lines
          if ((iloop.eq.1.and.nband.gt.0.and.ksnrw).or.
     .        (iloop.eq.2.and.nband.gt.0.and.ksnrw_1)) then !write SNRs

            nch=0
            do i1=1,nstatn
            do j1=i1+1,nstatn
              if(nch.eq. 0) then
                if(iloop .eq. 1) then
                  cbuf="SNR"
                  nch=5
                else
                  cbuf="SNR_1"
                  nch=7
                endif
              endif
              cbaseline=cpocod(i1)//"-"//cpocod(j1)
              ib=ibnum(i1,j1)
              do iba=1,nband
                if(iloop .eq. 1) then
                   itemp=isnrbl(iba,ib)
                else
                   itemp=isnrbl_1(iba,ib)
                endif
                write(cbuf(nch:nch+11),'(a5,1x,a2,1x,i3)')
     >             cbaseline,lband(iba),itemp
                nch=nch+13
              enddo
              if(nch .gt. 60) then
                call wrt_param_line(cbuf,lutmp,ciin)
                nch=0
              endif
            end do
            end do
            if(nch .gt. 8) then    !finish writing out buffer
               call wrt_param_line(cbuf,lutmp,ciin)
            endif

            if(iloop .eq. 1) then
              cbuf="SNR"
              nch=5 
            else
              cbuf="SNR_1"
              nch=7
            endif
            do iba=1,nband
              if(iloop .eq. 1) then
                 itemp=imarg(iba)
              else
                 itemp=imarg_1(iba)
              endif
              write(cbuf(nch:nch+11),'("MARGIN ",a2,i3,1x)')
     >           lband(iba),itemp
              nch=nch+13
            end do
            call wrt_param_line(cbuf,lutmp,ciin)
! Second line for AST_MARGIN
            if(iloop .eq. 1) then
              cbuf="SNR"
              nch=5            
              do iba=1,nband               
                write(cbuf(nch:nch+15),'("AST_MARGIN ",a2,i3,1x)')
     >           lband(iba),imarg_ast(iba)
                nch=nch+17
             end do
              call wrt_param_line(cbuf,lutmp,ciin)
            endif 
          endif !write SNRs
        enddo ! SNR and SNR_1 lines
      endif !SUBNET,ELEVATION,SNR commands
C
C SCAN lines
      nch=0
      do i=1,nsourc
        if(nch.eq.0) then
          cbuf="SCAN "
          nch = 8
        endif
        write(cbuf(nch:nch+6),'(i3,1x,i3)') i, isscan(i)
        nch=nch+8
        if(nch .gt. 68 .or. i .eq. nsourc) then
          call wrt_param_line(cbuf,lutmp,ciin)
          nch=0
        endif
      END DO  !sources
C
C    
      RETURN
      END
