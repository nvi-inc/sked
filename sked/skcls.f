      SUBROUTINE SKCLS(cmdcod_out,LINSTR,KERR)
C     CLOSE & WRITE SCHEDULE FILE
C     SKCLS writes out the new schedule file, section by section.
C
      use group_mod    ! module containing GROUP defintions and routines
      use twin_mod     ! module containing TWIN_TELESCOPES defintions and routines
     
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'major.ftni'
    
C
C  INPUT:
      integer*2 LINSTR(*)
C      - input string with possible file name
      character*2 cmdcod_out
C        cmdcod   - Function to be done: ER,EC,AB,WR,WC
C                   ER or VER (code VE) means replace file
C                   EC or VEC (code VC) means create new file
C                   AB means abort
C                   WR or VWR (code VR) means write replace
C                   WC or VWC (code VW) means write create
C  OUTPUT:
      integer kerr
      character*4 cmdcod
C
C  SUBROUTINES CALLED: EXOUT, PROUT, SOOUT, STOUT, COOUT, SKOUT,
C                      NULL_TERM, FLOUT, HDOUT,
C                      HOL2CHAR, CHSKE, UNOUT, ABCLS, SKW_FINISH 
C 
C  LOCAL:
      CHARACTER*128 CS2FIL ! added by P. Ryan
      INTEGER nch,I,ILEN,TRIMLEN,rwopen,iok,nc,ircurtmp,ierr
C            iok - variable to check for file access permission
C            ILEN - length of filename cs2fil
C            ircurtmp - temporary save for ircur
      CHARACTER CANS
C            response character
      LOGICAL*4 EX
C            EX  - variable to check for file existence, status
C            opn - variable to check if file is opened
      logical kvexout ! true for a VEX output format
      LOGICAL KSTART,KRWND,KHEAD,KGOT
C          - logical variable to write current observation after WR,WC
      character*128 ctempsk  !temporary filename
      integer*4 renam !C function of type "int"
      integer ptr_ch            !function for vex files 

C
C  PROGRAMMER: NRV
C  LAST MODIFIED: 810818
C   811211 - MAH More than one SCAN lines possible now.
C   840814 - MWH Change handling of EC, display file name on ER
C   840827 - MWH Allow switching schedule files after AB,ER,EC
C   841018 - MWH Allow saving schedule with incomplete selection
C   880311 - NRV DE-COMPC'D
C   880525 - PMR revised for workstation, changed file handling
C   881021 - GAG made PMR's changes work when closing
C   890425 - NRV added saving of SEFD and SNR values
C   890427 - GAG added new parameters to $PARAM
C   890428 - GAG removed $PARAM to its own file called PROUT
C   890531   NRV check schedule file temp name with PID before purging
C   891114 - GAG added logical knewpa and 
C            removed the write sections to own files.
C   891115 - NRV Removed call to SETSC because it's done when
C                returning to SKED.  Changed ISCUN(7) to luskd.
C   891121 - GAG Changed ISCUN(2) to lutmp
C   891121 - GAG added WR and WC. Cleaned the code up getting rid of
C            all the GOTO's.
C   891127 - GAG made the display the screen if its not already
C   891201 - gag made call to rspyn to get response to yes or no
C   900206 - gag change HEAD section from call to whead to hdout
C   900207 - gag changed calls to coout,stout,hdout and soout to chout
C 951018 nrv Remove hollerith constants
C 951116 nrv Add writing $NSKED section
C 990526 nrv New "V" commands.
C 990608 nrv New v??out calls.
C 000523 nrv Add separator lines between VEX sections.
! 2006Sep18 JMGipson.  Removed warning about having to reselect for VEX files.
!    2007Feb23  JMGipson.  Replaced call to RSPYN by call to read_cap_char
! 2007Mar05 JMGipson fixed bug in Feb23 change
! 2012Apr12  JMG. Write out STATWT, SRCWT sections. 
! 2013Apr23  JMG. Write out Broadband sections
! 2014May22  JMG. Set kvlba flag which indicates will be correlated at Socorro. VEX file is slightly modified. 
! 2014Jun03 JMG. kvlba-->kvlba_corr. Added kbonn_corr,and placed $SCHEDULING PARAMs at end of VEX file
! 2020Jun02 JMG. Don't write obsolete head stuff 
C
       cs2fil=" "
       KERR = 1
       kvexout=.false.
       cmdcod=cmdcod_out
       call capitalize(ccorname)
   
       kvlba_corr = ccorname .eq. "VLBA" .or. ccorname .eq. "SOCORRO" 
       kbonn_corr = ccorname .eq. "BONN"
         
       if (cmdcod.eq.'VE') cmdcod='VER'
       if (cmdcod.eq.'VC') cmdcod='VEC'
       if (cmdcod.eq.'VW') cmdcod='VWC'
       if (cmdcod.eq.'VR') cmdcod='VWR'
       if( cmdcod .eq.'VS') cmdcod='VCC'
       if (cmdcod.eq.'VER'.or.cmdcod.eq.'VEC'.or.cmdcod.eq.'VWC'.or.
     >    cmdcod.eq. 'VCC'.or.cmdcod.eq.'VWR') kvexout=.true.

C  1. If the unit command file is open (either a temporary print or
C     save file) give the user a chance to return when the command is 
C     to close.

      if ((ludsp.eq.lufil).and.(cmdcod.ne.'WR'.and.cmdcod.ne.'WC'.and.
     .cmdcod.ne.'VWR'.and.cmdcod.ne.'VWC')) then
        NC =-1 
        ilen = trimlen(ctpfil) 
        DO WHILE (NC.LT.0)
          WRITE(LUSCN,9100) ctpfil(1:ilen)
9100      FORMAT('File ',A,' is open. Continue with Exit? (Y/N)',$)
          call read_cap_char(cans)
          if (cans.eq.'N') then
            return
          else if (cans.eq.'Y') then
            close(ludsp,status='keep')
            write(luscn,"('Closing file ',A)") ctpfil(1:ilen)
            NC=0
          end if 
        END DO
      end if

C  2. On non-abort commands, check for filename and the writing conditions
C     associated with the command.
 
      IF (cmdcod.ne.'AB') THEN   ! non-abort
        IF  ((LINSTR(1)).EQ.0) THEN  ! no name specified
          IF (cmdcod.eq.'WC' .OR.cmdcod.eq.'EC' .OR.
     >        cmdcod.eq.'VWC'.OR.cmdcod.eq.'VEC' .or.
     >        cmdcod.eq.'VCC'.OR.KNEWFI) THEN
            WRITE(LUSCN,9200)
9200        FORMAT('SKCLS01 - No file name has been specified.')
            knewfi=.false.
            RETURN
          ELSE
            CS2FIL = CSKFIL
            CALL null_term(CS2FIL)
          END IF
        ELSE  !  name specified
          ctempsk=" "
          nch=linstr(1)
          CALL hol2char (LINSTR(2),1,nch,ctempsk)
          if (ctempsk(:1).eq.'/') then
            cs2fil = ctempsk
          else if(ctempsk.eq. " ") then
            cs2fil=cskfil
          else
            nch = trimlen(csked)
            cs2fil = csked(:nch)//ctempsk
          end if
          CALL null_term(cs2fil)
        END IF  ! no/name specified

C     Check for file existence. If no access to file, return. If create 
C     command and file exists, return. If replace command and file doesn't
C     exist, return. If file doesn't exist and cannot create, return.

        ilen = trimlen(cs2fil)
        inquire(file=cs2fil,exist=ex)
        if (ex) then
          iok = rwopen(cs2fil)
          if (iok.eq.-1) then
            write(luscn,9210) cs2fil(1:ilen)
9210        format('SKCLS02: You do not have access to the file 'A)
            knewfi = .false.
            return
          end if
          IF (cmdcod.eq.'EC'.OR.cmdcod.eq.'WC' .or.
     .        cmdcod.eq.'VEC'.OR.cmdcod.eq.'VWC') then
            WRITE(LUSCN,9220) CS2FIL(1:ilen)
9220        FORMAT ('SKCLS03 - Error: 'A' already exists')
            knewfi = .false.
            RETURN
          END IF
        ELSE  ! doesn't exist
          IF (cmdcod.eq.'ER'.OR.cmdcod.eq.'WR'.or.
     .        cmdcod.eq.'VER'.OR.cmdcod.eq.'VWR') then
            WRITE(LUSCN,9230) CS2FIL(1:ilen)
9230        FORMAT ('SKCLS04 - Error: 'A' does not exist')
            RETURN
          END IF
          open(lufil,file=cs2fil,status='unknown',iostat=ierr)
          if (ierr.ne.0) then
            write(luscn,9240) cs2fil(1:ilen)
9240        format('SKCLS05 - Cannot create file: 'A)
            return
          end if
          close(lufil,status='KEEP')
        end if

C     If replace command. Give the user a chance to not overwrite the
C     existing file.

        IF (cmdcod.eq.'ER'.OR.cmdcod.eq.'WR'.or.
     .      cmdcod.eq.'VER'.OR.cmdcod.eq.'VWR') then
          NC =-1 
          DO WHILE (NC.LT.0)
            WRITE(LUSCN,9250) CS2FIL(1:ilen)
9250        FORMAT('Replace ',A,'? (Y/N) ',$)
            CALL read_cap_char(cans)
            if (cans.eq.'N') then
              return
            else if (cans.eq.'Y') then
              nc=0
            endif
          END DO
          WRITE(LUSCN,9260) CS2FIL(1:ilen)
9260      FORMAT('Replacing file ',A)
        END IF
 
C  3.0 Check schedule for all sources/stations selected.
 
        ircurtmp = ircur
        CALL CHSKE(KERR)
        IF (KERR.EQ.-1) RETURN

C  4.0 Now write out the schedule file sections. Open a temp file to
C      write to. First delete the temp file if it already exists.
 
C     Set error return. The program will not terminate unless KERR=0.
        KERR = 1
        OPEN(LUTMP,FILE=CTMFI2,STATUS='UNKNOWN')
        CLOSE(lutmp,status='delete')
        OPEN (lutmp,file=CTMFI2,status='NEW',iostat=IERR)
        IF  (IERR.NE.0) THEN  !
          WRITE(LUSCN,9400) IERR
9400      FORMAT('SKCLS06 - Error 'I3' creating temp file.')
          RETURN
        END IF
 
C  Write out the schedule file sections.

        if(.not.kvexout) then ! standard format

C  4.1 The $EXPER section is always written out. Or $GLOBAL for VEX.
 
          CALL exout

! Write out various sections that influence how the schedule is setup.
C  4.2 $PARAM section. $SCHEDULING_PARAMS for VEX.
          CALL PROUT('s')
C  4.85 $OP.  Write if new selection.        
          CALL OPOUT(lutmp,'s') 
          knewop=.false.
! Note that $MAJOR and $MINOR must go after $PARAM and $OP because they supersede them. 
          call major_out(lutmp,'s')    ! Write out ALL the major modes to lutmp
          call minor_out(lutmp,.true.,.false.,'s') ! Write out ALL the minor modes to lutmp, no header row.
          call astro_out(lutmp,.false.,.false.,'s')  !don't number the sources. Only list set sources.
          call twin_out(lutmp,.false.,.false.,'s')  !don't number the stations. Only list set stations.
          call group_out(lutmp,.false.,.false.,0,'s')  !don't number the sources. Only list set sources.
          call srcwt_out(lutmp,.false.,.false.,'s')  !don't number the sources. Only list set sources
          call statwt_out(lutmp,.false.,.false.,'s')  !don't number the stations. Only list them. 
          call broadband_out(lutmp,.false.,.false.,'s')  !don't number the stations. Only list them. 
          call downtime_out(lutmp,'s')   
          call cat_out(lutmp,'s') 

C  4.3 $SOURCES.  Write out if there has been a change
C       or we're creating a new file.
          knewso = .true. 
          IF  (NSOURC.GT.0.AND.(KNEWSO.OR.KNEWFI)) then
            CALL chout('SOURCES  ',csofil)
          end if
 
C  4.4 $STATIONS.  Write if there has been a change, or creating
C       a new file.
         knewst =.true. 
         IF (NSTATN.GT.0.AND.(KNEWST.OR.KNEWFI)) then
            CALL chout('STATIONS ',cstfil)
          end if
 
C  4.5 $CODES section, written if changed or creating a new file.
 
          IF (NCODES.GT.0.AND.(KNEWFR.OR.KNEWFI)) then
            CALL chout('CODES   ',cfrfil)
          end if
 
C  4.6 $SKED. Write if changed or creating a new file.
           IF  (KNEWSK.OR.KNEWFI) THEN
            CALL skout(ierr)
            IF  (IERR.NE.0) RETURN
          END IF
 
C  4.7 $HEAD section.
!         if(.false.) then 
!          IF (NSTATN.GT.0.and.ncodes.gt.0.and.(knewfr.or.knewfi))THEN
!            call chout('HEAD    ',chdfil)
!          END IF
!         endif

C  4.8 $FLUX.  Write if sources or codes changed.
 
          IF ((KNEWFL).or.(knewfi.and.kflux)) CALL FLOUT
C        IF (KNEWFL.or.knewfi) CALL FLOUT
          call proc_out(lutmp,'s') 
C  Write the VEX format file.

        else ! VEX format     
!          call vex_begin_section_comment('$GLOBAL')
          call vglout ! $GLOBAL
!          call vex_end_section_comment(  '$GLOBAL')
!          call vex_begin_section_comment('$EXPER')
          call vexout ! $EXPER
          call vex_end_section_comment(  '$EXPER')       
          call vex_begin_section_comment('$MODE')
          call vmoout ! $MODE
          call vex_end_section_comment(  '$MODE')
  
          call vex_begin_section_comment('$STATION')
          call vstout ! $STATION
          call vex_end_section_comment(  '$STATION')
          call vex_begin_section_comment('$ANTENNA')
          call vanout ! $ANTENNA
          call vex_end_section_comment(  '$ANTENNA')
          call vex_begin_section_comment('$BBC')
          call vbbout ! $BBC
          call vex_end_section_comment(  '$BBC')
          call vex_begin_section_comment('$DAS')
          call vdaout ! $DAS
          call vex_end_section_comment(  '$DAS')
          call vex_begin_section_comment('$FREQ')
          call vfrout ! $FREQ
          call vex_end_section_comment(  '$FREQ')
! no longer need to do this. 
          if(.false.) then         
!          if(.not. kvlba_corr) then 
            call vex_begin_section_comment('$HEAD_POS')
            call vhdout ! $HEAD_POS
            call vex_end_section_comment(  '$HEAD_POS')
            call vex_begin_section_comment('$PASS_ORDER')
            call vpoout ! $PASS_ORDER
            call vex_end_section_comment(  '$PASS_ORDER')
          endif 


          call vex_begin_section_comment('$IF')
          call vifout ! $IF
          call vex_end_section_comment(  '$IF')
          call vex_begin_section_comment('$PHASE_CAL_DETECT')
          call vpcout ! $PHASE_CAL_DETECT
          call vex_end_section_comment(  '$PHASE_CAL_DETECT')

          call vex_begin_section_comment('$ROLL')
          call vroout ! $ROLL
          call vex_end_section_comment(  '$ROLL')

          call vex_begin_section_comment('$SCHED')
          call vscout ! $SCHED
          call vex_end_section_comment(  '$SCHED')
          call vex_begin_section_comment('$SITES')
          call vsiout ! $SITES
          call vex_end_section_comment(  '$SITES')
          call vex_begin_section_comment('$SOURCE')
          call vsoout ! $SOURCE
          call vex_end_section_comment(  '$SOURCE')
          call vex_begin_section_comment('$TRACKS')
          call vtrout ! $TRACKS
          call vex_end_section_comment(  '$TRACKS')

! The $SCHEDULING_PARAMS section contains information for the scheduling program.
         write(luscn,'("SCHEDULING_PARAMS")')
! Set it up.
         call vex_begin_section_comment('$SCHEDULING_PARAMS')
         call fcreate_block(ptr_ch('SCHEDULING_PARAMS'//char(0)))
         call fcreate_def(ptr_ch('SKED_PARAMS'//char(0)))
         call fcreate_literal(ptr_ch(' '//char(0)))
         if(cmdcod .ne. "VCC") then 
! Nowr write it out.  Note similarity to the '.skd' above. only difference is 's'-->'v'
! Write out sections that control how the schedule is setup. 
            CALL PROUT('v')
            CALL OPOUT(lutmp,'v')          
! Note that $MAJOR and $MINOR must go after $PARAM and $OP because they supersede them. 
            call major_out(lutmp,'v')    ! Write out ALL the major modes to lutmp
            call minor_out(lutmp,.true.,.false.,'v') ! Write out ALL the minor modes to lutmp, no header row.
            call astro_out(lutmp,.false.,.false.,'v')  !don't number the sources. Only list set sources.
            call twin_out(lutmp,.false.,.false.,'v')  !don't number the stations. Only list set stations.
            call group_out(lutmp,.false.,.false.,0,'v')  !don't number the sources. Only list set sources.
            call srcwt_out(lutmp,.false.,.false.,'v')  !don't number the sources. Only list set sources
            call statwt_out(lutmp,.false.,.false.,'v')  !don't number the stations. Only list them. 
            call broadband_out(lutmp,.false.,.false.,'v')  !don't number the stations. Only list them. 
            call downtime_out(lutmp,'v') 
            call stat_sefd_out(lutmp,'v') 
            call cat_out(lutmp,'v') 
            call add_file_to_vex("$FLUX",CFLFIL,lutmp)
!          call add_file_to_vex("$STATION",CSTFIL,lutmp)   
            call proc_out(lutmp,'v') 

! This string indicates to sked the end of the section
            cbuf="$END"
            call wrt_param_line(cbuf, lutmp,'v') 
          endif  
! Close it out. 
          call fcreate_literal(ptr_ch(char(0)))
          call vex_end_section_comment(  '$SCHEDULING_PARAMS')
! Back to standard VEX        

        endif ! standard/VEX format

C  4.9 Finally, copy from ORIGINAL schedule file any UN-CHANGED
C      or UNRECOGNIZED sections.
 
        if (.not.kvexout) then ! not VEX
        IF (.NOT.KNEWFI) THEN
          OPEN(luskd,file=CSKFIL,iostat=IERR)
          IF  (IERR.NE.0) THEN
            i = trimlen(cskfil)
            WRITE(LUSCN,9410) IERR,CSKFIL(1:i) 
9410        FORMAT('SKCLS07 - Error ',I3,' opening original schedule'
     .    ' file ',A,' Going to finish anyway.')
          ELSE
C     Write out the unchanged sections from original file.
            CALL UNOUT
            IF (IERR.NE.0) RETURN
            KERR=0
          CLOSE(luskd,status='KEEP')
          END IF
        END IF
        else ! write the VEX file
C         CLOSE(lutmp,status='delete')
          call create_vex(CTMFI2)
C         write(luscn,'("stopping after creating file ",a)') ctmfi2
C         stop
        endif ! not VEX/VEX
 
C  5.0 Close the temporary write file and rename to appropriate name.
 
        CLOSE (lutmp,status='KEEP')
        KERR = renam(CTMFI2,CS2FIL)
c        KERR = rename(CTMFI2,CS2FIL)
        IF  (KERR.LT.0) THEN
          i = trimlen(ctmfi2)
          WRITE(LUSCN,9500) KERR,CTMFI2(1:i),CS2FIL(1:ilen)
9500      FORMAT('SKCLS08 - Error ',I3,' renaming ',A,' to ',A)
          WRITE(LUSCN,9510)
9510      FORMAT('Re-issue EC, ER, WC, or WR command.')
          RETURN
        END IF
        KERR=0
        WRITE(LUSCN,9520) CS2FIL(1:ilen)
9520    FORMAT('SKED output file ',A,' finished.')
 
C  6.0 Give user a chance to change their mind on aborting when changes
C      have been made.

      ELSE  ! cmdcod.eq.'AB'
        if (knewso.or.knewst.or.knewfr.or.knewsk.or.knewpa.or.knewfl
     .  .or.knewop) then
          nc =-1 
          do while (nc.lt.0)
            write(luscn,'(a,$)')
     > 'Changes have been made. Are you sure you want to abort? (Y/N):'
            CALL read_cap_char(cans)
            IF (CANS.EQ.'N') then
              KERR=1
              RETURN
            else if (cans.eq.'Y') then
              nc=0
            endif
          end do
        end if 
      END IF
 
C  7.0 Purge temp files if not WC or WR. Otherwise, write current 
C      observation if display is to the screen.
 
      IF (cmdcod.ne.'WR'.AND.cmdcod.ne.'WC'.and.
     >    cmdcod.ne.'VWR'.and.cmdcod.ne.'VWC') THEN
        CALL delete_temp_files  
        ludsp = luscn
      ELSE   ! continue
        KNEWSO = .FALSE.
        KNEWST = .FALSE.
        KNEWFR = .FALSE.
        KNEWSK = .FALSE.
        KNEWFL = .FALSE.
        KNEWPA = .FALSE.
        IF (KNEWFI) THEN
          CSKFIL=CS2FIL
          KNEWFI = .FALSE.
        END IF
        if ((nobs.gt.0).and.(ludsp.ne.lufil)) then
          kstart = .true.
          krwnd = .false.
          khead = .true.
          irecgo = ircurtmp
          call gtobs(kstart,krwnd,kgot,ierr)
          if ((.not.kgot).or.(ierr.ne.0)) then
            write(luscn,9700)
9700        format('SKCLS09 -  Error going back to current observation')
            return
          end if
          call lscur(khead,isubst,nsubst,90.0)
        end if
      END IF
C
      KERR=0
      RETURN
      END

