      program SKED
C
! Previously was called by a small program. 
C   SKED is the scheduling program for VLBI observations.
C   This main subroutine gets commands from the user and
C   calls the appropriate subroutine to perform the
C   requested function.
C
      use group_mod    ! module containing GROUP definitions and routines
      use twin_mod     ! module containing TWIN_TELESCOPES definitions and routines
      use max_stat_scan  !module for maximum number of  scans per station
      implicit none

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cmdcmn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
      include 'mysql_common.i'      
      include 'cat_name_version.ftni' 
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'

C   Functions
      integer ifunc,gtcmd! function
      INTEGER GETPID
      integer trimlen
! local variables
      integer kerr,ierr
! AEM 20041125 int -> int*2 !only here we've got int*2
      integer*2 nch
      integer ind
      character*2 cmdcod  ! 2-letter code of current command, used for calling
C                           the appropriate subroutine to do the work.  Usually
C                           composed of first two letters of the command name.
C        KERR - General variable for errors returned.
C        fICH  - Index for character counter within input string.
      character*14 cmd    !full name of command

      integer*2 icmdlen
      integer*2 linestq(IBUFQ_LEN)
      character*(ibufq_len*2) cmdline
      equivalence (linestq(2),cmdline)

      character*(ibufq_len*2) cmdline_old          !and length


C               - Input string containing command and parameters.
      character*128 druddum  ! dummy variable to pass to rdctl
      logical kex
     
      character*6 ccat_pid,cpar_pid
      integer itemp
      integer i
          
! Used for tokens
      integer MaxToken
      integer NumToken
      parameter(MaxToken=4)
      character*82 ltoken(MaxToken)   !has to be long for station list.

C
C  History
!   2019Sep03  JMG.  Added implicit none
!
C    811125  MAH    BASELINE COMMAND ADDED
C    82????  MAH    ADDED LOG FILE
C    830424  NRV    CHANGED INTRODUCTORY MESSAGE.
C    830424  NRV    CHANGED RMPAR PARAMETERS TO ADD TYPE-6 CARTRIDGE
C    830425  NRV    REMOVED SYNCHRONIZE COMMAND AND ADDED ELEVATION COMMAND
C    840824  MWH    ALLOW FILE NAME IN RUN STRING, SWITCHING SCHEDULES
C    840827  MWH    INITIALIZE COMMON VARIABLES BEFORE PROCESSING SCHEDULE
C    841105  MWH    MOVE PART OF INITIALIZATION TO SEGMENT
C    880311  NRV    DE-COMPC'D
C    880524  PMR    revised for use on workstation
C    890420  GAG    REMOVED CALL HOL2UPPER AT CALLS TO SUBROUTINES
C    890425  NRV    Added call to VSCAN for VSCAN command
C                   Added call to SEFDSK for SEFD command
C                   Added call to SNRCM for SNR command
C    890516  NRV    Added call to CHCMD for REWRITE command
C    890523  NRV    Added ADD command.
C    890912  gag    added WC and WR commands.
C    891109  gag    added setup call for break subroutine
C    891114  nrv    Removed RMPAR (finally!)    Added FLUX command.
C                   Cleaned up sections, renumbered statements,
C                   changed name of SEGRP to SETSC, and
C                   changed return point after SKCLS to SETSC.
C    891121  gag    changed return from skcls and moved skw_init forward in
C                   sked and skw_finish to skcls
C    891129  NRV    Removed KFIRST
C    900302  gag    moved setsc call to skopn and
C                   added luscn and luusr
C    900307  gag    added calls for the control file
C **
C    911026  nrv    add linestq to param call for OP command
C    930119  nrv    Started merge of sked/autosked
C    930120  nrv    Changed 2h's to characters
C    930323  nrv    Added SUBCON command and call to subdis
C    930323  nrv    Changed to subroutine, called by C main program
C    930430  nrv    Removed X (finally)
C    940202  nrv    Initialize skversion here.
C    950601  nrv    New command UNTAG.
C    950628  nrv    New command WEIGHT.
C 951018 nrv Remove '2h'
C 951124 nrv Change RDCTL call for new catalog names
C 960226 nrv change rdctl call to add label script
C 960403 nrv Add rec_cat to RDCTL call
C 960513 nrv New release date.
C 960516 nrv New release date.
C 960627 nrv New release date.
C 960716 nrv New release date.
C 970314 nrv New command EARLY so it can be set by station.
C 970317 nrv New command TAPE to set up tape_motion_type.
C 970326 nrv New command XNEW for new source displays
C 970328 nrv Add station_cat to RDCTL
C 971124 nrv New command SUMOUT to make compact summary info file.
C 990524 nrv New command TTYPE to let user set tape type and density.
C 990524 nrv Ignore commands that start with "*" for comments.
C 990526 nrv Add "V" wr,er,wc,ec commands.
C 990915 nrv Replace REIO with READ + CHAR2HOL.
C 991027 nrv Add CATALOG command and call to CATCMD.
C 991108 nrv Add modes_description_cat to RDCTL.
C 991117 nrv Add cat_program_path to RDCTL.
C 991119 nrv New SNR_1 command.
C 991122 nrv Open the PID file written by the catalog program, and
C            read the PID, then kill the program.
C 000326 nrv Add par_program_path to RDCTL. Kill program at end.
C 000605 nrv New command ALLOCATION for tape allocation.
C 001003 nrv Add STREAMS command.
C 020227 nrv Add call to COMCMD for COMMENT command.
C
C 2003Jun24  JMG Modified to be able to set new optimization parameters.
C 2004Mar05  JMG Modified to be able to set up minor modes.
! 2004Oct08  JMG Made so that catcmd is passed character string.
! 2004Oct08  JMG Make trailing part of cmdline blank.
! 2006Jun13  JMG. Removed all ldisk2file from rdctl call and put in common.
!                 Removed all lmysql from rdctl and put in common.
! 2006Oct18. JMG. Fixed some minor problems.
! 2006Nov30.  Fixed call to rdctl.
! 2007Nov20   Added master command
! 2008Mar14  Added ability to use linux "readline" utility
! 2008Jun06 JMG. Changed cmdcod "AU" to "SH"  (shift)
!             Added new command: autosked.
! 2008Jun19 JMG. Slight formatting changes
! 2010Mar20 JMG. Removed baseline (BSELN) command. 
! 2010Apr12 JMG. Added Statwt, SrcWt. Also can reference by full command name.
! 2013Jan23 JMG. Added VCC option 
! 2017Dec04 JMG. moved opening logfile to wrlog.f 
! 2018Mar29 JMG. mysql is no longer hardwired in, but is in skedf.ctl
! 2019Mar14 JMG. Added exper command
! 2020Jun06 JMG. Removed reference to HP and authors 
! 2020Jun10 jmg. Changed argument of 'solve' from linestq to cmdline. 
! 2020Oct13 JMG. Changed argument of results from linestq to cmdline
! 2021-05-07 JMG Changed arg list to 'random' command

!
C   0. Opening message
C
      include 'sked_date.ftni'
      luscn = STDOUT
      ludsp = luscn
      luusr = STDIN
      ccat_pid = ' '
      cpar_pid = ' '

      write(luscn,'("sked version ", a)')  skversion
 
C
C   Get process ID [this is actually a C library routine]
C
      PID = GETPID()
      call pid_str(cpid,pid)
      call null_term(cpid)
!      call init_mysql()
C
C   Call default initialization routine for control file information.
C   Then call the routine that reads the control file(s).
C
    
      call dectl
   
      call  sked_rdctl(luscn,                                      
     >   source_cat,station_cat,antenna_cat,position_cat,           
     >   equip_cat, mask_cat,   freq_cat,   rx_cat,                
     >   loif_cat,  modes_cat,  modes_description_cat,  rec_cat,
     >   hdpos_cat, tracks_cat,flux_cat,flux_comments,
     >   cmaster_dir, cat_program_path, par_program_path,          
     >   csked, ctmpnam,cprtlan,cprtpor,cprttyp,cprport)
     ! 
      call scctl  !set up scratch file names
     
C

C  3. Initialize and prompt for schedule file name.


! Get the schedule file if there is one. 
      if ( iargc() .le. 0 ) then
         cmdline = ' '
      else
        call getarg(1,cmdline)
      end if
      nch = trimlen(cmdline)

      cmdline_old="wh"

200   continue
      
      call minor_init
      call major_init
      CALL SKINI
      linestq(1) = nch ! nch=0 forces a prompt
    

C    2. Get the schedule file name first.
210   continue
      cmdcod=" "
      cskfil = csktmp
      if (nch.eq.0) then ! do not have a file name
        WRITE(LUSCN,'(a,$)')
     > 'Schedule file (.skd assumed, blank if none, q or :: to quit): '
        read(luusr,'(a)') cmdline
        if(cmdline.eq. "::".or. cmdline .eq. " ::" .or. 
     >    cmdline.eq. "q".or. cmdline .eq. "Q") goto 9990    !abort
        nch=trimlen(cmdline)
        linestq(1)=nch
      endif
      CALL WRLOG(cmdline)
      IF (nch .EQ.0) THEN  !default empty file
        KNEWFI = .TRUE.
      else ! file name, append .skd
        ind=index(cmdline,'.')
        if(ind .eq. 0) cmdline(nch+1:nch+4)=".skd"
        linestq(1)=nch+4
      END IF

C  4. Open schedule file and read it in.

      IERRCM = 0
      INUMCM = 0
      CALL SKOPN(cmdline,ierr)    
    
! Do a quick check to make sure that things are setup at the stations.
      call check_stations(ierr)
      if(ierr .ne. 0) then
         write(*,'(a)') "Fix problem with stations and try again."
         stop        
      endif
      
      call check_sources(ierr)
      if(ierr .ne. 0) then
         write(*,'(a)') "Fix problems with sources and try again."
         stop
      endif       

      IF (IYRCUR(1).NE.-1) GOTO 700
      GOTO 800

C  6. The main prompt

700   continue
!      write(*,*) "fsked 700:   kcat_freq ", kcat_freq
      cmdline=" "
      linestq(1) = 0
      call read_cmdline(luscn,luusr,cmdline)     

      if(cmdline .eq. "!!") then    ! repeat last command
        cmdline=cmdline_old
      endif   
      linestq(1)=trimlen(cmdline)
      cmdline_old=cmdline

      IF (linestq(1).EQ.0) goto 700
  
      CALL WRLOG(cmdline)
      if(cmdline(1:1) .eq. '*') goto 700 ! comment
      ifunc = GTCMD(linestq,cmdcod)    
      cmd=cmdlist(ifunc)
      icmdlen=linestq(1)         !get the length of the command.
! if there is no argument, set the rest of the line to blanks.
!     Set the end of the command to blank.
      cmdline(icmdlen+1:ibufq_len*2) = ' '
      if(ifunc .le.0) goto 700 
C
C********************************************************************
C  7. We have a legal, recognized, unambiguous command.  Now decode
C     the rest of the line (if any), or do the function if there are
C     no parameters for this command.

750   continue   
   
      select case(cmdlist(ifunc))
      case("?","HELP")
        call Helpsk(linestq)
      CASE("/")            !New observation.
        CALL NEWCM(cmdline,0)
      case("!")            !shell
        CALL CSHELL
      case("^","BACK","CURRENT","LIST","NEXT","PREVIOUS")
        CALL LICMD(linestq,cmdcod) 
      case("ABORT") 
        goto 810        
      case("ADD", "CHECK","REMOVE","REWRITE","SHIFT","TAGALONG","UNTAG")
        CALL CHCMD(linestq,cmdcod)

      case("ALLOCATION") 
        CALL ATAPE(linestq,luscn,ludsp)
 
      case("ASTROMETRIC") 
        call astro_cmd(cmdline)
      case("AUTOSKED") 
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
        if(NumToken .eq. 0) then
           cmdline=" _ NO START "
        else if(NumToken .eq. 1) then
           cmdline=ltoken(1)(1:trimlen(ltoken(1)))//" NO START "
        else if(NumToken .eq. 2) then
           cmdline=ltoken(1)(1:trimlen(ltoken(1)))//" NO "//
     >       ltoken(2)(1:trimlen(ltoken(2)))
        else
          write(ludsp,'(A)') "Too many arguments!"
          goto 700
        endif
        linestq(1)=trimlen(cmdline)
        call nextc(linestq)
!     CASE("BACK")        ! see ^  above
      CASE("BESTSOURCE") 
        call bestsources(cmdline)
      CASE("BROADBAND") 
        call broadband_cmd(cmdline)
      case("CATALOG")
!         call tcl_cat_cmd(cmdline)     
        call cat_cmd(cmdline)          
!     case("CHECK")      !see ADD above

      CASE("COMMENT")
        CALL COMCMD(linestq)
      CASE("COVERAGE")
        call compute_coverage()
!     CASE("CURRENT")        ! see ^  above
      CASE('DELETE')
        CALL DELCM(linestq,'DE')
      case("DOWNTIME")
        call downtime(cmdline)
  
      CASE("EARLY")
        CALL SEARL(linestq,luscn,ludsp)
      case("EC")
        goto 800
      case("ELEVATION") 
        CALL SELEV(linestq,luscn,ludsp)
      case("ER") 
        goto 810
      CASE("EXPER")
        call exper_cmd(cmdline)

      case("FILL")
        CALL fillcmd(cmdline)

      case("FLUX") 
        CALL FLCMD(linestq)
      case("FREQUENCY") 
        CALL FRCMD(linestq)
!      case("HELP")       ! See "?" above
!     case("LIST")        ! see ^ above 

      case("GROUP")
        call group_cmd(cmdline)

      case("MAJOR") 
        call major_cmd(cmdline)
      case("MASTER") 
        call master_cmd(cmdline)
        
      case("MAX_STAT_SCAN")
        call max_stat_scan_cmd(cmdline)
      case("MAX")
        call mxlis
      case("MEDIA", "TAPE")
        CALL TTAPE(linestq,luscn,ludsp)
      case("MINOR")   
        call minor_cmd(cmdline)
      case("MONITOR")
        call monitorsources(cmdline)
      case("MODIFY") 
        IF ((NOBS.LE.0).AND.(IRCUR.LE.0)) THEN
          WRITE(LUSCN,'(a)') "NO OBSERVATIONS LISTED, CAN'T MODIFY"
        ELSE
          CALL MODCU
        END IF
      case("MOTION")
        CALL STAPE(linestq,luscn,ludsp)
      case("MUTUALVIS","SITEVIS")
        CALL MUVIS(linestq,cmdcod)
!     CASE("NEXT")        ! see ^  above
      CASE("NOW")
        call now_cmd(cmdline)
      case("OPTIMIZATION") 
        call opcmd(linestq)
      case("PARAMETERS")
        CALL PRCMD(linestq)
      case("PID")
        write(luscn,'("Process ID = ",a)') cpid   !Display Process ID
!     CASE("PREVIOUS")        ! see ^  above
      case("PRINTP", "PRINTL")
        CALL PRNCM(linestq,cmdcod)
      case("QUIT")       
        call delete_temp_files
        stop
!     case("REMOVE")   !See ADD above  
      case("RANDOM")   !Schedule random observation 
         call random_cmd(cmdline)
      case("RESULT") 
        CALL RESULT(cmdline)
!     case("REWRITE")  !See ADD above 
!     case("SITEVIS") !See MUTUAL above
      case("SCAN") 
        CALL SSCAN(linestq,'s')
!     case("SHIFT")   !See ADD above  
      case("SNR")
        CALL SNRCM(linestq,'s',' ')
      case("1SNR")
        CALL SNRCM(linestq,'s','1')
      case("SOLVE")
        call solve(cmdline)
      case("SOURCES")
        call source_cmd(cmdline) 
      case("SRCWT")
        call srcwt_cmd(cmdline)
      case("STATIONS")
!        CALL STCMD(linestq)  
        CALL STCMD(cmdline)  ! KLB Sept 2017 - changed from linestq to cmdline 
      case("STATWT")
        call statwt_cmd(cmdline)
      case("STREAMS")
        call stream_cmd(linestq)
      case("SUBCON")
        CALL SUBDIS(linestq)
      case("SUMMARY") 
        CALL SUMCM(linestq)
      Case("SUMOUT")
        CALL SUMOUT(linestq)
!     Case("TAPE")   !see media above. 
!     case("TAG")   !See ADD above  
      case("THIN")
         call thin_cmd(cmdline)
      case("TIMELINE")
        CALL TMLIN(linestq)
      case("TWIN_TELESCOPES")
        call twin_cmd(cmdline)
      CASE("UNIT")
        CALL LUCMD(linestq)
!     case("UNTAG")   !See ADD above 
      case("VEC","VWC","VCC")
        goto 800
      case("VER","VWR")
        goto 810
      case("VLBA") 
        call vlmode(linestq)
      case("VSCAN")
        CALL VSCAN(linestq)
!      case("VWC")  see VEC
!        goto 800
      case("WC")
        goto 800
      case("DISPLAY_WRAP") 
        do i=1,nstatn
           write(*,'(a,1x,a2)') cstnna(i), lcblcur(i)
        end do
    
          
      case("WHATSUP") 
        CALL NEXTC(linestq)
      case("WR")
        goto 810
      CASE("XNEW")
        CALL XNCMD(linestq)
      CASE("XLIST")
        CALL XLCMD(linestq)
      case default
        goto 700
      end select 
      goto 700             !get the next command.     
      

800   CONTINUE            ! Come here if writing a new file.
      knewfi=.true.
810   continue            ! Come here if writing an exisiting file. 
       
      CALL SKCLS(cmdcod,linestq,KERR)
      nch=0
      if(cmdcod .eq. 'AB'.and.KERR .eq. 0) then
         goto 200
      endif

      IF (KERR.NE.0.OR.cmdcod.EQ.'WR'.OR.cmdcod.EQ.'WC'
     .             .or.cmdcod.EQ.'VR'.OR.cmdcod.EQ.'VW') GOTO 700
C       if Y then continue same schedule
      GOTO 200
9990  CONTINUE
      inquire(file=ccat_pid_file,exist=kex)
      if (kex) then ! shut down catalog program
        OPEN (lucat,status='unknown',file=ccat_pid_file)
        read(lucat,'(a)',iostat=ierr,end=990,err=991) ccat_pid
990     continue
991     continue
        if (ccat_pid.ne.' ') call skcat(ccat_pid,cat_program_path)
        CLOSE (lucat,status='delete')
      endif ! shut down catalog program
      inquire(file=cpar_pid_file,exist=kex)
      if (kex) then ! shut down parameter program
        OPEN (lucat,status='unknown',file=cpar_pid_file)
        read(lucat,'(a)',iostat=ierr,end=992,err=993) cpar_pid
992     continue
993     continue
        if (cpar_pid.ne.' ') call skpar(cpar_pid,par_program_path)
        CLOSE (lucat,status='delete')
      endif ! shut down catalog program
      WRITE(LUSCN,9900)
9900  FORMAT('SKED TERMINATED')
      END

