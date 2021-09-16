      integer function GTCMD(LINSTQ,cmdcod)
C
C   GTCMD determines the command typed in using minimum-matching
C   Returns a value >0=LCMCOD has a 2-letter code
C                    0=unrecognized
C                   -1=ambiguous
C   NOTE: LINSTQ is modified on return: the command is removed
C         and the rest of the line is shifted left.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cmdcmn.ftni'

! function
      integer iStringMinMatch
C
C  INPUT VARIABLES:
      integer*2 LINSTQ(*)
C               - input string from user, word 1=length
C
C  OUTPUT VARIABLES:
!      integer*2 lcmcod
      character*2 cmdcod
C
C CALLING SUBROUTINES: FSKED
C CALLED SUBROUTINES: LNFCH
C
C History
C 970314 nrv New. Replaced old version, using characters.
C 970317 nrv New command "tape" for tape motion type.
C 970326 nrv New command "xnew" for new source displays.
C 970718 nrv Make "cmdin" longer in case user types some
C            extra long string by mistake.
C 971124 nrv New command "sumout" for compact summary file.
C 990505 nrv New variable max_nccmd for max length of a command.
C            Must be the same value as the character command name max.
C 990524 nrv New command TTAPE
C 990526 nrv New commands VER, VWR, VWC, VEC
C 991027 nrv New command CA
C 991119 nrv New command 1SNR
C 000319 nrv Change TTAPE-->TAPE and TAPE-->MOTION
C 000605 nrv New command ALLOCATION.
C 001003 nrv New command STREAMS
C 020227 nrv New command COMMENT. Don't capitalize the comment.
! 2010Sep20  JMG. Used case-select to select which commands DO NOT have their 
!                 arguments capitalized and added SUMOUT to this list. 
! 2012Oct10  JMG. Don't capitalize "CAT" command line 
! 2013Jan23 JMG. Don't capitalize "VCC" command 
! 2021-05-05 JMG Don't capitalize 'exper' command

C   LOCAL VARIABLES
      integer*2 lintmp(51)
      integer*2 LINCMD(16)
      character*32 cincmd
      equivalence (lincmd,cincmd)
C     IFUNC1 - first function which matches
C     IFUNC  - next function which matches
      integer max_nccmd
      integer nch,ic1,nchar,ifc,iec,nccmd,itest
      integer ichmv_ch,ichmv,ias2b,idummy,ifunc
C
C   INITIALIZED VARIABLES
C
! JMGipson. Alphabetized ccmd list, and rearranged corresponding cmdshort
!
      data max_nccmd/20/
C
C
C  1. Find the first word and pull it off into a separate string.
C     Shift the rest of the command to the left.
C     If it's a source number, insert the '/' command in front.
C
      ifunc=0
      IC1 = 1
      nch = linstq(1)
      CALL GTFLD(LINSTQ(2),IC1,nch,IFC,IEC)
C                   Scan for first word: command name
      IF (IFC.EQ.0) return
C
      NCCMD = IEC - IFC + 1
      if (nccmd.gt.max_nccmd) then
C 040627  ZMM  removed extra comma
        write(luscn,'("Too long command: ", 30a2)') linstq(2:(nch+1)/2)
        gtcmd=0 ! too long is same as unrecognized
        return
      endif
      ITEST = IAS2B(LINSTQ(2),IFC,NCCMD)
C                   Check for a number as the command
      IF (ITEST.NE.-32768.AND.ITEST.GT.0) THEN !new obs
        nch=linstq(1)
        idummy= ICHMV(LINTMP(1),1,LINSTQ(2),1,nch)
        idummy= ichmv_ch(LINSTQ(2),1,'/ ')
        idummy= ICHMV(LINSTQ(2),3,LINTMP(1),1,nch)
        NCCMD = 1
        IC1 = 2
        LINSTQ(1) = LINSTQ(1)+2
        NCHAR = LINSTQ(1)
      ENDIF !new obs

      cincmd=" "
      idummy= ICHMV(LINCMD,1,LINSTQ(2),IFC,NCCMD)
C                   Capitalize command word
      NCHAR = LINSTQ(1)
      CALL GTFLD(LINSTQ(2),IC1,nchar,IFC,IEC)
C                   Find out where the remaining string starts
      if (ifc.gt.0) idummy= ICHMV(LINSTQ(2),1,LINSTQ(2),IFC,NCHAR-IFC+1)
      LINSTQ(1) = NCHAR - IFC + 1
      IF (IFC.EQ.0) LINSTQ(1) = 0
C                   Shift remainder of the command left in LINSTQ
! find minium match.
      ifunc=iStringMinMatch(cmdList,MaxCmd,cincmd)
      gtcmd=ifunc
      if(ifunc .le. 0) then
        if(ifunc .eq. 0) then
           write(luscn,'("Command not found: ",a)') cincmd
        else if(ifunc .eq. -1) then
           write(luscn,'("Ambigous command: ",a)') cincmd
        endif
        return
      endif
     
      cmdcod=cmdshort(ifunc)
      
      select case(cmdlist(ifunc))
! All of the commands below do not have their argument capitalized 
      case("ER", "EC","WC","WR", "VEC","VER","VWR","VWC",    !write various schedule files.
     >   "VCC","FLUX","SOURCE",                            !open catalog files
     >   "PRINTL","PRINTP","SOLVE", "SUMOUT", "UNIT",      !Misc other kinds of files.
     >   "COMMENT","PARAMETER", "CATALOG","EXPER")                 !Miscellaneous
         continue
      case default        
! This is the default:  commands have their args capitalized. 
         nch=linstq(1)
         if (nch.gt.0) CALL HOL2UPPER(LINSTQ(2),nch)
      end select

      END
