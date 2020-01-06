      SUBROUTINE HELPSK(LINSTQ)
C
C  HELP types information on the user's terminal
C
      include '../skdrincl/skparm.ftni'
C
C     INPUT VARIABLES:
      integer*2 LINSTQ(*)
C               - input string, word 1=length
C
      include 'skcom.ftni'
      include 'cmdcmn.ftni'
C
C     CALLING SUBROUTINES: SKED
C     CALLED SUBROUTINES: GTCMD
C
      integer trimlen
      integer gtcmd
C  LOCAL VARIABLES
      character*2 ccode
      integer ifunc
      character*1 lq
      character*2 lqc
      integer i

      character*80 ctemp

C
C  PROGRAMMER: NRV
C     LAST MODIFIED: 830425 to add new commands
C                    841105 MWH to add new commands
C                    850605 MWH added arg to GTCMD call
C                    880310 NRV DE-COMPC'D
C                    880405 NRV UPDATED INDIVIDUAL COMMAND HELP INFO
C     890224  GAG MODIFIED STRUCTURE
C     890426 GAG ADDED MINSCAN,MAXSCAN,MODSCAN,WIDTH UNDER PARAMETERS
C     890427 GAG ADDED BWSCAN,SNRSCAN,CHANSCAN,VIS UNDER PARAMETERS
C     890428 GAG ADDED CONFIRM, BASESCAN AS PARAMETER
C     890501 NRV Added SNR, SEFD, VSCAN command help.
C     890516 NRV Added help for REWRITE, changed TAG, LIST
C     890517 GAG Added CORSYNCH as parameter
C     890519 NRV Added IGNORE to TAGALONG
C     890523 NRV Added ADD command
C     890531 NRV Removed IGNORE, added SUM keywords
C     890816 GAG Added to Parameter listing
C     891114 NRV Add FLUX command
C     891121 GAG Added WC and WR command
C     891127 GAG added to Parameter listing option general
C     891128 gag moved the listing around
C     891129 gag removed BASESCAN,SNRSCAN,BWSCAN, and CHANSCAN
C     891208 NRV Add SNR parameter, change SNR command
C     891214 NRV Changed FLUX command to, add optional catalog
C     900206 gag added PARAMETER option ALL, removed LENGTH 
C     900302 gag changed parameter listing from its var. to comment
C     910619 NRV Updated
C     930225 nrv implicit none
C     930324 nrv Changed WH syntax. Added SUBCON command.
C     930602 nrv Add options to OP command
C     940215 nrv Add file name option to SOURCE SELECT
C     940218 nrv Add DIST to SUMMARY command
C     950710 nrv Add UNTAG and WEIGHT commands. Leave VLBA command unadvertised.
C 960510 nrv Update FREQ
C 970307 nrv Add VLBA, MAX.
C 970328 nrv Add TAPE, EARLY, XNEW
C 990913 nrv Add new commands.
C 991109 nrv Add CATALOG command.
C 991119 nrv Add 1SNR command.
C 000319 nrv Change TAPE-->MOTION, TTAPE-->tape.
C 001004 nrv Add ALLOCATION, STREAM.
! 13Feb2003 JMG slight change in formatting.
! 2006Nov13 JMG. Added K5 to tape types.
! 2010Jan04 JMG. Added Media command
C
C     1. First decode the rest of the input line, if any.
C        If there is nothing there, write out the one-line-per-command
C        long message.  If there is something, decode the name.
C
      if(.false.) then
      open(99,file="Help.tmp")
      lq='"'
      lqc='",'
!      lqcq'","'
      do i=1,MaxCmd
        write(99,'(a,i4,a)') "!CMD: ",i," ----------"
        write(99,'(5x,">",a1,a,a2)')lq,cmdshort(i),lqc
        write(99,'(5x,">",a1,a,a2)')lq,cmdlist(i),lqc
        write(99,'(5x,">",a1,a,a2)')lq,cmdbrief(i),lqc
        write(99,'(5x,">",a1,a,a)')lq,cmdsyntax(i)(2:60),lqc
      end do
      close(99)
      endif

      CALL hol2char (LINSTQ(2),1,linstq(1),ctemp)

      IF (ctemp .eq. " ") THEN ! if only a ?
         write(luscn,'(1x,a,1x,a,2x,a,1x,a)')
     >   (cmdlist(i),cmdbrief(i),i=1,maxcmd)
         WRITE(LUSCN, '(a)')
     >    "Commands may be abbreviated so long as they are unique"
C
      ELSE ! if specific question 
        ifunc = GTCMD(LINSTQ,cCODE)
! first off do some special commmands
        if(ifunc .eq. 0) then
          WRITE(LUSCN,'(a)')  "?01 - Unrecognized command. "//
     >     "Type ? for a list oflegal commands."
          return
        ELSE if(ifunc .eq. -1) then
          WRITE(LUSCN,'(a)') "Ambiguous command. Use more letters."
          return
        Else if(cmdlist(ifunc) .eq. "PARAMETERS") then
          write(luscn,'(a)') ' PARAMETER names: DURATION,'//
     >     'CALIBRATION,PREOB,MIDOB,IDLE,POSTOB,SUBNET,FREQUENCY'
          write(luscn,'(a)')
     >   ' Use PARAMETER LIST ALL for the names of settable parameters.'
        ELSE IF (cmdlist(ifunc).eq."SUMMARY") then
          WRITE(LUSCN,'(a)') " SUMMARY [<range> [<source> [<subnet>"//
     >      "[STATS|LINE|XYAZEL|POLAZEL|EL|AZ|BASELINE|HIST|SNR|FILE"//
     >      "|COVERAGE|DIST [<xmin> <xmax> <ymin> <ymax>]]]]]"
        else if(cmdlist(ifunc) .eq. "TAPE" ) then
           write(luscn,"(
     >       ' Mk3/4 : TAPE [<station> <THICK|THIN> [<HIGH|LOW>]]',/,
     >       ' S2,K4 : TAPE [<station> <length in minutes>',/,
     >       ' MARK5A: TAPE [<station> MARK5A]',/,
     >            ' K5:     TAPE [<station> K5]')" )                                   
        else if(cmdlist(ifunc) .eq. "MEDIA" ) then
           write(luscn,"(
     >       ' Mk3/4 : MEDIA [<station> <THICK|THIN> [<HIGH|LOW>]]'/
     >       ' S2,K4 : MEDIA [<station> <length in minutes>'/
     >       ' MARK5A: MEDIA [<station> MARK5A]'/
     >       ' K5:     MEDIA [<station> K5]')"       )          
        else
           write(luscn,'(1x,a,1x,a)')
     >       cmdlist(ifunc)(1:trimlen(cmdlist(ifunc))),cmdsyntax(ifunc)
        endif
                                                       ! write range help command.
        if(ifunc .gt. 0 .and.
     >     cmdlist(ifunc).eq."ADD"       .or.
     >     cmdlist(ifunc).eq."SHIFT" .or.
     >     cmdlist(ifunc).eq."CHECK"     .or.
     >     cmdlist(ifunc).eq."DELETE"    .or.
     >     cmdlist(ifunc).eq."LIST"      .or.
     >     cmdlist(ifunc).eq."REMOVE"    .or.
     >     cmdlist(ifunc).eq."SUMMARY"   .or.
     >     cmdlist(ifunc).eq."TAGALONG"    ) then
            WRITE(LUSCN,9291)
9291  FORMAT(' <range> is ALL or <start>-<stop> or <start>#<number>',/,
     >12x,
     > ' <start>,<stop> are yydddhhmmss or ^(top), .(current), *(end)',/,
     > 12x,' or first, last, begin, end, ')
         endif
      endif
      return
      end
