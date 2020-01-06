      SUBROUTINE minor_cmd(cmdline)
! Set, list minor mode commands.
C History:
!  2004Mar08  JMGipson First version
!  2007Feb10  JMG. Pass MaxRval, MaxLVal to parsing routine.
!  2009Oct29   JMG. Minor print output change
!  2010Mar10  JMGipson. Removed obsolete srcfloor and TapeWaste
!  2012Apr12  JMGipson. Modified Statwt option to make it more like Astro.
!             Also added SrcWt
!  2015Nov13  JMGipson. Added kCovar for covariance optimization. 

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/statn.ftni'   !contains nstatn.
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include 'minor.ftni'
      include '../skdrincl/sourc.ftni'
      include 'statwt.ftni'

C Input
      character*(*) cmdline

! functions
      integer iStringMinMatch
      integer trimlen
      integer iwhere_in_string_list
! functions
      integer MaxToken
      integer NumToken, iToken
      integer NumLeft
      parameter(MaxToken=15)
      character*30 ltoken(MaxToken)
      integer icmd
      character*30 lcmd

C   LOCAL VARIABLES
      integer NumRVal,NumLVal
      integer MaxRVal,MaxLVal
      parameter (MaxrVal=5,MaxLVal=11)
      real rvalues(MaxRVal)
      Character*30 lvalues(MaxLVal)
      integer i
      integer NumUsed
      logical kall
      logical khead     !list header on listing.
      integer istat !counters
      integer ival
      integer iptr
      integer NumStatTest

! for SRCEVN mode.
      characteR*6 lEvnDist(4)

! valid command list.
      integer ilist_len
      parameter (ilist_len=16)
      character*12 list(ilist_len)
      character*60 lsyntax(ilist_len)
      character*50 lhelp(ilist_len)

      data list/
     > "LIST",      "ASTRO",    "BEGSCAN",    "COVAR",  "ENDSCAN",
     > "LOWDEC",    "NUMLOEL",  "NUMRISESET", "NUMOBS", "SKYCOV", 
     > "SRCEVN",    "SRCWT",    "STATEVN",    "STATIDLE","STATWT", 
     > "TIMEVAR"/
      data lEvnDist/"NONE","UPTIME","SQRT","EVEN"/

      data lsyntax/
!       123456789x123456789x123456789x123456789x123456789x123456789x
     >"List       [All]                                           ",
     >"Astro      [WtMode={Abs|Rel}] [Wt]                         ",
     >"BegScan    [WtMode={Abs|Rel}] [Wt]                         ",
     >"Covar      [WtMode={Abs|Rel}] [Wt]                         ",
     >"EndScan    [WtMode={Abs|Rel}] [Wt]                         ",
     >"LowDec     [WtMode={Abs|Rel}] [Wt]                         ",
     >"NumLoEl    [WtMode={Abs|Rel}] [Wt] [El_thres]              ",
     >"NumRiseSet [WtMode={Abs|Rel}] [Wt]                         ",
     >"NumObs     [WtMode={Abs|Rel}] [Wt]                         ",
     >"SkyCov     [WtMode={Abs|Rel}] [Wt]                         ",
     >"SrcEvn     [WtMode={Abs|Rel}] [Wt] [Mode={NONE|EVN|SQRT}]  ",
     >"SrcWT      [WtMode={abs|Rel}] [Wt]                         ",
     >"StatEvn    [WtMode={Abs|Rel}] [Wt] [Mode={NONE|EVN|SQRT}]  ",
     >"StatIdle   [WtMode={Abs|Rel}] [Wt]                         ",
     >"StatWT     [WtMode={Abs|Rel}] [Wt] [FFFFT TTFFF ...]       ",
     >"TimeVar    [WtMode={Abs|Rel}] [Wt]                         "/

      data lhelp/
     > "list options in use or ALL options",
     > "weighting of astrometric sources",
     > "prefer scans that start soon",
     > "prefer scans that minimize covariance", 
     > "prefer scans that end soon",
     > "prefer low-dec sources",
     > "prefer scans with elevation below El_thres",
     > "prefer scans with rising/setting sources",
     > "prefer scans with more observations",
     > "prefer scans with better sky coverage",
     > "modify distribution of observations of sources", 
     > "prefer scans involving certain sources",
     > "modify distribution of observations of stations",
     > "minimize sation idle time",
     > "prefer scans including some stations",
     > "prefer scans with equal end time"/

      khead=.true.
      call capitalize(cmdline)
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      itoken=1
      if(NumToken.eq.0 .or. Numtoken.eq.1 .and. ltoken(1).eq."?") then
        do i=1,ilist_len
          write(luscn,'(a)') lsyntax(i)//lhelp(i)
        end do
        return
      endif

! parse the token, see if it is valid.
      NumLeft=NumToken
      Do while(itoken .le. NumToken)
        icmd=istringMinMatch(list,ilist_len,ltoken(itoken))
        if(icmd .eq. 0) then
           write(luscn,'(a,a)') "minor_cmd: Not found: ",ltoken(itoken)
           return
        else if(icmd .le. 0) then
           write(luscn,'(a,a)') "minor_cmd: Ambigious  ",ltoken(itoken)
           return
        endif

        lcmd=list(icmd)
        itoken=itoken+1

! see if we want help.
        if(itoken .le. NumToken .and. ltoken(itoken) .eq. "?") then
          i=iwhere_in_string_list(list,ilist_len,lcmd)
          if(i .ne. 0) then
            write(luscn,'(a)') lsyntax(i)//lhelp(i)
          else
            return
          endif
          numUsed=2
          goto 90
        endif

! some command besides help. Check each one.
!        write(*,*) "Command: ",lcmd 
        if(lcmd .eq. "LIST") then
           kall=.false.
           NumUsed=1
           if(itoken .le. NumToken) then
             if(ltoken(itoken) .eq. "ALL") then
               NumUsed=2
               kall=.true.
             endif
           endif
           call minor_out(ludsp,kall,khead,'d')
           goto 90
        else if(lcmd .eq. "ASTRO") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kAstro,kAstroNorm,rAstroWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "BEGSCAN") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kBegScan,kBegScanNorm,rBegScanWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params
        else if(lcmd .eq. "COVAR") then      
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kCovar,kCovarNorm,rCovarWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "ENDSCAN") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kEndScan,kEndScanNorm,rEndScanWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "LOWDEC") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kLowDec,kLowDecNorm,rLowDecWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "NUMLOEL") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kNumLoEl,kNumLoElNorm,rNumLoElWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)

           if(NumRval .eq. 0 .and.  NumLval .eq. 0) goto 90 !just turning on or off
           if(NumRval .ne. 1 .or. NumLVal .ne. 0) goto 200  !wrong number of params.

           if(rvalues(1) .gt. 30 .or. rvalues(1) .lt. 0) then
             WRITE(LUSCN,*)
     >         " Minor_cmd: LoEl angle is outside limits (0-30): ",
     >          rvalues(1)

             return
            endif
            rLoel=rvalues(1)*deg2rad

        else if(lcmd .eq. "NUMOBS") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kNumObs,kNumObsNorm,rNumObsWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "NUMRISESET") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kNumRiseSet,kNumRiseSetNorm,rNumRiseSetWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)

           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "SKYCOV") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kSkyCov,kSkyCovNorm,rSkyCovWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "SRCEVN") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kSrcEvn,kSrcEvnNorm,rSrcEvnWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .eq. 0 .and. NumLval .eq. 0) goto 90 !just turning on or off

           if(NumRval .ne. 0 .or. NumLval .ne. 1) goto 200  !wrong number of parameters.
           icmd=iStringMinMatch(lEvnDist,4,lvalues(1))
           if(icmd .eq. 0) then
              WRITE(LUSCN,'(4(a," "))')  
     >           " Minor_cmd: SRCEVN must be one of:",
     >           (lEvnDist(i),i=1,4)
              return
            else
              iSrcEvnMode=icmd-1
            endif    

        else if(lcmd .eq. "SRCWT") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kSrcWt,kSrcWtNorm,rSrcWtWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .and. NumLval .ne. 0) goto 200 !just turning on or off

        else if(lcmd .eq. "STATEVN") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kStatEvn,kStatEvnNorm,rStatEvnWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)

           if(NumRval .eq. 0 .and.  NumLval .eq. 0) goto 90 !just turning on or off
           if(NumRval .ne. 0 .or. NumLval .ne. 1) goto 200  !wrong number of params

           icmd=iStringMinMatch(lEvnDist,4,lvalues(1))
           if(icmd .eq. 0) then
             WRITE(LUSCN,*)" Minor_cmd: StatEvn must be one of:",
     >           (lEvnDist(i),i=1,4)
               return
           else
               iStatEvnMode=icmd-1
           endif

        else if(lcmd .eq. "STATIDLE") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kStatIdle,kStatIdleNorm,rStatIdleWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params

        else if(lcmd .eq. "STATWT") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kStatWt,kStatWtNorm,rStatWtWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)

           if(NumRval .eq. 0 .and.  NumLval .eq. 0) goto 90 !just turning on or off
! read in all of the "On/OFF" switches.
           if(NumRVal .ne. 0) goto 200

           istat=1
           ival=1
           iptr=1
           if(nstatn .eq. 0) then
              NumStatTest=max_stn
           else
              NumStatTest=nstatn
           endif
           if(lvalues(1) .ne. " ") then
             write(*,'(a)')
     >        "Minor_cmd: This usage of minor statwt is obsolete."
             write(*,'(a)') 
     >      "    Please use new StatWt command to set station weights."
           endif
           do while(ival .le. NumLval .and. istat .le. max_stn)
             do while(lvalues(ival)(iptr:iptr) .ne. " ")                
               if(lvalues(ival)(iptr:iptr) .eq. "T") then
                 statwt(istat)=1.
               else if(lvalues(ival)(iptr:iptr) .eq. "F") then
                 statwt(istat)=0. 
               else if(lvalues(ival)(iptr:iptr) .ne. " ") then
                 goto 200      !found a character that was not "T","F", or space.
               else
                 if(nstatn .ne. 0) then
                    write(luscn, *)'MINOR_CMD: Not enoughStatWt Flags'
                    return
                 else
                   istat=Max_stn
                 endif
               endif
               istat=istat+1
               iptr=iptr+1
             end do
             ival=ival+1
             iptr=1
           end do

        else if(lcmd .eq. "TIMEVAR") then
           call minor_cmd_parse(ltoken(itoken),NumLeft-1,NumUsed,
     >          kTimeVar,kTimeVarNorm,rTimeVarWt,
     >          list,ilist_len,rvalues,NumRVal,MaxRVal,
     >          lvalues,NumLval,MaxLval)
           if(NumRval .ne. 0 .or. NumLVal .ne. 0) goto 200  !wrong number of params      
        endif
90      continue
        iToken=iToken+NumUsed         !The 1 is for the command itself.
        NumLeft=NumLeft-NumUsed
100     continue
      ENDDO
      RETURN

200   continue
      write(luscn,'(a,a)')
     >  "Minor_cmd: Unknown or wrong parameters for minor mode: "
      write(luscn,*) cmdline(1:trimlen(cmdline))
      return

      END


