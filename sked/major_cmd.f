!  2004Mar08F  JMGipson First version
      SUBROUTINE major_cmd(cmdline)
! Set, list major mode commands.
C History:
! 2008May22  Moved many parameters from $PARAM to $MAJOR
!            Wrote associated code.
! 2008May27  Made room for obsolete names,e.g.,  LAST_HRS=TIME_WINDOW
! 2009Mar03  Added lcmd_caps, rearranged data statements for lcmd_list, lhelp
! 2009Oct01  Because of previous limit on token size of 20, could only have 10 stations in subnet.
! 2000Nov05  Fixed bug found by AEM
! 2010Mar01  Modified so that "Major sub _"  and "Major sub all" turns on all stations.
!            Done for consistency with other sked commands. 
! 2010Mar24  Wasn't correctly setting MAXSLEW
! 2010May19  Wasn't correctly settimg FILLMINTIME
!            Took opportunity to reorganize parsing.
! 2011Apr25  Added MaxAngle 
! 2014Jan22  Was not correctly reading in MinAngle
! 2017Feb14  Increased dimension of lyt_list from char*4 to char*5
! 2017Dec20  Added SplitTwins
C
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'   !contains nstatn.
      include 'major.ftni'
      include 'covar.ftni'
!      include '../skdrincl/sourc.ftni'

C Input
      character*(*) cmdline

! functions
      integer iStringMinMatch
      integer trimlen
! Stuff dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=6)
      character*(Max_stn*2) ltoken(MaxToken)
      integer icmd
      character*20 lcmd
      logical kvalue

C   LOCAL VARIABLES
      real rval
      integer i,j         !counter
      integer iptr
      character*1 lchar
      integer isub_tmp(max_stn),nsub_tmp

! valid command list.
      integer icmd_list_len
      parameter (icmd_list_len=23)
      character*20 lcmd_list(icmd_list_len)
      character*20 lcmd_caps(icmd_list_len)
      character*65 lhelp(icmd_list_len)

      data (lcmd_list(i), lhelp(i), i=1,icmd_list_len)/
!1
     >" ? ",
     >"            List major commands and options",
!2
     >"List", 
     >"            List current values      ",
!3
     > "Subnet",
     >" <string>   Observing subnet, e.g: KkWzBr  or Kk-Wz-Br",
!4
     >"SkyCov",
     >" [Yes|No]   Optimize by sky coverage or covariance",
!5
     >"AllBlGood",
     >" [Yes|No]   If true, all baselines must meet SNR targets",  
!6 
     >"MaxAngle",
     >" <int>      Max angle (degree) between consecutive obs",
!7
     >"MinAngle",
     >" <int>      Min angle (degree) between consecutive obs",
!8 
     >"MinBetween",
     >" <int>      Min time (minutes) between obs of a source",
!9
     >"MinSunDist",
     >" <int>      Minimum distance (degrees) of source from sun",

!10
     >"MaxSlewTime",
     >" <int>      Maximum allowable slew time in seconds",
!11
     >"TimeWindow",
     >" <real>     Time window used in calculation (hours)",
!12
     >"MinSubNetSize",
     >" <int>      Minimum subnet size ",
!13     
     >"NumSubNet",
     >" <int>      Maximum number of subnets",
!14  
     >"Best",
     >" <int>      % of obs to consider in Normal mode",  
!15    
     >"FillIn",
     >" [Yes|No]   Turn on subnet mode ",
!16
     >"FillMinSub",
     >" <int>      Minimum subnet size in FillIn mode",
!17
     >"FillMinTime",
     >" <int>      Min time (seconds) before we fill in",
!18
     >"FillBest",
     >" <int>      % of obs to consider in FillIn mode", 
!19   
     >"AddPS",
     >" <real>     Amount of noise to add (ps)",
!20 
     >"SNRWts",    
     >" [YEs|No]   Use SNR to weight observations",
!21
     >"LastHrs",
     >" <real>     OBSOLETE! Use TimeWindow",
!22
     >"ObsWts",
     >" <real>     OBSOLETE! Use SnrWts",
!23
     >"SplitTwins",
     >" [Yes|No]   Schedule twin telescopes independently"/

   
      integer iyt_list_len
      parameter (iyt_list_len=6)
      character*5 lyt_list(iyt_list_len)
      data lyt_list/"TRUE","YES","ON","FALSE","NO","OFF"/
      integer iwt_list_len
      parameter (iwt_list_len=2)
      character*10 lwt_list(iwt_list_len)
      data lwt_list/"SNRWT","EQUALWT"/ 

      do i=1,icmd_list_len
         lcmd_caps(i)=lcmd_list(i)
         call capitalize(lcmd_caps(i))  
      end do 
    
      call capitalize(cmdline)
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      itoken=1
      if(NumToken.eq.0 .or. Numtoken.eq.1 .and. ltoken(1).eq."?") then
        do icmd=1,icmd_list_len
          write(luscn,'(a,1x,a)') lcmd_list(icmd),lhelp(icmd)
        end do
        return
      endif
     

! parse the token, see if it is valid.
      Do while(itoken .le. NumToken)
! Remove "%" and "_" from list
        lcmd=" "
        iptr=1
        do j=1,trimlen(ltoken(itoken))
          lchar=ltoken(itoken)(j:j)
          if(lchar.ne. "_" .and. lchar .ne. "%") then       
             lcmd(iptr:iptr)=lchar
             iptr=iptr+1
          endif
        end do
    
        icmd=istringMinMatch(lcmd_caps,icmd_list_len,lcmd)
         if(icmd .eq. 0) then
           write(luscn,'(a,a)') "major_cmd: Not found: ",ltoken(itoken)
           return
        else if(icmd .le. 0) then
           write(luscn,'(a,a)') "major_cmd: Ambigious  ",ltoken(itoken)
           Return
        endif
        lcmd=lcmd_caps(icmd)
        itoken=itoken+1           !point to the argument (if any)
 
! If command was of form: "Major CMD ?" then give help for command. 
        if(itoken .le. NumToken .and. ltoken(itoken) .eq. "?") then
          write(luscn,'(a,1x,a)') lcmd_list(icmd),lhelp(icmd)
          goto 90
        endif

! ORder of processing is 
        select case(lcmd)  
        case("LIST")                      
          call major_out(ludsp,'d')
        case("OBS_WTS") 
! This is obsolete! should use SNRWTS
          icmd=istringMinMatch(lwt_list,iwt_list_len,ltoken(itoken))
          if(icmd .le. 0) goto 200
          kSNRWts=icmd.eq. 1
        case("SUBNET")
! parse the subnet.
          if(itoken .gt. numToken) then 
             do i=1,nstatn
               isubst(i)=i
             end do
             nsubst=nstatn
           else
             call extract_station_list(
     >            luscn,ltoken(itoken),isub_Tmp,nsub_tmp)
             if(nsub_tmp .eq. 0) then
               write(luscn,*)
     >          "Major_cmd: Bad station list: ",ltoken(itoken)
                 return
             endif
             do i=1,nsub_Tmp
               isubst(i)=isub_tmp(i)
             end do
             nsubst=nsub_tmp
          endif
! The following all take Yes/No or true false.
        case("ALLBLGOOD", "FILLIN","SKYCOV","SNRWTS","SPLITTWINS")       
          icmd=istringMinMatch(lyt_list,iyt_list_len,ltoken(itoken))
          if(icmd .eq. 0) then
             write(luscn,*)
     >         "Major_cmd: Bad logical value: ",ltoken(itoken)
          else
             kvalue=icmd .le. 3
          endif
! set approrpirate flag.
           select case(lcmd)
           case("ALLBLGOOD") 
             kAllBLGood=kvalue
           case("SPLITTWINS") 
             kSplitTwins=kvalue
           case("FILLIN") 
             kfillin=kvalue
           case("SNRWTS") 
             ksnrWts=kvalue
           case("SKYCOV") 
             kOptBySky=kvalue
           end select
! Follwoing all take integer
        case("ADDPS",      "BEST",  "FILLBEST", "FILLMINSUB",
     >       "FILLMINTIME","LASTHRS","MAXSLEWTIME", "MAXANGLE", 
     >       "MINANGLE",  "MINBETWEEN",
     >       "MINSUBNETSIZE", "MINSUNDIST", "NUMSUBNET","TIMEWINDOW")

          read(ltoken(itoken),'(f20.0)',err=200) rval
     
          select case(lcmd)
          case("ADDPS")
            radd_noise=rval
          case("BEST") 
            if(rVal .gt. 100 .or. rval .lt. 0) then
              write(luscn,'(a)') "Percentage is out of limits."
              goto 200
            endif
            rBestPerCent=rval/100. 

          case("FILLBEST") 
            IFillBest=rval
            if(ifillbest .gt. 100) then
               ifillbest=100
               write(luscn,*)
     >           "Major_CMD: FillBest too big. Setting to 100"
            else if(ifillbest .lt. 10) then
               write(luscn,*)
     >           "Major_CMD: FillBest too small. Setting to 10"
               ifillbest=10
            endif
          case("FILLMINSUB") 
            iFillMin=rval
            if(iFillMin .gt. 5) then
               iFillMin=5
               write(luscn,*)
     >           "Major_CMD: FillMinSub  too big. Setting to 4"
            else if(iFillMin .lt. 2) then
               write(luscn,*)
     >           "Major_CMD: FillMinSub too small. Setting to 2"
               iFillMin=2
            endif
          case("FILLMINTIME")
            ifilltime=rval
            if(iFillTime .gt. 600) then
               iFillTime=600
               write(luscn,*)
     >           "Major_CMD: FillTime  too big. Setting to 600"
            else if(iFillTime .lt. 20) then
               write(luscn,*)
     >           "Major_CMD: FillSub too small. Setting to 20"
               iFillTime=20
            endif
          case("MAXSLEWTIME")
           iMaxSlewTime=rval 
          case("MAXANGLE") 
             rMaxAngle =rval
             if(rMaxAngle .lt. 15.0) then   
               write(luscn,
     >       '("Major_cmd: Max_angle must be at least 15.0 deg")')             
               write(*,*) "Setting to 15.0 deg"
               rMaxAngle=15.d0
             else if(rMaxAngle .gt. 180.0) then   
               write(luscn,
     >       '("Major_cmd: Max_angle can not be larger than 180 deg")')              
               write(*,*) "Setting to 180.0 deg"
               rMaxAngle=180.d0             
             endif

             if(rMaxAngle .le. rMinAngle) then
                write(*,'(a,f6.2)') 
     >           "MaxAngle can not be less than MinAngle of ",rMinAngle
                rMaxAngle=min(rMinAngle*2,180.0)
                write(luscn,'("      Setting to: ",i4)') rMinAngle
              endif 

          case("MINANGLE")     
            rMinAngle = rval  
            if(rMinAngle .lt. 2.0) then   
               write(luscn,
     >           '("Major_cmd: Min_angle must be at least 2.0 deg")')             
               write(*,*) "Setting to 2.0 deg"
               rMinAngle=2.d0
             else if(rMinAngle .gt. 90.0) then   
               write(luscn,
     >       '("Major_cmd: Min_angle can not be larger than 90.0 deg")')              
               write(*,*) "Setting to 90.0 deg"
               rMinAngle= 90.0d0             
             endif

             if(rMinAngle .ge. rMaxAngle) then
                write(*,'(a,f6.2)') 
     >        "MinAngle can not be greater than MaxAngle of ",rMaxAngle
                rMinANgle=rMaxAngle/2
                write(luscn,'("      Setting to: ",i4)') rMinAngle
              endif 

          case("MINBETWEEN") 
            iMinBetween=rval*60           !convert to seconds.    case("MINSUBNETSIZE")
          case("MINSUBNETSIZE") 
             MinSubNetSize = nint(rval)
             if(MinSubNetSize.gt.nstatn .or. MinSubNetSize .lt. 2) then
               write(luscn,
     >     '("Major_CMD: MinSubNet size ranges from 0 to # stats=",i2)')
     >           nstatn
               MinSubNetSize=max(2,MinSubNetSize)              !make sure >=2
               MinSubNetSize=min(MinSubNetSize,nstatn/2)       !default value
               write(luscn,*) "Major_cmd: Setting to: ",MinSubNetSize
             endif 
          case("MINSUNDIST")
            rSunMinAngle= rval
          case("NUMSUBNET") 
            NumSubNet=int(rval)
            if(NumSubNet .gt. MaxSubNet .or. NumSubNet .lt. 1) then
              write(luscn,
     >        '("Major_CMD: Valid values for NumSubNet are 0 - ",i2)')
     >            MaxSubNet
               NumSubNet=Min(NumSubNet,MaxSubNet)
               NumSubNet=Max(NumSubNet,1)
              write(luscn,'("          Setting NumSubNet=",i4)')
     >            MaxSubNet
            endif
          case("TIMEWINDOW","LASTHRS") 
            if(rcovar_win .ne. rval.and. rcovar_win .ne. -1.) then
              write(luscn,*) "Recalculating coverage"
              call opfill
            endif
            rcovar_win=rval
           case default
            write(*,*) "Major_cmd not processed: ", lcmd
          end select 
        case default
           write(*,*) "Major cmd not processed: ", lcmd
        end select 
    
90      continue
        iToken=iToken+1        !
100     continue
      ENDDO
      RETURN

200   continue
      write(luscn,'(a,a)')
     >  "major_cmd: Unknown parameter or eror for major mode: "
      write(luscn,*) cmdline(1:trimlen(cmdline))
      return

      END
