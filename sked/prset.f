      SUBROUTINE PRset(LINSTQ)
C
C   PRSET allows the user to set default values of parameters
C
!2023-02-08 JMG.  Set mininum schedule time to 5 minutes and made a parameter. 
!2022-03-18 Moved correlator list to a common block
!2021-05-05 JMG Added "Beg" as synonym for start
!2021-01-13 JMG Added Vien to correlator
!2009Nov05 JMGipson.  changed lyt_list from char*4 to char*5
!2010.06.15 JMGipson.  Better error message if didn't find frequency code
!2012.10.11 JMGipson. Added VIE_SCHED_VERSION and VIE_SCHED_CREATE_DATE
!2013Sep13  JMGipson.  Changed to use "SCHEDULING_SOFTWARE", "SOFTWARE_VERSION", "SCHEDULE_CREATE_DATE"
!2015Sep21  JMGipson. Was not correctly setting MARK6_OFF 
!2015Dec01  JMGipson. Added SHAO 
!2017Oct07  KOL. Added conf_equip
!2018Oct05  KOL. Added UTAS


      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/freqs.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'valid_correlator.ftni'

C
C     INPUT VARIABLES:
      integer*2 LINSTQ(*)
C      - input string, length=word 1
C
C     CALLING SUBROUTINES: PRCMD (the command routine for PARAMETERS)
C     CALLED SUBROUTINES:  YMDHM (to decode starting datemtime)
C                          IGTKY (to decode key words)
C
! functions
      integer iStringMinMatch
      double precision hms2seconds
      integer ichcm_ch,i2long,ichmv,ias2b !functions
      integer fc_gwinsz,fc_gwinw
      integer trimlen
      integer julda
      integer igtfr
      integer iwhere_in_string_list


C   LOCAL VARIABLES
      integer ilen
      integer iwhere
      integer itempsub(max_stn),ntempsub,ierr
      integer iyr,ida,ihr,imin,isc
      integer jkey,ich,ic1,ic2,nc,idummy,ikey,i,inum,is
      integer*2 LKEYWD(80)
      character*30 ckeywd
      equivalence (lkeywd,ckeywd)
C               - Key word, longest is 22 characters
      character*2 ckey
      logical kyesno

      integer*2 lfrqde
      integer icod
      character*2 cfrqde
      equivalence (lfrqde,cfrqde)

      integer MaxPr
      parameter (MaxPr=67)
      character*22 listPr(MaxPr),cParam
      character*2  listPrShort(MaxPr)

      character*6 ListAM(2),ListAS(2),listOnOff(2)      
      integer imin_sked_len                   !shortest time for a schedule.
      data imin_sked_len/300/                 !Set to 5 minutes
      
 
      integer iyt_list_len
      parameter (iyt_list_len=6)
      character*5 lyt_list(iyt_list_len)
      data lyt_list/"TRUE","YES","ON","FALSE","NO","OFF"/

      data listPr/
     > "ALL_BL_GOOD","BARREL",   "BEG", "CALIBRATION","CHANGE",
     > "CONFIRM",
     > "CONF_EQUIP",
     > "CORRELATOR","CORSYNCH", "DEBUG",      "DESCRIPTION","DURATION",
     > "EARLY",     "END",      "EXPERIMENT", "FILLIN",    "FILLBEST",
     > "FILLSUB",   "FILLTIME", "FILL_OFF","FREQUENCY", "GET", "HEAD",
     > "IDLE",      "JAVA",     "KEEP_LOG",  "LIST","LOOKAHEAD", 
     > "MAXSCAN",
     > "MIDOB",      "MIDTP", "MINBETWEEN","MINIMUM",    "MINSLEW",
     > "MINSCAN",
     > "MARK6_OFF", 
     > "MINSUBNET", "MODSCAN",   "MODULAR",    "NOREWIND", "PARITY",
     > "POSTOB",    "POSTPASS",  "PREOB",      "PREPASS",    "PRFLAG",
     > "SCHEDULER",
     > "SKED_CREATE_DATE",  "SKED_VERSION", "SETUP", "SNR",
     > "SOURCE",  "START",     "SUBNET",     "SUNDIS",    "SYNCHRONIZE",
     > "TAPETM",  "VERBOSE",   "VIS",        "VSCAN",      "WIDTH",
     > "VIE_SCHED_VERSION","VIE_SCHED_CREATE_DATE",
     > "SCHEDULING_SOFTWARE", "SOFTWARE_VERSION","SCHEDULE_CREATE_DATE",
     > "VERBOSE_LEVEL"/

      data listPrShort/
     >"AG","BR","BG","CA","CH",
     >"CO",
     >"CE",
     >"TC","CR","DG","DE","DU",
     >"TE","EN","EX","FI","FB",
     >"FS","FT","FO","FR","GT","HD",
     >"ID","JA","KP","LI","LO","XS",
     >"MI","MT","MB","MN","ML","MS","M6",
     >"SM","MD","MO","NR","PA",
     >"PO","PS","PR","PP","PF",
     >"PI",
     >"xD","xV","SP","SA",
     >"SO","ST","SU","SD","SY",
     >"TP","--","VI","VS","WI",
     >"V1","V2","--","--","--",
     >"VB"/

      data ListAM/"AUTO","MANUAL"/
      data ListAS/"ALL","SUBNET"/
      data listOnOff/"ON","OFF"/

C
C  History
C     880311 NRV DE-COMPC'D
C     890110 GAG ADDED SUNDIS AS PARAMETER
C     890426 GAG ADDED NEW PARAMETERS MINSCAN,MAXSCAN,MODSCAN,WIDTH
C     890427 GAG ADDED NEW PARAMETERS BWSCAN,SNRSCAN,CHANSCAN,VIS
C     890428 GAG ADDED NEW PARAMETERS CONFIRM,BASESCAN
C     890517 GAG ADDED NEW PARAMETER CORSYNCH
C     891129 gag removed BASESCAN,SNRSCAN,BWSCAN,CHANSCAN
C     891205 NRV Added new parameter SNR
C     900206 gag removed LENGTH(mxfeet)
C     900222 gag added an if to 2HSA
C     900326 gag added vscan and removed maxscan
C     900516 gag cleaned up. made all sections look like int section.
C     900530 gag added igtky calls instead of string comparisons.
C     910224 NRV Added EARLY, removed ELEVATION, PEAK
C     910618 NRV Check MODULAR for > 0
C     920529 nrv Add minbetween
C     930225 nrv implicit none
C     930408 nrv Put minbetween back in
C     931029 nrv Remove minbetween, it's now in OPSET etc.
C     950502 nrv Add MINSUBNET
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed gtfld call to remove linstq
C 951018 nrv Check character "ckey" instead of lkey hollerith
C 951214 nrv Add "BARREL" parameter
C 960709 nrv Remove BARREL, moved to freqs.ftni
C 960923 nrv ITEARL is an array, so set up all stations with the
C            same value for now. Move to separate command later.
C 970314 nrv If WIDTH is -1 get window size from X calls.
C 970314 nrv Remove EARLY.
C 970321 nrv Put EARLY back in for Mk3 correlator compatibility.
C            If you use it as a parameter, all stations are set.
C 990412 nrv Add MAXSCN.
C 990520 nrv Add DESCRIPTION, CORRELATOR, SCHEDULER.
C 991108 nrv Allow only know correlators.
C 991109 nrv Input string is lower case. Change to upper case for
C            checking key words as needed. The purpose is to allow
C            the experiment description and correlator names to be
C            upper/lower naturally.
C 991119 nrv Add nominal START and END times.
C 000114 nrv Uppercase the subnet.
C 000120 nrv Remove many 'returns' and leave at defaults instead.
C 000318 nrv Procedure names and experiment name was not set
c            correctly because the key word was not parsed.
C 000326 nrv New parameter "JAVA" to start the java program.
C 020228 nrv Fix logic on correlator name.
C 021010 nrv Add PS for postpass to be scheduled YES or NO.
! 2008May22 JMG minor changes in naming. Some parameters moved to
!           major,minor modes
! 2008Jun10 Added "keep_log" flag
! 2009Jul15 JMG.  on reading in subnetsize, set minium to 2
! 2009Oct15 Got rid of unused array lsrcdist
! 2010Mar23 JMG. Previously WASH was not a valid correlator
! 2010Mar26 JMG. Changed stutcm ->utstcm, enutcm->utencm for consistency with jdstcm and jdencm
! 2010Sep10 Ignore many tape commmands. 
C
C
C  1. Now parse the input string, getting each key word and its value.
C
      ICH = 1
      ilen = linstq(1)
! list command       
      if(ilen .eq. 0) then
         call prlis(lintsq)
         return
      endif      
      
100   continue
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
      IF  (IC1.EQ.0) RETURN 
      NC = min0(IC2-IC1+1,24)
      ckeywd=" "                      !initialize
      IDUMMY = ICHMV(LKEYWD,1,LINSTQ(2),IC1,NC)
      ikey=iStringMinMatch(listpr,MaxPr,ckeywd)      
!      write(*,*) ckeywd, ikey 
!      if(ikey .eq. 0) then
!         write(*,*) listpr(1:MaxPR)
!         stop
!      endif 
      
      IF  (IKEY.EQ.0) THEN  !invalid
        write(luscn,9110) ckeywd
9110    format('PRSET01 - ',a24,' is not a valid parameter name.')
        RETURN
      Else if(ikey .eq. -1) then
        write(luscn, 
     >   '("PRSET02 -", a, " ambiguous command.")') trim(ckeywd)
        if(ckeywd .eq. "VERBOSE") then
          CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
          goto 100
        else
         write(luscn,*) "Skipping rest of line" 
          return
        endif 
      endif 
      
      if(listpr(ikey) .eq. "LIST") then
         call prlis(lintsq)
         return
      endif 
      
      select case(listpr(ikey))

      case("SCHEDULING_SOFTWARE","SOFTWARE_VERSION",
     >     "SCHEDULE_CREATE_DATE",
     >     "SKED_VERSION","SKED_CREATE_DATE",
     >     "VIE_SCHED_VERSION","VIE_SCHED_CREATE_DATE")     
! Information. Don't need to do anything. 
        return
      case default
      end select 

      ckey=listPrshort(ikey)
      Cparam=listpr(ikey)
    
      if (ckey.eq.'JA'.or.ckey.eq.'GT') then ! call java program
        call parcmd(ckey)
        return
      endif
C
C  2. This is the integer-value section.
C
      IF  (ckey.eq.'DU'.OR.ckey.eq.'CA'.OR.ckey.eq.'MO'.OR.
     .     ckey.eq.'LO'.OR.ckey.eq.'MN'.OR.ckey.eq.'CH'.OR.
     .     ckey.eq.'ID'.OR.ckey.eq.'MS'.OR.ckey.eq.'SP'.OR.
     .     ckey.eq.'PA'.OR.ckey.eq.'PP'.OR.ckey.eq.'HD'.OR.
     .     ckey.eq.'SO'.OR.ckey.eq.'SD'.OR.ckey.eq.'MB'.or.
     .     ckey.eq.'MT'.OR.ckey.eq.'TP'.OR.ckey.eq.'MD'.OR.
     &     ckey.eq.'ML'.or.ckey.eq.'M6'.or.ckey.eq.'FO'.or. 
     .     ckey.eq.'WI'.OR.ckey.eq.'CR'.or.ckey.eq.'TE'.or.
     >     ckey.eq.'FS'.or.ckey.eq.'FB'.or.ckey.eq.'FT'.or.
     .     ckey.eq.'SM'.or.ckey.eq.'XS'.or.ckey.eq.'VB') Then
        CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
        if (ic1.eq.0) then
          write(luscn,'("PRSET03 - You must enter a number.")')
          return
        endif
        INUM = IAS2B(LINSTQ(2),IC1,IC2-IC1+1)
        IF  (INUM.lt.0) THEN
          write(luscn,'("PRSET04 - Invalid number for ",a,".")') ckey
          RETURN
        END IF
        if(inum .lt. 0) then
           write(luscn,*) " PRSET05 - ERROR:",
     >      Trim(Cparam)," must be >0"
          return
        endif
        IF (ckey.eq.'CA') THEN
          ICALDE = INUM
!        ELSE IF (ckey.eq.'CH') THEN
!          ITCTIM = INUM
        ELSE IF (ckey.eq.'DU') THEN
          IDURDE = INUM
        ELSE if(ckey .eq.'FB') then
          IFillBest=inum
          if(ifillbest .gt. 100) then
             ifillbest=100
             write(luscn,*) "PRSET: FillBest too big. Setting to 100"
          else if(ifillbest .lt. 10) then
             write(luscn,*) "PRSET: FillBest too small. Setting to 10"
             ifillbest=10
          endif
        ELSE if(ckey .eq.'FM') then
          iFillMin=inum
          if(iFillMin .gt. 5) then
             iFillMin=5
             write(luscn,*) "PRSET: FillSub  too big. Setting to 4"
          else if(iFillMin .lt. 2) then
             write(luscn,*) "PRSET: FillSub too small. Setting to 2"
             iFillMin=2
          endif
        ELSE if(ckey .eq.'FT') then
          iFillTime=inum
          if(iFillTime .gt. 600) then
             iFillTime=600
             write(luscn,*) "PRSET: FillTime  too big. Setting to 600"
          else if(iFillTime .lt. 20) then
             write(luscn,*) "PRSET: FillSub too small. Setting to 20"
             iFillTime=20
          endif
        ELSE IF (ckey.eq.'HD') THEN
!obsolete command        
!          IHDTM = INUM
        ELSE IF (ckey.eq.'ID') THEN
          IDLDEF = INUM
        ELSE IF (ckey.eq.'LO') THEN
          LOOKAH = INUM*60
        ELSE IF (ckey.eq.'MD') THEN
          IF (INUM.LE.0) THEN
            WRITE(LUSCN,"(' PRSET06 - ERROR: MODSCAN must be > 0')")
          else
            MODSCN = INUM
          END IF
        ELSE IF (ckey.eq.'MN') THEN
          IMINTM = INUM
        ELSE IF (ckey.eq.'MO') THEN
          IMODTM = INUM
        ELSE IF (ckey.eq.'MS') THEN
         MINSCN = INUM
        ELSE IF (ckey.eq.'MT') THEN
! obsolte midtape command        
!          IMTPTM = INUM
        else if(ckey .eq. 'FO') then
         ifill_off=inum
        else if(ckey .eq. 'M6') then
         imark6_off=inum
        ELSE IF (ckey.eq.'PA') THEN
! Obsolete parity command         
!          IPARTM = INUM
!        ELSE IF (ckey.eq.'PP') THEN
!          IPRETM = INUM
        ELSE IF (ckey.eq.'XS') THEN
         MAXSCN = INUM
C       ELSE IF (ckey.eq.'MB') THEN
C         mintdiff = INUM*60
!        ELSE IF (ckey.eq.'SD') THEN
!          ISUNDI = INUM
        else if(ckey .eq.'VB') then
          Iverbose_level=inum 
        ELSE IF (ckey.eq.'SO') THEN
          ISORTM = INUM
        ELSE IF (ckey.eq.'SP') THEN
          ISETTM = INUM
        ELSE IF (ckey.eq.'TP') THEN
          ITAPTM = INUM
        ELSE IF (ckey.eq.'WI') THEN
          IF (INUM.eq.0) THEN ! default
            iwdef = 0
            iwscn = fc_gwinw()
            if (iwscn.lt.1.or.iwscn.gt.999) IWSCN=79
            ihscn = fc_gwinsz()
            if (ihscn.lt.1.or.ihscn.gt.999) ihscn=24
          else ! set value
            iwdef = inum
            iwscn = inum
            ihscn = 24
          endif
        ELSE IF (ckey.eq.'CR') THEN
          IF (INUM.LT.0.or.(inum.gt.0.and.itearl(1).gt.0)) THEN
            WRITE(LUSCN,9230)
9230        FORMAT(' PRSET08 - ERROR: CORSYNCH must be > 0, EARLY',
     .             ' must be 0 to use CORSYNC')
          else
            ITSYNC = INUM
          END IF
        else if (ckey.eq.'TE') then
          if (inum.lt.0) then
            write(luscn,"(' PRSET09 - ERROR: EARLY must be > 0.')")
          else ! check stations
            if (nstatn.le.0) then
              write(luscn,'(a)') ' PRSET90 - Select stations first.'
            else !  check cal time
              if (inum.gt.0.and.inum.lt.icalde) then
                write(luscn,"(' PRSET10 - ERROR: EARLY must be>CAL.')")
              else ! check corsync
                if (inum.gt.0.and.itsync.gt.0) then
                  write(luscn,'(a)')
     >             ' PRSET11 - ERROR: CORSYNC must be 0 to use EARLY'
                else ! ok
                  do is=1,nstatn
                    itearl(is) = inum
                  enddo
                endif
              endif
            endif
          endif
        else if (ckey.eq.'SM') then ! subnet number
          if (inum.lt.2) then
            write(luscn,'(a)') " PRSET12-ERROR: MINSUBNET must be >= 2"
            write(luscn,'(a)') "Setting size to 2"
          endif
          minsubnetsize = max(inum,2)
        END IF

        GOTO 900
      END IF  !integer-value section
 
C
C  3. Real number section.
 
C
C  4. String parameter section.
 
      IF  (ckey.eq.'FR'.or.ckey.eq.'SY'.or.ckey.eq.'VI'.or.
     .     ckey.eq.'SA'.or.ckey.eq.'CO'.or.ckey.eq.'VS'.or.
     .     ckey.eq.'PR'.or.ckey.eq.'MI'.or.ckey.eq.'BR'.or.
     .     ckey.eq.'PO'.or.ckey.eq.'EX'.or.ckey.eq.'PF'.or.
     .     ckey.eq.'DE'.or.ckey.eq.'PI'.or.ckey.eq.'TC'.or.
     .     ckey.eq.'PS'.or.ckey.eq.'FI'.or.ckey.eq.'DN'.or.
     >     ckey.eq.'AG'.or.ckey.eq.'NR'.or.
     >     Cparam .eq. "KEEP_LOG" .or. Cparam .eq. "VERBOSE" .or.
     >     Cparam .eq. "SRCDIST"  .or. Cparam .eq. "DEBUG"  .or.
     >     Cparam .eq. "CONF_EQUIP"
     >      ) then
        CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
        nc=ic2-ic1+1
        if (ic1.eq.0) then
          write(luscn,'(a)') "PRSET12 - You must enter something "//
     >      "for the parameter value."
          return
        end if

        IF (ckey.eq.'FR') THEN  !frequency code.
          if(.not.kVexIn) then  ! only valid for non-vex files (at least on input.) 
            IDUMMY = ICHMV(LFRQDE,1,LINSTQ(2),IC1,MIN0(2,nc))
            IF (IGTFR(Lfrqde,icod).EQ.0) THEN  !not selected
              write(luscn,'("PRSET: Frequency code ",a, " not found!")')
     >             cfrqde
             write(*,*) "VAlid codes ",ccode(1:ncodes), "--"
             IERR = -1
             RETURN
            END IF  !not selected
            icode_set_last=icod         !save the frequency code.
          endif 
        else IF (ckey.eq.'VI') THEN  !vis
          ckeywd=" "
          idummy = ichmv(lkeywd,1,linstq(2),ic1,nc)
          jkey=iStringMinMatch(listAS,2,ckeywd)
          IF  (jkey.EQ.0) THEN
            WRITE(LUSCN,"(' PRSET14 - Error: VIS must be SUB or ALL')")
          ELSE
! 2008May23 Remove this set. Can't see that kallowsubnet does anything.
!            kallowsubnet = listAS(jkey).EQ.'SUBNET'
!            write(*,*) listAS(jkey), kallowsubnet
          END IF
        else IF (ckey.eq.'SA') THEN  !snr
          ckeywd=" "
          idummy = ichmv(lkeywd,1,linstq(2),ic1,nc)
          jkey=iStringMinMatch(listAM,2,ckeywd)
          IF  (jkey.EQ.0) THEN
            WRITE(LUSCN,"(' PRSET15 - Error: SNR must be MAN or AUTO')")
          ELSE
            KASNR= listAM(jkey).EQ.'AUTO'
          END IF
! Bunch of Yes-No parameters.
        else IF (Cparam .eq. "CONFIRM"  .or.
     >           Cparam .eq. "POSTPASS" .or.
     >           Cparam .eq. "VSCAN"    .or.
     >           Cparam .eq. "FILLIN"   .or.
     >           Cparam .eq. "DEBUG"    .or.
     >           Cparam .eq. "SYNCHRONIZE" .or.
     >           Cparam .eq. "VERBOSE"    .or.
     >           Cparam .eq. "KEEP_LOG" .or.
     >           Cparam .eq. "CONF_EQUIP" .or.
     >           Cparam .eq. "ALL_BL_GOOD" .or.
     >           ckey .eq. "NR") THEN
          ckeywd=" "
          idummy = ichmv(lkeywd,1,linstq(2),ic1,nc)

           jkey=istringMinMatch(lyt_list,iyt_list_len,ckeywd)
           if(jkey .eq. 0) then
              write(luscn,'("PRSET: for ",a " bad logical value:", a)')
     >          trim(cparam),ckeywd
           else
              kyesno=jkey.le. 3
              if(Cparam .eq. "CONFIRM") then
                KASK= kyesno
!              else if(Cparam .eq. "POSTPASS") then
!                kpostpass= kyesno
              else if(Cparam .eq. "VSCAN") then
                kVSCAN= kyesno
              else if(Cparam .eq. "FILLIN") then
                kfillin= kyesno
              else if(Cparam .eq. "DEBUG") then
                kdebug=kyesno
              else if(Cparam .eq. "KEEP_LOG") then
                kkeep_log=kyesno
                if(kyesno) then
                  write(*,*) "Will keep "//trim(clgfil)//" on closing"
                else
                  write(*,*) "Will delete log file on closing "
                endif 
              else if(Cparam .eq. "VERBOSE") then
                iverbose_level=5
              else if(Cparam .eq. "CONF_EQUIP") then
                kconf_equip=kyesno
              else if(Cparam .eq. "ALL_BL_GOOD") then
                kAllBlGood=kyesno
              else if(Cparam .eq. "SPLIT_TWINS") then
                ksplittwins=kyesno
!              else if(ckey .eq. "NR") then
!                kNoRewind=kyesno
              endif
          END IF
C
! mid,etc.
        else IF (ckey.eq.'PR' .or. ckey.eq.'MI' .or. ckey.eq.'PO') then
          ckeywd=" "
          idummy = ichmv(lkeywd,1,linstq(2),ic1,min(nc,6))
          call capitalize(ckeywd(1:6))  !make it upper case.
          if(ckey .eq. 'PR') then
            cprede=ckeywd(1:6)
          else if(ckey .eq. 'MI') then
            cmidde=ckeywd(1:6)
          else if(ckey .eq. 'PO') then
            cpstde=ckeywd(1:6)
          endif
        else IF (ckey.eq.'EX') THEN  !experiment
          cexper=" "
          idummy = ichmv(lexper,1,linstq(2),ic1,min(nc,8))
          call capitalize(cexper)
        else IF (ckey.eq.'PF') THEN !procedure flags
          DO  I=1,4
            if (ichcm_ch(linstq(2),ic1+i-1,'Y').eq.0.or.
     .          ichcm_ch(linstq(2),ic1+i-1,'y').eq.0) kflg(i)=.true.
            if (ichcm_ch(linstq(2),ic1+i-1,'N').eq.0.or.
     .          ichcm_ch(linstq(2),ic1+i-1,'n').eq.0) kflg(i)=.false.
          END DO  !
        else if (ckey.eq.'DE') THEN ! experiment description
C         description goes from here to the end of the line
          ic2=min(ic1+128,linstq(1))
          call hol2char(linstq(2),ic1,ic2,cexperdes)
          return    
        else if (ckey.eq.'PI') THEN ! scheduler's initials
          call hol2char(linstq(2),ic1,ic2,cpiname)
        else if (ckey.eq.'TC') THEN ! target correlator
          ckeywd=" "
          idummy = ichmv(lkeywd,1,linstq(2),ic1,min(nc,12))
          call capitalize(ckeywd)
          iwhere=iwhere_in_string_list(lcorr,num_corr,ckeywd)
          if(iwhere .ne. 0) then
            ccorname=ckeywd
          else
            write(*,*)"WARNING!!!  PRSET: Invalid correlator: ",ckeywd
            write(*,'("Must be one of ",22(a,1x))') 
     >                    (Trim(lcorr(i)),i=1,num_corr)
          endif 
        ENDIF ! code

C  Time parameters section
      else if (ckey.eq.'ST'.or. ckey .eq. "BG" .or.ckey.eq.'EN') then ! start/end
        CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
        if (ic1.eq.0) then
          write(luscn,'(a)') "PRSET20 - You must enter something for "//
     >     "the time parameter."
          return
        end if
        nc = IC2-IC1+1
        ckeywd=" "
        IDUMMY = ICHMV(lkeywd,1,LINSTQ(2),IC1,nc)
        CALL YDHMS(cKEYWD,IERR,IYR,IDA,IHR,iMIN,ISC)
!        write(*,*) "ym...", iyr,ida,ihr,imin,isc
        IF  (IERR.NE.0) THEN 
          write(luscn,'(a)')
     >      'PRSET21 - Nominal start/end must be of form YYDDDHHMMSS.'
          IERR = -1
        ENDIF
        if (ckey.eq.'ST'.or. ckey .eq. "BG") then ! start
!          call gtdat(lkeywd)
          iyr_start = iyr
          ida_start = ida
          ihr_start = ihr
          imin_start = imin
          isc_start = isc
          JDSTCM = JULDA(1, IDA, IYR-1900)
          utstcm =hms2secondS(ihr,imin,isc)

          if(iyr .ne. 0) then
            write(luscn,9022) iyr_start,ida_start,ihr_start,imin_start,
     .      isc_start
9022        format('PRSET22 - Initializing schedule starting time to ',
     .      i4,'-',i3.3,'-',2(i2.2,':'),i2.2)
          endif
          do i=1,max_stn
            iyrcur(i)=iyr_start
            idacur(i)=ida_start
            utcur(i)=hms2seconds(ihr_start,imin_start,isc_start)
            mjdcur(i)=JULDA(1,IDA_start,IYR_start-1900)
          end do

        else if (ckey.eq.'EN') then ! end
          iyr_end = iyr
          ida_end = ida
          ihr_end = ihr
          imin_end = imin
          isc_end = isc
        endif ! start/end
        
        call AdjustEndTime(itimestart,itimeend,imin_sked_len)
C  SUBNET section
      else if (ckey.eq.'SU') then
        CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
        IF (NSTATN.EQ.0) THEN !select stations first
          WRITE(LUSCN,'(a)')
     >    ' PRSET18 - Error: Select stations before choosing subnet.'
          CALL GTFLD(LINSTQ(2),IC1,i2long(LINSTQ(1)),IC1,IC2)
          GOTO 900
        END IF  !select stations first
        if (ic1.gt.0) then ! subnet specified
          nc = ic2-ic1+1
          idummy = ichmv(lkeywd,3,linstq(2),ic1,nc)
          lkeywd(1)= nc
! the second integer of lkeywd is the first station.
          CALL HOL2UPPER(lkeywd(2),nc) ! uppercase the stations
        else ! no char
          lkeywd(1) = 0
        endif 
        ntempsub = nsubst
        do i=1,nsubst
          itempsub(i) = isubst(i)
        end do
        ic1=1
        CALL GTSTI(LKEYWD,ic1,NSUBST,ISUBST,IERR,luscn)
        IF (IERR.NE.0) THEN
          nsubst = ntempsub
          do i=1,nsubst
            isubst(i) = itempsub(i) 
          end do
          write(luscn,'(a)')
     >     ' PRSET19 - Error: Invalid station name in SUBNET.'
          RETURN
        END IF  !
        IF (NSUBST.EQ.0) THEN !all stations
          NSUBST = NSTATN
          DO  I = 1,NSUBST
            ISUBST(I) = I
          END DO
        END IF  !all stations
      endif  !string section
C
C  5.  Test to see if there is more to the line which we need to
C      decode.  If so, go back to parse some more.
C      Do not parse any more after a DESCRIPTION.
 
900   IF ((LINSTQ(1)-ICH).GT.0) GOTO 100
      return

950   continue
      write(luscn,*) "PRSET: Error reading ",ckeywd
      IF ((LINSTQ(1)-ICH).GT.0) GOTO 100
C
      RETURN
      END
