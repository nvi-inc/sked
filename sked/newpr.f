C@NEWPR
      SUBROUTINE NEWPR(cINSTQ,NSOR,IYR,IDA,IHR,iMIN,ISC,UT,MJD,
     .IDUR,IDURSO,ICAL,IDLE,cCABLE,ICOD,cPRE,cMID,cPST,
     .NSTN,ISTN,KVIS,IERR)
C
C   NEWPR decodes the observation command line and checks/sets
C   all of the user's parameters for this obervation.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'major.ftni'
C

! functions
      double precision hms2seconds
      integer iStringMinMatch
      integer igetsrcnum
      integer trimlen
      integer julda,igtfr !functions
C
C  INPUT VARIABLES:
!      integer*2 LINSTQ(*)
      character*(*) cinstq

C               - input string containing users request, word 1=length
C
C  OUTPUT VARIABLES:
      integer nsor,iyr,ida,ihr,imin,isc,mjd,idur,idurso,ical,idle
      integer icod,ierr
C     NSOR - source index
C     IYR,IDA,IHR,iMIN,ISC,UT,MJD - time of observation, MJD=-1
C         means no start time specified
      real*8 UT
C     IDUR,ICAL,IDLE,LCABLE,ICOD,LPRE,LMID,LPST - parameters as
C        specified by user, or defaults if not specified
C     IDURSO - source duration taken from scan lengths
!      integer*2 LCABLE(MAX_STN)
      character*2 ccable(Max_Stn)
      character*6 cpre,cmid,cpst

C     NSTN - number of stations in subnet
      integer ISTN(max_stn),nstn
C      - list of stations in this observation
      LOGICAL KVIS
C      - true if all stations must have mutual visibility, set
C        to true if a subnet is specified, otherwise default
C     IERR - non-zero if any problems occur in checking paramgers
C
C     CALLING SUBROUTINES:  NEWOB
C     CALLED SUBROUTINES: LNFCH
C                         IGTSO, IGTST (get sources, stations)
C                         YDHMS (to decode START, if specified)
C

C   LOCAL VARIABLES
! AEM 20050303 add temp int to avoid memory conflicts
      integer ich_temp
      real*8 ut1
      integer ikey,i,inum
      character*(max_sorlen) csrcnam
      integer lsrcnam(max_sorlen/2)
      equivalence (csrcnam,lsrcnam)
      character*12 ckey

      character*24 ckeywd
      character*90 ctoken
      integer*2 ltoken(46)
      equivalence (ctoken(1:1),ltoken(2))
      integer istart,inext
      logical ktoken,knospace,keol

      integer ilist_len
      parameter(ilist_len=10)
      character*12 list(ilist_len)

      data list/"START","DURATION","PREOB","MIDOB","POSTOB",
     > "CALIBRATION","SUBNET","IDLE","FREQUENCY","CABLE"/

C
C  HISTORY
C     890428 NRV Created, removed from NEWOB
C     930225 nrv implicit none
C     940513 nrv Initialize VLBA cable to "V" for special slewing
C                algorithm
C     950411 nrv Replace some code with a call to the new GTSTI
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed gtfld call to remove linstq
C 970224 nrv Initialize more than 8 chars of LKEYWD for source name
C 981113 nrv Modify JULDA call to make it year since 1900 because
C            iyr not is 4-digit year.
! 2003Dec08 JMGipson. Replaced igtso by igetsrcnum
! 2009Jul15 JMGipson. Better error message
! 2009Nov02 JMGipson.  Modified setting of Kvis. Now set to true if "sub=" is specified
C
C
C     1. Set up default values for every variable which the user
C     can specify.
C

      IERR=0
      ICAL= ICALDE
      IDLE = IDLDEF
      ccable=" "

      icod=0
      MJD=-1
      UT=0.D0
      UT1=0.D0
      NSTN = NSUBST
      DO  I = 1,NSTN
        ISTN(I) = ISUBST(I)
      END DO
!      KVIS=Kallowsubnet
      KVIS=.false.
      cpre=cprede
      cmid=cmidde
      cpst=cpstde
C
C     2. Source name or number
C
      if(trimlen(cinstq) .eq. 0) then
        write(luscn,'(a)') "Newpr: Can't parse an empty line!"
        ierr=-1
        return
      endif
 
! First get the source name or number.
!
      istart=1
      call ExtractNextToken(cinstq,istart,inext,csrcnam,ktoken,
     >  knospace, keol)
      nsor=0
      if(ktoken) then
        nsor=igetsrcnum(csrcnam)
      endif
      if(nsor .eq. 0) then
        write (luscn,'("NEWPR01: Invalid source name ",a)') csrcnam
        ierr=-1
        RETURN
      else if(nsor .lt. 0) then
        write (luscn,'("NEWPR01: Ambiguous source name ",a)') csrcnam
        ierr=-1
        return
      endif

      IDURSO = ISSCAN(NSOR)
      IDUR=0
C
C     3.  Key word and its code number.
C
200   continue
      istart=inext
      call ExtractNextToken(cinstq,istart,inext,ckeywd,ktoken,
     >  knospace, keol)
      if(.not. ktoken .or. knospace .or. keol) then   !exit if no more tokens
        if(icod .eq. 0)  ICOD = icode_set_last    !set to default frequency
        return
      endif

      ikey=istringminmatch(list,ilist_len,ckeywd)
      IF  (IKEY.EQ.0) THEN  !invalid
        write(luscn,"('NEWPR02: Keyword invalid for new observation.')")
        IERR = -1
        RETURN
      END IF  !invalid

      IF  (IKEY.EQ.-1) THEN  !ambiguous
        write(luscn,"('NEWPR03 - Ambigous key word.')")
        IERR = -1
        RETURN
      END IF  !ambiguous

      ckey=list(ikey)

! Get the information.
      istart=inext
      call ExtractNextToken(cinstq,istart,inext,ctoken,ktoken,
     >  knospace, keol)
      if(.not. ktoken) then
        write(luscn,'(a)')
     >   ' NEWPR04 - No Information supplied with parameter! '
        write(*,*) "Input line: ", cinstq(1:trimlen(cinstq))
        ierr = -1
        return
      end if
C
C     4.  This is the START time section.  Pick off the next word,
C         decode it via YDHMS, set defaults to CUR values.
! some of the subroutines expect lkeywd. This puts ctoken into lkeywd.

      ltoken(1)=trimlen(ctoken)

      IF  (ckey.eq.'START') THEN  !start time
        CALL YDHMS(ctoken,IERR,IYR,IDA,IHR,iMIN,ISC)
        IF  (IERR.NE.0) THEN  !
          write(luscn,9410)
9410      format(' NEWPR05 - Start date/time must be of form ',
     .           'YYYYDDDHHMMSS. YY and DDD optional.')
          IERR = -1
        END IF  !
        MJD = JULDA(1,IDA,IYR-1900)
        ut=hms2seconds(ihr,imin,isc)
C
C     5.  This is the time section.  The other specifications may be
C         calibration, duration, idle time.
C
      else if(ckey .eq. "DURATION" .or. ckey .eq. "CALIBRATION" .or.
     >   ckey .eq."IDLE") then
        read(ctoken,*) inum
        IF  (INUM.LT.0) THEN  !
          write(luscn,9510)
9510      format(' NEWPR06 - Bad value for CALIBRATION or DURATION.')
          IERR = -1
          RETURN
        END IF  !
        if(ckey .eq. "DURATION") then
           idur=inum
        else if(ckey .eq. "IDLE") then
           idle=inum
        else if(ckey .eq. "CALIBRATION") then
           ical=inum
        endif
C
C     6. This is the station cable wrap designation.
C         Parse the next word for the STATION ID AND cable indicator.
C
C     Leave the code below alone since no one uses cable wrap
C     speciications anyway. If they do, they have to use the old
C     1-letter IDs.
      Else if  (ckey.eq.'CABLE') THEN  !cable wrap
        write(*,*) "NEWPR07: Cable wrap not implemented!"
        ierr=-1
        return

!        NCH = IC2-IC1+1
!        IDUMMY = ICHMV(LKEYWD,1,LINSTQ(2),IC1,2)
!        IDUMMY = IGTST(LKEYWD,IS)
!        IC = JCHAR(LKEYWD,2)
!        IF  (NCH.NE.2.OR.IS.EQ.0.OR.
!     .    (IC.NE.OCAPW.AND.IC.NE.OCAPC.AND.IC.NE.OMINUS)) THEN  !
!          write(luscn,'(a)')
!          ' NEWPR07 - Invalid cable specification. Must be '
!     .           '<station ID><C,W,->')
!          IERR = -1
!          RETURN
!        END IF  !
!        ccable(is)=" "
!        ccable(is)(1:1)=ckeywd(2:2)
C
C     7. String parameter section.  Check freq code.
C
      else iF(ckey .eq. "FREQUENCY") then
        IF  (IGTFR(Ltoken(2),ICOD).EQ.0) THEN  !not selected
          write(luscn,'(a)')' NEWPR08 - Frequency code not selected.'
          IERR = -1
          RETURN
        END IF  !not selected
        icode_set_last=icod         !save the frequency code.
C
! PREOB,MIDOB,POSTOB section
      else if(ckey.eq."PREOB") then
         cpre=ctoken(1:6)
      else if(ckey .eq. "MIDOB") then
         cmid=ctoken(1:6)
      else if(ckey .eq. "POSTOB") then
         cpst=ctoken(1:6)
C
C     8. This is the subnet section.  Get next word as network,
C     then check each character against list.
C
      else IF  (ckey.eq.'SUBNET') THEN  !subnet
! AEM 20050303 substitute initialized variable every time insted of '1'
        ich_temp = 1
        call gtsti(ltoken,ich_temp,nstn,istn,ierr,luscn)
        if (ierr.ne.0) return
        KVIS = .TRUE.
      END IF
      goto 200

      END
