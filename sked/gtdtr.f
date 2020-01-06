      SUBROUTINE GTDTR(LINDTQ,KERR)!GET DATE TIME RANGE C#870112:14:55#
C
C  GTDTR decodes the date-time range as required by commands
C
      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE 'skcom.ftni'
! function
      double precision hms2seconds
      integer julda

C
C  INPUT VARIABLES:
      integer*2 LINDTQ(*)
C               - input DTR string as typed by user, word 1=length.
C
C  OUTPUT VARIABLES:
      integer KERR   ! Set to 1 if error occurred, 0=OK
C
C  COMMON BLOCKS USED
C
C  CALLING SUBROUTINES: LICMD,ACCMD,COMND
C  CALLED SUBROUTINES: LNFCH ROUTINES
C
C  LOCAL VARIABLES
C        IPOS   - character position, returned by IPOSQ
      character*60 cin          !internal string
      character*30 cdt(2)

      integer   IYR, IDAY, IHR, IMIN, ISEC
C               - holders for year, day, hour, minute, seconds  as decoded
      integer   ic1,ifc,iec,ipos,nch
      integer   IOBS   ! number of observations, as decoded
C               - String holder for date/time: YYYYDDDHHMMSS
      integer num_time  !number of times we have.
      integer itime

! Input string can be of the form:
!   1.    .          current
!   2.    ^          first
!   3.    #10        from current to next 10
!   4.    .-#10      from current to next 10
!   5.    ^-#10      from top to next 10
!   6.    timestring-#10   from time string to next 10.
!   7.    timestring-timestring
C
C     INITIALIZED:
! 2006Nov15  JMGipson.  Rewritten.
! 2010Mar26 JMG. Changed stutcm ->utstcm, enutcm->utencm for consistency with jdstcm and jdencm

      IC1 = 1
      nch=lindtq(1)
      CALL GTFLD(LINDTQ(2),IC1,nch,IFC,IEC)

      cin="_"
      if(ifc .ne. 0) then
        nch=min(30,iec-ifc+1)
        call hol2char(lindtq(2),1,nch,cin)
      endif

      if(cin(1:1) .eq. "_") then    !this means not set (ifc =0), or set to "_"
        cin="^-*"
      endif
      cdt=cin(1:30)

! Now parse the string.
      ipos=index(cin,"-")
      if(ipos .ne. 0) then
        cdt(1)=cin(1:ipos-1)
        cdt(2)=cin(ipos+1:)
        num_time=2
      else
        num_time=1
        cdt(1)=cin
      endif
      nobscm=0
      UTenCM = 0.D0
      JDENCM = 0.D0

      UTstCM = 0.D0
      JDSTCM = 0.D0

! Check to see if have a # in the first slot.
      ipos=index(cdt(1),"#")
      if(ipos .ne. 0) then
        read(cdt(1)(2:),*,err=990) iobs
        if(iobs .lt. 0) goto 990
        nobscm=iobs
        if(num_time .eq. 1) then
          cdt(1)="."        !set to current
        else
          cdt(1)=cdt(2)
          num_time=1
        endif
      endif
! Now check second slot
      ipos=index(cdt(2),"#")
      if(ipos .ne. 0) then
        read(cdt(2)(2:),*,err=990) iobs
        if(iobs .lt. 0) goto 990
        nobscm=iobs
        num_time=1
      endif

C     3.  First right-justify the DTR string in the variable LDTR.
C         Then call YDHMS.
C         Then we have set up the starting JD and UT variables.
C
300   continue
      do itime=1,num_time
        CALL YDHMS(cdt(itime), KERR, IYR, IDAY, IHR, IMIN, ISEC)
        IF (KERR .EQ. 1) THEN
          if(itime .eq. 1) then
             KERR=5
          else
             kerr=7
          endif
          RETURN
        ENDIF
        if(itime .eq. 1) then
          JDSTCM = JULDA(1, IDAY, IYR-1900)
          utstcm =hms2secondS(ihr,imin,isec)
        else
         JDENCM = JULDA(1,IDAY,IYR-1900)
         utencm = hms2seconds(ihr,imin,isec)
        endif
      end do
      IF ((UTenCM.EQ.0.D0).AND.(JDENCM.EQ.0).AND.(NOBSCM.EQ.0)) NOBSCM=1
C
  990 RETURN
      END
