      SUBROUTINE MUVIS(LINSTQ,cmdcod)
C
C   MUVIS calculates and displays mutual visibility of sources.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  INPUT:
C     cmdcod - type of display, "MU" for mutual vis only,
C                              "SI" for individual site vis.
! functions
      integer iStringMinMatch
            
      integer*2 LINSTQ(*)
      character*2 cmdcod ! input string, word 1=length
C
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'major.ftni'
C
C     CALLING SUBROUTINES: SKED
C     CALLED SUBROUTINES: VISSS
C
C  LOCAL VARIABLES
      integer IUTSET(MAX_STN),IUTRIS(MAX_STN),mutset,mutris
C        IUTSET - minutes at setting
C        IUTRIS - minutes at rising
C     MUTSET - mutual setting
C     MUTRIS - mutual rising
      integer ihr,imr,ihs,ims
C        IHR,IMR,IHS,IMS
C               - hours,minutes at rising,setting
!      integer*2 LLINE(24)
      character*48 lline
C               - Output time line for station vis, mutual vis
      integer iline(48) ! holds number of stations per time slot
      integer ISTN(MAX_STN)
C      - station indices for which mutual vis wanted
      LOGICAL KTOTAL
      logical kup
      logical kplot_stat
C     NSTN - number of IDs in ISTN
C        M1,M2  - Indices for starting and stopping times (or vice versa)
      integer*2 LKEYWD(12)
      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      character*2 ckey
      integer ic1,nstn,i,ifc,iec,ikey,idum,n1,is,j,mjd,iso,
     .nch,ierr,luplt(MAX_STN),istcnt(MAX_STN)
      character*128 cplnam
      character*2 cnum
      logical kvis ! true, this is vis plots
      integer maxel,m1,m2,isor
      real*4 az,el,ha,dec,x30,y30,x85,y85,azd,eld,xd,yd,
     .xmin,ymin,xmax,ymax,elmax
      integer sonum,trimlen,ichmv,i2long !function
      character*36 lnumber/"  23456789abcdefghijklmnopqrstuvwxyz"/

      integer ilist_len
      parameter (ilist_len=15)
      character*8 list(ilist_len),listshort(ilist_len)

      data list/"DEFAULT","AZ",  "BASELINE","COVERAGE","DISTANCE","EL",
     >          "FILE","HIST",    "LINE",    "POLAZEL", "SNR",
     >          "STATS","XYAZEL", "TOTAL",   "MUPOL"/

      data listshort/"DE","AZ","BA","CO","DI","EL","FI","HI","LI",
     >      "PO","HS","ST","XY","TO","MU"/

C
C  MODIFICATIONS:
C     WEH  830713 CHANGE SOURCE LOOP TERMINATION TO NCELES SINCE
C                 SATELLITE VISIBLITY CALCULATIONS ARE NOT SUPPORTED YET
C     MWH  840813 Added printer LU lock
C     MWH  840828 Added option to display only times all stations see source
C     NRV  880310 DE-COMPC'D
C     NRV  890121 Moved check for TOTAL to IGTKY
C     NRV  890705 Added plot key words and call to plotting
C     GAG  891109 Added break subroutine, IFBRK, back in action
C     NRV  900228 Write header only for line displays
C     nrv  930225 implicit none
C     nrv  930616 Add plots
C     nrv  950404 Use 2-letter station codes
C 951017 nrv Change igtky call to use lkey, convert to ckey
C 981113 nrv Print 4-digit year
C
C
C     1. There are two loops over the selected sources and stations.  We cal
C     VISSS which returns the minutes of rising and setting.
C     Then print a line with the times for each station.
C

      IF  ((NSOURC.EQ.0).OR.(NSTATN.EQ.0)) THEN  !not enough info yet
        write(luscn,*)"MUVIS01 - Select sources and stations first."
        RETURN
      END IF  !not enough info yet
C
      IC1 = 1
      CALL GTSSI(LINSTQ,IC1,NSTN,ISTN,IERRCM,luscn)
      IF  (IERRCM.NE.0) RETURN
      IF  (NSTN.EQ.0) THEN  !all stations
        NSTN = NSUBST
        DO  I=1,NSTN
          ISTN(I) = ISUBST(I)
        END DO
      END IF  !all stations
C     Check for TOTAL key word or XY or AZEL plot
      KTOTAL = .FALSE.
      CALL GTFLD(LINSTQ(2),IC1,i2long(LINSTQ(1)),IFC,IEC)
      ikey=1            !default.
      IF (IFC.ne. 0) then
        ckeywd=" "
        IDUM = ICHMV(LKEYWD(1),3,LINSTQ(2),IFC,IEC-IFC+1)
        ikey = iStringMinMatch(list,ilist_len,ckeywd)
        if(ikey .eq. 0) then
          write(luscn,*) "MUVIS01: Invalid keyword. ",ckeywd
          return
        else if(ikey .eq. -1) then
          write(luscn,*) "MUVIS01b: Double match for keyword: ",ckeywd
          return
        endif
        ckey=listshort(ikey)
        if(list(ikey) .eq. "TOTAL") then
          ktotal=.true.
        endif
      endif

      if (list(ikey).eq.'XYAZEL'.or.list(ikey).eq.'POLAZEL') then
!        if (nstn.gt.8) then
!          write(luscn,'(a)')
!     >     'MUVIS02 - Too many stations, only 8 at a time.'
!          return
!         endif
        mjd=mjdcur(istn(1))
        kvis = .true.
C     Set up temp data files for each station and get horizon points
        do i=1,nstn !stations
          j=istn(i)
          istcnt(j)=0
          luplt(j) = 100+j
          write(cnum,'(a2)') cpocod(j)
          nch = trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cnum
          open(luplt(j),file=cplnam,status='unknown',iostat=ierr)
          xmin = 0.0
          xmax = 360.0
          ymin = 0.0
          ymax = 90.0
          call horpl(j,xmin,xmax,ymin,ymax,ckey,luplt(j))
          do iso=1,nsourc !sources
            if (isorcm.eq.0.or.iso.eq.isorcm) then !this source
              do ihr=1,24 !24 hours
                CALL CVPOS(iso,j,mjd,ihr*3600.d0,az,el,ha,
     .          DEC,X30,Y30,X85,Y85,KUP)
                IF (KUP) THEN 
                  azd = az*rad2deg
                  eld = el*rad2deg
                  if (ckey.eq.'XY') then
                    write(luplt(j),9607) sonum(iso),azd,eld
9607                format(i3,2f8.3)
                  else !PO
                    call azel2xy(azd,eld,xd,yd)
                    write(luplt(j),9607) sonum(iso),xd,yd
                  endif
                endif 
              enddo !24 hours
            endif !this source
          enddo !sources
        enddo !stations
        call sumpl(ckey,nstn,istn,xmin,xmax,ymin,ymax,luplt,istcnt,kvis)
      else if(list(ikey) .eq. 'MUPOL') then
        kplot_stat=.true.
        call soplt_pol(kplot_stat,istn,nstn)

      else !standard display
        N1 = ISTN(1)
        WRITE(LUDSP,9105) IYRCUR(N1),IDACUR(N1)
9105    FORMAT(' Source Visibility on ',I4,I3/'   for stations ',$)
        DO  IS=1,NSTN
          J = ISTN(IS)
          WRITE(LUDSP,9103) cSTNNA(J)
9103      FORMAT(1X,A8,' ',$)
          END DO  !
        WRITE(LUDSP,9104)
9104    FORMAT(1X)

        IF (cmdcod.EQ.'MU')  WRITE(LUDSP,9121)
9121  FORMAT(/14X,'  RISE   SET   ',
     .'|0     3     6     9     12    15    18    21    |'/
     .15X,'hh:mm  hh:mm  ',
     .'|------|-----|-----|-----|-----|-----|-----|-----|')
C
      ISOR = 0
C     DO WHILE (ISOR.LT.NCELES) !source loop
      DO WHILE (ISOR.LT.NSOURC) !source loop
          IF (ISORCM.GT.0) THEN ! single source specified
            IF (ISOR.EQ.ISORCM) GOTO 900   ! we're done
            ISOR = ISORCM
          ELSE
            ISOR = ISOR+1 !increment to next source
          ENDIF
          IF (cmdcod.EQ.'SI')
     .    WRITE(LUDSP,9120) ISOR,cSORNA(ISOR)
9120      FORMAT(/1X,I3,1X,A8,' RISE  SET  MAX ',
     .    '|0     3     6     9     12    15    18    21    |'/
     .    13X,'hh:mm hh:mm  EL ',
     .    '|------|-----|-----|-----|-----|-----|-----|-----|')
C
          DO  I=1,48
            iline(I)=0
          END DO
C
          CALL VISSS(ISOR,NSTN,ISTN,IUTRIS,IUTSET,MUTRIS,MUTSET)
C         If (IFBRK().LT.0) GOTO 900
          DO  IS=1,NSTN !station loop
              J = ISTN(IS)
C             Calculate max elevation of this source at this station
              if (isor.le.nceles) then ! max elevation
                ELMAX=PIov2-DABS(STNPOS(2,J)-sorp_now(2,ISOR))
                MAXEL = nint(ELMAX*rad2deg)
              else
                maxel = -1.0
              endif
! if these are both 0, source never rises.
              lline=" "
              IHR=IUTRIS(J)/60
              IMR=MOD(IUTRIS(J),60)
              IHS=IUTSET(J)/60
              IMS=MOD(IUTSET(J),60)
              if(iutset(j)+iutris(j) .ne. 0) then
                M1=min(1+iutris(j)/30,48)
                M2=min(1+iutset(j)/30,48)
                if(M1 .le. M2) then
                  do i=m1,m2
                   lline(i:i)="-"
                  end do
                else
                  do i=1,m2
                    lline(i:i)="-"
                  end do
                  do i=m1,48
                     lline(i:i)="-"
                  end do
                endif
                IF (M1.NE.M2) THEN
                  lline(m1:m1)="|"
                  lline(m2:m2)="|"
                END IF  !
              endif
              IF (cmdcod.EQ.'SI')
     .        WRITE(LUDSP,9200) cpoCOD(J),cSTNNA(J),
     .        IHR,IMR,IHS,IMS,MAXEL,LLINE
9200          FORMAT(1X,A2,1X,A8,2(1X,I2,':',I2),1X,I3,1X,'|',a48,'|')
C
            DO I=1,48
              if(lline(i:i) .eq. "-" .or. lline(i:i) .eq. "|") then
                 ILINE(I)=ILINE(I)+1
              endif
            END DO
C
          END DO  !station loop
C
        IF (NSTN.GT.1) THEN !true mutual vis
          IHR=MUTRIS/60
          IMR=MOD(MUTRIS,60)
          IHS=MUTSET/60
          IMS=MOD(MUTSET,60)
        ELSE !single station vis
          J=ISTN(1)
          IHR=IUTRIS(J)/60
          IMR=MOD(IUTRIS(J),60)
          IHS=IUTSET(J)/60
          IMS=MOD(IUTSET(J),60)
        END IF !true mutual vis 
C
          DO  I=1,48
            IF (KTOTAL.AND.ILINE(I).LT.NSTN) ILINE(I) = 0
            lline(i:i)=lnumber(iline(i)+1:iline(i)+1)
          END DO  !
          IF (cmdcod.EQ.'SI') then
            WRITE(LUDSP,9220) IHR,IMR,IHS,IMS,Lline
9220        FORMAT(' All stat''ns',2(1X,I2,':',I2),5X,'|',a48,'|')
          else IF (cmdcod.EQ.'MU') then
            WRITE(LUDSP,9221) ISOR,cSORNA(ISOR),IHR,IMR,IHS,IMS,LLINE
9221        FORMAT(1X,I3,1X,A8,2(2X,I2,':',I2),2X,'|',a48,'|')
          endif
      END DO  !source loop
900    continue
      endif !plotting/standard display
      RETURN
      END
