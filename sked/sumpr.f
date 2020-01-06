      SUBROUTINE SUMPR(LINSTQ,NST,IST,CTYPE,XMIN,XMAX,YMIN,YMAX,IERR)
C
C     SUMPR parses the input command line for the SUMMARY command.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'major.ftni'

! functions
      integer iStringMinMatch
      integer ichmv,ichcm_ch ! functions
C
C  INPUT
      integer*2 linstq(*)
C     - command string
C
C  OUTPUT
      integer ist(max_stn),nst,ierr
C     - array of subnet stations
C     nst - number of stations requested
C     ltype - type of output
      real*4 xmin,xmax,ymin,ymax
C     xmin,xmax,ymin,ymax - optional plotting limits
C     ierr - 0 for OK
C
C  LOCAL
      integer*2 lkeywd(12)
      character*2 ctype
      integer ich,nch,i,ifc,iec,idummy
      real*8 val,das2b

      character*24 ckeywd
      equivalence (lkeywd,ckeywd)

      integer ikey
      integer ilist_len
      parameter (ilist_len=12)
      character*12 list(ilist_len)
      character*2 listShort(ilist_len)

      data list/
     .'LINE','XYAZEL', 'POLAZEL','COVERAGE','DISTANCE',
     >'EL',  'AZ',     'FILE',   'BASELINE','STATS',
     >'HIST','SNR'/

      data listshort/
     >'LI','XY','PO','CO','DI','EL','AZ','FI','BA','ST','HI','SN'/

C
C  History
C  890623 NRV Created by removing from SUMCM
C             Disabled starting hour parameter
C  890624 NRV Added x,y limits
C  890714 NRV Fixed DAS2B call and values
C  910619 NRV Added HIST
C  910712 NRV Added SNR
C  931005 nrv Add COVERAGE
C  940216 nrv Add DISTANCE
C 951017 nrv Change igtky call to use lkey
! AEM 20050217 fix all messages (SUMCM->SUMPR)
C 
C
C        1. First, we pick the date-time range off the input line.
C
      ierr=0 
      ICH=1
      nch=linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,nch,IFC,IEC)
      LKEYWD(1) = IEC-IFC+1
      if (ifc.gt.0) IDUMMY = ICHMV(LKEYWD(2),1,LINSTQ(2),IFC,IEC)
      IF (IFC.EQ.0) LKEYWD(1)=0
      CALL GTDTR(LKEYWD,IERRCM)
      IF  (IERRCM.NE.0) THEN 
        CALL WRERR(IERRCM,INUMCM)
        ierr=1
        RETURN
      END IF
C
C     2. Get the second fields for sources, stations
C
      CALL GTSSI(LINSTQ,ICH,NST,IST,IERRCM,luscn)
      IF  (IERRCM.NE.0) THEN
        ierr=1
        RETURN
      END IF 
C
      IF  (NST.EQ.0) THEN  !default subnet stations
        NST=NSUBST
        DO  I=1,NST
          IST(I)=ISUBST(I)
        END DO
      END IF  !default subnet stations
C
C     3. Get starting hour for display
C
C     ISTIM=0
C     CALL GTFLD(LINSTQ(2),ICH,LINSTQ,IFC,IEC)
C     IF  (IFC.NE.0) THEN  ! starting hour specified
C       ISTIM = IAS2B(LINSTQ(2),IFC,IEC-IFC+1)
C       IF (ISTIM.LT.0.OR.ISTIM.GT.23) THEN !error
C         WRITE(LUSCN,8100) 
C8100      FORMAT('SUMCM01 - Error specifying starting hour. '
C    .    ' Must be 0 to 23.')
C         RETURN
C       ENDIF !error
C     END IF  ! starting hour specified
C     UTOFF = ISTIM*3600.D0
C 
C
C    4. Initialize optional parameters
C
      ctype = 'ST'
      xmin = -99.0
      ymin = -99.0
      xmax = -99.0
      ymax = -99.0
C
C    5. Check for type of display 
C 
      nch=linstq(1)
      call gtfld(linstq(2),ich,nch,ifc,iec) 
      if (ifc.eq.0) return

      nch=iec-ifc+1
      ckeywd = ' '
      idummy = ichmv(lkeywd,1,linstq(2),ifc,nch)
      ikey=istringMinMatch(list,ilist_len,ckeywd)
      if(ikey .eq. 0) then
        write(luscn, *) 'SUMPR01 - Invalid key word. Must be one of ',
     >  (list(i),i=1,ilist_len)
        ierr=1
        return
      else if(ikey .eq. -1) then
        write(luscn, *) 'SUMPR02 - Ambiguous key word.'
        ierr=1
        return
      endif


      ctype=listshort(ikey)
      if(ctype .eq. 'HS') ctype='SN'
C
C
C   6.  Optional plot limits
C
      nch=linstq(1)
      call gtfld(linstq(2),ich,nch,ifc,iec)
      if (ifc.ne.0) then !x-min
        nch=iec-ifc+1
        if (nch.eq.1.and.ichcm_ch(linstq(2),ifc,'_').eq.0) then !default
          xmin = -99.0
        else !value
          val=das2b(linstq(2),ifc,nch,ierr)
          if (val.lt.0.d0.or.ierr.ne.0) then !error
            write(luscn,'("SUMPR03 - Error in x-min limit.")')
            ierr=1
            return
          endif
          xmin=val
        endif !default/value
        nch=linstq(1)
        call gtfld(linstq(2),ich,nch,ifc,iec)
        if (ifc.ne.0) then !x-max
          nch=iec-ifc+1
          if (nch.eq.1.and.ichcm_ch(linstq(2),ifc,'_').eq.0) then !default
            xmax = -99.0
          else !value
            val=das2b(linstq(2),ifc,nch,ierr)
            if (val.lt.0.d0.or.ierr.ne.0) then !error
              write(luscn,'("SUMPR04 - Error in x-max limit.")')
              ierr=1
              return
            endif
            xmax=val
          endif !default/value
          nch=linstq(1)
          call gtfld(linstq(2),ich,nch,ifc,iec)
          if (ifc.ne.0) then !y-min
            nch=iec-ifc+1
            if (nch.eq.1.and.ichcm_ch(linstq(2),ifc,'_').eq.0) then !default
              ymin = -99.0
            else !value
              val=das2b(linstq(2),ifc,nch,ierr)
              if (val.lt.0.d0.or.ierr.ne.0) then !error
                write(luscn,'("SUMPR05 - Error in y-min limit.")')
                ierr=1
                return
              endif
              ymin=val
            endif !default/value
          nch=linstq(1)
            call gtfld(linstq(2),ich,nch,ifc,iec)
            if (ifc.ne.0) then !y-max
              nch=iec-ifc+1
              if (nch.eq.1.and.ichcm_ch(linstq(2),ifc,'_').eq.0) then !default
                ymax = -99.0
              else !value
                val=das2b(linstq(2),ifc,nch,ierr)
                if (val.lt.0.d0.or.ierr.ne.0) then !error
                  write(luscn,'("SUMPR06 - Error in y-max limit.")')
                  ierr=1
                  return
                endif
              ymax=val
              endif !default/value
            endif !y-max
          endif !y-min
        endif !x-max
      endif !x-min
      if (ctype.eq.'DI') then
        xmin=0.0
        xmax=180.0
        ymin=0.0
        ymax=12.0
      endif
      if (ctype.eq.'EL'.or.ctype.eq.'LI'.or.ctype.eq.'BA'
     .  .or.ctype.eq.'AZ'.or.ctype.eq.'ST'.or.ctype.eq.'FI'
     .  .or.ctype.eq.'HI'.or.ctype.eq.'SN'.or.ctype.eq.'CO') then 
        if (xmax.eq.-99.0) xmax=24.0
      else if (ctype.eq.'XY'.or.ctype.eq.'PO') then
        if (xmax.eq.-99.0) xmax=360.0
      endif
      if (ctype.eq.'EL') then
        if (ymax.eq.-99.0) ymax=90.0
      else if (ctype.eq.'AZ') then
        if (ymax.eq.-99.0) ymax=360.0
      endif
      if (xmin.eq.-99.0) xmin=0.0
      if (ymin.eq.-99.0) ymin=0.0
      if (ymax.eq.-99.0) ymax=90.0
      if (xmin.ge.xmax) then
        write(luscn,'("SUMPR07 - xmin >= xmax.")')
        ierr=1
        return
      endif
      if (ymin.ge.ymax) then
        write(luscn,'("SUMPR08 - ymin >= ymax.")')
        ierr=1
        return
      endif
C
      return
      end
