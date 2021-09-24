      subroutine whatsup(istnvec,NumStn,iSrcVec,NumSrc,MJDFree,UtFree,
     >  ielall,kmin)
! extracted from old nextc.
!  2021-09-24 Gipson Renamed NOBSSO to  NumObsSource
!  2021-04-02 JMG   Fixed IEEE denormal issue caused by checking elevation for  source that had not risen yet.
!                   (So no elevation defined.)

!  2003Nov13 JMGipson.  Added ispindelay call to tspin
!  2005Jun13 JMGipson.  When computing time difference, include duration of scans and slew time.
!  2007Sep24 JMGipson.  Added MJDFree to above call. Made change MJDCur-->MJDFree in code as appropriate.
!  2018Jan02 JMGipson.  Put in twin mod stuff. 

      use twin_mod 
      use obs_scan_counters
      implicit none 

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'major.ftni'
      include 'minor.ftni'
      include 'covar.ftni'
  
! functions
      real tspin !time to spin a given distance.
      double precision dsecdif          !double precision differenc in times.
      real speed                        !tape speed
      integer iwhere_in_string_list 

! passed variables
      integer NumStn,NumSrc
      integer istnvec(*)
      Integer IsrcVec(*)
      integer ielall(max_sor,max_stn)   !used for VLBA stations in special mode
      integer MJDFree(max_stn) !Day part of when station is free
      double precision UtFree(Max_stn)  !seconds part


      logical kmin                      !kmin    minimum display
! local variables
      logical ktoggle 
      integer Mjdlast
      double precision utlast
      double precision uttemp

      double precision dSecPerDay
      parameter (dSecPerDay=86400.d0)

      real TSLEW(max_sor,MAX_STN)       !slew times for each antenna to source.
      real tslew_all(max_sor)           !minimum time for all stations to arrive on source.

      logical ksrcup(max_sor)           !is src up at some antennas?

      integer ITR(MAX_SOR,MAX_STN)
      integer ITS(MAX_SOR,MAX_STN)      !time to rise, set in seconds.
      integer iokst(max_stn)            !returned by SNROK
      integer ncol                      !number of columns
      integer niter                     !number of iterations
      integer iter                      !iter counter
      integer ifst,ilst                 !first and last stations.
      integer icod
      integer ihr,imin,isec              !time units

      integer ipar,itmscn,idurre
      integer iFTREM(MAX_STN)           !feet remaining
      double precision tdiff(max_sor)   !difference in times
      real az,el,ha,x30,y30,x85,y85,dec  !used in cvpos
      integer iel                       !elevation in degrees
      real har
      logical kup                       !is antenna up
      integer istn                      !station index
      integer isrc                      !source index

      character*8 lhms                  !something like 12:32:14

      double Precision rMaxSlewTime
      parameter (rMaxSlewTime=600.d0)

      integer itemp                     !temporary variable
      double precision temp
      integer i,j                       !loop counters

      CHARACTER*11 CBLnk,CBLF            !format statements for writing blank fields for min,std,fixed
      integer is,ifix1,ivars,ivarm,nrise,nset
      integer ispinDelay
      integer istat1, istat2
      
      ispinDelay=0

      irs=0                                  !Reset the rise set array.
      kvs =.false.         !Initialize station visibility to false.

      call FindAllSlewTimes(istnvec,NumStn,IsrcVec,NumSrc,
     > MJDFree,UtFree, tslew,tslew_all,itr,its, ksrcup)
C
C     5.  Set column widths here, and create nnX formats
C
      IFIX1=25
      IVARS=16
      IVARM=12
      cblf='(26x," ",$)'
      if(kmin) then
        cblnk='(11x," ",$)'
      else
        cblnk='(15x," ",$)'
      endif
      NCOL=(IWSCN-IFIX1)/(IVARS+1)
      IF (KMIN) NCOL=(IWSCN-IFIX1)/(IVARM+1)
      NITER=(NumStn+NCOL-1)/NCOL

C     6. Output display
C
      icod=1 ! *** only 1 for now
      if (kdiswh) write(ludsp,9601) ccode(icod),cnafrq(icod)
9601  format(' WHATSUP display for frequency code ',a2,' (',a8,')')
      ILST = 0
! AEM 20050221 init var 'ifst'
      IFST = 0
      DO ITER= 1,NITER ! iteration for once across page
        IFST = ILST+1
        ILST = MIN0(NumStn,IFST+NCOL-1)          
C
C     6.1 Line with station names
C
        if (kdiswh) then
          WRITE(LUDSP,'()')
          WRITE(LUDSP,CBLF)
        endif
        if(kdiswh) then
         
          do i=ifst,ilst
            J = istnvec(I)
            if(kmin) then
              WRITE(LUDSP,'(5X,A2,5X," ",$)') cpoCOD(J)
            else
              WRITE(LUDSP,'("   ",A2,"(",A,")","  ",$)')
     .	                  cpoCOD(J),cSTNNA(J)
            endif
          END DO  !line with names
          WRITE(LUDSP,'()')
        endif
C
C    6.2  Lines with end time and footage of current observation
C
      if (kdiswh) then
        WRITE(LUDSP,'("      End of current obs:    ",$)')
      endif
      DO  I=IFST,ILST ! line with status
        J = istnvec(I)
        call seconds2hms(UtFree(j),ihr,imin,isec)
        if(ihr .ge. 24) ihr=ihr-24
        if (nsorcur(j).eq.0) then !not initialized
          iftend_cur(j) = 0
        endif
        if (kdiswh) then !end time of current obs
          write(lhms,'(i2.2,":",i2.2,":",i2.2)') ihr,imin,isec
          IF (.NOT.KMIN) THEN
            WRITE(LUDSP,'("    ", a,"     ",$)') lhms
          else
            WRITE(LUDSP,'(2X,a,"   ",$)') lhms
          ENDIF !ending footage
        endif
      END DO  !line with status

      if (kdiswh) WRITE(LUDSP,'()')

C
C     7. Loop over all sources, one line per source.
C    7.1 Header line
C
      if (kdiswh) then
        WRITE(LUDSP,'("  # Source   Scan Last    Obs ",$)')
        DO  I=IFST,ILST
          IF (.NOT.KMIN) THEN !standard
            WRITE(LUDSP,'(" H.A. Az El  Sl  ",$)')
          ELSE !minimum
            IF (IAXIS(istnvec(I)).EQ.1) then
              WRITE(LUDSP,'(" H.A. El  Sl ",$)')
            else
              WRITE(LUDSP,'(" Az  El  Sl  ",$)')
            endif
          ENDIF !standard/minimum
        ENDDO
        WRITE(LUDSP,'()')
      endif
C
C    7.2 Source lines
C

      istn=istnvec(1)
      Mjdlast=mjdfree(istn)
      utlast =utfree(istn)
      do i=2,numstn
        istn=istnvec(i)
        if(dsecdif(mjdfree(istn),utfree(istn),mjdlast,utlast).ge.0) then
           mjdlast=mjdfree(istn)
           utlast=utfree(istn)
        endif
      end do

      DO  I=1,NumSrc ! source loop for display
        IS=iSrcVec(I)
! Compute time since source last observed.
        j=istcur(1)
        IF (MJPRSO(IS).eq.0) then
         TDIFF(is)=0d0
         else
          TDIFF(is)= dsecdif(Mjdlast,utlast, mjprso(is),UTPRSO(IS))
         ENDIF
        call seconds2hms(tdiff(is),ihr,imin,isec)
        IF (ksrcup(is)) THEN  !display this source
          if (kdiswh) then
            WRITE(LUDSP,9721) IS,cSORNA(IS),ISSCAN(IS)
9721        FORMAT(I3,1X,A8,1X,I4,' ',$)
            IF (NumObsSource(IS).EQ.0) THEN !it's up but no obs
              WRITE(LUDSP,9722)
9722          format(11x,'|',$)
            ELSE
              WRITE(LUDSP,9723) IHR,IMIN,isec,NumObsSource(IS)
9723          FORMAT(I2.2,':',I2.2,':',i2.2,I3,'|',$)
            ENDIF !it's up but no obs
          endif
C         If no observations on this source so far, re-initialize
C         the difference to look as if it's been observed far enough
C         in the past that it's now available again.
          IF (MJPRSO(IS).eq.0) TDIFF(is)=iminbetween+1.0
C
C   Compute SNR and duration for stations and baselines
          if (kvscan)   CALL SNROK(istnvec,NumStn,is,icod,-1,iokst,
     >                              mjdcur(j),utcur(j))
          DO  J=IFST,ILST ! one station entry                   
            istn = istnvec(J)
            el=-1                       !Set source elevation to negative. Default not up
            if (.not.kasnr.or.(kasnr.and.iokst(j).ge.0)
     >      .or..not.kvscan.or.kOptBySky.or.(num_est.ne.0)) then
C                            Include station if manual SNR or auto and OK
              IF (ITR(IS,istn).GT.0) THEN !rising
                if (kdiswh) then
                  itemp=itr(is,istn)        
                  IF (KMIN) Then
                    WRITE(LUDSP,'(" rise ",I5," ",$)') itemp
                  ELSE
                    WRITE(LUDSP,'(" rise ",I5," s"," ",$)') itemp
                  ENDIF
                endif
                irs(is,istn)=+2   !will rise within lookahead
                ITR(IS,istn)=-1   !indicate we've already written it
              ELSE !calculate position
                IF (TSLEW(is,istn).LT.0.0) THEN ! not up
                  if (kdiswh) write(ludsp,cblnk)   
                  IF (ITS(IS,istn).GT.0) ITS(IS,istn)=-1 !it's already set
                ELSE ! this one up  
! Compute the elevation here so that we can use it later.
                  CALL CVPOS(IS,istn,MJDFree(istn),
     >                   UtFree(istn)+tslew(is,istn),
     >                   AZ,EL,HAR,DEC, X30,Y30,X85,Y85,KUP)
                  if (knov) then
                    ielall(is,istn)=0
                    if (kup) ielall(is,istn)=10
                    kup=.true.
                  endif

                  IF  (KUP) THEN ! up at end too
                    kvs(is,istn)=.true. ! source is visible
                    if (kdiswh) then !calculate position to display
                      HA=HAR*12.0/PI
                      IF (.NOT.KMIN.OR.(KMIN.AND.IAXIS(istn).EQ.1)) THEN !HA
                        IF (HA.LT.-9.99) THEN
                          WRITE(LUDSP,'(F4.0,$)') HA
                         ELSE
                          WRITE(LUDSP,'(F4.1,$)') HA
                        ENDIF
                      ENDIF !HA
                      IF (.NOT.KMIN.OR.(KMIN.AND.IAXIS(istn).NE.1)) THEN !AZ
                        WRITE(LUDSP,'(1X,I3,$)') nint(AZ*rad2deg)
                      ENDIF !AZ
                      WRITE(LUDSP,'(I3,$)') nint(EL*rad2deg)
                    endif
                    IF (IS.EQ.NSORcur(istn)) THEN !slew time
                      kvs(is,istn)=.false. !don't observe same source twice in a row
                      if (kdiswh) WRITE(LUDSP,'(" *** ",$)')
                    ELSE
                      if (kdiswh) then
                        Temp=TSLEW(is,istn)/60.
                        IF (temp.LT.10.0) then
                          WRITE(LUDSP,'(F4.1," ",$)') temp
                        ELSE
                          WRITE(LUDSP,'(F4.0," ",$)') temp
                        ENDIF
                      endif
                    ENDIF !slew time
                  ELSE  !not up at end
                    if (kdiswh) WRITE(LUDSP,CBLnk)
                    IF (ITS(IS,istn).GT.0) ITS(IS,istn)=-1 !it's already set
                  END IF !up/not up at end
                END IF  !this one up
              ENDIF !rising/calculate
            else ! write blanks
              if (kdiswh) WRITE(LUDSP,CBLnk)
            endif !include this station
            if (kdiswh) WRITE(LUDSP,'("|",$)')
C    At this point determine the rise/set flags and whether to re-set
C    the visibility flag to false.

            if (tdiff(is).lt.float(iminbetween)) then
               kvs(is,istn)=.false.
            endif

C           In any case, don't observe the same source twice in a row.
C           If the source is up now (vs=true) and was due to rise (irs=+2), set
C           the flag to +1=just risen.
            if (kvs(is,istn).and.irs(is,istn).eq.+2) irs(is,istn)=+1 ! just risen
C           Next check whether we're still within the lookahead time. If that time
C           is now gone, then change the rise/set flag to 0=no longer just-risen.
            if (kvs(is,istn).and.irs(is,istn).eq.+1) then
              if (itr(is,istn) .gt. lookah) irs(is,istn)=0
            endif
          END DO  !one station entry
          if (kdiswh) WRITE(LUDSP,'()')
        END IF  !display this source
C
        NRISE=0
        NSET=0
        DO J=IFST,ILST
          istn=istnvec(J)
          IF (ITR(IS,istn).GT.0) NRISE=NRISE+1
          IF (ITS(IS,istn).GT.0.AND.ksrcup(is)) NSET=NSET+1
        ENDDO
C
        IF (NRISE.GT.0) THEN !rising at some stations
          IF (.not.ksrcup(is)) then !not up anywhere
            if (kdiswh) then
              WRITE(LUDSP,9721) IS,cSORNA(IS),ISSCAN(IS)
              IF (NumObsSource(IS).EQ.0) THEN !it's up but no obs
                WRITE(LUDSP,9722)
              ELSE
                WRITE(LUDSP,9723) IHR,IMIN,isec,NumObsSource(IS)
              ENDIF !it's up but no obs
              DO J=IFST,ILST
                istn=istnvec(J)
                IF (ITR(IS,istn).GT.0) THEN
                  itemp=ITR(IS,istn)  
                  IF (KMIN) then
                    WRITE(LUDSP,'(" rise ",I5,"s",$)') itemp
                  ELse
                    WRITE(LUDSP,'(" rise ",I5,"s",$)') Itemp
                  ENDIF
                ELSE
                  write(ludsp,cblnk)
                ENDIF
                WRITE(LUDSP,'("|",$)')
              ENDDO
              WRITE(LUDSP,'()')
            endif
          ENDIF !not up anywhere
        ENDIF !rising at some stations
C
        IF (NSET.GT.0) THEN !setting at some stations
          if (kdiswh) then
             write(ludsp,'(29x,"|",$)')
          endif
          DO J=IFST,ILST
            istn=istnvec(J)
            IF (ITS(IS,istn).GT.0) THEN
              irs(is,istn)=-1 ! source is setting within lookahead time
              if (kdiswh) then
                itemp=ITs(IS,istn)   
                IF (KMIN) then
                   WRITE(LUDSP,'(" set ",I5,"s ",$)') itemp
                Else
                   WRITE(LUDSP,'(" set ",I5,"s ",$)') itemp
                ENDIF
              endif
            ELSE
              if (kdiswh) write(ludsp,cblnk)
            ENDIF
            if (kdiswh) WRITE(LUDSP,'("|",$)')
          ENDDO
          if (kdiswh) WRITE(LUDSP,'()')
        ENDIF !setting at some stations
      END DO  !source loop for display
      END DO  !iteration for once across page


! See if difference between last endtime of current station and time it arrives
! at source is longer than rMaxSlewTime. If so, mark the source as "invisible"
      do j=1,NumStn
        istn=istnvec(j)
        do i=1,NumSrc
          isrc=iSrcVec(i)
          uttemp=Utfree(istn)+tslew(isrc,istn)
          if(dsecdif(MJDFree(istn),uttemp,MJDLast,utlast) 
     >      .gt. rMaxSlewtime) then
             kvs(isrc,istn)=.false. 
          endif
        enddo
      enddo

! Here is where we acount for the twin-telescopes
      ktoggle=.false.
      do i=1,Num_Twins
        write(*,*) "Here", twin_list(i)%twinsplit 
        if(twin_list(i)%twinsplit .eq. "SPLIT") then   
          istat1=
     >     iwhere_in_string_list(cstnna,nstatn,twin_list(i)%stat1_name)
          istat2=
     >     iwhere_in_string_list(cstnna,nstatn,twin_list(i)%stat2_name)
          do isrc=1, NumSrc
            if(kvs(isrc,istat1) .and. kvs(isrc,istat2)) then
               if(ktoggle) then
                 kvs(isrc,istat1) = .false.
               else
                 kvs(isrc,istat2) = .false.
              endif
              ktoggle=.not.ktoggle 
           endif
         end do
        endif
      end do           
  




      end
