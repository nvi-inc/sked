      SUBROUTINE vscout
C
C  This routine writes out scan blocks in the VEX $SCHED section.
C
C   HISTORY:

! 2021-11-19 JMG got rid of all references to 'feet' which are no longer used. 
C 990923 nrv New. 
C 990927 nrv Writing version 1.5 for now.
C 991020 nrv Use station index within scan for dur and footage.
C 991020 nrv Change algorithm for determining pass.
C 991206 nrv Remove sequence number from scan ID per Haystack request.
C 000120 nrv Change scanID to have source name.
C 000522 nrv Change scanID to be record number.
C 000601 nrv Change scanID to be ddd-hhmm.
C 000602 nrv Call SNAME utility to create scan name (also called by drudg)
C 000619 nrv Scan names already created by obs_sort, just use them.
C 000711 nrv Order station list within scans by full list order.
C 000724 nrv Fix mixup with j/istn/k indices writing out scan info.
C 000929 nrv Adjust scan time so that station offset times and >= zero.
C 001006 nrv Also need to adjust the data-stop time so that it too is
C            referenced to the new ref time.
C 001109 nrv scan_name is character now.
! 2005Oct28 JMGipson. Got rid of ichcm_ch
! 2018Oct00 JMG. Hardcoded pass etc to be '1A'
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/freqs.ftni'
! function
      double precision hms2seconds      !convert hms to seconds
      integer igetsrcnum                !get the source number
      integer igetstatnum               !get the station number

      integer igtfr,julda,ias2b,ichmv,ptr_ch
      integer*4 isecdif ! function
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      character*36 c_subpass
      integer istn,is,irec,irecst(max_stn),ist(max_stn),ih,ip,ndx
      character*128 cscan_id,cid
      character*20 cso,cstat
C     integer*2 scan_name(8),scan_namep(8)
      character*128 ccal,cdatab,cfeet,cdatae,crece,
     .              cpo,cpa,cdrv,cpt
      integer*2 lsrcnam(max_sorlen/2)
      character*(max_sorlen) csrcnam
      equivalence (lsrcnam,csrcnam)
      equivalence (cso,csrcnam)

      integer*2 LPRE(3),LMID(3),LPST(3)

      character*1 c1
      integer itemp
      equivalence (c1,itemp)

      integer*2 LST(MAX_STN),ICB(MAX_STN)
      character*2 lcb(max_stn),cst(max_Stn)
      equivalence (icb,lcb),(cst,lst)
      integer iptr

      integer IDUR(MAX_STN)
      integer idstop(max_stn)
   
      integer ihd(max_pass),nh
      integer ich,ic1,ic2,idumy,ical,lfrq,iyr,ida,ihr,imin,isc,mjd,
     .idurx,idle,i,nst,nch,ni,k,j,isor,icod
      integer iyr_ref,ida_ref,ihr_ref,imin_ref,isc_ref
      integer iyr1(max_stn),ida1(max_stn),ihr1(max_stn),
     .        imin1(max_stn),isc1(max_stn)
      integer iyr4(max_stn),ida4(max_stn),ihr4(max_stn),
     .        imin4(max_stn),isc4(max_stn)
      integer ideltat(max_stn),ideltatmax,ideltatmaxj,ioffset(max_stn)
      double precision ut1(max_stn),ut,utpre(max_stn)
      real tslew,dum
      integer itu_early(max_stn)
      logical knewtp,knewt
      integer*2 lcbpre(max_stn),lcb_new
      integer isorp(max_stn)
      integer mjdpre(max_stn),mjd1(max_stn)
     
C  1. SCHED

      call fcreate_block(ptr_ch("SCHED"//char(0)))
      write(luscn,'("SCHED")')

C  2. Create scan ID for block name. Create scan block.

      cso=" "      !initialize to blanks
      cstat=" "    !initialize to blanks 
      IREC = 0
      do i=1,nstatn ! initialize count of scans for each station
        irecst(i)=0
      enddo ! initialize count of scans for each station
C     call ifill(scan_namep,1,16,oblank)
      DO WHILE (IREC.LT.NOBS) !read and write an observation
        IREC = IREC+1
C       Fill up IBUF from memory array, using index array
        cbuf=cskobs(iskrec(irec))
C*********************UNPAK************************
C Example:
C sor  cal co       yydddhhmmss dur       po       Stations footages dur
C 3C84 120 SX PREOB 80092120000 780 MIDOB 0 POSTOB K-F-G-OW 1F000
C LSN ical lfrq lpre           idurx lmid idle lpst lst-icb          idur
C                   iyr ida ihr imin isc, mjd UT            ipas,idir,ift
C
      ICH = 1
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
!      CALL IFILL(LSN,1,max_sorlen,oblank)
      csrcnam=" "
      IDUMY = ICHMV(lsrcnam,1,IBUF,IC1,MIN0(IC2-IC1+1,max_sorlen))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      ICAL = IAS2B(IBUF,IC1,IC2-IC1+1)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDUMY = ICHMV(LFRQ,1,IBUF,IC1,2)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      CALL IFILL(LPRE,1,6,oblank)
      IDUMY = ICHMV(LPRE,1,IBUF,IC1,MIN0(IC2-IC1+1,6))

      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      read(cbuf(ic1:ic2),'(i2,i3,i2,i2,i2)') iyr,ida,ihr,imin,isc
      if (iyr.ge.00.and.iyr.le.49) iyr=iyr+2000
      if (iyr.ge.50.and.iyr.le.99) iyr=iyr+1900
      MJD = JULDA(1,IDA,IYR-1900)
      ut=hms2seconds(ihr,imin,isc)

      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDURX = IAS2B(IBUF,IC1,IC2-IC1+1)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      CALL IFILL(LMID,1,6,oblank)
      IDUMY = ICHMV(LMID,1,IBUF,IC1,MIN0(IC2-IC1+1,6))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDLE = IAS2B(IBUF,IC1,IC2-IC1+1)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      CALL IFILL(LPST,1,6,oblank)
      IDUMY = ICHMV(LPST,1,IBUF,IC1,MIN0(IC2-IC1+1,6))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      nst=0
      iptr=ic1
      do while(iptr .lt. ic2 .and. nst .lt. max_stn)
        nst=nst+1
        cst(nst)=cbuf(iptr:iptr)
        iptr=iptr+1
        lcb(nst)=cbuf(iptr:iptr)
        iptr=iptr+1
      END DO  !
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      I = 1
      DO WHILE (IC1.NE.0.AND.I.LE.NST) ! decode footage counters
  
        nch = ic2-ic1+1
!        read(cbuf(ic1+2:ic2),*) ift(i)

        CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
        I = I+1
      END DO  !decode footage counters
      IF  (I.EQ.1) THEN  !no counters
        DO  NI = 1,NST   
!          IFT(NI) = 0
        END DO  !
      END IF  !no counters
      IF  (I.GT.1.AND.I.LT.(NST+1)) THEN  !too few counters
        DO  NI = I,NST   
!          IFT(NI) = IFT(I-1)
        END DO  !
      END IF  !too few counters
C Skip over procedure flags
C The GTFLD call was already done in the footage loop, so IC1
C points to the first duration
C Start reading durations
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      I = 1
      DO WHILE (IC1.NE.0.AND.I.LE.NST)  !decode durations
        IDUR(I) = IAS2B(IBUF,IC1,IC2-IC1+1)
        CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
        I = I+1
      END DO  !decode durations
      IF (I.EQ.1) THEN !no durations
        DO I=1,NST
          IDUR(I)=IDURX
        ENDDO
      ENDIF !no durations
C*********************UNPAK************************
C Example:
C sor  cal co       yydddhhmmss dur       po       Stations footages dur
C 3C84 120 SX PREOB 80092120000 780 MIDOB 0 POSTOB K-F-G-OW 1F000
C LSN ical lfrq lpre           idurx lmid idle lpst lst-icb          idur
C                   iyr ida ihr imin isc, mjd UT            ipas,idir,ift

C Create the scan id
C       write(scan_id,'(i3.3,"_"i3.3,"-",2i2.2)') irec,ida,ihr,imin
C       nnn_ddd-hhmm
C       write(scan_id,'(i3.3,"-",2i2.2)') ida,ihr,imin
C       ddd-hhmm
C       if (scan_id(1:8).eq.scan_idp(1:8)) then ! duplicate scan
C         if (scan_idp(9:9).eq.' ') then ! first one
C           scan_id(9:9) = 'a'
C         else
C           ix = ichar(scan_idp(9:9)) + 1
C           scan_id(9:9) = char(ix)
C         endif
C       endif ! duplicate scan
C       ddd-hhmm_source
C***** Old scan name format with source names
C       write(cscan_id,'(i3.3,"-",2i2.2,"_",20a2)') ida,ihr,imin,
C    .  (lsn(i),i=1,max_sorlen/2)
C***** scannnn format
C       write(scan_id,'("s",i3.3)') irec
C       call sname(ida,ihr,imin,scan_namep,scan_name)
C       idumy = ichmv(scan_namep,1,scan_name,1,16) ! save previous scan name
C**** New scan name format
C       Use names determined by sorting program sorting.
C       call hol2char(scan_name(1,iskrec(irec)),1,9,cscan_id)
        cscan_id = scan_name(iskrec(irec))
        
C Check each field for validity before writing out this scan.
        IF  (IGTFR(LFRQ,ICOD).EQ.0) then !no match on freq code
          WRITE(LUSCN,9220) LFRQ,cscan_id(1:9)
9220      FORMAT('VSCOUT03 - Frequency ',A2,' not selected.  ',
     .    'Scan ',a,' ignored.')
          goto 999
        endif !no match on freq code
        isor=igetsrcnum(csrcnam)
        IF  (isor .eq. 0) then
          WRITE(LUSCN,9200) csrcnam,cscan_id(1:9)
9200      FORMAT('VSCOUT01 - Source ',A,' not selected. Scan ',a,
     >    ' ignored.')
          goto 999
        endif ! no match on source name
        DO  J=1,NST !check station names
          itemp=lst(j)
          is=igetstatnum(c1)
          IF  (is.NE.0) THEN
            ist(J) = IS 
            irecst(is) = irecst(is)+1
          ELSE  !no match
            WRITE(LUSCN,9210) c1,cscan_id(1:9)
9210        FORMAT('VSCOUT02 - Station ',A1,' not selected. ',
     .      'Scan ',a,' ignored.')
            goto 999
          END IF  !no match
        END DO  !check station names

C Determine reference time for the scan. This time will be the
C same as the scan line time for start&stop. For continuous, the
C reference time will be the earliest of the start times, so that
C all the offsets can be positive.
C Use (is) indices where values have been saved from the previous scan.
C Use (j) indices for within-scan values.
C************************************************
C NOTE: This logic assumes that all stations are either S&S or CONT.
C I didn't take the time to figure out how to handle mixed modes.
C***********************************************
        if (tape_motion_type(1).eq.'START&STOP') then ! scan is ref
          iyr_ref=iyr
          ida_ref=ida
          ihr_ref=ihr
          imin_ref=imin
          isc_ref=isc
          do j=1,nst
            is=ist(j)
!            iftnew(is)=ift(j)
            idstop(is) = idur(j)
            ioffset(is)=0
          enddo
        else ! find new reference time, adjust offsets and footages
          do j=1,nst ! determine reference time
            is=ist(j)
            if (irecst(is).eq.1) then
              knewtp = .true.
            else
             knewtp =. false. 
            endif
            if (knewtp) then ! don't change anything
              MJD1(is)=JULDA(1,IDA,IYR-1900)
              ut1(is)=hms2seconds(ihr,imin,isc)
              iyr1(is)=iyr
              ida1(is)=ida
              ihr1(is)=ihr
              imin1(is)=imin
              isc1(is)=isc
              ideltat(j) = 0
!              iftnew(is)=ift(j)
              itu_early(is)=1
            else ! calculate new ref time 
C   data_begin is at the ref time in sked's observations, except
C   for continuous recording. data_beg = prev_stop + ical + tslew
C   _ref time is the earliest of the data_beg times.
              tslew = 0.0
              itu_early(is)=0
              if (irecst(is).gt.1) then ! calculate slewing
                lcb_new=icb(j)
                call slewo(isorp(is),mjdpre(is),utpre(is),isor,is,
     .            lcbpre(is),lcb_new,tslew,0,dum)
                if (tslew.lt.0) tslew=0.0
              endif ! calculate slewing
C             time1 is previous data_stop + slew + cal = new data_valid
              call tmadd(iyr4(is),ida4(is),ihr4(is),imin4(is),isc4(is),
     .                   ifix(tslew)+ical,
     .                   iyr1(is),ida1(is),ihr1(is),imin1(is),isc1(is))
              MJD1(is) = JULDA(1,IDA1(is),IYR-1900)
              ut1(is)=hms2seconds(ihr1(is),imin1(is),isc1(is))
              ideltat(j) = isecdif(mjd,ut,mjd1(is),ut1(is))
C             iftnew is footage at time1
!              iftnew(is) = iftpre(is) + (tslew+ical)*speed(icod,is)
            endif ! calculate new ref time
C         Save values for the next scan calculation
            mjdpre(is) = mjd
            utpre(is) = ut + idur(j)   
            lcbpre(is) = icb(j)
            isorp(is) = isor          
C           data_valid=off time is scan start + station's duration
C           (should add any idle time here to account for non-zero postob)
            call tmadd(iyr,ida,ihr,imin,isc,idur(j),
     .               iyr4(is),ida4(is),ihr4(is),imin4(is),isc4(is))
          enddo ! stations in this scan
C         Calculate offsets
          do j=1,nst
            if (j.eq.1.or.ideltat(j).gt.ideltatmax) then
              ideltatmax = ideltat(j)
              ideltatmaxj = ist(j)
            endif
          enddo
          do j=1,nst
            is = ist(j)
            ioffset(is) = isecdif(mjd1(is),ut1(is),
     .           mjd1(ideltatmaxj),ut1(ideltatmaxj))
            idstop(is) = ideltatmax + idur(j)
          enddo
          iyr_ref = iyr1(ideltatmaxj)
          ida_ref = ida1(ideltatmaxj)
          ihr_ref = ihr1(ideltatmaxj)
          imin_ref = imin1(ideltatmaxj)
          isc_ref = isc1(ideltatmaxj)
        endif ! scan is ref/determine ref time

C*****************************************************************
C Write the VEX scan block.
        call null_term(cscan_id)
        call fcreate_scan(ptr_ch(cscan_id))
C Start time field -- this is the ref time
        write(cid,'(i4,"y",i3.3,"d",i2.2,"h",i2.2,"m",i2.2,"s")')
     .  iyr_ref,ida_ref,ihr_ref,imin_ref,isc_ref
        call null_term(cid)
        call fcreate_start(ptr_ch(cid))
C Mode
        call fcreate_mode(ptr_ch(modedef_name(icod)))
C Source
!        cso(1:max_sorlen)=csrcnam(1:max_sorlen)
        call null_term(cso)
        if(.false.) then
        nch=len_trim(cso)
        do i=1,nch
           write(*,*) i, " ",cso(i:i)," ",ichar(cso(i:i))
        end do
        stop  
        endif 

        call fcreate_source(ptr_ch(cso))
C Station lines
        do j=1,nstatn ! order by full list of stations
          k = 1
          istn = 0
          do while (k.le.nst.and.istn.eq.0) ! is this station in this scan?
C           istn = station index in the full list
            if (ist(k).eq.j) istn = j
C           k = station index in this scan
            k = k + 1
          enddo
        if (istn.gt.0) then ! this station in this scan
          k = k-1
C   station code
          cstat=cpocod(istn)//char(0)
C   cal time
          ical = -ical
          write(ccal,'(i4)') ical
          call null_term(ccal)
          write(cdatab,'(i4)') ioffset(istn)
          call null_term(cdatab)
C   footage at the start of recording.
        write(cfeet,'(i5)') 0
        call null_term(cfeet)
C   rec_begin is the time recording starts. This is data_begin minus early
C   start for start-stop. THIS WILL BE IN VEX 1.6.
C       write(crecb,'("-",i3.3)') itearl(istn)
C       call null_term(crecb)
C   data_end is the end of valid data. This is the duration for
C   start-stop.
        write(cdatae,'(i5)') idstop(istn)
        call null_term(cdatae)
C   rec_end is the time recording stops. This is duration plus late stop
C   for start-stop.
        write(crece,'(i4)') idur(k)+itlate(istn)
        call null_term(crece)
C   postob time is the end of postob
        write(cpo,'(i5)') idur(k)+itlate(istn)+idle
        call null_term(cpo)
! Don't need to write out pass order stuff. 
        if(.false.) then 
C   pass 
        ndx = ihddir(1,ipas(k),istn,icod) ! subpass number
C       Create the head position index by doing all of them up to the one we need.
        nh=0
        do ip=1,ipas(k)
          if (ihddir(1,ip,istn,icod).eq.1) then ! first subpass
            nh=nh+1
            ihd(nh)=ihdpos(1,ip,istn,icod) ! save the head position
            ih=nh
          else ! find the index
            ih=1
            do while (ih.le.nh.and.ihdpos(1,ip,istn,icod).ne.ihd(ih))
              ih=ih+1
            enddo
          endif
        enddo
C       do while (ip.le.ipas(k).and.
C    .    ihdpos(1,ipas(k),istn,icod).ne.ihd(ih))
C         if (ip.gt.1.and.ihddir(1,ip,istn,icod).eq.1) then
C           ih=ih+1 
C           ihd(ih)=ihdpos(1,ipas(k),istn,icod) ! save the position
C         endif
C         ip=ip+1
C       enddo
        if(ih .le. 9) then
          write(cpa,'(i1,a1)') ih,c_subpass(ndx:ndx)
        else
          write(cpa,'(i2,a1)') ih,c_subpass(ndx:ndx)
        endif
        endif 
        cpa="1A"//char(0) 
        call null_term(cpa)
C   pointing sector
        if (lcb(k) .eq.'-') then
           cpt = 'n'//char(0)
        else if (lcb(k) .eq.'C') then
           cpt = 'cw'//char(0)
        else if (lcb(k) .eq.'W') then
           cpt = 'ccw'//char(0)
        endif
C   drv is the drive number
        cdrv = '1'//char(0)

C  Now create the station lines.
C  This is rev 1.5 format.
          call fcreate_station(ptr_ch(cstat),
     .    ptr_ch(cdatab),ptr_ch('sec'//char(0)),
     .    ptr_ch(cdatae),ptr_ch('sec'//char(0)),
     .    ptr_ch(cfeet),ptr_ch('ft'//char(0)),
     .    ptr_ch(cpa),
     .    ptr_ch(cpt))
          call fcreate_station_drive_list(ptr_ch(cdrv))
          call fcreate_station_drive_list(ptr_ch(char(0)))

C  Save the end times for calculating the next scan's data start.
        endif ! this station in this scan
        enddo ! order by full station list
        call fend_scan 

999     continue
      END DO  !read and write an observation

      RETURN
      END
