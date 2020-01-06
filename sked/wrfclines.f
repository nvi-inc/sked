      SUBROUTINE wrfclines(nrx,cfrname,cfrcode,istn_rx_xref,
     >  nfr,cmode,cfmt,bw_stn,ichan,ibbc,csw,cb,csky,cpcfr,
     >  itrk_xref,ierr)        !write the "F" and C" lines
C
C  WRFCLINES writes the "F" and "C" lines for the $CODES section,
C  for one frequency code.
!  Each F line describes a series of statins that have the same:
!    Format, and RX and BW.
!  Each F line is followed by a series of C lines.
C
C   HISTORY:
C 951128 nrv New.
C 960221 nrv Change LCH to ICHAN
C 960513 nrv Write out track assignments by channel index
C 960516 nrv Write out BBC numbers at end of line for nrx>1
C 970408 nrv Add LFMT to call. Write out mode per station.
C 990505 nrv Recognize 'K4' as separate mode and copy mode in place.
C 990616 nrv Recognize 'S2' as separate mode and copy mode in place.
C 020112 nrv Print only 1 character for the Mk3 modes
! 2005May15. Removed channel index from ichan,ibbc,lsw.
! 2005Aug02. Corrected bug with many submodes. Was indexing on ichan(ifreq,is)
!            and should have been ichan(ifreq,irx)
! 2005Oct05. Got rid of all remmaining hollerith.
! 2005Nov16. Modified to use fact that ctrk_ass is an indexed array.
! 2005Nov28. JMGipson. If track is not assigned (ichan=-99) then skip it.
! 2006Jul26. JMGIpson. Issue an error message if track assignment is missing.
!            This means the catalog is setup wrong.
! 2011Mar21  JMG. Teston different Bits,Fanout as wel. (See cfmtname construction and test) 
! 2012Sep21  Test on track map as well. 
! 2012Sep24. Issue error message if missing track assignment. 

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_mode.ftni'
      include 'cat_rec.ftni'
      include 'cat_track.ftni'
C
C Input:
      integer nrx ! number of sub-codes in this freq. sequence

      character*2 cfrcode
      character*8 cfrname
      character*8 cmode
      real*8  bw_stn(max_stn)             ! bandwidth. May vary by station
      integer istn_rx_xref(max_stn)       ! index to which RXname
      integer ichan(max_chan,max_stn)     ! channel number
      integer   nfr(max_stn)              ! number of freq lines
      integer itrk_xref(max_stn)
      character*2 cb(max_chan,max_stn)
      character*4 csw(max_chan,max_stn)
      character*8 csky(max_chan,max_Stn)
      character*8 cpcfr(max_chan,max_stn)
      integer   ibbc(max_chan,max_stn)    ! BBC number from freq lines
      character*6 cfmt(max_stn)           ! recording format or MK3 mode.    

! functions
      integer iwhere_in_string_list
      integer trimlen

C  OUTPUT:
      integer ierr ! if error writing scratch file
C
C   SUBROUTINES
C     CALLED BY: WRFRS
C
C  LOCAL VARIABLES
      integer ich,irx,ifreq,nfmt,ifmt
      character*20 cfmtname(max_stn)     !list of formats-BW. First 6chars are formats. Next 4 are BW., next 4 fan, bits
      character*20 ctemp 
      integer ifmt_xref(max_stn)         !pointer into list.
      character*8 cmode_print  
      integer is,js                      !counter
      integer istn
      integer itrk
      real bw_fline
      logical kdone(max_stn)             !indicate that we have done this station.
      logical kerror

      ierr=0
      if(kverbose) write(luscn,'(a)') "Writing out F&C lines."

!  1. Make up the list and station index of (recording formats+Bandwidth)+track format!
!  Note that if we are using a Mark3 mode (A-E) we ignore the formatter info.
      nfmt=0
      kerror =.false. 
      do is=1,nstatn
        itrk=itrk_xref(is)  
        if(trimlen(cat_rec_trk(itrk)) .eq. 1 .and.
     >     cat_rec_trk(itrk).ge."A".and.cat_rec_trk(itrk).le."E") then
          write(ctemp,'(a,f4.2)') cat_rec_trk(itrk)(1:6),bw_stn(is)
        else if(cat_rec_trk(itrk)(1:2) .eq. "S2") then
!AEM undo          ctemp=cat_rec_trk(itrk)
!AEM20060711 for sked to support S2-RT, S2 rec_mod is cfmt and cat_rec_trk is just a flag/pointer
          ctemp=cfmt(is)
        else
          write(ctemp,'(a,f4.2,3i2)') cfmt(is),bw_stn(is),
     >        icat_trk_fan(itrk),icat_trk_bit(itrk), itrk     !default case
        endif
        ifmt=iwhere_in_string_list(cfmtname,nfmt,ctemp)
        if(ifmt .eq. 0) then
           nfmt=nfmt+1
           cfmtname(nfmt)=ctemp
           ifmt=nfmt 
        endif
        ifmt_xref(is)=ifmt
        if(cat_trk_map(itrk,1) .eq. " ") then
           write(*,
     >      '("WRFCLINES: ERROR!  For station ",a, 
     >        " Track mode is not in catalog: ", a)')
     >       cstnna(is), cat_rec_trk(itrk)
           kerror=.true.
        endif 
      end do
      if(kerror) then
         write(*,'(19x,a)') "PLEASE check tracks catalog and fix error!"
         kerror=.false.
      endif 

C  2. Major loop is over the number of sub-codes. Write out a set of one
C     "F" line and multiple "C" lines for each sub-code. F format: 
C         F name code station-list

      do irx=1,nrx ! try each sub-code
C       Don't write anything if this sub-code isn't needed by any of
C       the selected stations.

        do istn=1,nstatn
          if (istn_rx_xref(istn).eq.irx) goto 100 !found a match.
        enddo
        goto 200         !Skip rest of code because not found.

100     continue
        do ifmt=1,nfmt ! each recording format
          cbuf="F "//cfrname//" "//cfrcode
          ich=15
          do is=1,nstatn
            if (istn_rx_xref(is).eq.irx.and.ifmt_xref(is).eq.ifmt) then !
C              this station on this RXname and in this format
              itrk=itrk_xref(is)
              cbuf(ich:ich+7)=cstnna(is)
              ich=ich+8+1
              bw_fline=bw_stn(is)
            endif

          enddo
          if(ich .eq.  15) goto 180           !no stations found.

          write(lutmp,'(a)') cbuf(1:ich)
          if(kverbose) write(luscn,'(a)') cbuf(1:ich)
C  3. Set up the mode name.
          cmode_print=" "

          if(cat_rec_trk(itrk).ge."A".and.cat_rec_trk(itrk).le."E".and.
     >      trimlen(cat_rec_trk(itrk)) .eq. 1 ) then
            cmode_print(1:1)=cat_rec_trk(itrk)
          else if (cfmtname(ifmt)(1:1) .eq. "K") then !K4
            cmode_print=cmode
          else if(cat_rec_trk(itrk)(1:2) .eq. "S2") then !S2
            cmode_print=cfmtname(ifmt)
          else ! Mk4/VLBA
            cmode_print=cfmtname(ifmt)(1:4)
            if(icat_trk_fan(itrk) .lt. 10) then
              write(cmode_print(5:7),'("1:",i1)') icat_trk_fan(itrk)
            else
              write(cmode_print(5:8),'("1:",i2)') icat_trk_fan(itrk)
            endif
          endif ! Mk3/Mk4/VLBA

C  4. For each "F" line, now write the "C" lines that apply.
          do ifreq=1,nfr(irx) ! each frequency in this sub-code
            if(ichan(ifreq,irx) .ne. -99) then
              write(cbuf,'("C ",4(a,1x),i3,1x,a,1x,f6.2,1x,a)')
     >          cfrcode,cb(ifreq,irx), csky(ifreq,irx),cpcfr(ifreq,irx),
     >          ichan(ifreq,irx),cmode_print,bw_fline,
     >          cat_trk_map(itrk,ichan(ifreq,irx))
                if(cat_trk_map(itrk,ichan(ifreq,irx)) .eq. " ") then
                   write(*,*) 
     >          "ERROR: Missing track information for channel: ",ifreq
                   ierr=1
                endif
              ich=trimlen(cbuf)
              if(irx.gt.1) then
                write(cbuf(ich+1:),'(i3,1x,a)') ibbc(ifreq,irx),
     >            csw(ifreq,is)
                ich=trimlen(cbuf)
              endif
              write(lutmp,'(a)') cbuf(1:ich)
            endif
          end do  !each frequency this sub-code
180       continue
        enddo  !each recording format.
! Come here if not found.
200   continue
      enddo ! try each sub-code     
      if(ierr .ne. 0) then
         write(luscn,'(a)')
     >     "wrfclines: Catalog ERORR: Please check track assignments."
      endif
         
    
      close(lucat)

      RETURN
      END
