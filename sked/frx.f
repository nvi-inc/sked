      SUBROUTINE frx(crxname,nrx,istn_rx_xref,cloifname,ierr)
C
C  FRX opens the receiver setup catalog to 1) find the stations
C  that use the named setup, and 2) store the LOIFNAME for later 
C  searching the loif catalog. 
C
C   HISTORY:
C 951127 nrv New.
! 2005Jul10 JMGipson. Modified getting rid of hollerith.
! 2005Aug10 JMGipson. Pretty print stuff.
! 2005Nov29 JMGipson. Increased size of cloifname to 10 chars
! 2005Jan19 JMGipson. Fixed error message if a staiton appears in two codes.
! 2006Jul26 JMGipson. MAke sure that when writing out stations, don't go past end of line.
! 2006Sep28 JMGipson. Printy print stuff.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C  Input:
      character*8 crxname(max_stn)
      integer nrx ! number of RX names filled in

C  OUTPUT:
      integer ierr ! if error reading catalog file
      integer istn_rx_xref(max_stn) ! index per station of which rxname applies
      character*10 cloifname(max_stn)

!    functions
      integer iwhere_in_string_list

C
C   SUBROUTINES
C     CALLED BY: WRFRS
C     CALLED: NAGET
C
C  LOCAL VARIABLES


! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*10 ltoken(MaxToken)
! other local variables.
      integer irx
      integer istat
      logical krx_found(nrx)
      logical keof
      integer iw, icol_wid,iwid_pre


C  1. Open the catalog and get the frequency for which time
C     through the loop (iloop).

      icol_wid=len(cantna(1))+1
      iwid_pre=len(crxname(1))+2
       
      write(*,*) "Read rx.cat: "//trim(rx_cat) 
      call open_cat(rx_cat,ierr)
      if (ierr.ne.0) then
        close(lutmp)
        return
      endif

C  2. For each RX name that came from the freq.cat file, find the
C     LOIF name for each of the stations.
      istn_rx_xref=0
      krx_found=.false.

100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 200
110   continue
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      irx=iwhere_in_string_list(crxname,nrx,ltoken(1))
      if(irx .eq. 0) goto 100
      krx_found(irx)=.true.
      iw=0  !flag indicating we haven't put up the frequency code yet.
      write(*,'("Searching through ", a," found: ",$)') trim(ltoken(1))

! found a match. Now go through all the stations.
      do while(.true.)
        call skip_to_next_non_comment(keof)
        if(keof) goto 100 
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1)(1:1) .ne. "-") then
          if(iw .ne. 0 .and. kverbose) write(luscn,'()')
          write(*,*) " " 
          goto 110  !done with this group. Find the next one.
        endif

        istat=iwhere_in_string_list(cantna,nstatn,ltoken(2))
        if(istat .ne. 0) then
          write(*,'(" ",a,$)') trim(ltoken(2)) 
          if(istn_rx_xref(istat) .eq. 0) then
            istn_rx_xref(istat)=irx
            cloifname(istat)=ltoken(3)
            if(iw .eq. 0) then
              if(kverbose) write(luscn,'(A,": ",$)') crxname(irx)
              iw=iwid_pre+icol_wid
            endif

            if(kverbose) write(luscn,'(a, " ",$)') cantna(istat)
            iw=iw+icol_wid
            if(iw .gt. iwscn) then
               if(kverbose) write(luscn,'()')   !new line
               iw=0
            endif
          else
            write(luscn,
     >   "(/,'FRX: Warning! ',a,' found in both ',a,' and ',a,
     >       ' Using first.')")
     >       cantna(istat), crxname(istn_rx_xref(istat)),crxname(irx)
          endif
        endif
      end do
200   continue

      do irx=1,nrx
       if(.not.krx_found(irx)) then
          write(luscn,'("frx: ERROR! Code ",a "not found. ")')
     >     crxname(irx)
       endif
      end do

      do istat=1,nstatn
       if (istn_rx_xref(istat).eq.0) then
        write(luscn,'("frx: ERROR! Station ",a," not found.")')
     >    cantna(istat)
       endif
      enddo

500   continue
      close(lucat)

      return
      end
