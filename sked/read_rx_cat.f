      SUBROUTINE read_rx_cat(ierr)
      implicit none 
! Read in the frequency catalog.
! Only read in modes defined in modes.cat (Read in previously).
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'
      include 'cat_rx.ftni'
      include 'cat_loif.ftni'
! Returned variable
      integer ierr      !0=no error.

! History
! First version:
!  2005Nov21 JMGipson
!  2008Jun11 JMGipson.  Only write to screen if verbose is on.
!  2019Sep03 JMGipson.  Added implcit none

!
! Functions
      integer iwhere_in_string_list
! Local variables
      logical keof
      integer irx       !which RX number
      integer istat
      integer iloif
!      integer ichan    !which channel
      integer iline

! Holds tokenized line.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*8 ltoken(MaxToken)
! More stuff
      integer iwid

!  1. Open the frequency catalog
      ierr=0
      if(kcat_rx) return
      call open_cat(rx_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

! 2.  Read the catalog
      read(lucat,'(a100)',end=200) cbuf
      iwid=0
      irx_cat_num=0
      irx_cat_off=0
      iline=0

! Lines in the catalog look like:
*RXname  Stn.Name    LOIFname
*
! SX_DSN
! -       DSS15       DSN_CRF
! -       DSS45       DSN_CRF
! We skip the RXnames that weren't picked up by read_freq_cat.

! 3. Should be at a receiver station setup.
100   continue
! Parse a line that looks like:
      call skip_to_next_cat_group(keof)
      if(keof) goto 200
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      if(NumToken .ne. 1) then
         write(*,*) "read_rx_cat: Error! Shouldn't get here!"
         write(*,*) "Tell John Gipson."
      endif
      irx=iwhere_in_string_list(cat_rx_name,num_cat_freq,ltoken(1))
      if(irx .eq. 0) goto 100               !not in freq.cat. Don't need to read it in.

! Found an RX.
! Begin parsing lines that look like.  Continue to the end of the continuation lines.
! -       DSS15       DSN_CRF
! -       DSS45       DSN_CRF
120   continue
      do while(.true.)
        call skip_to_next_non_comment(keof)
        if(keof) goto 200
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 100                         !Not a continution line. Look at next sequence

        istat=iwhere_in_string_list(cat_ant_name,num_cat_ant,ltoken(2))
        if(istat .eq. 0) then
          writE(luscn,'("read_rx_cat: Error! Unknown station ",a)')
     >          ltoken(2)
          goto 130
        endif

        iline=iline+1
        if(irx_cat_off(irx) .eq. 0) irx_cat_off(irx)=iline      !This is offset of first one
        irx_cat_num(irx)=irx_cat_num(irx)+1                     !This is the number for this RX.
        irx_stat_xref(iline)=istat

        call update_string_list(cat_loif_name,num_cat_loif,max_cat_loif,
     >     ltoken(3),iloif)
        if(iloif .lt. 0) then
          ierr=1
          write(luscn,'(a,/,a)')
     >      "Read_rx_cat: Erorr! Out of space in LOIF list.",
     >      "Increase max_cat_loif in cat_loif.ftni and recompile."
          goto 300
        endif
        irx_loif_xref(iline)=iloif
130     continue
      end do
200   continue
      kcat_rx=.true.
300   continue

      if(kverbose) then
      write(luscn,'("Read_rx_cat: num_line/max_line: ",i4,"/",i4)')
     >    iline,max_rx_lines
      endif

      close(lucat)
      return
      end

