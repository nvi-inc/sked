      SUBROUTINE read_freq_cat(ierr)
! Read in the frequency catalog.
! Only read in modes defined in modes.cat (Read in previously).
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'
      include 'cat_rx.ftni'
! Returned variable
      integer ierr      !0=no error.

! History
!  2020Jun02  JMG. Removed debugging write. 
!  2019Sep03  JMG. 1) Added implicit none. 2) Replaced unitialized variable with correct one.
!  2008Jun11  JMGipson.  Only write to screen if verbose is on.
!  2005Nov21  JMGipson  First version

! Functions
      integer trimlen
      integer iwhere_in_string_list
! Local variables
      logical keof
      integer ifrq      !which frequency sequence
      integer itemp

! Holds tokenized line.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=10)
      character*8 ltoken(MaxToken)
! More stuff
      integer nch
      integer iwid
      integer icnt

!  1. Open the frequency catalog
      ierr=0
      if(kcat_freq) return   !skip re-reading the catalog
      call open_cat(freq_cat,ierr)
      if(ierr .ne. 0)then
        close(lutmp)
        return
      endif

! 2.  Read the catalog
      iwid=0

! 3. Should be at a frequency sequence.
      ifrq=0
      num_cat_freq=0
100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 300
110   continue
! Parse a line that looks like:
* Name   Code Sub-group RXname
! CDP-SX SX   STD       SX_STD
! Note that name can be repeated many times with different modifiers.
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
       itemp=iwhere_in_string_list(cat_mode_freq,num_cat_freq,
     >   ltoken(1))
      if(itemp .eq. 0) goto 100   !not in modes.cat. Don't need to read it in.

      if(ifrq .lt. max_cat_freq) then
        num_cat_freq=num_cat_freq+1
        ifrq=ifrq+1
      else
        write(luscn,'(a,/,a)')
     >    "Read_freq_cat: Error! Out of space in sub_group list.",
     >    "Increase max_cat_freq in cat_freq.ftni and recompile."
        goto 300
        ierr=-1
        stop
      endif

! Found a frequency sequence.  Parse rest of the info on this line.
! A. Frequency code.
      cat_seq_name(ifrq)=ltoken(1)
      cat_freq_code(ifrq)=ltoken(2)
      cat_sg_name(ifrq)=ltoken(3)
      cat_rx_name(ifrq)=ltoken(4)

! Read in the rest of the info for this sequence.
! Looks like sequence of lines:
!- X    R   8210.99  U   CH1     1    10000.0
!- X    R   8220.99  U   CH2     2    10000.0  1,2
      icnt=0
120   continue
      do while(.true.)
        ierr=0
        call skip_to_next_non_comment(keof)
        if(keof) goto 300
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 110                          !Not a continution line. Look at next sequence

        if(NumToken .ne. 8 .and. NumToken .ne.9) then
          nch=trimlen(cbuf)
          write(luscn,'(a,/,a,/,a,i4)')
     >      "read_freq_cat: Error! Wrong number of tokens in line: ",
     >      "               Should be 8 or 9.",
     >      "We found:    ",NumToken
          ierr=-1
          goto 300
        endif

! Read in another channel
        icnt=icnt+1
        cat_freq_band(ifrq,icnt)=ltoken(2)
        cat_freq_pol(ifrq,icnt)=ltoken(3)
        ierr=-4
        read(ltoken(4),*,err=200) rcat_freq_sky(ifrq,icnt)
        cat_freq_sb(ifrq,icnt)=ltoken(5)

        ierr=-6
        if(ltoken(6)(3:3) .eq. "X") then
           icat_freq_chan(ifrq,icnt)=-99            !indicate not used.
        else
            read(ltoken(6)(3:6),*,err=200) icat_freq_bbc(ifrq,icnt)
        endif

        ierr=-7
        read(ltoken(7),*,err=200) icat_freq_bbc(ifrq,icnt)
        ierr=-8
        read(ltoken(8),*,err=200) rcat_freq_pcal(ifrq,icnt)

        if(numToken .eq. 9) then
           cat_freq_sw(ifrq,icnt)=ltoken(9)
        else
           cat_freq_sw(ifrq,icnt)=" "
        endif
        nchan_cat_freq(ifrq)=icnt
      end do              !end of reading frequency sequence.
200   continue
      write(luscn,'("read_freq_cat: Error in field ",i4)') -ierr
      write(luscn,*) cbuf(1:trimlen(cbuf))

300   continue
      if(kverbose) then
      write(luscn,'("Read_freq_cat: num_freqs/max_freqs: ",i4,"/",i4)')
     >    num_cat_freq,max_cat_freq
      endif

      kcat_freq=.true.
      close(lucat)
      return
      end

