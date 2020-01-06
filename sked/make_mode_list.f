      subroutine make_mode_list(cdo,ierr)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_mode.ftni'
! read the mode catalog and find matches.
! History
! 2005Aug10 JMGipson. Extracted from frcat
! 2006Jun22 JMGipson.  Changed name of kfreq_match-->kfreq_cat
! 2008Nov11  JMGipson.  Replaced call to RSPYN by call to kyes_to_prompt
! 2011Aug11 JMGipson. Modified error  message

! Passed
      character*1 cdo !s=standard, a=automatic. find current mode, and exit.
! returned
      integer ierr

! functions
      logical kfreq_cat
      logical kyes_to_prompt

! local
      character*60 cmode_name    
      integer icode
      integer icat
      logical kmatch
      logical kmissing
      integer ifreq
      integer irec
      integer istn    !station
      integer npass,ntrks,nhead,nbit  !#pases, #tracks, #heads, #nbits. 
      logical kduplicate_mode

! Beginning of code.

      call read_mode_cat(ierr)
C     ierr=1 means too many entries, use first set only, but continue
      if(ierr .lt. 0 .or. ierr .eq. 1) then   !Some serious error.
        return
      endif
! Read in all of the auxiliary catalogs.
      call read_antenna_cat(ierr)
      if(ierr .ne. 0) return

      call read_freq_cat(ierr)
      if(ierr .ne. 0) return
      call read_rx_cat(ierr)
      if(ierr .ne. 0) return
      call read_loif_cat(ierr)
      if(ierr .ne. 0) return

      call read_rec_cat(ierr)
      if(ierr .ne. 0) return
      call read_hdpos_cat(ierr)
      if(ierr .ne. 0) return
      call read_track_cat(ierr)
      if(ierr .ne. 0) return

! 1.2 Check to see if already selected modes are in the catalog.
      kcat_mode_sel=.false.

! get #passes, #ntrks, #nheads, #nbits for first code, first station
      istn=1
      icode=1
      call itras_params(istn,icode,npass,ntrks,nhead,nbit)

      kduplicate_mode=.false.
      do icode=1,ncodes
        kmissing =.true.
! check to see if current mode in catalog.
        do icat=1,num_cat_mode
          ifreq=icat_mode_freq_ptr(icat)
          irec =icat_mode_rec_ptr(icat)
! Check frequcency code, bandwidth, and #bits. 
          if(cnafrq(icode) .eq. cat_mode_freq(ifreq) .and.  
     >      kfreq_cat(cat_mode_rec(irec),rcat_mode_bw(icat),icode) .and.
     >      icat_mode_tcfb(4,irec) .eq. nbit) then            !check # of bits. 
            call display_mode_item(cmode_name,icat)
            write(luscn, '("MAKE_MODE_LIST: Found mode ",a)') cmode_name
            if(kmissing) then
              kcat_mode_sel(icat)=.true.
            else
              kduplicate_mode=.true. 
              endif
            kmissing=.false.          
          endif
        end do
        if(kduplicate_mode) then
          write(luscn,*)
     >      "Make_Mode_List: Duplicate mode(s) found. Using first one."
        endif 

        if(kmissing) then
          ierr = 10
          write(luscn,'(a,a,a)')
     >    "ERR: MAKE_MODE_LIST: Observing mode ",  cnafrq(icode)(1:8),
     >       " is not in catalog."
          if(cdo .eq. "a" .or. cdo .eq. "A") return
          write(luscn,'(A)')
     >      "If you continue mode information will be lost."
          if(.not.kyes_to_prompt("Continue? (Y/N)")) return
             
        endif
      enddo
      ierr=0

      return
      end
