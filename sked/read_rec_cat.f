      subroutine read_rec_cat(ierr)
! Read in the tracks catalog, and save the different assignments.
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_mode.ftni'
      include 'cat_stat.ftni'
      include 'cat_rec.ftni'

! History.
!  2019Sep03  JMG. Added implicit none.
!
!    2005Nov21 JMGipson.  First version.
!    2018Jul10 JMGipson. Increased size of token to handle longer arguments. 
!    2018Jul10 JMGipson. Output error message if did not find expected mode. 

! Passed
! Return
      integer ierr

! functions
      integer iwhere_in_string_list

! Local variables
! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=7)
      character*20 ltoken(MaxToken)
! Other local variables.
      logical keof
      integer irec      !irec_type
      integer ifmt
      integer itrk
      integer ihd
      integer istat
      integer iwid
      integer iline
      logical kerror
      integer i 

!--------------Start of code-----------------------------------------------
!  1. Check that the catalog exists.
      ierr=0
      if(kcat_rec) return    !skip if already read in.
      call open_Cat(rec_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
         return
      endif
! Initialize.

!2.  Read the catalog
      iwid=0
      iline=0

100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 200
    
110   continue
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)

      irec=iwhere_in_string_list(cat_mode_rec,num_cat_mode_rec,
     >        ltoken(1))
   
      if(irec .eq. 0) goto 100                 !only read REC settings referenced in modes.cat

! Now start processing lines like:  (Roll and bW are optional).
!         stat       hdpos.cat    tracks.cat format   roll  Bandwidth
! -       ALGOPARK   MK3V-A       8U-4-1     Mk34     16
! -       GILCREEK   MK3A-A       8U-4-1     VLBA     8:1
! -       FORTLEZA   MK3V-A      14U2L-2-1    Mk34    none   4

      irec_cat_num(irec)=0
      irec_cat_off(irec)=0
      do while(.true.)
        call skip_to_next_non_comment(keof)
        if(keof) goto 200   
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 110                          !Not a continution line. Look at next sequence

        istat=iwhere_in_string_list(cat_ant_name,num_cat_ant,ltoken(2))
        if(istat .eq. 0) then
           writE(luscn,'("read_rec_cat: Error! Unknown station ",a)')
     >        ltoken(2)
           goto 130
        endif

        iline=iline+1
        if(irec_cat_off(irec) .eq. 0) irec_cat_off(irec)=iline     !This is offset of first one
        irec_cat_num(irec)=irec_cat_num(irec)+1                    !This is the number for this Rec.

        irec_stat(iline)=istat

        call update_string_list(cat_rec_hdpos,num_cat_rec_hdpos,
     >      max_cat_rec_hdpos, ltoken(3),ihd)
        if(ihd .lt. 0) then
          ierr=1
          write(luscn,'(a,/,a)')
     >      "Read_rec_cat: Error! Out of space in HDPOS list.",
     >      "Increase max_cat_loif in cat_hdpos.ftni and recompile."
          goto 300
        endif
        irec_hdpos(iline)=ihd

        call update_string_list(cat_rec_trk,num_cat_rec_trk,
     >       max_cat_rec_trk, ltoken(4),itrk)
        if(itrk .lt. 0) then
          ierr=1
          write(luscn,'(a,/,a)')
     >      "Read_rec_cat: Error!  Out of space in Track list.",
     >      "Increase max_cat_trk in cat_track.ftni and recompile."
          goto 300
        endif
        irec_trk(iline)=itrk

        call update_string_list(cat_rec_fmt,num_cat_rec_fmt,
     >      max_cat_rec_fmt,ltoken(5),ifmt)
        if(ifmt .lt. 0) then
          ierr=1
          write(luscn,'(a,/,a)')
     >      "Read_rec_cat: Error! Out of space in format list.",
     >      "Increase max_cat_fmt in cat_rec.ftni and recompile."
          goto 300
        endif
        irec_fmt(iline)=ifmt
        if(numtoken .ge. 6) then
          cat_rec_brl(iline)=ltoken(6)
        else
          cat_rec_brl(iline)="none"
        endif

        if(numtoken .ge. 7) then
          read(ltoken(7),*) rcat_rec_bw(iline)
        else
          rcat_rec_bw(iline)=-1.00  !Use default.
        endif

130     continue
      end do
200   continue
      if(kverbose) write(luscn,'()')

300   continue
      if(kverbose) then
      write(luscn,'("Read_rec_cat: num_lines/max_lines: ",i4,"/",i4)')
     >    iline,max_rec_lines
      write(luscn,'("Read_rec_cat: num_fmt/max_fmt:     ",i4,"/",i4)')
     >    num_cat_rec_fmt, max_cat_rec_fmt
      write(luscn,'("Read_rec_cat: num_hdpos/max_hdpos: ",i4,"/",i4)')
     >    num_cat_rec_hdpos, max_cat_rec_hdpos
      write(luscn,'("Read_rec_cat: num_trk/max_trk:     ",i4,"/",i4)')
     >    num_cat_rec_trk, max_cat_rec_trk
      endif

      kerror=.false. 
      do irec=1,num_cat_mode_rec
       if(irec_cat_num(irec) .eq. 0) then
          write(luscn,
     >     '("Read_rec_cat: Warning! ",a," not found in catalog!")')
     >     cat_mode_rec(irec)
          kerror=.true.
        endif
      end do
      if(kerror) then
        write(luscn,*) "Valid modes for rec.cat are: "
        write(luscn,'(" ",a)') (cat_mode_rec(i),i=1,num_cat_mode_rec)
      endif 

     
      kcat_rec=.true.
      close(lucat)
      ierr=0

      return
      end
