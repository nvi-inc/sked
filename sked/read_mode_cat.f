      SUBROUTINE read_mode_cat(ierr)
! Read modes catalog. Read old (pre 2005Nov15) or New version.
C
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'
      include 'cat_rec.ftni'

C
C  Called by: FRCAT
C  Calls:       utilities
!  History.
!  2019Sep03  JMG. Added implicit none.
!
!  2005Nov21 JMGipson.  First version finished.
!  2008Jun11 JMGipson.  Only write to screen if verbose is on.
!  2009Apr02 JMGipson. Changed name: icat_mode_freq-->icat_mode_freq_ptr
!                                   icat_mode_rec -->icat_mode_rec_ptr 
!  2009Sep09 JMGipson. Modified to rec.cat key that includes U and L, e.g., 32-8U+8L-2-2
!  2019Jun13 JMG.  Output better error messages if can't parse line

C  OUTPUT: 
      integer ierr
! functions
      integer trimlen
C
C  LOCAL:
      logical keof
      integer nch
! used for storing tokens
      integer MaxToken
      integer NumToken
      parameter(MaxToken=11)
      character*20 ltoken(MaxToken)
      character*80 lerr_msg
! 
      integer ind               !location of character in string 
!     Offset of various columns
      integer ibw_col,isamp_col,irec_col  
      integer ifreq
      integer irec
      integer itemp(5)
 
C  1. Read all of catalog and get mode names.
      ierr=0
      if(kcat_mode) return         !only need to read once.
      call open_Cat(modes_cat,ierr)
      if(ierr .ne. 0) then
         close(lutmp)
         return
      endif

      num_cat_mode = 0
      lerr_msg=" "

100   continue
      ierr=0
      call skip_to_next_cat_group(keof)
      if(keof) goto 500
      call capitalize(cbuf)
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)

! New format has 5 tokens/line.
! Old format has 10 tokens/line.
! The columns we want are in different places. Adjust accordingly.
! If we have the wrong number of tokens, indicate an error and exit.
      if(NumToken .eq. 5) then
        ibw_col=3
        isamp_col=4
        irec_col=5
      else if(NumToken .eq. 10) then
        ibw_col=4
        isamp_col=5
        irec_col=10
      else
        write(luscn,'("Read_mode_cat: Wrong # of tokens in line: ",a)')
     >          cbuf(1:trimlen(cbuf))
        write(luscn,'("Found: ", i4)') NumToken
        write(luscn,'(a)')
     >           "  Correct number is 5(New Cat) or 10 (Old Cat)."
        ierr=1
        goto 500
      endif

      if(num_cat_mode .eq. max_cat_mode) then
        write(luscn,'("Read_mode_cat: Too many entries in catalog")')
        write(luscn,'("            Keeping first: ",i4)') num_cat_mode
        ierr=2
        goto 500
      endif

      num_cat_mode=num_cat_mode+1
C 1. Mode name, max 16 characters
      nch=trimlen(ltoken(1))
      if(nch .gt. 16) then
        write(lerr_msg,'(a,i6)')
     >     "Maximun length of mode name is 16 characters. You had ",nch
        ierr=-1
        goto 500
      else
        cat_mode(num_cat_mode)=ltoken(1)(1:nch)
      endif

C 2. Frequency sequence name, max 8 characters.

      nch=trimlen(ltoken(2))
      if(nch .gt. 8) then
        write(lerr_msg,'(a,i6)')
     >     "Maximun length of freqency name is 8 characters. You had "
     >     ,nch
        ierr=-2
        goto 500
      endif

      call update_string_list(cat_mode_freq,num_cat_mode_freq,
     >       max_cat_mode_freq, ltoken(2),ifreq)
      if(ifreq .lt. 0) then
        ierr=1
        write(luscn,'(a,/,a)')
     >      "Read_mode_cat: Out of space in freq list.",
     >      "Increase max_cat_trk in cat_mode.ftni and recompile."
          goto 500
      endif
      icat_mode_freq_ptr(num_cat_mode)=ifreq


C 3. Bandwidth.
      ierr=-ibw_col
      lerr_msg="Error reading BW "//trim(ltoken(ibw_col))
      read(ltoken(ibw_col),*,err=500) rcat_mode_bw(num_cat_mode)

C 4. Sample rate.
      ierr=-isamp_col
      write(lerr_msg,*) 
     >  "Error reading sample rate "//trim(ltoken(isamp_col))
      read(ltoken(isamp_col),*,err=500) rcat_mode_samp(num_cat_mode)   
        

C 5. Recording mode reference.
      nch=trimlen(ltoken(irec_col))
      if(nch .gt. 16) then
        write(lerr_msg,'(a,i6)')
     >    "Maximun length of recording mode name is 16 chars. You had ",
     >    nch 
        ierr=-irec_col
        goto 500
      endif

      call update_string_list(cat_mode_rec,num_cat_mode_rec,
     >       max_cat_mode_rec, ltoken(irec_col),irec)
      if(irec .lt. 0) then
        ierr=1
        write(luscn,'(a,/,a)')
     >      "Read_mode_cat: Out of space in rec list.",
     >      "Increase max_cat_trk in cat_mode.ftni and recompile."
          goto 500
      endif
      icat_mode_rec_ptr(num_cat_mode)=irec
! here we decipher tracks-chan-fan-bit
      if(irec_col .eq. 5) then
         ind=index(ltoken(irec_col),"-")
         do while(ind .ne. 0)
           ltoken(irec_col)(ind:ind)=" "
           ind=index(ltoken(irec_col),"-")
         end do
         ind=index(ltoken(irec_col),"U") 
         if(ind .ne. 0) ltoken(irec_col)(ind:ind)=" "
         ind=index(ltoken(irec_col),"L") 
         if(ind .ne. 0) ltoken(irec_col)(ind:ind)=" "

         ierr=-irec_col
         ind=index(ltoken(irec_col),"+") 
         lerr_msg="Wrong format for recording mode!" 
         if(ind .ne. 0) then
            ltoken(irec_col)(ind:ind)=" " 
            read(ltoken(irec_col),*,err=500,end=500) itemp(1:5)
            icat_mode_tcfb(1,irec)=itemp(1)
            icat_mode_tcfb(2,irec)=itemp(2)+itemp(3)    !sum of sidebands
            icat_mode_tcfb(3,irec)=itemp(4)
            icat_mode_tcfb(4,irec)=itemp(5)
         else
            read(ltoken(irec_col),*,err=500,end=500)
     >          icat_mode_tcfb(1:4,irec)
         endif
       else
         icat_mode_tcfb(1:4,irec)=0
       endif 
       ierr=0 

      goto 100  !read next mode

500   continue
      if(ierr .lt. 0) then
        write(luscn,'("read_mode_cat: error parsing field ",i4)') -ierr       
        write(luscn,'("In line --> ", a)') trim(cbuf)  
        write(luscn,'(a)') trim(lerr_msg)
! Addditional info
        if(irec_col .eq. 5) then
          write(luscn,'(a)') "Options are: "
          write(luscn,'(a)') "   trk-chan-fan-bit" 
          write(luscn,'(a)') "   trk-U#-L#-fan-bit" 
          write(luscn,'(a)') "   fan, bit can be arbitrary int2"
          write(luscn,'(a)') "   Example 1:  32-16-2-1"
          write(luscn,'(a)') "   Example 2:  32-14U-2L-1-2"
          write(luscn,'(a)') "   Example 2:  32-14U-2L-44-93"
        endif          

      endif

510   continue 

      close(lucat)
      if(kverbose) then
      write(luscn,'("Read_mode_cat: num_modes/max_modes: ",i3,"/",i3)')
     >    num_cat_mode,max_cat_mode
      kcat_mode=.true.

      write(luscn,'("Read_mode_cat: num_freq/max_freq:   ",i3,"/",i3)')
     >    num_cat_mode_freq,max_cat_mode_freq

      write(luscn,'("Read_mode_cat: num_rec/max_rec:     ",i3,"/",i3)')
     >    num_cat_mode_rec,max_cat_mode_rec
      endif      

      RETURN

      END
