      subroutine read_track_cat(ierr)
! Read in the tracks catalog, and save the different assignments.
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_mode.ftni'
      include 'cat_rec.ftni'
      include 'cat_track.ftni'

! History.
! 2019Sep03 JMG.  Added implicit none 
! 2012Sep24 JMG.  Initialize cat_track_map to " " 
! 2005Nov18 JMGipson.  First version.

! Passed
! Return
      integer ierr

! functions
      integer iwhere_in_string_list
      integer trimlen
!      integer itras_magic

! Local variables
! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*20 ltoken(MaxToken)
! Other local variables.
      integer nch
      character*40 ctrk_ass
      integer ind,ind1,ind2
      integer icnt
      integer iwid
      integer ichan
      integer itrk
      integer ihd_pass   !ihd_pass=ih*100+ipass
      integer ihd
      integer ipass
      integer isb
      integer ibit
      integer itx
      logical kdone_pass
      logical keof
      logical kcat_rec_trk(max_cat_rec_trk)

!--------------Start of code-----------------------------------------------
!  1. Check that the catalog exists.
      ierr=0
      if(kcat_track) return   !Don't need to read it in.
      call open_Cat(tracks_cat,ierr)
      if(ierr .ne. 0) then
         return
      endif

! Initialize.
      icat_trk_fan=1   !initialize
      icat_trk_bit=1   !initialize
      icat_trk_map=-99
       kcat_rec_trk=.false.
       cat_trk_map=" " 

!2.  Read the catalog
      iwid=0

!3. Process a track assignment.
! Read a line like:
!  8U-4-1   4  1
! If a Mark3 mode, just a single letter:  A-E


100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 200

110   continue
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)

      nch=trimlen(cbuf)
      itrk=iwhere_in_string_list(cat_rec_trk,num_cat_rec_trk,ltoken(1))
      if(itrk .eq. 0) goto 100             !If the track was not referenced in rec.cat, then skip reading it.

      kcat_rec_trk(itrk)=.true.
      if(NumToken .eq. 3) then             !fanout and bits given on track_label line. Read them in.
         read(ltoken(2),*,iostat=ierr) icat_trk_fan(itrk)
         if(ierr .ne. 0) then
           write(*,*)
     >       "Read_track_cat: Error in reading fanout (2nd arg): "//
     >        cbuf(1:nch)
         elseif(icat_trk_fan(itrk).lt.1 .or.
     >           icat_trk_fan(itrk).gt.4) then
           write(*,*) "Read_track_cat: Error in fanout  (2nd arg): "//
     >        cbuf(1:nch)
           write(*,*) "Must be between 1 and 4"
         endif

         read(ltoken(3),*,iostat=ierr) icat_trk_bit(itrk)
         if(ierr .ne. 0) then
           write(*,*)
     >       "Read_track_cat: Error in reading bit (2nd arg): "//
     >        cbuf(1:nch)
         elseif(icat_trk_bit(itrk).lt. 1 .or.
     >          icat_trk_bit(itrk).gt. 2) then
           write(*,*) "Read_track_cat: Error in bit  (2nd arg): "//
     >        cbuf(1:nch)
           write(*,*) "Must be between 1 and 4"
         endif
      endif

      cat_trk_map(itrk,:)=" "
!3.3 Process the track assignments.
      do while(.true.)
        call skip_to_next_non_comment(keof)
        if(keof) goto 200
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 110   !not a comment line. Must be next track assignment.

        read(ltoken(2),*,err=300) ichan

        nch=trimlen(ltoken(3))
        ind=index(cbuf,ltoken(3)(1:nch))
        nch=trimlen(cbuf)
        ctrk_ass=cbuf(ind:nch)
        cat_trk_map(itrk,ichan)=ctrk_ass
        nch=trimlen(ctrk_ass)
        ind1=1
! Parse this.
! This string contains something that looks like:
!       1(1,15) 2(2,16)        or
!       101(15,19,17,21)
!  or   1(15,,19)
!
!  where pass/head(us,ls,um,lm)  pass/head(us,ls,um,lm)       etc.
!  pass/head is pass+100*headstack, and us,ls,um,lm are tracks where UpperSign etc are written.
!      
        do while(ind1 .lt. nch)
          ind2=index(ctrk_ass,"(")
          if(ind .eq. 0) then
            write(*,*) "Read_Track_Cat: Error in reading track catalog."
            write(*,*) "Did not find '(' "
            write(*,*) "At line---> ",trim(cbuf)         
            stop
          endif
          read(ctrk_ass(ind1:ind2-1),*,err=300) ihd_pass
          ctrk_ass(ind1:ind2)=" "  !Erase as we read
          ihd=(ihd_pass/100+1)
          if(ihd .gt. Max_headstack) then
             write(*,*) "Read_Track_Cat: Headstack too big!"
             stop
          endif

          ipass=mod(ihd_pass,100)
          ind1=ind2+1
          icnt=0
          kdone_pass=.false.
! A pass consists of reading upto 4 numbers between two parenthesis (1,2,,4)
! Skip commas.
          do while(ind1.lt. nch .and. icnt .lt.4 .and. .not.kdone_pass)
            ind2=index(ctrk_ass,",")
            if(ind2 .eq. 0) then
              ind2=index(ctrk_ass,")")
              if(ind2 .eq. 0) then
                 write(*,*)
     >            "Read_Track_Cat: Error in reading track catalog."
                 write(*,*) "Did not find ')' "
                 write(*,*) "At line---> ",trim(cbuf) 
                stop
              endif
              kdone_pass=.true.
            endif
            icnt=icnt+1
            if(ind1.ne.ind2) then
              read(ctrk_Ass(ind1:ind2-1),*,err=300) itx
              itx=itx+3        !convert to Mark4
              ibit=(icnt-1)/2
              isb=icnt-2*ibit
              ibit=ibit+1
!              icat_trk_map(itrk,ihd,itx)=itras_magic(isb,ibit,ichan,ipass)
            endif
            ctrk_ass(ind1:ind2)=" "
            ind1=ind2+1
          end do
        end do
      end do

190   continue
      ierr=-1
      writE(luscn,'("read_track_cat: Error parsing: ",a)')
     >   cbuf(1:trimlen(cbuf))

200   continue
      if(kverbose)write(luscn,'()')

300   continue
      kcat_track=.true.
      close(lucat)

! See if we did not find any.
      do itrk=1,num_cat_rec_trk
        if(.not.kcat_rec_trk(itrk)) then
         write(luscn,'("Read_track_cat: ERROR! Track assignment ",a,
     >     " not found.")')    cat_rec_trk(itrk)
        endif
      end do
      ierr=0

      return
      end
