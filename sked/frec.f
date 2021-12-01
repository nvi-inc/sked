      SUBROUTINE frec(crec_mode,bw_default,itrk_xref,
     >    crecfmtname,cbarrelname,bw_stn,ierr) ! read rec.cat

!
! 2005Nov18 JMGipson  Completely rewritten.
! 2005Nov30 JMGipson. Forgot to read in the bandwidth. Do so.
! 2006May11 JMGipson. Output an error if we don't find the recieving mode.
! 2006Jul26 JMGipson. Output track assignment following station name.
!                     MAke sure that when writing out stations, don't go past end of line.
! 2018Jul10 JMGipson. Write out valid track layouts if did not find track 
! 2020Oct05 JMGipson. Don't return cnahdpos


      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_mode.ftni'
      include 'cat_rec.ftni'
      include 'cat_track.ftni'

C  Input:
      character*16 crec_mode ! Recording mode.
      real*8 bw_default      !default bandwidth

C  OUTPUT:
      integer ierr ! if error reading catalog file
      character*8 chdposname(max_stn)
      character*12 ctrk_name(max_stn)
      integer itrk_xref(max_stn)               !itrk_xref(istn)=pointer to track name.
      real*8 bw_stn(max_stn)                   !bandwidth per station. Set to default unless
                                               ! modified in rec.cat
      character*4 cbarrelname(max_stn)
      character*6 crecfmtname(max_stn)

!    functions
      integer iwhere_in_string_list
      integer trimlen
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
      character*16 ltoken(MaxToken)
! Otehr variables
      integer istn
      integer itrk
      integer iw,icol_wid
      integer nch
      logical kfound(Max_stn)   
      integer num_track_found(max_stn)  !number of times we found a track assignment. Should be 1.   
      logical kmode_found
      logical keof
      real*8 bw_temp         !holds the bandwidth read in.  Becomes bw_stat if <bw_default.
      logical kfirst_err        

C  1. Open the catalog and get the mode.
      call open_cat(rec_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

      icol_wid=len(cantna(1))+len(cat_Rec_Trk(1))+4

! Initialize
      chdposname=" "
      ctrk_name=" "
      cbarrelname=" "
      crecfmtname=" "
      kfound=.false.
      num_track_found=0
      iw=icol_wid
      itrk_xref=0
      bw_stn=bw_default
      kmode_found=.false.

! Space to mode name.
100   continue
      call skip_to_next_non_comment(keof)
      if(keof) goto 190
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      if(ltoken(1) .ne. crec_mode) goto 100
      kmode_found=.true.

! Found the mode. Read the continuation lines.
      do while(.true.)
        call skip_to_next_non_comment(keof)
        if(keof) goto 190

        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 190                           !done

        istn=iwhere_in_string_list(cantna,nstatn,ltoken(2))

        if(istn .ne.0) then    
          kfound(istn)=.true.
          num_track_found(istn)=num_track_found(istn)+1
!          chdposname(istn)=ltoken(3)
        
        
          itrk=iwhere_in_string_list(cat_rec_trk,num_cat_rec_trk,
     >         ltoken(4))  !find the track assginm.
          if(itrk .eq. 0) then !not found in catalog.
                 nch=trimlen(ltoken(4))
             write(luscn,*) "FREC: ERROR! Track: ", ltoken(4)(1:nch),
     >                 " not found in catalog!"
             write(luscn,*) "Valid track layouts are: "
             write(luscn,'("    ",a)')
     >            (cat_rec_trk(i),i=1,num_cat_rec_trk) 
             stop
          endif
          itrk_xref(istn)=itrk

          if(numToken .ge. 5) then
             crecfmtname(istn)=ltoken(5)
          else
            crecfmtname(istn)="Mk34"
            write(luscn,'(/A)') "FREC02 - WARNING: Recording format"//
     >             "was not  found. Default *Mk34* was used."
          endif
          if(NumToken .eq. 6 .and. ltoken(6) .ne. "none")
     >         cbarrelname(istn)=ltoken(6)

          if(NumToken .eq. 7) then
            read(ltoken(7),*,err=180) bw_temp
            if(bw_temp .lt. bw_default) then
              bw_stn(istn)=bw_temp
            endif
          endif

! Write out the antenna name and track
          if(iverbose_level.ge.5) write(luscn,'(a," (",a,") ",$)') 
     >       cantna(istn), cat_Rec_trk(itrk)
          if(num_track_found(istn) .gt. 1) then 
             write(*,*) "ERROR:  For mode ",trim(crec_mode), 
     >      " duplicate track assignment ",
     >      trim(cat_rec_trk(itrk)), " for ",cantna(istn)
             write(*,*) "Please fix rec.cat!"
             write(*,*) "Sked aborting!"
             stop
          endif 
          iw=iw+icol_wid
          if(iw .gt. iwscn) then
             if(iverbose_level.ge.5) write(luscn,'()')
             iw=icol_wid
          endif

        endif
      end do

! Come here on error reading bandwidth
180   continue
      ierr=-2
      write(luscn,'("FREC: ERROR reading bandwidth (arg7):",/,a)')
     >  cbuf(1:60)
      goto 500

190   continue
      if(iw .ne. 0 .and. iverbose_level.ge.5) write(luscn,'()')
      ierr=0

      kfirst_err=.true.
      do istn=1,nstatn
        if(.not.kfound(istn)) then
          if(kfirst_err) then
            write(luscn,"('FREC: ERROR! In catalog ',a)")
     >           rec_cat(1:trimlen(rec_cat))
            write(luscn,
     >        "('      Recording mode = ',a,' missing stations: ',$)")
     >      crec_mode
            kfirst_err=.false.
          endif
          write(luscn,"(a,' ',$)") cantna(istn)
          ierr=-1
        endif
      end do
      if(.not.kfirst_err) write(luscn,'()')   !close the line.

500   continue
      if(.not.kmode_found) then
         write(luscn,"('FREC: Did not find mode ', a)") crec_mode
         ierr=1
      endif

      close(lucat)

      return
      end
