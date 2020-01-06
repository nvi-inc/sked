      subroutine tcl_cat_cmd(lstring)
! Routine to dump info for the tcl catalog routine.

! common blocks used.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
!
      include 'cat_mode.ftni'
      include 'cat_src.ftni'
      include 'cat_stat.ftni'

! 2006 May02  JMGipson. Removed "ccat" from argument list of wrsos.
!

! passed variable.
      character*(*) lstring
! functions
      integer trimlen
      integer istringMinmatch
      integer getpid
! refrences to external subroutines
      External display_stat_item
      External display_src_item
      External display_mode_item

! local variables
      character*128 ctcl_cat_tmp  !file to store tcl data.
      character*128 ccat
      character*2 cdo             !how we call mode.
      character*80 ldum

      integer isrc_wid/20/        !widht of output
      integer istat_wid/35/
      integer imode_wid/63/
      logical kmode_section_found,kstat_section_found,ksrc_section_found
      logical kmode_found

      integer iptr
      integer ierr
      integer idone_flag
      save idone_flag

! local.
      INTEGER istart            !place to start parsing
      Integer inext             !end
! returned
      CHARACTER*10 ltoken
      logical ktoken            !returned token?
      logical knospace          !no space left.
      logical keol              !end of line
      integer ifunc

      integer ilist_len
      parameter (ilist_len=2)
      character*5 list(ilist_len)
      data list/"GET","START"/

! Extract the next token.
      istart=1
      call ExtractNextToken(lstring,istart,inext,ltoken,ktoken,
     >  knospace, keol)
      if(.not.ktoken) then
        IFUNC = 1 ! default is get
      ELSE
        ifunc=istringMinMatch(list,ilist_len,ltoken)
      END IF
      if (ifunc.le.0) then
        write(luscn,'(a)') 'CATCMD - Function must be START or GET.'
        RETURN
      END IF

      write(ctcl_cat_tmp,'("/tmp/tcl_cat_tmp",i5.5)') getpid()
!      ctcl_cat_tmp="/tmp/tcl_cat_tmp"//cpid

! now write out the stuff.
      if(list(ifunc) .eq. "START") then   !write out the temporary file.
! read in catalogs into memory
        cdo='a'               !automatic mode for making the lists.
        ccat=source_cat
        call make_source_list(cdo,ierr)
        if(ierr .ne. 0 .and. ierr .ne. 10) return  !10 means source not found.

        call make_mode_list(cdo,ierr)
        if(ierr .ne. 0 .and. ierr .ne. 10) return  !10 means mode not found.

        call make_stat_list(cdo,ierr)
        if(ierr .ne. 0 .and. ierr .ne. 10) return  !10 means station not found.
        call save_station_state

! Write out the stuff.
        open(lucat,file=ctcl_cat_tmp)

! 1.
        call write_tcl_select_list(lucat,"$STATIONS",display_stat_item,
     >       kcat_stat_sel,num_cat_stat,istat_wid)
        call write_tcl_select_list(lucat,"$SOURCE",display_src_item,
     >       kcat_src_sel,num_cat_src,isrc_wid)
        call write_tcl_select_list(lucat,"$CODES",display_mode_item,
     >       kcat_mode_sel,num_cat_mode,imode_wid)
        close(lucat)
        write(luscn,'(a)') ctcl_cat_tmp       !indicate the file we wrote out.
        idone_flag=1234
        return
      else
        if(idone_flag .ne. 1234) then
           write(luscn,'(a)') "You need to run cat start first!"
           return
        endif
! This is cat get.
        open(lucat,file=ctcl_cat_tmp)  !open the file

        kmode_section_found=.false.
        ksrc_section_found=.false.
        kstat_section_found=.false.

200     continue
        read(lucat,'(a80)',end=300) ldum    !prime the system
        if(ldum(1:1) .ne. "$") goto 900

        if(ldum .eq. "$CODES") then
          kmode_found=.false.
          read(lucat,'(a80)') ldum  !skip the first line.
          do iptr=1,num_cat_mode
            read(lucat,'(a80)') ldum
            if(ldum(1:1) .eq. "T") then
               kcat_mode_sel(iptr)=.true.
               kmode_found=.true.
            else if(ldum(1:1) .eq. "F") then
               kcat_mode_sel(iptr)=.false.
            else
               goto 900
            endif
          end do
          kmode_section_found=.true.
          goto 200
        else if(ldum .eq. "$STATIONS") then
          read(lucat,'(a80)') ldum  !skip the first line.
          do iptr=1,num_cat_stat
            read(lucat,'(a80)') ldum
            if(ldum(1:1) .eq. "T") then
               kcat_stat_sel(iptr)=.true.
            else if(ldum(1:1) .eq. "F") then
               kcat_stat_sel(iptr)=.false.
            else
               goto 900
            endif
          end do
          kstat_section_found=.true.
          goto 200
        else if(ldum .eq. "$SOURCE") then
          read(lucat,'(a80)') ldum  !skip the first line.
          do iptr=1,num_cat_src
            read(lucat,'(a80)') ldum
            if(ldum(1:1) .eq. "T") then
               kcat_src_sel(iptr)=.true.
            else if(ldum(1:1) .eq. "F") then
               kcat_src_sel(iptr)=.false.
            else
               goto 900
            endif
          end do
          ksrc_section_found=.true.
          goto 200
        else
          goto 900
        endif

300     continue
        close(lucat)
        if(.not.(kmode_section_found .and. kstat_section_found .and.
     >      ksrc_section_found)) then
           write(luscn,"(a)")"TCL_CAT_MODE: Did not get all information"
           return
        endif
! Write out and read in the station selection files.
! SOURCES
        ccat=source_cat
        CALL wrsos(IERR)
        knewso = .true.
        IF (IERR.NE.0)  THEN
           WRITE(LUSCN,'("Error ",I3," writing select file")') ierr
           CLOSE(lusel)
        END IF !write out select file
        cdo="r"       !reset the sources.
        call sosel(ccat,cdo)
        cdo=" "
        call flget(cdo)    !argument blank means use default flux file.
! Now the stations
        CALL WRSTS(IERR)
        knewst = .true.
        cdo="r"
        call stsel(cdo)

! FREQUENCIES
        if(kmode_found) then  ! Successfully found frequency
          cdo="r"                  !refresh frequency
          call wrfrs(IERR)
          if(ierr .ne. 0) return
          call FRSEL(cdo,ierr)
        else
          write(luscn,'(a)')
     >      "TCL_CAT_CMD: Please re-select frequencies."
        endif
        return
      endif

900   continue
      write(luscn,"(a)")
     >    "TCL_CAT_MODE: unexpected line! "//ldum(1:trimlen(ldum))
      close(lucat)
      return
      end

!********************************************************
      subroutine write_tcl_select_list(lu,ltype,cdisplay,kcat_sel,
     >  num_cat,iwid)
! write out the $SOURCE, $STATIONS or $CODES
! input
      integer lu              !where to write
      character*(*) ltype     !$SOURCE, etc
      external cdisplay
      integer num_cat         !number of entries
      logical kcat_sel(num_cat)            !selected?
      integer iwid            !width.

! local
      character*70 cname
      integer iptr

      write(lu,'(a)') ltype            !header
      call cdisplay(cname,0)
      write(lu,'("H ",a)') cname(1:iwid) !discriptive info

      do iptr=1,num_cat
        call cdisplay(cname,iptr)
        write(lu,'(l1," ",a)') kcat_sel(iptr), cname(1:iwid)
      end do

      return
      end
