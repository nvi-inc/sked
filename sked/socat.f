      subroutine socat(ierr,knsose,fname)
C
C     SOCAT opens the source catalog file, presents all
C     names for selection, and writes file for SKED
C
C   History
C   NRV 891110 Created
C   gag 900129 added knsose
C   gag 900131 added OR option
C   nrv 931108 Add call to rsini when new sources are selected
C   nrv 931124 Remove call ""
C   nrv 940127 Add fname to socat call and to getso call 
C   nrv 950329 Add call to grget.
C 951018 nrv Remove holleriths
C 970307 nrv Change 8 to max_sorlen
C 970310 nrv Set knsose=F for ABort
C 000126 nrv Remove "ierr=0" from end of routine. If an error occurred
C            we need to send it back.
!  2005Nov22 JMGipson.  Instead of passing fname down to other routines,
!            1) copy it to source_cat; 2) read the source catalog;
!            3) restore source_cat.
!            This makes the all the catalog routines similar.
! 2012Oct10 JMG. Modified to update version, catalog name info. 

C   Called by: SOSEL
C   Calls: GETSO, SESO, LISO, WRSOS, grget
!

C
C   Common/include:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'cat_src.ftni'
      include 'cat_name_version.ftni'
C   Input:
      character*128 fname ! alternate catalog name

C   Output:
      integer ierr !error return
      logical knsose  ! true if new selections

! local
      external display_src_item
      integer icol_wid/10/       !width of display.
      character*30 cname
      integer num_sel
      integer nsourctmp
      character*2 cfunc
      character*128 cat_name_old
      integer iptr
      character*1 cdo
C
C   Initialized:
C
C
C   1. Open the source catalog file.
C
      nsourctmp = nsourc

      cat_name_old=source_cat

      if (fname.ne.' ') then ! alternate catalog
        source_cat=fname
        kcat_src=.false.
      endif !alternate/standard

      cdo ="s"
      call make_source_list(cdo,ierr)
      if(ierr .ne. 0) goto  500

C   1.5  Save the current source names in LNASEL
      knsose = .true.
      norig=nsourc

C  2. This is the repeat loop in the program.
C     Continue through this section until user terminates.


      cfunc="SE"            !initilize to 'SELECT'
      goto 210              !skip prompts the first time through.

! ************Start of loop******************************
200   continue
      write(luscn,'(" SE - select entries for SKED")')
      write(luscn,'(" LI - list selected entries so far")')
      write(luscn,'(" AB - abort and return to SKED")')
      write(luscn,'(" :: - return to sked with new information")')
      WRITE(LUSCN,'("> ",$)')
      READ(LUUSR,'(a)') cfunc
      call capitalize(cfunc)

210   continue
      IF(CFUNC.EQ.'SE') THEN
        call Select_From_List(display_src_item,icol_wid,
     >       kcat_src_sel,num_cat_src,ierr)
! count number set.
        num_sel=0
        do iptr=1,num_cat_src
          if(kcat_src_sel(iptr)) then
            num_sel=num_sel+1
          endif
        end do
      ELSE IF (CFUNC.EQ.'LI') THEN
        iptr=0
        call display_src_item(cname,iptr)
        write(luscn,'(4x,a)') cname(1:icol_wid)
        num_sel=0
        do iptr=1,num_cat_src
          if(kcat_src_sel(iptr)) then
            num_sel=num_sel+1
            call display_src_item(cname,iptr)
            write(luscn,'(i3,1x,a)') num_sel,cname(1:icol_wid)
          endif
        end do
      ELSE IF (CFUNC.EQ.'AB') THEN
        nsourc = nsourctmp
        knsose = .false. ! no need to read select file again
        goto 500
      ELSE IF (CFUNC.EQ.'::') THEN  !finish
        IF (num_sel.GT.0)  THEN !prepare select file
          IF (knsose) THEN !write out select file
            WRITE(LUSCN,*) 'Writing out select file for SKED.'
            CALL wrsos(IERR)
            knewso = .true.
            IF (IERR.NE.0)  THEN
              WRITE(LUSCN,'("Error ",I3," writing select file")') ierr
              CLOSE(lusel)
            END IF !write out select file
            cfunc=" "
            call flget(cfunc)    !argument blank means use default flux file.
          else
            open(lusel,file=csofil)
            close(lusel,status='delete')
            open(lusel,file=csofil)
            close(lusel)
          endif
        END IF !prepare select file
        lsource_cat_use=source_cat
        call get_cat_version(lsource_cat_use,lsource_cat_version,ierr)
        goto 500
      END IF !finish
      goto 200
! ************ End of loop******************************
500   continue
      source_cat=cat_name_old
      return
      end

