      SUBROUTINE FRCAT(ierr,knfrse,cdo)

!  FRCAT handles selection, and listing of the frequency catalog selections.
C
C  COMMON BLOCKS USED:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_mode.ftni'
C
C  INPUT VARIABLES:
      character*1 cdo !s=standard, c=current--find current mode, and exit.

C  OUTPUT:
      logical knfrse   ! true if new selections were made
      integer IERR     ! error return
C
C  CALLED BY: FRSEL
C  CALLS:       GETMO      read modes.cat
C               GETFR      read freq.cat
C               SEFR       select interactively
C               FRLIST     list selections
C               WRFRS      write scratch files
C
C  LOCAL VARIABLES:
      external display_mode_item
      integer icol_wid/58/
      character*80 cname
      character*2 cfunc
      integer ncodes_sav
      integer num_sel
      integer iptr

! 2005Jun06  JMG. Completely rewritten.
! 2005Aug10  JMG. Extracted part of routine that reads in modes & checks for match.
! 2020Jun02  JMG. Was not initilizing ncodes_sav. 

      ncodes_sav=ncodes
!  1.1  Read in catalog.
      knfrse = .true.   !initialize

! Read in the modes catalog & find matches.
      call make_mode_list(cdo,ierr)
      if(cdo .eq. "a") return
      if(ierr .ne. 0) return

! If we are just refreshing, then we are done.
      if(cdo .eq. "c") then
        knewfr = .true.
        return
      endif
! 2.0  Continue with selection
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
        call Select_From_List(display_mode_item,icol_wid,
     >       kcat_mode_sel,num_cat_mode,ierr)
! count number set.
        num_sel=0
        do iptr=1,num_cat_mode
          if(kcat_mode_sel(iptr)) then
            num_sel=num_sel+1
          endif
        end do
      ELSE IF (CFUNC.EQ.'LI') THEN
        iptr=0
        call display_mode_item(cname,iptr)
        write(luscn,'(4x,a)') cname(1:icol_wid)
        num_sel=0
        do iptr=1,num_cat_mode
          if(kcat_mode_sel(iptr)) then
            num_sel=num_sel+1
            call display_mode_item(cname,iptr)
            write(luscn,'(i3,1x,a)') num_sel,cname(1:icol_wid)
         endif
        end do
      ELSE IF (CFUNC.EQ.'AB') THEN
        ncodes = ncodes_sav
        knfrse = .false.
        goto 500
      ELSE IF (cfunc.eq.'::') THEN  !finish
        IF (num_sel.GT.0)  THEN !prepare select files
          IF (knfrse) THEN !write out select file
            write(luscn,
     >         "(/'Reading catalogs and writing out files for sked.')")
            call wrfrs(IERR)
            knewfr = .true.
            if (ierr.gt.0) then ! incomplete selection
              write(luscn,"('FRCAT02 - Incomplete selection. You may',
     .          ' need to modify the catalogs to supply the missing',
     .          ' information.')")
            else IF (IERR.lt.0)  THEN
              write(luscn,"('FRCAT01 - Error ',I5,' reading catalog ',
     .          'or writing select file.')")
               goto 500
            else ! ok
              goto 500
            END IF
          END IF !write out select file
        else
          open(lusel,file=cfrfil)
          close(lusel,status='delete')
          open(lusel,file=chdfil)
          close(lusel)
          goto 500
          END IF !prepare select files
        END IF !finish

      goto 200
! ************ End of loop******************************

500   continue
      write(luscn,'("FRCAT FINISHED")')
      return
      END
