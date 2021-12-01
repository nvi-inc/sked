      SUBROUTINE STCAT(ierr,knstse) 

C  STCAT handles selection, listing, and editing of the station
C  catalog files using a terminal interface.
C
C  COMMON BLOCKS USED:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_stat.ftni'

! History
!  2005Jun06  JMGipson  Completely rewritten.
!  2005Jul07  JMGipson.  Fixed problem with setting cat_ant_id1.
!                        Previous index was iptr.
!  2005Aug10  JMGipson.  Extracted part of routine that reads catalog
!                          and determines if we have a match.
!  2005Sep13  JMGipson.  Fixed problem with writing out list of selected stations.
!                        was cname(1:col_wid), but col_wid was too big.

C  INPUT VARIABLES:
C
C  OUTPUT:
      logical knstse  ! true if new selections were made
      integer IERR    ! error return

! Subroutine
      External display_stat_item
! local variables
      integer icol_wid/50/       !width of display.
      character*1 cdo

      character*50 cname
      character*2 cfunc
      character*2 cfunc_pre
      integer  nstatntmp

      integer iptr
      integer num_sel

! 1. Have user select stations interactively

      nstatntmp = nstatn
      knstse = .true.

      cdo="s"     !standard way of opening list.
      call make_stat_list(cdo,ierr)

      if (ierr.ne.0) return

!  2. This is the repeat loop in the program.
!     Continue through this section until user terminates by typing "::" or "AB"

      cfunc="SE"            !initilize to 'SELECT'
      goto 210              !skip prompts the first time through.

! ************Start of loop******************************
200   continue
      cfunc_pre=cfunc
      write(luscn,'(" SE - select entries for SKED")')
      write(luscn,'(" LI - list selected entries so far")')
      write(luscn,'(" AB - abort and return to SKED")')
      write(luscn,'(" :: - return to sked with new information")')
      WRITE(LUSCN,'("> ",$)')
      READ(LUUSR,'(a)') cfunc
      call capitalize(cfunc)

210   continue
      IF(CFUNC.EQ.'SE') THEN
        call Select_From_List(display_stat_item,icol_wid,
     >       kcat_stat_sel,num_cat_stat,ierr)
! count number set.
        num_sel=0
        do iptr=1,num_cat_stat
          if(kcat_stat_sel(iptr)) then
            num_sel=num_sel+1
          endif
        end do
      ELSE IF (CFUNC.EQ.'LI') THEN
        iptr=0
        call display_stat_item(cname,iptr)
        write(luscn,'(4x,a)') cname
        num_sel=0
        do iptr=1,num_cat_stat
          if(kcat_stat_sel(iptr)) then
            num_sel=num_sel+1
            call display_stat_item(cname,iptr)
            write(luscn,'(i3,1x,a)') num_sel,cname
         endif
        end do
      ELSE IF (CFUNC.EQ.'AB') THEN
        nstatn = nstatntmp
        knstse = .false. ! no need to read select file again
        goto 500
      ELSE IF (CFUNC.EQ.'::') THEN  !finish
        IF (num_sel.GT.0)  THEN !prepare select file
! indicate stations selected if  we haven't just done a listing
          if(cfunc_pre .ne. "LI") then
            iptr=0
            write(*,*) "Selected stations: "
            call display_stat_item(cname,iptr)
            write(luscn,'(4x,a)') cname//" ID" 
            num_sel=0
            do iptr=1,num_cat_stat
              if(kcat_stat_sel(iptr)) then
                num_sel=num_sel+1
                call display_stat_item(cname,iptr)
                write(luscn,'(i3,1x,a, " ", a )') num_sel,cname,
     >                cat_ant_id2(icat_stat_vec(1,iptr))
                endif
            end do
          endif
          IF (knstse) THEN !write out select file
            WRITE(LUSCN,'("Writing out select file for SKED.")')
            CALL WRSTS(IERR)
            knewst = .true.
            IF (IERR.NE.0)  THEN
              WRITE(LUSCN,'("Error ",I5," writing select file")') ierr
              CLOSE(lusel)
            END IF
          END IF !write out select file

        else
          write(*,*) "we are here?"
          open(lusel,file=cstfil)
          close(lusel,status='delete')
          open(lusel,file=cstfil)
          close(lusel)
        END IF !prepare select file
        goto 500
      END IF !finish
      goto 200
! ************ End of loop******************************



500   continue
      if(iverbose_level.ge.5) write(luscn,'("STCAT FINISHED")')
      END

