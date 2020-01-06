      SUBROUTINE Select_From_List(cname,icol_wid,kselect,num_in_list,
     > ierr)
C
C     SEST lets the user interactively select from the list
C     of catalog stations for scheduling.
C
C  COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C  CALLed by: STCAT
C  CALLED SUBROUTINES: DSPST,SENCR,SETCR
C
!  Input
      External cname 			!External function that returns name.
      integer icol_wid
      integer num_in_list                   !Number of entries.
      logical kselect(num_in_list)          !true, then selected.

! Output
      integer ierr  ! error return

! functions
      integer fc_gwinsz,fc_gwinw
C  LOCAL:
      integer ilist_ptr,istart
      integer max_row,max_col          !number of data rows, columns.
      integer ihead,ifoot              !space for header and footer.
      integer max_x,max_y
      integer max_per_screen
      integer num_in_last_line

      character*1 lchar
      integer ikey
      character*80 cdisplay
      integer max_hscn,max_wscn
      parameter (max_hscn=48,max_wscn=135)
C     ilist_ptr - ilist_ptr of name in array of all names
C     knaeq - logical routine for comparison
      integer ix,iy,izero,itype
      logical krefresh             !refresh screen


      ix=0
      iy=0
      izero=0

! Completely rewritten by JMGipson

C  1. Display the page.
 
      itype=0 
      call start_mn(itype)
      if (itype.ne.1) then
        write(luscn,*) "Invalid terminal type: ",  itype 
        write(luscn,'("Can not sense cursor ")') 
        ierr=-1
        return
      endif
      ISTART = 1
      izero=0
     
      max_per_screen=0
      krefresh=.true.       		!write up the whole screen ?
C  2. Now sense each cursor position
      DO WHILE (.TRUE.)  		!get selection
99      continue
! A. Find current screen size.

        max_x=fc_gwinw()               	!reset the screen width in case this has changed.
        if(max_x .lt. 1 .or. max_x .gt. 999) then
          max_x=79
        endif
        max_x=min(max_x,iwscn)          !CURSES doesn't update the screen size, so the screen
        max_y=fc_gwinsz()
        max_y=min(max_y,ihscn)          !can't be bigger than when started, thoug it can be smaller.

        max_col=(max_x-1)/icol_wid
! B. Display the screen if appropriate.
        if(krefresh) then
          istart=max(istart,1)     !has to be at least 1.
          if(istart-1+max_per_screen .gt. num_in_list) then
            istart=1+
     >             (num_in_list-((max_row-1)*max_col+num_in_last_line))
            istart=max(istart,1)     !has to be at least 1.
          endif

          call display_list(istart,cname,icol_wid,kselect,num_in_list,
     >       max_x,max_y, ihead,ifoot,max_row,max_col)
          krefresh=.false.
          num_in_last_line=mod(num_in_list,max_col)  !Calculate how many items in last row
          if(num_in_last_line .eq. 0) num_in_last_line=max_col

        endif
        max_per_screen=max_col*max_row
      
! C. Find keys struck if any.
        call getxy_mn(ix,iy)  ! doesn't get the cursor?
! move cursor off of header line if it is there.
        if(iy .lt. ihead) then
          istart=istart-max_per_screen/3
          if(istart .lt. 0) istart=1
          krefresh=.true.
          goto 99
        else if(iy .ge. ihead+max_row) then
          istart = istart + max_per_Screen/3
          krefresh=.true.
          goto 99
        endif

        CALL SENkR_mn(IX,IY,ikey)
        lchar=char(ikey)
        call capitalize(lchar)

        if(lchar .eq. " ") then      !select or deselect stations.
! calculate which entry we point to.
          if(ix .ge. icol_wid*max_col) then
             ilist_ptr=max_col+(istart-1)
          else
             ilist_ptr=(ix/icol_wid)+istart
          endif
          ilist_ptr=ilist_ptr+(iy-ihead)*max_col
          if(ilist_ptr .ge. num_in_list) ilist_ptr=num_in_list           !don't go past the last one to display.
! now space to start of this entry
          iy=((ilist_ptr-istart)/max_col)+ihead
          ix=mod(ilist_ptr-istart,max_col)*icol_wid
          call setcr_mn(ix,iy)
          kselect(ilist_ptr)=.not.kselect(ilist_ptr)
! And rewrite it.
          if(kselect(ilist_ptr)) call reverse_on_mn
          call cname(cdisplay,ilist_ptr)
          call addstr_f(cdisplay(1:icol_wid))
          if(kselect(ilist_ptr)) call reverse_off_mn
! And space to the start of th next entry.
          ilist_ptr=min(ilist_ptr+1,num_in_list)
          iy=((ilist_ptr-istart)/max_col)+ihead
          ix=mod(ilist_ptr-istart,max_col)*icol_wid
          call setcr_mn(ix,iy)
          goto 99
        endif
! something else.

        IF(lchar .eq. "E") then
          call end_mn
          ierr=0
          return
        else if(lchar .eq. "F") then   !goto first page
          istart=1
          krefresh=.true.
        else if(lchar .eq. "L") then   !goto last page
          istart=num_in_list           !appropriate start value is found above.
          krefresh=.true.
        else if(lchar .eq. "N") then
          istart=istart+max_per_screen
          krefresh=.true.
        else if(lchar .eq. "P") then
          istart=istart-max_per_screen
          if(istart .le. 0) istart=1
          krefresh=.true.
        else if(lchar .eq. "R") then
          if(istart+max_per_screen .gt. num_in_list) then
             istart=num_in_list-max_per_screen
          endif
          krefresh=.true.
        endif
      END DO      ! selection loop
      END
