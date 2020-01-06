      subroutine display_list(istart,cname,icol_wid,kselect,num_in_list,
     >  max_x,max_y, ihead,ifoot,max_row,max_col)

!  COMMON:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
! History
!  2005Aug10  First version.
!  2017Feb24  Changed "Cursor keys" to "Cursor" to fit in lcmd.

!  Input
      integer istart                    !First entry to display
      External cname 			!Note, this is a function.
      integer icol_wid
      integer num_in_list               !Number of entries.
      logical kselect(num_in_list)      !true, then selected.
      integer max_x,max_y
      integer max_row,max_col           !maximum number of
! output
      integer ihead,ifoot              !height of header,footer in lines

! Local.
      character*80 cdisplay
      integer max_cmd,icmd
      parameter(max_cmd=8)
      character*10 lcmd(max_cmd)/"Cursor"," or ijkl"," ",
     >  "<E>nd","<F>irst","<N>ext","<P>rev","<R>efresh"/
      integer ilen
      integer itemp

! variables
      integer izero,ione
      integer iptr
      integer icol,irow

!  History
!    2005Jun02  JMGipson

      itemp=max_x/10            !commands per line
      ifoot=(max_cmd-1)/itemp+1 !number of lines.
      if(itemp*10 .eq. max_x) ifoot=ifoot+1

      ifoot=ifoot+1   !blank line

! clear the screen and put the cursor at the top.
      izero=0
      ione=1
      CALL SETCR_mn(izero,izero)
      CALL clear_mn

! Write up header line
      iptr=0
      call reverse_on_mn
      call cname(cdisplay,iptr)
      do icol=1,max_col
         call addstr_f(cdisplay(1:icol_wid))
      end do
      call reverse_off_mn
      call nl_mn
      ihead=1

      max_row=max_y-ihead-ifoot       !calculate number of lines of data to write.
      max_col=(max_x-1)/icol_wid
! Now write up info
      icol=0
      irow=0
      iptr=istart
      do while(irow .lt. max_row .and. iptr .le. num_in_list)
        if(kselect(iptr)) call reverse_on_mn
        call cname(cdisplay,iptr)
        call addstr_f(cdisplay(1:icol_wid))
        if(kselect(iptr)) call reverse_off_mn
        iptr=iptr+1
        icol=icol+1
        if(icol .eq. max_col) then
          call nl_mn
          icol=0
          irow=irow+1
        endif
      end do
      if(icol .ne. 0) then
        call nl_mn
      endif

      call nl_mn
      call refresh_mn
! Output footer lines.
      call reverse_on_mn
      ilen=0
      do icmd=1,max_cmd
        if(ilen+10 .ge. max_x) then
           call nl_mn
           ilen=0
        endif
        call addstr_f(lcmd(icmd))
        ilen=ilen+10
        call refresh_mn
      end do
      call reverse_off_mn
      CALL SETCR_mn(izero,ione)
      call refresh_mn

      RETURN
      END
