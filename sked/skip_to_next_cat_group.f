      subroutine skip_to_next_cat_group(keof)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
! Read in lines from the catalog until we come to a new group.
! Can tell it is a new group because:
! 1. Line is not blank   AND
! 2. first character is not "*"    comment
! 3. first non-blank character is not "-"   -->continuation line
      logical keof     !Return if at EOF

! function
! local varialbes
      integer nch
      integer i
      nch=len(cbuf)   !get the length of this string.

      keof=.false.
100   continue
      read(lucat,'(a256)',end=200) cbuf
      if(cbuf .eq. " " .or. cbuf(1:1) .eq. "*") goto 100
! check to see if a continuation line, which we also ignore.
      do i=1,nch
        if(cbuf(i:i) .ne. " ") then
          if(cbuf(i:i) .eq. "-") goto 100   !first non-blank is "-": Read next line.
          return                            !something else:         exit.
        endif
      end do

200   continue
      keof=.true.
      return
      end
