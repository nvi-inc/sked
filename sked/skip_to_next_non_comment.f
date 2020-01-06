      subroutine skip_to_next_non_comment(keof)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
! Read in lines from the catalog until we come to a new group.
! Can tell it is a new group because:
! 1. Line is not blank   AND
! 2. first character is not "*"    comment
      logical keof

      keof=.false.
100   continue
      read(lucat,'(a256)',end=200) cbuf
      if(cbuf .eq. " " .or. cbuf(1:1) .eq. "*") goto 100
      return

200   continue
      keof=.true.
      return
      end
