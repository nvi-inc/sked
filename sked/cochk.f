      SUBROUTINE cochk(ic,code)
C
C  This subroutine checks for identical frequency code and
C  replaces the second character with a number.
C
C  CALLED BY: WRFCLINES
C
C     WHO   WHEN   WHAT
C 900523 gag created
C 960408 nrv Cleaned up
! 2005Nov28 JMGipson. Rewritten to use ASCII.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C  INPUT:
      integer ic   !index of this code
      character*2 code(max_code)
!  Output
!    character*2 code(ic)        which may be changed.
! local
      logical kchange
      character*2 ctemp
      character*10 cnum
      data cnum/'123456789A'/
      integer iptr
      integer j
! Start of code
      ctemp=code(ic)

      kchange=.false.
      iptr=0
100   continue
      iptr=iptr+1
      do j=1,ic-1
        if(code(j) .eq. code(ic)) then  !check if a match. If so,  then change.
          kchange=.true.
          code(ic)(2:2)=cnum(iptr:iptr)
          if(iptr .eq.  10) then
            write(luscn,'("COCHK: Failure changing code")')
            code(ic)=ctemp
            return
          endif
          goto 100
        endif
      end do
! Arrive here if we go through once without a match.
      if(kchange) then
        write(luscn,'("COCHK:  Changing freq code from ",A," to ",A)')
     >     ctemp, code(ic)
      end if
C
      RETURN
      END
