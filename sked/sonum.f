      integer function sonum(isrc)
!
      integer isrc
! function
      character*1 sochr
C  Return the integer equivalent for ASCII characters.
C  Used to indicate sources on sked plots.
C  940329 nrv created
!  2005JMgipson  Modified to use sochr so that is consistent.
!

      sonum=ichar(sochr(isrc))
      return
      end
