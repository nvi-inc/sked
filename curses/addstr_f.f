      subroutine addstr_f(str)

C 040506 ZMM IMPLICIT NONE
      IMPLICIT NONE

      character*(*) str
c
      integer*2 istr(128)
      integer il
c
      il=min(254,len(str))
      call char2hol(str,istr,1,il)
      call char2hol(char(0),istr,il+1,il+1)
      call addstr_mn(istr)

      end

C ***********************************************************

      subroutine menul_f(str,logi,iypos)

C  040506  ZMM  IMPLICIT NONE
      IMPLICIT NONE

      character*(*) str
      logical logi
      integer*4 iypos
c
      integer*4 ix
c
      call addstr_f(str)
      call getxy_mn(ix,iypos)
      IF(logi) then
        call addstr_f("T")
      else
        call addstr_f("F")
      endif
      call nl_mn

      end

C ***********************************************************

      subroutine menuf_f(str,name,ixpos,iypos)

C  040506  ZMM  IMPLICIT NONE
      IMPLICIT NONE

      character*(*) str,name
      integer*4 ixpos,iypos
      integer trimlen
c
      call addstr_f(str)
      call getxy_mn(ixpos,iypos)
      call addstr_f(name(:trimlen(name)))
      call nl_mn

      end

C ***********************************************************

      subroutine getstr_f(str)

C  040506  ZMM  IMPLICIT NONE
      IMPLICIT NONE

      character*(*) str
c
      integer*2 istr(128)
      integer ilen
      integer*4 getstr_mn
c
      ilen=min(getstr_mn(istr),len(str))
      call hol2char(istr,1,ilen,str)
c
      end
