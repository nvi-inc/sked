      integer function renam(csrc,cdest)
      implicit none
! Rename a file using system move command
! WRitten 2006Dec06 JMGipson. previous version was written in (poor) C
! and had a bug
! AEM20170119 add quotes to the system call arguments
      character*(*) csrc,cdest
! function
      integer trimlen
! local
      integer nch1,nch2
      integer system
! start of code
      nch1=trimlen(csrc)
      nch2=trimlen(cdest)
      renam=system("mv '"//csrc(1:nch1)//"' '"//cdest(1:nch2)//"'"
     > //char(0))
      return
      end
