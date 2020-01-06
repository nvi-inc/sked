      SUBROUTINE SUMHD(lkind,lback2,iper_hour,ISTIM,LU)
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT
      character*(*) lkind,lback2
      integer iper_hour
      integer lu,istim

C  LOCAL
      character*8 lfront
      character*22 lback
      integer i

C INITIALIZED
C
C  DATE  WHO  WHAT
C 841111 MWH  CREATED
C 880314 NRV  DE-COMPC'D
C 930225 nrv  implicit none
C 951017 nrv Remove holleriths
C 990915 nrv Replace REIO with WRITE
C 2003Nov25 JMG completely rewritten
C
!   per_hour  Total   Labels   Spacing
!    2         48       4        12
!    4         96       8        12
!    6        144      12        12
!    8        192      24         8
!   10        240      24        10


      lfront=lkind
      lfront(8:8)="|"
      lback="| "//lback2

      write(lu,"(10x, i2,' chars/hour')") iper_hour
      if(iper_hour .eq. 2) then
        write(lu,"(2x,a,4(i2,10x),a)") lfront,(i*6,i=0,3),lback
      else if(iper_hour .eq. 4) then
        write(lu,"(2x,a,8(i2,10x),a)") lfront,(istim+i*3,i=0,7),lback
      else if(iper_hour .eq. 6) then
        write(lu,"(2x,a,12(i2,8x),a)") lfront,(i*2,i=0,11),lback
      else if(iper_hour .eq. 8) then
        write(lu,"(2x,a,24(i2,6x),a)") lfront,(i,i=0,23),lback
      else if(iper_hour .eq. 10) then
        write(lu,"(2x,a,24(i2,8x),a)") lfront,(i,i=0,23),lback
      endif

      RETURN
      END
