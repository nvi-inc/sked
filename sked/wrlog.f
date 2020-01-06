      SUBROUTINE WRLOG(cdum)

C
C   HISTORY:
C     gag   891201 CREATED
C     gag   900326 added write buffer ibuftmp
C     nrv   930225 implicit none
C     nrv   930312 If the buffer is null, write a blank
!  2007Oct01 JMG   changed to use strings.
!    2017Dec04 JMG.  Now does open,write,close here. 
!   does open, write close here. 

C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'

      character*(*) cdum  !string to write

      OPEN(lulog,FILE=CLGFIL,err=100,  position="Append") 
      write(lulog,'(a)') trim(cdum)
      close(lulog)
      return

100   continue
      write(*,*) "Problem opening log file! "//trim(clgfil)
      stop
      
C
      RETURN
      END
