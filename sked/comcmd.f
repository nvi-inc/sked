      SUBROUTINE COMCMD(linstq) 
C
C COMCMD lists the text typed by the user. The text goes
C onto the display unit. This is useful for annotating
C output files.
C
      include '../skdrincl/skparm.ftni'
C
C     INPUT VARIABLES:
      integer*2 linstq(*) 
C
C COMMON BLOCKS USED
      include 'skcom.ftni'
C
C CALLING SUBROUTINES: SKED 
C
C  LOCAL VARIABLES
      integer ic1
      character*256 cin
C
C  History
C 020227 nrv NEW.
C 020510 nrv Allow blank lines.
C
C
C     1. write out the string to the display unit.

      ic1 = linstq(1)
      if (ic1.eq.0) then ! blank
        write(ludsp,'()')
      else ! characters
        call hol2char(linstq(2),1,ic1,cin)
        write(ludsp,'(a)') cin(1:ic1)
      endif

      RETURN
      END
