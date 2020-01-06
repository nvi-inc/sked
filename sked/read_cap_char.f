      SUBROUTINE read_cap_char(lchar)
C
C  This subroutine gets a single-character user response. 
C  Any response is returned in lchar.
C
!   completely rewritten and simplified.
!    2006Sep26 JMGipson
C  INPUT:
       include '../skdrincl/skparm.ftni'
       include 'skcom.ftni'  !This is where luusr is passed

      character*1 lchar
C
      read(luusr,*) lchar
      call capitalize(lchar)

      RETURN
      END
