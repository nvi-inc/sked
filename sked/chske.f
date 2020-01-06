      SUBROUTINE chske(kerr)
C
C     Checks schedule for all sources/stations selected.
C
C   HISTORY:
C     WHO   WHEN   WHAT
C     GAG   891116 CREATED
C     gag   891201 made call to rspyn to get user response
!    2008Nov11  JMGipson.  Replaced call to RSPYN by call to kyes_to_prompt

C
C   parameter file 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
C
C  OUTPUT:
      INTEGER IREC,  KERR 
C      - IREC is loop counter
C      - KERR is error return from subroutines 
C
C   SUBROUTINES
C     CALLED BY:  SKCLS
C     CALLED: UNPAK, RSPYN
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
! Functions     
      logical kyes_to_prompt
! local
      character*60 lprompt
     
      do irec=1,nobs           
        cbuf=cskobs(iskrec(irec))
        CALL UNPAK(KERR,0)
        IF (KERR.NE.0) goto 100
      end do 
      return

100   continue     !come here on error.
     
        WRITE(LUSCN,'(a)')
     > 'CHSKE01 - Source/station/frequency selection is incomplete for'
     >   //' scheduled observations.'
   
        lprompt=
     >   'Would you rather exit now, saving the shedule as is? (Y/N)'
        if(kyes_to_prompt(lprompt)) then
          kerr=0
         else
          kerr=-1
         endif
         return
        
      END
