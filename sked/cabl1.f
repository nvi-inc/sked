      subroutine CABL1(ISTN,NSOR,mjd,ut,cwrap)
      implicit none
      
!  CABL1 determines the cable wrap setting for the first source.
!  There is no previous source, so we don't have a "come from" cable wrap. 
!  If there is a unique wrap, use that, otherwise pick one.

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

!  INPUT VARIABLES:
      integer nsor,mjd,istn
      double precision UT
!  OUTPUT VARIABLES:
      character*2 cwrap

!  COMMON BLOCKS USED
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'

! History      
! 2022-05-12 J Gipson.  Changed default wrap to "-" from " "
      

!  CALLING SUBROUTINES: NEWOB
!  CALLED SUBROUTINES: CVPOS,CABLW
!
!  LOCAL VARIABLES
      REAL aznow,elnow,hanow,decnow,x30now,y30now,x85now,y85now
      LOGICAL KUP ! Returned from CVPOS, TRUE if source within limits
! Default is no cable wrap.
      cwrap="-"

      IF (IAXIS(ISTN).EQ.3.or.iaxis(istn).eq.6.or.iaxis(istn).eq.7) then
        CALL CVPOS(nsor,ISTN,MJD,UT,AZNOW,ELNOW,HANOW,DECNOW,
     >    X30NOW,Y30NOW,X85NOW,Y85NOW,KUP)
C                    this calculates the current telescope position
        IF (AZNOW.LT.STNLIM(1,1,ISTN)) AZNOW=AZNOW+TWOPI
        IF (AZNOW.LT.(STNLIM(2,1,ISTN)-TWOPI)) cwrap="W" 
      
      endif
C
990   RETURN
      END
