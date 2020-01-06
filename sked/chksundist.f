      subroutine ChkSunDist(isource,csorna,mjd,ut,
     > kdisplay,luscn,rSunMinAngle,ierr)
      implicit none
! Check the distance to the sun. Indicate an error if too close.
!
! History
!  2014Sep23 JMG. Previously always output the name of the 1st source. 
! function
      real sunarc
! passed
      integer isource  !source index
      character*8 csorna(*)
      integer mjd
      double precision ut
      real   rSunMinAngle   !minimum sunangle.

      logical kdisplay
      integer luscn     !output
 
      integer ierr      !ierr

! local
      real arcd

      ierr=0
  
      ARCD = SUNARC(isource,mjd,ut)
!      write(*,*) "ChkSunDist ", arcd, rSunMinAngle
      IF (ARCD.NE.-1.0.AND.ARCD.LT.rSunMinAngle) THEN !too close
        if(kdisplay.and. luscn .ne.0)  
     >    WRITE(LUSCN,  "(' ChkSunDist: ',a8,' is ',f6.2,
     >       ' deg from sun. Must be larger than ',f6.2, ' deg.')")
     >        csorna(isource),arcd, rSunMinAngle 
        IERR=1
        RETURN
      ENDIF !too close
      return
      end

