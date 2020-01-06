      subroutine major_init
! initialize the major modes.
! History
! 2008May22  Moved many parameters from $PARAM to $MAJOR
! 2011Apr25  Added MaxAngle
! 2013Oct08  Changed rMinSunAngle to 4 deg from 15deg
! 2017DEC20  KOL added a parameter ksplittwins
      include '../skdrincl/skparm.ftni'
      include 'major.ftni'
! Default values. These are used if not set in schedule.
      rcovar_win=-1      	!indicate not set
      radd_noise=30
      rBestPerCent=60.0/100.0   !Keep 60% by default

      ksnrwts  =.true.
      kOptBySky=.true.
      kallblgood=.false.        !all baselines don't  have to be good.
      kfillin =  .true.
      ksplittwins=.false.
!      kallowsubnet =.true.      !allow dynamic subnetting

      rSunMinAngle = 4          !minimum sun distance
      rMinAngle    =15          !15 degrees.
      rMaxAngle    =180         !180 degrees
      iMinBetween=20*60  	!20 minutes in units of seconds
 
      iMaxSlewTime=300          !5 minutes
      ifilltime = 120
      ifillmin  = 3
      ifillbest =80

      return
      end
