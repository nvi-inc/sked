      subroutine minor_init 
      implicit none 
! initialize the minor modes.
!  2010Mar10  JMGipson. Removed obsolete srcfloor and TapeWaste
!  2015Nov13  JMGipson. Added kCovar for covariance optimization. 
!  2019Mar14  JMGipson. Added implicit none. Fixed several uninitalized variables

! default is like the last NRV version of sked.
      include 'minor.ftni'
      include '../skdrincl/constants.ftni'
! if the KModeNorm=.true., use relative normalization

!    kAstro=         kastro
      kAstro=.false.
      kAstroNorm=.false.
      rAstroWt=1.
!    kBegScan=          kslew,
      kBegScan=.false.
      kBegScanNorm=.true.
      rBegScanWt=1.
    
      kCovar=.false.
      kCovarNorm=.true.
      rCovarWt=1.
!    kEndScan=          ktimo,
      kEndScan=.false.
      kEndScanNorm=.true.
      rEndScanWt=1.
!    LoDec
      kLowDec=.false.
      kLowDecNorm=.false.
      rLowDecWt=1.
       
!    kNumLoEl=          iloel .gt. 0
      kNumLoEl     =.false.
      kNumLoElNorm =.false.
      rNumLoElWt   =1.
      rloel=7.*deg2rad

!    kNumRiseSet=       krisst,
      kNumRiseSet     =.false.
      kNumRiseSetNorm =.false.
      rNumRiseSetWt   =1.
!    KNumObs=           kobso,
      kNumObs=.false.
      kNumObsNorm=.true.
      rNumObsWt=1.
!    kSkyCov=           kpara,
      kSkyCov=.false.
      kSkyCovNorm=.true.
      rSkyCovWt=1.
!    kSrcEvn=        iSrcEvnMode
      kSrcEvn=.false.
      kSrcEvnNorm=.false.
      rSrcEvnWt=1.
      iSrcEvnMode=0
!    kStatEvn=       iEvnSrcMode
      kStatEvn=.false.
      kStatEvnNorm=.false.
      rStatEvnWt=1.
      iStatEvnMode=0
!    kStatIdle=         ktstStatIdle,
      kStatIdle=.false.
      kStatIdleNorm=.false.
      rStatIdleWt=1.
!    kStatWt=           ktapo,
      kStatWt=.false.
      kStatWtNorm=.false.
      rStatWtWt=1.
!    kTimeVar=          NONE
      kTimeVar=.false.
      kTimeVarNorm=.false.
      rTimeVarWt=1.
  
      return
      end










