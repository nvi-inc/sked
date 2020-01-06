C@LOOKA
C
      SUBROUTINE LOOKA(IS,NST,ISTN,TSLEWST,ITR,ITS,NRISE,NSET)
C
C     LOOKA calculates rise/set times at all stations for a source
C
      implicit none 
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/constants.ftni'
C
C     CALLING ROUTINES: NEXTC
C     CALLED SUBROUTINES: CVPOS
C
C  INPUT:
      integer ISTN(MAX_STN),nst,is
C        - array with station indices
C     NST - number of stations in subnet
C     IS - source index number
      real*4 TSLEWST(MAX_SOR,MAX_STN)
C     - slewing times for each station, already calculated
C
C  OUTPUT:
      integer ITR(MAX_SOR,MAX_STN),ITS(MAX_SOR,MAX_STN),nrise,nset
C       - number of seconds to rise, set for sources at all stations,
C         value of -1 indicates no change during lookahead time
C     NRISE, NSET
C       - number of elements in ITR, ITS with info
C
C  LOCAL VARIABLES
      real*4 az,el,har,dec,x30,y30,x85,y85
      real*8 UT
C      - holds time for CVPOS calculations
      integer il,iil,j,kj
C     IL,ILL - counter, increment for lookahead loop
      LOGICAL KRISE,KSET
C      - true if a source rises,sets during lookahead
      LOGICAL KUP
C               - from CVPOS, true if source is up at station
      LOGICAL KUPPRE
C      - previous loop value of KUP
      integer nrs
      real*8 gst    ! current GST 
      real*8 temp
C
C    History
C    890427 NRV Created
C    930225 nrv implicit none
C    931021 nrv Remove iterative loop for lookah and use rise/set arrays
C    931109 nrv Change to real*8 rise/set arrays
C    931112 nrv Slewing array is 2D including sources now
C    940112 nrv Replace old code for satellites
!    2016Sep26  JMG. Fixed subtle bug in the way that Rise/set times were calculated.

C
C
C   1. Initialize and start loop over stations.
C
      IIL=LOOKAH/20  ! units of 1/20th lookahead time
      NRISE=0
      NSET=0
      DO J=1,NST !station rise/set loop
        KJ=ISTN(J)
        KRISE=.FALSE.
        KSET=.FALSE.
        ITR(IS,KJ)=-1
        ITS(IS,KJ)=-1
        UT=UTCUR(KJ)+IDURcur(KJ)+IDLCUR(KJ)
        IF (TSLEWST(is,KJ).GT.0) UT=UT+TSLEWST(is,KJ)
        if (is.gt.nceles) then !satellite
          CALL CVPOS(IS,KJ,MJDCUR(KJ),UT,AZ,EL,HAR,DEC,
     .    X30,Y30,X85,Y85,KUPPRE)
        else !celestial
          CALL isup(IS,KJ,UT,KUPPRE,nrs)
        endif
        IF (NSORcur(KJ).GT.0) THEN !initialized
C
C   2. Check source position every 1/20 lookahead, and
C    quit when KUP changes.
C
          if (lookah.gt.0) then !check ahead for rise/set
C         The following code replaces the commented code below it.
            if (is.le.nceles) then !celestial

              call isup(is,kj,ut+lookah,kup,nrs)
              if (kup.and..not.kuppre) then !rising
                krise=.true.
! Compute the local GST 
                gst=st0cur(kj)+UT*frac
                if(gst .gt. twopi) gst=gst-twopi                
! this is the difference in times in GST. 
                temp=tsris(is,kj,nrs)-gst
                if(temp .lt. 0) temp=temp+twopi
! convert from radians to seconds.           
                itr(is,kj)=temp/frac 
!******* Old incorrect code
!                itr(is,kj)=((tsris(is,kj,nrs)-st0cur(kj))/frac) - ut ! time till rise
!                if (itr(is,kj).lt.0.d0) itr(is,kj)=itr(is,kj)+86400.d0
! ***end old 
              endif !rising
              IF (.NOT.KUP.AND.KUPPRE.AND.TSLEWST(is,KJ).GT.0) THEN !setting
                kset=.true.
! Compute the local GST 
                gst=st0cur(kj)+UT*frac
                if(gst .gt. twopi) gst=gst-twopi                
! this is the difference in times in GST. 
                temp=tsset(is,kj,nrs)-gst
                if(temp .lt. 0) temp=temp+twopi
! convert from radians to seconds.           
                its(is,kj)=temp/frac

!****** old incorrect code
!                its(is,kj)=((tsset(is,kj,nrs)-st0cur(kj))/frac) - ut ! time till set
!                if (its(is,kj).lt.0.d0) its(is,kj)=its(is,kj)+86400.d0
! **** end old 
              endif !setting
            else !satellite (old style calculations)
              IL=0
              DO WHILE (IL.LE.LOOKAH.AND..NOT.(KRISE.OR.KSET)) !lookahead loop
                IL=IL+IIL
                CALL CVPOS(IS,KJ,MJDCUR(KJ),UT+IL,AZ,EL,HAR,DEC,
     .          X30,Y30,X85,Y85,KUP)
                IF (KUP.AND..NOT.KUPPRE) THEN !station rise
                  KRISE=.TRUE.
                  ITR(IS,KJ)=IL
                  NRISE=NRISE+1
                ENDIF !station rise
                IF (.NOT.KUP.AND.KUPPRE.AND.TSLEWST(is,KJ).GT.0) THEN !station set 
                  KSET=.TRUE.
                  ITS(IS,KJ)=IL
                  NSET=NSET+1
                ENDIF !station set
                KUPPRE=KUP
              ENDDO !lookahead loop
            endif!celestial/satellite
          endif
        ENDIF !initialized
      ENDDO !station rise/set loop
C
C
C     3. Find mutual time to rising, setting
C
C       KM=ISTN(1)
C       DO J=2,NST !find first minimum
C         KJ=ISTN(J)
C         IF (ITR(KJ).LT.ITR(KM).AND.ITR(KJ).NE.-1) KM=KJ
C       ENDDO !find first minimum
C       ITR(KM)=-1
C       KM2=ISTN(1)
C       DO J=2,NST !find second minimum
C         KJ=ISTN(J)
C         IF (ITR(KJ).LT.ITR(KM2).AND.ITR(KJ).NE.-1) KM2=KJ
C       ENDDO !find second minimum
C       ITR(KM)=ITR(KM2)
C
C       KM=ISTN(1)
C       DO J=2,NST !find first minimum
C         KJ=ISTN(J)
C         IF (ITS(KJ).LT.ITS(KM).AND.ITS(KJ).NE.-1) KM=KJ
C       ENDDO !find first minimum
C       ITS(KM)=-1
C       KM2=ISTN(1)
C       DO J=2,NST !find second minimum
C         KJ=ISTN(J)
C         IF (ITS(KJ).LT.ITS(KM2).AND.ITS(KJ).NE.-1) KM2=KJ
C       ENDDO !find second minimum
C       ITS(KM)=ITS(KM2)
C
      RETURN
      END 
