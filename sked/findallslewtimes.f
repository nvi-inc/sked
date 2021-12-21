      subroutine FindAllSlewTimes(IstnVec,NumStn,IsrcVec,NumSrc,
     > MJD,UT, tslew,tslew_all, itr,its,ksrcup)


      include '../skdrincl/skparm.ftni'
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
! passed variables
      integer NumStn,NumSrc     !number of stations, number of sources
      integer IstnVec(NumStn)
      integer iSrcVec(NumSrc)

      integer MJD(*)        !current MJDay
      double precision UT(*)  !and time in seconds
! returned
      real TSLEW(max_sor,MAX_STN)       !slew time
      real tslew_all(max_sor)           !time for all stats to arrive on source.
      integer ITR(MAX_SOR,MAX_STN),ITS(MAX_SOR,MAX_STN)

      logical ksrcup(Max_sor)        !is source up at some stations.

! local variables
     
      character*2 cwrap_new
   
      integer isrc,istat
      integer nup
      logical kup
      integer islew_info
      integer nrs
      integer nrise,nset
      double precision  AZ,EL, HAR,DEC,X30,Y30,X85,Y85
      integer i,j
      double precision trise
      real az_now,az_new
      real el_now,el_new

! 2005Jun13 JMGipson.  Modified to compute time when all stations on source.
      DO  I=1,NumSrc ! source loop for mutual vis
        ISrc=iSrcVec(I)
        NUP = 0
        tslew_all(isrc)=0
        DO  J=1,NumStn ! station loop
          istat = iStnVec(J)
          cwrap_new=" "
          IF  (NSORcur(istat).GT.0) THEN ! we're observing a source, calculate slewing
            CALL SLEWT(NSORcur(istat),MJD(istat),ut(istat),
     .      ISrc,istat,cwrap_cur(istat),cwrap_new,
     .      TSLEW(isrc,istat),0,trise,tsris,st0cur,frac,knov,islew_info,
     &      az_now,el_now,az_new,el_new)
          ELSE  ! not initialized
            TSLEW(isrc,istat) = 0.
          ENDIF
          IF  (TSLEW(isrc,istat).GE.0) THEN
            tslew_all(isrc)=max(tslew_all(isrc),tslew(isrc,istat))
            if (isrc.gt.nceles) then !satellite
              CALL CVPOS(ISrc,istat,MJD(istat),
     >            UT(istat)+tslew(isrc,istat),
     >            AZ,EL, HAR,DEC,X30,Y30,X85,Y85,KUP)
            else !celestial
              CALL isup(ISrc,istat,UT(istat)+tslew(isrc,istat),kup,nrs)
              if (knov) kup=.true.
            endif
            IF (KUP) NUP = NUP+1
          END IF
        END DO  !station loop
C
        CALL LOOKA(Isrc,NumStn,iStnVec,TSLEW,ITR,ITS,NRISE,NSET)
        ksrcup(isrc) = (NUP.GE.2.OR.NUP.EQ.1.AND.NRISE.GE.1)
      END DO  !source loop for mutual vis
      return
      end
C







