      real*8 function snr_per_sec(icod,iba,nsor,is,js,mjd,ut,
     >  ibit,corr_eff,bit_eff)
! computer SNR for 1 second of observing. 
! written so that snrac and snrsk use the same algorithm for computing snr. 
! History
! 2014Mar25  First version 
! 2018Jul30  Now account for different bandwidths for different stations. 
! 2021-12-07  REmoved calculation of elevation dependent SEFD. 
! 2021-12-07  Removed ibl argument. Now calculated in obsfl. 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/freqs.ftni'

! Passed variables
      integer icod     !code
      integer iba      !band
      integer nsor     !source number
      integer is,js    !station number  
      integer mjd      !mJD
      real*8 UT
      integer ibit     !one or two bit sampling
      real*8 corr_eff   !correlator efficiency
      real*8 bit_eff    !bit efficiency. Adjust for number of bits sampled. 

! Functions
      real sefdel !function
      integer ibnum 
    
! local
      real*4 sefdsti,sefdstj  ! elevation-adjusted SEFDs
      real*4 obsflux          ! flux on baselines
      integer numtrk          !number of tracks. 
      real*8 bw_tot  
    
!initialize.
      snr_per_sec=0

      call obsfl(icod,iba,nsor,is,js,mjd,ut,obsflux)
!      sefdsti =  sefdel(iba,nsor,is,mjd,ut)
!      sefdstj =  sefdel(iba,nsor,js,mjd,ut)
      numtrk= min(ntrkn(iba,is,icod),ntrkn(iba,js,icod))/ibit     !note correction for bit!           
      BW_tot=min(bb_bw(is),bb_bw(js))
      if(Bw_tot .eq. 0) then         !at least one of the stations is not broadband.
         BW_tot=min(vcband(1,is,icod),vcband(1,js,icod))*numtrk     
      endif
!Note: Below assumes that sefdstel arrays have been previously calculated.       
      if(obsflux .gt. 0) then
        snr_per_sec=corr_eff*bit_eff*obsflux*
     >    sqrt(2.d0*Bw_tot*1.d6/(sefdstel(iba,is)*sefdstel(iba,js)))    
!     >    sqrt(2.d0*Bw_tot*1.d6/(sefdstel(iba,is)*sefdstel(iba,js)))    

      else
        snr_per_sec=0.d0
      endif 

      return
      end      


