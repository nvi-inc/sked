      SUBROUTINE SNRAC(NSTN,ISTN,nsor,icod,lu,mjd,ut,IERR)
C
C   SNRAC calculates actual SNRs achieved in the schedule,
C         given the station durations in IDURST and the
C         input date/time (for calculating observed flux
C         and SEFDs). SNRs are placed in IACTBL.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
!      include 'flux.ftni'  
      include '../skdrincl/constants.ftni'
      include 'major.ftni'
C
C  INPUT:
C     NSTN - number of stations
      integer ISTN(MAX_STN) ,nstn,nsor,icod,lu,mjd
C     nsor - source index
C     icod - frequency code index
C     lu - non-zero to print error messages
C     mjd - date of observation, set to -1 for full-baseline
      real*8 ut ! time of observation
C
C  OUTPUT:
      integer ierr
C
C  Subroutines:
C  CALLED: gtban (get bands in this code), OBSFL (observed flux),
C          sefdel (adjust SEFD for elevation)
C  Called by: AUCHK, CHSNR, LSCUR, NEWOB, SNRSK, SUMCM
C
! functions
      integer ibnum !function
      real*8 snr_per_sec

! local variables
   
      integer NumBand
      integer iband(max_band) ! band indices for this freq. code
C     obsflux - returned from OBSFL
C     sefdsti,sefdstj - elevation-adjusted SEFDs returned from SEFDEL
   
      integer ist   
      integer is,i,j,js,ibl,iscan_time,k,iba
  
  
      real*8 temp                            !short lived temporary variable
      real*8 bit_eff                         !loss due to 1 or 2 bit sampling
      real*8 corr_eff                        !other losses in correlation process.
      real*8 bit_eff_1bit 
      real*8 bit_eff_2bit
     
      integer npass,ntrks,nhead,ibit
 
  
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 890711 CREATED
C     NRV 891127 Changed calling sequence, moved in code from
C                SNRSK to calculated SNRs
C     NRV 891207 Added IMARG margin to check on SNRs
C     NRV 910924 Add call to OBSFL, remove baseline-flux loop
C     NRv 920522 Add call to SEFDEL
C     nrv 940620 Change to use number of recorded tracks instead of
C                simply the number of frequencies. This distinguishes
C                between modes A and C and accounts for VLBA having
C                fewer channels.
C     nrv 950405 Use 2-letter codes in messages.
C 970317 nrv For continuous tape, adjust durations by running time.
C 990126 nrv Loop index should be NumBand from GTBAN (not "NumBandnd" from where?)
C 991119 nrv Use single-baseline SNR target if specified.
! 2003Nov18 JMG got rid of holleriths.
! 2007Jul02 JMG. Added flux.ftni (split off from sourc.ftni)
! 2008Jun10 JMG better error messages.
!               Moved checking of trk_flux_sefd into separate routine
! 2010Mar25 JMG Print out source message. 
! 2013Apr25 JMG. Modified for simple broadband calculations.
! 2013Jul02 JMG. Modified to do the calculation correctly in the case of 
!                A) 2-bit sampling; B) DiFX processing.
! 2013Sep17 JMG. Added ast_margin
! 2014Mar27 JMG. Now this routine just calculates SNR. A separate routine determines if good/bad. 
! 2021-12-07 JMGipson. Removed checking to see if stations and sources were set up.
!            This is now done once after the schedule file is read. 

C
C
C 
      IERR=0
      
C  2. Calculate actual SNR achieved by baseline using the
C     shortest scan length of the two stations, since this
C     is the amount of data that will be correlated.
C
! For 1-bit sampling.
!      bit_eff=0.6366d0
! For 2-bit  
!      bit_loss=0.881d0     
      call gtban(icod,NumBand,iband)
 
! Find the bit_efficiency and hte correlator efficiency.
      is=istn(1)
      call itras_params(is,icod,npass,ntrks,nhead,ibit)
! This assumes that all stations use the same efficiency.
      bit_eff_1bit=0.637
      bit_eff_2bit=0.881
      if(ibit .eq. 1) then
           bit_eff=bit_eff_1bit
      else
           bit_eff=bit_eff_2bit
      endif 

! The correlator effiency depends on the correlator.
! Bonn switched over in December 2010.  
! Haystack, Westford switched in July 2013.
! USNO will switch sometimein 2014. 
! For simplicity use single cutoff date.
      if(iyrcur(is) .le. 2010) then
         corr_eff=0.8995d0    !mark4 correlator
      else
         corr_eff=0.970d0     !diFX correlator
      endif 
!      write(*,*) "Duration: ", idurst(1:nstn) 


! Initialize elevation dependent SEFDs for the staitons.               
       do iba=1,2 
         do i =1, Nstn 
            j=istn(i)         
            sefdstel(iba,j) =sefdel(iba,nsor,j,mjd,ut)
         end do
       end do     
     
      DO I=1,NSTN-1 ! first station
        IS=ISTN(I)          
        DO J=I+1,NSTN ! second station
          JS=ISTN(J)
          ibl=ibnum(is,js)
C Calculate scan-time w/o the synch factor. 
          iscan_time=MIN(IDURST(IS),IDURST(JS))-itsync
! idurxt(is) is time before start of official scan that station arrives on source.
! Determine the MUTUAL time onsource.  
          ist=min(idurxt(is),idurxt(js))
          iscan_time=iscan_time+ist
          do k=1,NumBand
            iba = iband(k)  
            if(kvscan) then 
              temp=snr_per_sec(icod,iba,nsor,is,js,mjd,ut,
     >                   ibit,corr_eff,bit_eff)
              if(temp .gt. 0) then
!                 write(*,*) iscan_time, i,j,temp*sqrt(dble(iscan_time))
                 iactbl(iba,ibl)=nint(temp*sqrt(dble(iscan_time)))
!                 write(*,*) iba, is,js, " | ", iactbl(iba,ibl) 
              else
                 iactbl(iba,ibl)=-1              
              endif 
            else
              iactbl(iba,ibl)=-1
            endif
          end do 
        end do
      end do 

      call snr_find_numgood_numbad(icod,nsor,istn,nstn,lu)
      return
    

      RETURN
      END
