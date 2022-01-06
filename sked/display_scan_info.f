      subroutine display_scan_info(mjd,ut,mjdt,utt,icod,
     >  nsor,nstn,istn, tslew)
! Display information about new scan

! History
! 2008Nov12 JMGipson.  Removed from newob. Cleaned up a bit.
! 2014Apr07 JMGipson. Removed printing out of tape footage.
! 2014May02 JMGipson

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

! functions
      real*8 speed 
      integer*4 isecdif

! Input variables
      integer mjd                    !time of scan.
      double precision ut            !time of scan(seconds)
      double precision UTT(MAX_STN)  !Trial time (seconds)
      integer MJDT(MAX_STN)          !Trial date 
      integer icod
      
      integer nsor
      integer nstn                   !number of stations
      integer ISTN(*)                !Final stations in scan.        
      real TSLEW(MAX_STN)            !time allowed for slewing, by station       
    

      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'

! Local varaiables
      logical knewtape
      integer i,j                   !counter
      integer ift                   !foot (old or new)
      integer istat 
      integer ifspin                !spin (footage)
      integer ih, im,is
    
! 1. Make Header which lists the stations.
      WRITE(LUSCN,'(14x," ",64(5x,a,"    "))') 
     >  (cpocod(istn(i)),i=1,nstn)  


! 7. Slew
      WRITE(LUSCN,'("Slewing (min): ",4x,64(1x,f5.1,4x," "))') 
     >  (tslew(istn(i))/60.0, i=1,nstn)
      
! 8. Idle time
      WRITE(LUSCN,'("Idle time (sec): ",64(i8,"   "))')
     >  (isecdif(mjd,ut,mjdt(istn(i)),utt(istn(i))),i=1,nstn)
    
! 9. Durations.
      WRITE(LUSCN,'("Duration (sec): ",4x,64(I5,"      "))') 
     >  (IDURST(istn(i)),i=1,nstn)      

! 10.  Actual start time.
      call seconds2hms(ut,ih,im,is)
      WRITE(LUSCN,'("Obs start time:  ",2(I2.2,":"),I2.2)') IH,IM,IS

! 11.  SNR matrix
      IF (kvscan.and.kxnewsnr) CALL SNRDS(NSTN,ISTN,ICOD)
! 12.   Flux matrix
      IF (kvscan.and.kxnewflux) CALL FLUXDS(NSTN,ISTN,ICOD)
      IF (kvscan.and.kxnewbase) CALL BASEDS(NSTN,ISTN)
! 13. SEFDs adjusted for elevation
      if (kvscan.and.kxnewsefd) CALL SEFDDS(icod,nstn,istn,nsor,mjd,ut)

! 14.  Subnet
      write(luscn,'("Subnet: ",9x,64a)') 
     >  cpocod(istn(1)),("-"//cpocod(istn(i)),i=2,nstn)      
      return
      end
