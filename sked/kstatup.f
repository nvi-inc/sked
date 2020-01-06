      logical function kstatup(istat,mjd_scan,ut_scan,idur)
! on exit, kstatup=true only if the station is up for this entire period.
! 

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'downtime.ftni'
! History
!  2010Jan26 JMGipson. Previously did not have idur argument.
!                      
! Passed
      integer istat        !station
      integer mjd_scan     !start of epoch
      real*8  ut_scan       ! seconds part of this
      integer idur         !duration of epoch (seconds)

! function 
      integer isecdif

! See if there is any overlap between downtime and Scan
!        |-----Downtime-----|
!                        |----Scan----|
! if there is any overlap, then say the station is down.

! Possibilities are:
! 1. Beginning of scan occurs in down time. 
!        |-----Downtime-----|
!                        |----Scan----|
!    ->    Tscan_Beg <Tdown_end  &          
!          Tscan_Beg >Tdown_beg
!
! 2. End of scan occurs in downtime
!        |-----Downtime-----|
!   |----Scan----|
!
!    ->    Tscan_end <Tdown_end  & 
!          Tscan_end >Tdown_beg

! 3. Downtime occurs in middle of scan:
!        |-----Downtime-----|
!      |------Scan----------------|
!  
!    ->   Tscan_beg < Tdown_beg
!       & Tscan_end > Tdown_beg (or Tscan_end > Tdown_end)
! Note that if one end of the downtime is outside of the scan, then test 1 or 2 will catch.

! local
      integer isb_db                 !Tscan_beg-Tdown_beg
      integer isb_de                 !Tscan_beg-Tdown_end
      integer ise_db                 !Tscan_end-Tdown_beg
      integer ise_de                 !Tscan_end-Tdown_end

      integer i

      kstatup=.true.
      do i=1,Num_Down
        if(istat .eq. idown_Stat(i)) then
          isb_db=isecdif(mjd_scan,ut_scan,
     >                        mjd_down_beg(i),ut_down_beg(i))
          isb_de=isecdif(mjd_scan,ut_scan,
     >                        mjd_down_end(i),ut_down_end(i))
          ise_db=isb_db+idur
          ise_de=isb_de+idur 
          if((isb_db .ge. 0 .and. isb_de  .le. 0) .or.
     >       (ise_db .ge. 0 .and. ise_de  .le. 0) .or.
     >       (isb_db .le. 0 .and. ise_de  .ge. 0)) then
              kstatup=.false.
              return 
            endif
        endif
      end do
      return
      end
