      subroutine remove_bad_snr(isrc,icod)

      include "../skdrincl/skparm.ftni"
      include "skcom.ftni"
      include "../skdrincl/statn.ftni"
      include "major.ftni"
      include 'astro.ftni'
! passed
      integer isrc
      integer icod 

! Remove the stations from a scan that don't meet their SNR targets.
! This is called by chcmd.f
! History 
! 2010Mar26 JMGipson.  First version.
! 2014Mar26 JMGipson.  Modified to include nsor,icod.
! 2014Mar27 JMGipson. Modified to include NumGoodSNR

! Functions
      integer ibnum

! local
      logical kastro_source                  !is it an astrometric source?
      integer imarg_use       !margin to use. 
  
      integer i,j
      integer ikey(max_stn)   ! Key for sorting
   
      logical kOK             !no bad SNRS?
      integer lu              !logical unit. Don't use bu snr_find_numgood_numbad  needs. 
      integer itarget         !



! Now check the SNRS Remove stations that have bad SNRs, starting with worst.    
       kok=.false. 
       do while(nstncur .ge. 2 .and. .not.  kok)
         kok=.true. 
         lu=-1
        call snr_find_numgood_numbad(icod,isrc,istcur,nstncur,lu)             

        call indexxint(nstncur,numbadsnr,ikey)  !sort NumBad, return results in ikey
! Point to station that has the most BAD snrs.
        j=ikey(nstncur)
              
        if(kAllblGood) then
          itarget=0             !A single bad baseline marks a station as bad
        else
          itarget=NumGoodSNR(j)   !want to have at least as many GOOD snrs as bad.
        endif

        if(numbadsnr(j) .ge.itarget) then
          writE(luscn,
     >      '(" Removing station ", a, " because of low SNR: ")') 
     >      cstnna(istcur(j))      
          istcur(j)=-iabs(istcur(j))
          kok=.false.
        endif
        do j=1,nstncur
          if(numGoodSNR(j) .eq. 0) then 
             istcur(j)=-iabs(istcur(j))
             kok=.false.
          endif
        end do 

        if(.not. kok) call destn(nstncur,istcur)
        
      end do
      return
      end 
