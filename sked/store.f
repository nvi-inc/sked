      subroutine store(itrial,sky_score,covar_score,nsubc)
! Store the trial scans.
! Hisotry
!   2005May24 JMGipson.  Completely rewritten.
!   2022-04-20 JMGipson Fixed a bug. If we ran out space previously just inserted a new scan in the first place where we had a lower score. 
!                       Now put it in the place that had the LOWEST score.  
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'covar.ftni'
      include 'major.ftni'
! passed
      integer itrial                     !number of sub-configurations checked.
      integer nsubc                      !number of observed sources (sub-scans)
      real*8  sky_score                  !optimization criterion
      real*8  covar_score                !sky coverage criterion
! local
! completely rewritten by JGipson
      integer imin_score 
      integer itest

      if(itrial .le. max_trial) then
! Easy case--just store it in next slot.
         imin_score=itrial
      else
! No room. Find scan with the lowest score. 
        imin_score=1
        if(kOptbySky) then
          do itest=1,max_trial
            if(sky_trial_vec(itest) .lt. sky_trial_vec(imin_score)) 
     &         imin_score = itest 
          end do 
          if(sky_score .lt. sky_trial_vec(imin_score)) return           
        else 
          do itest=1,max_trial
            if(covar_trial_vec(itest) .lt. covar_trial_vec(imin_score)) 
     &       imin_score = itest 
          end do
          if(covar_score .lt. covar_trial_vec(imin_score)) return
        endif       
      endif
! At this point have found the previous trial scan with the LOWEST score and our score is higher.  Replace the previous scan.
      nsub_trial_vec(imin_score)=nsubc
      ctrial_vec(1:MaxSubNet,imin_score)=ctrial_scan(1:MaxSubNet)
      sky_trial_vec(imin_score)  =sky_score
      covar_trial_vec(imin_score)=covar_score 

      return
      end
