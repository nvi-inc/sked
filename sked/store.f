      subroutine store(itrial,sky_score,covar_score,nsubc)
! Store the trial scans.
! Hisotry
!   2005May24 JMGipson.  Completely rewritten.

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
      integer inext


      if(itrial .le. max_trial) then
! Easy case--just store it in next slot.
         inext=itrial
      else
! no room. See if something else has a worse score.
        do inext=1,max_trial
          if(kOptBySky) then
             if(sky_trial_vec(inext) .lt. sky_score) goto 100
          else 
             if(covar_trial_vec(inext) .lt. covar_score) goto 100
          endif        
        end do
        return
      endif
100   continue
! found a place in the appropriate vector where the score was less. Stuff the results thre. 
      nsub_trial_vec(inext)=nsubc
      ctrial_vec(1:MaxSubNet,inext)=ctrial_scan(1:MaxSubNet)
      sky_trial_vec(inext)  =sky_score
      covar_trial_vec(inext)=covar_score 

      return
      end
