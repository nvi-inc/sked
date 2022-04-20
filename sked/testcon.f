      subroutine testcon(isrc_num, istn,nstn,iSubNet)

! 2020Oct13 JMG.  Was testing on nobs=1, should be nobs=0. 
CHS General purpose
C  Testcon was created in order to check a subconfiguration and assign a score to it.

C
C 900601 HS  Created
C 911026 NRV Removed dleq call and replaced with inv
C            Add indx4 call to access inverse array
C 930426 nrv Add display of subcons
C 930720 nrv Add calculation of source "up" time variance
C 930930 nrv Remove max,min calculations for minor options, do all
C            of it in NEXTC
C 951116 nrv Add station index to SPEED call
C
C 2002Nov12 JMG  Cleanup.
! 2005MAr15 JMG. indx4 expects int*2 arguments. Made it so.
! 2005May23 JMG. More cleanup.
! 2008Jun16 JMG. Fixed inconsistent calls to store.
! 2008Jun18 JMG. Changed order of argument list.
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'covar.ftni'
      include 'major.ftni'
C
C INPUT:
      integer isrc_num(*)
      integer istn(max_stn,*) 	!stations
      integer nstn(*)
      integer iSubNet         !how many subnets (can be upto 4)
C     NumTrial is used but not changed
C function
      integer*4 indx4

C LOCAL:
      real*8 score
      double precision sky_score
      double precision covar_score
      integer iptr
      integer*2 iopt      !must be integer*2 since an arg to indx4
      double precision diag_change
      integer isub
      logical klist, ksolve    !used in call to simul.      

      integer*2 job                !det or inverse?
      double precision rcond
      double precision delta_t    
      
    
      NumTrial=NumTrial+1
      cbuf=ctrial_scan(iSubNet)   
! 1. Add contribution of this scan to normal equations.
!****NOTE: this must be done first because it computes the array elev(i) which is used by coverage.
      klist= .false.
      ksolve=.false. 
      call simul(iSubNet,ircur,1,klist,ksolve)

! if we have a single observation, then sort by sky coverage.
! Else, by # of obs.
      if(nobs .gt.0) then
        call coverage(isrc_num,istn,nstn,iSubNet,sky_score)
      else
        sky_score=0.d0
        do isub=1,iSubNet
          sky_score=sky_score+nstn(isub)*(nstn(isub)-1)/2
        end do
      endif  

!      koptbysky=.false. 
!      write(*,*) "Koptybysky: ",kOptbySky
! if sky coverage option is turned on, exit now.
      covar_score=0.d0
      if(kOptBySky .or. nobs .eq. 0) goto 100 
            
! Optimizing by covariance.
! Compute contribution of this subnet to normal equations.    
!      write(*,*) "num_est, num_tri_est: ", num_est, num_tri_est 
       
      dnorm_tmp(1:num_tri_est)=dnorm_tri(1:num_tri_est,0)   !This is the normal equations upto now.
      
      write(*,*) dnorm_tmp(1:num_tri_est) 
      do isub=1,iSubnet        
        dnorm_tmp(1:num_tri_est)=dnorm_tmp(1:num_tri_est) + 
     &                           dnorm_tri(1:num_tri_est,iSubNet)  !normal equations so far    
      end do   
   
      job=11                                                     !compute inverse
!     write(*,*) dnorm_inv(1:2)," | ", dnorm_tmp(1:2) 
      call invert_and_con_tri(dnorm_tmp,rcond,num_est,job)
      stop 
          
! In this part the subconfiguration is tested due to an optimization criterion.
! 1. Compute relative (%) decrease of the diagonal elements for optimized parameters.
! 2. Compute sum of relative decrease as score. THis is the "goodness" of this scan.    
      do iopt=1,num_opt
        iptr=indx4(ixref_opt2est(iopt),ixref_opt2est(iopt))     
! Find the change of the diagonal elements that are optimized..
!        diag_change=(dnorm_inv(iptr)-dnorm_tmp(iptr))/dnorm_inv(iptr)
         diag_change = 1.-sqrt(dnorm_tmp(iptr)/dnorm_inv(iptr))
!        write(*,*) ">>>>>>>>>",NumTrial, iopt, diag_change 
!        write(ludsp,"('---- ',a,' ',2f8.4)") csorna(nsortst(1)),
!     >        sqrt(dnorm_inv(iptr))-sqrt(dnorm_tmp(iptr)),
!     >        diag_change*1e3
        covar_score=covar_score+diag_change
      enddo ! i=1,npara
 
100   continue 
      call store(NumTrial,sky_score,covar_score,iSubNet)
! Debugging.
!     if(.true.) then 
      if(kdissub) then
           write(ludsp,'(i4,2(f9.4,1x)," | ", a)')
     >       NumTrial,sky_score,covar_score,  ctrial_scan(1)(1:80)
      endif

      end
