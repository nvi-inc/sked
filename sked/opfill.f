      SUBROUTINE OPFILL
! History, now most recent top
! 2015Nov17 JMGipson. Rewritten to initialize dnorm_tri to small diagonal matrix. Comments also re-written.
! 


CHS------------------------------------------------------------------
CHS Opfill was created in order to determine the est_scale factor, the 
CHS name and the dimension of any selected parameter to be estimated
CHS or optimized.
CHS
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C 910907 NRV Created from original PARAM
C 910911 NRV Removed dleq, added inv and indx4
C 921005 NRV Add final parameter to SIMUL call
C 921104 NRV Changed units of lat-lon to "m" from "as"
C 930225 nrv implicit none
C 930324 nrv Add check for maximum number of parameters
C 950629 nrv Put back the count of num_est.
! 
C
C COMMON
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'covar.ftni'

! Functions
      integer indx4 

C LOCAL
      integer i

     

CHS------------------------------------------------------------------
CHS Initialization
! Update the normal equations.  First set them to 0.
      dnorm_tri=0      !Reset normal matrix.
      kopgo=.true.
! Set the diagonal to a very small number.  This allows us to invert matrix. 
      do i=1,num_est
        dnorm_tri(indx4(i,i),0)=1.e-6
      end do 

      DO I=1,NOBS
         cbuf=cskobs(iskrec(i))
         CALL SIMUL(0,ISKREC(I),1,.false.,.false.)
      END DO 

C
      if (kcovar) then 
         write(luscn,'("Number of parameters to estimate",i5)')  num_est
         write(luscn,'("Number of parameters to optimize",i5)')  num_opt

        if (num_opt.gt.0.and.nstatn.gt.10)   write(luscn,'(a)') 
     & ' WARNING: Optimization can not handle more than 10 stations.'

         if (num_opt.gt.max_par_opti.or.num_est.gt.max_par_esti) then
           write(luscn,
     &    '("WARNING: Maximum ",i5, "Parameters can be optimized")')
     &         max_par_opti
           write(luscn,
     &    '("         Maximum ",i5, "Parameters can be estimated")') 
     &         max_par_esti 
         endif 
      ELSE
        write(luscn,'(a)') ' Optimizing for sky coverage, '//
     &               'covariance parameters will be ignored.'
      endif

    
      RETURN
      END
