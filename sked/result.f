      subroutine result(linstq)
CHS----------------------------------------------------------------
CHS General purpose
CHS Result was created in order to display the correlation matrix,
CHS the formal errors of the solve-for parameters and the sky coverage
CHS evaluation number covs.
C
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
      include 'major.ftni'
      include 'covar.ftni'
! functions
      integer istringminmatch
      integer*4 indx4
      integer i2long,trimlen,ichmv
C
C Input:
      integer*2 linstq(*) ! command line

C LOCAL:
      double precision sig(max_dim_esti)
      integer*2 job                            !flag to indicate what we do 10=compute rcond
      double precision rcond                     !rconderminate


      integer nch,ierr,ich,ifc,iec,idummy
      integer*2 i,j
      integer*2 lkeywd(12)
      character*2 ctype

      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=3)
      character*12 list(ilist_len),listshort(ilist_len)
      data list/ "CORRELATION","COVARIANCE","FE"/
      data listshort/"CR","CV","FE"/


C 911026 NRV Add indx4 to index into qimne and qmy
C            Remove call to simul, already done with OP GO command
C            Removed extra blank lines.
C 921104 NRV Added approx. conversion from as to m for lat,lon
C 930421 nrv Add header with file name
C 951017 nrv Fixed gtfld call to remove linstq
! 2005Mar15 JMGipson. Indx4 expects to have integer*2 variables.
!           Made i,j integer*2
! 2005May24 JMGipson.  Modified to use new correlation.
! 2009Jul08 JMGipson. Fixed bug 

C        1. Pick the type of result from the command string.
C
      ierr=0 
      ICH=1
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IFC,IEC)
      if (ifc.ne.0) then !type of result specified 
        nch=iec-ifc+1
        ckeywd=" "
        idummy = ichmv(lkeywd,1,linstq(2),ifc,nch)
        idummy = istringMinMatch(list,ilist_len,ckeywd)
        if(idummy .eq. 0) then
          write(luscn,'(a)') "RESULT01 - Invalid key word.  Must be"//
     >    " one of COVERAGE, COVARIANCE, CORRELATION, or FE"
          ierr=1
          return 
        endif !error
        ctype=listshort(idummy)
      else
        ctype = 'FE'
      endif !type of display specified
C

      nch = trimlen(cskfil)
      WRITE(LUDSP,9101) CSKFIL(1:nch),cEXPER
9101  FORMAT(/3X,'SKED results from file ',A,' for experiment ',A8)

       
! If the normal matrix hasn't been built yet, do so.
      if(dnorm_tri(1,0) .eq. 0) then    
        write(luscn,*) "RESULT:  Making normal matrix."
        call opfill      
      endif
! Display sky coverage, computed in COVER.
      if(nobs .ne. 0) then 
        call compute_coverage()
       endif 

      if (kOptBySky) then
        write(ludsp,
     >'(//"WARNING: Sky coverage only optimization is specified!!"//)')
      endif

      if(num_est.eq.0) then
         write(ludsp,'("Result: No parameters to estimate!")')
         return
      endif

    
      dnorm_inv(1:num_tri_est)=dnorm_tri(1:num_tri_est,0)  !normal equations so far
! Write out the normal equations.
      if (ctype.eq.'CV') then !covariance matrix
        write(ludsp,'(/"The matrix of normal equations:")')
        write(ludsp,20) (cparname(j),j=1,num_est)
        do i=1,num_est
          write(ludsp,21)cparname(i),
     >	  (dnorm_tri(indx4(i,j),0),j=1,num_est)
        end do
  20    format(14x,50(a12,1x))
  21    format(a,1x,50(g12.4,1x))
      endif

! Invert the normal matrix.
      job=11           !compute the inverse AND the rcond
!      call invert_or_det_tri(dnorm_inv,det,num_est,job)
      call invert_and_con_tri(dnorm_inv,rcond,num_est,job)
      write(ludsp,'("Condition number:",g10.4)') rcond

!Check for singularity. If singular, can't go any further.
      if(abs(rcond) .lt. 1.d-12) then
         write(ludsp,'("RESULT: Condition number too small!")')
         return
      endif

      if(ctype .eq. 'CV') then
        write(ludsp,'(/"The variance/covariance matrix:")')
        write(ludsp,20) (cparname(j),j=1,num_est)
        do i=1,num_est
          write(ludsp,21)cparname(i),
     > 	  (dnorm_inv(indx4(i,j)),j=1,num_est)
        end do
        return
      endif !covariance

! Find the formal errors.
      do i=1,num_est
        sig(i)=dnorm_inv(indx4(i,i))
        if(sig(i) .lt. 0) then
           write(ludsp, '("RESULT03: diag element of inverse < 0 !!")')  !Should never happen.
           return
        endif
        sig(i)=sqrt(sig(i))
      end do

      if (ctype.eq.'CR') then !correlation
        write(ludsp,'(/"The correlation matrix:")')
        do i=1,num_est
          write(ludsp,21) cparname(i),
     >      (dnorm_inv(indx4(i,j))/(sig(i)*sig(j)),j=1,num_est)
        end do
      else if(ctype.eq.'FE') then !formal errors
        write(ludsp,
     >   '(/"Standard deviations of the unknown parameters:")')
         do i=1,num_est
           write(ludsp,40) cparname(i),sig(i),cpardim(i)
         enddo
  40    format(6x,a13,5x,g10.4,1x,a)
      endif !formal errors
      return
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      end
