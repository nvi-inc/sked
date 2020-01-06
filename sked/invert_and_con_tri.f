!***************************************************************************************
      subroutine invert_and_con_tri(a,rcond,ndim,job)
! invert square matrix given in triangular form.
      implicit none 
      DOUBLE PRECISION a(*)
      INTEGER*2 ndim
      integer*2 info
      integer*2 job           !10=cond# only, 1=inverse, 11=both
      double precision rcond
! Function
      integer indx4

   ! local
      double precision z(ndim)
      double precision rescale(ndim)
      real*8 det(2)
      integer i, j

!function
      rcond=0.d0 
 
      call dppco(a,ndim,rcond,z,info)
      if(rcond .eq. 0) return
      if(job .eq. 1 .or. job .eq. 11) then    !compute inverse
        call dppdi(a,ndim,det,job)
      endif
 
      return
      end

