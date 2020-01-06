C***************************************************************************************
      subroutine invert_or_det_tri(a,det_out,ndim,job)
C invert square matrix given in triangular form.
      DOUBLE PRECISION a(*)
      double precision det_out
      INTEGER*2 ndim
      integer*2 info
      integer*2 job           !10=det, 1=inverse, 11=both
      double precision det(2)

      call dppfa(a,ndim,info)
      call dppdi(a,ndim,det,job)

      det_out=det(1)*10.**det(2)
      return
      end

