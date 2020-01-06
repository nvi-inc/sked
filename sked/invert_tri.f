C***************************************************************************************
      subroutine invert_tri(a,ndim)
C invert square matrix given in triangular form.
      DOUBLE PRECISION a(*)
      INTEGER*2 ndim
      INTEGER*2 info

      call dppfa(a,ndim,info)
      call dppin(a,ndim)
      return
      end

