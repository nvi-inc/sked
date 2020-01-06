      subroutine Setlvec2lval(klvec ,ilen,kvalue)
      logical klvec (ilen)
      logical kvalue
      integer ilen

      do i=1,ilen
        klvec (i)=kvalue
      end do
      return
      end
!*************************************************************
      subroutine Setivec2iVal(ivec,ilen,iVal)
      integer ilen
      integer ivec(ilen)
      integer iVal

      do i=1,ilen
        ivec(i)=iVal
      end do
      return
      end
!*************************************************************
      subroutine SetdVec2dVal(dVec,ilen,dVal)
      integer ilen
      Double Precision dVec(ilen)
      Double Precision dVal

      do i=1,ilen
        dVec(i)=dVal
      end do
      return
      end
!*****************************************************************
      subroutine CopyIvec2Ivec(OutVec,Invec,ilen)
      integer ilen
      integer Outvec(ilen)
      integer Invec(ilen)

      integer i

      do i=1,ilen
        Outvec(i)=InVec(i)
      end do
      return
      end
!*****************************************************************
      subroutine CopyI2vec2I2vec(OutVec,Invec,ilen)
      integer ilen
      integer*2 Outvec(ilen)
      integer*2 Invec(ilen)

      integer i

      do i=1,ilen
        Outvec(i)=InVec(i)
      end do
      return
      end
!*********************************************************************
      subroutine AdddVal2dVec(dVecOut,dVecIn,ilen,dValIn)

      integer ilen
      Double precision dVecOut(ilen)
      Double precision dVecIn(ilen)
      Double precision dValin

      do i=1,ilen
        dVecOut(i)=dVecIn(i)+dValin
      end do
      return
      end
!**********************************************************************
      subroutine MuldVecbydVal(dVecOut,dVecIn,ilen,dValIn)

      integer ilen
      Double precision dVecOut(ilen)
      Double precision dVecIn(ilen)
      Double precision dValin
      do i=1,ilen
        dVecOut(i)=dVecIn(i)*dValin
      end do
      return
      end


