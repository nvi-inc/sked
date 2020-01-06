      subroutine Norm_By_Range(A,ilen,idir)
      implicit none
! return a number between 1 and 0 specifying where A lies.

!      A=(A-Amax)/(Amin-Amax)      i.e., A=1 if at the minimum.
!

! passed.
      integer ilen      !length of vector
      double precision A(ilen)
      integer idir

! local
      integer i
      double precision Amin,Amax

! get range of values.
      Amin=A(1)
      Amax=A(1)
      do i=2,ilen
        Amin=Min(Amin,A(i))
        Amax=Max(Amax,a(i))
      end do

      if(Amin .eq. Amax) then
         A=0
      else if(idir .eq. 1) then
         A=(A-Amin)
         A= A/(Amax-Amin)
      else if(idir .eq. -1) then
         A=(Amax-A)
         A= A/(Amax-Amin)
      else
        write(*,*) "Norm_by_range: Bad value for idir: ",idir
      endif

      return
      end



