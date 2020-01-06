      SUBROUTINE DPPIN(A,N)
      IMPLICIT NONE
C
C 1.  DPPIN PROGRAM SPECIFICATION
C
C 1.1 Calculate inverse from a Cholesky factorization of a SOLVE
C     format matrix.  This is a double precision version of the
C     LINPACK SPPDI routine with direct calls to the HP Vector
C     Instruction Set and I*4 indexing.  Only the inversion part
C     of the original LINPACK routine is here.
C
C 1.2 REFERENCES:
C
C 2.  DPPIN INTERFACE
C
C 2.1 Parameter File
C
C 2.2 INPUT Variables:
C
      INTEGER*2 N
      REAL*8 A(*)
C
C A - The SOLVE format matrix
C N - Order of the matrix
C
C 2.3 OUTPUT Variables:
C
C A - The inverted matrix
C
C 2.4 COMMON BLOCKS USED
C
C 2.5 SUBROUTINE INTERFACE
C
C	CALLING SUBROUTINES:
C       CALLED SUBROUTINES: dscal,daxpy
C
C 3.  LOCAL VARIABLES
C
      INTEGER*2 J,K
      INTEGER*4 K1,KK,J1,KJ,JJ,iblas1,nblas
      REAL*8 T
      INTEGER*4 I4P0, I4P1
      DATA I4P0, I4P1 / 0, 1 /
C
C J,K - Loop indices
C K1,KK,J1,JJ,KJ - Used for indexing matrix elements
C T - Intermediate value in matrix element calculation
C
C 4.  HISTORY
C   WHO   WHEN   WHAT
C
C 5.  DPPIN PROGRAM STRUCTURE
C
      iblas1=1
      KK=I4P0
      DO K=1,N
        K1=KK+I4P1
        KK=KK+K
        A(KK)=1.0D0/A(KK)
        T=-A(KK)
        nblas=k-1
        call dscal(nblas,t,a(k1),iblas1)
        J1=KK+I4P1
        KJ=KK+K
        DO J=K+1,N
          T=A(KJ)
          A(KJ)=0.0D0
          nblas=k
          call daxpy(nblas,t,a(k1),iblas1,a(j1),iblas1)
          J1=J1+J
          KJ=KJ+J
        ENDDO
      ENDDO
C
      JJ=I4P0
      DO J=1,N
        J1=JJ+I4P1
        JJ=JJ+J
        K1=I4P1
        KJ=J1
        DO K=1,J-1
          T=A(KJ)
          nblas=k
          call daxpy(nblas,t,a(j1),iblas1,a(k1),iblas1)
          K1=K1+K
          KJ=KJ+I4P1
        ENDDO
        T=A(JJ)
        nblas=j
        call dscal(nblas,t,a(j1),iblas1)
      ENDDO
C
      RETURN
      END
