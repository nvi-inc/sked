      FUNCTION INDX4(I,J)

      IMPLICIT NONE

      integer*4 indx4
C
C INDX4
C
C Calculate the array element occupied by the matrix element I,J
C where I is the row and J is the column.  The matrix is stored
C in row major, lower triangular form: A(1,1),A(2,1),A(2,2)...
C ..A(N,1),A(N,N)
C
C Input:
      INTEGER*2 I,J !  Row and column of element to be located
C         - see calling routine DPPINC [ZMM]
C
C Output:
C
C     INTEGER INDX4 ! Array index of the specified matrix element
C
C  040506  ZMM  IMPLICIT NONE
C               removed trailing RETURN

      IF(J.GT.I) THEN
        INDX4=I+(J-1)*J/2
      ELSE
        INDX4=J+(I-1)*I/2
      ENDIF

      END
