C@IBNUM
      integer function ibnum(i,j)
C
C     IBNUM returns the baseline index given station indices
C
C  INPUT
C     i,j - station indices
C
C  OUTPUT
C     ibnum - baseline index into a one-dimensional array
C             that is n*(n-1)/2 long, where n=# stations
C
C 
      if (j.gt.i) then
        ib=((j-1)*(j-2))/2 + i
      else 
        ib=((i-1)*(i-2))/2 + j
      endif
C
      ibnum=ib
      return
      end

