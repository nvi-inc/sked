      subroutine gtban(icod,nba,iband)
C 
C     Find number of bands and their indices given the frequency code
C     NRV 891127 created
C     NRV 891201 re-created
C
      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE '../skdrincl/freqs.ftni'
      INCLUDE '../skdrincl/statn.ftni'

C Input:
      integer iband(max_band),icod,nba

! function
      integer igtba ! function


C Local:
      integer is,ib(max_band),i,n,j
      integer*2 lb(max_band)
      character*2 cb(max_band)
      equivalence (lb,cb)
      integer iba,nb
C
      nba = 0
      IF (NBAND.LE.0) RETURN
      if (nband.eq.1) then
        iband(1)=1
        nba=1
        return
      endif
C
C  1. Look at the subgroups for the code and find their indices.
C
      n=0
      do i=1,2 ! maximum number of bands in a code is 2
        iba = igtba(cifinp(1,i,icod))
        if (iba.ne.0) then
          if (n.lt.max_band) then
            n=n+1
            ib(n)=iba
          endif
        endif
      enddo
C
C  2. If we got some matches, fill in iband.
C
      if (n.gt.0) then
        nba = n
        do i=1,nba
          iband(i)=ib(i)
        enddo
      endif
C
C  1. Find the unique bands in this code and put them in lb.
C
      nb=0
      do is=1,nstatn ! each station
        do i=1,nchan(is,icod) !each VC
          j=1
          do while (j.le.nb.and.lsubvc(invcx(i,is,icod),is,icod).
     .         ne.lb(j))
            j=j+1
          enddo
          if (j.gt.nb) then !a new one
            if (nb.lt.max_band) then
              nb=nb+1
              lb(nb)=lsubvc(invcx(i,is,icod),is,icod)
            endif
          endif !a new one
        enddo !each VC
      enddo ! each station
C
C  2. Find the index for each band and put in iband.
C
      do i=1,nb
        iband(i) = igtba(cb(i))
      enddo
      nba=nb
C
      RETURN
      END
