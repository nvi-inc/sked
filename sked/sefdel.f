      real*4 function sefdel(iba,nsor,is,mjd,ut)

C  SEFDEL adjusts the zenith SEFD for the source elevation
C
      include '../skdrincl/skparm.ftni'

C Input:
      integer iba,nsor,is,mjd
C     iba - band index
C     nsor - source index
C     is - station index (absolute)
C     mjd - MJD of the observation, set to -1 to compute
C           flux for full baseline length
      real*8 UT ! time of the observation, set to 0.0 if
C                 mjd=-1
C
C Output:
C     sefdel - the adjusted SEFD

C Common:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'

C History
C 920522 NRV Created. Algorithm from D. Shaffer

C Local
      real*4 az,el,ha,dc,x,y
      real*4 xel ! 1/el^y, y=first parameter
      real*4 fac ! adjustment to SEFD including all parameters
      logical kup
      integer i


C  Calculate elevation, SEFD adjustment. If no parameters or if
C  mjd=-1 (VSCAN command) use zenith value.

      if (mjd.gt.0.and.nsefdpar(iba,is).gt.0) then ! get elevation
        call cvpos(nsor,is,mjd,ut,az,el,ha,dc,x,y,x,y,kup)
        if (kup) then !continue with calculation
          xel = 1.d0/(sin(el)**sefdpar(1,iba,is))
          fac = 0.0
          do i=1,nsefdpar(iba,is)-1
            fac = fac + sefdpar(i+1,iba,is)*(xel**(i-1))
          enddo
        else ! use zenith value
          fac = 1.0
        endif
        sefdel = sefdst(iba,is)*fac
      else ! use zenith value
        sefdel = sefdst(iba,is)
      endif ! elevation/zenith

   
C  Store in common for later display

      sefdstel(iba,is) = sefdel

      return
      end
