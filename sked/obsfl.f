      subroutine obsfl(icode,iba,nsor,is,js,mjd,ut,obsflux)

C  OBSFL computes the observed flux density of a source.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

C Input:
      integer icode,iba,nsor,is,js,mjd
C     icode - frequency code index
C     iba - band index
C     nsor - source index
C     is,js - station indices (absolute)
C     ibl - baseline index from ibnum
C     mjd - MJD of the observation, set to -1 to compute
C           flux for full baseline length
      real*8 UT ! time of the observation, set to 0.0 if
C                 mjd=-1
C
C Output:
      real*4 obsflux ! observed flux

C Common:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'

C Called by: SNRSK, SNRAC

C History
C 910924 NRV Created. Borrowed code from D. Shaffer.
C 910925 NRV Put observed flux into FACTBL for display
C 930225 nrv implicit none
C 930802 nrv Move calculation of cos/sindec out of if test
C 931110 nrv Don't need to calculate ST0, get from common
C 950816 nrv Convert pa to radians before taking sin/cos (!)
! 2007Jul02  JMG added flux.ftni which was separated from sourc.ftni
! 2021-12-06 JMGipson. Changed size-->axis because size is reserved word. 
! 2021-12-07 Calculate baseline here. Previously passed as argument. 

! function
      integer ibnum 

C Local
      real*8 gha,u,v,pbase,bxb,byb,bzb
      real*8 cosgha,singha,sindec,cosdec
      real*4 fluxmax,axis,ratio,pa,cospa,sinpa,arg1,arg2,arg,fl
      real*4  ucospa,usinpa,vcospa,vsinpa
      integer ic1,ic,ifb,ib1
      integer ibl

C 1. Initialize.
      obsflux = 0.0

      ibl=ibnum(is,js) 
C 2. Compute u,v and projected baseline

      bxb = bx(ibl)
      byb = by(ibl)
      bzb = bz(ibl)
      cosdec = dcos(sorp_now(2,nsor))
      sindec = dsin(sorp_now(2,nsor))

      if (mjd.gt.0) then !projected baseline
C       call sidtm(mjd,st0,frac)
        gha = st0cur(is) + ut*frac - sorp_now(1,nsor) 
        cosgha = dcos(gha)
        singha = dsin(gha)
        u = bxb*singha + byb*cosgha                 ! meters
        v = bzb*cosdec + sindec*(-bxb*cosgha + byb*singha)  ! meters
        pbase = dsqrt(u*u + v*v)/1000.d0     !projected baseline in km
        u = u*wavei(iba,is,icode) ! wavelengths
        v = v*wavei(iba,is,icode) ! wavelengths
      else !full baseline
        pbase = dsqrt(bxb*bxb + byb*byb + bzb*bzb)/1000.d0  ! km
        u = dsqrt(bxb*bxb+byb*byb)*wavei(iba,is,icode) ! wavelengths
        v = bzb*cosdec*wavei(iba,is,icode)
      endif !projected/full

C 3. Compute observed flux density

      if (cfltype(iba,nsor).eq.'M') then !model components
        do ic = 1,nflux(iba,nsor) !number of components
          ic1 = 1 + (ic-1)*6 
          fluxmax = flux(ic1,iba,nsor)       ! Jy
          axis = flux(ic1+1,iba,nsor)*flcon2   ! radians
          ratio = flux(ic1+2,iba,nsor)       ! no units
          pa = flux(ic1+3,iba,nsor)*pi/180.d0          ! radians
          cospa = cos(pa)
          sinpa = sin(pa)
          ucospa = u*cospa
          usinpa = u*sinpa
          vcospa = v*cospa
          vsinpa = v*sinpa
          arg1 = (vcospa + usinpa)*(vcospa + usinpa)
          arg2 = (ratio * (ucospa-vsinpa))*(ratio * (ucospa-vsinpa))
          arg = -flcon1 * (arg1+arg2) * axis*axis
          fl = 0
          if (arg.lt.20.) fl = fluxmax*exp(arg)
          if (fl.gt.0.001) obsflux = obsflux + fl
        enddo
      else !baseline steps
        do ifb = 1,nflux(iba,nsor)  !number of steps
          ib1=ifb*2-1
          if (pbase.ge.flux(ib1,iba,nsor).and.
     .        pbase.le.flux(ib1+2,iba,nsor).and.
     .        flux(ib1+1,iba,nsor).gt.0)
     .        obsflux = flux(ib1+1,iba,nsor)
        enddo
      endif !model/baseline
   
      factbl(iba,ibl) = obsflux
      projbase(ibl) = pbase

      return
      end
