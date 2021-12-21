      double precision function sky_cov(isrc)
! This is to try to find a number for sky-coverage.
! Currently returns closest distance of source=isrc from
! sources observed in the time window (=lobc=.true.)

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
! passed
      integer isrc    !source of interest
! functions
      double precision dot8


! History
! 2007Oct05  JMG  First version.

! local
      double precision src_vec0(3), src_vec(3)  !unit vectors in direction of source.
      integer isor    !source for obs iobs
      double precision dtemp   !store dot product
      double precision angle   !angular distance
      integer k,iobs

! Make unit vector in the direction of this source.
      call make_unit_vector(sorp_now(1,isrc),sorp_now(2,isrc),src_vec0)

      sky_cov=twopi

      do k=1,nobs
        iobs=iskrec(k)
        if(kobc(iobs)) then            !use this observation?
          isor=isrc_obs(iskrec(k))
          call make_unit_vector(sorp_now(1,isor),sorp_now(2,isor),
     >      src_vec)
          dtemp=dot8(src_vec0,src_vec)
          if(dtemp .gt. 0.9999999) then
            angle=0.d0
          else
            angle=dacos(dtemp)
          endif
          sky_cov=min(angle,sky_cov)
        endif
      end do
      return
      end
