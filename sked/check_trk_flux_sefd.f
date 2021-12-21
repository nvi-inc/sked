      subroutine check_trk_flux_sefd(istn,nstn,nsor,icod,lu,ierr)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
! History
! 2008Jun10  JMG. First version. Taken from snrac

! input
      integer ISTN(MAX_STN) ,nstn,nsor,icod,lu
      integer ierr

! local
      integer i
      integer is
      integer ib
      integer nba
      integer iband(max_band) ! bands in this freq. code

      call gtban(icod,nba,iband)

      ierr=0
! check to see that tracks have been set, and that we have flux and SEFDs
      do i=1,nba !check for tracks
        ib=iband(i)
        do is=1,nstn
          if(trkn(ib,istn(is),icod) .le. 0) then
            if (.not.kauto .and. lu .gt. 0) then
              WRITE(LU,
     > "('check_trk_flux_sefd: Track assignments not set up for band ',
     >             a2,' at ',a8)") cband(ib), cstnna(istn(is))
            endif
            IERR=-1
            RETURN
          else if(sefdst(ib,istn(is)).le.0) then
            if (.not.kauto.and. lu .gt. 0) then
               WRITE(LU,
     > "('check_trk_flux_sefd: SEFDs not present for band ',a2,
     >        ' at ',a8)")   cband(ib),cstnna(istn(is))
            endif
            IERR=-1
            RETURN
          else if(nflux(ib,nsor).le.0) then
            if (.not.kauto .and.  lu .gt. 0) then
             write(lu,
     > "('check_trk_flux_sefd: No flux information for ',
     >        a,'-band for source ',a)")   cband(ib),csorna(nsor)
            endif
            ierr=-1
            return
          endif
        end do
      enddo !check for tracks
      return
      end


