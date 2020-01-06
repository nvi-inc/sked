      subroutine get_new_fluxes(lfluxcat,iflux_type,flux_default)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
      include 'astro.ftni'

! History
!    2004Jul24  JMGipson. First version.
!    2007Jul02  JMG.  Added flux.ftni (separated from sourc.ftni)
! passed
      character*(*) lfluxcat
      integer iflux_type(max_band,max_sor)
      real flux_default(2)        !default values for S and X.
! local
      integer isrc,ib

! get the fluxes.
      nflux=0                 		!initialize all fluxes to 0.
      call flget(lfluxcat)    		!get fluxes from source catalog
! If a source wasn't in the source catalog, use the default model.
      do isrc=1,Nsourc
        do ib=1,nband
          if(nflux(ib,isrc) .eq. 0) then  !no flux found. put in default weak flux.
            cfltype(ib,isrc)="B"
            nflux(ib,isrc)=1
            flux(1,ib,isrc)=0.0
            flux(2,ib,isrc)=flux_default(ib)
            flux(3,ib,isrc)=13000.0d0
            iflux_type(ib,isrc)=2
           else
            iflux_type(ib,isrc)=1
           endif
        end do
      end do
      return
      end

