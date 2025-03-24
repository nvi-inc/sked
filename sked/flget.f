      SUBROUTINE FLGET(fname)
C
C   FLGET gets fluxes from the flux catalog.
C
      include '../skdrincl/skparm.ftni'
C
C  Input
      character*(*) fname ! optional catalog name
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
      include 'cat_name_version.ftni'
C
C     CALLING SUBROUTINES: FLCMD
C
! functions
      integer trimlen
C  LOCAL VARIABLES
      character*128 ccat
      integer ib,is,j,ierr,ilen
! AEM 20041207 add new templorary variable lucat_a and initialize it
      integer lucat_a
      lucat_a = 55
!      
C  INITIALIZED VARIABLES
C
C  History
C  891114 NRV Created, modeled after SOSEL
C  891201 NRV Added option to read from alternate catalog
C  930224 nrv implicit none
C  940216 nrv Make specified name simply the file, do not prepend a path to it.
! 2007Jul02  JMG  Added flux.ftni
! 2012Oct10 JMG. Modified to update version, catalog name info. 
! 2023-10-18 JMGipson. Simplified reading in catalog. 

C
C
C    1. Set bands to X/S if none selected yet.
C
      if (nsourc.le.0) then
        write(luscn,'(a)' ) 'FLGET - Select sources first.'
        return
      endif
C
      if (ncodes.le.0) then !set default bands
        nband=2
        cband(1)="X "
        cband(2)="S "
        write(luscn,'(a)')
     >   "Default bands X,S being used to select  fluxes."
      endif
C
C   2. Open flux catalog and get fluxes for sources.
C
      if (fname.ne.' ') then ! alternate source,e.g., flux catalog, or from catcmd.
        ccat = fname
      else !standard catalog
        ccat = FLUX_CAT
      endif !alternate/standard

      OPEN(lucat_a,file=ccat,status='old',iostat=IERRCM)     
      IF  (IERRCM.NE.0) THEN
       write(luscn,'("Error ",i5," opening file ",a)') ierrcm,trim(ccat)
       RETURN
      else
        write(luscn,'("Getting fluxes from file ",a)') trim(ccat) 
      END IF  !
C

! Re-initialize flux information since we are going to get it all now.
! JMG  Don't do!  Want to preserve this info, in case we don't pick it up.
      if(.false.) then
      do ib=1,max_band
        do is=1,max_sor
          nflux(ib,is)=0
          cfltype(ib,is)=' '
          do j=1,max_flux
            flux(j,ib,is)=0.0
          enddo
        enddo
      enddo
      endif

! Setting nflux to negative is a flag indicating that we have info on this source.
! if nflux is 0, then this has no effect. Used in flinp.
!
      do ib=1,max_band
        do is=1,max_sor
         nflux(ib,is)=-nflux(ib,is)
        end do
      end do

! Now read in the fluxes.
      cbuf="*"


! read in the file a line at a time.      
      do while(.true.) 
! skip blank lines and comments. if we reach the EOF exit
        cbuf="*"
        do while(cbuf(1:1) .eq. "*" .or. cbuf .eq. " ") 
          read(lucat_a, '(a)',end=100) cbuf 
        end do 
!        write(*,*) trim(cbuf)
        CALL FLINP(cBUF,luscn,ierrcm)
        IF  (IERRCM.NE.0) goto 100 
      END DO  !read each source record

100   continue 
      CLOSE(lucat_a)
      knewfl=.true.

! This restores fluxes we had before for those cases
! where we didn't get new fluxes.
      do ib=1,max_band
        do is=1,max_sor
          nflux(ib,is)= abs(nflux(ib,is))
        end do
      end do
    
      lflux_cat_use=ccat 
      call get_cat_version(lflux_cat_use,lflux_cat_version,ierr)
 
C
      RETURN
      END
