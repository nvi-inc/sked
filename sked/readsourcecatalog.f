      subroutine ReadSourceCatalog(lsrc_cat,lsrc_fil,keep_astro,ierr)
! Replace current sources with all sources in source catalog file, src_cat.
! keep the first NumKeep sources in the current list, add the rest
! of the sources in the source catalog.

! 16September2003 JMGipson  First version
! 2007Jul02  JMGipson added astro.ftni,flux.ftni (split off from sourc.ftni)
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  

      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'
      include 'flux.ftni'

! functions
      integer trimlen
      integer iwhere_in_string_list
      integer renam
! passed
      character*(*) lsrc_cat     !source catalog name
      character*(*) lsrc_fil     !output file
      logical       keep_astro   !keep astrometric sources?
! returned
      integer ierr               !some errror

      character*128 lsrc_filtmp   !temporary file.
! Open up the source catalog, and write out the results
! local
      character*100 ldum
      character*1 cme
      integer lu_cat,lu_srcfil,lu_srcfiltmp
      logical kexist
      character*(max_sorlen) csrc_name

      logical ktoken,knospace,keol
      integer istart,inext

      integer i
      integer iwhere

! used in keeping astrometric sources
      character*(max_sorlen) castro_save(max_sor)
      real rmin_astro_save(max_sor),rmax_astro_save(max_sor)

      integer nflux_astro(max_band,max_sor)                   ! used to store fluxes of astrometric sources
      character*1 cfltype_astro(max_band,max_sor)
      real flux_astro(max_flux,max_band,max_sor) 
   
      integer num_save
! Make the tmpfile name, setup lus.
      lsrc_filtmp=lsrc_fil(1:trimlen(lsrc_fil))//".tmp"
      lu_cat=198
      lu_srcfil=199
      lu_srcfiltmp=200

! check to see if catalog exists.
      ierr=0
      inquire(file=lsrc_cat,exist=kexist)
      if(.not.kexist) then
        write(*,*) "ReadSourceCatalog: Catalog file does not exist!",
     >  lsrc_cat
        ierr=1
        return
      endif
! now see if source file does
      inquire(file=lsrc_fil,exist=kexist)
      if(.not.kexist) then
        write(*,*) "ReadSourceCatalog: Source file does not exist! ",
     >   lsrc_fil
        ierr=1
        return
      endif

! Temporary file to keep the sources.
      open(lu_srcfiltmp,file=lsrc_filtmp)

! if we keep the astrometric sources, extract the ones that are, and write to lsrc_filtmp
      if(keep_astro)   then

! save the astrometric sources
        num_save=0.
        do i=1,nsourc
          if(kastro_src(i)) then 
            num_save=num_save+1
! Save source name and astro targets
            rmin_astro_save(num_save)=rmin_astro(i)
            rmax_astro_save(num_save)=rmax_astro(i)
            castro_Save(num_save)=csorna(i)
! Save flux information.             
            nflux_astro(:,num_save) =nflux(:,i)
            cfltype_astro(:,num_save)=cfltype(:,i)
            flux_astro(:,:,num_save) =flux(:,:,I)
           
          endif
        end do

! read in the source
         open(lu_srcfil,file=lsrc_fil)
50       continue
         read(lu_srcfil,'(a100)',end=60) ldum
         if(ldum(1:1) .eq. "*") goto 50    !skip comment lines.

! look at the next two tokens to see if either one is the astrometric name.
         inext=1
         do i=1,2
           istart=inext
           call extractnexttoken(ldum,istart,inext,csrc_name,
     >        ktoken,knospace,keol)
           if(csrc_name .ne. "$") then
             iwhere=iwhere_in_string_list(Castro_save,num_save,
     >         csrc_name)
             if(iwhere .ne. 0) then
               write(lu_srcfiltmp,'(a)') ldum(1:trimlen(ldum))
               goto 50
             endif
           endif
         end do
         goto 50

60       continue    !come here on EOF
         close(lu_srcfil)
      endif


! Open the source catalog, and write out the rest.
      open(lu_cat,file=lsrc_cat)
      write(luscn,'("Reading sources from ",a)')
     >   lsrc_cat(1:trimlen(lsrc_cat))

!  Here is the loop where we read in the sources from the source catalog.
100   continue
      read(lu_cat,'(a100)',end=200) ldum
      if(ldum(1:1) .eq. "*") goto 100    !skip comment lines.

!      read(ldum,*) csrc_name
      if(keep_astro)  then          !check to see if we already have these sources.
! look at the next two tokens to see if either one is the astrometric name.
         inext=1
         do i=1,2
           istart=inext
           call extractnexttoken(ldum,istart,inext,csrc_name,
     >        ktoken,knospace,keol)
           if(csrc_name .ne. "$") then
             iwhere=iwhere_in_string_list(Castro_save,num_save,
     >         csrc_name)
             if(iwhere .ne. 0) then
               goto 100            !we have written out the astrometric sources previously, don't do again.
             endif
           endif
         end do
      endif


120   continue
      if(ldum .ne. " ") then
        write(lu_srcfiltmp,'(a)') ldum(1:trimlen(ldum))
      endif
      goto 100

200   continue
!  End of loop
      close(lu_srcfiltmp)
      close(lu_cat)
      ierr=renam(lsrc_filtmp,lsrc_fil)

! Read the sources and fluxes into common.
      cme='b'
      ldum=" "
      CALL SOSEL(ldum,cme)       ! read sources into common
      nflux=0
      if(keep_astro) then
         nflux(:,1:num_save)=nflux_astro(:,1:num_save)
         cfltype(:,1:num_save)=cfltype_astro(:,1:num_save)
         flux(:,:,1:num_save)=flux_astro(:,:,1:num_save)
       endif
     

      ldum=" "
      call flget(ldum)
! initilize astrometric limits.
      rmin_astro=0
      rmax_astro=1

! if we kept some, update them.
      kastro_src=.false.
      if(keep_astro) then
        rmin_astro(1:num_save)=rmin_astro_save(1:num_save)
        rmax_astro(1:num_save)=rmax_astro_save(1:num_save)
        kastro_src(1:num_save)=.true. 
      endif

      return
      end

