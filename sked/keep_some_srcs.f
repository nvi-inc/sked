      subroutine Keep_Some_Srcs(ikpSrc,NumKeep,lsrc_fil,ierr)
! keep only the best, and discard the rest.
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'
      include 'flux.ftni'

! 2007Jul02  Added flux.ftni which were separated from sourc.ftni

! functions
      integer trimlen
      integer iwhere_in_string_list
      integer renam


! passed
      integer NumKeep                   !number to keep
      integer ikpSrc(NumKeep)           !pointer to the ones to keep.
      character*(*) lsrc_fil            !sked scratch file
      integer ierr

! This routine will keep some sources (determined externally) and discard the rest.
! On entry ikpSrc is index into sources.
!   we copy everything to temporary arrays, and then copy into the top of the arrrys.
! Define the temporary arrays
      character*(max_sorlen) csorna_keep(NumKeep), ciauna_keep(NumKeep)

      double precision sorp50_keep(2,NumKeep),sorpda_keep(2,NumKeep)   !source positions
      integer ISSCAN_keep(NumKeep)                       !scan lengthis
      real Flux_keep(max_flux,max_band,NumKeep)
      integer NFLux_keep(max_band,NumKeep)  !flux info.
      character*1 cfltype_keep(max_band,NumKeep)
      integer ikp,iptr
      real rmin_astro_keep(NumKeep),rmax_astro_keep(NumKeep)

! temporary file.
      character*128 lsrc_filtmp   !temporary file.
      integer lu_srcfil,lu_srcfiltmp
      logical kexist
      character*100 ldum
      integer i,j
      character*(max_sorlen) csrc_name,ccom_name
      logical ktoken,knospace,keol
      integer istart,inext

! Refresh scratch file.
! now see if source file does
      inquire(file=lsrc_fil,exist=kexist)
      if(.not.kexist) then
        write(*,*) "Keep_some_srcs:Source file does not exist! ",
     >   lsrc_fil
        ierr=1
        return
      endif

! copy everything to the temporary best arrays.
      do ikp=1,NumKeep
         iptr=ikpsrc(ikp)
         csorna_keep(ikp)	=csorna(iptr)
         ciauna_keep(ikp)       =ciauna(iptr)
         isscan_keep(ikp)	=isscan(iptr)
         rmin_astro_keep(ikp)   =rmin_astro(iptr)
         rmax_astro_keep(ikp)   =rmax_astro(iptr)

         do j=1,2
           sorp50_keep(j,ikp)	=sorp50(j,iptr)
           sorpda_keep(j,ikp)	=sorpda(j,iptr)
         end do
         do j=1,max_band
           Nflux_keep(j,ikp)  	=Nflux(j,iptr)
           cfltype_keep(j,ikp)	=cfltype(j,iptr)
           do i=1,max_flux
             flux_keep(i,j,ikp)	=flux(i,j,iptr)
           end do
         end do
      enddo
! now put everthing in the top of the old arrays
      Nsourc=NumKeep
      Nceles=NumKeep
      do ikp=1,NumKeep
        csorna(ikp)=csorna_keep(ikp)
        ciauna(ikp)=ciauna_keep(ikp)
        isscan(ikp)=isscan_keep(ikp)
        rmin_astro(ikp)=rmin_astro_keep(ikp)
        rmax_astro(ikp)=rmax_astro_keep(ikp)


        do j=1,2
          sorp50(j,ikp) =sorp50_keep(j,ikp)
          sorpda(j,ikp) =sorpda_keep(j,ikp)
        end do
        do j=1,max_band
          Nflux(j,ikp)	=Nflux_keep(j,ikp)
          cfltype(j,ikp)=cfltype_keep(j,ikp)
          do i=1,max_flux
            flux(i,j,ikp)=flux_keep(i,j,ikp)
          end do
        end do
      end do

      lu_srcfil=199
      lu_srcfiltmp=200
! Now refresh the scratch file.
      lsrc_filtmp=lsrc_fil(1:trimlen(lsrc_fil))//".tmp"
      open(lu_srcfil,file=lsrc_fil)
      open(lu_srcfiltmp,file=lsrc_filtmp)
100   continue
      read(lu_srcfil,'(a100)',end=200) ldum
      if(ldum(1:1) .eq. "*") goto 100    !skip comment lines. (Shoudlnt be any!)

      istart=1
      call extractnexttoken(ldum,istart,inext,csrc_name,
     >   ktoken,knospace,keol)

      istart=inext
      call extractnexttoken(ldum,istart,inext,ccom_name,
     >   ktoken,knospace,keol)

!      read(ldum,*) csrc_name,ccom_name
      if(ccom_name .ne. "$") csrc_name=ccom_name
      if(NumKeep .gt.0) then
        i=iwhere_in_string_list(csorna,NumKeep,csrc_name)  !If already found, don't need to write again
        if(i .eq. 0) goto 100           !skip this line.
      endif
      write(lu_srcfiltmp,'(a)') ldum(1:trimlen(ldum))
      goto 100

200   continue
!  End of loop
      close(lu_srcfiltmp)
      close(lu_srcfil)
      ierr=renam(lsrc_filtmp,lsrc_fil)
      if(ierr .ne. 0) then
         write(*,*) "Error renaming file"
      endif
      knewso=.true.
      return
      end
