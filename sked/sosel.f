      SUBROUTINE SOSEL(fname,ccall)
C
C   SOSEL allows the user to select sources for scheduling.
C
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'astro.ftni'
      include 'flux.ftni'
! functions
      integer iwhere_in_string_list
C
C INPUT:
      character*128 fname ! alternate source catalog name
      character*1 ccall ! who called us "J" for Java GUI, "S" for screen, r for reset.

C     CALLING SUBROUTINES: SOCMD or CATCMD
C     CALLED SUBROUTINES: SOCAT,SOINP
C
C  LOCAL VARIABLES
! Sources before call.
      integer nsourc_old                       		!number of sources previously
      character*(max_sorlen) csorna_old(Max_sor)        !names
      integer ISSCAN_old(Max_SOR)                       !scan lengthis
      real Flux_old(max_flux,max_band,Max_Sor)
      real rmin_astro_old(max_sor)                      !minimum %obs in astrometric mode
      real rmax_astro_old(Max_sor)
      integer NFLux_old(max_band,max_sor)  !flux info.
      character*1 cfltype_old(max_band,max_sor)

C     nsorc_old - number of original sources before selection
      integer i,j,nsor,nsat,ierr,ilen
      logical knsose ! true if we have a new select file to read
      double precision TJD

C
C INITIALIZED VARIABLES
C
C   WHEN  WHO  CHANGES
C   830423 NRV ADDED 5TH PARAMETER TO IRP CALL FOR DISK LU
C   830524 WEH INITIALIZE NCELES AND NSATEL, CHANGE PRECESSION LOOP
C              LIMIT FROM NSOURC TO NCELES, ADD LOOP TO MAKE SATELLITE
C              NAMES CONTIGUOUS WITH CELESTIAL SOURCE NAMES
C   840915 MWH SUPPORT J2000 COORDINATES
C   880314 NRV DE-COMPC'D
C   880418 NRV CHANGED EXEC(23) TO FMPRUNPROGRAM
C   891110 NRV Removed program SOCAT and replaced with subroutine
C   891227 NRV Changed MOVE to APSTAR
C   900130 NRV Changed call to SOINP to add LUSCN
C   910924 NRV Changed flux variables
C   930715 nrv Initialize rise/set flag
C   940127 nrv Add fname to calling sequence here and socat call
C 970307 nrv Change 4 to max_sorlen/2 and 8 to max_sorlen
C 991116 nrv Remove FNAME from call. Remove call to SOCAT. This
C            routine is now called by CATCMD.
C 000106 nrv Add CCALL to the call to identify caller, either
C            SOSEL or CATCMD.
C 2003Sep09  JMG  Changed lsorn and lnam to their ASCII counterparts.
C 2003DEc11 JMG further clean up.
! 2007Jul02  Added flux.ftni, astro.ftni which were separated from sourc.ftni
! 2007Jul03  Changed call soinp to ASCII
C
C     1. First save current scan lengths and fluxes.
C
      nsourc_old = nsourc
      csorna_old=csorna
      isscan_old=isscan
      flux_old=flux
      nflux_old=nflux
      cfltype_old=cfltype
      rmin_astro_old=rmin_astro
      rmax_astro_old=rmax_astro

!      DO  I=1,nsourc_old !save current scan lengths
!        csorna_old(i) = csorna(i)
!        isscan_old(I) = ISSCAN(I)
!        do j=1,max_flux
!          do k=1,max_band
!            FLUX_old(j,k,I) = FLUX(j,k,I)
!            NFlfux_old(k,I) = NFLux(k,I)
!            cfltype_old(k,I) = cfltype(k,I)
!          enddo
!        enddo
!      END DO  !save current scan lengths

C
C     2. Call SOCAT for selection.
C NEW: this function may have been done by CATCMD and a call to WRSOS
C
      if (ccall.eq.'s') then
        call socat(ierr,knsose,fname)
        if (ierr.ne.0.or..not.knsose) return
      endif



C     3. Read the file written by SOCAT.
C
      OPEN(lusel,file=CSOFIL,status='old',iostat=IERRCM)
      IF  (IERRCM.LT.0) THEN
        INUMCM = 56
        CALL WRERR(INUMCM,IERRCM)
        RETURN
      END IF  !
C
      CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)
      NSOURC = 0
      NCELES = 0
      NSATEL = 0
      DO WHILE (IERR.GE.0.AND.ILEN.GT.0) !read each source record
        CALL SOINP(cbuf,luscn,IERRCM)
        IF  (IERRCM.NE.0) THEN
          CALL WRERR(INUMCM,IERRCM)
          CLOSE(lusel)
          RETURN
        END IF  !
        CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)
      END DO  !read each source record
C
      CLOSE(lusel)
      KNEWSO = .TRUE.
C
C   5. Check to see if any of the new sources are in the old list.
!      If so, use their scan lengths and flux info.
!
! Initialize new fluxes to "NOT found"--everything set to 0.
      Flux=0.0
      Nflux=0
      cfltype= " "

      IF  (NSOURC.GT.0) THEN
        rmin_astro=0.
        rmax_astro=1.
! check if any of the new sources are in the old list--if so use the old values for flux and duration.
        do i=1,Nsourc
           j=iwhere_in_string_list(csorna_old,nsourc_old,csorna(i))
           if(j .ne. 0) then
             isscan(i)    =isscan_old(j)
             cfltype(:,i) =cfltype_old(:,j)
             nflux(:,i)   =nflux_old(:,j)
             flux(:,:,i)  =flux_old(:,:,j)
             rmin_astro(i)=rmin_astro_old(j)
             rmax_astro(i)=rmax_astro_old(j)
           endif
        end do
      END IF
C
C     6.  Precess the celestial sources.
C
      IF  (NCELES.GT.0) THEN
        tjd  =mjdcur(1)+2440000.d0
        DO  I=1,NCELES
          call apstar_Rad(tjd,sorp50(1,i),sorp50(2,i),
     >         sorpda(1,i),sorpda(2,i))
        END DO  !
      END IF  !
C
C    Move the satellite source names so that they are contiguous with
C      the celestial soure names. This is pretty inelegant and will
C      cause even greater ugliness if there is ever a way to add a
C      celestial source without going through SOCAT, since the satellite
C      names will have to be moved to make room. However this technique
C      causes the least disturbance to other parts of SKED. Note in
C      particular that the rest of the code assumes that names are
C      contigous at the start of the array, and that satellites follow
C      celestial sources. See SKOPN where this code is also used.
C
      IF  (NSATEL.GT.0.AND.NCELES.LT.MAX_CEL) THEN  !
        DO  I=1,NSATEL
          NSOR=NCELES+I
          NSAT=MAX_CEL+I
          csorna(nsor)=csorna(nsat)
        END DO  !
      END IF  !
C
C     Re-UNPAK the current observation, if any, to re-establish the
C     source and station indices. They may have changed.
      if (ircur.gt.0) then
        cbuf=cskobs(iskrec(ircur))
        CALL UNPAK(IERR,0)
      endif

      krsini = .false.
      RETURN
      END
