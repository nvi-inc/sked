      SUBROUTINE STSEL(ccall)
C
C   STSEL allows the user to select stations for scheduling.
C
      include '../skdrincl/skparm.ftni'
C
C  COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'cat_name_version.ftni' 
C
C     CALLING SUBROUTINES: STCMD
C     CALLED SUBROUTINES: STCAT,STINP,READF_ASC,WRERR
C
C  INPUT
      character*1 ccall ! 's' for standard GUI, 'j' for Java, "r" for refresh
                        ! 'm' for MASTER GET
! functions
      integer ibnum

C  LOCAL VARIABLES
      character*1 cdo
      logical knstse ! true when we should read the select file
      integer iband(max_band)
      integer ib
      integer i,j
      integer ierr
      integer ilen
      integer icod
      integer nba
      integer ifreq_err
! new version of stsel.
! History
! 2005Jun02 JMGipson   First version.
! 2006Aug03 JMGipson.  Got rid of call to trans. Not needed
! 2007Nov20 JMGipson.  Modified so that we can be called from "master_cmd"
! 2012Oct10 JMG. Modified to update version, catalog name info. 
! 2013Oct10 JMG. Better (=more noticable) warning message if it does not find the frequency

      call save_station_state
! First read in the catalog info.
      norig = nstatn
     
!  1. Schedule STCAT and have it do the selection.
      cdo="a"         !try to find the current mode.
      if (ccall.eq.'s' .or. ccall .eq. "m") then
        call FRSEL(cdo,ifreq_err)
      endif

      if(ccall .eq. 's') then
        call stcat(ierr,knstse)
        if (ierr.ne.0.or..not.knewst) return
      endif

C  3. We have returned from STCAT.  Open the file it passed back.

! Update info about the catalogs that we have used and their version.  
! Station catalog is NEVER used. 
!      lstation_cat_use=station_cat
!      call get_cat_version(lstation_cat_use,lstation_cat_version,ierr)
      lantenna_cat_use=antenna_cat
      call get_cat_version(lantenna_cat_use,lantenna_cat_version,ierr)
      lequip_cat_use=equip_cat
      call get_cat_version(lequip_cat_use,lequip_cat_version,ierr)
      lposition_cat_use=position_cat
      call get_cat_version(lposition_cat_use,lposition_cat_version,ierr)
      lmask_cat_use=mask_cat
      call get_cat_version(lmask_cat_use,lmask_cat_version,ierr)


      OPEN (lusel,file=CSTFIL,iostat=IERRCM,status='old')
      IF  (IERRCM.NE.0) THEN  !
        INUMCM = 56
        CALL WRERR(INUMCM,IERRCM)
        RETURN
      END IF  !
C
C  3.1 First re-initialize everything that will be read back from
C      the catalogs.
C
      do i=1,max_stn
        cstcod(i)=" "
        cpocod(i)=" "
        nhorz(i)=0
        ncord(i)=0 
        cterid=" "
        maxtap(i)=0
        nrecst(i)=1
C Don't re-set maxpas because we can't figure it out again unless
C we call gnpas. Warn the user to re-select frequencies, then it
C will get calculated correctly. If no changes in stations are
C made, then maxpas will be ok.
C       maxpas(i)=0
        sefdst(1,i)=0
        sefdst(2,i)=0
        do j=1,max_sefdpar
          sefdpar(j,1,i)=0.0
          sefdpar(j,2,i)=0.0
          nsefdpar(1,i)=0
          nsefdpar(2,i)=0
        enddo
        cterna(i)=" "
        coccup(i)=" "
      enddo
      NSTATN = 0
      KNEWST = .TRUE.
C
C   3.2  Loop on records in the selection file
C
      CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)
      DO WHILE (IERR.GE.0.AND.ILEN.GT.0) ! read each station record
        CALL STINP(IBUF,ILEN,LUSCN,IERR)
        IF (IERR.NE.0) THEN  !
C         Don't quit on error, but keep going to detect all errors first.
C         CLOSE(lusel,status='keep')
C         RETURN
        END IF  !
        CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)
      END DO  !read each station record
 
      CLOSE(lusel,status='keep')
 
C  4. Now check that we have gotten complete station information.
C     Errors found by STINP that are a problem will (?) show up here.
 
      call chstn

C  4.1 Replace previous snr's. Now done below.

      icod = 1 !***only 1 for now
      call gtban(icod,nba,iband)

C
C  4.2 Set everything to default, then replace previous values for
C      early start, tape motion, elevation.
C      Initialize the default subnet to the entire new list of stations.
C
      if (nstatn.gt.0) then
! Restore station state for stations that we had previously,
!  set defaults for the ones we didn't.
        call restore_station_state
      endif

C  5. Compute baseline lengths.
 
      do i=1,nstatn-1
        do j=i+1,nstatn
          ib=ibnum(i,j)
          bx(ib) = stnxyz(1,j) - stnxyz(1,i) ! meters
          by(ib) = stnxyz(2,j) - stnxyz(2,i) ! meters
          bz(ib) = stnxyz(3,j) - stnxyz(3,i) ! meters
          baselen(ib)=dsqrt((stnxyz(1,i)-stnxyz(1,j))**2 +
     .    (stnxyz(2,i)-stnxyz(2,j))**2 + 
     .    (stnxyz(3,i)-stnxyz(3,j))**2) / 1000.d0
        enddo
      enddo
 
! Refresh frequency.
!
      if(ccall .eq. "s" .or. ccall  .eq. "m") then
        if(ifreq_err .eq. 0) then ! successfully found frequency
          cdo="r"                  !refresh frequency
          call wrfrs(IERR)
          if(ierr .ne. 0) return
          call FRSEL(cdo,ifreq_err)
        else
          write(luscn,'(a)')
     >    "WARNING!  stsel: Couldn't automatically determine frequency!"
          write(luscn,'(a)')
     >     "         *******You must re-select frequencies!!!*********"
        endif
      endif

C     Re-UNPAK the current observation, if any, to re-establish the
C     source and station indices. They may have changed.
      if (ircur.gt.0) then
        cbuf=cskobs(iskrec(ircur))
        CALL UNPAK(IERR,0)                         
      endif

      krsini = .false.
      RETURN
      END
