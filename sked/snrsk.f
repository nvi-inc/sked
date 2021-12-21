      SUBROUTINE SNRSK(IDURSO,NSTN,ISTN,nsor,icod,IERR,lu,mjd,ut)
C
C   SNRSK calculates variable scan lengths for one source.
C         Results are in IDURST.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'flux.ftni'
C
C  INPUT:
C     IDURSO - default source scan length 
C     NSTN - number of stations
      integer ISTN(MAX_STN),nstn,nsor,icod,lu,mjd,idurso
C      - station indices
C     nsor - source index
C     icod - frequency sequence code index
C     lu - non-zero to write error messages on unit lu
C     mjd - date of this observation, set to -1 for full-baseline
      real*8 ut ! time of this observation
C
C  OUTPUT:
      integer ierr   !  ierr - if not enough information, set to the problem station index

! functions
      real*8 snr_per_sec
!
C  Called by: CHCMD, NEWOB, VSCAN, SNROK
C
C  LOCAL VARIABLES
      real*8 anum,anu1,anu2 ! intermediate calculations
      integer iband(max_band) ! bands in this freq. code
      integer idur(max_band,max_stn) ! station durations for each band
   
      logical kmatch
      integer i,j,k,is,js,iba,ibl,nba,kk,id,isc,id2,isc2,idmax
      integer ibnum ! function
   
      integer ibit,npass,ntrks,nhead      
      real*8 bit_eff                        !loss due to 1 or 2 bit sampling
      real*8 corr_eff                       !other losses in correlation process.
      real*8 bit_eff_1bit
      real*8 bit_eff_2bit
      real*8 temp                           !short lived temporary variable
C
C
C   HISTORY:
C     WHO  WHEN   WHAT
C     NRV 890425 CREATED
C         890711 Added check on actual SNR vs minimum required
C         890714 Added 'lu' for above error message
C         891117 Changed to use SEFD by band and source fluxes
C         891205 Added logic to handle 0-flux case
C         900326 Changed MAXSCN to by station
C         910924 Remove flux/baseline loop, add mjd,ut to SNRAC call
C         920522 Add call to SEFDEL
C         940620 Change to use number of recorded tracks instead of just
C                number of frequencies. Accounts for mode A and C and
C                VLBA having fewer channels.
C 950816 nrv Add check for a single station having a scan length that
C            is longer than the rest, and set it to the next highest.
C 960325 nrv If RECTRK=0, sed ID (calculated duration) to 0 instead of
C            trying to divide by 0.
C 960923 nrv ITEARL array
C 990412 nrv Change to MAXSCN as single parameter.
! 2004Jun15 JMG  changed test for maximum allowable duration.
! 2007Jul02 JMG Added flux.ftni (split off from sourc.ftni)
! 2008Jun10 JMG better error messages.
! 2008Jun10 JMG better error messages.
!               Moved checking of trk_flux_sefd into separate routine
! 2008Oct03 JMG. Fixed problem in calculating mxdur.  This would overflow to a negative number.
! 2013Apr25 JMG. Modified for simple broadband calculations.
! 2014Feb27 JMG. Added some comments. 
! 2014Mar24  JMG. Modified calculation of duration to be consistent with SNR calculation in snrac. 

C
C  1. Check that the parameters we need were set.
C
      IERR=0
      call gtban(icod,nba,iband)

! This assumes that all stations use the same efficiency.
      is=istn(1)
      npass=1
      call itras_params(is,icod,npass,ntrks,nhead,ibit)
      bit_eff_1bit=0.637
      bit_eff_2bit=0.881
      if(ibit .eq. 1) then
           bit_eff=bit_eff_1bit
      else
           bit_eff=bit_eff_2bit
      endif 
! The correlator effiency depends on the correlator.
! Bonn switched over in December 2010.  
! Haystack, Westford will switch in July 2013.
! For simplicity use single cutoff date.
      if(iyrcur(is) .le. 2010) then
         corr_eff=0.8995d0    !mark4 correlator
      else
         corr_eff=0.970d0     !diFX correlator
      endif 
C
C  2. Compute durations.
C
      do i=1,max_band
        do j=1,max_baseline
          iactbl(i,j)=-1
        enddo
      enddo
      
      do iba=1,2 
         do i =1, Nstn 
            j=istn(i)         
            sefdstel(iba,j) =sefdel(iba,nsor,j,mjd,ut)
         end do
       end do     
      
      
      do k=1,nband ! each band
        iba=iband(k)
        DO I=1,NSTN-1 ! first station
          IS=ISTN(I)
          DO J=I+1,NSTN ! second station
            JS=ISTN(J)
            ibl=ibnum(is,js)
            IF (kvscan) then !compute scan lengths
               temp=snr_per_sec(icod,iba,nsor,is,js,mjd,ut,
     >                   ibit,corr_eff,bit_eff)             
! Here we calculate the required duration to acheive the SNR. 
                 if(temp .gt. 0) then
                   iactbl(iba,ibl)=
     >               int((dble(isnrbl(iba,ibl))/temp)**2)+1
                 else
                   iactbl(iba,ibl)=0
                 endif                                       
              if (iactbl(iba,ibl).ge.0) then !valid duration
                IF (IACTBL(iba,IBL).LT.1) IACTBL(iba,IBL)=1
C    Add sync time. Early start is not part of duration. 
                IACTBL(iba,IBL)=IACTBL(iba,IBL)+ITSYNC
              endif !valid duration
            ELSE !use default
              IACTBL(iba,ibl)=IDURSO
            ENDIF !compute/default
          ENDDO ! second station
        ENDDO ! first station
      enddo ! each band
C
C  3. Select station duration by finding longest duration 
C      of participating baselines.
C
!      writE(*,*) "Sync", itsync 
      do k=1,nba !each band
        iba=iband(k)
        do kk=1,max_stn
          idur(iba,kk)=-1
        enddo
        DO I=1,NSTN
          IS=ISTN(I)
          DO J=I+1,nstn 
            JS=ISTN(J)           
            ibl=ibnum(is,js)
!            write(*,*) iactbl(iba,ibl) 
            idur(iba,is)=max(idur(iba,is),iactbl(iba,ibl))
            idur(iba,js)=max(idur(iba,js),iactbl(iba,ibl))
!            write(*,*) "IJD ",i,j, idur(iba,is), idur(iba,js)          
          ENDDO
        ENDDO
      enddo !each band
C
C  Now, choose the longest of each band's duration.
C  If either band has -1, then the station cannot observe.
C     
!      write(*,'(a," ",$)') csorna(nsor)
      do i=1,nstn
        is=istn(i)
        idurst(is)=0
        do k=1,nba
          iba=iband(k)
          if (idur(iba,is).eq.-1) then
            idurst(is)=-1         !quick exit. 
            goto 100
          endif
          idurst(is)=max(idurst(is),idur(iba,is))
        enddo        
100     continue
!        write(*,'(i7,$)') idurst(is)
      enddo
!      write(*,*) " <<"
       
C
C  4.  Round times to next highest modular unit.
C
      IF (kvscan) THEN !round
        DO I=1,NSTN
          IS=ISTN(I)
          if (idurst(is).ne.-1) then
            ISC=FLOAT(IDURST(IS))+(FLOAT(MODSCN)-.001)
            IDURST(IS)=(ISC/MODSCN)*MODSCN
            IDURST(IS)=MAX(IDURST(IS),MINSCN)
            IDURST(IS)=MIN(IDURST(IS),MAXSCN)
          endif
        ENDDO
      ENDIF !round

C  5. Final check to make sure one lone station does not
C     have a duration that is not matched with anyone else.

      if (kvscan) then ! check
        idmax=-1
        id2=-1
        isc=-1
        do i=1,nstn ! find max
         is=istn(i)
         if (idurst(is).ne.-1) then
           if (idurst(is).gt.isc) then
             isc=idurst(is)
             idmax=is ! also keep the index
           endif
         endif
        enddo ! find max
        isc2=-1
        do i=1,nstn ! find second max
          is=istn(i)
          if (idurst(is).ne.-1.and.is.ne.idmax) then
            if (idurst(is).gt.isc2) then
              isc2=idurst(is)
              id2=is
            endif
          endif
        enddo ! find second max
        kmatch=.false.
        do i=1,nstn ! check for a lonely max
          is=istn(i)
          if (idurst(is).ne.-1.and.is.ne.idmax) then
            if (idurst(is).eq.isc) kmatch=.true.
          endif
        enddo
        if (.not.kmatch.and.idmax.ne.-1.and.id2.ne.-1) 
     .  idurst(idmax)=idurst(id2)
      endif

C  6. Only one baseline, force same duration.

      if (nstn.eq.2) then !only one baseline
        id=max(idurst(istn(1)),idurst(istn(2)))
        idurst(istn(1))=id
        idurst(istn(2))=id
      endif !only one baseline
C
      call snrac(nstn,istn,nsor,icod,lu,mjd,ut,ierr)
C
      RETURN
      END
