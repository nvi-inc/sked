      SUBROUTINE SETBA
C
C   SETBA looks through information in freqs.ftni and figures out
C   which frequency bands are in use.
C   It also counts the number of frequencies in each subgroup.
C   It also computes the maximum scan length.
C   It also computes the constant for baseline to wavelength conversion
C   It also counts the recorded tracks.
C   It also fills in the sample rate if none was specified.

      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE '../skdrincl/constants.ftni'
      INCLUDE 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      INCLUDE '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'flux.ftni'

! functions
      integer iwhere_in_string_list

C
C     CALLING SUBROUTINES: FRSEL,SKOPN
C     CALLED SUBROUTINES: 
C
C  LOCAL VARIABLES
      character*2 cbnd(2)
      integer     nbnd     !ditto
      integer ic,i,iv,iwhere,nx,nfr,if1,is,j,ifr,isub
      integer ixband,isband
      real*8 reffreq,frim,fovref
      real*8 sum_del_f,sum_del_f2,sum_del_fovf,del_f
      real*8 frsum,frsqsum,s
      real*8 frq
      integer nfrq_use

C
C  INITIALIZED VARIABLES
C
C  DATE  WHO CHANGE
C 891116 NRV Created
C 891130 NRV Fixed
C 900326 NRV Added MAXSCN calculation
C 910924 NRV Added WAVEI calculation
C 940620 nrv Add count of recorded tracks
C 940705 nrv NOTO is a special case, only 8 channels. There is no
C            way in the $CODES section to show this. Hard code to
C            use 4 tracks at S and X.
C 950306 nrv Do calculation for effective frequency and correction
C            factor for ionosphere here. Used in simul.
C 950406 nrv Modify loop to calculate rms bw and iono correction
C            to account for single-frequency cases and non-continguous
C            frequencies.
C 950515 nrv Remove special handling of NOTO. 
C 950710 nrv Check itras for -99, not 0
C 951019 nrv Modify for new frequency variables
C 951116 nrv Add station index to SPEED call, change indices for per station
C 951213 nrv Add one more dimension to itras for sign/mag bit
C 960403 nrv Change to 0.978 for magnitude bit track
C 960405 nrv Remove hard-coded 14 in loop to calculate rms spanned bw
C 960409 nrv Add check of ITRA2
C 970219 nrv Remove ITRA2 and put in head index. Loop over subpasses.
C 990412 nrv Remove maxscn calculation since it's a parmeter.
C 000126 nrv Set NTRKN as the total number of tracks. Same as TRKN 
C            except not reduced for switching or magnitude.
! 2004Sep20 JMG.  Removed counting of tracks to a subroutine.
! 2006Jun21 JMG.  Modified to only use freqs where freqrf>0.
!                 if freqrf<0, flag that freq is not used.
! 2007Jul02 JMG.  Added flux.ftni (split off from sourc.ftni)

C
C
C  1. Count number of frequencies and the number of tracks being
C     recorded at each station on each frequency.
C

      call count_freq_tracks(cbnd,nbnd,luscn)

! Moved to a subroutine, since also used by drudg.
C  2. Fill in band array and number of bands.
C

! No longer needed, since cbnd and nbnd are returned.

C
C  3. Compare old (LBAND) and new (LB) list of bands.  Reset flux data to
C     invalid if not the same.
C
      if (nband.gt.0) then !compare to old
        do i=1,nband
          iwhere=iwhere_in_string_list(cbnd,2,cband(i))
          if(iwhere .eq. 0) goto 10
        end do
10      continue
        if(nband .ne. nbnd .or. iwhere .eq. 0) then
          write(*,*) "SETBA - Difference between old and new bands."
          write(*,*) "Resetting flux info."
          write(*,*) "Execute Flux select to get info."
          nflux=0
          flux=0.
        endif
      endif !compare to old
C       
C  3. Finally fill in new common variables.
C     And make sure SEFD information is stored correctly.
C     When "T" lines were read, SEFD and band were stored until
C     frequency bands were known.  Now, if LBAND does not match
C     LBSEFD, assume the X and S are reversed and swap them.
C     ASSUMES: only 2 bands allowed!
C
      cband(1:2) =cbnd(1:2)
      nband=nbnd

      if (nband.gt.1) then ! check SEFDs
        do is=1,nstatn
          if (cbsefd(1,is).ne.cband(1)) then !swap
            s = sefdst(1,is)
            sefdst(1,is) = sefdst(2,is)
            sefdst(2,is) = s
            do j=1,max_sefdpar
              s = sefdpar(j,1,is)
              sefdpar(j,1,is) = sefdpar(j,2,is)
              sefdpar(j,2,is) = s
            enddo
            i = nsefdpar(1,is)
            nsefdpar(1,is) = nsefdpar(2,is)
            nsefdpar(2,is) = i
          endif
        enddo
      endif
C

C Compute the average frequency and the average squared frequency,
C to get the rms bandwidth. Also compute conversion factor from
C m to wavelengths. Calculate effective frequency for ionosphere
C correction.

      do ic=1,ncodes
      do is=1,nstatn
        do nx = 1,nband !maximum 2 bands
          nfr = nfreq(nx,is,ic)
          if1=0
          if (nx.eq.2) if1=nfreq(1,is,ic) ! assumes freqs are in order within a band
          if (nfr.gt.0) then
            frsum = 0.d0
            frsqsum = 0.d0
            frim = 0.d0
            fovref = 0.d0
            sum_del_f = 0.d0
            sum_del_f2 = 0.d0
            sum_del_fovf = 0.d0
            reffreq = 1.0d7
            nfrq_use=0
            do ifr=1,nchan(is,ic)
              iv=invcx(ifr,is,ic)
              if (iv.gt.0 .and. freqrf(iv,is,ic).gt. 0) then
                isub=iwhere_in_string_list(cbnd,nbnd,csubvc(iv,is,ic))

                if (isub.eq.nx) then
                  if (freqrf(iv,is,ic).lt.reffreq) then
                      reffreq = freqrf(if1+1,is,ic)
                  endif
                  nfrq_use=nfrq_use+1
                  frq=freqrf(iv,is,ic)
                  frsum = frsum + frq
                  frsqsum = frsqsum + frq*frq
                  frim = frim + 1.d0/frq
                  fovref = fovref + frq/reffreq
                  del_f = frq - reffreq
                  sum_del_f = sum_del_f + del_f
                  sum_del_f2 = sum_del_f2 + del_f**2
                  sum_del_fovf=sum_del_fovf+del_f/frq
                endif ! correct band
              endif ! this VC in use
            enddo ! max channels
            if(nfr .eq. 1) then
              effreq(nx,is,ic)=frsum
            else
              effreq(nx,is,ic)=dsqrt((nfrq_use*frsqsum-frsum**2)/
     .                    (frsum*frim-nfrq_use**2))
            endif
            wavei(nx,is,ic) = 1d6*frsum/(dble(nfrq_use)*c)
            bwrms(nx,is,ic) = dsqrt(frsqsum/dble(nfrq_use) -
     .                       (frsum/dble(nfrq_use))**2)
          endif
        enddo ! 2 bands
        ixband=1 !start assuming X is the first band
        isband=2
        if(csubvc(1,is,1) .eq. "S") then
          ixband=2
          isband=1
        endif
        ffact(is,ic) = effreq(isband,is,ic)**2 / 
     .         (effreq(ixband,is,ic)**2-effreq(isband,is,ic)**2)
      enddo ! stations
      enddo ! ncodes

      RETURN
      END

