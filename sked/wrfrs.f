      SUBROUTINE WRFRS(IERR)
C
C     WRFRS reads the catalogs and writes the scratch files
C     for the frequency codes and head positions.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_mode.ftni'
C
C  OUTPUT: IERR - error return
C
C  CALLED BY: FRCAT
C  CALLS:       FFREQ        read freq.cat
C               FRX          read rx.cat
C               FREC         read rec.cat
C               FTRACKS      read tracks.cat
C               COCHK        change freq code if duplicates
c               WRFCLINES    write "F" and "C" lines in $CODES
C               WRRBLINES    write "R" and "B" lines (rate and barrel)
C               WRLLINES     write "L" lines in $CODES
C               FHDPOS       read hdpos.cat, write $HEAD section

C  History
C 951127 nrv New version. Calls one routine for each catalog file
C            that is opened and read. 
C 960207 nrv Add reading/writing head positions.
C 960221 nrv Change WRFCLINES calling sequence
C 960223 nrv Change WRLLINES calling sequence
C 960403 nrv Remove sub-code stuff.
C 960408 nrv Add call to COCHK
C 960709 nrv Add lbarrelname to frec call, add call to WRRBLINES
C 970408 nrv Add lrecfmtname to frec call, add to WRFCLINES call
! 2005Apr26 JMG.  Made lbarrelname, l2code ascii through equivalence
! 2005May13 JMG.  Call to frec made to use ASCII, not holerith.
! 2005May20 JMG.  Got rid of lbarrelname
!                 Much cleanup. Got rid of saving stuff not used, etc.
! 2005OCt05 JMG.  Got rid of holleriths in call to wrfcline, ftracks
! 2005Nov18 JMG.  Rewrote ftracks, frec.
! 2005Nov28 JMG. All the routines now use ascii. Get rid of hollerith
! 2009Apr02 JMGipson. Changed name: icat_mode_freq-->icat_mode_freq_ptr
!                                   icat_mode_rec -->icat_mode_rec_ptr 
      integer renam ! function
C  LOCAL:
      integer ierr,ic
      character*2 c2code(max_frq)
      integer isub(max_stn) ! indices into LNASEL for sub-codes
      integer istn_rx_xref(max_stn) ! the station's RXname index
      integer nrx ! number of sub-codes in a frequency sequence
C     Following are gathered from the lines in the freq.cat catalog and 
C     returned in the call to FFREQ. One set of frequency info per sub-code, 
C     up to one sub-code per station. Info is preserved as strings mostly.
      character*2 cb(max_chan,max_stn)     	! band ID from freq lines
      character*2 cpol_local(max_chan,max_stn)	! pol from freq lines
      character*8 csky(max_chan,max_Stn)   	! sky freq from freq lines
      character*2 csb(max_chan,max_stn)     	! sideband from freq lines
      integer   ichan(max_chan,max_stn)   	! channel ID from freq lines
      integer   ibbc(max_chan,max_stn)    	! BBC number from freq lines
      character*8 cpcfr(max_chan,max_stn)  	! pcal freq from freq lines
      character*4 csw(max_chan,max_stn)		!sw lines
      real*8 bw_stn(max_stn)


      integer   nfr(max_stn)              ! number of freq lines 
C     Storage for reference names from catalogs
      character*8 crxname(max_stn)

      character*10 cloifname(max_stn)

      character*4 cbarrelname(max_stn)

      integer itrk_xref(max_stn)
      integer num_sel
      integer ifreq
      integer irec

!AEM 20050720      character*4 cfmt(max_stn)
!AEM increase size due to frec.f:30
      character*6 cfmt(max_stn)


C  0. First check for complete information, i.e. a sub-code, mode, and
C     bandwidth must all be selected.
C     Find the unique frequency codes and sub-codes.

      cb=" "
      ierr=0
C  1. Open the temporary file to which the lines will be written.

      OPEN (lutmp,file=CTMFIL,status='unknown',iostat=ierr) ! SKW*
      IF (IERR.NE.0) then
        close(lutmp)
        write(luscn,'("WRFRS: Error opening temp file: ",a)') ctmfil
        RETURN
      end if
C  2. Handle each frequency code separately.

      num_sel=0
      do ic=1,num_cat_mode ! loop for number of observing modes
        if(kcat_mode_sel(ic)) then
          num_sel=num_sel+1
          ifreq=icat_mode_freq_ptr(ic)
          irec=icat_mode_rec_ptr(ic)

          if(kverbose) write(luscn,
     >     "('Getting catalog information for frequency sequence ',a)")
     >      cat_mode_freq(ifreq)
          isub=0

C  3. Call to subroutines to retrieve frequency information from the catalogs
C     and write to the temporary file. At the end, the "F", "C", and "L" lines
C     for the $CODES section are written out for one frequency code.

          call ffreq(cat_mode_freq(ifreq),c2code(num_sel),crxname,nrx,
     >        cb,cpol_local, csky,csb, ichan, ibbc, cpcfr,csw,nfr,ierr)        ! read freq.cat
          if (ierr.ne.0) return
          call cochk(num_sel,c2code)

          call frx(crxname,nrx,istn_rx_xref,cloifname,ierr)                    ! read rx.cat
          if (ierr.ne.0) return

          call frec(cat_mode_rec(irec),rcat_mode_bw(ic),
     >      cnahdpos(1,num_sel),itrk_xref, cfmt,cbarrelname, 
     >      bw_stn,ierr)                                           ! read rec.cat
          if (ierr.ne.0) return

          call wrfclines(nrx,cat_mode_freq(ifreq),c2code(num_sel),
     >     istn_rx_xref,
     >     nfr,cat_mode(ic), cfmt,bw_stn,ichan,ibbc,csw,cb,csky,cpcfr,
     >     itrk_xref,ierr)        !write the "F" and C" lines
          if (ierr.ne.0) return

          call wrrblines(rcat_mode_samp(ic),c2code(num_sel),
     >         cbarrelname,ierr)        ! write "R" and "B" lines for this code
          if (ierr.ne.0) return

          call wrllines(cloifname,c2code(num_sel),nfr,ierr)                    ! write "L" lines for this code
          if (ierr.ne.0) return
        endif
      enddo

C  3. Now the $CODES section is done. Close the temp file and rename it.
      close(lutmp)
      ierr = renam(ctmfil,cfrfil)
      if(ierr .lt. 0) goto 900

C  4. Open the temporary file to which the head lines will be written.

      OPEN (lutmp,file=CTMFIL,status='new',iostat=ierr) ! SKW*
      IF (IERR.NE.0) then
        close(lutmp)
        write(luscn,'("Error opening temp file: ",a)') ctmfil
        RETURN
      end if

C  5. Call to subroutine to get the head positions and write out
C     the lines in $HEAD section. Write out lines for all stations,
C     one code at a time.

      do ic=1,num_sel
        call fhdpos(ierr,cnahdpos(1,ic),c2code(ic))
      end do

C  6. Close the temp file and rename it.
      close(lutmp)
      ierr = renam(ctmfil,chdfil)
      if(ierr .eq. 0) return

! Come here on error renaming the temp file.
900   continue
      write(luscn,"('Error ',i5,' renaming work file: ',a,
     >                  ' to scratch file: ',a)") ierr,ctmfil,cfrfil
      return
C
      END
