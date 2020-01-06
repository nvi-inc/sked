      SUBROUTINE FRSEL(ccall,ierr)
C
C   FRSEL allows the user to select frequencies for scheduling.
C
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include 'cat_name_version.ftni' 
C
C   CALLING SUBROUTINES: CATCMD
C   CALLED SUBROUTINES: 
C
C   INPUT
      character*1 ccall ! s=standard, j=JAVA,
                        ! if a, then automatic.
                        ! if R, then reinitialize frequencies
C   LOCAL VARIABLES
      integer ilen,iserr(max_stn)
      integer ierr

C History:
C 841018  MWH   SUPPORT SELECTION ON DUMB TERMINAL
C 880310  NRV   DE-COMPC'D
C 880423  NRV   CHANGED EXEC(23) TO FmpRunProgram
C 880829  PMR   changed FmpRunProgram to Exec/Fork
C 891121  GAG   Changed ISCUN(2) to lutmp
C 891130  NRV   Removed restriction on reading LO records (?)
C 900130  gag   made compatible with new catalog routine
C 930707  nrv   Add call to HDINP
C 951019 nrv Remove VBINP call
C 960209 nrv Add error by station to GNPAS call.
C 960221 nrv Change ivix -> ibbcx, initialize lifinp
C 960221 nrv Read the entire file, report all errors, then return.
C 960228 nrv Re-initialize itras before getting new file
C 960409 nrv Initialize itra2
C 970219 nrv Remove initialization and add a call to FRINIT
C 991116 nrv Remove FRCAT call. This routine is now called by CATCMD.
C 000106 nrv Replace FRCAT call.
! 2012Oct10 JMG. Modified to update version, catalog name info. 
 
C  1. Schedule FRCAT and have it do the selection.
C NEW: This routine is called by CATCMD, which reads the
C selected modes and calls WRFRS first. 
 
      norig = 0
      if (ccall.eq.'s' .or. ccall .eq. 'a') then
        call frcat(ierr,knewfr,ccall)
        if (.not.knewfr .or. ierr.ne.0 .or. ccall .eq. 'a') return
      endif


      lmodes_cat_use=modes_cat
      call get_cat_version(lmodes_cat_use,lmodes_cat_version,ierr)
      lfreq_cat_use=freq_cat
      call get_cat_version(lfreq_cat_use,lfreq_cat_version,ierr)
      lrec_cat_use=rec_cat
      call get_cat_version(lrec_cat_use,lrec_cat_version,ierr)
      lrx_cat_use=rx_cat
      call get_cat_version(lrx_cat_use,lrx_cat_version,ierr)
      lloif_cat_use=loif_cat
      call get_cat_version(lloif_cat_use,lloif_cat_version,ierr)
      ltracks_cat_use=tracks_cat
      call get_cat_version(ltracks_cat_use,ltracks_cat_version,ierr)
      lhdpos_cat_use=hdpos_cat
      call get_cat_version(lhdpos_cat_use,lhdpos_cat_version,ierr)

! initialize frequencies.
      call freq_init

C  2. We have returned from FRCAT.  Read the files it wrote.

      OPEN(lusel,file=CFRFIL,iostat=IERRCM)
      IF  (IERRCM.NE.0) THEN 
        INUMCM = 56
        CALL WRERR(INUMCM,IERRCM)
        RETURN
      END IF  !
C
      cbuf=" "
      CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)
      NCODES = 0
      call frinit(max_stn,max_frq)
      DO WHILE (IERR.GE.0.AND.ILEN.GT.0)
C     read each frequency record
        CALL FRINP(IBUF,ILEN,LUSCN,IERRCM)
        cbuf=" "
        CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)                           
      END DO  !read each frequency record                             
      close(lusel)
C                                                                           
C  Read the head file SKH****

      OPEN(lusel,file=CHDFIL,iostat=IERRCM)
      IF  (IERRCM.NE.0) THEN  !
        write(luscn,9302) ierrcm,chdfil
9302    format('FRSEL02 - Error ',I3,' opening head temp file ',A)
        RETURN
      END IF  !

      cbuf=" "
      CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)

      DO WHILE (IERR.GE.0.AND.ILEN.GT.0)
C     read each head record
        CALL HDINP(IBUF,ILEN,LUSCN,IERRCM)
        IF  (IERRCM.NE.0) THEN  !
          CLOSE(lusel)
          RETURN
        END IF  !                                                     
        cbuf=" "
        CALL READF_ASC(lusel,IERR,IBUF,IBLEN,ILEN)                           
      END DO  !read each head record

      CLOSE(lusel)                                                    
      KNEWFR = .TRUE.                                                       
      icode_set_last=1
      CALL GNPAS(luscn,ierr,iserr)
      call setba

      RETURN
      END

