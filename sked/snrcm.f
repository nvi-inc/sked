      SUBROUTINE SNRCM(LINSTQ,cfrom,ctype)
C
C    SNRCM reads/writes subnet SNRs
C
C  COMMON:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'major.ftni'

! functions
      integer igtba,ibnum,ichmv
      integer istringminmatch
      real*8 DAS2B
C
C  INPUT:
C     LINSTQ - input buffer, first word is length
      integer*2 LINSTQ(*)
! AEM 20050202 char -> char*1      
      character*1 cfrom !character to distinguish where called from.
      character*1 ctype ! ' ' for SNR command, '1' for SNR_1 command.
C
C  OUTPUT: none
C
C
C  LOCAL
C     LKEYWD - holder for each station name/number
      integer*2 LKEYWD(12),lf
      character*2 cf
      integer IST(MAX_STN)
      integer istat
! AEM 20050203 grow up cnnx (8->12)
      integer idum
      character*12 cnnx ! format 'nnX'
      real*4 snr_in
      integer iband(max_band)
      integer i,j,k,is,js,ib,nch,ich,ic1,ic2,icod,
     .ics,ns,nba,iba,ierr
      integer icmd

! AEM undo      character*12 ckeywd
! AEM 20050202 char*12->char*24 to match lkeywd
      character*24 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=2)
      character*10 list(ilist_len)
      data list/"MARGIN","AST_MARGIN"/
C
C MODIFICATIONS:
C   890425 NRV Created, modeled on SELEV
C   891128 NRV Changed syntax to specify band
C   891207 NRV Added optional MARGIN specification
C   900312 gag  added cfrom and knewpa
C   950405 nrv Use 2-letter station codes.
C 991119 nrv Add TYPE to call. Set isnrbl_1 for snr_1 command.
C
C
C  1. Check for some input.  If none, write out current.
C
      if (ncodes.le.0) then
        write(luscn,'(" SNRCM01 - Select frequencies first.")')
        return
      end if

      ICH = 1
      nch=linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,nch,IC1,IC2)
      IF  (IC1.EQ.0) THEN  !no input
        IF  (NSTATN.LE.0) THEN  !no stations selected
          write(luscn,'(" SNRCM02 - Select stations first.")')
          RETURN
        END IF  !no stations selected
C
C  2. Top line with title and band names
C
        WRITE(LUDSP,'(" Minimum SNR by baseline ",$)')
        if (ctype.eq.' ') write(ludsp,'("for multi-baseline scans")')
        if (ctype.eq.'1') write(ludsp,'("for single-baseline scans")')
        icod=1 ! *** only 1 for now
        call gtban(icod,nba,iband)
        do k=1,nba
          iba=iband(k)
          write(ludsp,'(5X,A1,"-band (margin     ",$)') lband(iba)
          if (ctype.eq.' ') then
             write(ludsp,'(i3,")",$)') imarg(iba)         
          endif
          if (ctype.eq.'1') write(ludsp,'(i3,")",$)') imarg_1(iba)
        enddo 
        WRITE(LUDSP,*)
        if(ctype .eq. ' ') then
         do k=1,nba
          iba=iband(k)
           write(ludsp,'(5X,A1,"-band (ast_margin " i3, ")",$)') 
     >       lband(iba), imarg_ast(iba)     
         enddo
         writE(ludsp,*) 
        endif      
C
C  3. Second line with station names across.
C
        do k=1,nba
! AEM undo          if (k.eq.1) write(ludsp,'(5x,$)')
!          if (k.eq.2) write(ludsp,'(8x,$)')
! AEM 20050202 modify 'Nx'
          if (k.eq.1) write(ludsp,'(4x," ",$)')
          if (k.eq.2) write(ludsp,'(7x," ",$)')
          DO I=1,NSUBST-1
            J=ISUBST(I)
            WRITE(LUDSP,'(A2,"  ",$)') cpoCOD(J)
          ENDDO
        enddo
        WRITE(LUDSP,'()')
C
C  4. Matrix lines, one for each station.
C
        DO I=2,NSUBST ! each station down
          IS=ISUBST(I)
          cnnx = '(   x," ",$)'
	  write(cnnx(2:4),'(i3)') 4+(nsubst-i)*4
          do k=1,nba ! each band
            iba=iband(k)
! AEM 20050203 calc length for cnnx
            if (iba.gt.1) write(ludsp,cnnx)
            WRITE(LUDSP,'(1x,a2,$)') cpoCOD(IS)
            DO J=1,I-1 ! each station across
              JS = ISUBST(J)
              IB=ibnum(is,js)
              if (ctype.eq.' ') WRITE(LUDSP,'(1x,i3,$)')ISNRBL(iba,IB)
              if (ctype.eq.'1') WRITE(LUDSP,'(1x,i3,$)')ISNRBL_1(iba,IB)
            ENDDO ! each station across
          enddo ! each band
          WRITE(LUDSP,'()')
        ENDDO ! each station down
        RETURN
      END IF  !no input
C
C
C 5. Something is specified.  Get each station/band/SNR combination.
C
      lf=0
      if ((ic1.ne.0).and.(cfrom.eq.'s')) knewpa = .true.
      DO WHILE (IC1.NE.0) !more decoding
        ics=ic1
! AEM 20050203 blank 'cf' else can cause an error setting up snr
        icmd=0
        CALL GTSTI(LINSTQ,ICS,NS,IST,IERRCM,0)
        if(ierrcm .eq. 0) then        !found a station.
          IF (NS.EQ.0) THEN !all stations
            NS=NSUBST
            DO I=1,NS
              IST(I)=ISUBST(I)
            ENDDO
          ENDIF !all stations
        else   !not a station. See if margin or ast_margin      
          ckeywd=" "
          idum=ichmv(lkeywd,3,linstq(2),ic1,ic2-ic1+1)
          icmd =istringMinMatch(list,ilist_len,ckeywd) ! check for margin.
          if (icmd.le.0) then
            write(luscn,'(" SNRCM03 - Invalid station: ",a2)') ckeywd
            RETURN
          endif                      
        endif !stations not margin
! Get the band. 
        nch=linstq(1)
        CALL GTFLD(LINSTQ(2),ICH,nch,IC1,IC2)
        IF  (IC1.EQ.0) THEN  !no matching band
          write(luscn,'(" SNRCM04 - No matching band.")')
          RETURN
        END IF  !no matching band
        nch=ic2-ic1+1
        ckeywd = " "
        idum=ichmv(lkeywd,1,linstq(2),ic1,min0(nch,20))
        iba=igtba(ckeywd)
        if (iba.eq.0) then
          write(luscn,'(" SNRCM05 - Invalid band ",a2)') lkeywd(1)
          return
        endif
        nch=linstq(1)
        CALL GTFLD(LINSTQ(2),ICH,nch,IC1,IC2)
        IF  (IC1.EQ.0) THEN  !no matching SNR
          write(luscn,'(" SNRCM06 - No matching SNR.")')
          RETURN
        END IF  !no matching SNR
        snr_in= DAS2B(LINSTQ(2),IC1,IC2-IC1+1,IERR)
        IF  (IERR.LT.0.OR.snr_in.LT.0.0) THEN  !invalid
          write(luscn,'(a,1x,i4,1x,f8.2)')
     >       " SNRCM07 - Invalid value for SNR (ierr,val): ",ierr,snr_in
          RETURN
        END IF  !invalid
        if (icmd .eq. 1) then
          if (ctype.eq.' ') imarg(iba)=snr_in
          if (ctype.eq.'1') imarg_1(iba)=snr_in
        else if(icmd .eq. 2) then
           imarg_ast(iba)=snr_in
        else !stations
          istat=0             !default for single station.
          if(ns .eq. 1 .and. ist(1) .ne. -1) then
             do i=1,nstatn
               if(i .ne. ist(1)) then
                  ib=ibnum(i,ist(1))
                  if (ctype.eq.' ') ISNRBL(iba,IB) = snr_in
                  if (ctype.eq.'1') ISNRBL_1(iba,IB) = snr_in
                endif
             end do
          else
            DO I=1,NS-1
              IS=IST(I)
              DO J=I+1,NS
                JS=IST(J)
                ib=ibnum(is,js)
                if (ctype.eq.' ') ISNRBL(iba,IB)   = snr_in
                if (ctype.eq.'1') ISNRBL_1(iba,IB) = snr_in
              ENDDO
            ENDDO
          endif
        endif !margin/stations
C
100     continue 
        nch=linstq(1)
        CALL GTFLD(LINSTQ(2),ICH,nch,IC1,IC2)
        END DO  !more decoding
C
        RETURN
        END
