      SUBROUTINE UNPAK(KERR,nsubc)
CHS------------------------------------------------------------------------
CHS Unpak was extended for the parameter nsubc in the parameter list. This
CHS parameter can either be 0 (insert/delete mode) or 1,2,3 or 4 (optimization
CHS mode).
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    UNPAK unpacks the record found in IBUF and puts the data into
C              the CUR variables
C
      use obs_scan_counters
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

! function
      double precision hms2seconds
      integer igetsrcnum
      integer ibnum,igtfr,julda ! function
      integer jchar,ias2b,ichmv,ichmv_ch
      integer trimlen
      integer igetstatnum

C   INPUT:
        integer nsubc

C   OUTPUT VARIABLES:
        integer kerr
C        KERR   - Error returned non-zero if problems.
C        CUR variables are not updated if KERR=1
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
C
C   LOCAL VARIABLES
      double precision ST0,GST,UT !for GST and SIDTM calculations
      integer*2 lsrcnam(max_sorlen/2)
      character*(max_sorlen) csrcnam
      equivalence (lsrcnam,csrcnam)

      integer*2 LPRE(3),LMID(3),LPST(3)
      character*6 cpre,cmid,cpst
      equivalence (cpre,lpre), (cmid,lmid),(cpst,lpst)

C      - holders for source, procedure names
      integer*2 LST(MAX_STN),ICB(MAX_STN)
      integer istn(max_stn)
      character*1 c1
      integer itemp
      equivalence(itemp,c1)
      logical keep_index

C                   - temporary station code and cable wrap holder
      integer IFT(MAX_STN),IDUR(MAX_STN)
C              - temp holders for pass, dirn and footage count
      integer ich,ic1,ic2,idumy,ical,iyr,ida,ihr,imin,isc,mjd,
     >   idurx,idle,i,nst,nch,ni,j,ist,isor,icod,ib
! AEM 20041206 integer -> integer*2 (status critical)
      integer*2 lfrq
C
C   HISTORY
C     880315 NRV DE-COMPC'D
C     890502 NRV Added reading durations
C     930219 nrv merge sked/autosked: add ireccu initialization
C     930225 nrv implicit none
C     930722 nrv Increment nsorobs
C     931013 nrv Remove DMOD and check GST for > 2PI
C     931110 nrv Add st0cur
C 970407 nrv Call IGTUSE to find ITUCUR
C 981113 nrv Read 2-digit year from input line but store a 4-digit year.
!     2003May30  JMG remove igtuse
!     2003Dec08  JMG replaced igtso by igetsrcnum
! 2005Jun13 JMgipson.  Modified to add in duration for UTPRSO.
! 2006Sep26 JMGipson. Replaced iflch by trimlen
! 2010Apr26 JMGipson. Better error messages.
! 2014May02 JMG. Removed ipas,idir, ift from call to set_scan_param. No longer used. 
! 2021-12-02 JMG. Got rid of filling lpre with blanks. Used cpre 

C
C
C     1. For our first trick, we decode all of the entries in
C     the buffer.
C     **CAUTION** No error checking is done.  It is assumed
C                 that the schedule entries were written by
C                 SKED originally and so should not have to
C                 be checked.
C     The format of the entries is the following:
C
C     source cal code preob start duration midob idle postob scscsc pdfoot p
C     Example:
C     3C84      120 SX PREOB 800923120000  780 MIDOB    0 POSTOB K-F-G-OW 1F
C     where all items are not restricted to specific columns.
C     **NOTE: "code" IS REALLY ONLY CODE NOW, NOT MODE&BANDWIDTH TOO.
C

      KERR = 0
      ICH = 1
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      csrcnam=" "
      IDUMY = ICHMV(LSrcnam,1,IBUF,IC1,min0(IC2-IC1+1,max_sorlen))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      ICAL = IAS2B(IBUF,IC1,IC2-IC1+1)
      if(ical .lt. 0) then
         write(*,*) "Invalid cal ", ical
         goto 999 
      endif

      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDUMY = ICHMV(LFRQ,1,IBUF,IC1,2)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
!      CALL IFILL(LPRE,1,6,oblank)
      cpre=" " 
      IDUMY = ICHMV(LPRE,1,IBUF,IC1,min0(IC2-IC1+1,6))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      read(cbuf(ic1:ic1+11),"(i2,i3,i2,i2,i2)") iyr,ida,ihr,imin,isc
      if(iyr .le. 49) then
        iyr=iyr+2000
      else
        iyr=iyr+1900
      endif
! Check that the time is valid 
      if(iyr .lt. 1979 .or. iyr .gt. 2050) then
         write(*,*) "Invalid year ", iyr
         goto 999
      else if(ida .lt. 0 .or. ida .gt. 366) then
         write(*,*) "Invalid day ",ida
         goto 999
      else if(ihr .lt. 0 .or. ihr .gt. 24) then
         write(*,*) "Invalid hour ", ihr
         goto 999
      else if(imin .lt. 0 .or. imin .gt. 59) then
         write(*,*) "Invalid minute ", imin
         goto 999
      else if(isc .lt. 0 .or. isc .gt. 59) then
         write(*,*) "Invalid second ", isc
         goto 999
      endif           

      MJD = JULDA(1,IDA,IYR-1900)
      ut=hms2seconds(ihr,imin,isc)

      CALL SIDTM(MJD,ST0,FRAC)
      GST = DMOD(ST0 + UT*FRAC, 2.D0*PI)

      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDURX = IAS2B(IBUF,IC1,IC2-IC1+1)
      if(idurx .le.0) then
         write(*,*) "Invalid duration: ",idurx
         goto 999
      endif


      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
!      CALL IFILL(LMID,1,6,oblank)
      cmid=" " 
      IDUMY = ICHMV(LMID,1,IBUF,IC1,min0(IC2-IC1+1,6))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      IDLE = IAS2B(IBUF,IC1,IC2-IC1+1)
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
!      CALL IFILL(LPST,1,6,oblank)
      cpst=" " 
      IDUMY = ICHMV(LPST,1,IBUF,IC1,min0(IC2-IC1+1,6))
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      I=1
      DO WHILE ((IC1+(I-1)*2).LE.IC2.AND.I.LE.MAX_STN)
          IDUMY = ICHMV(LST(I),1,IBUF,IC1+(I-1)*2,1)
          IDUMY = ICHMV(ICB(I),1,IBUF,IC1+1+(I-1)*2,1)
          IDUMY = ichmv_ch(ICB(I),2,' ')
          I=I+1
      END DO  !
111   NST = I-1
CHS---------------------------------------------------------------
CHS This information is needed to compute the number of total obser-
CHS vations within a subconfiguration in nextc and testcon.
C
      lobs = (nst*(nst-1))/2
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      I = 1

      DO WHILE (IC1.NE.0.AND.I.LE.NST) ! decode footage counters
        nch = ic2-ic1+1
        IFT(I) = IAS2B(IBUF,IC1+2,nch-2)
        CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
        I = I+1
      END DO  !decode footage counters

      IF  (I.EQ.1) THEN  !no counters
        DO  NI = 1,NST
          IFT(NI) = 0
        END DO  !
      END IF  !no counters
      IF  (I.GT.1.AND.I.LT.(NST+1)) THEN  !too few counters
        DO  NI = I,NST 
          IFT(NI) = IFT(I-1)
        END DO  !
      END IF  !too few counters
C Skip over procedure flags
C The GTFLD call was already done in the footage loop, so IC1
C points to the first duration
C Start reading durations
      CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
      I = 1
      DO WHILE (IC1.NE.0.AND.I.LE.NST)  !decode durations
        IDUR(I) = IAS2B(IBUF,IC1,IC2-IC1+1)
        CALL GTFLD(IBUF,ICH,IBUF_LEN*2,IC1,IC2)
        I = I+1
      END DO  !decode durations
      IF (I.EQ.1) THEN !no durations
        DO I=1,NST
          IDUR(I)=IDURX
        ENDDO
      ENDIF !no durations
C
C
C     2. Check out the source and stations and codes to make sure they
C     are selected.  If not, return an error.  We might mess up
C     the CUR variables unknowingly if we go ahead with an
C     unrecognized station or source.
C
      isor=igetsrcnum(csrcnam)
      IF  (isor .le. 0) then
        WRITE(LUSCN,'("Source ", A ," not selected.")') csrcnam
        goto 999
      endif ! no match on source name

      DO  J=1,NST !check station names
        itemp=lst(j)                    !note: itemp=c1
        ist=igetstatnum(c1)
        if(ist .ne. 0) then
          LST(J) = IST
          istn(j)=ist
        ELSE  !no match
          WRITE(luscn,'("Station ", a, " not selected. ")') C1
          goto 999
        END IF  !no match
      END DO  !check station names
C
      IF  (IGTFR(LFRQ,ICOD).EQ.0) then !no match on freq code
        WRITE(LUSCN,'("Frequency ",a2, " not selected!")') LFRQ
        goto 999
      endif !no match on freq code

C     3. Now we have all the variables we need in temporary storage.
C     Stuff them into the CUR variables now.
C
CHS------------------------------------------------------------------
CHS Insert/delete mode
CHS The data is put into the CUR-variables.
C
      keep_index=.false.
!      write(*,*) "Unpak nsubc: ", nsubc
      if(nsubc.eq.0) then ! insert/delete mode
! A real scan
       call set_scan_param(
     >  nst,    istn,     mjd,    ut,
     >  isor,   ical,    idle,   icod, ircur,   idur,  icb,
     >  cpre,cmid,cpst,keep_index,
     >  nstncur,istcur,  mjdcur, utcur,gstcur,st0cur,iyrcur, idacur,
     >  nsorcur,icalcur, idlcur, icodcur,ireccur,idurcur,lcblcur,
     >  cprecur,cmidcur,cpstcur)

 
      UTPRSO(ISOR)=UT+idurx
      MJPRSO(ISOR)=MJD
!      write(*,*) "UNPAK: incrementing counters" 
      call update_obs_scan_counters(isor,istcur,nstncur)
      
    
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CHS-------------------------------------------------------------------
CHS Optimization mode
CHS Now the data is put into the tst-variables.
C
      else ! optimization algorithm
! A fake scan
       mjdtst=0
       call set_scan_param(
     >  nst,    istn,     mjd,    ut,
     >  isor,   ical,    idle,   icod, ircur,   idur,  icb,
     >  cpre,cmid,cpst,keep_index,
     >  nstntst,isttst,  mjdtst, uttst,gsttst,st0tst,iyrtst, idatst,
     >  nsortst,icaltst, idltst, icodtst,irectst,idurtst,lcbltst,
     >  cpretst,cmidtst,cpsttst)

C     For minimum time between two start times
      utobs=dble(mjd)*86400.d0+ut
C     For minimum time between end of one obs and start of next
      utobss=dble(mjd)*86400.d0+ut+idurx
      endif
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      RETURN

999   continue
      write(luscn,"('UNPAK Problem record: ',a)") cbuf(1:trimlen(Cbuf))
      KERR = 1
      RETURN
C
      END
