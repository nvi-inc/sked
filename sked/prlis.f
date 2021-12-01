      SUBROUTINE PRLIS(linstq,ich)
C
C PRLIS lists the values of the parameters used as defaults in
C              the scheduling program.
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
! functions
      integer istringminmatch
      integer trimlen,ichmv ! function
C
!functions
      character*1 lyn
C     INPUT VARIABLES:
      integer*2 linstq(*)
      integer ich
C
C COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/freqs.ftni'
!      include 'minor.ftni'


! Recent history in reverse order
! 2016Jan04 JMG. Print out modscn various additional values.
! 2015Mar17 JMG. Added Mark6_off
! 2016Dec08 KOL. Added Fill_off
! 2017Oct06 KLB. Add kconf_equip
! 2021-05-04 JMG got rid of 'modular' (unitilaized variable) and added implicit none 
! 
C
C CALLING SUBROUTINES: PRCMD (the command decoder for parameter
C                                 selection)
C          PRSET (the parameter setting routine lists the parameters first
C
C  LOCAL VARIABLES
      integer*2 lkeywd(12)
      character*2 ckey
      integer nc,ic1,ic2,ikey,lookm,i1
      integer*2 LMON(2),LDAY(2)
      integer i,ihr,imin,isec,imon,iday,iyear,iih,iim,iis
      integer idummy,irah,idch
      real*4 rah,ram,dch,ep,dcm

      character*4 lsnr

      integer iprp,iprl
C          -  Holders for synchronization, vis, confirm, snr
      LOGICAL KALL
C          KALL - List all if true
      character*4 LFLAG
C               - Holder for procedure 'required' flags
      real*8 GSTSEC
C               - Convert GSTCUR to seconds
! AEM 20050204 char*12->char*24
      character*24 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=5)
      character*10 list(ilist_len),listshort(ilist_len)

      data list/"ALL","GENERAL","NOTES","PROCEDURE","SNR"/
      data listshort/"AL","GE","NO","PR","SN"/

C
C  History
C     880310 NRV DE-COMPC'D
C     880803 PMR changed format for file name                             param
C     890110 GAG added SUNDIS
C     890426 GAG ADDED MINSCAN,MAXSCAN,MODSCAN,WIDTH
C     890427 GAG ADDED BWSCAN,SNRSCAN,CHANSCAN,VIS,CONFIRM,BASESCAN
C     890517 GAG ADDED CORSYNCH
C     890522 GAG ADDED ID CALL AND PRINT OUT
C     890815 GAG added parameter list option snr & procedure
C     891127 GAG added parameter list option general
C     891129 GAG added logical kall
C            gag removed BASESCAN,SNRSCAN,BWSCAN,CHANSCAN
C     891205 NRV Added SNR parameter
C     900206 gag added all option for listing and removed mxfeet
C     900323 gag removed maxscan and added vscan
C     910224 NRV Added EARLY, removed PEAK, ELEVATION
C     930408 nrv Put minbetween back in
C     931029 nrv Remove MINBETWEEN (it's now in OPSET etc.)
C     940202 nrv Add a place to display the sked version number
C     940705 nrv Add display of printer commands
C     950405 nrv Use 2-letter station codes in SUBNET line.
C     950502 nrv Add MINSUBNET
C 951015 nrv MJD has gone over 10000
C 951017 nrv Change igtky call to use lkey
C 951214 nrv Add BARREL
C 960709 nrv Remove barrel, moved to freqs.ftni
C 960923 nrv Print out ITEARL for station 1
C 970314 nrv Remove EARLY, it was moved to a separate command
C 981113 nrv Remove LDOY and use format I3.3. Print 4-digit year.
C 990412 nrv Add MAXSCN.
C 990520 nrv Add description, scheduler, correlator.
C 991116 nrv Add nominal start,end.
C 000126 nrv MAXSCN needs 4 digits.
C 020227 nrv Add option for NOTES output.
C 021011 nrv Add POSTPASS to listing.
! 20050301 AEM  cosmetic changes
! 2008MAY21 JMG. Major changes in writing.
! 2008May22  Moved many parameters from $PARAM to $MAJOR
! 2009Feb05  Missed writing setup time in 2008May21. Put  back in. 
! 2009Oct30  Got rid of unused vector array lsrcdist
! 2010Sep16 Got rid tape parameters knorewind, kpostpass, ksynch

C
C     1. We simply write out the parameter name keyword, followed by
C        the value of the parameter, and finally a brief description
C        of the parameter.
C        First, convert some internal variables from seconds to minutes.
C
      KALL = .FALSE.

      call seconds2hms(utcur(1),ihr,imin,isec)
      IMON = 0
      IDAY = IDACUR(1)
      IYEAR = IYRCUR(1)
      CALL CLNDR(IYEAR,IMON,IDAY,LMON,LDAY)
      ep = 1.d0*iyrcur(1) + idacur(1)/365.d0
      GSTSEC = GSTCUR(1)*43200.D0/PI
      call seconds2hms(gstsec,iih,iim,iis)
      
      call sunpo(mjdcur(1),utcur(1),pi,rasun,decsun)
      RAH = RASUN*12.0/PI
      IRAH = RAH
      RAM = (RAH-IRAH)*60.0
      DCH = DABS(DECSUN*180.D0/PI)
      IDCH = DCH
      DCM = (DCH-IDCH)*60.0
      IF (DECSUN.LT.0) IDCH=-IDCH
      
      if(kasnr) then
         lsnr="MAN"
      else
         lsnr="AUTO"
      endif

      lflag="NNNN"
      DO  I = 1,4
         IF (KFLG(I)) lflag(i:i)='Y'
      END DO
      LOOKM = LOOKAH/60
C
      ikey = 0
      ckey=" "
      nc = linstq(1)
      if(nc .eq. 0) then
         KALL =.true.
         ckey="AL"
         goto 100
      endif

      call gtfld(linstq(2),ich,nc,ic1,ic2)
      if (ic1.gt.0) then
        nc = ic2-ic1+1
        ckeywd=" "
        idummy = ichmv(lkeywd,3,linstq(2),ic1,nc)
        ikey=istringMinMatch(list,ilist_len,ckeywd)
        if (ikey.eq.0) then
          write(luscn,9110) ckeywd
9110      format('PRLIS01 - Key word ',a,' invalid for parameter ',
     .    'listing.')
          return
        end if
        ckey=listshort(ikey)
      else
        KALL = .TRUE.
        ckey ="AL"
      end if

100   continue
      i1=trimlen(cexperdes)
      if (i1.eq.0) i1=1
      write(ludsp,'(A)')
     >"----- Parameter listing --------------------------------------"

      write(ludsp,9100) cexper,cexperdes(1:i1),
     > cpiname(1:10),ccorname(1:10)
9100  format(' Experiment: ',a8, 20x,'Description ',a, /,
     >       ' Scheduler:  ',a10,18x,'Correlator: ',a)

      write(ludsp,9101) iyr_start,ida_start,ihr_start,imin_start,
     .isc_start,iyr_end,ida_end,ihr_end,imin_end,isc_end
9101  format(' Start:      ',i4,'-',i3.3,'-',2(i2.2,':'),i2.2,10x,
     .       ' End:        ',i4,'-',i3.3,'-',2(i2.2,':'),i2.2)

      if ((ckey.eq.'SN').or.(ckey.eq.'AL')) then
       write(ludsp,'(A)')
     >"---------- Scan data parameters-------------------------------"

       write(ludsp,
     > '(" VScan       ",a3," (Compute scan length)  ",$)')lyn(kvscan)
       write(ludsp,
     > '(" Duration    ",I3,"sec (default duration)  ")') idurde

       write(ludsp,
     > '(" Minslew     ",i3,"sec (min slew time)     ")') imintm
       write(ludsp,
     > '(" Minscan     ",i4,"sec (min scan length)  ",$)') minscn
       write(ludsp,
     > '(" Maxscan     ",i4,"sec (max scan length)  ")') maxscn
       write(ludsp,
     > '(" Modscan     ",i4,"sec (mod scan time)    ",$)') modscn
 
       write(ludsp,
     > '(" Calibration ",I3,"sec (time before obs)   ",$)') icalde

       write(ludsp,
     > '(" Early       ",i3,"sec (start recording)   ")')
     > itearl(1)

       write(ludsp,
     > '(" Corsync     ",i3,"sec (pad scan at end)   ",$)') itsync


       write(ludsp,
     > '(" Idle        ",I3,"sec (idle after obs)    ")') idldef


       write(ludsp,
     > '(" Setup       ",I3,"sec (scan setup)        ",$)') isettm

      write(ludsp,
     > '(" Mark6_off   ",I3,"sec (buffer offset)     ")') imark6_off 

      write(ludsp,
     > '(" Fill_off    ",I3,"sec (buffer offset)     ")') ifill_off 

      end if


      if ((ckey.eq.'PR').or.(ckey.eq.'AL')) then
        write(ludsp,'(A)')
     >"---------- Procedure parameters ------------------------------"

        write(ludsp,
     >'(" PRFLAG  ",A4," (required procedures)     ",$)')  lflag
        write(ludsp,
     >'(" PREOB  ",A6," (pre-ob procedure)        ")') cprede
        write(ludsp,
     >'(" MIDOB  ",A6," (mid-ob procedure)       ",$)') cmidde
        write(ludsp,
     >'(" POSTOB ",A6," (post-ob procedure)        ")') cmidde

        write(ludsp,'(A)')
     >"---------- Timing parameters ------------------------------"
        write(ludsp,
     >'(" Parity    ",I4,"sec (parity check time)  ",$ )') ipartm
    
       write(ludsp,
     > '(" SOURCE    ",I4,"sec (SOURCE time)        " )')
     > isortm

       write(ludsp,
     > '(" Tapetm    ",I4,"sec (TAPE command time)    ")') itaptm

      end if

      if (ckey.eq.'NO') then ! for the notes file
        write(ludsp,9143)
     .  IYRCUR(1),idacur(1),ep,MJDCUR(1),LDAY,IDAY,LMON,
     .  IIH,IIM,iis,IHR,IMIN,ISEC,IRAH,RAM,IDCH,DCM
9143    format(
     .' Current yyyyddd:    ',I4,i3.3,' (',f7.2,')  (',
     .                         I6,' MJD, ',2A2,' ',I2,2A2,')'/
     .' Greenwich sidereal time:   ',2(I2.2,':'),i2.2,' (',
     .       2(I2.2,':'),I2.2,' UT)'/
     .' Sun''s RA and DEC:   ',I2,'h',F5.1,'m   ',I3,'d',F5.1)

      endif ! for the notes file
      if ((ckey.eq.'GE').or.(kall).or.(ckey.eq.'AL')) then
        iprl=trimlen(cprtlan)
        iprp=trimlen(cprtpor)
        write(ludsp,'(A)')
     > "---------- General parameters -------------------------------"


        write(ludsp,
     >   "(' MODULAR   ',I3,'sec (start time mark)',4x,' ',$)")   imodtm
        write(ludsp,"(' MINIMUM ',I3,'sec (time between obs.)  ')")
     >    imintm

        write(ludsp,
     >   "(' LOOKAHEAD ',I3,'min (for WHATSUP)    ',4x,' ')") lookm

          write(ludsp,
     >   "(' SNR     ',A4,' (reject for low SNR)      ',$)")
     >   lsnr

        write(ludsp,"(' WIDTH ',I3,'columns (width of screen)')")
     >   iwscn

        write(ludsp,
     >   "(' CONFIRM     ',A1,' (ask before adding obs)  ',$)")
     >   lyn(kask)
     
        write(ludsp,
     >   "(' KEEP_LOG    ',A1,' (keep log upon exit)      ')")
     >   lyn(kkeep_log)   
     
         write(ludsp,
     >   "(' CONF_EQUIP  ',A1,' (update info from cat)   ',$)")
     >   lyn(kconf_equip)      
     
        writE(ludsp,
     >   "(' DEBUG       ',a1,' (display debugging info)  ')")
     >    lyn(kdebug)
    
!        write(ludsp,
!     >   "(' VERBOSE    ',A1,' (output lots of info)    ')")
!     >   lyn(kverbose)

       write(ludsp, '(" VERBOSE LEVEL ", i4)') iverbose_level

       write(ludsp,'(A)')
     > "---------- Informational only -------------------------------"

        writE(ludsp, "(' SKED version:     ',a,'           ',$)")
     >     skversion
        write(ludsp,
     >    "(' Schedule file: ',A)") cskfil(1:trimlen(cskfil))


        write(ludsp,
     >   "(' FREQUENCY ',A2,' (default freq. code)      ')")
     >    ccode(icode_set_last)


        write(ludsp,"(' Process ID:  ',A5,20x,' ',$)")
     >   cpid
        write(ludsp,"(' Printer commands: ',a,', ',a)")
     >   cprtlan(1:iprl),cprtpor(1:iprp)

        write(ludsp,"(' Current yyyyddd:    ',I4,i3.3,' (',f7.2,')',$)")
     >   iyrcur(1),idacur(1),ep
        write(ludsp,"('(', I6,' MJD, ',2A2,' ',I2,' ',2A2,')')")
     >   MJDCUR(1),LDAY,IDAY,LMON

        write(ludsp,"(' Greenwich sidereal time:',3x,
     >                i2.2,':',i2.2,':',i2.2,$)") iih,iim,iis
        write(ludsp,"(' (',2(I2.2,':'),I2.2,' UT)')")
     >  ihr,imin,isec

        write(ludsp,
     >   "(' Sun''s RA and DEC:   ',I2,'h',F5.1,'m   ',I3,'d',F5.1)")
     >  IRAH,RAM,IDCH,DCM
      end if

      RETURN
      END
