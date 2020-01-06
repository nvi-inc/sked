      SUBROUTINE XLCMD(LINSTR)
C
C     XLIST processes the XLIST=ON/OFF command
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
      integer*2 LINSTR(*)
C
      include 'skcom.ftni'
C
C  LOCAL
! function
      integer istringMinMatch   !Minimum match
      character*3 lon_off
      integer trimlen 
!local
      integer*2 LKEYWD(12)
      character*22 ckeywd
      equivalence (lkeywd,ckeywd)

      integer ikey,ich,ic1,ic2,idum
      integer i2long,ichmv

      integer max_list
      parameter (max_list=16)
      character*5 list(max_list)
      data list/
     >  '?',   'AZEL','CLEAR','DUR','FLUX',  
     > 'FREQ','FEET','HA', 'LIST', 'LONG','MAX',
     > 'OFF',  'ON','SKY','SNR','WRAP'/

      integer i
C
C HISTORY
C 880314 NRV DE-COMPC'D
C 890121 NRV Moved key word checking to IGTKY
C 890502 NRV Added option for listing durations
C 890524 NRV Allow any or all az, dur, feet to be specified
C 890711 NRV Added snr option
C 891010 NRV Added KMAXL (maximum list info)
C 911112 NRV Add observed flux
C 951017 nrv Fixed gtfld call to remove linstq
C 970226 nrv Add az2 option
C 2004Feb03  JMG Modified so that XL just toggles, leaving other settings alone.
! 2005Jan04  Turn on vscan if we issue an 'SNR' command.
! 2007Jan24 JMGipson.  Changed all xlist logical flags to be kx---, ie, kwrap-->kxwrap.
!           Added kxfreq option to display frequency band.
! 2007Oct28 JMGipson.  Added sky coverage option.
! 2008Jan23 JMG. Made everything toggles. Added "?" and "CLEAR" as commands.
! 2008Jun20 JMG. Added kXlong & case construct.
! 2010Jan04 JMG. Added "list"


! Xlist without argument toggles xlist on/off
      ICH = 1
      CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)
      IF  (IC1.EQ.0) THEN  !toggle XLIST
        kxlist=.not.kxlist
        write(luscn,*) "Xlist turned ",lon_off(kxlist)
        RETURN
      END IF  !toggle XLIST
C
      DO WHILE (IC1.GT.0)
        ckeywd=" "
        IDUM = ICHMV(LKEYWD,1,LINSTR(2),IC1,IC2-IC1+1)
        ikey=iStringMinMatch(list,max_list,ckeywd)  

        IF (IKEY.LE.0) THEN !error
          write(luscn,'("Xlist: Invalid option ",a )') ckeywd
          WRITE(LUSCN,'("Xlist must be one of: ",12(1x,A))') 
     >      (list(i)(1:trimlen(list(i))), i=1,max_list)
          RETURN
        ENDIF !error


        Select case (list(ikey))
        case("?")
          write(luscn,'("List, Clear, Toggle Extended listings")')
          write(luscn,'(a)') "Usage: Xlist <option>"
          write(luscn,'(a)') "?         This screen"
          write(luscn,'(a)') "Clear     Clear all values"
          write(luscn,'(a)') "List      List values currently set"
          write(luscn,'(a)') "Off       Turn off extended listing"
          write(luscn,'(a)') "On        Turn on extended listing"
          Write(luscn,'(a)') "--otions listed below--"
          write(luscn,'(a)') "AzEl      AzEl"
          write(luscn,'(a)') "Dur       Duration"
          write(luscn,'(a)') "Flux      Fluxes by baseline"
          write(luscn,'(a)') "Freq      2-letter freq code"
          write(luscn,'(a)') "Feet      Tape footage"
          write(luscn,'(a)') "HA        Hour Angle"
          write(luscn,'(a)') "Long      Long format for AzEl"
          write(luscn,'(a)') "Max       Include cal time, procedures"
          write(luscn,'(a)') "Sky       Sky distribution info"
          write(luscn,'(a)') "SNR       SNR by baseline"
          write(luscn,'(a)') "Wrap      Include cable wrap"
        case("LIST")
          write(luscn,'("Current Xlist: ",$)')
          if(kxFeet) write(luscn,'("Feet ",$)')
          if(kxAzel) write(luscn,'("AzEl ",$)')
          if(kxLong) write(luscn,'("Long ",$)')
          if(kxAzel2)write(luscn,'("Ha-Dec ",$)')
          if(kxWrap) write(luscn,'("Wrap ",$)')
          if(kxDur)  write(luscn,'("Duration ",$)')
          if(kxSNR)  write(luscn,'("SNR ",$)')
          if(kxFreq) write(luscn,'("Freq_Code ",$)')
          if(kxobsf) write(luscn,'("Flux ",$)')
          if(kxMaxl) write(luscn,'("Max  ",$)') 
          if(kxSky)  write(luscn,'("Sky ",$)')      
          write(luscn,'()') 
        case("AZEL")
          kxAzeL = .not. kXAZel
        case("FEET")
          KxFeet = .not. KxFeet
        case("HA")
          kxazel2=.not. kxazel2
        case("CLEAR")
          KxFeet =.false.
          KxAzel =.false.
          kxwrap =.false.
          kxazel2=.false.
          kxdur  =.false.
          kxsnr  =.false.
          kxmaxl =.false.
          kxobsf =.false.
          kxfreq =.false.
          kxsky  =.false.
        case("DUR")
          kxdur  =.not. kxdur
        case("FLUX")
           kxobsf =.not. kxobsf
        case("FREQ")
           kxfreq =.not. kxfreq
        case("LONG")
          kxlong = .not. kxlong
        case("MAX")
            kxmaxl =.not. kxmaxl
        case("OFF")
          kxlist=.false.
        case("ON")
          kxlist=.true.
        case("SKY")
            kxsky  =.not. kxsky
        case("SNR")
           kxsnr  =.not. kxsnr
        case("WRAP")
          kxwrap = .not. kxwrap
        end select

C
        CALL GTFLD(LINSTR(2),ICH,i2long(LINSTR(1)),IC1,IC2)
      ENDDO
      if(kxsnr .and. .not.kvscan) then
        write(luscn,*) "Vscan was set to 'N'. Turning it on!"
        kvscan=.true.
      endif

      if(kxsky .and. .not.kopgo) then
         write(luscn,*) "Calculating sky coverage!"
         call opfill
      endif

      RETURN
      END
