      SUBROUTINE SKINI

C
C  SKINI initializes many of the common variables used in SKED
C
      use group_mod  ! module containing GROUP definitions and routines
!      use twin_mod   ! module containing TWIN_TELESCOPES definitions and routines

      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni'
      include 'major.ftni'
      include 'minor.ftni'
      include 'downtime.ftni'
      include 'covar.ftni'
      include 'cat_src.ftni'
      include 'cat_stat.ftni'
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'
      include 'cat_rx.ftni'
      include 'cat_loif.ftni'
      include 'cat_rec.ftni'
      include 'cat_track.ftni'
      include 'flux.ftni'
      include 'pixelation.ftni'
      include 'statwt.ftni'
      include 'srcwt.ftni'
      include 'cat_name_version.ftni' 
! Now most recent at top.
!
! 2019Sep03  JMG: 1) Added implicit none:  2) initialize kcat_freq=.false. 
!
C
C  891010 NRV Changed default value of KXLIST, KDUR to TRUE.
C             Added KMAXL.
C  891110 NRV Added values for new lus.
C  891128 NRV Removed initialization of ISNRBL to SKINI.
C  891129 gag removed ISTSC1,ISTSC2,ISNRSC,IBWSC, and ICHSC
C  900206 gag removed mxfeet
C  900413 nrv removed initialization of PRN (printer names)
C  920529 nrv added iminbetween
C  930513 nrv Changed IMTPTM to 10 seconds (MIDTP)
C  950329 nrv Add lucat1
C  MODIFIED:   NRV 880314 DE-COMPC'D
C              NRV 890531 Added initialization of CSKFIL to temp name
C              gag 900216 removed cskfil to scctl
C              NRV 911026 Add new flux initialization
C              NRV 920527 Add SEFD parameter initialization
C  nrv 920706 added radd_noise
C  nrv 930219 merge sked/autosked
C  nrv 930225 implicit none
C  nrv 930708 initialize head position arrays
C  nrv 930803 initialize nsorobs
C  nrv 931110 initialize st0cur, tris,tset etc.
C  nrv 940620 initialize ivix,trkn,itras
C  nrv 950710 initialize itras to -99, not 0
C 951018 nrv Initialize with char2hol instead of equivalences
C 951019 nrv Remove kswitch, nchanv. Change subscripts on ivix. Change
C            14->max_chan
C 960307 nrv Move block data initialization into here.
C 960409 nrv Initialize itra2
C 960412 nrv Initialize IRCUR so that sked doesn't think it has
C            observations when a new schedule is started.
C 960610 nrv Move freqs.ftni initialization of large arrays to frinit.
C 960923 nrv ITEARL array
C 970321 nrv Removed numerous duplicate initializations
C 970226 nrv Add KAZEL2
C 970307 nrv Get window size.
C 970326 nrv Initialize XNEW variables
C 971006 nrv Don't need prepass as default any more.
C 990412 nrv Remove maxscn(i) and set MAXSCN=784.
C 990520 nrv Initialize description, correlator, pi variables.
C 990524 nrv Initialize tape_length, tape_dens
C 990920 nrv Initialize rack and recorder type names.
C 990921 nrv Call skdrini first to set variables used by both programs.
C 991006 nrv Initialize lnahdpos, needed for writing VEX names.
C 991108 nrv Initialize kcatalog
C 000405 nrv Initialize kparam
C 010622 nrv Move roll_def initialization from skdrini because 
C            these are needed only for sked to write VEX files.
C 020111 nrv Move roll_def back to skdrini because drudg needs
C            the tables too.
! 2004Feb16 JMG Added Downtime initialize.
! 2005Nov22 JMGipson. Modified to indicate that none of the catalogs have been read.
! 2007Jan24 JMGipson.  Changed all xlist logical flags to be kx---, ie, kwrap-->kxwrap.
!           Added kxfreq option to display frequency band.
!           Initializied all of the xlist flags.
!           Initialize Icode_set_last to 1.
! 2007Jul02 JMG. Added flux.ftni which was separated from sourc.ftni
! 2009Jul15 JMG. Changed default minsubnetsize to 2.
! 2009Dec11 JMG. changed default modtime to 1 from 10
! 2010Jan25 JMG. Initialize pixelation. 
! 2010Mar26 JMG. Changed stutcm ->utstcm, enutcm->utencm for consistency with jdstcm and jdencm
! 2013Apr23 JMG. Added code for broadband command.
! 2013Sep13 JMG. Removed initialization of minimum sun distance which is done in major_init.
! 2015Mar17 JMG. Added Mark6_off
! 2017Apr18 JMG. Icalde changed from 5 to 10 
! 2017Oct06 KLB. Added kconf_equip
! 2020Jun08 JMG. Reference to broadband.ftni. Initialize bb_off
! functions
      integer fc_gwinsz,fc_gwinw

C LOCAL
      integer ic,ix,ib,is,j,iv,i,iba,ibl,ist,ip

      double precision dang_max
      double precision elvdef
      ELVDEF = 0.08726

! Indicate none of the catalogs have been read.
      kcat_stat =.false.
      kcat_src  =.false.
      kcat_mode =.false.
      kcat_rx   =.false.
      kcat_loif =.false.
      kcat_rec  =.false.
      kcat_track=.false.
      kcat_freq =.false.
   
      call skdrini
      KTMLIN=.FALSE.
    
 ! Xlist defaults
      KXLIST=.TRUE.
      KxAZEL=.FALSE.
      KxAZEL2=.FALSE.
      KxDUR=.TRUE.
      KxLong=.false.
      KxFEET=.FALSE.
      kxfreq=.false.
      kxmaxl=.false.
      kxobsf=.false.
      kxsnr=.false.
      kxwrap=.false.
      kxsky =.false.
!
      kxnewflux=.false.
      kxnewsnr=.false.
      kxnewsefd=.false.
      kxnewbase=.false.
      kvscan=.true.
      kflux=.false.
      KASK=.TRUE.
      KASNR=.TRUE.
      kparam=.false.
      kconf_equip=.false.

! Catalog version defaults
      lmodes_cat_version = "unknown"  
      lfreq_cat_version = "unknown" 
      lrx_cat_version = "unknown"
      lrec_cat_version = "unknown"
      lhdpos_cat_version = "unknown"
      lloif_cat_version = "unknown"
      ltracks_cat_version = "unknown"

      lsource_cat_version = "unknown"
      lflux_cat_version = "unknown"

      lstation_cat_version = "unknown"
      lantenna_cat_version = "unknown"
      lequip_cat_version = "unknown"
      lposition_cat_version = "unknown"
      lmask_cat_version = "unknown"

      lmodes_cat_use = "unknown"  
      lfreq_cat_use  = "unknown"
      lrx_cat_use = "unknown"
      lrec_cat_use = "unknown"
      lhdpos_cat_use = "unknown"
      lloif_cat_use = "unknown"
      ltracks_cat_use = "unknown"

      lsource_cat_use = "unknown"
      lflux_cat_use = "unknown"

      lstation_cat_use = "unknown"
      lantenna_cat_use = "unknown" 
      lequip_cat_use = "unknown"
      lposition_cat_use = "unknown"
      lmask_cat_use = "unknown"

!
      NumInGroupList=0

!      num_twins=0

      LOOKAH=1200
      ISORTM=5
   
      ITAPTM=1
      nhorz=0
      azhorz=0
      elhorz=0

      iwscn = fc_gwinw()
      if (iwscn.lt.1.or.iwscn.gt.999) IWSCN=79
      ihscn = fc_gwinsz()
      if (ihscn.lt.1.or.ihscn.gt.999) ihscn=24
      ITSYNC=20
      MINSCN=90
      lulog=70
      LUFIL=90
      lusel=40
      lucat=30
      lutmp=80
      luskd=50
      lutm2=60 
      ifill_off  = 0 
      imark6_off  = 0 
C     *NOTE* ABOVE WE MADE MXFEET=ITSKIP, EFFECTIVELY DISABLING THE
C     "CAN'T WASTE TAPE" ERRORS.
C     FILE NAMES AND CARTRIDGES
C     The number of LUs with cursor sensing
      IBLEN=IBUF_LEN
      RASUN=0.0
      DECSUN=0.0

C  1. Variable initialization
C
C  File format
C  Experiment name, description, scheduler, and correlator
      cexper=" "
      cexperdes='tbd'
      cpiname='tbd'
      ccorname='tbd'
C  Logical switches
      KNEWSK = .FALSE.
      KNEWSO = .FALSE.
      KNEWST = .FALSE.
      KNEWFR = .FALSE.
      KNEWFL = .FALSE.
      KNEWPA = .FALSE.
      KNEWFI = .FALSE.
      knewop = .false.
      krsini = .false.
      kauto = .false.
      kopgo = .false.
      knov = .false.

      kdebug=.false.
      kkeep_log=.false.

      statwt=0.d0
      srcwt=0.d0
      bb_bw=0.d0
      idata_mbps=0
      isink_mbps=0
      ibb_off =0 
      iMark6_off = 0 

C  Current variables
      nstncur = 0
      num_down=0
C
C
      icode_set_last=1  !set to first frequency code.
      DO  I=1,MAX_STN   ! Initialize current variables
        isubst(i)=0
        STNELV(I) = ELVDEF
        ISTCUR(I) = 0
        MJDCUR(I) = 0
        IYRCUR(I) = 0
        IDACUR(I) = 0
        UTCUR(I) = 0.D0
        GSTCUR(I) = 0.D0
        st0CUR(I) = 0.D0
        NSORcur(I) = 0
        LCBLcur(I) = 0
        ICALcur(I) = 0
        IDURcur(I) = 0
        IDLCUR(I) = 0
        ICODcur(I) = 0
        IFTCUR(I) = 0
        ITUCUR(i) = 1
        MAXPAS(I) = 1
        ireccur(i)=0
        cprecur(i)=" "
        cmidcur(i)=" "
        cpstcur(i)=" "
      END DO  ! Initialize current variables
      NSTNtst = 0
      DO  I=1,MAX_STN   ! Initialize current variables
        ISTtst(I) = 0
        MJDtst(I) = 0
        IYRtst(I) = 0
        IDAtst(I) = 0
        UTtst(I) = 0.D0
        GSTtst(I) = 0.D0
        st0tst(I) = 0.D0
        NSORtst(I) = 0
        LCBLtst(I) = 0
        ICALtst(I) = 0
        IDURtst(I) = 0
        IDLtst(I) = 0
        ICODtst(I) = 0 
        IFTtst(I) = 0
        itutst(i) = 0
        cpretst=" "
        cmidtst=" "
        cpsttst=" "
      END DO  ! Initialize current variables
C  Number of selected sources, stations, codes
      NSOURC = 0
      NSTATN = 0
      nsubst = 0
      NCODES = 0
      do ic=1,max_frq
        do is=1,max_stn
          do iv=1,max_chan
            ibbcx(iv,is,ic)=0
            freqrf(iv,is,ic)=0.d0
          enddo
          do i=1,max_band
            trkn(i,is,ic)=0.0
            ntrkn(i,is,ic)=0
            nfreq(i,is,ic)=0
          enddo
        enddo
        lcode(ic)=0
        do ix=1,max_band
          do is=1,max_stn
            wavei(ix,is,ic) = 0.0
            bwrms(ix,is,ic) = 0.0
          enddo
        enddo
      enddo
      NCELES = 0
      NSATEL = 0
      nband = 0
      do ib=1,max_band
        do is=1,max_sor
          nflux(ib,is)=0
          cfltype(ib,is)=' '
          do j=1,max_flux
            flux(j,ib,is)=0.0
          enddo
        enddo
      enddo
      do iba=1,max_band
        imarg(iba)=0
        imarg_1(iba)=0
        imarg_ast(iba)=0
        do ibl=1,max_baseline
          isnrbl(iba,ibl)=0
          isnrbl_1(iba,ibl)=-1
          factbl(iba,ibl)=0.0
          projbase(ibl)=0.d0
        enddo
        do ist=1,max_stn
          nsefdpar(iba,ist)=0
          do ip=1,max_sefdpar
            sefdpar(ip,iba,ist)=0.0
          enddo
        enddo
      enddo
      dnorm_tri=0.0
      dnorm_inv=0.0
C  Command variables
      JDSTCM = 0
      UTSTCM = 0.D0
      JDENCM = 0
      UTENCM = 0.D0

      NOBSCM = 0
      NOBS = 0
      ircur=0
      NXTREC = 1
C
! Pixel variabls
      num_pix_bands=3
      ipix_bands(1)=1
      ipix_bands(2)=4
      ipix_bands(3)=8
      dang_max=(90.d0-3.d0)*deg2rad
      call init_sphere_pix_ang(dang_max, num_pix_bands, ipix_bands,
     >  dang_pix_band)
       kinit_pixel=.false.                       !set this to false. 
 
! End pixel
      eleva=-99.d0
      azimu=0.d0


C Opt variables
      kOptBySky=.false.
      num_est=0
      do i=1,max_dim_esti
        do j=1,2
          lpara(i,j)=.false.
        enddo
      enddo
      cov=0.d0
      covs=0.d0
      kexpand=.false.
      rloel = 0
C
      ICALDE = 10      !2017Apr18   changed from 5 to 10 
      IDURDE = 196
      IDLDEF = 0
      cprede="PREOB"
      cmidde="MIDOB"
      cpstde="POSTOB"
    
      ITSKIP = 17640
      IMINTM = 0
      IMODTM = 1      !changed from 10 to 1
      ISETTM = 20
C     For VLBA, allow 45s+2s/track. For 32 tracks this is 64+45=109.
C     For FS 9.4 standard is 100 sec.
      IPARTM = 100
      ITSYNC = 20
      MINSCN = 90
      MAXSCN = 784
      MODSCN = 10 
      minsubnetsize = 2
      numsubnet = 1

C     *NOTE* ABOVE WE MADE MXFEET=ITSKIP, EFFECTIVELY DISABLING THE
C     "CAN'T WASTE TAPE" ERRORS.
C     FILE NAMES AND CARTRIDGES
C     The number of LUs with cursor sensing
      IBLEN = IBUF_LEN
      
C     INITIALIZE PROCEDURE FLAGS TO YYNN
C     NRV 971006 Don't need prepass as default any more.
      KFLG(1) = .TRUE.
      KFLG(2) = .TRUE.
      KFLG(3) = .FALSE.
      KFLG(4) = .FALSE.
      do i=1,max_sor
        NOBSSO(i) = 0
        do j=1,max_baseline
          nsorobs(i,j)=0
        enddo
        UTPRSO(i)=0.D0
        MJPRSO(i)=0
        ISSCAN(i)=idurde
      end do
!

C
      RETURN
      END
C
