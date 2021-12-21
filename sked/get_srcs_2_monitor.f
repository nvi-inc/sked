      subroutine monitorsources(cmdline)

!   Choose sources to monitor.
!   2006May31 Check that duration of experiment is not longer than 1 day.
!   2009Nov16 Wasn't properly setting default value to 10

      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'

C
! functions
      integer julda
      integer trimlen

! select sources to monitor
! passed
      character*(*) cmdline

! local
      integer*2 ilen
      integer Num2Monitor
      integer mjd_start,mjd_end

! Can have upto 2 arguments

      ilen=trimlen(cmdline)
      if(ilen .eq. 0) then
         write(luscn,*) "MonitorSources: # not specified. Using 10"
         Num2Monitor=10
         goto 10
      endif

      read(cmdline,*,err=5)Num2Monitor
      goto 10

5     continue
      read(cmdline,*,err=6) Num2Monitor
      goto 10


6     continue
      write(luscn,*) "MonitorSources: Error reading #. Aborting"
      return

10    continue
!     ida_start is DOY, which is why the month is set to 1.

! Start and end date.

      MJD_start=JULDA(1,IDA_start,IYR_start-1900)
      MJD_end=JULDA(1,IDA_end,IYR_end-1900)

      if(MJD_end-MJD_start .gt. 1) then
         write(*,*) "MonitorSources:  Experiment duration too long."
         write(*,*) "                 Max is 1 day."
         return
      endif

      call get_srcs_2_monitor(Num2Monitor,cexper(1:3),mjd_start)

      return
      end
!*****************************************************************************
      subroutine get_srcs_2_monitor(Num2Monitor,cexptype,mjd_in)
! open source database, and find out which sources need to be monitored.
! History
!    V1.01  Have two periods Year, and last quarter.
!           If a source has been observed in last quarter, don't observe again.
! 2007Jul02 JMG  added astro.ftni (split off from sourc.ftni')
! 2011Feb16 JMG. If Num2Monitor is <0, then write list of monitored sources to file.
! 2019Jun06 JMG. USe non-NR gdate. 

! On entry
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'astro.ftni'

! passed
      integer Num2Monitor       !number of sources to include.
      character*(*) cexptype
      integer mjd_in         !date to start monitoring

! local variables.
      integer NumSrc,MaxSrc,isrc
      parameter (MaxSrc=1000)

      character*8 lsrc_name(MaxSrc)
      integer iexp_Src_want(MaxSrc)
      integer iexp_src_got_yr(MaxSrc)
      integer iobs_src_got_yr(MAxSrc)
      integer iexp_src_got_qtr(MaxSrc)
      integer iobs_src_got_qtr(MAxSrc)

      integer iyear_beg,imonth_beg,iday_beg
      integer iyear_end,imonth_end,iday_end

      double precision src_ra2k(MaxSrc)  !J2000 RA and Dec
      double precision src_dec2k(MaxSrc)
      double precision src_ra(MaxSrc)    !ra and dec of sources at epoch
      double precision src_dec(MaxSrc)

      double precision obs_ratio(MaxSrc)
      double precision src_rank(Max_Sor)
      integer ikey(MaxSrc)
      integer iBestSrc(MaxSrc)
      integer NumNotFound
      integer ierr

      integer iobs_threshold/10/
      logical kdisplay
      integer julian
      double precision dnotFound/-9999./
      integer NumCover
      character*128 lfluxcat
      double precision TJD
      integer iflux_type(max_band,max_sor)

      character*8 lfluxtype(2)/"Found","Default"/
      real flux_default(2)
      integer ib
      integer i,j
      real stride
      integer irank_mode
      integer NumKeep
      integer NumUnderObs
      integer mjd

! start with a fresh schedule
      if(Num2Monitor .gt. 0) then
         call delete_all_obs()
      endif 

      kdisplay=.false.
! convert mjd into year, month, day.

! This bit of code (and below, also .false.) gives an indication of the success
! of the program.

      mjd=mjd_in+30          !Number of days to look ahead.
                             !Suppose it is now June 5, and we are scheduling for June 12
                             !Suppose a schedule is out for June 21 with some monitored source.
                             !Without this factor of 21, there is a chance we will pick up the monitored sources.
                             !
      julian=mjd+2440000
      call gdate(julian,iyear_end,imonth_end,iday_end)

      if(cexptype .eq. "RDV") then
        write(*,*) "Getting number of obs over last 2 years"
        julian=mjd+2440000-730
      else if(cexptype(1:2) .eq. "RD") then
        write(*,*) "Getting number of obs over last 2 years"
        julian=mjd+2440000-730
      else
        write(*,*) "Getting number of obs over last year"
        julian=mjd+2440000-365
      endif
      call gdate(julian,iyear_beg,imonth_beg,iday_beg)

      call get_src_obs_dens_mysql(cexptype,
     > iyear_beg,imonth_beg,iday_beg,iyear_end,imonth_end,iday_end,
     > lsrc_name,iexp_src_want,iexp_src_got_yr,iobs_src_got_yr,
     > NumSrc,MaxSrc)

      write(*,*) "Getting number of obs over last qtr"
      julian=mjd+2440000-91
      call gdate(julian,iyear_beg,imonth_beg,iday_beg)

      call get_src_obs_dens_mysql(cexptype,
     > iyear_beg,imonth_beg,iday_beg, iyear_end,imonth_end,iday_end,
     > lsrc_name,iexp_src_want,iexp_src_got_qtr,iobs_src_got_qtr,
     > NumSrc,MaxSrc)
      write(*,*) "Getting position"

      call get_src_pos_mysql(lsrc_name,src_ra2k,src_dec2k,NumSrc,
     > NumNotFound)
      write(*,*) "Got position"
      if(NumNotFound .gt. 0) then
         write(*,*) "Did not find source positions for the following: "
         do isrc=1,NumSRc
           if(src_ra(isrc)  .eq. dNotFound .or.
     >       src_dec(isrc)  .eq. dNotFound) then
             write(*,*) lsrc_name(isrc)
           endif
         end do
      endif

! Goal. Observe Geodetic srcs 6 times in last 2 years, others 2times.
      if(cexptype .eq. "RDV") then
        do isrc=1,NumSrc
          if(iexp_src_want(isrc) .eq. 12) iexp_src_want(isrc)=6
        end do
      else if(cexptype(1:2) .eq. "RD") then
        write(*,*) "RD kludge. If you see this please tell JGipson"
        do isrc=1,NumSrc
          iexp_src_want(isrc)=4
        end do     
      endif

      if(Num2Monitor .le. 0) then
        write(*,*) "Writing monitor list in mon_stat.tmp"
        open(1,file="mon_stat.tmp")
        if(cexptype .eq. "RDV") then
          write(1,*) "    Name      Want  Exp_2Y Obs_2y "//
     >     " Exp_Q  Obs_Q  HA   DEC"
        else
           write(1,*) "    Name     Want  Exp_Y  Obs_Y  "//
     >     " Exp_Q  Obs_Q  HA   DEC"
        endif

        do isrc=1,NumSrc
          write(1,'(i3,1x,a,5(1x,i6),2(1x,f5.1))')
     >    isrc, lsrc_name(isrc),iexp_src_want(isrc),
     >    iexp_src_got_yr(isrc), iobs_src_got_yr(isrc),
     >    iexp_src_got_qtr(isrc),iobs_src_got_qtr(isrc),
     >    src_ra2k(isrc)*rad2ha,src_dec2k(isrc)*rad2deg
        end do
        close(1)
        return
      endif


      write(*,*) "Converting to epoch"
! convert from J2000 to epoch.
      tJd=mjd_in+2440000.d0
      do isrc=1,NumSrc
         call apstar_Rad(tjd,src_ra2k(isrc),src_dec2k(isrc),
     >      src_ra(isrc),src_dec(isrc))
      end do

! 1. Rank sources by observing threshold.

! now stuff the info we got into sked arrays.
      NSourc=0
      Nceles=NumSrc
      obs_ratio=0.

      obs_ratio=0.
      NumUnderObs=0
      do isrc=1,NumSrc
        if(iexp_src_want(isrc) .gt. 0 .and.
     >     iexp_src_got_qtr(isrc) .eq. 0) then
           obs_ratio(isrc)=float(iexp_src_got_yr(isrc))/
     >                          float(iexp_src_want(isrc))

          if(iobs_src_got_yr(isrc) .lt. 10) obs_ratio(isrc)=0.  !less than 10 obs same as 0.
! Quick fix to pick up under observed sources in RDs
      
          if(obs_ratio(isrc) .lt. 1) then
            NumUnderObs=NumUnderObs+1
          endif
        else
          obs_ratio(isrc)=500.
        endif
         
      end do

! OK, didn't find enough. Loosen the constraints.
      if(NumUnderObs .lt. Num2Monitor*1.5) then
        NumUnderObs=0
        do isrc=1,NumSrc
          if(iexp_src_want(isrc) .gt. 0) then
             obs_ratio(isrc)=float(iexp_src_got_yr(isrc))/
     >                          float(iexp_src_want(isrc))
            if(iobs_src_got_yr(isrc) .lt. 10) obs_ratio(isrc)=0.  !less than 10 obs same as 0.
            if(obs_ratio(isrc) .lt. 1) then
              NumUnderObs=NumUnderObs+1
            endif
          else
            obs_ratio(isrc)=500.
          endif
        end do
      endif

      if(NumUnderObs .eq. 0) then
         write(luscn,'(a)') "No underobserved sources found!"
         return
      endif

! At this point obs_ratio contains #obs/#want.
!    If this is less than 1, then source underobserved.

! This sorts into increasing order.
      call indexx8(NumSrc,obs_ratio,ikey)

! We do the following because it might happen that the
! sources are in some kind of order, .e.g, RA.  This makes sure we sample them all.
! Two options: number underobserved is less than 100, rank them all.
      if(NumUnderObs .lt. 100) then
        stride=1
        Nsourc=NumUnderObs
      else
        stride=float(NumUnderObs)/100.
        Nsourc=100
      endif

      nceles=Nsourc
! OK, have picked a set of underobserved sources.
      do i=1,Nsourc
        j=nint((i-1)*stride)+1
        isrc=ikey(j)
        csorna(i)=lsrc_name(isrc)
        sorp_now(1,i)=src_ra(isrc)
        sorp_now(2,i)=src_dec(isrc)
        sorp2000(1,i)=src_ra2k(isrc)
        sorp2000(2,i)=src_dec2k(isrc)
        call getiauname(ciauna(i),sorp2000(1,i),sorp2000(2,i))
      end do
      call replace_sksrc()

! Read in fluxes   from source catalog if we have them.
! Else use default values.
      lfluxcat=" "
      flux_default=0.3          !.3 Janskys is conservative guess.
      call get_new_fluxes(lfluxcat,iflux_type,flux_default)

! Now
      call rsini            !now calculate the rise set time of all sources

      irank_mode=3
      if(.not.kvscan) then
         write(*,*) "Get_Srcs_2_monitor: Turning on VSCAN!"
         kvscan=.true.
      endif
      call ranksources(irank_mode,src_rank)
! Pick the best sources out of list.
      if(Num2Monitor .lt. Nsourc) then
        NumCover=3         !
        NumKeep=0
        call FindBestSources(sorp_now,NSourc,NumKeep,src_rank,
     >    Num2Monitor,NumCover,iBestSrc,luscn)

        call Keep_Some_Srcs(iBestSrc,Num2Monitor,csofil,ierr)
        Nsourc=Num2Monitor
      else if(Nsourc .gt. 0) then
        write(*,*) "Can only find ",Nsourc, " sources to monitor."
      else
        write(*,*) "No more sources to monitor!"
        return
      endif
      Nceles=Num2Monitor

! Initialize all fluxes to not-found, and re-get them.
      lfluxcat=" "
      call get_new_fluxes(lfluxcat,iflux_type,flux_default)

! Write out the sources, together with the fluxes.
      if(luscn .ne. 0)   write(luscn,'(a)')
     >   "   # Source        RA    Dec   FluxX    FluxS"

      do isrc=1,Nsourc
        rmin_astro(isrc)=1./100.
        rmax_astro(isrc)=1.5/100.
        if(luscn .ne.0) write(*,'(i4,1x,a,1x,2f8.2,2(1x,a8))') isrc,
     >      csorna(isrc)(1:8), sorp_now(1,isrc)*rad2ha,
     >      sorp_now(2,isrc)*rad2deg,
     >      (lfluxtype(iflux_type(ib,isrc)),ib=1,nband)
      end do

      kastro=.true.
    
      end
!*****************************************************************************
      subroutine chksrcvis(isrc,kdisplay,lu,ierr)
! This finds the rise and set time at all stations, and sees if there is some overlap.
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'

      integer isrc
      logical kdisplay
      integer lu
      integer ierr

      integer istn(Max_stn)
      integer i,j,k
! from mutual visibility
      integer iutris(max_stn),iutset(max_stn)
      integer mutris,mutset

! line segments for rise and set times.
      integer LineSegJ(2,2),LineSegK(2,2),numj,numk
      integer LineSeg(2),j2,k2
      integer intTime


      do i=1,Nstatn
        istn(i)=i
      end do

      call visss(isrc,nstatn,istn,iutris,iutset,mutris,mutset)
      ierr=-1        !default is no mutual visibility.

! see if overlap between rise and set times.
! this code borrowed from rsini.
      do j=1,nstatn-1
         if(iutset(j) .eq. 0) goto 110  !source is not up. skip loop.
         call MakeLineSegs(iutris(j),iutset(j),LineSegJ,NumJ)
         do k=j+1,nstatn
           if(iutset(k) .eq.0) goto 100  !source is not up. skip loop.
           call MakeLineSegs(iutris(k),iutset(k),LineSegK,NumK)
           do j2=1,NumJ
           do k2=1,NumK
              call FindSegOverlap(LineSegJ(1,j2),LineSegK(1,k2),
     >            LineSeg)
              intTime=LineSeg(2)-LineSeg(1)
              if(intTime .gt. 10) then    !assume we need at least 10 minuts of overlap
                ierr=0
                return
              endif
            end do
            end do
100     continue
        end do  !k loop
110   continue
      end do    !j loop
      return
      end
!******************************************************************************
      subroutine get_src_obs_dens_mysql(cexptype,
     > iyear_beg,imonth_beg,iday_beg,iyear_end,imonth_end,iday_end,
     > lsrc_name,iexp_src_want,iexp_src_got,iobs_src_got,NumSrc,MaxSrc)
! 1.) Get observing density we want (exp_src_want) and lsrc_name.
!     Also get number of experiments and # of good observatins on these sources
!     since iyear,imonth,idate

! 2006Jun13 JMGipson.  Changed location of common.
! 2020Jun04 JMG. Changed it back. 

      implicit none
      include "mysql_common.i"          !common area.
! functions
      integer iwhere_in_string_list
      integer trimlen
! mysql functions
      integer imysql_init
      integer imysql_real_connect
      integer imysql_query
      integer imysql_num_rows
      integer imysql_num_fields
      integer imysql_error
      integer imysql_errno
      integer imysql_store_result
      integer imysql_fetch_row
      integer imysql_free_result
      integer imysql_close

! passed
      character*3 cexptype                   !type of experiment to consider
      integer iyear_end,imonth_end,iday_end   !
      integer iyear_beg,imonth_beg,iday_beg   !

      integer MaxSrc            ! maximum number of sources
! returned
      integer NumSrc
      character*8 lsrc_name(MaxSrc)
      integer iexp_src_want(MaxSrc)
      integer iexp_src_got(MaxSrc)
      integer iobs_src_got(MaxSrc)

! local
      character*8 lsrc_name_tmp
      integer isrc

! local variables
      integer ierr
      integer NumRow, irow
      integer NumFields
! stuff for VLBI observations.
      double precision dnotFound/-9999./

      character*50 ldate_clause
      character*25 lname_clause
      character*100 lsuffix
      character*74 lprefix
      integer nch
      integer itemp
      integer j

      character*1 lq
      lq='"'

! Initialize database
      ierr=imysql_init(MySQLHandle)
      if(ierr .ne. 0) then
         write(*,*) "imysql_init error: ",ierr
      endif

! connect to vlbi database.   

      ierr=imysql_real_connect(MySQLHandle,lmysql_host,lmysql_user,
     >    lmysql_password,
     >    lmysql_db, iport_mysql,lmysql_socket,iclient_flag)
      if(ierr .ne. 0) then
        write(*,*) "imysql_real_connect error: ",ierr
        ierr=imysql_error(MySQLHandle)
        goto 900
      endif

! Execute the query to get the SQL names.
      lsqlquery="Select src_name, obsdens from srcobsdens;"//char(0)
      ierr=imysql_query(MySQLHandle,lSQLquery)
      if(ierr .ne. 0) then
        write(*,*) "imysql_query: ",ierr
        ierr=imysql_error(MySQLHandle)   !write it out
        ierr=imysql_errno(MySQLHandle)
        write(*,*) "SQL error #",ierr
        goto 900
      endif

      ierr=imysql_store_result(MySQLHandle,MySQLRes)
      if(ierr .ne. 0) then
        write(*,*) "istore_resut: ",ierr
        goto 890
      endif

      NumRow=imysql_num_rows(MySQLRes)
      Numfields=imysql_num_fields(MySQLRes)

      do irow=1,NumRow
        lfield=" "
        ierr=imysql_fetch_row(MySQLRes,lfield)
        call fix_mysql_fields(lfield,NumFields)

        lsrc_name(irow)=lfield(1)
        read(lfield(2),'(i5)') iexp_src_want(irow)
      end do
      ierr=imysql_free_result(MySQLRes)
      numSrc=NumRow
 
!Now get ready for reading in # of good and scheduled obs.
      write(ldate_clause,'(2(a,a,i4,"-",i2.2,"-",i2.2,a))')
     > ' and date >= ',lq, iyear_beg,imonth_beg,iday_beg,lq,
     > ' and date <= ',lq, iyear_end,imonth_end,iday_end,lq

      lname_clause=" "
      if(cexptype .eq. "RDV") then
        lname_clause=' and code like '//lq//cexptype//'%'//lq
      endif
      nch=trimlen(lname_clause)
      lsuffix=ldate_clause//lname_clause(1:nch+1)//
     > ' group by src_name;'//char(0)

! Go through this loop twice.
! First time for sessions which have been correlated, use the actually number of good.
! Second time for sessions which have  not been, use # scheduled.
        iexp_src_got=0
        iobs_src_got=0

      do j=1,2
        if(j .eq. 1) then
         lprefix='Select src_name,count(*),sum(obs_good) from srcexp '
     >          //'where obs_good>0 '
        else
         lprefix='Select src_name,count(*),sum(obs_sked) from srcexp '
     >         //'where obs_good is null '
        endif
        lsqlquery=lprefix//lsuffix

        ierr=imysql_query(MySQLHandle,lSQLquery)
        if(ierr .ne. 0) then
          write(*,*) "imysql_query: ",ierr
          ierr=imysql_error(MySQLHandle)   !write it out
          ierr=imysql_errno(MySQLHandle)
          write(*,*) "SQL error #",ierr
          goto 900
        endif
        ierr=imysql_store_result(MySQLHandle,MySQLRes)
        if(ierr .ne. 0) then
          write(*,*) "istore_result: ",ierr
          goto 890
        endif

        NumRow=imysql_num_rows(MySQLRes)
        Numfields=imysql_num_fields(MySQLRes)


        do irow=1,NumRow
          lfield=" "
          ierr=imysql_fetch_row(MySQLRes,lfield)
          call fix_mysql_fields(lfield,NumFields)
          lsrc_name_tmp=lfield(1)
          isrc=iwhere_in_string_list(lsrc_name,numSrc,lsrc_name_tmp)
          if(isrc .ne. 0) then
             read(lfield(2),'(i5)') itemp
              iexp_src_got(isrc)=iexp_Src_got(isrc)+itemp
             read(lfield(3),'(i5)') itemp
              iobs_src_got(isrc)=iobs_src_got(isrc)+itemp
          endif
        end do
        ierr=imysql_free_result(MySQLRes)
      end do
      goto 900

890   continue
      ierr=imysql_free_result(MySQLRes)

900   continue
      ierr=imysql_close(MySQLHandle)
      end subroutine
!******************************************************************************
      subroutine fix_mysql_Fields(lfield,NumFields)
! subroutine to get rid of NULL at the end of the fields
! passed
      integer NumFields
      character*(*) lfield(NumFields)
! local
      integer i         !loop index
      integer nch

      do i=1,NumFields
        nch=index(lfield(i),char(0))
        if(nch .ne. 0) lfield(i)(nch:)=" "
      end do
      return
      end subroutine
!******************************************************************************
      subroutine get_src_pos_mysql(lsrc_name,src_ra,src_dec,NumSrc,
     >  NumNotFound)
! 1.) Get observing density we want (exp_src_want) and lsrc_name.
!     Also get number of experiments and # of good observatins on these sources
!     since iyear,imonth,idate

! 2006Jun13 JMGipson.  Changed location of common.
! 2020Nov04 JMipson. Changed it again.

      implicit none
      include "mysql_common.i"          !common area.
      include "../skdrincl/constants.ftni"
! functions
      integer iwhere_in_string_list

! mysql functions
      integer imysql_init
      integer imysql_real_connect
      integer imysql_query
      integer imysql_num_rows
      integer imysql_num_fields
      integer imysql_error
      integer imysql_errno
      integer imysql_store_result
      integer imysql_fetch_row
      integer imysql_free_result
      integer imysql_close

! passed
      integer NumSrc                    !Number
      character*(*) lsrc_name(NumSrc)   !name
! returned
      double precision src_ra(NumSrc)   !RA and DEc. if not found, set to -99.
      double precision src_dec(NumSrc)
      integer NumNotFound               !number for which we don't have positions.

! local
      integer isrc
      character*8 lsrc_name_tmp

! local variables
      integer ierr
      integer NumRow, irow
      integer NumFields
      double precision dnotFound/-9999./
      character*1 lq
      lq='"'


      NumNotFound=0.
! Initialize database
      ierr=imysql_init(MySQLHandle)
      if(ierr .ne. 0) then
         write(*,*) "imysql_init error: ",ierr
      endif

! connect to vlbi database.
      ierr=imysql_real_connect(MySQLHandle,lmysql_host,
     >    lmysql_user,lmysql_password,
     >    lmysql_db, iport_mysql,lmysql_socket,iclient_flag)
      if(ierr .ne. 0) then
        write(*,*) "imysql_real_connect error: ",ierr
        ierr=imysql_error(MySQLHandle)
        goto 900
      endif

      src_ra =dNotFound   !initialize to not found values.
      src_dec=dNotFound

! Get the positions all at once.
      lsqlquery="Select src_name,src_ra,src_dec from srcpos"//char(0)
      ierr=imysql_query(MySQLHandle,lSQLquery)
      if(ierr .ne. 0) then
        write(*,*) "imysql_query: ",ierr
        ierr=imysql_error(MySQLHandle)   !write it out
        ierr=imysql_errno(MySQLHandle)
        write(*,*) "SQL error #",ierr
        goto 900
      endif
      ierr=imysql_store_result(MySQLHandle,MySQLRes)
      if(ierr .ne. 0) then
        write(*,*) "istore_resut: ",ierr
        goto 890
      endif

      NumRow=imysql_num_rows(MySQLRes)
      NumFields=imysql_num_fields(MySQLRes)

      do irow=1,NumRow
        ierr=imysql_fetch_row(MySQLRes,lfield)
        if(ierr .ne. 0) then
           write(*,*) "ifetch_row error: ",ierr
           goto 890
        endif
        call fix_mysql_fields(lfield,NumFields)
        lsrc_name_tmp=lfield(1)(1:8)
        isrc=iwhere_in_string_list(lsrc_name,numSrc,lsrc_name_tmp)   !See if this is one of the sources we are looking for
        if(isrc .ne. 0) then
          read(lfield(2),*,err=90) src_ra(isrc)
          read(lfield(3),*,err=90) src_dec(isrc)
        endif
90      continue
      end do
      NumNotFound=0
      do isrc=1,NumSrc
         if(src_ra(isrc)  .eq. dNotFound .or.
     >      src_dec(isrc) .eq. dNotFound) then
             NumNotFound=NumNotFound+1
         write(*,*) "Get_Src_pos_mysql: Source postion not found for ",
     >     lsrc_name(isrc)
         else
           src_ra(isrc)=src_ra(isrc)*ha2rad
           src_dec(isrc)=src_dec(isrc)*deg2rad
         endif
      end do

890   continue
      ierr=imysql_free_result(MySQLRes)

900   continue
      ierr=imysql_close(MySQLHandle)
      end subroutine
