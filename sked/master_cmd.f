      SUBROUTINE master_cmd(cmdline)
! 1. CHECK the schedule against the master file.
! 2. GET stations, time from the master file.

C History:
!  2007Nov20 JMGipson First version
!  2008Jun06 JMG.  Wasn't correctly setting/checking end time.
!                  Also, made parsing more flexible.
!                  Initialize starting time.
! 2009Mar03 JMG.  Fixed bug in end time.
! 2009Jul15 JMG.  Checked SNR to make sure some of the entries are non-zero.
! 2009Sep03 JMG.  Modified so that can accept upto 64 stations 
! 2009Sep09 JMG.  Returns if can't find master file
! 2010Jan27 JMg.  Modified to read scheduler and correlator from master file.
! 2010Mar24 JMG. Modified error message. 
! 2011Aug11 JMG. Modified error message. 
! 2012Oct26 JMG. ditto.
! 2014Apr23 JMG. Set correlator and scheduler from master file...


C   COMMON BLOCKS USED

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/statn.ftni'   !contains nstatn.
      include '../skdrincl/skobs.ftni'   !contains nstatn.

      include 'skcom.ftni'
      include 'cat_stat.ftni'

C Input
      character*(*) cmdline

! functions
      integer iStringMinMatch
      integer trimlen
      integer iwhere_in_string_list
      double precision hms2seconds
      integer julda   

! used to  extract tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=10)
      character*20 ltoken(MaxToken)  
      integer icmd

C   LOCAL VARIABLES
      character*256 ldum
      character*8 cexper_master                  !Session in master file
      integer iyr_mst_start,ida_mst_start,ihr_mst_start,imin_mst_start  !start, end times
      integer iyr_mst_end  ,ida_mst_end  ,ihr_mst_end  ,imin_mst_end  

      character*128 lstat_list
      character*2  lstat_vec(2)
      equivalence (lstat_list,lstat_vec(1))
      character*2 cpo_master(Max_stn)            !abbreviated session code in master file
      character*10 cpiname_mst                    !Who is supposed to schedule it
      character*10 ccorname_mst                    !Where is it correlated.
      character*10 ctemp
      integer inum_stat
      integer i
      integer ibl              !ibaseline
      integer iband
      integer nch
      character*2 cpo_vlba(10)
      logical kfound(max_stn)
      logical kexist
      logical kbeg
      integer ind
      integer len 
      integer iwhere
      character*2 cdo            !how to open catalogs.
      character*1 cdo1
      equivalence (cdo1,cdo)
      integer iptr
      integer num_sel
      character*50 cname
      integer  ierr
      logical kerr
      integer ihrs_long
      logical kbad_snr
      integer*2 linestq(30)
      integer*4 itime_vec(6)   !holds date and time
      character*128 cmaster_file   !local copy of this.
      integer itmp                 !short term dummy variable
      integer iyear
      logical kfound_line          !did we find the entry in the master file?

! valid command list.
      integer ilist_len
      parameter (ilist_len=2)
      character*6 list(ilist_len)
      character*40 lhelp(ilist_len)

      data list/"CHECK", "GET"/
      data lhelp/
     > " stations, times against master file",
     > " stations, times against master file"/

      data cpo_vlba/"Br","Fd","Hn","Kp","La","Mk","Nl","Ov","Pt","Sc"/

      if(cmaster_file.eq."NONE") then
         write(luscn,*)
     >     "master_cmd: You must specify a master file in skedf.ctl"
         return
      endif
     
! Start of code
      call capitalize(cmdline)
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      itoken=1
      if(NumToken.eq.0 .or. ltoken(1).eq."?") then
        do i=1,ilist_len
          write(luscn,'(a)') list(i)//lhelp(i)
        end do
        return
      endif

      if(NumToken .gt. 1) then
         write(luscn,'(a)')
     >      "master_cmd: Too many arguments!"
         return
      endif
! parse the token, see if it is valid.
      icmd=istringMinMatch(list,ilist_len,ltoken(1))
      if(icmd .eq. 0) then
         write(luscn,'(a,a)') "master_cmd: Not found: ",ltoken(1)
         write(luscn,*) "master_cmd: Valid commands ",
     >       (list(i),i=1,ilist_len)
           return
      else if(icmd .le. 0) then
         write(luscn,'(a,a)') "master_cmd: Ambigious  ",ltoken(1)
         return
      endif

100   continue
      write(luscn,'(" Finding session ", a)') cexper
! For compatibility with older versions of sked, see if cmaster_dir is really a file.
      itmp=trimlen(cmaster_dir)
      if(cmaster_dir(itmp-3:itmp) .eq. ".txt") then
        call return_master_line(cmaster_dir,cexper,ldum,
     >   iyr_mst_start,kfound_line)
        if(kfound_line) goto 110
        write(luscn,'("Master_cmd ERROR: file not found! ", a)') 
     >     cmaster_dir(1:itmp)
        return
      endif

! Assume that we specified a directory.
      call add_slash_if_needed(cmaster_dir)
      itmp=trimlen(cmaster_dir)

! Get the current year. 
      call date_and_time_sked(itime_vec)
      iyear=itime_vec(1)+1  -2000 ! sometimes we schedule for next year.
!      write(*,*) "Starting with year ", itime_vec(1)+1
        
      do i=iyear,0, -1
! First try normal master file.     
        write(cmaster_file,'(a,"master",i2.2, a)')
     >      cmaster_dir(1:itmp),i,".txt"  
        call return_master_line(cmaster_file,cexper,ldum,
     >         iyr_mst_start, kfound_line)   
        if(kfound_line) goto 110

        write(cmaster_file,'(a,"master",i2.2, a)')
     >      cmaster_dir(1:itmp),i,"-int.txt"
        call return_master_line(cmaster_file,cexper,ldum,
     >        iyr_mst_start, kfound_line)  
        if(kfound_line) goto 110
      end do
      write(luscn,'(a)') "Master_cmd: Did not find experiment code!"
      return     


! Begin parsing the line.
110   continue  
      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)
! get the start time
      read(ltoken(4),'(i3)') ida_mst_start
      read(ltoken(5),"(i2,1x,i2)") ihr_mst_start,imin_mst_start
! get the length, and calculate the end time.
      read(ltoken(6),"(i2)"), ihrs_long
      iyr_mst_end  =iyr_mst_start
      ida_mst_end  =ida_mst_start
      ihr_mst_end  =ihr_mst_start+ihrs_long
      imin_mst_end =imin_mst_start

      if(ihr_mst_end .ge. 24) then
         ihr_mst_end = ihr_mst_end-24
         ida_mst_end = ida_mst_start+1
      endif
! handles everything except leap year.
      if(ida_mst_end .gt. 365) then
         ida_mst_end =1
         iyr_mst_end =iyr_mst_end+1
      endif

      ind=index(ldum,ltoken(7)(1:4))
!      write(*,*) "ldum ",ldum(1:80)
      nch=index(ldum(ind:256)," ")-1
      lstat_list=ldum(ind:ind+nch-1) 

! make list of stations in master schedule
      inum_stat=0
      do i=1,nch/2
       inum_stat=inum_stat+1
       if(lstat_vec(i) .ne. "Va") then
          cpo_master(inum_stat)=lstat_vec(i)
       else
          cpo_master(inum_stat:inum_stat+9)=cpo_vlba
          inum_stat=inum_stat+9
       endif
      end do
     
      if(ltoken(8)(1:1) .eq. "-") then
        cpiname_mst=ltoken(9)
        ccorname_mst=ltoken(10)
      else
        cpiname_mst=ltoken(8)
        ccorname_mst=ltoken(9)
      endif

      kerr=.false.
! Here is where we process the stuff.
! CHECK  command

      if(icmd .eq. 1) then   !check command
        kfound=.false.
        if(iyr_mst_start .ne. iyr_start .or.
     >     ida_mst_start .ne. ida_start .or.
     >     ihr_mst_start .ne. ihr_start .or.
     >     imin_mst_start .ne. imin_start) then
          write(luscn,'(a)')"master_cmd: ERROR! Mismatch in start time!"
          write(luscn,'("   Master file: ",i3,"-",i2.2,":",i2.2)')
     >       ida_mst_start, ihr_mst_start,imin_mst_start
          write(luscn,'("   Sked file  : ",i3,"-",i2.2,":",i2.2)')
     >     ida_start, ihr_start, imin_start
           kerr=.true.
        endif

        if(iyr_mst_end .ne.  iyr_end .or.
     >     ida_mst_end .ne. ida_end .or.
     >     ihr_mst_end .ne. ihr_end .or.
     >     imin_mst_end .ne. imin_end) then
          write(luscn,'(a)')"master_cmd: ERROR! Mismatch in end time!"
          write(luscn,'("   Master file: ",i3,"-",i2.2,":",i2.2)')
     >       ida_mst_end, ihr_mst_end,imin_mst_end
          write(luscn,'("   Sked file  : ",i3,"-",i2.2,":",i2.2)')
     >     ida_end, ihr_end, imin_end
           kerr=.true.
        endif

        kbeg=.false.
        do i=1,inum_stat
          iwhere=iwhere_in_string_list(cpocod,nstatn,cpo_master(i))
          if(iwhere .eq. 0) then
             kerr=.true.
             if(.not. kbeg) write(luscn,'(a,$)')
     >         "master_cmd: ERROR Station in master, not in schedule! "
             kbeg=.true.
             write(luscn,'(" ",a," ",$)') cpo_master(i)
          else
            kfound(iwhere) =.true.
          endif
        end do
        if(kbeg) write(luscn,'()')
        kbeg=.false.

        do i=1,nstatn
          if(.not.kfound(i)) then
             kerr=.true.
             if(.not.kbeg) write(luscn,'(a,$)')
     >         "master_cmd: ERROR Station in schedule, not in master! "
             kbeg=.true.
             write(luscn,'(" ",a," ",$)') cpocod(i)
          endif
        end do
        if(kbeg) write(luscn,'()')

! Check for agreement between scheduler and correlator

        ctemp=cpiname
        call capitalize(ctemp)
        call capitalize(cpiname_mst)

        if(ctemp .ne. cpiname_mst) then
           write(luscn,
     > '("master_cmd: Scheduler disagreement!")')
           write(luscn,'("Master: ",a," Schedule file: ",a)') 
     >      cpiname_mst,cpiname 
           kerr=.true.
        endif

        ctemp=ccorname
        call capitalize(ctemp)
        call capitalize(ccorname_mst)

        if(ctemp .ne. ccorname_mst) then
           write(luscn,
     > '("master_cmd: Correlator disagreement!")')
           write(luscn,'("Master: ",a," Schedule file: ",a)') 
     >      ccorname_mst, ccorname
           kerr=.true.
        endif

        if(.not. kerr)  write(luscn,'(a)')
     >        "master_cmd: schedule and master file agree!"
        return
      endif

! GET  command
300   continue
      if(icmd .eq. 2) then
        cpiname =cpiname_mst
        ccorname=ccorname_mst

        cbuf(3:)="CORRELATOR "//ccorname_mst
        ibuf(1)=trimlen(cbuf)
        call prset(ibuf)
      
        cbuf(3:)="SCHEDULER "//cpiname_mst
        ibuf(1)=trimlen(cbuf) 
        call prset(ibuf)

        call delete_all_obs()  !start from scratch

        iyr_start=iyr_mst_start
        ida_start=ida_mst_start
        ihr_start=ihr_mst_start
        imin_start=imin_mst_start
        isc_start=0
        do i=1,max_stn
          iyrcur(i)=iyr_start
          idacur(i)=ida_start
          utcur(i)=hms2seconds(ihr_start,imin_start,isc_start)
          mjdcur(i)=JULDA(1,IDA_start,IYR_start-1900)
        end do

        iyr_end=iyr_mst_end
        ida_end=ida_mst_end
        ihr_end=ihr_mst_end
        imin_end=imin_mst_end

        cdo="a"
        call make_stat_list(cdo,ierr)

        kcat_stat_sel=.false.   !initialze all to not selected.
        do i=1, inum_stat
          do iptr=1,num_cat_stat
            if(cpo_master(i).eq.cat_ant_id2(icat_stat_vec(1,iptr))) then
               kcat_stat_sel(iptr)=.true.
               goto 310
            endif
          end do
310     continue
        end do

        write(luscn,'(a)') "master_cmd: Initializing experiment. "
        write(luscn,'("START: ",i4,"/",i3,"-",i2.2,":",i2.2)')
     >      iyr_mst_start, ida_mst_start, ihr_mst_start,imin_mst_start

        write(luscn,'("END:   ",i4,"/",i3,"-",i2.2,":",i2.2)')
     >      iyr_mst_end,ida_mst_End, ihr_mst_End,imin_mst_End

        write(luscn,'(a)') "Stations: "
        iptr=0
        call display_stat_item(cname,iptr)
        write(luscn,'(4x,a)') cname
        num_sel=0
        do iptr=1,num_cat_stat
          if(kcat_stat_sel(iptr)) then
            num_sel=num_sel+1
            call display_stat_item(cname,iptr)
            write(luscn,'(i3,1x,a)') num_sel,cname
         endif
        end do
        CALL WRSTS(IERR)
        cdo1='m'
        call stsel(cdo1)

! Initialize the subnet to all stations
        ldum="sub"
        call major_cmd(ldum)   !this will set the subnet to all     

        kbad_snr=.false.
        do ibl=1, (inum_stat*(inum_stat-1))/2
          do iband=1,2
            if(isnrbl(iband,ibl) .eq. 0)  kbad_snr=.true.
          end do
        end do
         
       if(kbad_snr) then
           
           write(luscn,'(a)') 
     >   "****WARNING!!! Some baselines have 0 SNR! Please set."
           write(luscn,'(a)') "HINT:  'SNR Subnet Band Value'"
           linestq=0           
           CALL SNRCM(linestq,'s',' ')
          return
        endif
           
        write(luscn,'(a)') "master_cmd: Be sure to check SNR, tape,etc!"
        return
      endif

500   continue
      write(luscn,'(a)') "master_cmd: Experiment "//cexper//"not found!"

      return
      end

