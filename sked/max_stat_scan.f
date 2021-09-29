      Module max_stat_scan
! Module to 
      integer max_ss_list(60)   !should strictly be number of stations. 
      contains
! *********************************************************************
      subroutine max_stat_scan_out(luout,kall,knumber,lkind)
! write out the stations in $TWIN_TELESCOPES
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'

! passed
      integer luout
      logical kall      !list all lists
      logical knumber   !number the stations
      character*1 lkind 
! local
      integer istat 
      
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$MAX_STAT_SCAN"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      if(knumber) then
        write(luscn,'(a)') '  #  Station Scan'
      endif 
      
      do istat=1,nstatn
        if(kall .or. max_ss_list(istat) .ne. 0) then
           if(knumber) then 
              write(luout,'(i4," ",$)') istat 
           endif 
           write(luout,'(a8," ",i3)') cstnna(istat),max_ss_list(istat)
        endif
      end do
      return
      end subroutine
! ***********************************************************************    
      subroutine max_stat_scan_cmd(cmdline)
! 
! Set, display station weights.

! Common blocks
       include '../skdrincl/skparm.ftni'
       include 'skcom.ftni'
       include '../skdrincl/statn.ftni'

! History
!  2021-09-23  JMG Modeled on max_ss_list 
!  20Apr11 JMG   First version. Modeled on max_ss_list


! passed
      character*(*) cmdline
! functions
      integer istringMinMatch
      integer trimlen
      integer iwhere_in_string_list
      integer igetstatnum2

! local.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)

      integer icmdlen
      integer istat
      real rwt

! Stuff dealing with finding which max_stat_scan command" to do.
      character*12 cmd
      equivalence (ltoken(1),cmd)
      integer ifunc
      integer ilist_len
      parameter (ilist_len=5)
      character*12 list(ilist_len)
      logical kall,knumber
      data list/"LIST","ADD","SET","DELETE","?"/

      icmdlen=trimlen(cmdline)
      if(icmdlen .eq. 0) then
        ifunc=1         !default is list
      else
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
        ifunc = iStringMinMatch(list,ilist_len,ltoken(1))
      endif

! Some kind of bad command
      if(ifunc .eq. 0) then
        write(luscn,'(A,a)')"max_stat_scan: Keyword not found: ",
     >    ltoken(1)
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"max_stat_scan: Ambigous keyword: ",
     >    ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)
      if(cmd.eq. "?") then
        write(luscn,'(A)')
     >   "Max_stat_Scan  [List | Delete <Station> "//  
     >  "| Add <Station> <Max_scan> | Set <Station> <Max_scan> ]"
        return
      else if(cmd.eq."LIST") then  
        kall=.false.
        knumber=.true.
        call max_stat_scan_out(ludsp,kall,knumber,'d')
        return     
      endif

! Must be "ADD","DELETE", or "SET" 

 !ADD/SET  Stat Wt                 Number of tokens is 3 
 !DEL      station                 Number of tokens is 2
 ! find the ranges for the source(s)

! If we add sources, then we get the min and max values below.
      if(NumToken .eq. 1) then
         write(luscn,'(a)') "max_ss_list:  Must specify station. "
        return
      endif 
 
      if((cmd .eq. "ADD" .or. cmd .eq."SET") .and.  NumToken .ne. 3 .or.
     >   (cmd  .eq. "DELETE" .and. NumToken .ne. 2)) then
          write(luscn,*) "max_ss_list: Wrong number of arguments"
          return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq. "SET") then        
        read(ltoken(3),*,err=900) rWt    !get the minimum value.
      else
        rwt=0.d0
      endif

! Get the station.
      if(ltoken(2) .eq. "_" .or. ltoken(2) .eq. "ALL") then
        max_ss_list(1:nstatn)=rwt      !set all stations to this.
      else
        istat=istringminmatch(cstnna,nstatn, ltoken(2))   ! Check against full name.
        if(istat .le. 0) then     ! now check against two character code.
          istat=igetstatnum2(ltoken(2)(1:2))
          if(istat .eq. 0) then
            write(luscn,'(a)') "max_ss_list: Did not find station: ",
     >         ltoken(2)
            return
          endif
        endif       
        max_ss_list(istat)=rwt
      end if   
      return

! Different error conditions
900   continue
      write(luscn, *) "max_ss_list: Error reading Wt"
      return
      end subroutine 
!--      
      end module



           
          
      


