      subroutine broadband_cmd(cmdline)

! Set, display station weights.

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
! History
!  2010Apr11 JMG   First version. Modeled on astro_Cmd. 
!  2010Apr23 JMG  Wasn't handling ADD correctly. 
!  2015Mar26 JMG. Modified to add idata_mbps and isink_mbps
!  2016Nov11 JMG. Modified help/error messages to include idata_mbps and isink_mbps


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
      integer itemp1,itemp2 

! Stuff dealing with finding which "broadband command" to do.
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
        write(luscn,'(A,a)')"broadband_cmd: Keyword not found: ",
     >   ltoken(1)
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"broadband_Cmd: Ambigous keyword: ",
     >   ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)
      if(cmd.eq. "?") goto 910

      if(cmd.eq."LIST") then
        write(luscn,'(a)')
     &      '   # Station    BW(MHz) Data(mbps) Sink(mbps)'
        kall=.false.
        knumber=.true.
        call broadband_Out(ludsp,kall,knumber,'d')
        return     
      endif

! Must be "ADD","DELETE", or "SET" 

 !ADD/SET  Stat Wt                 Number of tokens is 3 
 !DEL      station                 Number of tokens is 2
 ! find the ranges for the source(s)

! If we add sources, then we get the min and max values below.
      if(NumToken .eq. 1) then
         write(luscn,'(a)') "broadband_Cmd:  Must specify station. "
        return
      endif 
 
      if((cmd .eq. "ADD" .or. cmd .eq."SET") .and. 
     >   (NumToken .gt. 5 .or. NumToken .lt. 3) .or. 
     >   (cmd  .eq. "DELETE" .and. NumToken .ne. 2)) then
          write(luscn,*) "bbwt_Cmd: Wrong number of arguments"
          return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq. "SET") then        
        read(ltoken(3),*,err=900) rWt    !get the effective BW
        read(ltoken(4),*,err=900) itemp1   !get the effective data_rate (mbps) mega-bits-per-sec
        read(ltoken(5),*,err=900) itemp2   !get the effective sink rate
      else
        rwt=0.d0
      endif

! Get the station.
      if(ltoken(2) .eq. "_" .or. ltoken(2) .eq. "ALL") then
        bb_BW(1:nstatn)=rwt      !set all stations to this.
      else
        istat=istringminmatch(cstnna,nstatn, ltoken(2))   ! Check against full name.
        if(istat .le. 0) then     ! now check against two character code.
          istat=igetstatnum2(ltoken(2)(1:2))
          if(istat .eq. 0) then
            write(luscn,'(a)') "broadband_Cmd: Did not find station: ",
     >         ltoken(2)
            return
          endif
        endif       
        bb_BW(istat)=rwt
        if(NumToken .ge. 4) idata_mbps(istat)=itemp1
        if(NumToken .eq. 5) isink_mbps(istat)=itemp2
      end if   
      return

! Different error conditions
900   continue
      write(luscn, *) "broadband_cmd: Error reading arguments."
      write(luscn, *) "Format is: "

! Come here if user typed: "broadband ?"
910   continue      
      write(luscn,'(A)')"broadband  [ LIST | "//  
     >  "| ADD <Station> <BW(Mhz)> <Data(Mps)> <Sink(Mps)> | "//
     >   " SET <Station> <BW(Mhz)> <Data(Mps)> <Sink(Mps)> |"//
     >   " DELETE <Station> ]"
    
      return
      end

