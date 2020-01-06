      subroutine statwt_cmd(cmdline)

! Set, display station weights.

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'statwt.ftni'
! History
!  2010Apr11 JMG   First version. Modeled on astro_Cmd. 
!  2010Apr23 JMG  Wasn't handling ADD correctly. 


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

! Stuff dealing with finding which "StatWt command" to do.
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
        write(luscn,'(A,a)')"Statwt_Cmd: Keyword not found: ",ltoken(1)
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"Statwt_Cmd: Ambigous keyword: ",ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)
      if(cmd.eq. "?") then
        write(luscn,'(A)')"StatWt  [List | Obs | Delete <Station> "//  
     >  "| Add <Station> <Weight> | Set <Station> <Weight> ]"
        return
      else if(cmd.eq."LIST") then
        write(luscn,'(a)') '   # Station      Wt'
        kall=.false.
        knumber=.true.
        call StatWt_Out(ludsp,kall,knumber,'d')
        return     
      endif

! Must be "ADD","DELETE", or "SET" 

 !ADD/SET  Stat Wt                 Number of tokens is 3 
 !DEL      station                 Number of tokens is 2
 ! find the ranges for the source(s)

! If we add sources, then we get the min and max values below.
      if(NumToken .eq. 1) then
         write(luscn,'(a)') "Statwt_Cmd:  Must specify station. "
        return
      endif 
 
      if((cmd .eq. "ADD" .or. cmd .eq."SET") .and.  NumToken .ne. 3 .or.
     >   (cmd  .eq. "DELETE" .and. NumToken .ne. 2)) then
          write(luscn,*) "Statwt_Cmd: Wrong number of arguments"
          return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq. "SET") then        
        read(ltoken(3),*,err=900) rWt    !get the minimum value.
      else
        rwt=0.d0
      endif

! Get the station.
      if(ltoken(2) .eq. "_" .or. ltoken(2) .eq. "ALL") then
        statwt(1:nstatn)=rwt      !set all stations to this.
      else
        istat=istringminmatch(cstnna,nstatn, ltoken(2))   ! Check against full name.
        if(istat .le. 0) then     ! now check against two character code.
          istat=igetstatnum2(ltoken(2)(1:2))
          if(istat .eq. 0) then
            write(luscn,'(a)') "StatWt_Cmd: Did not find station: ",
     >         ltoken(2)
            return
          endif
        endif       
        statwt(istat)=rwt
      end if   
      return

! Different error conditions
900   continue
      write(luscn, *) "Statwt_Cmd: Error reading Wt"
      return
      end

