      subroutine srcwt_cmd(cmdline)

! Set, display station weights.

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'srcwt.ftni'
! History
!  2010Apr11 JMG   First version. Modeled on astro_Cmd. 
!  2010Apr23 JMG  Wasn't handling ADD correctly. 
!  2010Oct04 JMG. Originally copied from StatWt_cmd. In the process didn't fix 
!                 all of the references to stations. Fixed in this version.

! passed
      character*(*) cmdline
! functions
      integer istringMinMatch
      integer trimlen
      integer iwhere_in_string_list
      integer igetsrcnum

! local.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)

      integer icmdlen
      integer isrc
      real rwt

! Stuff dealing with finding which "srcwt command" to do.
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
        write(luscn,'(A,a)')"SrcWt_Cmd: Keyword not found: ",ltoken(1)
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"SrcWt_Cmd: Ambigous keyword: ",ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)
      if(cmd.eq. "?") then
        write(luscn,'(A)')"srcwt  [List | Obs | Delete <Source> "//  
     >  "| Add <Source> | Set <Source> <Weight> ]"
        return
      else if(cmd.eq."LIST") then
        write(luscn,'(a)') '   # Source       Wt'
        kall=.false.
        knumber=.true.
        call srcwt_Out(ludsp,kall,knumber,'d') 
        return     
      endif

! Must be "ADD","DELETE", or "SET" 

 !ADD/SET  Src Wt                 Number of tokens is 3 
 !DEL      sRC                 Number of tokens is 2
 ! find the ranges for the source(s)

! If we add sources, then we get the min and max values below.
      if(NumToken .eq. 1) then
         write(luscn,'(a)') "srcwt_Cmd:  Must specify source. "
        return
      endif 
 
      if((cmd .eq. "ADD" .or. cmd .eq."SET") .and.  NumToken .ne. 3 .or.
     >   (cmd  .eq. "DELETE" .and. NumToken .ne. 2)) then
          write(luscn,*) "srcwt_Cmd: Wrong number of arguments"
          return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq. "SET") then        
        read(ltoken(3),*,err=900) rWt    !get the minimum value.
      else
        rwt=0.d0
      endif

! Get the source
      if(ltoken(2) .eq. "_" .or. ltoken(2) .eq. "ALL") then
        srcwt(1:nsourc)=rwt      !set all source to this.
      else
        isrc=igetsrcnum(ltoken(2))
        if(isrc .eq. 0) then
           write(luscn,'(a)') "srcwt_Cmd: Did not find source: ",
     >         ltoken(2)
           return 
        endif    
        srcwt(isrc)=rwt
      end if   
      return

! Different error conditions
900   continue
      write(luscn, *) "srcwt_Cmd: Error reading Wt"
      return
      end

