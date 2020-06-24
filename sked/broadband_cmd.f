      subroutine broadband_cmd(cmdline)

! Set, display station weights.
      implicit none 

! Common blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
! History
!  2010Apr11 JMG   First version. Modeled on astro_Cmd. 
!  2010Apr23 JMG  Wasn't handling ADD correctly. 
!  2015Mar26 JMG. Modified to add idata_mbps and isink_mbps
!  2016Nov11 JMG. Modified help/error messages to include idata_mbps and isink_mbps
!  2020Jun08 JMG. Reference to new broadband.ftni. 
!  2020Jun09 jmg. Fixed bug in parsing.  Added implicit none. 


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
      parameter(MaxToken=6)
      character*12 ltoken(MaxToken)

      integer icmdlen
      integer istat
      real BW_tmp
      integer idata_tmp, isink_tmp, ibb_off_tmp !default/temporary values. 

      integer i 

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
     &      '   # Station    BW(MHz) Data(mbps) Sink(mbps) BB_OFF'
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
         write(luscn,'(a)') "Broadband_Cmd:  Must specify station. "
        return
      endif 
 
      if((cmd .eq. "ADD" .or. cmd .eq."SET") .and. 
     >   (NumToken .gt. 6 .or. NumToken .lt. 3) .or. 
     >   (cmd  .eq. "DELETE" .and. NumToken .ne. 2)) then
          write(luscn,*) "Broadband_Cmd: Wrong number of arguments"
          return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq. "SET") then      
        BW_tmp=   8
        idata_tmp=4096
        isink_tmp=4096 
        ibb_off_tmp=0 
        if(NumToken .ge. 3) read(ltoken(3),*, err=900) BW_tmp
        if(NumToken .ge. 4) read(ltoken(4),*, err=900) idata_tmp
        if(NumToken .ge. 5) read(ltoken(5),*, err=900) isink_tmp
        if(NumToken .ge. 6) read(ltoken(6),*, err=900) ibb_off_tmp            
        if(isink_tmp .eq. 0) isink_tmp=idata_tmp  
!        write(*,*) BW_tmp, idata_tmp, isink_tmp, ibb_off_tmp
      endif

! Get the station.
      if(ltoken(2) .eq. "_" .or. ltoken(2) .eq. "ALL") then
        bb_BW(1:nstatn)     =BW_tmp      !set all stations to this.
        idata_mbps(1:nstatn)=idata_tmp
        isink_mbps(1:nstatn)=isink_tmp
        ibb_off(1:nstatn)   =ibb_off_tmp
      else
        istat=istringminmatch(cstnna,nstatn, ltoken(2))   ! Check against full name.
        if(istat .le. 0) then     ! now check against two character code.
          istat=igetstatnum2(ltoken(2)(1:2))
          if(istat .eq. 0) then
            write(luscn,'(a)') "Broadband_Cmd: Did not find station: ",
     >         ltoken(2)
            return
          endif
        endif       
        bb_BW(istat)     =BW_tmp      !set all stations to this.
        idata_mbps(istat)=idata_tmp
        isink_mbps(istat)=isink_tmp
        ibb_off(istat)   =ibb_off_tmp
      end if   
      return

! Different error conditions
900   continue
      write(luscn, *) "broadband_cmd: Error parsing cmdline:"
      write(luscn, '(10x, a)') trim(cmdline) 
      write(luscn, *) "Format is: "

! Come here if user typed: "broadband ?"
910   continue      
      do i=1,4
        write(luscn,'(a,$)') "   broadband "
        select case(i)
        case(1)
          write(luscn,'(a)') "LIST"
        case(2)
          write(luscn,'(a)') "DELETE <station>"
        case(3)
          write(luscn,'(a)') 
     >     "ADD <Station> <BW(Mhz)> <Data(Mps)> <Sink(Mps)> <BB_off>" 
        case(4)
          write(luscn,'(a)') 
     >     "SET <Station> <BW(Mhz)> <Data(Mps)> <Sink(Mps)> <BB_off>" 
        end select
      end do 

    
    
      return
      end

