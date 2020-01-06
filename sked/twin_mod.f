      module twin_mod
      implicit none
! This file contains definitions of the sked 'twin_telescopes' datatype and associated routines

! 2018Jan02  KOLeBail. First version from group_mod.f

! Parameter definition: maximum number of (station1,station2,SPLIT|JOIN|-) in the twin_telescopes section
      integer max_twins
      parameter (max_twins=200)

      integer Num_Twins              !number in the list. 
      logical ktwin_read_valid      !is the 'twin read' command valid?

      type twinpair
          sequence
          character*8:: stat1_name
          character*8:: stat2_name
          character*5:: twinsplit
      end type twinpair

      type(twinpair) twin_list(max_twins)
    
      contains

! ****************************************************
! Subroutines
!
! ****************************************************
! COMPARE TWO ELEMENTS OF TYPE TWINPAIR
      integer function twinpairdiff(twinpair1,twinpair2)

! Test if two elements of twinpair type are equal, lower or greater
! On exit:
!  twinpairdiff= 0 : the two are equal
! to modify: this is only comparing stat1 of pair1 with stat1 of pair2
!  twinpairdiff= 1 : twinpair1 > twinpair2
!  twinpairdiff=-1 : twinpair1 < twinpair2
! ATTENTION! The order is done alphabetically and not by number of the station
! We do not check the last variable of the type (twinsplit), just the
!  station name

      integer igetstatnum

      type(twinpair) twinpair1, twinpair2

!      if (twinpair1%twinsplit.eq.twinpair2%twinsplit) then
        if (twinpair1%stat1_name.eq.twinpair2%stat1_name) then ! judge by the second station
          if (twinpair1%stat2_name.eq.twinpair2%stat2_name) then
            twinpairdiff=0
          elseif (twinpair1%stat2_name.gt.twinpair2%stat2_name) then
            twinpairdiff=1
          else
            twinpairdiff=-1
          endif
        elseif (twinpair1%stat1_name.eq.twinpair2%stat2_name) then
          if (twinpair1%stat2_name.eq.twinpair2%stat1_name) then
            twinpairdiff=0
          elseif (twinpair1%stat2_name.gt.twinpair2%stat1_name) then
            twinpairdiff=1
          else
            twinpairdiff=-1
          endif
        elseif (twinpair1%stat2_name.eq.twinpair2%stat1_name) then
          if (twinpair1%stat1_name.eq.twinpair2%stat2_name) then
            twinpairdiff=0
          elseif (twinpair1%stat1_name.gt.twinpair2%stat2_name) then
            twinpairdiff=1
          else
            twinpairdiff=-1
          endif
        else
          if (twinpair1%stat1_name.gt.twinpair2%stat1_name) then
            twinpairdiff=1
          else
            twinpairdiff=-1
          endif
        endif
!      else 
!        if (igetstatnum(twinpair1%stat1_name).gt.
!     >          igetstatnum(twinpair2%stat1_name)) then
!          twinpairdiff=1
!        else
!          twinpairdiff=-1
!        endif
!      endif

      return
      end function twinpairdiff

! ****************************************************
! PRINT TWIN LIST
      subroutine twin_out(luout,kall,knumber,lkind)
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
      integer i
      integer istat1, istat2

      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$TWIN_TELESCOPES"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      if(knumber) then
        write(luscn,'(a)') '#      Stat1    Stat2  Action'
      endif 

      do i=1,Num_Twins      
        if(knumber) then
          write(cbuf,'(i4," ",a," ",a," ",a)') i, twin_list(i) 
        else 
          write(cbuf,'(a,1x,a,1x,a)') twin_list(i)
        endif
        call wrt_param_line(cbuf,luout,lkind) 
      end do

      end subroutine twin_out

! ****************************************************
! TWIN COMMAND CALL IN SKED
      subroutine twin_cmd(cmdline)
! 
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'

! passed
      character*(*) cmdline
! functions
      integer istringMinMatch
      integer trimlen
      integer igetstatnum
      integer iwhere_in_string_list

! local.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)

      integer icmdlen
      integer isrc
      integer i,j
      logical kfound 
      integer istat1, istat2

      type(twinpair) twin_pair_in           !input stat/stat and split
      type(twinpair) twin_pair_temp         !temporary swap variable

      character*5 twin_split
      integer itw,twinNB
      character*128 ldum
      integer ind_newcm

! Stuff dealing with finding which "twin command" to do.
      character*12 cmd
      equivalence (ltoken(1),cmd)
      integer ifunc
      integer ilist_len
      parameter (ilist_len=6)
      character*12 list(ilist_len)
      integer itemp 
      logical kall,knumber
! NOTE: READ must be the first one in the list. 
      data list/"READ","LIST","ADD","SET","DELETE","?"/
      

      icmdlen=trimlen(cmdline)
      if(icmdlen .eq. 0) then
        ifunc=1         !default is list
      else 
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)  
        ifunc = iStringMinMatch(list,ilist_len,ltoken(1))
      endif
! 'Read' is a hidden command only valid on input. 
      if(ifunc .eq. 1 .and. .not. ktwin_read_valid) ifunc=0

! Some kind of bad command. Write out valid commands, but omit 'READ' twin
      if(ifunc .eq. 0) then
        write(luscn,'(A,a)')"Twin_cmd: Command not found: ",ltoken(1)
        write(luscn,'("Valid commands are: ", 8(a,1x))') 
     >        (trim(list(i)),i=2,ilist_len) 
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"Twin_cmd: Ambigous command: ",ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)

      select case (cmd)
!===== ?
         case ("?")
            write(luscn,'(A)')"TWIN [List | "//
     >            "Add Stat1 Stat2 [SPLIT|JOIN] | "//
     >            "Set Stat1 Stat2 [SPLIT|JOIN] | "//
     >            "Delete Stat1 Stat2 ] "
!===== LIST
         case ("LIST")
            kall=.false.
            knumber=.true.
            call twin_out(ludsp,kall,knumber,'d')    !output to the display      
!===== READ DELETE ADD SET
!READ/DEL/ADD/SET  Stat1 Stat2 [SP|TOG]         Number of tokens is 4
         case DEFAULT
            if (NumToken .eq.1) then
              write(luscn,*) "Twin_cmd: Must specify twin station "
     >                       //"names and if treated together or not"
              return 
            else if (NumToken .eq. 2) then
              write(luscn,*) "Twin_cmd: must set second twin name "
     >                       //"and if twin tel treated together or not"
              return
            else if (NumToken .eq. 3) then
              write(luscn,*) "Twin_cmd: must specify if "
     >                       //"twin telescopes treated together or not"
              return
            else if(NumToken .gt. 5) then
              write(luscn,*) "Twin_cmd:   too many arguments"
              goto 900
            endif
            ! read [SPLIT|JOIN]
            read(ltoken(4),*,err=900) twin_split   !get the value for twinsplit
            call capitalize(twin_split) 
            if (twin_split.ne."SPLIT" .and. twin_split.ne."JOIN" .and. 
     >          twin_split .ne. "-" ) then
               write(luscn,*) 
     >        "Twin_cmd: value for twin number is not SPLIT, JOIN or -" 
               goto 990
            else if (twin_split.eq."JOIN") then
               write(luscn,*) "Twin_cmd: warning! Value is JOIN: the"
     >                    ," two stations will be scheduled together."
            endif
            ! read station names
            istat1=iwhere_in_string_list(cstnna,nstatn,ltoken(2))
            if(istat1 .le.0 ) then
               write(luscn,*) "Twin_cmd: Station not found ",ltoken(2) 
               return
            endif
            istat2=iwhere_in_string_list(cstnna,nstatn,ltoken(3))
            if(istat2.le.0) then
               write(luscn,*) "Twin_cmd: Station not found ",ltoken(3) 
               return
            endif
! we write the pair in alphabetical order: stat1 .lt. stat2
            if (cstnna(istat1).lt.cstnna(istat2)) then
              twin_pair_in%stat1_name=cstnna(istat1)
              twin_pair_in%stat2_name=cstnna(istat2)
            elseif (cstnna(istat1).gt.cstnna(istat2)) then
              twin_pair_in%stat2_name=cstnna(istat1)
              twin_pair_in%stat1_name=cstnna(istat2)
            else
              write(luscn,*) "Twin_cmd: same station: ",ltoken(2),'-',
     >                        ltoken(3)
              write(luscn,*) "No additional entry in TWIN_TELESCOPES ",
     >                       "section"
              return
            endif
            twin_pair_in%twinsplit=twin_split
            select case (cmd)
            !===== READ from SKED file
             case("READ")
              ! Assume that things are more or less in order when we read in.
              ! 1st insert at the end of the list.                
               Num_Twins=Num_Twins+1
               twin_list(Num_Twins)=twin_pair_in
              ! Now do swaps if necessary to put it in the correct place.
               do  i=Num_Twins,2,-1             !Start at end    
                if(TwinPairDiff(twin_list(i),twin_list(i-1)) 
     >                                                   .lt. 0) then
              ! This means that twin_list(i) belongs before twin_list(i-1).  Swap them. 
                   twin_pair_temp=twin_list(i)
                   twin_list(i)=twin_list(i-1)
                   twin_list(i-1)=twin_pair_temp
                 else
              ! No need to swap. Just exit. 
                   return                   !Done 
                 endif
               enddo   
10             continue                     
!===== DELETE
             case ("DELETE")
               kfound=.false.
               i=1
               do while (i.le.Num_Twins.and. .not.kfound)
                   if (twinpairdiff(twin_list(i),twin_pair_in).eq.0)
     >                then
                      do j=i+1,Num_Twins
                         twin_list(j-1)=twin_list(j)
                      enddo               
                      kfound=.true.            
                      Num_Twins=Num_Twins-1
                  endif
                  i=i+1
               enddo
               if(.not. kfound) write(luscn,
     >    '("Twin_cmd: Stations ",a," ",a," not found in twin # ",a)')
     >            trim(cstnna(istat1)),trim(cstnna(istat2)),twin_split
!===== ADD/SET
             case ("ADD","SET")
               if (Num_Twins .eq. 0) then
                  twin_list(1)=twin_pair_in
                  Num_Twins=1
               elseif (Num_Twins .lt. max_twins) then
                    kfound=.false.
                    i=1
                    do while (i.le.Num_Twins .and..not.kfound) 
                      if (twinpairdiff(twin_pair_in,twin_list(i))
     >                   .lt.0) then 
                    ! insert 
                        if (i.eq.Num_Twins) then
                          twin_list(i+1)=twin_list(i)
                          twin_list(i)=twin_pair_in
                          Num_Twins=Num_Twins+1
                        else
                          do j=Num_Twins+1,i+1,-1
                            twin_list(j)=twin_list(j-1)
                          enddo
                          twin_list(i)=twin_pair_in
                          Num_Twins=Num_Twins+1
                        endif
                        kfound=.true.
                      elseif
     >           (twinpairdiff(twin_pair_in,twin_list(i)).eq.0) then
                        if
     >        (twin_pair_in%twinsplit.eq.twin_list(i)%twinsplit) then
                          write(luscn,*) "Twin_cmd: WARNING ",
     >                      "stations and action already in the list."
                          kfound=.true.
                        else
                          write(luscn,*) "Twin_cmd: WARNING ",
     >              "stations already in the list but different action."
                          if (cmd.eq."SET") then
                            write(luscn,*) "The action will be changed."
                            ! replace
                            twin_list(i)=twin_pair_in
                            kfound=.true.
                          else
                            write(luscn,*) "Use SET if you want the ",
     >                                     "action to be changed."
                            kfound=.true.
                          endif
                        endif
                      endif
                      i=i+1
                    enddo
                    if (.not.kfound) then ! we are at the end of the list
                      twin_list(Num_Twins+1)=twin_pair_in
                      Num_Twins=Num_Twins+1
                    endif
               else ! Num_Twins=max_twins
                  write(luscn,*) "Twin_cmd: revise size of ",
     >              "max_twins. Too many stations."
                  return
               endif
!===== invalid commands
             case DEFAULT ! invalid command
               write(luscn,*) "Twin_cmd: invalid command"
               return
            end select
      end select
      return 

! Different error conditions
900   continue
      write(luscn, *) "Twin_cmd: Too many parameters"
      goto 990

990   continue
      write(luscn,*)  "         ",cmdline(1:trimlen(cmdline))

      end subroutine twin_cmd

! ****************************************************
! 

      end module twin_mod
