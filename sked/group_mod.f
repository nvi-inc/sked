      module group_mod
      implicit none
! This file contains definitions of the sked 'group' datatype and associated routines

! 2016Nov07  KOLeBail.
! 2016Nov14  KOLeBail. Changed format to type
! 2016Dec02  KOLeBail. Changed group.ftni / group_cmd.f / group_out.f / group_obs.f / 
!                      group_cull / oroup_count.f into module

! Parameter definition: maximum number of (sources,group_nb) in the group section
      integer maxingroup
      parameter (maxingroup=200)

      integer NumInGroupList              !number in the list. 
      logical kgroup_read_valid      !is the 'group read' command valid?

      type sourcepair
          sequence
          character*8:: src_name
          integer:: group_number
      end type sourcepair

      type(sourcepair) group_list(maxingroup)
    
      contains

! ****************************************************
! Subroutines
!
! ****************************************************
! COMPARE TWO ELEMENTS OF TYPE SOURCEPAIR
      integer function sourcepairdiff(sourcepair1,sourcepair2)

! Test if two elements of sourcepair type are equal, lower or greater
! On exit:
!  sourcepairdiff= 0 : the two are equal
!  sourcepairdiff= 1 : sourcepair1 > sourcepair2
!  sourcepairdiff=-1 : sourcepair1 < sourcepair2

      integer igetsrcnum

      type(sourcepair) sourcepair1, sourcepair2

      if (sourcepair1%group_number.eq.sourcepair2%group_number) then
        if (sourcepair1%src_name.eq.sourcepair2%src_name) then
          sourcepairdiff=0
        elseif (igetsrcnum(sourcepair1%src_name).gt.
     >          igetsrcnum(sourcepair2%src_name)) then
          sourcepairdiff=1
        else
          sourcepairdiff=-1
        endif
      elseif 
     >  (sourcepair1%group_number .gt. sourcepair2%group_number) then
        sourcepairdiff=1
      else
        sourcepairdiff=-1
      endif

      return
      end function sourcepairdiff

! ****************************************************
! COUNT NUMBER OF SOURCES IN EACH GROUP
      integer function group_count(group1,nbgr)

! Calculate the number of "valid" sources in group1 (of type group)
! If nbgr = 0, it counts all sources
! If nbgr > 0, it counts all sources in group nbgr

      implicit none

      integer i,nbgr,counter
      type(sourcepair) group1(maxingroup)

      counter=0

      do i=1,maxingroup
        if (((nbgr.eq.0).and.(group1(i)%group_number.gt.0)) .or.
     >      (nbgr.ne.0).and.(group1(i)%group_number.eq.nbgr)) then
           counter=counter+1
        endif
      enddo

      group_count=counter

      end function group_count

! ****************************************************
! PRINT GROUP LIST
      subroutine group_out(luout,kall,knumber,kgrnb,lkind)
! write out the sources in $GROUP
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'

! History
!     2016Nov15 KLB - from astro_out.f

! passed
      integer luout
      logical kall      !list all sources, or just ones set.
      logical knumber   !number the sources
      character*1 lkind 
      integer kgrnb      ! group number to list. If =0, list of sources

! functions
      integer igetsrcnum  ! function to get the source number

! local
      integer i
      integer isrc

      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$GROUP"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      if(knumber) then
        write(luscn,'(a)') '#    SrcName Group #'
      endif 

      do i=1,NumInGroupList
       if(group_list(i)%group_number .gt. 0) then
         if ((kgrnb.eq.0) .or. 
     >       (kgrnb.eq.group_list(i)%group_number)) then
           isrc=igetsrcnum(group_list(i)%src_name)
           if(knumber) then
! JMG: Changed to output 'i', nand not isrc. 
             write(cbuf,'(i4," ",a," ",i4)') i, group_list(i) 
!     >                group_list(i)%src_name,group_list(i)%group_number
!       write(*,*) "Indice: ",i,iscr,csorna(isrc),csorna(i),
!     >                  group_list(i)%src_name,group_list(i)%group_number
           else 
             write(cbuf,'(a,1x,i4)') group_list(i)
!     >                group_list(i)%src_name,group_list(i)%group_number
!       write(*,*) "Indice: ",i,iscr,group_list(i)%src_name,
!     >                       group_list(i)%group_number
           endif
         call wrt_param_line(cbuf,luout,lkind) 
         endif
        endif
      end do

      end subroutine group_out

! ****************************************************
! PRINT OBS OF SOURCES IN GROUP SECTION
      subroutine group_obs(luout)

! display the sources in section $GROUP

! History
!    2016Nov17  KLB - from astro_obs


C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'

! passed
      integer luout

! local 

      Integer*4 NumObsSrc(Max_Sor)
      integer*4  NumScansSrc(Max_sor)
      Integer*4 NumObsTot,NumScansTot
      logical kfirst                    !first line output.

! functions
      integer igetsrcnum
!      integer group_count

! local
      integer i,isrc
      integer NumScansGr           !Number scans of sources in groups
      Integer NumObsGr             !Number obs 

      if(luout .le. 0) return

      call find_obs_per_src(NumObsSrc,NumScansSrc,
     >     NumObsTot,NumScansTot)
    
100   continue

! Now extract the sources in group section.
      kfirst=.true.
      NumObsGr=0
      NumScansGr=0
      do i=1,group_count(group_list,0)
        if(group_list(i)%group_number .eq. 0) then
            continue
        else
          if(kfirst) then
            write(luout,*)
     >        "Source   Gr#   Actual  #Num #Scans"
            kfirst=.false.
          endif
          isrc=igetsrcnum(group_list(i)%src_name)
          NumObsGr=NumObsGr+NumObsSrc(isrc)
          NumScansGr=NumScansGr+NumScansSrc(isrc)
          write(luout,'(a,1x,i3,1x,f8.2,1x,i6,1x,i4)') 
     >       group_list(i)%src_name, !csorna(isrc)(1:8),
     >       group_list(i)%group_number,
     >       dble(NumObsSrc(isrc))/dble(NumObsTot)*100.,
     >       NumOBsSrc(isrc),NumScansSrc(isrc)
        endif
      end do
      write(luout,'(a,1x,4x,f8.2,1x,i6,1x,i4)') "Total   ",
     > dble(NumObsGr)/dble(NumObsTot)*100.,NumObsGr,NumScansGr

      end subroutine 

! ****************************************************
! CULL SOURCES IN GROUP SECTION
      subroutine group_cull(luout,min_obs,rmin_ratio)
! Remove sources from astrometric list that
!    Either have <min_obs     OR  #obs/#scans   <rmin_ratio 

C   COMMON BLOCKS USED
 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'

! History:
!     2016Nov18 KOL - from astro_cull

! passed
      integer luout
      integer min_obs          !minimum # of accepatable obs
      real*8  rmin_ratio       !min ratio:   #obs/#scans

! functions
!      integer group_count      ! to count number of sources in group section
      integer igetsrcnum

! local
      Integer*4 NumObsSrc(Max_Sor)      
      integer*4 NumScansSrc(Max_sor)
      integer*4 NumObsTot, NumScansTot
      logical kfirst                    !first line output.
      integer i,isrc
    
! This is total over sources which are removed      
      integer NumScansGr 
      Integer NumObsGr 

      if(luout .le. 0) return

      write(*,'("Culling sources with numObs < ",i4)')   Min_obs
      write(*,'("         or  numObs/NumScan < ",f4.1)') rmin_ratio
 

      call find_obs_per_src(NumObsSrc,NumScansSrc,
     >     NumObsTot,NumScansTot)

100   continue

! Now extract the sources in group section
      kfirst=.true.
      NumObsGr=0
      NumScansGr=0
      do i=1,group_count(group_list,0)
 
        if(group_list(i)%group_number .le. 0) then
           continue
        else 
           isrc=igetsrcnum(group_list(i)%src_name)
           if (NumObsSrc(isrc) .lt. Min_obs .or. 
     >      (dble(NumObsSrc(isrc))/dble(NumScansSrc(isrc))
     >        .lt.rmin_ratio)) then
!            write(*,*) NumObsSrc(i),  NumScansSrc(i),
!     >            dble(NumObsSrc(i))/dble(NumScansSrc(i))
       
              if(kfirst) then
                 write(luout,*)
     >              "Source     Gr#  Actual  Num #Scans"
                 kfirst=.false.
              endif
              NumObsGr=NumObsGr+NumObsSrc(i)
              NumScansGr=NumScansGr+NumScansSrc(i)
              write(luout,'(a,1x,f8.2,1x,i5,1x,i4)') csorna(isrc)(1:8),
     >          dble(NumObsSrc(isrc))/dble(NumObsTot)*100.,
     >          NumOBsSrc(isrc),NumScansSrc(isrc)
! remove from group
              group_list(i)%src_name=""
              group_list(i)%group_number=0
           endif
        endif
      end do
      if(.not. kfirst) then
        write(luout,'(a,1x,16x,f8.2,1x,i5,1x,i4)') "Total   ",
     >    dble(NumObsGr)/dble(NumObsTot)*100.,NumObsGr,NumScansGr
      else
        write(luout,'(A)') "Did not cull any sources." 
      endif 

      end subroutine group_cull

! ****************************************************
! GROUP COMMAND CALL IN SKED
      subroutine group_cmd(cmdline)
! 
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'

! History
!    First version
!    2016Nov04  KLB - creation of the command from  astro_cmd.f
!    2017Feb14  JMG   Changed found-->kfound.  Changed (found.eq..false.)-->.not.kfound

! passed
      character*(*) cmdline
! functions
      integer istringMinMatch
      integer trimlen
      integer igetsrcnum
!      integer group_count
!      integer sourcepairdiff

! local.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)

      real*8 rmin_ratio     !minimum acceptable ratio for   #obs/#scan
      integer min_obs       !min # of obs

      integer icmdlen
      integer isrc
      integer i,j
      logical kfound 

      type(sourcepair) group_pair_in           !input source and group#
      type(sourcepair) group_pair_temp         !temporary swap variable

      integer group_numVal
      integer igr,groupNB
      character*128 ldum
      integer ind_newcm
      integer kgrnb

! Stuff dealing with finding which "Group command" to do.
      character*12 cmd
      equivalence (ltoken(1),cmd)
      integer ifunc
      integer ilist_len
      parameter (ilist_len=9)
      character*12 list(ilist_len)
      integer itemp 
      logical kall,knumber
! NOTE: READ must be the first one in the list. 
      data list/"READ","LIST","ADD","SET","DELETE","OBS","?","CULL","/"/
      

      icmdlen=trimlen(cmdline)
      if(icmdlen .eq. 0) then
        ifunc=1         !default is list
      else 
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)  
        ifunc = iStringMinMatch(list,ilist_len,ltoken(1))
      endif
! 'Read' is a hidden command only valid on input. 
      if(ifunc .eq. 1 .and. .not. kgroup_read_valid) ifunc=0

! Some kind of bad command. Write out valid commands, but omit 'READ'group
      if(ifunc .eq. 0) then
        write(luscn,'(A,a)')"Group_cmd: Command not found: ",ltoken(1)
        write(luscn,'("Valid commands are: ", 8(a,1x))') 
     >        (trim(list(i)),i=2,ilist_len) 
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"Group_cmd: Ambigous command: ",ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)

      select case (cmd)
!===== ?
         case ("?")
!            write(luscn,'(A)')"GROUP  [List | Obs | Add Src Group# |"//
!     >            " Set Src Group# | Delete Src | Cull Group# "//
!     >            "| / Group# [START <time>] [SUBNET <subnet>] "//
!     >            "[DUR <duration>] ]"
            write(luscn,'(A)')"GROUP  [List | Obs | Add Src Group# |"//
     >            " Set Src Group# | Delete Src | "//
     >            "/ Group# [START <time>] [SUBNET <subnet>] "//
     >            "[DUR <duration>] ]"
!===== LIST
         case ("LIST")
            kall=.false.
            knumber=.true.
            if (NumToken .lt. 2) then
              kgrnb=0
            elseif (NumToken .gt.4) then
              write(luscn,*) "Group_cmd: too many arguments"
            else
              read(ltoken(2),*,err=930) kgrnb
            endif
            call group_out(ludsp,kall,knumber,kgrnb,'d')    !output to the display      
!===== OBS
         case ("OBS")
            call group_obs(ludsp)
!===== CULL
         case ("CULL")
            write(luscn,*) "Under construction"
!            if(NumToken .lt. 2) then
!              write(luscn,*) "Group_cmd: must set MinObs#"
!              return
!            endif
!            rmin_ratio=1.   
!            read(ltoken(2),*,err=930) min_obs
!            if(NumToken .eq. 3) then
!              read(ltoken(3),*,err=935) rmin_ratio
!            else if(NumToken .eq. 4) then
!              goto 900 
!            endif
!            call group_cull(ludsp,min_obs,rmin_ratio)
!===== SCHEDULE A GROUP = number of tokens is at least 2 (/ and group#)
         case ("/") 
            if (NumToken.lt.2) then
              write(luscn,*) "Specify group number"
            else
              read(ltoken(2),*), groupNB
              if (groupNB.le.0) then
                 write(luscn,*) "Specify group# correctly"
              else
                 if (group_count(group_list,groupNB).le.0) then
                    write(luscn,*) "No source in this group: ",groupNB
                 else
                    if (NumToken.ge.3) then
                       ind_newcm=index(cmdline,trim(ltoken(3)))                  
                    endif
                    do igr=1,group_count(group_list,0)
                       if (group_list(igr)%group_number.eq.groupNB) then
                          if(NumToken .ge. 3) then 
                             ldum=group_list(igr)%src_name//" "//
     >                         trim(cmdline(ind_newcm:))
                          else
                             ldum=group_list(igr)%src_name
                          endif 
                          CALL NEWCM(ldum,0)
                       endif
                    enddo
                 endif
              endif
            endif
!===== READ DELETE ADD SET
!READ/DEL/ADD/SET  SRC Group#                   Number of tokens is 3
         case DEFAULT
            if (NumToken .eq.1) then
              write(luscn,*) "Group_cmd:  Must specify source name "
     >                       //"and group number"
              return 
            else if (NumToken .eq. 2) then
              write(luscn,*) "Group_cmd: must set group#"
              return
            else if(NumToken .gt. 4) then
              write(luscn,*) "Group_cmd:   too many arguments"
              goto 900
            endif
            ! read group number
            read(ltoken(3),*,err=900) group_numVal   !get the value for group_number
            if (group_numVal.lt.0) then
               write(luscn,*) "Group_cmd: value for group number is" 
     >                    ," negative!"
               goto 990
            else if (group_numVal.eq.0) then
               write(luscn,*) "Group_cmd: warning! Value is 0: the"
     >                    ," source is not taken into account."
               goto 990
            endif
            ! read source name
            isrc=igetsrcnum(ltoken(2))
            if(isrc .le.0) then
               write(luscn,*) "Group_cmd: Source not found ",ltoken(2) 
               return
            endif
            group_pair_in%src_name=csorna(isrc)
            group_pair_in%group_number=Group_numVal
            select case (cmd)
            !===== READ from SKED file
             case("READ")
              ! Assume that things are more or less in order when we read in.
              ! 1st insert at the end of the list.                
               NumInGroupList=NumInGroupList+1
               group_list(NumInGroupList)=group_pair_in
              ! Now do swaps if necessary to put it in the correct place.
               do  i=NumInGroupList,2,-1             !Start at end    
                if(SourcePairDiff(group_list(i),group_list(i-1)) 
     >                                                   .lt. 0) then
              ! This means that group_list(i) belongs before group_list(i-1).  Swap them. 
                   group_pair_temp=group_list(i)
                   group_list(i)=group_list(i-1)
                   group_list(i-1)=group_pair_temp
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
               do while (i.le.NumInGroupList.and. .not.kfound)
                   if (sourcepairdiff(group_list(i),group_pair_in).eq.0)
     >                then
                      do j=i+1,NumInGroupList
                         group_list(j-1)=group_list(j)
                      enddo               
                      kfound=.true.            
                      NumInGroupList=NumInGroupList-1
                  endif
                  i=i+1
               enddo
               if(.not. kfound) write(luscn,
     >          '("Group_cmd: Source ",a, " not found in group # ",i4)')
     >            trim(csorna(isrc)), group_numVal
!===== ADD/SET
             case ("ADD","SET")
               if (NumInGroupList .eq. 0) then
                  group_list(1)=group_pair_in
                  NumInGroupList=1
               elseif (NumInGroupList .lt. maxingroup) then
                    kfound=.false.
                    i=1
                    do while (i.le.NumInGroupList .and..not.kfound) 
                      if(sourcepairdiff(group_pair_in,group_list(i))
     >                                                       .lt.0) then
                    ! insert 
                        if (i.eq.NumInGroupList) then
                          group_list(i+1)=group_list(i)
                          group_list(i)=group_pair_in
                          NumInGroupList=NumInGroupList+1
                        else
                          do j=NumInGroupList+1,i+1,-1
                            group_list(j)=group_list(j-1)
                          enddo
                          group_list(i)=group_pair_in
                          NumInGroupList=NumInGroupList+1
                        endif
                        kfound=.true.
                      elseif
     >           (sourcepairdiff(group_pair_in,group_list(i)).eq.0) then
                        write(luscn,*) "Group_cmd: source and group ",
     >                                "number already in the list."
                        kfound=.true.
                      endif
                      i=i+1
                    enddo
                    if (.not.kfound) then ! we are at the end of the list
                      group_list(NumInGroupList+1)=group_pair_in
                      NumInGroupList=NumInGroupList+1
                    endif
               else ! NumInGroupList=maxingroup
                  write(luscn,*) "Group_cmd: revise size of ",
     >              "maxingroup. Too many sources."
                  return
               endif
!===== invalid commands
             case DEFAULT ! invalid command
               write(luscn,*) "Group_cmd: invalid command"
               return
            end select
      end select
      return 

! Different error conditions
900   continue
      write(luscn, *) "Group_cmd: Too many parameters"
      goto 990

930   continue
      writE(luscn,*) "Group_cmd: Erorr reading min_obs"
      goto 990

935   continue
      write(luscn,*) "Group_cmd: Error reading rmin_ratio"
      goto 990

990   continue
      write(luscn,*)  "         ",cmdline(1:trimlen(cmdline))

      end subroutine group_cmd

! ****************************************************
! 

      end module group_mod







       
   
      

