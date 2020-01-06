      SUBROUTINE STCMD(cmdline) 
C
C   STCMD determines the function requested in the STATIONs command,
C              and then calls the appropriate subroutine to do it.
C
C
C   COMMON BLOCKS USED
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
C History
C     880314 NRV DE-COMPC'D
C 951017 nrv Change igtky call to use lkey
C 170912 klb Added ADD and DEL options to the command + change to use tokens/cmdline copied from statwt_cmd
! 2018Dec22 JMG.  Added implicit none. Renamed del_stat -->kdel_stat. (Logical start with 'k')
C
C
C     CALLING SUBROUTINES: SKED (main program)
C     CALLED SUBROUTINES: splitNtokens (to decode the function)
C                         STLIS (to list the stations selected)
C                         STSEL (to select stations)
C
! passed
      character*(*) cmdline ! cmdline - input string from user, beginning after the command
! function
      integer istringMinMatch
      integer trimlen
! local variables
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)
      integer icmdlen
      integer istat
      integer*2 linestq(ibufq_len)
      character*(ibufq_len*2) cmdline2
      equivalence (linestq(2),cmdline2)
      integer ierr

! finding which stcmd command to do
      character*12 cmd
      equivalence (ltoken(1),cmd)
      integer ifunc
      integer ilist_len
      parameter (ilist_len=6)
      character*12 list(ilist_len)
      data list/"LIST","SELECT","ADD","DELETE","REMOVE","?"/

      character*1 cme
!
      integer i,iptr,i2
      integer inum_stat
      character*2 cdo,cdo1 ! open catalog
      character*2 cpo_temp(max_stn)
      character*2 cpo_temp2(max_stn)
      character*2 stat_temp
      integer char_temp 
      character*2 cptemp
      logical kdel_stat
      integer ntemp
C
      icmdlen=trimlen(cmdline)
      if(icmdlen.eq.0) then
        ifunc=1          ! default is list
      else
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
        ifunc=iStringMinMatch(list,ilist_len,ltoken(1))
      endif

! some kind of bad command
      if(ifunc .eq. 0) then
        write(luscn,'(A,a)')"Stcmd: Keyword not found: ",ltoken(1)
        return
      else if (ifunc .eq. -1) then
        write(luscn,'(A,a)')"Stcmd: Ambiguous keyword: ",ltoken(1)
        return
      endif

! start of valid command
      cmd=list(ifunc)
      if(cmd.eq."?") then
        write(luscn,'(A)')"Station [List | Sel | Add <station> "//
     >  "| Del <station>]"
        return
      elseif (cmd.eq."LIST") then
        call STLIS
        return
      elseif (cmd.eq."SELECT") then
        write(luscn,9101)
9101    format('STCMD - You may also use the command CATALOG ',
     .  'START to select stations.')
        cme='s'
        CALL STSEL(cme)
        RETURN
      elseif (cmd.eq."ADD" .or. cmd.eq."DELETE" .or. 
     >            cmd .eq. "REMOVE") then
! need to read the names of the station(s)
! number of tokens is 2: ADD/DEL/REM station

        if(NumToken .eq. 1) then
          write(luscn,'(a)') "Stcmd: Must specify station. "
          return
        endif

        if(NumToken .ne. 2) then
          write(luscn,'(a)') "Stcmd: Wrong number of arguments. "
          return
        endif

        if (cmd.eq."ADD") then

          inum_stat=nstatn
          cpo_temp(1:inum_stat)=cpocod(1:inum_stat) 

          ntemp=trimlen(ltoken(2))
          do i=1,ntemp,2
            cptemp=ltoken(2)(i:i+1)
            istat=istringminmatch(cpocod,nstatn,cptemp) ! check 2-letter name
            if (istat.gt.0) then ! the station is already in the list of stations
              write(luscn,*)
     >              "Stcmd: Station already in the network:",cptemp
            else
              char_temp=ichar(cptemp(2:2))
              stat_temp=cptemp(1:1)//char(char_temp+32)
              inum_stat=inum_stat+1
              cpo_temp(inum_stat)=stat_temp
            endif
          enddo

          if (inum_stat.eq.nstatn) return ! no new station to add

          cdo="a"
          call make_stat_list(cdo,ierr)

          kcat_stat_sel=.false.
          do i=1,inum_stat 
            do iptr=1,num_cat_stat
             if (cpo_temp(i).eq.cat_ant_id2(icat_stat_vec(1,iptr))) then
                   kcat_stat_sel(iptr)=.true.
                   goto 310
             endif
            enddo
 310      continue
          enddo
          call wrsts(ierr)
          cdo1='m'
          call stsel(cdo1)

        elseif (cmd.eq."DELETE".or.cmd .eq. "REMOVE") then
          if (nstatn.eq.0) then
            write(luscn,'(a)')"Stcmd: No station in the current list "//
     >             "- can not delete."
            return
          endif

          kdel_stat=.false. ! boolean to indicate if there is at least one station to delete

          inum_stat=nstatn
          cpo_temp2(1:inum_stat)=cpocod(1:inum_stat)

          ntemp=trimlen(ltoken(2))
          do i=1,ntemp,2
            cptemp=ltoken(2)(i:i+1)
            istat=istringminmatch(cpo_temp2,inum_stat,cptemp) ! check 2-letter name
            if (istat.le.0) then ! the station is not in the list of stations
              write(luscn,'(a,a)')"Stcmd: Did not find station in the"//
     >              " current list: ",cptemp
            else
              kdel_stat=.true. ! there is at least one station to delete
              char_temp=ichar(cptemp(2:2))
              stat_temp=cptemp(1:1)//char(char_temp+32)

! first, delete all observations for this station
              cmdline2="_ "//stat_temp
              linestq(1)=6  
              CALL CHCMD(linestq,'RM') 
! then, modifying the list of stations
              if (istat.eq.1) then ! the stat to delete is first in the list
                do i2=1,nstatn-1
                  cpo_temp(i2)=cpo_temp2(i2+1)
                enddo
              elseif (istat.eq.nstatn) then ! the stat to delete is last in the list
                do i2=1,nstatn-1
                  cpo_temp(i2)=cpo_temp2(i2)
                enddo
              else ! the stat to delete is anywhere but not first or last
                do i2=1,istat-1
                  cpo_temp(i2)=cpo_temp2(i2)
                enddo
                do i2=istat,nstatn-1
                  cpo_temp(i2)=cpo_temp2(i2+1)
                enddo
              endif
              inum_stat=inum_stat-1
              cpo_temp2=cpo_temp
            endif
          enddo

          if (kdel_stat) then
            cdo="a"
            call make_stat_list(cdo,ierr)

            kcat_stat_sel=.false.
            do i=1,inum_stat
              do iptr=1,num_cat_stat
             if (cpo_temp(i).eq.cat_ant_id2(icat_stat_vec(1,iptr))) then
                 kcat_stat_sel(iptr)=.true.
                 goto 311
               endif
              enddo
 311        continue
            enddo
            call wrsts(ierr)
            cdo1='m'
            call stsel(cdo1)

          endif

        endif

      endif

      return
      end

