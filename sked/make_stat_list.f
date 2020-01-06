      subroutine make_stat_list(cdo,ierr)
! Read in antenna catalog, and find matches in current schedule.
! history Now put in reverse order.
!  2017Dec04  JMGipson. Fixed buf if a) equipment mis-match and b) more than one set of equip for station in catalog
!  2005Aug10 JMGipson. First version.  removed from stcat.
!  2005Sep14 JMGipson.  Be more forgiving if cstrec and strack.
!              assume "VLBA" = "VLBA4"
!  2005Nov07 JMGipson.  Ooops. A bug in the above fix.  Now fixed.
!               Also issue better warning messages.
!  2006Jan18 JMGipson.  Modified so that when checking if recorders match between
!               schedule and catalog, Mark5A in the catalog matches any recorder.
!              Also modified warning messages.
!  2006Nov30 Use cstrec(istn,irec)
!  2008Nov11 JMGipson.  Added "kyes_to_prompt"
!  2010Apr21 JMGipson.  If nobs<>, then preserve 1 character station ID, else use catalog.
!                       This allows you to add in a station to tag it along.
      implicit none

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_stat.ftni'
      include '../skdrincl/valid_hardware.ftni'
      include '../skdrincl/skobs.ftni'

! Passed
      character*1 cdo !s=standard, a=automatic. find current mode, and exit.
! Returned
      integer ierr
! Function
      logical kyes_to_prompt

! Local
      character cans
      integer iptr,iptr0
      integer istat
      integer irack_ptr
      integer irec_ptr
      logical kstat_found   !Station is found.
      logical kequip_found  !Also equipment
      integer num_missing   !number of missing stations.
      logical kvlba_or_vlba4
      integer num_match
      logical kstop_asking

      kstop_asking=.false.

      ierr=0
      call read_antenna_cat(ierr)
      if(ierr .ne. 0) return
      kcat_stat_sel=.false.

! Now search for matches between catalog and current stations.
      num_missing=0
      do istat=1,nstatn
        kstat_found=.false.
        kequip_found=.false.
        num_match=0
        do iptr=1,num_cat_stat
          if(cat_equip_band(icat_stat_vec(4,iptr)) .ne. "XS") goto 50   
          if(cantna(istat) .ne.cat_ant_name(icat_stat_vec(1,iptr))) 
     >                                                           goto 50
  
          kstat_found=.true.
          irack_ptr=icat_stat_vec(2,iptr)
          irec_ptr =icat_stat_vec(3,iptr)
          if(cstrack(istat).eq.crack_type(irack_ptr) .and.
     >      cstrec(istat,1) .eq.crec_type(irec_ptr)) then
            kequip_found=.true.
            goto 100
          else if(cstrack(istat) .eq. crack_type(irack_ptr) .and.
     >      cstrec(istat,1) .eq. "Mark5A") then   !Mark5A is wildcard.
            kequip_found=.true.
            goto 100
          else if((kvlba_or_vlba4(cstrack(istat))       .and.
     >              kvlba_or_vlba4(crack_type(irack_ptr))) .and.
     >            (kvlba_or_vlba4(cstrec(istat,1)) .and.
     >               kvlba_or_vlba4(crec_type(irec_ptr)))) then
            write(luscn,
     >        '("MAKE_STAT_LIST: Warning! For station ",a,$)')
     >             cantna(istat)
            write(luscn,*) " Slight mismatch in equipment: "
            write(luscn,*) "          Rack   Recorder"
            write(luscn,'("Schedule:  ",a8," ",a8)')
     >         cstrack(istat),cstrec(istat,1)
            write(luscn,'("equip.cat: ",a8," ",a8)')
     >         crack_type(irack_ptr),crec_type(irec_ptr)
            write(luscn,*) "This should not effect the schedule "
            kequip_found=.true.
            goto 100
          else if(crec_type(irec_ptr) .eq. "S2") then 
            continue 
          else
            iptr0=iptr
            num_match=num_match+1
          endif
50        continue    !fast exit

        end do

100     continue
        if(.not.kstat_found) then
          write(luscn,*) "MAKE_STAT_LIST: Did not find antenna ",
     >          cantna(istat), " in anntenna catalog."
          num_missing=num_missing+1
        else if(kequip_found) then
          kcat_stat_sel(iptr)=.true.
          if(nobs .ne. 0) 
     >          cat_ant_id1(icat_stat_vec(1,iptr))=cstcod(istat)
        else  !station found, but not equipment.
          if(num_match .eq. 0) then
             write(luscn,*) "MAKE_STAT_LIST: Equipment not found for ",
     >          cantna(istat), " in equipment catalog."
             num_missing=num_missing+1
          else if(num_match .eq. 1 .and. kstop_asking) then
             kcat_stat_sel(iptr0)=.true.
              if(nobs .ne. 0) 
     >             cat_ant_id1(icat_stat_vec(1,iptr0))=cstcod(istat)
          else if(num_match .ge. 1) then 
! a match for the antenna type found.
            write(luscn,'("MAKE_STAT_LIST: Warning! For station ",a,$)')
     >               cantna(istat)
             write(luscn,*) " mismatch in equipment."
            write(luscn,*) "          Rack   Recorder"
            write(luscn,'("Schedule:  ",a8," ",a8)')
     >          cstrack(istat),cstrec(istat,1)
            write(luscn,'("equip.cat: ",a8," ",a8)')
     >          crack_type(irack_ptr),crec_type(irec_ptr)
            cans="X"
            write(luscn,*)"Use catalog equipment? (Y/N/A (A=Y to rest))"
            do while(cans.ne."Y".and.cans.ne."N".and.cans.ne."A")
              call read_cap_char(cans)
              if(cans .eq. "Y" .or. cans .eq. "A") then
                kcat_stat_sel(iptr0)=.true.
                if(nobs .ne. 0) 
     >             cat_ant_id1(icat_stat_vec(1,iptr0))=cstcod(istat)
                kstop_asking=CANS .eq. "A" 
              else if(cans .eq. 'N') then
                num_missing=num_missing+1
              endif
            enddo                        
          endif
        endif
      end do

      
      if(num_missing .gt. 0) then
        ierr=10
        write(luscn,*)
     >   "ERR: MAKE_STAT_LIST: Some stations not found in the catalog."
        if(cdo .eq. "A" .or. cdo .eq. "a") return
        write(luscn,*) " This information will be lost if you proceed."
        if(.not. kyes_to_prompt("Continue? (Y/N) ")) return       
      endif
      ierr=0
      end
****************************************************
      logical function kvlba_or_vlba4(cstring)

      character*8 cstring

      kvlba_or_vlba4=cstring.eq."VLBA" .or. cstring.eq. "VLBA4"
      return
      end




