      subroutine read_antenna_cat(ierr)
! routine to open antenna catalog file, and read info into memory.
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/valid_hardware.ftni'

      include 'cat_stat.ftni'
! passsed
      integer ierr
! function
      integer iwhere_in_string_list

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)
! Used to hold equip.cat info in memory.
      integer num_equip,max_equip
      parameter (max_equip=300)
      character*8 lequip_stat(max_equip)  !station identifier
      character*8 lequip_term(max_equip)  !Terminal ID.
      integer iequip_rec(max_equip)       !Recorder
      integer iequip_rack(max_equip)      !rack
      integer iequip_band(max_equip)      !rack
! Other miscellaneous
      integer istat
      logical keof
      character*8 ltemp
      integer iwhere

!  2005Jun02 JMGipson   First version.
!  2006Jul27 JMGipson.  Also process & store terminal ID from Equipment catalog.
!  2008Dec23 JMGipson.  Check recorder type
! 2019Sep03 JMG.  Added implicit none 

      if(kcat_stat) return                  !Already read.
      call open_Cat(antenna_cat,ierr)
      if(ierr .ne. 0) then
         close(lutmp)
         return
      endif
      
      IF (IERR.NE.0) RETURN
! Now read it in, a line at a time.
      num_cat_ant=0
      num_cat_stat=0
100   continue
      call skip_to_next_non_comment(keof)
      if(keof) goto 190

      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      num_cat_ant=num_cat_ant+1
      cat_ant_id1(num_cat_ant) =ltoken(1)
      cat_ant_name(num_cat_ant)=ltoken(2)
      cat_ant_id2(num_cat_ant) =ltoken(14)
      goto 100

190   continue
      close(lucat)

! *************************************************************
! Open up the equipment catalog, and do read it in.
      call open_Cat(equip_cat,ierr)
      if(ierr .ne. 0) then
         close(lutmp)
         return
      endif

      num_cat_stat=0
      num_equip=0
      num_equip_band=1
      cat_equip_band(1)="__"

200   continue
      call skip_to_next_non_comment(keof)
      if(keof) goto 290

      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)

      num_equip=num_equip+1
      lequip_stat(num_equip)=ltoken(1)
      lequip_term(num_equip)=ltoken(2)

! Get the bands.
      ltemp=ltoken(6)(1:1)//ltoken(8)(1:1)
      iwhere=iwhere_in_string_list(cat_equip_band,num_equip_band,ltemp)
      if(iwhere.eq.0) then
        iwhere=num_equip_band+1
        num_equip_band=iwhere
        cat_equip_band(iwhere)=ltemp(1:2)
      endif
      iequip_band(Num_equip)=iwhere

! Now find the rack type and rec type.  For most lines these are the last 2 entries.
! Some lines don't have entries-- not to worry. These are assigned "unknown" since
! they won't match.
      ltemp=ltoken(NumToken-1)
      call capitalize(ltemp)
!      write(*,*) "RACK ", ltemp 
      if(ltemp .eq. 'DBBC') ltemp='DBBC_DDC'
 
      iwhere=iwhere_in_string_list(crack_type_cap,max_rack_type,ltemp)      
      if(iwhere.eq.0) then
        iwhere=max_rack_type
      endif
!      write(*,*) iwhere, crack_type_cap(iwhere) 
      iequip_rack(Num_equip)=iwhere
!      write(*,*) crack_type_cap
!      if(ltemp .eq. "DBBC_DDC") stop 
     
      ltemp=ltoken(NumToken)
      call capitalize(ltemp) 
!      call check_rec_type(ltemp) 
      iwhere=iwhere_in_string_list(crec_type_cap,max_rec_type,ltemp)
    
      if(iwhere.eq.0) then
        iwhere=max_rec_type
      endif
      iequip_rec(Num_equip)=iwhere

! Now we match the station in the equip.cat with station in antenna.cat
      istat=iwhere_in_string_list(cat_ant_name,Num_cat_ant, ltoken(1))
      if(istat .ne. 0) then
        if(num_cat_stat .eq. max_cat_stat) then
            write(*,*) "Read_antenna_cat: Out of space!"
            write(*,*) "Recompile and increase max_cat_stat"
         endif
         num_cat_stat=num_cat_stat+1
         icat_stat_vec(1,num_cat_stat)=istat
         icat_stat_vec(2,num_cat_stat)=iequip_rack(num_equip)
         icat_stat_vec(3,num_cat_stat)=iequip_rec(num_equip)
         icat_stat_vec(4,num_cat_stat)=iequip_band(num_equip)
         cat_term(num_cat_stat)=lequip_term(num_equip)
       endif             
      goto 200

290   continue
      close(lucat)

! Now we do a match between antenna.cat and equip.cat.
! The only complicated thing is that there may be more than one match, indicating
! more than one equipment type at a station.

300   continue
      goto 320 
      num_cat_stat=0
      do istat=1,num_cat_ant
        iwhere=iwhere_in_string_list(lequip_stat,Num_Equip,
     >    cat_ant_name(istat))
        if(iwhere .eq. 0) then
          continue
        else
          do while(lequip_stat(iwhere) .eq. cat_ant_name(istat))
            num_cat_stat=num_cat_stat+1
            if(num_cat_stat .gt. max_cat_Stat) then
               write(*, *) "Read_antenna_cat01: Out of space!"
               stop
            endif
            icat_stat_vec(1,num_cat_stat)=istat
            icat_stat_vec(2,num_cat_stat)=iequip_rack(iwhere)
            icat_stat_vec(3,num_cat_stat)=iequip_rec(iwhere)
            icat_stat_vec(4,num_cat_stat)=iequip_band(iwhere)
            cat_term(num_cat_stat)=lequip_term(iwhere)
            iwhere=iwhere+1
          end do
        endif
      end do

320   continue

      if(iverbose_level.ge.5) then
        write(luscn,
     >   '("Read_antenna_cat: num_ants/max_ants:   ",i4,"/",i4)')
     >      num_cat_ant,max_cat_ant
        write(luscn,
     >   '("Read_ant_cat: num_stats/max_stats: ",i4,"/",i4)')
     >      num_cat_stat,max_cat_stat
      endif

      kcat_stat=.true.
      return
      end

