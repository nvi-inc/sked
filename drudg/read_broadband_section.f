*
* Copyright (c) 2020 NVI, Inc.
*
* This file is part of VLBI Field System
* (see http://github.com/nvi-inc/fs).
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
*
      subroutine read_broadband_section(ivexnum) 
! Read the broadband section from schedule file. 
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'drcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni' 

! 2023-10-02 JMGIpson. Modified, based on dprrd.f 
! 2020Jun08.  Added in ibb_off parameter.  Added in new broadband.ftni 



! function
      integer iwhere_in_string_list
      integer trimlen 
      integer fget_literal,ptr_ch,fget_all_lowl

! local     
      integer istat   
      integer ilen 
      integer ivexnum 

      integer NumToken,MaxToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)           
     
! Intiailze broadband stuff     
      do istat=1,nstatn
         bb_bw(istat) =0.0       !set these all to 0. 
         idata_mbps(istat)=0
         isink_mbps(istat)=0
         ibb_off(istat)=0 
      end do 
      
! Initialize location in file. 
      if (.not.kvex) then !
        rewind(lu_infile)        
      else ! find SCHEDULING_PARAMS literal
        ilen=fget_all_lowl(ptr_ch(char(0)),ptr_ch(char(0)),
     .  ptr_ch('literals'//char(0)),
     .  ptr_ch('SCHEDULING_PARAMS'//char(0)),ivexnum)
        if (ilen.lt.0) return
        kgeo = .true.
      endif ! $PARAM or SCHEDULING_PARAMS
      
! This reads a line  from the sked file, or from the $SCHEDULING_PARAMS section of the VEX file.      
      cbuf(1:6) = "$foo"
      do while(cbuf(1:10) .ne. "$BROADBAND") 
        call read_sked_vex_line(ilen)
        if(ilen .lt. 0) return      !EOF            
      end do     
!      write(*,*) "Found broadband section" 
! now read the first line of the broadband section        
      call read_sked_vex_line(ilen)    
     
      do while(cbuf(1:1) .ne. "$")              !"$" means start of next section.        
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)           
        istat=iwhere_in_string_list(cstnna,nstatn,ltoken(1))
!        write(*,*) "NuMToken ", NumToken,  "| ", ltoken(1:NumToken) 
        if(istat .ne. 0) then
         if(NumToken .ge. 2) then   
           read(ltoken(2), *,err=550) bb_bw(istat)
         endif
         if(NumToken .ge. 3) then    
            read(ltoken(3),*, err=550) idata_mbps(istat)
         endif   
         if(NumToken .ge. 4) then 
           read(ltoken(4),*,err=550)  isink_mbps(istat)
         end if
         if(NumToken .ge. 5) then     
           read(ltoken(5),*,err=550)  ibb_off(istat) 
         endif 
        endif 
        call read_sked_vex_line(ilen)
        if(ilen .le. 0) return           !EOF 
      end do  
500   continue 
      return

550   continue
      write(*,*) "Error reading broadband section on line: "
      write(*,*) cbuf(1:trimlen(cbuf))


      return
      end 
