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
      subroutine check_dbbc_setup(icode,ierr)
      implicit none  !2020Jun15 JMGipson automatically inserted.

! Some simple checking of dbbc.
! This checks to make sure that the lo frequencys for each IF are the same
! and that the filter used is the same.
! The first IF "A" connects to BBC01-BBC04. The lo frequencies of these BBCs should be the same.
! Also the filter frequencies should be the same.
      include 'hardware.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'drcom.ftni'
      include 'bbc_freq.ftni'

! Passed
      integer icode
! returned
      integer ierr    !0=> no error.  Anything else, some inconsistency.
! Local
      integer if_num
      integer ib      !counter over bbcs.
      integer ib0, ib_beg, ib_end
      character*1 lyesno
      logical kfreq_error, kfilter_error,kpol_error
      
 
      ierr=0
      do if_num=1,4     !this is over Ifs
        ib0=0
        ib_beg=(if_num-1)*4+1
        ib_end=if_num*4
! Initilize to no error for this IF.         
        kfreq_error=.false.
        kfilter_error=.false.
        kpol_error=.false. 
        
        DO ib=ib_beg, ib_end
          if(ibbc_present(ib,istn,icode) .gt. 0) then    !Check if BBC is present 
! if so, check for some errors. 
            if(flo(ib_beg) .ne. flo(ib)) 
     >         kfreq_error=.true.
            if(ibbc_filter(ib_beg) .ne. ibbc_filter(ib)) 
     >         kfilter_error=.true. 
            if(cbbc_pol(ib_beg) .ne. cbbc_pol(ib)) 
     >         kpol_error=.true.
          endif           
        end do 
        if(kfreq_error .or. kfilter_error .or. kpol_error) then
          ierr=1                                                           
! write the kind of error     
          call write_return_if_needed(luscn, kwrite_return)  
          write(luscn, '("DBBC_error for IF# ",i3)') if_num
          if(kfreq_error)
     >       write(luscn,'(a)')   "   Inconsistent lo frequencies"
          if(kfilter_error)
     >        write(luscn,'(a)')  "   Inconsistent filters"           
          if(kpol_error) 
     >        write(luscn,'(a)')   "   Inconsistent polarizations"      
          write(luscn, '(a)') "   BBC#    Freq   Filter  Pol "
          do ib=ib_beg,ib_end
            if(ibbc_present(ib,istn,icode) .gt. 0) then        
              write(luscn, '(i5,2x, f10.2,2x,i2,4x, a3)') 
     >         ib, flo(ib), ibbc_filter(ib), cbbc_pol(ib)
            endif
          end do    
         endif        
       end do
       if(ierr .ne. 0) then
         lyesno="G"
         do while(lyesno .ne. "Y")
           write(luscn,'(a)')
     >    "ERROR! 'prc' file will need to be fixed! Continue on (Y/N)?"
           read(*,'(a)') lyesno
           call capitalize(lyesno)
           if(lyesno .eq. "N") then
             ierr=1
             return
           endif
         end do
         ierr=0
       endif

      return
      end

