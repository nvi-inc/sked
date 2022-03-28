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
C@READF_ASC

       subroutine readf_asc (iunit,kerr,ibuf,ibl,il)
C
C  ASCII only version of READF
      implicit none
C 880523  -written by P. Ryan
C 960212 nrv Extend buffer
C 000907 nrv Call IFILL with IBL instead of 80
! 2020Sep14 JMGipson. Cleanup, get rid of some obsolete stuff. 
! 2022-03-19 JMGipson. Basically rewritten


C  Input:
       integer iunit    !logical unit for reading
       integer kerr     !variable to return error on input (nonzero if error)
       integer ibl      ! buffer length
C
C  Output:
       integer il      ! number of characters read in
       integer*2 ibuf(*) !buffer that stuff is stored in. 

C  Local:
       character*1024 cbuf_in  ! character buffer for initial input
       integer*2 ibuf_in(512)
       equivalence (cbuf_in,ibuf_in) 
       
       integer    trimlen      ! find number of character read in
       integer i               ! counter
       integer oblank
       data oblank /O'40'/

! Fill the buffer with blanks   
       il=-1                               !only true if somekind of error.     
       call ifill(ibuf,1,ibl,oblank)
       read(iunit,'(a1024)',end=20,iostat=kerr) cbuf_in
       if(kerr .ne. 0) return                            !return on I/O error
       
       if(cbuf_in .eq. " ") then                         !retron if blank. 
         il=0
         return
       endif 
     
       il   = trimlen(cbuf_in)                    
!get rid of CR 
       if(il .gt. 0 .and. cbuf_in(il:il) .eq. char(13)) then  !is the last character CR
         cbuf_in(il:il)=" "
         il=il-1 
         if(il .eq. 0) return 
       endif

! Fill the output buffer.        
       il=(il+1)/2
       do i=1,il
         ibuf(i)=ibuf_in(i)
       end do 
! EOF reached        
20     continue  
       kerr = 0
       return

       end

