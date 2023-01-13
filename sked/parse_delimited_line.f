      subroutine parse_delimited_line(ldum,ldelimiter, ifield, lvalue,ierr)
      implicit none
! passed         
      character*(*) ldum         !line from master file. 
      character*1 ldelimiter     !The delimiter. Can be comma, "|", etc. 
      integer ifield             !which field we want returned.  
! returned         
      character*(*) lvalue       !value of the field.  This is left justified. 
      integer ierr               !<>0 some error.    
! read a string (ldum) and return the value in ifield.  The string is delimited by ldelimiter. 
      
                   
! 2023-01-12 John Gipson first version.
!    
!Origianlly written to parse a line from the maser file that looks something like this     
! We strip off the leading "|" before entry. 
!   arg1      2       3    4   5    6   7                                    8    9   10      11   12   13    14    15 
!|IVS-R4927 |R4927 |JAN02|  2|18:30|24|AgFtKkNyWnWzYg                      |USNO|WASH|20JAN17|1.0 | XE |USNO|  14 |4927|

! local
       integer num_delim   
       integer ibeg,iend
       integer ind 
       integer i
       
! Default no error condition. 
       ierr=0
! check some pathalogical cases
! First character is the delimiter. This means first argument is " " 
       if(ldum(1:1) .eq. ldelimiter) then
         if(ifield .eq. 1) then
           lvalue=" "
           return
         endif
       endif 
       
!       write(*,'(a)') "ldum: "//ldum 

       ibeg=1
       iend=0 
       num_delim=0 
! Find location of delimiter
       do i=1,len(ldum)
          if(ldum(i:i) .eq. ldelimiter) then
             num_delim=num_delim+1
             if(num_delim .eq. ifield) then
                iend=i-1 
                exit      
             endif
             ibeg=i+1             
          endif     
       end do     
!       write(*,*) "ibeg, iend ", ibeg, iend                  
       
       if(iend .eq. 0) then     !did not find field. This is OK if last field.       
         if(num_delim+1 .eq. ifield) then
            iend=len(ldum)
         else
            ierr=-1             !Not the last field. Not enough fields. 
            return
         endif              
       endif        
       
! This will happen if we have two delimiters next two each other, e.g. ",," 
! Then ibeg will point to second ","  while iend points to first.  

       if(iend .lt. ibeg) then
          lvalue=" "
       endif 
 
 ! now we copy appropriate field to the value       
       if(ldum(ibeg:iend) .eq. " ") then   !all blanks
         continue 
       else
         do while(ldum(ibeg:ibeg) .eq. " ")  !get rid of leading spaces. 
           ibeg=ibeg+1
         end do
       endif 
       lvalue=ldum(ibeg:iend) 
  
       return
       end 
                
         
         
         
         
         
         
