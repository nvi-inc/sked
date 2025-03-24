      subroutine read_sked_vex_line(ilen) 
! This reads a line  from the sked file, or from the $SCHEDULING_PARAMS section of the VEX file.    
! Assumes that the file is open and rewound, or that we have called SCHEDULING_PARAMS
!
! Note: This puts in ibuf, which is in drcom.ftni.
      implicit none
      include 'drcom.ftni'  
      integer ilen              !length of buffer. ilen=-1 means EOF, 
      integer ierr 
      integer fget_literal
      integer ind 
          
      cbuf=" "     
      if (.not.kvex) then ! read sk file first line
         CALL READS(lu_infile,ierr,IBUF,isklen,ilen,2)      
      else ! get first literal line 
         ilen=fget_literal(ibuf) 
         ind=index(cbuf,char(0))
         if(ind .ne. 0) cbuf(ind:ind)=" " 
      endif ! sk/vex
 !     write(*,*) "read_sked_vex_line ", ilen, " | ", cbuf(1:ilen) 
      
      end 
