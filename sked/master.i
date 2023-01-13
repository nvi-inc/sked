! 2023-01-11 John Gipson. 
!It contains information about the number and kinds of master files.
! This should be ordered from most likely to least likely.
! A master file is named somehting like 
!   master96-foo.txt     where the '-foo' indicates the kind of file. 
! For simplicity in the code we include the '.txt' part. 
      integer, parameter :: num_master_type=4
      character*10  lmaster_type(num_master_type)
      data  lmaster_type/".txt ",      
     &                   "-int.txt",   
     &                   "-vgos.txt", 
     &                   "-lcl.txt"   / 
     
! Masterfiles began in 1979, but not all types have masterfiles for all years.  
!    Normal masterfiles run from 1979  and continue. I put 2100 as the ending date. 
!    Intensive masteriles start in 1991 and continue 
!    VGOS masterfiles start in 2013 and end in 2018
!    GSFC does not have any local masterfiles which is why the 'lcl' type has the range it does.  
     
! First and last year to look for corresponding masterfiles     
      integer imaster_year_beg(4),imaster_year_end(4) 
 
      data imaster_year_beg/1979, 1991, 2013, 2100/
      data imaster_year_end/2100, 2100, 2018, 2100/
