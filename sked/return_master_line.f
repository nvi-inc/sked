      subroutine return_master_line(cmaster_file,cexper,line,
     <  iyr_mst_start, kfound)
! Open a master file.
! Space through until we find the experiment.
! Return with it.
      implicit none
! Passed
      character*(*) cmaster_file    !master file
      character*(*) cexper          !experiment code
! returned  
      character*(*) line            !line corresponding to exper
      integer iyr_mst_start   
      logical kfound                ! if true, then found. If false,not found.    
! function
      integer trimlen 

! History
!  2007Nov20. First version. 
!  2015Oct23. Print update date of master file. 
!  2020Nov04. Print error message if we can't open the file. 
! 2022-12-15  JMGipson. Modified to handle new masterfile format
! 2023-01-13  JMGipson capitalize internal copy of cexper and check against capitalized masterfile version. 

! local
      character*20 cexper_cap              !capitalized version of cexper
! used to  extract tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=10)
      character*20 ltoken(MaxToken) 
      integer i   !count 
      character*20 cexper_master  
 
      integer ind 
      integer istat
      logical kintensive
    
      inquire(exist=kfound,file=cmaster_file)
      if(.not.kfound) return        !master file does not exist. That's a bummer.   
      
      cexper_cap=cexper
      call capitalize(cexper_cap)   !capitalize the experiment. This  makes checking easier below. 
!      write(*,*) cexper_cap
     

      kintensive = index(cmaster_file,"-int") .ne. 0
! Open the master file. Extract the station list, start & end times
      write(*,'("Checking ",a, $)')  trim(cmaster_file) 
      open(13,file=cmaster_file,err=500,iostat=istat)
 
  
! skip the header
      do i=1,10
        read(13,'(a)',err=200,end=200) line
        if(i .eq. 3) then
           read(line,*) iyr_mst_start      
        endif
        ind=index(line,"Last Updated")  
        if(ind .ne. 0)  then           
           if(.not. kintensive) ind=ind-4
           write(*,'(2x,a,$)') trim(line(ind:))                     
        endif 
!                          123456789x            
        if(line(1:10) .eq."----------") then      !Next line is start
           goto 90
        endif           
      end do   
 

! Parse a line that looks like:
! OLD FORMAT
! Spacing is arbitrary, but tokens separated by "|"
!    1          2      3    4   5    6   7
! |IVS-R1309 |R1309 |JAN02|  2|17:00|24|FtKkNyShTcWfWz                           |NASA|BONN|08JAN23|3.0 | XA |NASA|  20 |2150|
! -----OR----
! NEW FORMAT
!   1             2        3           4   5    6     7 
! |IVS-R1      |20230103|r11084      |  3|17:00|24:00|AgHbHtIsKeKkKvMaNsNyOnWzYg                    |NASA|BONN|        | XA |NASA| -48|

90    continue
      write(*,*) " " 
    
100   continue
! Now we are at the start of the sessions
      read(13,'(a)',end=200,err=200) line     
!      write(*,*) trim(line) 
          
      if(line(1:1) .ne. "|") goto 100
 
! Now remove the "|"s
      ind =index(line,"|")
      do while(ind .ne. 0)
        line(ind:ind)=" "
        ind =index(line,"|")
      end do

      call splitNtokens(line,ltoken,Maxtoken,NumToken)
      if(iyr_mst_start .ge. 2023) then
        cexper_master=ltoken(3) 
      else
        cexper_master=ltoken(2)
      endif 
      call capitalize(cexper_master) 
  
      if(cexper_cap .eq. cexper_master) then
         kfound=.true.
          close(13)
         return
       endif
       goto 100

200    continue
       kfound = .false.    
       close(13)      
       return

500    continue
       kfound = .false. 
       write(*,'("I/O error ",i4, " opening file. Skipping it.")') istat
       end 

       

