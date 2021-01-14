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

! local
! used to  extract tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=10)
      character*20 ltoken(MaxToken) 
      integer i   !count 
      character*8 cexper_master  
      integer ind 
      integer istat
      logical kline_closed
      logical kintensive
    
      inquire(exist=kfound,file=cmaster_file)
      if(.not.kfound) return        !master file does not exist. That's a bummer.   

      kintensive = index(cmaster_file,"-int") .ne. 0
! Open the master file. Extract the station list, start & end times
      write(*,'("Checking ",a, $)')  trim(cmaster_file) 
      open(13,file=cmaster_file,err=500,iostat=istat)
 
      kline_closed=.false. 
! skip the header
      do i=1,10
        read(13,'(a)',err=200,end=200) line
        if(i .eq. 3) then
           read(line,*) iyr_mst_start      
        endif
        ind=index(line,"Last Updated")  
        if(ind .ne. 0)  then           
           if(.not. kintensive) ind=ind-4
           write(*,'(2x,a,$)') line(ind:ind+50)        
           
           kline_closed=.true.
        endif 
      end do   
      write(*,*) " " 

! Parse a line that looks like:
! Spacing is arbitrary, but tokens separated by "|"
!    1          2      3    4   5    6   7
! |IVS-R1309 |R1309 |JAN02|  2|17:00|24|FtKkNyShTcWfWz                           |NASA|BONN|08JAN23|3.0 | XA |NASA|  20 |2150|
    
100   continue
! Now we are at the start of the sessions
      read(13,'(a)',end=200,err=200) line 
 
          
      if(line(1:1) .ne. "|") goto 100
 
! Now remove the "|"s
      ind =index(line,"|")
      do while(ind .ne. 0)
        line(ind:ind)=" "
        ind =index(line,"|")
      end do

      call splitNtokens(line,ltoken,Maxtoken,NumToken)
      cexper_master=ltoken(2)
      if(cexper .eq. cexper_master) then
         kfound=.true.
          close(13)
         return
       endif
       goto 100

200    continue
       kfound = .false.   
       if(.not. kline_closed) write(*,*) " " 
       close(13)      
       return

500    continue
       kfound = .false. 
       write(*,'("I/O error ",i4, " opening file. Skipping it.")') istat
       end 

       

