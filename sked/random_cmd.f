      subroutine random_cmd(cmdline_in,nsourc)
! Schedule a random source between 1 and nsourc
      character*(*) cmdline_in
!function
      integer iget_random_integer     !return a random number 

! Local
      character*80 cmdline
      integer isrc 

! Used for tokens
      integer MaxToken
      integer NumToken
      parameter(MaxToken=4)
      character*82 ltoken(MaxToken)   !has to be long for station list.
    
      character*64 lsubnet  
      integer*4 itemp
      integer ilen
      logical kby_iter
      integer num_iter, iter 
      integer*4 itemp4

      call random_seed()     !reset random number generator

! Default. 
      num_iter=1                 !One iteration
      lsubnet="_"                !All stations
      kby_iter=.true.            !Doing by iterations, not by time
    
      call splitNtokens(cmdline_in ,ltoken,Maxtoken,NumToken)
      if(NumToken .eq. 0) goto 100  

      if(NumToken .eq. 1 .and. ltoken(1) .eq. "?") then 
          write(*,*) "Syntax: "
          goto 510 
      else if(NumToken .eq. 2) then 
         lsubnet=ltoken(2)
      endif


! Have either 1 or two arguments. In any case, read the first argument. 
      ilen=len_trim(ltoken(1))          !this can be either a time or number of iterations
      if(ilen .le. 3) then              !Maximum number of iterations is 999
         read(ltoken(1), *) num_iter 
      else
         kby_iter = .false.             !Doing by time, not iterations. 
      endif 
    
      iter = 0 
 
100   continue 
      do while(.true.) 
        call random_number(rtemp)
        isrc=(nsourc+1)*rtemp              
        write(cmdline,'(i4," subnet ",a)') isrc,trim(lsubnet)
!        write(*,*) cmdline 
         CALL NEWCM(cmdline,0)
         iter=iter+1 
        write(*,*) "Iteration: ", iter 
        if(kby_iter) then
            if(iter .ge. num_iter) return
         else
            write(*,*) "SHould not get here yet!"
            stop
         endif 
      end do
      return

500   continue
      write(*,*) "Syntax error "
510   continue 
      write(*,*) "Random Subnet [#iter | stop time] "

      return
      end 





