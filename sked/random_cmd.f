      subroutine random_cmd(cmdline_in)
      
      implicit none
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'          
          
! Schedule a random source between 1 and nsourc
      character*(*) cmdline_in

!function
      integer iget_random_integer     !return a random number            
      integer JULDA
      double precision hms2seconds 


! Local
      character*80 cmdline
      integer isrc 
!      character*60 cinstq
!      integer*2    linstq(31)
!      equivalence (cinstq, linstq(2)) 

      double precision TimeExpEnd
      double precision TimeRandEnd 
      double precision TimeEndCurObs
      double precision TimeStop
      
      integer istn      !pointer to station 
      integer i         ! counter  

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
      real    rtemp 
      integer*4 itemp4
      integer ierr
      integer iyr,iday,ihr,imin,isec
      integer, parameter :: max_iter=2000     !So we don't run away
      integer nobs_old   !number of observations previous iteration. 
      
      call random_seed()     !reset random number generator
! Default. 
      num_iter=1                 !One iteration
      lsubnet="_"                !All stations
      kby_iter=.true.            !Doing by iterations, not by time
      
      if(iyr_end .ne. 0) then
        TimeExpEnd=dble(JULDA(1,IDA_end,IYR_end-1900))+
     >      hms2seconds(ihr_end,imin_end,isc_end)/SecPerDay
!        write(*,*) "TimeExpEnd ", TimeExpEnd 
      else
        write(*,*) "Random_Cmd: Should never get here!"
      endif
      
      TimeStop=TimeExpEnd            !don't schedule observations past this.     
      call splitNtokens(cmdline_in ,ltoken,Maxtoken,NumToken)
      if(NumToken .eq. 0) goto 100       

      if(NumToken .eq. 1 .and. ltoken(1) .eq. "?") then 
          write(*,*) "Syntax: "
          goto 510 
      else if(NumToken .eq. 2) then 
         lsubnet=ltoken(2)
      endif
     
! Read the first argument. 
      if(ltoken(1)(1:1) .eq. "#") then
         ltoken(1)(1:1) = " " 
         read(ltoken(1), *) num_iter 
      else
         kby_iter = .false.             !Doing by time, not iterations. 
!         linstq(1)=len_trim(ltoken(1))
!         cinstq=trim(ltoken(1))
!         call gtdtr(linstq,kerr)        !Unpack data stuff 
         
         call YDHMS(ltoken(1), ierr, IYR,IDAY,IHR,IMIN,ISEC)
         if(ierr .ne. 0) then
           write(*,*) "Random_cmd: Invalid time ", ltoken(1)
           return
         else
           write(*,*) iyr,iday,ihr,imin,isec          
         endif

         TimeRandEnd=dble(JULDA(1,IDAY,IYR_end-1900))+
     >      hms2seconds(ihr,imin,isec)/SecPerDay    
         TimeStop=min(TimeRandEnd,TimeExpEnd)  
         write(*,*) "TimeRandEnd ", TimeRandEnd        
      endif       
      iter = 0 
  
      nobs_old = nobs 
100   continue 
      do while(.true.) 
        call random_number(rtemp)
        isrc=nsourc*rtemp+1  
        iter=iter+1 
                  
        write(cmdline,'(i4," subnet ",a)') isrc,trim(lsubnet)    
        CALL NEWCM(cmdline,0)
        if(nobs .eq. nobs_old) then
          write(*,'(a, " fails ")') csorna(isrc) 
        endif 
        nobs_old=nobs
      
! Now check if exit based on time.          
        TimeEndCurObs=0.d0 
        do i=1,Nstncur
          istn=iStcur(i)
          TimeEndCurObs=max(TimeEndCurObs,   dble(mjdcur(istn))+
     >        (utcur(istn)+dble(idurcur(istn)))/SecPerDay)
        enddo
        if(.false.) then 
        write(*,'("Iteration ",i4, " of ", i4, " sec left ",i5)') 
     >   iter,num_iter, int((TimeStop-TimeEndCurObs)*SecPerDay)     
        endif 
     
 ! Check if exit based on iteration         
        if(kby_iter .and. iter .eq. num_iter) return 
        if(iter .eq. max_iter) return       
! or by time        
        if(TimeEndCurObs .ge. TimeStop) return 
                                      
      end do
      return

500   continue
      write(*,*) "Syntax error "
510   continue 
!      write(*,*) "Random Subnet [#iter | stop time] "
      write(*,*) "Random #iter (max=999)"

      return
      end 





