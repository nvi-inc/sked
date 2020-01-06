      subroutine stat_sefd_out(luout,lkind)
! Program to write the station dependent SEFDs
! Format is something like:
!   FORTLEZA X 5000
! or 
!   KOKEE S 750  1.00 0.9453 0.0547 
! 
! Written 2012Sep JMGipson
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'     
! Passed
      integer luout
      character*1 lkind 
! Local
      integer istat, ib    !counters of station, bandwidth 


      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$STAT_SEFD"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,lutmp,lkind) 
      endif 

      do istat=1,nstatn
        do ib=1,2 
          write(cbuf,'(a," ",a," ",i5)') cstnna(istat),
     >        cbsefd(ib,istat),nint(sefdst(ib,istat))

          if(nsefdpar(ib,istat) .gt. 0) then          
            write(cbuf(20:),'(f3.1,10f9.4)')
     >        sefdpar(1:nsefdpar(ib,istat),ib,istat)            
          endif 
          call wrt_param_line(cbuf,luout,lkind)
        end do
      end do
      return
      end 
! *****************************************************
! Parse a line that was written out above.
      subroutine stat_sefd_in(cbuf,ierr) 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/statn.ftni'   

! Passed
      character*(*) cbuf
      integer ierr 
! Functions
      integer iwhere_in_string_list

! Local
      integer NumToken, MaxToken 
      parameter(MaxToken=10)
      character*12 ltoken(MaxToken)
      integer istat    !staiotn
      integer ib       !band
      integer ipar     !parameter
      
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)    
      if(NumToken .lt. 3) then
         write(*,*) "Not enough tokens on SEFD line: ",trim(cbuf)
      endif

      istat=iwhere_in_string_list(cstnna,nstatn,ltoken(1))
      if(istat .eq. 0) then
         write(*,*) "stat_sefd_in: Did not find station ", ltoken(1)
         ierr=-1
      endif
      if(ltoken(2) .eq."X") then
        ib=1
      else if(ltoken(2) .eq. "S") then
        ib=2
      else
        write(*,*) "stat_sefd_in: Unknown band!", ltoken(2)
      endif 
      cbsefd(ib,istat)=ltoken(2)
      read(ltoken(3),*,err=900) sefdst(ib,istat)
      nsefdpar(ib,istat)=NumToken-3

      do ipar=1,nsefdpar(ib,istat)
        read(ltoken(ipar+3),*,err=900,end=900)  sefdpar(ipar,ib,istat)  
      end do
      ierr=0
      return
900   continue
      write(*,*) "stat_sefd_in: Error parsing line ", trim(cbuf) 
      ierr=-3
      return
      end 
 

 

    




