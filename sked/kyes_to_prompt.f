      logical function kyes_to_prompt(lprompt)
!     get yes no response.
! passed
      character*(*) lprompt    
! History:
!   2008Nov11  JMGipson. First version


! fuctions
      integer trimlen

! local
      integer nch
      character*10 lresponse
! start of code
   
      nch=trimlen(lprompt)
    
100   continue     
      write(*,"(a,2x,$)") lprompt(1:nch)
      read(*,*) lresponse
      call capitalize(lresponse)
      if(lresponse .eq. "Y" .or. lresponse .eq. "YES") then
        kyes_to_prompt=.true.      
      else if(lresponse .eq. "N" .or. lresponse .eq. "NO") then
        kyes_to_prompt=.false.        
      else
        write(*,*) "Valid responses are Yes, No, Y, N. Try again!"
        goto 100
      endif
      return
      
      end
     



    
           

