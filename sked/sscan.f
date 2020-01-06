      SUBROUTINE SSCAN(LINSTQ,cfrom)
C
C     SSCAN handles scan durations by source
C
      implicit none 
      include '../skdrincl/skparm.ftni'

C
C  INPUT:
C     LINSTQ - input buffer, first word is length
      integer*2 LINSTQ(*)
      character cfrom !to distinguish where the call came from
! functions
      integer igetsrcnum
  
C  OUTPUT: none
C
C  COMMON:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
C
C  LOCAL
C     LKEYWD - holder for each source name/number
      integer isrc            !source number 
      integer idur            !duration 
      integer i               !counter 
  
  
      character*10 lsrc_token, ldur_token    !this holds token. 
      logical ktoken           !token found
      logical knospace         !not enough space to return token. 
      logical keol             !EOL without a token 
      integer istart, inext

! ldum holds the input line which is in hollerith. 
      integer*2 idum(128)
      character*256 ldum
      equivalence (idum,ldum)
      integer  nch
      integer nword    
  
! 2018Jan18. JMG Rewritten to get rid of hollerith except as input. 
! 2018Jan29. Fixed uninitialized error found by valgrind. 
! 2018Mar30. Was not copying over enough from linstq into ldum. 


! If input line has 0 length, then just list the scan times for the sources. 

      if(linstq(1) .eq. 0) then
        if(nsourc .le. 0) then
          write(ludsp,'(a)') "SCAN: Error! must first select sources!"
        else
          write(ludsp,"('  #  SOURCE   DURATION(sec)')") 
          DO  I=1,NSOURC
             WRITE(LUDSP,"(1X,I3,1X,A,1X,I5)") I,cSORNA(I),ISSCAN(I)
          END DO
        endif 
        return
       endif
  
      nch=linstq(1)  
      nword=(nch+1)/2
 
! Copy the input hollerith into idum=ldum.
      idum(1:nword)=linstq(2:nword+1)
      ldum(nch+1:)=" " 

! Indicate that we have chaned the parameters. 
      if(cfrom .eq. 's') knewpa=.true. 


! Parse lines of the form:
!     SRC  Dur  Src Dur SRC Dur
! SRC can be src-name, or src_number, or "-". The last means all sources. !   

      inext=1
      keol=.false.                 !this was not unitialized. Found by valgrind 
      do while(.not. keol .and. inext .le. nch)
        istart=inext
! Get the source token
        call ExtractNextToken(ldum,istart,inext,lsrc_token,ktoken,
     >     knospace, keol)

! and the duration token    
        istart=inext
        ldur_token=" " 
        call ExtractNextToken(ldum,istart,inext,ldur_token,ktoken,
     >     knospace, keol)

        if(keol) then
          write(ludsp,*) 
     >   "SCAN error: no matching duration for source: "//lsrc_token
          goto 150  
        endif 
    
      read(ldur_token,*,err=100) idur
      if(lsrc_token .eq. "_") then
        isscan(1:nsourc)=idur
      else
! See if a valid source name. 
        isrc=igetsrcnum(lsrc_token)
        if(isrc .le. 0) then
! No. See if a number.
          read(lsrc_token,*,err=110) isrc
          if(isrc .gt. nsourc) then
           write(ludsp,
     >       '("SCAN: Src# too big. Got ",i4, ", Max is ", i4)') 
     >         isrc,  nsourc
            goto 150       
          endif
        endif
        isscan(isrc)=idur
       endif                 
      end do 
      return 

100   continue
      write(ludsp,*) "SCAN: Error parsing duration ",ldur_token
      goto 150 
 

110   continue
      write(ludsp,*) "SCAN: Error parsing source ",lsrc_token 
      goto 150 
 

150   continue
      write(ludsp,*) "Line==> SCAN  "//trim(ldum)
      return
  
C
      RETURN
      END

