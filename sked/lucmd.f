      SUBROUTINE LUCMD(LINSTQ)
C
C   LUCMD changes the display unit
C
      include '../skdrincl/skparm.ftni'
! functions
      integer istringminmatch
C
C  INPUT VARIABLES:
      integer*2 LINSTQ(*)
C               - input string containing new LU, word 1=length
C
      include 'skcom.ftni'
C
C LOCAL VARIABLES
      integer i2long,rwopen !function
      integer*2 LKEYWD(12)
      character*2 ckey,ckey2
      character*256 tempbuf
      integer     IERR,ich,ic1,ic2,nc,ikey,ikey2,idum,ichmv
      LOGICAL*4     kexist     !does the file exist.
      CHARACTER     RSP,tst
      character*128  cfilename ! temporary filename holder
      character*128  ctemp
      integer it1,it2,iok2
      integer i

      character*24 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=4)
      character*10 list(ilist_len),listshort(ilist_len)
      data list/"APPEND","OVERWRITE","PRINT","SCREEN"/
      data listshort/"AP","OV","PR","SC"/

C History
C          840813 MWH Lock new display unit (and unlock previous one)
C          840905 MWH Make user terminal the default
C          850605 MWH Use LUTRU to get real LU of terminal
C          880310 NRV DE-COMPC'D
C          8805?? PMR added print/screen
C          890401 GAG added an else statement to make filename work
C          890518
C            to 
C          890601 GAG rewrote routine with added parameters append
C                     and overwrite
C          890606 GAG added rwopen call to check for file access
C          890612 GAG added check for illegal filename character
C          900301 gag added the new logic to go with the control file
C          900312 gag added temporary filename holder
C 930225 nrv implicit none
C 950414 nrv File names can begin with numbers too
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed gtfld call to remove linstq
! 2013May17 JMGipson. Indicate when we are producing graphics files. 
C
 
C
C  1. If there are no parameters with the UNIT call, the program will
C     respond with what is the current output device.
C
      IF (LINSTQ(1).EQ.0) then
        if (ludsp.eq.lufil) then
          write(luscn,9010) trim(ctpfil)
9010      format('Output is the file ',a) 
        else if (ludsp.eq.luscn) then
          write(luscn,9020)
9020      format('Output is the screen')
        end if
        return
      end if 

C  2. If parameters are specified, see if first parameter is PRINT or
C     SCREEN. If neither, it assumes a filename. The filename is kept
C     as the user has typed it. 
C 

      ICH = 1
      rsp = ' '
      ikey2 = -1
      ckey=" "
      cfilename = ctpfil
     
      nc = linstq(1)
      call gtfld(linstq(2),ich,nc,ic1,ic2)
      nc = ic2 - ic1 + 1  
      if (nc.gt.12) nc = 12
      ckeywd=" "
      idum= ichmv(lkeywd,3,linstq(2),ic1,nc) 
      ikey = istringMinMatch(list,ilist_len,ckeywd)

      if(ikey .gt. 0) ckey=list(ikey)

      if ((ckey.ne.'PR').and.(ckey.ne.'SC')) then
        call hol2char(linstq(2),ic1,ic2,ctpfil)  
!        call null_term(ctpfil)
        it1 = 1
        it2 = 1
        do while (it2.ne.0)
          it2 = index(ctpfil(it1:nc),'/') 
          it1 = it1 + it2
        end do 
        tst = ctpfil(it1:it1)
        if (((LLT(tst,'a')).or.(LGT(tst,'z'))).and.(tst.ne.'/')) then
          if ((LLT(tst,'A')).or.(LGT(tst,'Z'))) then 
            if ((LLT(tst,'0')).or.LGT(tst,'9')) then
              write(luscn,9025) trim(ctpfil)
9025          format('Illegal file name ',A)
              return
            endif
          end if
        end if
      else if (ckey.eq.'PR') then
        ctpfil = cprfil
      end if

C  3. Check for a 2nd parameter. The two legal parameters are append and 
C     overwrite.  

      call gtfld(linstq(2),ich,i2long(linstq(1)),ic1,ic2) 
      nc = ic2 - ic1 + 1  
      if (nc.gt.12) nc = 12
      if (ic1.gt.0) then
        idum= ichmv(linstq(2),1,linstq(2),ic1,nc)
        linstq(1) = nc
        ckeywd=" "
        do i=1,(nc+1)/2
          lkeywd(i)=linstq(i+1)
        end do
        if(nc .lt. 12) ckeywd(nc+1:nc+1)=" "   !this makes sure last byte is space.
        ikey2=istringMinMatch(list,ilist_len,ckeywd)
        if(ikey2 .le. 0) then
          write(*,*) "LUCMD: bad keyword or double match"
          return
        endif
        ckey2=list(ikey2)

        if ((ckey2.ne.'AP').and.(ckey2.ne.'OV')) then
          write(luscn,'(a)')
     >     ' LUCMD - The 2nd parameter must be APPEND or OVERWRITE'
          return
        end if
      end if

! See if "unit screen". If so quick exit. 
      if (ckey.eq.'SC') then
        if (ludsp.eq.lufil) then
          close(lufil)
          write(luscn,9032) cfilename
          ludsp = luscn
          write(luscn,"(a)") 'Output returning to the screen'
        else
          write(luscn,"(a)") 'Output is already the screen'
        end if
        return
      endif
    
      ctemp=ctpfil
      call capitalize(ctemp)    
      if(ctemp(1:6) .eq. "SAVEPS") then
         writE(luscn,'(a)') "Getting ready to produce PS files"
         return
      endif 

C  4. If unit command is other than screen and already saving to a file
C     or print file then send a message if same file and close old file.
C

      if (ludsp.ne.luscn) then
        close(lufil)
        write(luscn,9032) trim(cfilename)
9032    format('Closing file ',a)
        ludsp = luscn
      end if 
 

C  6. Check access permission and open. 
C
      
      inquire(file=ctpfil,exist=kexist)
      if (kexist) then  ! default to APPEND
        iok2 = rwopen(ctpfil)
        if (ckey.ne.'PR') then
          if ((iok2.eq.-1).or.(ctpfil.eq.cprfil)) then
            write(luscn,'("Cannot access the file ",a)')  ctpfil
            return
          end if
        end if
       if(ckey2 .eq. 'OV') then
          open(lufil,file=ctpfil,status='unknown',iostat=ierr)      
       else
          open(lufil,file=ctpfil,status='unknown',position='append',
     &      iostat=ierr)
       endif 
       if (ierr.ne.0) then
            write(luscn,9070) ierr,trim(ctpfil)
9070        format(' LUCMD - Error 'I5' opening file: ',A)
            return
        end if
        if(ckey2 .eq. 'OV') then
          write(*,"('Overwriting file: ', a)") trim(ctpfil)
        else
          write(*,"('Appending to file: ',a)") trim(ctpfil)
        endif 

      else
        if ((ctpfil.eq.cprfil).and.(ckey.ne.'PR')) then 
          write(luscn,'("Cannot access the file ",a)')  trim(ctpfil)
          return
        end if
        open(lufil,file=ctpfil,status='unknown',iostat=ierr)
        if (ierr.ne.0) then
          write(luscn,9070) ierr,ctpfil
          return
        else
          write(luscn,'("Saving to ", a)')  trim(ctpfil)
        end if
      end if
      ludsp = lufil
     

C
990   RETURN
      END

