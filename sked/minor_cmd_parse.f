      subroutine minor_cmd_parse(ltokens,NumTokens,NumUsed,
     >          kMinorCmd,kMinorCmdNorm,rMinorCmdWt,
     >          lcmds,icmd_len,rvalues,NumRVal,MaxRval,
     >          lvalues,NumLval,MaxLval)
     
      implicit none

! Process a minor command (given as a list of tokens)
! Until we hit another minor command, or the end of the tokens
! General format of the input tokens are:
!  [ABS|Rel] [ON|OFF] [YES|NO] [WT]  [AUX STRING] [AUX WT]
! Everything is optional.
!
! History:
!  2004Mar08  JMGipson. First version
!  2005Aug09  JMGipson. Improved comments.
!  2006Nov06  JMGipson. lcmds was dimensioned incorrectly
!  2007Feb10  JMGipson. MaxRVal, MaxLVal added to arg list. Previously hard coded.

! functions
      integer iwhere_in_string_list

! passed
      integer       Numtokens           !number of tokens to examine
      character*(*) ltokens(NumTokens)  !list of tokens
      integer icmd_len                  !Command length
      character*(*) lcmds(icmd_len)         !list of minor commands
      integer MaxLval,MaxRval
! returned
      integer NumUsed                   !number of tokens used.
      logical kMinorCmd                 !True if we want this option
      logical kMinorCmdNorm             !True if we normalize
      real rMinorCmdWt                  !Wt of minor command.

      real rvalues(MaxRval)
      character*(*) lvalues(MaxLVal)
      integer NumRval,NumLval

! local
      integer itoken, iwhere
! AEM 20050215 list local vars
      integer i
      real rtemp

      NumUsed=0
      NumRVal=0
      NumLval=0
      do itoken=1,NumTokens
        call capitalize(ltokens(itoken))
        iwhere=iwhere_in_string_list(lcmds,icmd_len,ltokens(itoken))  !see if we have found another command.
        if(iwhere .ne. 0) goto 200      !EXIT

        NumUsed=NumUsed+1
        if(ltokens(itoken) .eq. "ON"  .or.
     >     ltokens(itoken) .eq. "YES" .or.
     >     ltokens(itoken) .eq. "Y") then
           kMinorCmd=.true.
           goto 100
        else if(ltokens(itoken) .eq. "OFF" .or.
     >          ltokens(itoken) .eq. "NO"  .or.
     >          ltokens(itoken) .eq. "N") then
           kMinorCmd=.false.
           goto 100
        endif

        if(ltokens(itoken) .eq. "REL" .or.
     >     ltokens(itoken).eq."NORM") then
           kMinorCmdNorm=.true.
           goto 100
        else if(ltokens(itoken) .eq. "ABS") then
           kMinorCmdNorm=.false.
           goto 100
        endif

! See if this is a number by trying to read it.
! AEM undo       read(ltokens(itoken),*,err=80) rtemp
! AEM 20050215 use proper format 'f20.0' to indentify real or integer and reinit 'rtemp' everytime
	rtemp = 0d0
        read(ltokens(itoken),'(f20.0)',err=80) rtemp
	
        NumRval=NumRval+1
        if(numrval .gt. maxrval) then
           write(*,*) "Minor_cmd_parse:  no more space for real parms"
           return
        endif
        rvalues(NumRVal)=rtemp
        goto 100

! not a number. Must be a character string.
80      continue
        NumlVal=NumLVal+1
        if(numLval .gt. maxLval) then
          write(*,*) "Minor_cmd_parse:  no more space for logical parms"
          return
        endif
        lValues(NumLVal)=ltokens(itoken)

100     continue
      end do

200   continue
! First real number found becomes the weight, if there is a real number.
      if(NumRval .eq. 0) then
         rMinorCmdWt=1.
      else
         rMinorCmdWt=Rvalues(1)
         NumRVal=NumRVal-1
         do i=1,NumRval
           Rvalues(i)=Rvalues(i+1)
         end do
      endif
      return
      end
