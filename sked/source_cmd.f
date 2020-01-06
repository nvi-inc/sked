      subroutine source_cmd(cmdline)
C
C   Determines the function requested in the SOURCES command,
!    and then calls the appropriate subroutine to do it.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'

C
C  INPUT VARIABLES:
      character*(*) cmdline 
C        LINSTR - input string from user, beginning after the command
C
C
C     CALLING SUBROUTINES: SKED (main program)
C                         SOLIS (to list the sources selected)
C                         SOSEL (to select sources)
C                         SOPLT (to plot sources)
C

! functions
      integer istringminmatch
      integer trimlen 


C   LOCAL VARIABLES       
  
      character*1 cme
      
! All of these have to do with parsing cmdline.
      character*128 ltoken             !token returned from parsing command line.
      integer istart                   !starting point to parse. 
      integer inext                    !beginning point of next token
      logical ktoken                   !Found a token
      logical keol                     !reached EOL in parsing
      logical knospace                 !ran out of space.

! Miscellanouse other 

      integer ifunc 
      integer i 

      logical kvlba_out                   !If true output source in VLBA format
      logical kplot_stat              !hard wired. For consistency with mutualvis.
      integer istn,nstn               !hard wired. Ditto. 
      integer min_obs

      integer ilist_len
      parameter (ilist_len=8)
      character*12 lcmd_list(ilist_len)
      character*80 lhelp(ilist_len)
      data (lcmd_list(i), lhelp(i), i=1,ilist_len)/
!1     
     >"?           ",
     >"               List source commands and options",
!2
     >"Help        ",
     >"               List source commands and options",
!3
     >"List       ", 
     >"               List sources and positions",
!4
     >"VLBA_Format ", 
     >"               List source and positions in VLBA format",

!6
     >"XY_Plot     ", 
     >"               Plot sources in XY-format.",
!7
     >"Pol_Plot    ", 
     >"               Plot sources in Polar format",
!5
     >"Select      ", 
     >" <source.cat>  Select sourcess from catalog source.cat.",
!8
     >"Cull        ", 
     >" <Min_Obs>     Cull sources with fewer than Obs. (Default=3)"/
   
C
C  HISTORY
C     880314 NRV DE-COMPC'D
C     890628 NRV Added plot call
C     930225 nrv implicit none
C     940127 nrv Add option of catalog name following SELECT
C 951017 nrv Fixed gtfld call to remove linstq
!     2006Apr24 JMGipson.  Added mode to print in VLBA mode.
!    2012Apr09 JMGipson. Re-written to use command line, make simpler.


      i=trimlen(cmdline)
      if(i .eq. 0) then
        ifunc=1         !default is help
      else
        istart=1
        call ExtractNextToken(cmdline,istart,inext,ltoken,ktoken,
     >  knospace, keol)
        if(ktoken) then        
          ifunc = iStringMinMatch(lcmd_list,ilist_len,ltoken)
          istart=inext
        else
          ifunc=0
         endif
      endif
          
      if(ifunc .le. 0) then
         write(luscn,'(A)') "Source command not found. "
         write(luscn,'("    Valid commands are: ",12(2x,a))')
     >    (trim(lcmd_list(i)),i=1,ilist_len)
        RETURN
      endif

      select case(lcmd_list(ifunc))
      case("?","Help")     
        do i=1,ilist_len
          write(luscn,'(a,1x,a)') lcmd_list(i),lhelp(i)
        end do     
      case("List","VLBA_Format")       
        kvlba_out=lcmd_list(ifunc) .eq. "VLBA_Format"
        CALL SOLIS(kvlba_out)       
      case("Select")      
        cme = 's'
       call ExtractNextToken(cmdline,istart,inext,ltoken,ktoken,
     >   knospace, keol)
        if(.not.ktoken) ltoken=" "         
        CALL SOSEL(ltoken,cme)
        RETURN
      case("XY_Plot") 
        call soplt
      case("Pol_Plot")        
        kplot_stat=.false.
        istn=0
        nstn=0
        call soplt_pol(kplot_Stat,istn,nstn)
        return
      case("Cull") 
        call ExtractNextToken(cmdline,istart,inext,ltoken,ktoken,
     >   knospace, keol)
        if(ktoken) then
           read(ltoken,*,err=900) min_obs
           if(min_obs.gt. 10 .or. min_obs.lt. 1) then
              write(*,*) "Threshold must be between 1 and 10"
           endif
        else
           min_obs=3        !Minuminum number
        endif   
        call source_cull(ludsp,min_obs) 
      case default
        write(*,*) "source_cmd: Should never get here!"
        write(*,*) "Command found was "//lcmd_list(ifunc) 
      end select
      RETURN

900   continue
      write(*,*) "Error parsing: "//trim(cmdline) 
      END
