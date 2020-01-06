      subroutine astro_cmd(cmdline)
! display astrometric limits
      implicit none 
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'
!     include 'minor.ftni'
! History
!    ???? First version
!    2008Dec01  JMG   Added "cull" command
!    2019Mar16  JMG  added use of "kastro_src"
!    2019Apr03  JMG  Fixed bug. for kastr_src was using indiex 'i' instead of 'isrc'
! passed
      character*(*) cmdline
! functions
      integer istringMinMatch
      integer trimlen
      integer igetsrcnum

! local.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*12 ltoken(MaxToken)

      real*8 rmin_ratio     !minimum acceptable ratio for   #obs/#scan
      integer min_obs       !min # of obs

      integer icmdlen
      integer isrc
      integer i

      real rMinVal, rMaxVal     !minimum and maximum observing %

! Stuff dealing with finding which "Astro command" to do.
      character*12 cmd
      equivalence (ltoken(1),cmd)
      integer ifunc
      integer ilist_len
      parameter (ilist_len=7)
      character*12 list(ilist_len)
      logical kall,knumber
      data list/"LIST","ADD","SET","DELETE","OBS","?","CULL"/
  
      icmdlen=trimlen(cmdline)
      if(icmdlen .eq. 0) then
        ifunc=1         !default is list
      else
        call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
        ifunc = iStringMinMatch(list,ilist_len,ltoken(1))
      endif

!      cmd=ltoken(1)
! Some kind of bad command
      if(ifunc .eq. 0) then
        write(luscn,'(A,a)')"Astro_Cmd: Keyword not found: ",ltoken(1)
        return
      else if(ifunc .eq. -1) then
        write(luscn,'(A,a)')"Astro_Cmd: Ambigous keyword: ",ltoken(1)
        return
      endif

! Start of valid commands.
      cmd=list(ifunc)
      if(cmd.eq. "?") then
        write(luscn,'(A)')"ASTRO  [List | Obs | Add Src Min% [Max%] |"//
     >   " Set Source Min% [Max%]"//
     >  "| Delete Source | Cull MinObs [MinRatio] ]"
        return
      else if(cmd.eq."LIST") then
        write(luscn,'(a)') 'SOURCE                    Min%Obs Max%Obs'
        kall=.false.
        knumber=.true.
        call astro_out(ludsp,kall,knumber,'d')    !output to the display
        return
      else if(cmd .eq. "OBS") then
        call astro_obs(ludsp)
        return
      endif

! Must be "ADD","DELETE", or "SET" or "CULL"

 !ADD/SET  SRC Min [Max]                Number of tokens is 3 or 4.
 !DEL      SRC                          Number of tokens is 2
 ! find the ranges for the source(s)

      rMinVal=0.             !this is flag for not done.
      rMaxVal=1.
! If we add sources, then we get the min and max values below.

      if(cmd .eq. "CULL") then
         if(NumToken .lt. 2) then
            write(luscn,*) "Astro_cmd: must set minimum #obs"
            return
         endif
         rmin_ratio=1.   
         read(ltoken(2),*,err=930) min_obs
         if(NumToken .eq. 3) then
           read(ltoken(3),*,err=935) rmin_ratio
         else if(NumToken .eq. 4) then
           goto 900 
         endif
         call astro_cull(ludsp,min_obs,rmin_ratio)
         return
      endif

      if(cmd .eq. "ADD" .or. cmd .eq."SET") then
        if(NumToken .lt. 3) then
          write(luscn,*) "Astro_Cmd: Must set minimum value"
          return
        else if(NumToken .gt. 4) then
          goto 900
        endif

        read(ltoken(3),*,err=900) rMinVal      !get the minimum value.
        rMinVal=rMinVal/100.                   !convert to percent
        if(rMinVal .gt. .1) then
          write(luscn,*)"Astro_Cmd: Min value too big. Largest is 10%"
          goto 990
        else if(rMinVal .lt. 0) then
          write(luscn,*)"Astro_Cmd: Min value is negative!"
          goto 990
        endif

        if(NumToken .eq. 4) then
           read(ltoken(4),*,err=910) rMaxVal
           rMaxVal=rMaxVal/100.
        endif
        if(rMaxVal .lt. rMinvAl) then
           write(luscn,*)
     >      "Astro_Cmd: Max value specified is less than min value"
           goto 990
        else if(rMaxVal .gt. 1.) then
           write(luscn,*)
     >      "Astro_Cmd: Max value is larger than 100%"
           goto 990
        endif
      else if(cmd .eq. "DELETE") then
         if(NumToken .eq. 1)  then
           write(*,*) "Astro_Cmd:  Must specify source name"
           goto 990
         else
           if(NumToken .ge. 3) goto 900
         endif                     
      endif

! Now get the source argument.
      if(ltoken(2).eq. "_" .or. ltoken(2) .eq. "ALL") then
         do i=1,nsourc
            rmin_astro(i)=rMinVal
            rmax_astro(i)=rMaxVal
            kastro_src(i) = .true. 
         end do
      else
        isrc=igetsrcnum(ltoken(2))
        if(isrc .le.0) then
           write(luscn,*) "Astro_Cmd: Source not found ",ltoken(2) 
           return
        endif
        rmin_astro(isrc)=rMinVal
        rmax_astro(isrc)=rMaxVal
        kastro_src(isrc)=.true. 
        if(cmd .eq."DELETE") then
           write(luscn,
     >      '("Astro_cmd: deleting ",a, " from astrometric list")')
     >         csorna(isrc)
           rmin_astro(isrc)=0.0
           rmax_astro(isrc)=0.0
           kastro_src(isrc)=.false. 
        endif 
      endif
      return

! Different error conditions
900   continue
      write(luscn, *) "Astro_Cmd: Too many parameters"
      goto 990


910   continue
      write(luscn, *) "Astro_Cmd: Error reading min %"
      goto 990

920   continue
      write(luscn, *) "Astro_Cmd: Error reading max %"
      goto 990

930   continue
      writE(luscn,*) "Astro_cmd: Erorr reading min_obs"
      goto 990

935   continue
      write(luscn,*) "Astro_cmd: Error reading rmin_ratio"
      goto 990

990   continue
      write(luscn,*)  "         ",cmdline(1:trimlen(cmdline))
      return
      end

