      subroutine Op_Read(luin)
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'covar.ftni'

! History
!  2003Mar04   JMGipson.  First version.
!  2005Mar28   JMGipson.  Convert input low-elevation to radians.
!  2005May23   JMGipson.  Modified to check that all OPTIMIZED parameters are estimated.
!  2006Oct04   JMGipson.  Better error messages
!  2009Nov05   AEM fixed bug in reading in parameters
!  2010Apr15   JMG. Removed referecne to many obsolete parameters. 
! 2018Jan23  JMG. Ignore lnes that start with '*'. These are comments 

! On entry, points to first line of ops section. On exit, done reading ops.
! First several lines of ops consist of:
!    Keyword1 Value1   Keyword2  Value2  ....
!    Keywordj Valuej
!  Followed by parameters used in covariance. This section begins with  XP
!

C Input
      integer luin      !logical unit
! functions
      integer trimlen
      integer iwhere_in_string_list
!
! Local
      logical ktoken,knospace,keof
      character*20 lkeyword,lvalue
      character*20 ltoken(20)

      logical kdone(max_stn)
      logical kbad
      
      integer ivalue
      logical kvalue
      real    rvalue
      integer itype             !1=optimize. 2=estimate.
      character*3 leop(5)
      character*4 lstatparm(8)
      integer np(2),maxnp(2)
      integer ioff
      integer istart,inext,i,j
      integer istat
      integer NumWant           !Num of tokens we want
      integer NumGot            !number we get
      integer isrc              !source index
      integer iline
      integer nch 

      data leop/"XP","YP","DUT","PSI","EPS"/
      data lstatparm/"AOFF","ARAT","COFF","CRT1","CRT2","X","Y","Z"/

! initialize
      kfillin=.false.
      lpara=.false.
      iline=0
      rewind(luin) 
! Read until we get to the the start of the $OP section. 
      cbuf="NOT OP" 
      do while(cbuf .ne. "$OP")
        read(luin,'(a)',end=900) cbuf
!       iline=iline+1
!       write(*,'("OP ",i4, " | ", a)') iline, trim(cbuf)
      end do


100   continue
      read(luin,'(a128)',end=900) cbuf
      if(cbuf(1:1) .eq. "*") goto 100
      if(cbuf(1:1) .eq.  "$") return 
!     iline=iline+1 
!     write(*,'("OP ",i4, " | ", a)') iline, trim(cbuf)

      call capitalize(cbuf)
    
      istart=1
      keof=.false.

! Process
110   continue
      call ExtractNextToken(cbuf,istart,inext,lkeyword,ktoken,
     >     knospace,keof)
      if(knospace) then
         write(luscn,*) "Op_read 2: not enough space for tokens!"
         write(luscn,*) "String is: ",cbuf(istart:128)
         stop
      endif
      if(.not. ktoken) goto 100           !no more tokens on this line. Read another.
      if(keof) then
         write(luscn,*) "Op_read 3: keyword without token on line!"
         write(luscn,*) "String is: ",cbuf(istart:128)
         stop
      endif
      if(lkeyword .eq. "XP") goto 200
      istart=inext
! Got a keyword. Now get value.
      call ExtractNextToken(cbuf,istart,inext,lvalue,ktoken,
     >     knospace,keof)
      if(.not. ktoken) then
         write(luscn,*) "Op_read 4: keyword without token on line!"
         write(luscn,*) "String is: ",cbuf(1:128)
         stop
      endif

! Assume value is integer. Find out what it is.
      ivalue=0
      read(lvalue,*,err=120) ivalue
120   continue
      rvalue=0.
      read(lvalue,*,err=125) rvalue
125   continue
! assume it is logical. is either "T" or "F"
      kvalue = lvalue .eq. "T" .or. lvalue .eq. "TRUE"

      if(lkeyword .eq. "COVERAGE") then         	!1
        kOptBySky = kvalue
      else if(lkeyword .eq. "LASTHR") then       	!2
        if(ivalue .eq. 0) then
           write(luscn,'(a,i3,a)') "Op_read warning! Invalid lasthr: ",
     >       ivalue, " Must be positive. Setting Window to 24"
           rcovar_win=24
        else
           rcovar_win=ivalue
        endif
      else if(lkeyword .eq. "MAXOBS") then       	!3
        KNumObs = kvalue
      else if(lkeyword .eq. "MINTIM") then        	!4
        kEndScan = kvalue
      else if(lkeyword .eq. "LOCALCOV") then      	!5
        kSkyCov = kvalue
      else if(lkeyword .eq. "BEST%") then         	!6
        if(rvalue .le. 0 .or. rvalue .gt. 100) then
           write(luscn,'(a,i3,a)') "Op_read warning! Invalid Best%. ",
     >        int(rvalue), " Valid range 0-100. Setting to 50%"
           rBestPerCent=0.5
         else
           rBestPerCent=rvalue/100.
         endif
      else if(lkeyword .eq. "CART") then         	!7
         kcar=kvalue
      else if(lkeyword .eq. "SNRWT") then        	!8
         ksnrwts=kvalue
      else if(lkeyword .eq. "NOISE") then
        if(ivalue .le. 0) then
           write(luscn,'(a,i3,a)')
     >      "Op_read Warning! Invalid Noise floor: ",
     >       ivalue, " Must be >=0.  Ignoring command."
         else
           radd_noise=ivalue
         endif
      else if(lkeyword .eq. "EVN#SOR") then
        kSrcEvn = kvalue
      else if(lkeyword .eq. "LOWEL") then
        if(ivalue .le. 1) then
           write(luscn,'(a,i3,a)') "Op_read Warning! Invalid LowEl ",
     >      ivalue, ". Must be >0. Setting LowEl to ignore."
           kNumLoEl=.false.
           rloel=3
         else
           kNumLoEl=.true.
           rloel=ivalue
         endif
          rloel=rloel*deg2rad
      else if(lkeyword .eq. "EXPAND") then
         kexpand=kvalue
      else if(lkeyword .eq. "RISESET") then
         kNumRiseSet = kvalue
      else if(lkeyword .eq. "MINSLEW") then
         kBegScan = kvalue
      else if(lkeyword .eq. "MINBETW") then
        if(ivalue .lt. 0) then
           write(luscn,'(a,i3,a)')
     >       "Op_read Warning. Invalid MinBetw. ",ivalue,
     >      ". Must be positive. Ignoring command."
         else
           iminbetween=ivalue*60           !convert to seconds.
         endif
      else if(lkeyword .eq. "SRCFLR%" .or. lkeyword .eq. "#SRCFLR") then
        write(*,*) "Op_read: Ignoring obsolete command ",lkeyword           
      else if(lkeyword .eq. "EVNSRCMODE") then
        if(ivalue .lt. 0 .or. ivalue .gt. 3) then
         write(luscn,'(a,i3,a)') "Op_read warning: Invalid EvnSrcMode ",
     >      ivalue,   ". Valid range 0-3. Ignoring command."
        else
           iSrcEvnMode=ivalue
        endif
      else if(lkeyword .eq. "FILLIN") then
         kfillin=kvalue
      else
        write(luscn, *) "Op_read 12: Unknown keyword: ", lkeyword    
      endif

      if(keof) goto 100         !last token on this line. Read a new line.
      istart=inext              !Get ready to read next token and loop around to read next one.
      goto 110


! now process eop stuff and other stuff.
! This section is repeated twice. Once for optimization and once for estimation.
200   continue
      np(1)=0
      np(2)=0
      maxnp(1)=MAX_PAR_OPTI
      maxnp(2)=MAX_PAR_ESTI

! On entry, has read one line containing "XP ...."
! read in EOP file
      do itype=1,2
!1. Read in the EOP parameters.
        NumWant=10
        call splitNtokens(cbuf,ltoken,NumWant,NumGot)
        if(NumWant .ne. NumGot) then
          write(luscn,*) "Op_read 101: Invalid EOP line: "
          write(luscn,*) cbuf(1:60)
          return
        endif

        do i=1,5
          if(ltoken(2*i-1) .eq. leop(i)) then
            lpara(i,itype) = ltoken(2*i) .eq. "T"
            if(lpara(i,itype)) then
              if(np(itype) .le. maxnp(itype)) then
                 np(itype)=np(itype)+1
               else
                 write(luscn,*) "Op_read 102: out of parameter space!"
              endif
            endif
          else
             write(luscn,*)
     >         "Op_read 103: Invalid EOP parameter",ltoken(2*i-1)
          endif
        end do

! 2. Read in the station parameters.
        kdone=.false.
        do j=1,nstatn
          read(luin,'(a128)',end=920) cbuf
!          read(cbuf,*) ltoken(1:17)
          NumWant=17
          call SplitNTokens(cbuf,ltoken,NumWant, NumGot)
          if(NumGot.ne.NumWant .or.
     >       .not. (ltoken(2).eq."AOFF" .or. ltoken(2).eq."ATM0")) then
             write(luscn,'("Op_read 104: Invalid station line: ",a)')
     >                 cbuf(1:trimlen(cbuf))
             write(luscn,*) "Aborting reading $OP"
             return
          endif
          nch=trimlen(ltoken(1))
          if(nch .eq.1) then 
             istat=iwhere_in_string_list(cstcod,nstatn,ltoken(1))
          else if(nch .eq. 2) then
             istat=iwhere_in_string_list(cpocod,nstatn,ltoken(1))
          endif
          if(istat .eq. 0) then
             write(luscn,'("Op_read: Invalid station line ",a)')
     >          cbuf(1:trimlen(cbuf))
             return
          endif
          kdone(istat)=.true.

          do i=1,8
            if(ltoken(2*i) .eq. lstatparm(i)) then
               if(i .le. 2) then
                  ioff=5+2*(istat-1)+i
               else if(i .le. 5) then                 ! 3<=i<=5
                  ioff=5+2*nstatn+3*(istat-1)+i-2
               else                                   ! 6<= i <8
                  ioff=5+5*nstatn+3*(istat-1)+i-5
               endif
               lpara(ioff,itype) = ltoken(2*i+1) .eq. "T"
               if(lpara(ioff,itype)) then
                 if(np(itype) .lt. maxnp(itype)) then
                   np(itype)=np(itype)+1
                 else
                   write(luscn,*) "Op_read 104: out of parameter space!"
                   return
                 endif
              endif
            else
               write(luscn,*)
     >           "Op_read 105: Invalid statparm parameter",ltoken(2*i)
               write(luscn,*) "Aborting"
               return
            endif
          end do
        end do
        kbad=.false.
        do i=1,nstatn
          if(.not.kdone(i)) then
            write(luscn,'("Op_read. Did not set station ",a)') cstnna(i)
            kbad=.true.
          endif
        end do
        if(kbad) then
          write(luscn,'("Aborting!")')
          return
        endif

! 3. Now we read the sources

! Read in sources 10 at a time. If we do hand editing on source list,
! may not have correct number of parameters set in this list.
! This is why we check for "XP" appearing at the start of a line.
! WE also check that we don't start to read another sked section.
        isrc=0
250     continue
        read(luin,'(a128)',end=910) cbuf
!        NumWant=min(Nsourc-isrc,10)*2
        NumWant=20
        call SplitNTokens(cbuf,ltoken,NumWant, NumGot)
        if(ltoken(1) .ne. "XP"  .and. ltoken(1)(1:1) .ne. "$") then
          do i=1,NumGot/2
            isrc=isrc+1
            if(isrc .le.Nsourc) then
              if(ltoken(2*i)(1:1) .eq. "T") then
                lpara(5+8*nstatn+2*isrc-1,itype)=.true.
                lpara(5+8*nstatn+2*isrc,itype)=.true.
                np(itype)=np(itype)+2
                if(np(itype) .gt. maxnp(itype)) then
                  write(luscn,*)
     >              "Op_read 105: Out of parameter space for EOP!"
                endif
              endif
            endif
          end do
          if(isrc .lt. Nsourc) goto 250
        endif
! If  isrc<>Nsourc, then a mismatch between the number of parameters and sources.
260     continue
        if(isrc .ne. Nsourc) then
          write(luscn, '(a,2i4)') "Op_read 106: Warning! Mismatch "//
     >      "between # source params and # of sources: ",
     >       isrc,Nsourc
        endif
! make sure we prime the pump for next go round.
        do while(ltoken(1) .ne. "XP" .and. itype .eq. 1)
           read(luin,'(a128)',end=920) cbuf
           NumWant=2
           call SplitNTokens(cbuf,ltoken,NumWant, NumGot)
       end do
      end do
      num_est=np(2)
      num_opt=np(1)
      return
500   continue 

900   continue
      write(luscn,'(a)') "Op_read 1: Unexpected EOF!"
      return

910   continue
      write(luscn,'(a)') "Op_read 106: Unexpected EOF reading EOP line."
      return

920   continue
      write(luscn,'(a)')
     >   "Op_read 108: Unexpected EOF reading station line."
      return
      end

