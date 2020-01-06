      subroutine grget(lfilename,cdo,ierr)

      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'cat_src.ftni'

!    2008Nov11  JMGipson.  Replaced call to RSPYN by call to kyes_to_prompt

! Passed
      character*(*) lfilename
      character*1 cdo !s=standard, a=automatic. find current mode, and exit.
      integer ierr
! functions
      integer iwhere_in_string_list
      integer trimlen
      logical kyes_to_prompt

! local
      character*1 cans
      integer nc

      logical kexist
      character*80 ldum

      integer MaxToken
      integer NumToken
      parameter(MaxToken=5)
      character*30 ltoken(MaxToken)
      integer iwhere
      integer nch

! initialize
      cat_src_grade="__"

      nch=trimlen(lfilename)
      ierr=0
      inquire(file=lfilename,exist=kexist)
      if(.not.kexist) then
         write(luscn,*) "grget: File does not exist"//lfilename(1:nch)
         if(cdo .eq. "a" .or. cdo .eq. "A") return   !return if in auto mode.

         write(luscn,*)
     >    "If you continue all source grades will be set to '_' "
         if(kyes_to_prompt("Do you want to continue? (Y/N)")) then
           continue
         else
           ierr = 1
           return
         endif       
      endif
      write(luscn,*) "Opening file: "//lfilename(1:nch)
      open(99,file=lfilename)

100   continue
      read(99,'(a80)',end=200) ldum
      if(ldum(1:1) .eq. "*" .or. ldum(1:1) .eq. " ") goto 100

      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)

      iwhere=iwhere_in_string_list(cat_src_iau,num_cat_src,
     >  ltoken(1)(1:8))
      if(iwhere .eq. 0) then       !no match on IAU name
        iwhere=iwhere_in_string_list(cat_src_name,num_cat_src,
     >  ltoken(1)(1:8))
        if(iwhere .eq. 0) goto 100  !or on normal name.
      endif
      cat_src_grade(iwhere)=ltoken(2)(1:2)  !Found a match.

      goto 100

200   continue
      close(99)
      return
      end
