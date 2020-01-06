      SUBROUTINE WRSOS(IERR)
!
!      WRSOS writes the selection file for sources
!
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'
      include 'skcom.ftni'
      include 'cat_src.ftni'
! 2005 Jun07  JMGipson completely rewritten.
! 2006 May02  JMGipson. Removed "ccat" from argument list which was no longer being used.

C  OUTPUT: IERR - error return
      integer ierr

! functions
      integer iwhere_in_string_list
C
C  Local:
! Used to hold tokenized line
      character*80 ldum
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)
      integer iwhere

      character*80 lsrc_line(Max_sor)
      double precision rhs(max_sor)
      integer ikey(max_sor)
      integer num_found(max_sor)
      logical kduplicate
      integer i,iptr
      integer num_sel           !number selected
      integer num_done          !number read in from source catalog
      integer ihr,imin
      double precision rsec

      integer trimlen  !variable and function for var length

      call open_cat(source_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

      OPEN (lusel,file=CSOFIL,status='OLD',iostat=ierr)
C                    SKX*

! See which sources were selected.
      num_sel=0
      do i=1,num_cat_src
         if(kcat_src_sel(i)) then
           num_sel=num_sel+1
           csorna(num_sel)=cat_src_name(i)
           ciauna(num_sel)=cat_src_iau(i)
         endif
      end do

! Read in the source catalog, keeping only those sources which were selected.
      num_done=0
      num_found=0
100   continue
      read(lucat,'(a80)', end=190) ldum
      if(ldum(1:1) .eq. "*" .or. ldum .eq. " ") goto 100
      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)
! Check for matches.
      iwhere=iwhere_in_string_list(ciauna,num_sel,ltoken(1)(1:8))  !check IAU name
      if(iwhere .eq. 0) then
        if(ltoken(2) .ne. "$") then
         iwhere=iwhere_in_string_list(csorna,num_sel,ltoken(2)(1:8)) !check other name.
        endif
        if(iwhere .eq. 0) goto 100
      endif

      num_done=num_done+1
      read(ltoken(3),*) ihr
      read(ltoken(4),*),imin
      read(ltoken(5),*) rsec
      lsrc_line(num_done)=ldum
      RHS(num_done)=float(ihr)+float(imin)/60.d0+rsec/3600.d0

      num_found(iwhere)=num_found(iwhere)+1
      if(num_found(iwhere) .gt. 1) then
         kduplicate=.true.
      endif
      goto 100

190   continue
      close(lucat)

      do i=1,num_sel
        if(num_found(i) .eq. 0) then
           write(luscn,'("Missing from cat: ",a,1x,a)')
     >       ciauna(i),csorna(i)
         else if(num_found(i) .gt. 1) then
           write(luscn,'("Duplicate source: ",a,1x,a)')
     >       ciauna(i),csorna(i)
         endif
      end do

! Now sort in RA order
200   continue
      call indexx8(num_done,rhs,ikey)
      do i=1,num_done
         iptr=ikey(i)
         write(lusel,'(a)') lsrc_line(iptr)(1:trimlen(lsrc_line(iptr)))
      end do
      CLOSE(lusel)
C
      RETURN
      END
