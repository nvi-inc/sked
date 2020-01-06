      subroutine read_source_cat(ierr)
C
C  This routine will get all the source names from the
C  source catalog and puts them in LNASE1.
C  It also checks for schedule sources not found in the catalog.
C
C   History
C  gag  900608  removed from seso routine.
C  nrv  930225  implicit none
C  nrv  940112  Fixed up logic so ORBIT is an OK name.
C  nrv  940127  Add fname to call so that message is correct.
C  nrv  950321  Check for duplicates before starting.
C  nrv  950329  One too many sources in catalog was being counted.
C  nrv  950413  Revise duplicates check to be less stringent so that
C               names with A and B suffixes are not called the same.
C 970307 nrv change 4 and 8 to max_sorlen/2, revise format statements
! 2019Sep03 JMG. Added implicit none 
C
C   Common/include
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'cat_src.ftni'

C   Called by: SOCAT
C
C   Input:
C   Output:
      integer ierr ! error return
! functions
      integer iwhere_in_string_list

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)
! Other local variables
      integer iwhere
      logical keof

      ierr=0
      if(kcat_src) return           !Already read. Don't need to re-read.
      call open_cat(source_cat,ierr)
      if(ierr .ne. 0) return

! 1.0 Read in all the sources into two arrays: IAU and common.
      num_cat_src=0
100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 190

      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
! Check for duplicates
      iwhere=iwhere_in_string_list(cat_src_iau,num_cat_src,
     >   ltoken(1)(1:8))
      if(iwhere .ne. 0) then    !found match for IAU name.  Check common name.
        if(ltoken(2) .ne. "$") then
          iwhere=iwhere_in_string_list(cat_src_name,num_cat_src,
     >      ltoken(2)(1:8))
        endif
        if(iwhere .ne. 0) then
          if(ltoken(2) .eq. "$") ltoken(2)=" "
          write(luscn,'(3(a," "))')
     >      "Read_source_cat: Duplicate source. Ignoring 2nd entry:",
     >       ltoken(1),ltoken(2)
        endif
      endif
! New source.
      num_cat_src=num_cat_src+1
      cat_src_iau(num_cat_src)=ltoken(1)
      cat_src_name(num_cat_src)=ltoken(2)

      if(num_cat_src .lt. max_cat_src) goto 100
      write(luscn,
     >  '("Read_source_cat: Exceeded  maximum number of sources")')
      write(luscn,'("  keeping first: ",i4)') max_cat_src
190   continue
      close(lucat)

! Now read in the source grades for the sources that have them.
200   continue
      kcat_src=.true.
      ierr = 0

      return
      END
