      subroutine check_barrel_match(cmode_in,imode,kmatch,ierr)
! Check to see if there is a match between current stations and the mode.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
! Passed
      character*12 cmode_in
      integer imode          !which schedule mode.
! returned
      logical kmatch
      integer ierr
! HISTORY
!     2005Jun15  JMGipson. First version.
!     2005Jul06  JMGipson. cmode_in changed from char*8 to char*12.
!                          ltoken changed to char*12 to match.
!     2005Sep19  JMGipson.  Opps, was matching on only first 8 characters. Make it 12.
!     2005Sep19  JMGipson.  Skip comment lines.

! Functions
      integer trimlen
      integer iwhere_in_string_list

! local
      character*80 ldum                 !hold input line
      character*4 croll_tmp(max_stn)    !hold default barrel roll
      integer istat
      integer nch

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)

! Check to see if file rec.cat exists.
      open(lucat,file=rec_cat,status='old',iostat=ierr)
      nch = trimlen(rec_cat)
      if (ierr.ne.0) then
        write(luscn,9011) ierr,rec_cat(1:nch)
9011    format('Check_Barrel_Match: ERROR ',i5,' opening catalog ',a)
        close(lucat)
        return
      endif

      croll_tmp="NONE"                                  !Default barrel roll

! Read in the catalog until we find a match.
100   continue
      read(lucat,'(A80)',end=190) ldum
      if(ldum(1:1) .eq. "*" .or. ldum .eq. " ") goto 100

      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)
      if(ltoken(1) .ne. cmode_in) goto 100           !space until a match on the mode.
! Now we read in the list

120   read(lucat,'(A80)',end=190) ldum
      if(ldum(1:1) .eq. "*") goto 120                !skip comment lines.
      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)
      if(ltoken(1) .ne. "-") goto 190                     !No more continuation lines.

      if(NumToken .ne. 6) goto 120                        !We are only interested in lines with Barrell roll
      istat=iwhere_in_string_list(cantna,nstatn,ltoken(2))
      if(istat .ne. 0) then
        croll_tmp(istat)=ltoken(6)(1:4)
      endif
      goto 120


190   continue
      close(lucat)

200   continue
      kmatch=.true.
      do istat=1,nstatn
        if(cbarrel(istat,imode) .ne. croll_tmp(istat)) then
           kmatch =.false.
           return
        endif
      end do
      return
      end







