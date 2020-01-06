      SUBROUTINE read_loif_cat(ierr)
! Read in the frequency catalog.
! Only read in modes defined in modes.cat (Read in previously).
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
      include 'cat_mode.ftni'
      include 'cat_freq.ftni'
      include 'cat_rx.ftni'
      include 'cat_loif.ftni'
! Returned variable
      integer ierr      !0=no error.

! History
!  2019Sep03  JMG. Added implicit none.
!  2008Jun11  JMGipson.  Only write to screen if verbose is on.
!  2005Nov21 JMGipson  First version.

!
! Functions
      integer trimlen
      integer iwhere_in_string_list
! Local variables
      integer iloif      !which RX number
!      integer ichan    !which channel
      logical keof

! Holds tokenized line.
      integer MaxToken
      integer NumToken
      parameter(MaxToken=6)
      character*10 ltoken(MaxToken)
! More stuff
      integer nch
      integer iwid
      integer icnt

!  1. Open the frequency catalog
      ierr=0
      if(kcat_loif) return
      call open_cat(loif_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
         return
      endif

! 2.  Read the catalog

! Lines in the catalog look like:
*LOIF_name
*  BBC/VC IF Band Freq  SB
! CDP_STDN
! -   1  1N    X   8080  U
! -   2  1N    X   8080  U

! We skip the LOIF names that weren't picked up in read_rx_cat.f

! 3. Should be at an LOIF line.
100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 200
110   continue
! Parse a line that looks like:

      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      if(NumToken .ne. 1) then
         write(*,*) "Read_LOIF_CAT! Shouldn't get here!"
         write(*,*) "Tell John Gipson."
         write(*,*) "Parsing: "//trim(cbuf) 
         stop
      endif
      iloif=iwhere_in_string_list(cat_loif_name,num_cat_loif,ltoken(1))
      if(iloif .eq. 0) goto 100

120   continue
      icnt=0
      do while(.true.)
        ierr=0
        call skip_to_next_non_comment(keof)
        if(keof) goto 200
! Parse it.
! -   2  1N    X   8080  U
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 110                          !Not a continution line. Look at next sequence

        icnt=icnt+1
        num_chan_cat_loif(iloif)=icnt
        ierr=-2
        read(ltoken(2),*,err=190) ibbc_cat_loif(iloif,icnt)
        ierr=-3
        cat_loif_if(iloif,icnt)  =ltoken(3)
        cat_loif_band(iloif,icnt)=ltoken(4)
        ierr=-5
        read(ltoken(5),*,err=190) freq_cat_loif(iloif,icnt)
        cat_loif_sb(iloif,icnt)=ltoken(6)
      end do

190   continue
      nch=trimlen(cbuf)
      write(luscn,'("read_loif_cat: Error in filed: ",i4)') -ierr
      write(luscn,'(a)') cbuf(1:nch)
      ierr=1

200   continue
      kcat_loif=.true.
      if(kverbose) then
      write(luscn,'("Read_loif_cat: num_loifs/max_loifs: ",i4,"/",i4)')
     >    num_cat_loif,max_cat_loif
      endif

      close(lucat)
      return
      end
