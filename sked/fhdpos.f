      SUBROUTINE fhdpos(ierr,chdpos,cname)
C
C  This routine reads the hdpos catalog, retrieves the information
C  for each station one at a time, and writes that information to
C  the sked working file, already open.
C
C   HISTORY:
C     gag   900202 CREATED
C     gag   900205 finalized logic and documentation
C     nrv   930225 implicit none
C 960207 nrv Modified for new catalogs.
C 960619 nrv List antenna name if not found, not position name.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C  INPUT:
      character*8 chdpos(max_stn)
      character*2 cname

C  OUTPUT
      integer ierr
! function
      integer trimlen
C
C  LOCAL:
      integer nch  !variable and function for var length
      integer ind
      logical kfound ! false until a station is matched
      integer i
      logical keof

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*8 ltoken(MaxToken)

C   SUBROUTINES
C     CALLED BY:  WRFRS
C     CALLED:   CATAS

C  1. Open the catalog and go through once for every station.
      call open_cat(hdpos_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return 
      endif

! get the hdpos stuff for each station.
      do i=1,nstatn
        rewind(lucat,iostat=ierr)
        if (ierr.ne.0) return
        kfound=.false.
100     continue
        call skip_to_next_cat_group(keof)
        if(keof) goto 190

        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1)(1:8) .ne. chdpos(i)) goto 100

! found a match
        do while(.true.)
          call skip_to_next_non_comment(keof)
          if(keof) goto 190
          call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
          if(ltoken(1) .ne. "-") goto 190

          ind=index(cbuf,"-")
          nch=trimlen(cbuf)
          writE(lutmp,'(a,1x,a2,1x,a)') cstcod(i),cname,cbuf(ind+2:nch)
          kfound=.true.
        end do

190     continue
        if (.not.kfound) then
          write(luscn,9910) cantna(i),chdpos(i),cname
9910      format('FHDPOS02 - Warning: head positions ',a,' not found',
     .    ' for ',a,', frequency code ',a2)
        endif
      end do  !all stations
C
      RETURN
      END
