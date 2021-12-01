      SUBROUTINE wrllines(cloifname,cfrcode,nfr,ierr)
C
C  WRLLINES writes the "L" lines for the $CODES section,
C  for one frequency code.
C
C   HISTORY:
C 951130 nrv New.
C 960121 nrv Do not write out switching or channel index.
C 960223 nrv Change call for UNPLOIF, remove bbc and lb from this call.
C 960515 nrv Add ICHANSAVE to call
! 2005May19 JMGipson. ic,ichansave removed from call. Never used.
! 2005Oct06 JMGipson. Got rid of all holleriths.
! 2006May11 JMGipson. Got rid of extraneous Carriage Return
! 2006Jul26 JMGipson. Modified so that number of stations written to screen depends on width of screen.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C Input:
      character*2 cfrcode
      integer nfr(max_stn) ! number of freq channels
      character*10 cloifname(max_stn)

C  OUTPUT:
      integer ierr ! if error writing scratch file
C
C   SUBROUTINES
C     CALLED BY: WRFRS
C     CALLED: UNPLOIF
C
C  LOCAL VARIABLES
! function
      integer is
      integer iw,icol_wid

      integer MaxToken
      integer NumToken
      parameter(MaxToken=8)
      character*10 ltoken(MaxToken)
      logical keof
      logical kfound

      icol_wid=len(cantna(1))+len(cloifname(1))+4

C
C  1. Open the catalog. 
      call open_cat(loif_cat,ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

      iw=icol_wid

C  2. Loop over each station

      do is=1,nstatn ! all stations
        rewind(lucat,iostat=ierr)
        if (ierr.ne.0) return
        kfound=.false.
100     continue
        call skip_to_next_cat_group(keof)
        if(keof) goto 190

        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. cloifname(is)) goto 100

! Found a match
        if(iverbose_level.ge.5)
     >    WRITE(LUSCN,'(A,"(",a,") ",$)') cloifname(is),cantna(is)
        iw=iw+icol_wid
        if (iw.gt. iwscn) then
          if(iverbose_level.ge.5) write(luscn,'()')
          iw=icol_wid
        endif
! Now write out the L lines.
        kfound=.false.
        do while(.true.)
          call skip_to_next_non_comment(keof)
          if(keof) goto 190
          call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
          if(ltoken(1) .ne. "-") goto 190
          kfound=.true.
! Token     1   2   3        4      5       6
!              ibbc lif     band    clo     csb
!           -   1   A        X    7600.1    U
! Write it out in a different order.
           write(lutmp,"('L ',7(a,1x))")
     >       cstcod(is),cfrcode,ltoken(4)(1:2),ltoken(3)(1:2),ltoken(5),
     >       ltoken(2)(1:2),ltoken(6)
         enddo ! get extension lines

190     continue
        if (.not.kfound) then
          WRITE(LUSCN,9102) cloifname(is),cantna(is)
9102      format('wrllines: ERORR! - LO name "',a,'" for ',a,
     .          ' not in catalog.')
        endif
      enddo ! all stations

      ierr=0
      if(iw .ne. 0 .and. iverbose_level.ge.5)  write(luscn,'()')
      close(lucat)

      RETURN
      END
