      SUBROUTINE ffreq(cfrq_name, c2code,crxname,nrx,cb,
     >   cpol,csky,csb,ichan,ibbc,cpcfr,csw,nfrq,ierr)
     
! 2022-01-10 JMGipson. Close lucat on error (previously closed lutmp
! 2005Oct07 JMGipson. Replaced hollerith, got rid of call to unpfreq
! 2006May11 JMGipson. Did not give error message if did not find frequency.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C Input:
      character*8 cfrq_name

C  OUTPUT:
      integer nrx ! number of subcodes put into LRXNAME
      integer ierr ! if error reading catalog file
      character*2 c2code
      character*8 crxname(max_stn)
      character*2 cb(max_chan,max_stn)      ! band ID from freq lines
      character*2 cpol(max_chan,max_stn)
      character*8 csky(max_chan,max_stn)    ! sky freq from freq lines
      character*2 csb(max_chan,max_stn)     ! sideband from freq lines
      integer     ichan(max_chan,max_stn)   ! channel ID from freq lines
      integer     ibbc(max_chan,max_stn)    ! BBC number from freq lines
      character*8 cpcfr(max_chan,max_stn)   ! pcal freq from freq lines
      character*4 csw(max_chan,max_stn)     ! switching from freq lines
      integer     nfrq(max_stn)              ! number of freq lines
C
C   SUBROUTINES
C     CALLED BY: WRFRS
! functins
      integer trimlen

C  LOCAL VARIABLES
      integer MaxToken
      integer NumToken
      parameter(MaxToken=10)
      character*8 ltoken(MaxToken)
! other local

      integer ifr
      logical keof

C  1. Open the frequency catalog 
 
      call open_cat(freq_cat,ierr)
      if (ierr.ne.0) then
        close(lucat)
        return
      endif
C  2.  Find each sub-code in the freq.cat file and save the frequency info.
      nrx=0
      ierr=0
100   continue
      call skip_to_next_cat_group(keof)
      if(keof) goto 200
110   continue
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      if(ltoken(1) .ne. cfrq_name) goto 100
      if(nrx .eq. 0) then
         if(iverbose_level.ge.5) write(luscn,'(a,": ",$)') cfrq_name
      endif
! Parse a line like:
!    CDPSX-WB WB SW  VLBA_WID
      nrx=nrx+1     !update number of codes.
      c2code=ltoken(2)
      crxname(nrx)=ltoken(4)
      if(iverbose_level.ge.5) write(luscn,'(a," ",$)') crxname(nrx)

! At this piont, found a match.
      ifr=0
! Process the lines
      do while(.true.)
        ierr=0
        call skip_to_next_non_comment(keof)
        if(keof) goto 200
        call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
        if(ltoken(1) .ne. "-") goto 110          !done with this subcode
        ifr=ifr+1
! Unpack a line like:
!           1 2    3    4       5   6       7      8        9
!           - X    R   8212.99  U   CH1     1    10000.0    1,2
        if(NumToken .lt. 7 .or. NumToken .gt. 9) then
          ierr=-99
          write(luscn,'("ffreq: ERROR! Invalid line: ",a)')
     >       cbuf(1:trimlen(cbuf))
          goto 200
        endif

        cb(ifr,nrx)=ltoken(2)
        cpol(ifr,nrx)=ltoken(3)
        csky(ifr,nrx)=ltoken(4)
        csb(ifr,nrx)=ltoken(5)

        if(ltoken(6)(3:3) .eq. "X") then
          ichan(ifr,nrx)=-99
        else
          ierr=-5
          read(ltoken(6)(3:5),*,err=190) ichan(ifr,nrx)
        endif
        ierr=-6
        read(ltoken(7),*,err=190) ibbc(ifr,nrx)
        cpcfr(ifr,nrx)=ltoken(8)
        if(numToken .ge. 9) then
          csw(ifr,nrx)=ltoken(9)
        else
          csw(ifr,nrx)=" "
        endif
        nfrq(nrx)=ifr
      enddo ! get all frequencies

190   continue
      write(luscn,'("Ffreq: ERROR in field ",i4)') -ierr
      write(luscn,'(a)') cbuf(1:trimlen(cbuf))

200   continue
      if(nrx .eq. 0) then
        write(luscn,'("Ffreq: ERROR! Code not found ",a)') cfrq_name
        ierr=1
      else
        if(iverbose_level.ge.5) write(luscn,'()')
      endif
      close(lucat) 
      return
      end
