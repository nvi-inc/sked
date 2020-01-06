      SUBROUTINE flinp(cBUF,lu,ierr)
C
C     FLINP reads and decodes a source flux line, and puts
C           the information into common
C
      include '../skdrincl/skparm.ftni'
C  Common
      include '../skdrincl/sourc.ftni'
      include 'flux.ftni'
C
C  INPUT:
      character*(*) cbuf
      integer lu
C      - buffer holding source entry
C     ILEN - length of IBUF in words
C
C  OUTPUT:
      integer ierr
C     IERR - error number
! functions
      integer igetsrcnum
      integer trimlen
      integer igtba

C
C  Called by: FLGET
C
C  LOCAL:
      character*(max_sorlen) csrcnam
      real*4 fl(max_flux)

      logical ktoken,knospace,keof
      character*30 ltoken
      integer istart,inext

C      - temporary baseline/flux holders for unpacking
      integer j,j1,nfl,i,isor,ib
      character*1 cfl
C
C  PROGRAMMER: NRV
C   NRV 891113 Created, based on SOINP
C   NRV 910924 Change UNPFL call, store in new flux variables
C   NRV 911106 Fixed calculation of number of flux steps
C   nrv 950626 Make IGTBA a function
C 970114 nrv Change dimension of lsrcnam to max_sorlen
C   2003Dec08 JMGipson  Changed igtso to igetsrcnum
C   2004Apr29 Changed unpfl to extractnexttoken
! 2007Jul02  JMG  Added flux.ftni
! 2009Oct13 JMG. Modified so that a "!" means the rest of the line is a comment


C     1. Call UNPFL to unpack the buffer we were passed.
C     Put all of the fields into temporary variables.
C
      ierr=0
! get the source name.
      istart=1

      call ExtractNextToken(cbuf,istart,inext,csrcnam,ktoken,
     >     knospace,keof)
      if(.not.ktoken .or. knospace .or. keof) goto 900

      isor=igetsrcnum(csrcnam)
      if(isor .le. 0) return

! Band.
      istart=inext
      call ExtractNextToken(cbuf,istart,inext,ltoken,ktoken,
     >     knospace,keof)
      if(.not.ktoken .or. knospace .or. keof) goto 900
      i=trimlen(ltoken)
      if(i .gt. 2) then
         write(lu,*) "FLINP: Error in band. "
         goto 900
      endif

      ib = igtba(ltoken)
      if(ib .lt. 0) then
         write(lu,*) "FLINP: Error in band. "
         goto 900
      endif

! Type
      istart=inext
      ltoken=" "
      call ExtractNextToken(cbuf,istart,inext,ltoken,ktoken,
     >     knospace,keof)
      if(.not.ktoken .or. knospace .or. keof) goto 900
      i=trimlen(ltoken)
      if(i .gt. 1) then
        cfl="B"              !old stype model. (didn't specify B or M.)
        inext=istart         !set to re-read.
      else
        cfl=ltoken(1:1)
      endif

! Now read in the components.
      nfl=0
      do while(ktoken .and. .not.(knospace .or. keof))
         istart=inext
         ltoken=" "
         call ExtractNextToken(cbuf,istart,inext,ltoken,ktoken,
     >      knospace,keof)
         if(.not.ktoken .or. knospace .or. keof) goto 50
         if(ltoken(1:1)  .eq. "!") goto 50     !comment 
         nfl=nfl+1
         read(ltoken,*,err=900) fl(nfl)
      end do

50    continue
      if(cfl .eq. "M" .and. nfl .gt. 6) then
        write(*,*) "FLINP: Model fluxes have a maximum of 6 components."
        goto 900
      endif

C
C     2. Find out which source and which band.
C        Store baselines and flux into common arrays.
C
      if (ib.gt.0.and.isor.gt.0) then !band and source are selected
        if(cfltype(ib,isor) .ne. cfl) then
          nflux(ib,isor)=0       !Change in model type (baseline<-->model), reset values.
        endif
        cfltype(ib,isor) = cfl
        if (cfl.eq.'M') then !model
          if(nflux(ib,isor) .lt. 0) then  !This reinitializes source models.
             nflux(ib,isor)=0
          endif
          nflux(ib,isor)=nflux(ib,isor)+1
          if (nflux(ib,isor).gt.MAX_FLUX/6) then
            write(lu,'(a,i4)')
     >       "FLINP04 - Too many model components, max is ",
     >        MAX_FLUX/6
              goto 900
          endif
          do j=1,6
            j1 = 1 + (nflux(ib,isor)-1)*6
            flux(j+j1-1,ib,isor) = fl(j)
          enddo
        else !baseline pairs
          nflux(ib,isor) = ((nfl+1)/2)-1
          if (nflux(ib,isor).gt.MAX_FLUX) then
            write(lu,'(a,i4)')
     >      "FLINP04 - Too many baseline/flux entries:  max is ",
     >       MAX_FLUX
            goto 900
          endif
          DO j=1,nfl
            flux(j,ib,isor) = fl(j)
          END DO
        endif
      endif
      RETURN

900   continue
      write(lu,*) "FLINP: Error reading line: ", trim(cbuf)
      write(lu,*)  "Fix flux catalog before continuing."
      stop
      ierr=1
      pause 
      return

      END
