      SUBROUTINE STREAM_CMD(linstq)
C
C   STREAM_CMD analyzes scan "streams" for the Mk4 correlator.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
      include 'skstreams.ftni'
! functions
      integer istringminmatch
C
C     CALLING SUBROUTINES: SKED
C
C  Input
      integer*2 linstq(*)
C  LOCAL VARIABLES
      integer ifunc,ic1,ic2,nch,ich
      integer i2long,ichmv
      integer*2 lkeywd(10)
      integer j,i,max_used,iunerr,idum
      integer ierr,is,idh,idm

      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=2)
      character*6 list(ilist_len)
      data list/"EXPAND","NO"/

C
C  History
C 001003 nrv New. 
C
C
      ich=1
      call gtfld(linstq(2),ich,i2long(linstq(1)),ic1,ic2)
      IF (ic1 .LE. 0) THEN
        ifunc=2         !default is no.
      ELSE
        nch=ic2-ic1+1
        ckeywd=" "
        idum=ichmv(lkeywd(2),1,linstq(2),ic1,min0(nch,20))
        ifunc = istringMinmatch(list,ilist_len,ckeywd)
      END IF
      if (ifunc.le.0) then
        write(luscn,9109)
9109    format('STREAM00 - The only command option is EXPAND.')
        RETURN
      END IF

      do i=1,max_stream
        kopen_stream(i)=.true.
        nobs_stream(i)=0
        idur_stream(i)=0
      enddo
 
      do ircur=1,nobs
        cbuf=cskobs(iskrec(ircur))
        CALL UNPAK(IUNERR,0)
        CALL STREAM_ANL(ierr)
        if (ierr.lt.0) return
      END DO  !

      do i=1,max_stream
        if (nobs_stream(i).gt.0) max_used = i
      enddo
      write(ludsp,'("Number of streams used: ",i5)') max_used
      do i=1,max_used
        idh=idur_stream(i)/3600.d0
        idm=(idur_stream(i)-idh*3600.d0)/60.d0
        write(ludsp,
     > '("Stream:",i5,"  Scans:",i10," Time:",i10," (",i2," hr ",'//
     >   'i2," min)")') i,nobs_stream(i),idur_stream(i),idh,idm
      enddo
      if (list(ifunc).eq.'EXPAND') then ! expanded list
        do i=1,nobs_stream(1) ! longest list
          do j=1,max_used ! number across
            if (i.le.nobs_stream(j)) then ! this stream not done
! nothing is done with these times!
!              call seconds2hms(utstart_stream(j,i),ihr1,imin1,isc1)
!              call seconds2hms(utend_stream(j,i),ihr2,imin2,isc2)
              write(ludsp,9900) i,(scanname_stream(is,j,i),is=1,5)
9900          format(2x,i3,1x,5a2,$)
            endif ! this stream not done
          enddo ! number across
          write(ludsp,'()') ! end the line
        enddo ! longest list
      endif ! expanded list

      WRITE(LUDSP,9100)
9100  FORMAT('End of streams analysis.')
      RETURN
      END
C
