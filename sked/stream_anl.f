      SUBROUTINE STREAM_ANL(ierr)
C
C   STREAM_ANL accumulates information in stream arrays
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
      include 'skstreams.ftni'
C
C  Input
      integer ierr
C    LOCAL VARIABLES
      double precision utend_scan
      integer il,is,j,i,idurstr,mjdend_scan
      integer trimlen
C
C History
C 001003 nrv New. 
C 001109 nrv Scan_name is character.
C
C
      ierr=0

C 1. Determine end time for the current scan.
      idurstr = -1
      do i=1,nstncur
        J = ISTCUR(I)
        if (idurcur(j).gt.idurstr) idurstr=idurcur(j)
      enddo
      call addsec2ut(mjdcur(istcur(1)),utcur(istcur(1)),
     >  idurstr,mjdend_scan,utend_scan)

C 2. For each stream, if the current scan starts later than the
C    stream end time, then mark that stream as now available.
      do i=1,max_stream
        j=nobs_stream(i)
        if (nobs_stream(i).ge.1) then ! this stream has started
          if (.not.kopen_stream(i)) then ! can it be opened?
            if ((mjdcur(istcur(1)).gt.mjdend_stream(i,j)).or.
     .       (mjdcur(istcur(1)).eq.mjdend_stream(i,j).and.
     .        utcur(istcur(1)).gt.utend_stream(i,j))) then
              kopen_stream(i)=.true.
            endif
          endif ! can it be opened?
        endif ! this stream has started
      enddo

C 3. Find the first available stream number.
      is= 1
      do while (is.le.max_stream.and..not.kopen_stream(is))
        is=is+1
      enddo
      if (is.gt.max_stream) then
        il=trimlen(scan_name(iskrec(ircur)))
        write(luscn,9901) max_stream,ircur,
     .  (scan_name(iskrec(ircur))(1:il))
C    .  (scan_name(i,iskrec(ircur)),i=1,5)
9901    format('STREAM01 - Too many streams. Max is ',i5,
     .  ' Scan ',i5,2x,a)
        ierr=-1
        return
      endif

C 4. For stream "is", increment its count and save the start
C    time, end time, and scan ID.

      kopen_stream(is) = .false. ! we're in use now
      nobs_stream(is) = nobs_stream(is)+1
      idur_stream(is)=idur_stream(is)+idurstr
      j = nobs_stream(is)
      utstart_stream(is,j)=utcur(istcur(1))
      mjdstart_stream(is,j)=mjdcur(istcur(1))
      utend_stream(is,j)=utend_scan
      mjdend_stream(is,j)=mjdend_scan
C     idum = ichmv(scanname_stream(1,is,j),1,
C    .             scan_name(1,iskrec(ircur)),1,10)
      call char2hol(scan_name(iskrec(ircur)),scanname_stream(1,is,j),
     >   1,10)

!      call seconds2hms(utstart_stream(is,j),ihr1,imin1,isc1)
!      call seconds2hms(utend_stream(is,j),ihr2,imin2,isc2)
C      write(luscn,9902)
C9902  format('Obs Stream Scan Start  End')
C      write(luscn,9900) j,is,(scanname_stream(i,is,j),i=1,5),
C     Hihr1,imin1,isc1,ihr2,imin2,isc2
C9900  format(2x,i2,1x,i2,1x,5a2,2(2x,i2.2,':',i2.2,':',i2.2))
C      write(luscn,9902) is,nobs_stream(is),
C     .utstart_stream(is,nobs_stream(is)),
C     .utend_stream(is,nobs_stream(is))
C9902  format(i2,i4,f10.2,f10.2)

      return
      end
