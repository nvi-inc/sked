      SUBROUTINE SUMOUT(linstq)
C
C     SUMOUT generates a compact summary of the schedule.
C
C 971124 nrv New.
C 980114 nrv Add 1-letter code to output.
C 980116 nrv Put the file in the local area, not on temp. Change name
C            to sksum.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
C
C     CALLING SUBROUTINES: SKED
C
C Input:
      integer*2 linstq(*)
! functions
      integer trimlen,julda !functions

C LOCAL VARIABLES
      character*128 ctmp
      integer*2 lmon(2),lday(2)
      integer mjd,iyr,ida,ihr,imin,isc,iday,imon,nch,ierr,i

      character*6 lbeg_end
      integer iptr

      character*12 ctime
C
C  1. Make sure there is enough information.

      if (nsourc.eq.0.or.nstatn.eq.0.or.nobs.eq.0) then
        write(luscn,9102) 
9102    format('Select sources and stations first.')
        goto 900
      endif

C  1.5 Get file name or generate a default file name.

      if (linstq(1).gt.0) then !file name specified
        nch=linstq(1)
        call hol2char(linstq(2),1,nch,ctmp)
      else !default file name
        if (cexper .eq. " ") then
          ctmp = 'exper-sksum.txt'
        else !use experiment name
          ctmp=cexper
          call c2lower(ctmp,ctmp)
          nch = trimlen(ctmp)
          ctmp = ctmp(1:nch)//'-sksum.txt'
        endif
      endif
 
C  2. Create the output file. 

      nch = trimlen(ctmp)
      open(lutmp,file=ctmp,status='unknown',iostat=ierr)
      CLOSE(lutmp,status='delete')
      OPEN (lutmp,file=ctmp,status='NEW',iostat=IERR)
      if (ierr.ne.0) then
        write(luscn,9201) ierr,ctmp(1:nch)
9201    format('SUMOUT: Error ',i5', trying to create SUMOUT output',
     .  ' file ',a)
        goto 900
      else
C       write(luscn,9291) ctmp(1:nch)
C9291    format('SUMOUT: Opened output file ',a)
      endif

C  3.  Write experiment name on first line

      write(lutmp,'("Session ",a)') cexper

C  4.  Write the start date/time, end time, recording mode.
C      Unpack the first and last obs.
      do i=1,2
        if(i .eq. 1) then
          iptr=iskrec(1)
          lbeg_end='Start'
        else
          iptr=iskrec(nobs)
          lbeg_end='End '
        endif

        call sktime(cskobs(iptr),ctime)
        read(ctime,'(i2,i3,i2,i2,i2)') iyr,ida,ihr,imin,isc
        MJD = JULDA(1,IDA,IYR)
        call ymday(iyr,ida,imon,iday)
        CALL CLNDR(IYR,IMON,IDAY,LMON,LDAY)
C     Note clndr converts "97" to "1997"
        write(lutmp,9301)lbeg_end, iyr,ida,ihr,imin,isc,lmon,iday
9301    format(a,i4,'-',i3.3,1x,i2.2,':',i2.2,':',i2.2,1x,2a2,' ',i2.2)
      end do


C  5. Write out the station list

      write(lutmp,9401) nstatn
9401  format('Stations',i6)
      do i=1,nstatn
        write(lutmp,9402) cpocod(i),cstnna(i),cstcod(i)
9402    format(a2,2x,a8,2x,a1)
      enddo

      close(lutmp)
9591  format('SUMOUT: Output file ',a)
 
900   RETURN
      END
