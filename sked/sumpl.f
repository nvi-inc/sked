      subroutine sumpl(ctype,nst,ist,xmin,xmax,ymin,ymax,
     .luplt,istcnt,kvis)

C SUMPL creates and writes the control file lines, and calls
C       PC8 to draw the plot.

C History:
C 940330 nrv created
C 930503 nrv Add ending part, write right-side label
C 930616 nrv Add kvis argument so this same routine can be used
C            by muvis
C 980113 nrv Add option 4=ps file output
! 2003Oct28 JMGipson.  Modified to plot if more than 8 stations.
!                      Plots limited to max of 8/page. If several pages, then divided evenly.
!                      E.g., 11 stations would plot as two pages, one 6, one 5.
! 2011Feb08 JMG. Ignore case on 'saveps' test

C Called by: sumcm, muvis
C Calls: pc8

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

      integer system
      integer renam

C  Input:
      character*2 ctype
      integer nst,ist(*),luplt(*),istcnt(*)
      real*4 xmin,xmax,ymin,ymax
      logical kvis ! true if this is a visibility plot
      character*12 lpgplotfil

C  Local:
      integer NumScreen, NumPerScreen,ibeg,iend,iscreen
      character*25 ldum
      integer nch,i,j,ixtick,iytick,ng,ierr,itype
      integer*4 ixdisp
      integer trimlen ! function
      character*128 cplnam
      character*6 ctemp
      character*7 begin(5)
      character*15 view(5,8,2)
      character*7 label(5,8,2)

C  Initialized:
      data begin/'500 500','600 300','600 600','750 500','999 500'/
      data view(1,1,1)/'.01 .95 .01 .95'/,view(1,1,2)/'.06 .94 .06 .94'/
      data view(2,1,1)/'.03 .49 .01 .96'/,view(2,1,2)/'.06 .46 .06 .94'/
      data view(2,2,1)/'.51 .97 .01 .96'/,view(2,2,2)/'.54 .94 .06 .94'/
      data view(3,1,1)/'.03 .49 .50 .96'/,view(3,1,2)/'.06 .46 .54 .94'/
      data view(3,2,1)/'.51 .97 .50 .96'/,view(3,2,2)/'.54 .94 .54 .94'/
      data view(3,3,1)/'.03 .49 .01 .48'/,view(3,3,2)/'.06 .46 .06 .46'/
      data view(3,4,1)/'.51 .97 .01 .48'/,view(3,4,2)/'.54 .94 .06 .46'/
      data view(4,1,1)/'.03 .33 .50 .96'/,view(4,1,2)/'.04 .32 .52 .94'/
      data view(4,2,1)/'.35 .65 .50 .96'/,view(4,2,2)/'.36 .64 .52 .94'/
      data view(4,3,1)/'.67 .97 .50 .96'/,view(4,3,2)/'.68 .96 .52 .94'/
      data view(4,4,1)/'.03 .33 .01 .48'/,view(4,4,2)/'.04 .32 .04 .46'/
      data view(4,5,1)/'.35 .65 .01 .48'/,view(4,5,2)/'.36 .64 .04 .46'/
      data view(4,6,1)/'.67 .97 .01 .48'/,view(4,6,2)/'.68 .96 .04 .46'/
      data view(5,1,1)/'.03 .25 .50 .96'/,view(5,1,2)/'.04 .24 .52 .94'/
      data view(5,2,1)/'.27 .49 .50 .96'/,view(5,2,2)/'.28 .48 .52 .94'/
      data view(5,3,1)/'.51 .73 .50 .96'/,view(5,3,2)/'.52 .72 .52 .94'/
      data view(5,4,1)/'.75 .97 .50 .96'/,view(5,4,2)/'.76 .96 .52 .94'/
      data view(5,5,1)/'.03 .25 .01 .48'/,view(5,5,2)/'.04 .24 .04 .46'/
      data view(5,6,1)/'.27 .49 .01 .48'/,view(5,6,2)/'.28 .48 .04 .46'/
      data view(5,7,1)/'.51 .73 .01 .48'/,view(5,7,2)/'.52 .72 .04 .46'/
      data view(5,8,1)/'.75 .97 .01 .48'/,view(5,8,2)/'.76 .96 .04 .46'/
      data label(1,1,1)/'.06 .95'/,label(1,1,2)/'.84 .95'/
      data label(2,1,1)/'.06 .95'/,label(2,1,2)/'.39 .95'/
      data label(2,2,1)/'.54 .95'/,label(2,2,2)/'.84 .95'/
      data label(3,1,1)/'.06 .95'/,label(3,1,2)/'.36 .95'/
      data label(3,2,1)/'.54 .95'/,label(3,2,2)/'.84 .95'/
      data label(3,3,1)/'.06 .47'/,label(3,3,2)/'.36 .47'/
      data label(3,4,1)/'.54 .47'/,label(3,4,2)/'.84 .47'/
      data label(4,1,1)/'.04 .95'/,label(4,1,2)/'.27 .95'/
      data label(4,2,1)/'.36 .95'/,label(4,2,2)/'.59 .95'/
      data label(4,3,1)/'.68 .95'/,label(4,3,2)/'.91 .95'/
      data label(4,4,1)/'.04 .47'/,label(4,4,2)/'.27 .47'/
      data label(4,5,1)/'.36 .47'/,label(4,5,2)/'.59 .47'/
      data label(4,6,1)/'.68 .47'/,label(4,6,2)/'.91 .47'/
      data label(5,1,1)/'.04 .95'/,label(5,1,2)/'.19 .95'/
      data label(5,2,1)/'.28 .95'/,label(5,2,2)/'.43 .95'/
      data label(5,3,1)/'.52 .95'/,label(5,3,2)/'.67 .95'/
      data label(5,4,1)/'.76 .95'/,label(5,4,2)/'.91 .95'/
      data label(5,5,1)/'.04 .47'/,label(5,5,2)/'.19 .47'/
      data label(5,6,1)/'.28 .47'/,label(5,6,2)/'.43 .47'/
      data label(5,7,1)/'.52 .47'/,label(5,7,2)/'.67 .47'/
      data label(5,8,1)/'.76 .47'/,label(5,8,2)/'.91 .47'/


C 1. Create the control file, write the initial line to set the
C    window size. Use lutm2,ctmfi2 for the control file.

      NumScreen=(nst-1)/8+1

      numPerScreen=(nst-1)/NumScreen+1
      if (numPerScreen.eq.1) ng = 1
      if (numPerScreen.ge.2) ng = 2
      if (numPerScreen.ge.3) ng = 3
      if (numPerScreen.ge.5) ng = 4
      if (numPerScreen.ge.7) ng = 5

      do iscreen=1,NumScreen
        open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
        write(lutm2,9599) begin(ng)
9599    format('begin ',a)

C 2. Write header line in control file

        write(lutm2,'("charsz 1.0")')
        nch = trimlen(cskfil)
        if (kvis) then
          write(lutm2,9603) cskfil(1:nch),cexper
9603      format(
     >     'label .04 .98 1 0 Source visibility from schedule file '
     >    ,a,' for experiment ',a8)
        else
          write(lutm2,9604) cskfil(1:nch),cexper,nobs
9604      format('label .04 .98 1 0 Observations from schedule file ',
     .    a,' for experiment ',a8,' (',i4,' scans)')
        endif
        itype = 2
        if (ctype.eq.'PO') itype=1
        if (ng.gt.3) write(lutm2,'("charsz 0.5")')


!       do i=1,nst
        ibeg=(iscreen-1)*numPerScreen+1
        iend=min(numPerScreen*iscreen,nst)
        do i=ibeg,iend
          j=ist(i)
          write(lutm2,9608) view(ng,i-ibeg+1,itype)
9608      format('view ',a)

! Setup top of file for different kinds of plots.
C 3. EL plot -- elevation vs. time
          if (ctype.eq.'EL') then
            write(lutm2,9609)
9609        format('diffsyms 1'/
     .      'x_field 1 2 0 Time (hours)'/
     .      'y_field 1 3 0 Elevation (degrees)'/
     .      'axes BCNT BCNT')
            ixtick = 3
            iytick = 10
            if (xmax-xmin.le.3.0) ixtick = 1
            if (ymax-ymin.le.10.0) iytick = 2
            if (ymax-ymin.le.20.0) iytick = 5
C           if (ng.gt.1) iytick = 30
C  4. AZ plot -- azimuth vs. time
          else if (ctype.eq.'AZ') then
            write(lutm2,9611)
9611        format('diffsyms 1'/
     .      'x_field 1 2 0 Time (hours)'/
     .      'y_field 1 3 0 Azimuth (degrees)'/
     .      'axes BCNT BCNT')
            ixtick = 3
            iytick = 30
            if (xmax-xmin.le.3.0) ixtick = 1
            if (ymax-ymin.le.30.0) iytick = 5
            if (ng.gt.1) iytick = 90
C  5. XY plot -- elevation vs. azimuth
          else if (ctype.eq.'XY') then
            write(lutm2,9612)
9612        format('diffsyms 1'/
     .      'x_field 1 2 0 Azimuth (degrees)'/
     .      'y_field 1 3 0 Elevation (degrees)'/
     .      'axes BCNT BCNT')
            ixtick = 30
            iytick = 30
            if (xmax-xmin.lt.90.0) ixtick = 10
            if (ymax-ymin.lt.30.0) iytick = 5
C  7. DI plot -- histogram of spherical distances between obs
          else if (ctype.eq.'DI') then
            write(lutm2,9605)
9605        format('diffsyms 1'/
     .      'line 1'/
     .      'x_field 1 2 0 Separation (degrees)'/
     .      'y_field 1 3 0 Percentage of Pairs'/
     .      'axes BCNT BCNT')
            ixtick = 30
            iytick = 2
C  6. PO plot -- polar plot of elevation and azimuth
          else if (ctype.eq.'PO') then
            write(lutm2,9610)
9610        format('diffsyms 1'/
     >      'x_field 1 2 0'/
     >      'y_field 1 3 0'/
     >      'axes AT AT'/
     >      'window -90 90 -90 90'/
     >      'x_mjtiv 10'/
     >      'y_mjtiv 10')
          endif

! some special stuff if not PO file.
          if (ctype.ne.'PO') write(lutm2,9613) ixtick,iytick,
     .      xmin,xmax,ymin,ymax
9613      format('x_mjtiv ',i2/'y_mjtiv ',i2/'window ',4f6.1)

C  8. Axis labels. 

          if(kvis) then
            write(lutm2,9616) label(ng,i-ibeg+1,1),cstnna(j),
     .      cpocod(j),label(ng,i-ibeg+1,2)
9616        format('label ',a,' 1 0 ',a8,'(',a2,')'/
     .             'label ',a,' 1 0 ')
          else
            write(lutm2,9615) label(ng,i-ibeg+1,1),cstnna(j),
     .      cpocod(j),label(ng,i-ibeg+1,2),istcnt(j)
9615        format('label ',a,' 1 0 ',a8,'(',a2,')'/
     .           'label ',a,' 1 0 ',i4,' scans')
          endif

C  7. File name, read data, draw the plot
!          write(cnum,'(a2)') lpocod(j)
          nch=trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cpocod(j)
          nch = trimlen(cplnam)
          write(lutm2,9614) cplnam(1:nch)
9614      format('file ',a/'read'/'draw')
C  9. Done with the control file
        enddo
        write(lutm2,'("end")')
        close(lutm2)
C 10. Close the files, draw the plots.
        do i=ibeg,iend
          j=ist(i)
          close(luplt(j))
        enddo
        ixdisp = 1 ! screen
        if (ludsp.eq.lufil) ixdisp = 2 ! printed output

        ctemp=ctpfil(1:6)
        call capitalize(ctemp)
        if(ctemp.eq.'SAVEPS') ixdisp = 4 ! ps file
 
        nch = trimlen(ctmfi2)
        call pc8(ixdisp,ctmfi2,nch)
        if(NumScreen .gt. 1 .and. ixdisp .eq. 4) then
           write(lpgplotfil,'("pgplot",i1,".ps")') iscreen
           ierr=renam("pgplot.ps", lpgplotfil)
           if(ierr .ne. 0) then
             write(*,*) "SUMPL: Error making postscript files: ",ierr
             write(*,*) "When executing ",ldum
           endif
        endif

        do i=ibeg,iend
          j=ist(i)
!          write(cnum,'(a2)') lpocod(j)
          nch=trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cpocod(j)
          open(luplt(j),file=cplnam,status='unknown')
          close(luplt(j),status='delete')
        enddo
        open(lutm2,file=ctmfi2,status='unknown')
        close(lutm2,status='delete')
      end do

      return
      end
