      subroutine sumpl(ctype,nst,ist,xmin,xmax,ymin,ymax,
     .luplt,istcnt,kvis)

! AEM 20051107 created based on sumpl to use Gnuplot
C SUMPL creates and writes the control file lines, and calls
C       Gnuplot to draw the plot.

C History:
C 940330 nrv created
C 930503 nrv Add ending part, write right-side label
C 930616 nrv Add kvis argument so this same routine can be used
C            by muvis
C 980113 nrv Add option 4=ps file output
! 2003Oct28 JMGipson.  Modified to plot if more than 8 stations.
!                      Plots limited to max of 8/page. If several pages, then divided evenly.
!                      E.g., 11 stations would plot as two pages, one 6, one 5.
! 2005Nov07 AEM Gnuplot
! 2005Nov10 AEM max plots per page - 6 for Gnuplot version
! 2008May05 JMG.  Modified so that would not clip edges
! 2011Feb08 JMG. Ignore case on 'saveps' test
! 2013May01 JMG. Added "set polar" so that it would plot polar grid points for more recent 
!                versions !of gnuplot. Also added "set grid linewidth 1" to make grids darker. 

C Called by: sumcm, muvis

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

      integer system

C  Input:
      character*2 ctype
      integer nst,ist(*),luplt(*),istcnt(*)
      real*4 xmin,xmax,ymin,ymax
      logical kvis ! true if this is a visibility plot

C  Local:
      integer NumScreen,NumPerScreen,ibeg,iend,iscreen
      integer nch,i,j,ng,ierr,itype,ilabel,nch2,xdisp
      integer trimlen ! function
      character*128 cplnam
      character*24 label(2)
      character*4 ratio(4)
      character*8 orig(4,6)
      character*10 limt(5,2)
      character*40 rules(5,2)
      character*22 axis(5,2)
      character*4 tics(5,2)
      character*7 disp(4,2)
      character*12 extension(5)
      character*256 aem
      character*128 ctmpcommand
      character*128 cpsfile
      character*6 ctemp

C  Initialized:
! text axis labels
      data axis(1,1)/' '/,axis(1,2)/' '/ !xyPO
      data axis(2,1)/' "Time, hours"        '/ !xAZ
      data axis(2,2)/' "Azimuth, degrees"   '/ !yAZ
      data axis(3,1)/' "Time, hours"        '/ !xEL
      data axis(3,2)/' "Elevation, degrees" '/ !yEL
      data axis(4,1)/' "Azimuth, degrees"   '/ !xXY
      data axis(4,2)/' "Elevation, degrees" '/ !yXY
      data axis(5,1)/' "Separation, degrees"'/ !xDI
      data axis(5,2)/' "Percentage of Pairs"'/ !yDI
! axis tics
      data tics(1,1)/' '/,tics(1,2)/' '/ !PO
      data tics(2,1)/' 3  '/,tics(2,2)/' 60 '/ !AZ
      data tics(3,1)/' 3  '/,tics(3,2)/' 30 '/ !EL
      data tics(4,1)/' 60 '/,tics(4,2)/' 30 '/ !XY
      data tics(5,1)/' 30 '/,tics(5,2)/' 2  '/ !DI
! position of axis labels for screen and postscript plots
      data disp(1,1)/' 0,-1  '/,disp(1,2)/' -.5,0 '/
      data disp(4,1)/' 0,-2  '/,disp(4,2)/' -2,0  '/
! position of labels - common and per station
      data label/' at screen .5,.98 center',' at graph .5,.99 center '/
! size of area for single per station plot
      data ratio/' 1.0',' .60',' .45',' .45'/
! position of area for single per station plot
! Origin for single plot.   NEver happens.
      data orig(1,1)/' 0.0,0.0'/
! Origin for two plots.
      data orig(2,1)/' -.1,.25'/,orig(2,2)/' 0.4,.25'/
! ORigin for 3, 4 plots.
      data orig(3,1)/' 0.0,0.5'/,orig(3,2)/' 0.5,0.5'/
      data orig(3,3)/' 0.0,0.0'/,orig(3,4)/' 0.5,0.0'/

! Origins for 5,6 plots.
      data orig(4,1)/' 0.0,0.5'/,orig(4,2)/' .30,0.5'/
      data orig(4,3)/' .60,0.5'/,orig(4,4)/' 0.0,0.0'/
      data orig(4,5)/' .30,0.0'/,orig(4,6)/' .60,0.0'/
! data limits for various plot types
      data limt(1,1)/' [-93:93] '/,limt(1,2)/' [-93:99] '/ !PO
      data limt(2,1)/' [0:24]   '/,limt(2,2)/' [0:375]  '/ !AZ
      data limt(3,1)/' [0:24]   '/,limt(3,2)/' [0:96]   '/ !EL
      data limt(4,1)/' [0:360]  '/,limt(4,2)/' [0:96]   '/ !XY
      data limt(5,1)/' [0:180]  '/,limt(5,2)/' [0:14.9] '/ !DI
! file extensions for various plot types
      data extension/'_polazel','_az','_el','_xyazel','_distance'/
! using ($1!=1 ? $2 : 1/0):3 notitle with points/dots/lines
! drawing rules
      data rules(1,1)/' u ($1!=1 ? $2 : 1/0):3 notitle w p     '/ !PO sources
      data rules(1,2)/' u ($1==1 ? $2 : 1/0):3 notitle w d     '/ !PO visible limit
      data rules(2,1)/' u 2:3 notitle w p pt 2                 '/ !AZ sources
      data rules(3,1)/' u 2:3 notitle w p pt 2                 '/ !EL sources
      data rules(4,1)/' u ($1!=1 ? $2 : 1/0):3 notitle w p pt 2'/ !XY sources
      data rules(4,2)/' u ($1==1 ? $2 : 1/0):3 notitle w l     '/ !XY visible limit
      data rules(5,1)/' u ($1==1 ? $2 : 1/0):3 notitle w l     '/ !DI separation

C0 define plot type (default 1)
      itype = 1
      if(ctype.eq.'PO') itype = 1
      if(ctype.eq.'AZ') itype = 2
      if(ctype.eq.'EL') itype = 3
      if(ctype.eq.'XY') itype = 4
      if(ctype.eq.'DI') itype = 5
! AEM 20051111 select output unit - screen or file to write to ps-file type 'unit saveps'
      xdisp = 1 ! screen
      ctemp=ctpfil(1:6)
      call capitalize(ctemp)
      if(ctemp.eq.'SAVEPS') xdisp = 4 ! ps file

C    Create the control file, write the initial line to set the
C    window size. Use lutm2,ctmfi2 for the control file.

      NumScreen=(nst-1)/6 + 1
      numPerScreen=(nst-1)/NumScreen + 1
      if(numPerScreen.eq.1) ng = 1
      if(numPerScreen.eq.2) ng = 2
      if(numPerScreen.ge.3) ng = 3
      if(numPerScreen.ge.5) ng = 4

C1 start per-page generation of control file
      do iscreen=1,NumScreen
        ilabel = 2
        open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)

C2 write common lines in control file
        if(itype.ne.1) then
          write(lutm2,'(a)')
     >  "set xlabel"//axis(itype,1)//" offset "//disp(xdisp,1)
          write(lutm2,'(a)')
     >   "set ylabel"//axis(itype,2)//" offset "//disp(xdisp,2)
        endif
        write(lutm2,'(a)') "set xtics axis nomirror"
        write(lutm2,'(a)') "set ytics axis nomirror"
        if(itype.eq.1) then
          write(lutm2,'(a)') "unset border"
!set xtics border ("0" -90,"30" -60,"60" -30,"90" 0,"60" 30,"30" 60,"0" 90)
!set ytics border ("0" -90,"30" -60,"60" -30,"90" 0,"60" 30,"30" 60,"0" 90)
          aem = 'set xtics border ("0" -90,"30" -60,"60" -30,"90" 0,'//
     .	      '"60" 30,"30" 60,"0" 90)'
          write(lutm2,'(a)') aem(1:trimlen(aem))
          aem = 'set ytics border ("0" -90,"30" -60,"60" -30,"90" 0,'//
     .	      '"60" 30,"30" 60,"0" 90)'
          write(lutm2,'(a)') aem(1:trimlen(aem))
        else
          write(lutm2,'(a)') "set border 3 lw 0.5"
          write(lutm2,'(a)') "set xtics"//tics(itype,1)
          write(lutm2,'(a)') "set ytics"//tics(itype,2)
        endif
        write(lutm2,'(a)') "set xrange"//limt(itype,1)
        write(lutm2,'(a)') "set yrange"//limt(itype,2)
        if(itype.ne.5) then
          write(lutm2,'(a)') "set mxtics 3"
          write(lutm2,'(a)') "set mytics 3"
        else
          write(lutm2,'(a)') "set mxtics 3"
          write(lutm2,'(a)') "set mytics 2"
        endif
        write(lutm2,'(a)') "set grid linewidth 1"
        if(itype.eq.1) then
!          write(lutm2,'(a)') "set polar"
          write(lutm2,'(a)') "set grid polar"
        else
          write(lutm2,'(a)') "set grid xtics"
          write(lutm2,'(a)') "set grid ytics"
        endif
        write(lutm2,'(a)') "set size square"//ratio(1)
        write(lutm2,'(a)') "set origin"//orig(1,1)
        nch = trimlen(cskfil)
        if(kvis) then
! AEM comment: when kvis is true? and what it should be?	  
          write(lutm2,'(a)') "set label 1 'Source visibility from "//
     .         "schedule file "//cskfil(1:nch)//" for experiment "//
     .         cexper//"'"//label(1)
        else
          aem = "set label 1 'Observations from schedule file"
          nch2 = trimlen(aem)
          aem = aem(1:nch2+1)//cskfil(1:nch)//" for experiment"
          nch2 = trimlen(aem)
          write(aem(nch2+1:),'(1x,a," (",i6," scans)")') cexper,nobs
          if(NumScreen.gt.1) then
            nch2 = trimlen(aem)
            write(aem(nch2+1:),'(" page ",i1)') iscreen
          endif
          aem = aem(1:trimlen(aem))//"'"//label(1)
          write(lutm2,'(a)') aem(1:trimlen(aem))
        endif
	
! AEM 20051111 combine name of new .ps file
        if(xdisp.eq.4) then
          nch = trimlen(cskfil) - 4
          cpsfile = cskfil(1:nch)//extension(itype)
          if(NumScreen.eq.1) then
            nch = trimlen(cpsfile)
            cpsfile = cpsfile(1:nch)//".ps"
          else
            nch = trimlen(cpsfile)
            write(cpsfile(nch+1:),'("_p",i1,".ps")') iscreen
          endif
          write(*,*) "Creating PS file: "//trim(cpsfile)
          nch = trimlen(cpsfile)
          write(lutm2,'(a)') "set terminal postscript eps"//
     .                       " color solid lw 1 'Arial' 8"
          write(lutm2,'(a)') "set output '"//cpsfile(1:nch)//"'"
        endif
        if (ng.ge.2) write(lutm2,'(a)') "set multiplot"
        write(lutm2,'(a)') "set size square"//ratio(ng)

C3 write labels
        ibeg=(iscreen-1)*numPerScreen+1
        iend=min(numPerScreen*iscreen,nst)
        do i=ibeg,iend
          j=ist(i)
          write(lutm2,'(a)') "set origin"//orig(ng,i-ibeg+1)
          if(kvis) then
! AEM comment: when kvis is true? and what it should be?	  
            write(aem,'("set label ",i2)') ilabel
            aem = aem(1:trimlen(aem))//" '"//cstnna(j)
            write(aem(trimlen(aem)+1:),'(" (",a2,")")') cpocod(j)
            aem = aem(1:trimlen(aem))//"'"//label(2)
            write(lutm2,'(a)') aem(1:trimlen(aem))
            if(ilabel.ge.3) write(lutm2,'("unset label ",i2)') ilabel-1
            ilabel = ilabel + 1
          else
            write(aem,'("set label ",i2)') ilabel
            aem = aem(1:trimlen(aem))//" '"//cstnna(j)
            write(aem(trimlen(aem)+1:),'(" (",a2,")")') cpocod(j)
            write(aem(trimlen(aem)+1:),'(1x,i4," scans")') istcnt(j)
            aem = aem(1:trimlen(aem))//"'"//label(2)
            write(lutm2,'(a)') aem(1:trimlen(aem))
            if(ilabel.ge.3) write(lutm2,'("unset label ",i2)') ilabel-1
            ilabel = ilabel + 1
          endif

C4 file name, read data, draw the plot
          nch=trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cpocod(j)
          nch = trimlen(cplnam)
          aem = "plot '"//cplnam(1:nch)//"'"//rules(itype,1)
          write(lutm2,'(a)') aem(1:trimlen(aem))
          if(itype.eq.1.and.itype.eq.4) then
            aem = "replot '"//cplnam(1:nch)//"'"//rules(itype,2)
            write(lutm2,'(a)') aem(1:trimlen(aem))
          endif
	  
C5 done with the plot file
        enddo
        if (ng.ge.2) write(lutm2,'(a)') "unset multiplot"
        close(lutm2)
	
C6 close the files, call GNUPLOT to draw the plots
        do i=ibeg,iend
          j=ist(i)
          close(luplt(j))
        enddo
	
        nch = trimlen(ctmfi2)
        ctmpcommand="gnuplot -persist "//ctmfi2(1:nch)
        call null_term(ctmpcommand)
!        call system(ctmpcommand)
        ierr=system(ctmpcommand)
        if(ierr.ne.0) then
          write(*,*) "SUMPL: Error executing gnuplot file(s): ",ierr
          write(*,*) ctmpcommand
        endif

        do i=ibeg,iend
          j=ist(i)
          nch=trimlen(ctmfil)
          cplnam = ctmfil(1:nch)//'.'//cpocod(j)
          open(luplt(j),file=cplnam,status='unknown')
          close(luplt(j),status='delete')
        enddo
        open(lutm2,file=ctmfi2,status='unknown')
        close(lutm2,status='delete')

      enddo

      return
      end
