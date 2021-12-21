C
      subroutine soplt
C
C     SOPLT plots source ra/dec
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
C
C  INPUT: none
C  OUTPUT: none
C  Subroutines: Calls sonum
C               Called by socmd
C
C  LOCAL
      real*8 ras,decs
      integer mjd,i,iyr,ic
      integer julda,system !function
      integer ierr,trimlen,sonum
      integer nch,nch2,nch3,xdisp
      character*128 ctmpcommand
      character*128 cpsfile
      character*6 ctemp
C
C  HISTORY
!  2005Mar23 JMGipson. Created based on soplt.
!  2005Nov01 AEM create a PostScript file of the same name as schedule (*.ps) containing 
!                source distribution over the sky and display it on the screen with GNUplot.
!  2005Nov11 AEM manage saving ps-file with command 'UNIT saveps' (like in summary)
!  2011Feb08 JMG. Ignore case on 'saveps' test
!  2013Aug07 JMG. Modified to have more points in printing sun's position. Do 1-per day instead of 1 every 2 weeks.

! AEM 20051111 select output unit - screen or file
! to write to ps-file type 'unit saveps' then 'sou pl'
      xdisp = 1 ! screen
      ctemp=ctpfil(1:6)
      call capitalize(ctemp)
      if(ctemp.eq.'SAVEPS') xdisp = 4 ! ps file
 
! AEM 20051101 create name of new .ps file
      nch = trimlen(cskfil) - 4
      cpsfile = cskfil(1:nch)//"_sources.ps"
      nch = trimlen(cpsfile)
      
C  lutm2 is for the control file
      nch3 = trimlen(ctmfil)
      nch2 = trimlen(ctmfi2)
      open(lutm2,file=ctmfi2(1:nch2))
C
C  1. Write control file for GNUplot
C
      write(lutm2,'(a)') "set size 1.0,1.0"
      write(lutm2,'(a)') "set origin 0.0,0.0"
      write(lutm2,'(a)') "set key outside Right box 3"
      write(lutm2,'(a)') "set title  'Source positions (J2000) from"//
     ." schedule file "//cskfil(1:trimlen(cskfil))//" for experiment "//
     .cexper(1:trimlen(cexper))//"'"
      write(lutm2,'(a)') "set xlabel 'Right Ascension, hours'"
      write(lutm2,'(a)') "set ylabel 'Declination, degrees'"
      write(lutm2,'(a)') "set xtics axis nomirror"
      write(lutm2,'(a)') "set ytics axis nomirror"
      write(lutm2,'(a)') "set xrange [-.5:24.5]"
      write(lutm2,'(a)') "set yrange [-90:90]"
      write(lutm2,'(a)') "set xtics border 6"
      write(lutm2,'(a)') "set ytics border 30"
      write(lutm2,'(a)') "set mxtics 6"
      write(lutm2,'(a)') "set mytics 3"
      write(lutm2,'(a)') "set grid xtics"
      write(lutm2,'(a)') "set grid ytics"
      write(lutm2,'(a)') "set border 3 lw 0.5"
      if(xdisp.eq.4) then
        write(lutm2,'(a)') "set terminal postscript eps "//
     .   "color lw 1 'Arial' 12"
        write(lutm2,'(a)') "set output '"//cpsfile(1:nch)//"'"
      endif
!plot 'file' using ($1==3 ? $2 : 1/0):3 title 'Ecliptic' with points pointsize 0.5
      write(lutm2,'(a)') "plot '"//ctmfil(1:nch3)//"' u ($1==3 ? "//
     .      "$2 : 1/0):3 t 'Ecliptic' w p ps 0.5"
      if(xdisp.eq.4) then
        write(lutm2,'(a)') "set output '"//cpsfile(1:nch)//"'"
      endif
!replot 'file' using ($1==83 ? $2 : 1/0):3 title 'Sun today' with points pointsize 1.5
      write(lutm2,'(a)') "replot '"//ctmfil(1:nch3)//"' u "//
     .      "($1==83 ? $2 : 1/0):3 t 'Sun today' w p ps 1.5"
!replot 'file' using ($1!=3 && $1!=83 ? $2 : 1/0):3 title 'Sources'
      if(xdisp.eq.4) then
        write(lutm2,'(a)') "set output '"//cpsfile(1:nch)//"'"
      endif
      write(lutm2,'(a)') "replot '"//ctmfil(1:nch3)//"' u "//
     .      "($1!=3 && $1!=83 ? $2 : 1/0):3 t 'Sources'"
      close(lutm2)
C
C  2. Write out data file
C
      open(lutmp,file=ctmfil(1:nch3))
      do i=1,nsourc
        ic = sonum(i)
        ic = 80
        write(lutmp,'(i3,1x,2f6.1)') ic,
     .	     sorp2000(1,i)*rad2ha,sorp2000(2,i)*rad2deg
      enddo

C     Sun's position throughout the year as "*"
      iyr=iyrcur(1)
      ic = 3
      do i=1,365
        mjd=julda(1,i,iyr-1900)
        call sunpo(mjd,0.0d0,pi,ras,decs)   
        write(lutmp,'(i3,1x,2f6.1)') ic,ras*rad2ha,decs*rad2deg
      enddo
      
! Today's Sun positon is marked by "S" 
      ic = 83
      write(lutmp,'(i3,1x,2f6.1)') ic,rasun*rad2ha,decsun*rad2deg
      close(lutmp)
C
C   3.  Call plot routine
C
! AEM 20050324 add -persist for gnuplot
      ctmpcommand="gnuplot -persist "//ctmfi2(1:nch2)
      call null_term(ctmpcommand)
!      call system(ctmpcommand)
      ierr = system(ctmpcommand)
      if(ierr.ne.0) then
        write(*,*) "SOPLT: Error executing gnuplot file(s): ",ierr
        write(*,*) ctmpcommand
      endif

      open(lutmp,file=ctmfil,status='unknown',iostat=ierr)
      close(lutmp,status='delete')
      open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
      close(lutm2,status='delete')

      return
      end
