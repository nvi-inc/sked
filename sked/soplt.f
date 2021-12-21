C@SOPLT
C
      subroutine soplt
C
C     SOPLT plots source ra/dec
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
C
C  INPUT: none
C  OUTPUT: none
C  Subroutines: Calls sonum
C               Called by socmd
C
C  LOCAL
      real*8 ras,decs
      integer sonum
      integer mjd,i,iyr
      integer julda !function
      integer ierr,ic,nch,nch2,trimlen
      integer*4 xdisp
C
C  HISTORY
C  890628 NRV Created
C  890725 PMR use_mask set to 0
C  930430 nrv Write control and data files for pc8
C 981113 nrv Modify JULDA call to send year since 1900.
C
C
C  lutmp, ctmfil is for the data file
      open(lutmp,file=ctmfil,status='unknown',iostat=ierr)
      nch = trimlen(ctmfil)
C  lutm2, ctmfi2 is for the control file
      open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
      nch2 = trimlen(ctmfi2)
C
C  1. Write control file
C
      write(lutm2,9101) ctmfil(1:nch)
9101  format('begin'/
     .       'view .1 .9 .1 .9'/
     .       'diffsyms 1'/
     .       'x_field 1 2 0 Right Ascension (hours)'/
     .       'y_field 1 3 0 Declination (degrees)'/
     .       'label .15 .97 1 0 Source and Sun (*, @=today)',
     .             ' Positions (J2000)'/
     .       'axes ABCNMT BCNMT'/
     .       'x_mjtiv 3'/
     .       'y_mjtiv 30'/
     .       'window 0.0 24.0 -90.0 90.0'/
     .       'file ',a/
     .       'read'/'draw'/'end')
      close(lutm2)
C
C  2. Write out data file
C
      do i=1,nsourc
        ic = sonum(i)
        write(lutmp,9100) ic,sorp2000(1,i)*rad2ha,sorp2000(2,i)*rad2deg
9100    format(1x,i3,1x,2f6.1)
      enddo
C
C     Sun's position throughout the year as "*"
      iyr=iyrcur(1)
      do i=1,26
        mjd=julda(1,i*14,iyr-1900)
        call sunpo(mjd,0.0d0,pi,ras,decs)
        ic = 3
        write(lutmp,9100) ic,ras*rad2ha,decs*rad2deg
      enddo
C
C     Today's sun position is marked with "S"
      ic = 64
      write(lutmp,9100) ic,rasun*rad2ha,decsun*rad2deg
      close(lutmp)
C
C   3.  Call plot routine
C
      xdisp = 1
      if (ludsp.eq.lufil) xdisp = 3
!      call pc8(xdisp,ctmfi2,nch2)
! AEM 20041215 change pc8 call, remove nch2
      call pc8(xdisp,ctmfi2)
      open(lutmp,file=ctmfil,status='unknown',iostat=ierr)
      close(lutmp,status='delete')
      open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
      close(lutm2,status='delete')

      return
      end
