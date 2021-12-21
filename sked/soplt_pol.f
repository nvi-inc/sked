      subroutine soplt_pol(kplot_stat,istn,nstn)
C
C     SOPLT plots source ra/dec in polar coordinates.
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
       
! passed
      logical kplot_stat        !if true, plot stations
      integer istn(*)           !index into stations.
      integer nstn              !# of stations
      real*8  rlat0,rlon0       !origen of projection
C
C  INPUT: none
C  OUTPUT: none
C  Subroutines: Calls sonum
C               Called by socmd
C
C  LOCAL
      integer mjd
      double precision ut
      real*8 ST0
      double precision el_min
      double precision rhadec(2,360)   !space for rha and dec

      real*8 rlat,rlon
      real*8 ras,decs
      real*8 rha, rdec
      real*8 rha_del     
      real*8 xd,yd
      integer idel_az    
      integer sonum
      integer i,j,k
      integer iyr
      integer julda !function
      integer ierr,ic,nch,nch2,trimlen
      integer*4 xdisp
C
C  HISTORY
!  2006Oct26 JMGipson. Based loosely on soplt.
! 2020Oct03 JMGipson.  Replaced Real*8 del_deg-->idel_az 

C  lutmp, ctmfil is for the data file
      open(lutmp,file=ctmfil,status='unknown',iostat=ierr)
      nch = trimlen(ctmfil)
C  lutm2, ctmfi2 is for the control file
      open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
      nch2 = trimlen(ctmfi2)
C
C  1. Write control file
C
      write(lutm2,9610) ctmfil(1:nch)
9610  format('begin'/
     .       'view .1 .9 .1 .9'/
     >       'diffsyms 1'/
     >       'x_field 1 2 0'/
     >       'y_field 1 3 0'/
     .       'label .15 .97 1 0 Source and Sun=@ ',
     .       ' Positions (J2000)   Equal Area Projection'/
     >       'axes ABCT ABCT'/
     >       'window -100 100 -100 100'/
     >       'x_mjtiv 10'/
     >       'y_mjtiv 10'/
     >       'file ',a/
     >       'read'/'draw'/'end')
      close(lutm2)

! get the current time. (This is for 1st station, but probably good enough)
      rlon0=0.
      rlat0=piov2
      call init_equal_area_proj(rlon0,rlat0)

      if(kplot_stat) then
        mjd=mjdcur(istn(1))
        ut=utcur(istn(1))
        CALL SIDTM(MJD,ST0,FRAC)
        rha_del = mod(ST0+UT*FRAC,twopi)
      else
        rha_del=0.
      endif

      do i=1,nsourc
        ic = sonum(i)
        rha=sorp_now(1,i)-rha_del
        rdec=sorp_now(2,i)
        call equal_area_proj(rha,rdec,xd,yd)
        write(lutmp,"(i3,2f9.2)") sonum(i),xd,yd
      enddo

      if(.not.kplot_stat) then
C     Sun's position throughout the year as "*"
        iyr=iyrcur(1)
        do i=0,365,5
          mjd=julda(1,i,iyr-1900)
          call sunpo(mjd,0.0d0,pi,ras,decs)
          rha=-ras
          rdec=decs
          call equal_area_proj(rha,rdec,xd,yd)
          ic=3
          write(lutmp,"(i3,2f9.2)") ic,xd,yd
        enddo
      endif

C     Today's sun position is marked with "S"
      ic = 64
      rha=rasun-rha_del
      rdec=decsun
      call equal_area_proj(rha,rdec,xd,yd)
      write(lutmp,"(i3,2f9.2)") 64,xd,yd

! Plot circle at declination 0=equator.
      rdec=0.
      do i=0,365,2
        rha=i
        call equal_area_proj(rha,rdec,xd,yd)
        write(lutmp,"(i3,2f9.2)") 46,xd,yd
      end do
! Plot circle at dec=-90
      if(rlat0 .ne. -piov2) then
        rdec=-piov2+0.0001
        do i=0,365,2
          rha=i
          call equal_area_proj(rha,rdec,xd,yd)
          write(lutmp,"(i3,2f9.2)") 46,xd,yd
        end do
      endif
! Plot circle at dec=90
      if(rlat .ne. piov2) then
        rdec=piov2-0.0001
        do i=0,365,2
          rha=i
          call equal_area_proj(rha,rdec,xd,yd)
          write(lutmp,"(i3,2f9.2)") 46,xd,yd
        end do
      endif

! Now plot the circles for the stations.

      el_min=5.0
      idel_az=2 
      do i=1,nstn !stations
        j=istn(i)
        rlat=stnpos(2,j)
        rlon=-stnpos(1,j)
        call find_stat_vis(rlon,rlat,el_min,idel_az,rhadec)
        do k=1,360/idel_az 
!          rha=rhadec(1,k)
!          rdec=rhadec(2,k)
          call equal_area_proj(rhadec(1,k),rhadec(2,k),xd,yd)
          write(lutmp,"(i3,2f9.2)") 3,xd,yd
        end do
      end do
      close(lutmp)
C
C   3.  Call plot routine
C
      xdisp = 1
      if (ludsp.eq.lufil) xdisp = 3
      write(*,*) "file:"//ctmfi2
      stop
      call pc8(xdisp,ctmfi2)
      if(.false.) then
      open(lutmp,file=ctmfil,status='unknown',iostat=ierr)
      close(lutmp,status='delete')
      open(lutm2,file=ctmfi2,status='unknown',iostat=ierr)
      close(lutm2,status='delete')
      endif

      return
      end

