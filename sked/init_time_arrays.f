      subroutine init_time_arrays(iyr,ida,ihr,imin,isec)
! get ready to delete all observations.
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
! History
! 2010Mar25  JMGipson. Removed setting of istcur(i)=i because this messes up current counter.       
! function
      double precision hms2seconds
      integer julda

! passed
      integer iyr,ida,ihr,imin,isec
! local
      integer i

      double precision ut_start
      integer mjd_start
      double precision st0_start
      double precision gst_start

      ut_start =hms2seconds(ihr,imin,isec)
      mjd_start=Julda(1,ida,iyr-1900)

      CALL SIDTM(MJD_start,st0_start,FRAC)
      GST_start = ST0_start + UTCUR(1)*FRAC
      IF (GST_start.GE.twoPI) GST_start = GST_start-twoPI

      iftcur=0
      do i=1,max_stn
        iyrcur(i)=iyr
        idacur(i)=ida
        utcur(i)=ut_start
        mjdcur(i)=mjd_start
        st0cur(i)=st0_start
        gstcur(i)=gst_start
!        istcur(i)=i
      end do
      return
      end

