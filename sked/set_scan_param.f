      subroutine set_scan_param(
     >  nstn,   istn,    mjd,    ut,
     >  nsor,   ical,    idle,   icod, ircur,   idurst,  lcable,
     >  cpre,cmid,cpst,keep_index,
     >  nstncur,istcur,  mjdcur, utcur,gstcur,st0cur,iyrcur, idacur,
     >  nsorcur,icalcur, idlcur, icodcur,ireccur,idurcur,lcblcur,
     >  cprecur,cmidcur,cpstcur)

! copy current scan parameters into approprate places.
      include "../skdrincl/constants.ftni"

! History

! 2014May02 JMG. Removed ipas,idir, ift from call to set_scan_param. No longer used. 


! input
      integer nstn     		!number of stations.
      integer istn(*)        	!station vector
      integer mjd               !time (day part)
      double precision ut       !secondis
      integer nsor              !source
      integer ical              !calibration
      integer idle              !idle time
      integer icod
      integer ircur
      integer idurst(*)
      integer*2 lcable(*)
    
      character*6 cpre,cmid,cpst
      logical keep_index        !If true, then keep the index.  (called from newob)
                                !Else, then map from one to another (called from unpak)

! output
      integer nstncur   	!number of stations
      integer istcur(*)         !station vector
      integer mjdcur(*)
      double precision utcur(*)
      double precision gstcur(*)
      double precision st0cur(*)
      integer iyrcur(*)
      integer idacur(*)
      integer nsorcur(*)
      integer icalcur(*)
      integer idlcur(*)
      integer icodcur(*)
      integer idurcur(*)
      integer ireccur(*)
      integer*2 lcblcur(*)  
      character*6 cprecur(*),cmidcur(*),cpstcur(*)

! local
      integer iyr,ida           !day and year part
      integer i,j
      integer jin
      double precision gst, st0, frac

      call mjd2yrDOY(mjd,iyr,ida)

      CALL SIDTM(MJD,ST0,FRAC)
      GST = DMOD(ST0 + UT*FRAC, 2.D0*PI)

      nstncur =   NSTN
      DO  I=1,NSTN !set CUR variables
        ISTCUR(I) =istn(i)
        j=istn(i)
        if(keep_index) then
           jin=j
        else
           jin=i
        endif

        MJDCUR(J) = MJD
        UTCUR(J) = UT
        IYRCUR(J) = IYR
        IDACUR(J) = IDA
        GSTCUR(J) = GST
        st0cur(j) = ST0

        NSORcur(J) = NSOR
        ICALcur(J) = ICAL
        IDLCUR(J) = IDLE
        ICODcur(J) = ICOD
        ireccur(j) = ircur
        IDURcur(J) = IDURST(jin)
        LCBLcur(J)=  LCABLE(jin)  
        cprecur(j)=cpre
        cmidcur(j)=cmid
        cpstcur(j)=cpst
      END DO  !set CUR variables
      return
      end

