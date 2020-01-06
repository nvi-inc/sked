      SUBROUTINE isup(NSOR,ISTN,UT,KUP,nrs)
C
C   isup determines whether source NSOR is up at station ISTN
C   at time UT by checking the rise/set arrays in common.  
C   The result is KUP, true or false, and which rise/set pair.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  INPUT VARIABLES:
      integer nsor,istn
C        NSOR   - Source index number into DB arrays
C        ISTN   - Station index number into DB arrays
      double precision UT ! UT for which position is requested
C  OUTPUT VARIABLES:
      LOGICAL KUP ! TRUE if source is above limits at MJD,UT
      integer nrs
C
      include 'skcom.ftni'
C
C     LOCAL VARIABLES:
      integer i
      double precision gstris,gstset,gst

C  HISTORY:
C  931021 nrv Created
C  931109 nrv Changed to real*8 arrays for actual UT. Add nrs to call.
C  931110 nrv Rise/set arrays are now in GST. Convert back to UT for
C             each call.
C  931112 nrv Convert input UT to GST and use that to check arrays.
C             Remember in which rise/set pair the change was found.


C  1. Initialize.

      kup = .false.
      gst = ut*frac + st0cur(istn)
      if (gst.gt.TWOPI) gst=gst-TWOPI

C  2. Check the rise/set times stored in common to see if the current
C     time is an up or down time.

      do i=1,ntsrisset(nsor,istn)
        gstris = tsris(nsor,istn,i)
        gstset = tsset(nsor,istn,i)
        if (dabs(gstris-gstset).lt.(3.0d-6)) then ! never up
          nrs = i
          kup = .false.
        else if (gstris.lt.gstset) then ! case [   |-----|    ]
          if (gst.gt.gstris.and.gst.lt.gstset) then
            nrs = i
            kup = .true.
          endif
        else if (gstris.gt.gstset) then ! case [---|     |----]
          if (gst.gt.gstris.or.gst.lt.gstset) then 
            nrs = i
            kup = .true.
          endif
        endif
      enddo

      return
      end

