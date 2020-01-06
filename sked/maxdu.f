C@MAXDU
C
      integer FUNCTION MAXDU(idurst,nstn,istn)
C
C     MAXDU determines the maximum duration in an array of durations
C
      include '../skdrincl/skparm.ftni'
C
C INPUT
      integer idurst(max_stn),istn(max_stn),nstn
C    - durations by station
C    - list of station IDs
C    nstn - number of stations
C
C OUTPUT
C   maxdu - largest of the values in idurst
C
      include '../skdrincl/statn.ftni'
C
C LOCAL
      integer idmax,i,j
C
C   1. Loop over stations and save the largest value
C
      idmax=0
      do i=1,nstn
        j=istn(i)
        if (idurst(j).gt.idmax) idmax=idurst(j)
      enddo
C
C   2. Return value
C
      maxdu = idmax
C
      return
      end

