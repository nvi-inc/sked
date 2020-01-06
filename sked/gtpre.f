      subroutine GTPRE(nspre,cwrap_pre,icod_pre)

C GTPRE saves information from the previous scan. Call
C       it before GTOBS, then call GTRUN after to calculate
C       running time for continuous or adaptive tape motion.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'

C History
C 970401 nrv New.
C 970403 nrv Add iftend to call.
C 970406 nrv Add itupr to call.
! 2015Mar18 Trimmed by removing tape stuff. 

C Called by: SUMCM, CHCMD, LICMD

C Input:

C Output:
      integer nspre(max_stn)
      integer icod_pre(max_stn)
      character*2 cwrap_pre(max_stn)

C Local:
      integer i,j
    

C     Save the ending time of this scan 
      do i=1,nstncur
        j=istcur(i)
        call addsec2ut(mjdcur(j),utcur(j),idurcur(j),
     >    mjdstart(j),utstart(j))
        nspre(j)=nsorcur(j)
        cwrap_pre(j)=cwrap_cur(j)     
        icod_pre(j)=icodcur(j)       
      enddo

      return
      end
