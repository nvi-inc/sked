      SUBROUTINE SETSC(IERR)
C
C    SETSC creates the scratch files.
C
      include '../skdrincl/skparm.ftni'
C
C  COMMON BLOCKS
      include 'skcom.ftni'
C
C OUTPUT:
      integer ierr

C LOCAL VARIABLES
C
C     CALLING SUBROUTINES: SKED
C     CALLED SUBROUTINES: IRP
C
C     LAST MODIFIED:   810105
C     830423  NRV  ADDED 5TH PARAMETER TO IRP CALLS
C     841018  MWH  USE TYPE 2 FILE FOR WORK FILE
C     880311  NRV  DE-COMPC'D
C     880330  NRV  CHANGED SCRATCH FILE NAMES FOR MILTI-COPY
C     880524  PMR  rewrote subroutine for workstation
C     890120  GAG  cleaned up
C     890531  NRV  Added CSKFIL to files that get the PID
C     891114  NRV  Changed name to SETSC from SEGRP
C     891201  gag  Moved getting pid to sked.f
C     900131  gag  added CHDFIL
C     900216  gag  removed file concantenation to scctl and
C                  changed all iscun's to lutmp
C     900302  gag  cleaned up comments
C
C
C   1. Create the scratch files whose variable names were
C      made in scctl.

      OPEN (lutmp,status='UNKNOWN',file=CTMFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CTMFI2)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CSOFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CSTFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CFRFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=COPFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CFLFIL)
      CLOSE (lutmp,status='KEEP')
      OPEN (lutmp,status='UNKNOWN',file=CHDFIL)
      CLOSE (lutmp,status='KEEP')

      IERR = 0

      RETURN
      END
