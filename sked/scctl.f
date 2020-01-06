      SUBROUTINE scctl
C
C   This routine will set up the path names for the 
C   scratch files. The path is read from the control
C   file, if one exists, or the default is the current
C   directory.
C
C   HISTORY:
C
C     WHO   WHEN   WHAT
C     gag   900215 created
C 991108 nrv Add skcat control files.
C 000326 nrv Add param control files.
C
C   parameter file
      include '../skdrincl/skparm.ftni'
C
C   SUBROUTINES
C     CALLED BY: sked
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
C
C  LOCAL VARIABLES
      integer trimlen
      integer nch
C
C   Create scratch file names.
C 
      nch = trimlen(ctmpnam)
      clgfil = ctmpnam(:nch)//'SKlog'//cpid
      csofil = ctmpnam(:nch)//'SKsrc'//cpid
      cstfil = ctmpnam(:nch)//'SKstat'//cpid
      cfrfil = ctmpnam(:nch)//'SKfreq'//cpid
      copfil = ctmpnam(:nch)//'SKops'//cpid
      cflfil = ctmpnam(:nch)//'SKflux'//cpid
      chdfil = ctmpnam(:nch)//'SKhead'//cpid
      ctmfil = ctmpnam(:nch)//'SKTmp'//cpid
      ctmfi2 = ctmpnam(:nch)//'SKtmp2'//cpid
      cskfil = ctmpnam(:nch)//'SKsked'//cpid
      cprfil = ctmpnam(:nch)//'SKprint'//cpid
      cplfil = ctmpnam(:nch)//'SKplot'
      csktmp = cskfil
      cskselect_file = ctmpnam(:nch)//'SKselect'//cpid
      cskcontrol_file = ctmpnam(:nch)//'SKctl'//cpid
      cskcat_file = ctmpnam(:nch)//'SKcat'//cpid
      cparam_file = ctmpnam(:nch)//'SKparam'//cpid
      ccat_pid_file = ctmpnam(:nch)//'SKpid'//cpid
      cpar_pid_file = ctmpnam(:nch)//'SKpar'//cpid
C
      RETURN
      END
