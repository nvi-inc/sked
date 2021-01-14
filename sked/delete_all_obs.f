      subroutine delete_all_obs()
! get ready to delete all observations.
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'

! History
! 2008Jun17  JMGipson.  Initialize Az and El to not found
! 2009Oct03  JMG.  Initialize Nsorobs to 0.
! 2014May02  JMG. Initialze ipascur, iftcur, idircur

      call init_time_arrays(iyr_start,ida_start,ihr_start,
     >   imin_start,isc_start)
      nxtrec=1
      ircur=0
      nobs=0
      nsorobs=0  !Added 2009Oct03 
      nsorcur=0  !no previous sources
      idurcur=0
      nsortst=0
      eleva=-99.d0  !this is el of stations.
      azimu=-99.d0
!
      iftcur=0

      kobc=.false.

      nobsso=0
      mjprso=0

      knewsk=.true.
      return
      end
