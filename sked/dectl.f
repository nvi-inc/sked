      SUBROUTINE dectl
C
C  All the catalogs, scratch files, printer,
C  and other control file information are set to
C  the default settings here.
C
C
C   HISTORY:
C
C     WHO   WHEN    WHAT
C     gag   900302  created
C     NRV   901018  Added cprttyp, cprport
C     nrv   950329  Add flux comments file name.
C 951124 nrv Add modes catalog file name, remove head, sequence
C            becomes freq, remove vlba
C 960403 nrv Add rec.cat
C 970328 nrv Add station.cat
C 991109 nrv Add modes_description.cat
! Gave better names.
C
C   parameter file 
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
C  OUTPUT:
C
C
C   SUBROUTINES
C     CALLED BY: SKED
C     CALLED:
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
C
C  LOCAL VARIABLES
C
      source_cat = 'source.cat'
      station_cat = 'station.cat'
      antenna_cat='antenna.cat'
      position_cat='position.cat'
      equip_cat='equip.cat'
      mask_cat='mask.cat'
      freq_cat='freq.cat'
      rx_cat='rx.cat'
      loif_cat='loif.cat'
      modes_cat='modes.cat'
      modes_description_cat='modes_description.cat'
      rec_cat='rec.cat'
      hdpos_cat='hdpos.cat'
      tracks_cat='tracks.cat'
      flux_cat='flux.cat'
      flux_comments='flux.cat.comments'

      clgfil = 'SKlog'//cpid
      csofil = 'SKsrc'//cpid
      cstfil = 'SKstat'//cpid
      cfrfil = 'SKfreq'//cpid
      copfil = 'SKop'//cpid
      cflfil = 'SKflux'//cpid
      chdfil = 'SKhd'//cpid
      ctmfil = 'SKtmp'//cpid
      ctmfi2 = 'SKtmp2'//cpid
      cskfil = 'SKsked'//cpid
      cprfil = 'SKprint'//cpid
      cplfil = 'SKplot'
      csked = './'
      ctmpnam = './'

      csktmp = cskfil
      cprtpor='ljp'
      call null_term(cprtpor)
      cprtlan='lj'
      call null_term(cprtlan)
      cprttyp=''
      call null_term(cprttyp)
      cprport=''
      call null_term(cprport)

      RETURN
      END
