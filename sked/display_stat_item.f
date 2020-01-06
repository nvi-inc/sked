!****************************************************************************************
      subroutine display_stat_item(cname,iptr)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/valid_hardware.ftni'
      character*(*) cname
      integer iptr

      if(iptr .eq. 0) then
        cname="Station  Rack                 Recorder     Bnds"
      else
        cname=  cat_ant_name(icat_stat_vec(1,iptr))//" "//
     >          crack_type(icat_stat_vec(2,iptr))//" "//
     >          crec_type(icat_stat_vec(3,iptr))//" "//
     >          cat_equip_band(icat_stat_vec(4,iptr))
      endif
      return
      end

