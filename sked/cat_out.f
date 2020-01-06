      subroutine cat_out(luout,lkind)
! Write out the catalogs in this schedule.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_name_version.ftni'

! 2013Sep18  JMG. Was not putting out Position.cat info
! 2014Jan16  JMG. Some of the 'luout' were 'lutmp'. This resulted in writing to a non-open file. 

       
! Passed
      integer luout
      character*1 lkind   
  
! function 
      integer ptr_ch
      character*3 lyesno
! local
      integer i
    
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$CATALOGS_USED" 
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      else if(lkind .eq. "d") then
        cbuf="Catalog   Version     Full Path Name"
        call wrt_param_line(cbuf,luout,lkind) 
      endif 
     
! Source catalogs
      write(cbuf,'(a,1x,a,5x,a)') "SOURCE   ", 
     >    trim(lsource_cat_version), trim(lsource_cat_use)
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "FLUX     ", 
     >   trim(lflux_cat_version),  trim(lflux_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
   
! Station catalogs
!      write(cbuf,'(a,1x,a,5x,a)') "STATION  ", 
!     >   trim(lstation_cat_version),  trim(lstation_cat_use) 
!      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "ANTENNA  ", 
     >   trim(lantenna_cat_version),  trim(lantenna_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 

      write(cbuf,'(a,1x,a,5x,a)') "POSITION ", 
     >   trim(lposition_cat_version),  trim(lposition_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 


      write(cbuf,'(a,1x,a,5x,a)') "EQUIP    ", 
     >   trim(lequip_cat_version),  trim(lequip_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "MASK     ", 
     >   trim(lmask_cat_version),  trim(lmask_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 

! Mode catalogs
      write(cbuf,'(a,1x,a,5x,a)') "MODES    ", 
     >   trim(lmodes_cat_version),  trim(lmodes_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "FREQ     ", 
     >   trim(lfreq_cat_version),  trim(lfreq_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 

      write(cbuf,'(a,1x,a,5x,a)') "REC      ", 
     >   trim(lrec_cat_version),  trim(lrec_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "RX       ", 
     >   trim(lrx_cat_version),  trim(lrx_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "LOIF     ", 
     >   trim(lloif_cat_version),  trim(lloif_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "TRACKS   ", 
     >   trim(ltracks_cat_version),  trim(ltracks_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 
      write(cbuf,'(a,1x,a,5x,a)') "HDPOS    ", 
     >   trim(lhdpos_cat_version),  trim(lhdpos_cat_use) 
      call wrt_param_line(cbuf,luout,lkind) 

      return
      end


