      SUBROUTINE cat_cmd(cmdline)
! Various catalog info.
! 2012Oct09  JMGipson
! 2012Oct11 JMGipson. Removed "STATION .." command because sked does not use station.cat
! 
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_name_version.ftni'

C Input
      character*(*) cmdline

! functions
      integer iStringMinMatch
      integer trimlen
! Stuff dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=4)
      character*128 ltoken(MaxToken)
  
      integer icmd
      character*20 lcmd
      logical kvalue
! input
! valid command list.
      integer i   
      integer icmd_list_len    
      parameter (icmd_list_len=15)
      character*20 lcmd_list(icmd_list_len)
      character*20 lcmd_caps(icmd_list_len)
      character*65 lhelp(icmd_list_len)

      data (lcmd_list(i), lhelp(i), i=1,icmd_list_len)/
     >" ? ",                
     >"            List 'catalog' commands and options",   !1
     >"List", 
     >"            List all catalogs                  ",   !2
     > "Source",
     > "  Set catalog to use OR Version Catalog_used  ",   !3
     > "Flux",
     > "  Set catalog to use OR Version Catalog_used  ",   !4
     > "Position",
     > "  Set catalog to use OR Version Catalog_used  ",   !5
     > "Antenna",
     > "  Set catalog to use OR Version Catalog_used  ",   !6
     > "Equip",
     > "  Set catalog to use OR Version Catalog_used  ",   !7
     > "Mask",
     > "  Set catalog to use OR Version Catalog_used  ",   !8
     > "Modes",
     > "  Set catalog to use OR Version Catalog_used  ",   !9
     > "Freq",
     > "  Set catalog to use OR Version Catalog_used  ",   !10
     > "Rec",
     > "  Set catalog to use OR Version Catalog_used  ",   !11
     > "Rx",
     > "  Set catalog to use OR Version Catalog_used  ",   !12 
     > "LOIF",
     > "  Set catalog to use OR Version Catalog_used  ",   !13
     > "Tracks",
     > "  Set catalog to use OR Version Catalog_used  ",   !14
     > "HDPOS",
     > "  Set catalog to use OR Version Catalog_used  "/   !15

  
! Start of the subroutine
      do i=1,icmd_list_len
         lcmd_caps(i)=lcmd_list(i)
         call capitalize(lcmd_caps(i))  
      end do 
  
! Parse it.    
      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      call capitalize(ltoken(1)) 
      itoken=1
      if(NumToken.eq.0 .or. Numtoken.eq.1 .and. ltoken(1).eq."?") then
        do icmd=1,icmd_list_len
          write(luscn,'(a,1x,a)') lcmd_list(icmd),lhelp(icmd)
        end do
        return
      else if(NumToken .gt. 3) then
         write(luscn,'("cat_cmd: Only takes 3 or fewer arguments")') 
         write(luscn,'("   can not parse line: ", a)') trim(cmdline)
         return
      endif

      icmd=istringMinMatch(lcmd_caps,icmd_list_len,ltoken(1))
      if(icmd .eq. 0) then
        write(*,*) "cat_cmd: Unknown command: ", trim(ltoken(1))
        write(*,'("    Valid commands are: ",10(a,1x))') 
     >             (trim(lcmd_list(i)),i=1,icmd_list_len)
      endif 
      lcmd=lcmd_caps(icmd)

      if(lcmd .eq. "LIST") then   
         call cat_out(ludsp,'d')
         return
      endif

! the remaining commands are of two forms. If a single argument, then set the catalog to use.
! If two arguments, set the version number and the catalog name used. (Typically done when reading in a sked file.) 
    
      select case(lcmd)

      case("SOURCE") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           source_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lsource_cat_version=ltoken(2)
           lsource_cat_use    =ltoken(3)
        endif 

      case("POSITION") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           flux_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lposition_cat_version=ltoken(2)
           lposition_cat_use    =ltoken(3)
        endif 
      case("FLUX") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           flux_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lflux_cat_version=ltoken(2)
           lflux_cat_use    =ltoken(3)
        endif 
      case("STATION") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           station_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lstation_cat_version=ltoken(2)
           lstation_cat_use    =ltoken(3)
        endif 
      case("ANTENNA") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           antenna_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lantenna_cat_version=ltoken(2)
           lantenna_cat_use    =ltoken(3)
        endif 

      case("EQUIP") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           equip_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lequip_cat_version=ltoken(2)
           lequip_cat_use    =ltoken(3)
        endif 
      case("MASK") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           mask_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lmask_cat_version=ltoken(2)
           lmask_cat_use    =ltoken(3)
        endif 
      case("MODES") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           modes_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lmodes_cat_version=ltoken(2)
           lmodes_cat_use    =ltoken(3)
        endif 
      case("FREQ") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           freq_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lfreq_cat_version=ltoken(2)
           lfreq_cat_use    =ltoken(3)
        endif 
      case("REC") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           rec_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lrec_cat_version=ltoken(2)
           lrec_cat_use    =ltoken(3)
        endif
      case("RX") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           rx_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lrx_cat_version=ltoken(2)
           lrx_cat_use    =ltoken(3)
        endif 
      case("LOIF") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           loif_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lloif_cat_version=ltoken(2)
           lloif_cat_use    =ltoken(3)
        endif 

      case("TRACKS") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           tracks_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           ltracks_cat_version=ltoken(2)
           ltracks_cat_use    =ltoken(3)
        endif
       case("HDPOS") 
        if(NumToken .eq. 2)  then  !Two tokens. set source catalog.
           hdpos_cat=ltoken(2)
        else if(NumToken .eq. 3) then  
           lhdpos_cat_version=ltoken(2)
           lhdpos_cat_use    =ltoken(3)
        endif

      case default

      end select

      
      return
      end 





  



