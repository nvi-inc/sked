      subroutine save_station_state
! Save the station state. This is done before selecting new stations.
! Used to preserve information about the station.
      use max_stat_scan 
      implicit none 

C  COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
     
      include 'downtime.ftni'
      include 'station_state.ftni'
      include 'covar.ftni'
      include 'major.ftni'

      integer numbl
      integer i
! History
!  2021-10-20 JMG. Now include MAX_STAT_SCAN info
!  2020-06-08.  Include broadband.ftni.  restore ibb_off
!  2008-08-06 JMGipson.  Minor change in output statement.
!  2005-07-06  JMGipson.  Better save/restore of subnet.
  

      nstatn_save=nstatn
      nsubst_save=nsubst

! Save subnet information.
!     isubst_save(1:nsubst)=isubst(1:nsubst)
      do i=1,nsubst
        cposub_save(i)=cpocod(isubst(i))
      end do 

! Save downtime info.
      num_down_save=num_down

      do i=1,num_down
        cpo_down_save(i)=cpocod(idown_stat(i))
        mjd_down_beg_save(i)=mjd_down_beg(i)
        ut_down_beg_save(i)=ut_down_beg(i)
        mjd_down_end_save(i)=mjd_down_end(i)
        ut_down_end_save(i)=ut_down_end(i)
      end do

      numbl=nstatn*(nstatn-1)/2

! save the info.
      cstnna_save(1:nstatn)           =cstnna(1:nstatn)
      stnelv_save(1:nstatn)           =stnelv(1:nstatn)
      tape_motion_type_save(1:nstatn) =tape_motion_type(1:nstatn)
      tape_allocation_save(1:nstatn)  =tape_allocation(1:nstatn)
      itearl_save(1:nstatn)           =itearl(1:nstatn)
      tape_allocation_save(1:nstatn)  =tape_allocation(1:nstatn)  
      cstcod_save(1:nstatn)           =cstcod(1:nstatn)

! And the broadband info.
      idata_mbps_save(1:nstatn)=idata_mbps(1:nstatn)
      isink_mbps_save(1:nstatn)=isink_mbps(1:nstatn)
      ibb_off_save(1:nstatn)=ibb_off(1:nstatn)
      bb_bw_save(1:nstatn)=bb_bw(1:nstatn)      
      
! Max_stat_scan info       
      max_ss_list_save(1:nstatn)=max_ss_list(1:nstatn)

! SNR info
      isnrbl_save(1:max_band,1:numbl) =isnrbl(1:max_band,1:numbl)
      isnrbl_1_save(1:max_band,1:numbl) =isnrbl_1(1:max_band,1:numbl)

! optimization info.
      lpara_save=lpara

      return
      end

! ************************************************************
      subroutine restore_station_state
      use max_stat_scan 
      
! Restore the station state. This is done after selecting new stations.
! Stations that we had before get values that they had previously.
! History
! 2013May06. JMGipson. Try to restore 1-letter codes ONLY if we had observations previously.
! 2013May27. JMGipson. Removed fixing 1-letter code. Now handled in wrsts.f 
! 2020Jun08.  Include broadband.ftni.  restore ibb_off 
      implicit none

C  COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/broadband.ftni'
      include '../skdrincl/freqs.ftni'
      include 'downtime.ftni'
      include 'station_state.ftni'
      include 'covar.ftni'
      include 'major.ftni'
      include '../skdrincl/skobs.ftni'

! functions
      integer iwhere_in_string_list
      integer trimlen
      integer ibnum

! local variables
      integer iwhere
      integer i,j               !indices for new stations
      integer i0,j0             !for old
      integer iband             !index
      integer ibl,ibl0          !which baseline (new, old)
      integer numbl             !number of baselines
      integer ki,ki0            !pointer into lpara  (estimation parameters)
      integer itype             !lpara. Type=estimate,optimize
      integer ixref(max_stn)
      logical knew(max_stn)     !set to true if
      logical knew_station      !true if one new station
      integer il
      logical kfirst
      logical kfirst_snr(max_band)
      logical ksame_snrbl(max_band),ksame_snrbl_1(max_band)
      integer ifirst

      logical ksame_tape_motion_type,ksame_tape_allocation
      logical ksame_itearl, ksame_stnelv
    
  
! set defaults.
      lpara=.false.

      kfirst=.true.
! restore station dependent info.
      knew_station=.false.

      ksame_tape_motion_type=.true.
      ksame_tape_allocation =.true.
      ksame_itearl=.true.
      ksame_stnelv=.true.

!      stnelv=3.
! AEM 20050305 init stnelv with old value of 0.08726 rad (=5deg)
! CHanged to more symbolic. JMG
!      stnelv=0.08726
      stnelv=5.0*deg2rad
      tape_motion_type="START&STOP"
      tape_allocation="SCHEDULED"

! Initialize the broadband stuff so that no station is broadband. 
      bb_bw=0.0
      idata_mbps=0
      isink_mbps=0   
      ibb_off=0
      max_ss_list=0 
   
      do i=1,nstatn
        iwhere=iwhere_in_string_list(cstnna_save,nstatn_save,cstnna(i))
        ixref(i)=iwhere
        if(iwhere .eq. 0) then
          knew(i)=.true.
          knew_station=.true.
! Only execute if trying to preserve 1-letter station codes. Must do if we have observations. 
        else
          knew(i)=.false.
!          cstnna(i)           =cstnna_save(iwhere)
! IMPORTANT: Need to restore previous 1-letter code if any observations were scheduled!                                    
          stnelv(i)           =stnelv_save(iwhere)
          itearl(i)           =itearl_save(iwhere)
          tape_motion_type(i) =tape_motion_type_save(iwhere)
          tape_allocation(i)  =tape_allocation_save(iwhere)

          bb_bw(i)       =bb_bw_save(iwhere)
          idata_mbps(i)  =idata_mbps_save(iwhere)
          isink_mbps(i)  =isink_mbps_save(iwhere)
          ibb_off(i)     =ibb_off_save(iwhere)
          
          max_ss_list(i)=max_ss_list_save(iwhere)
 
          if(kfirst) then
            ifirst=i
            kfirst=.false.
          else
            if(stnelv(ifirst).ne. stnelv(i))
     >            ksame_stnelv=.false.
            if(itearl(ifirst) .ne. itearl(i))
     >           ksame_itearl=.false.
            if(tape_motion_type(ifirst). ne. tape_motion_type(i))
     >           ksame_tape_motion_type=.false.
            if(tape_allocation(ifirst) .ne. tape_allocation(i))
     >           ksame_tape_allocation=.false.
          endif
        endif
      end do

! if all of the old stations have the same characteristics, set the new ones to it.
      if(.not. kfirst) then
        if(ksame_tape_motion_type) then
           tape_motion_type(1:nstatn)=tape_motion_type(ifirst)
        endif
        if(ksame_tape_allocation) then
           tape_allocation(1:nstatn)=tape_allocation(ifirst)
        endif
        if(ksame_itearl) then
           itearl(1:nstatn)=itearl(ifirst)
        endif
        if(ksame_stnelv) then
           stnelv(1:nstatn)=stnelv(ifirst)
        endif
      endif

! default values for snr is 0.
      numbl=nstatn*(nstatn-1)/2

      isnrbl(1:max_band,1:numbl)=0
      isnrbl_1(1:max_band,1:numbl)=-1

! now restore SNR info.
      kfirst_snr=.true.
      ksame_snrbl=.true.     !used to see if all snr targets are the same
      ksame_snrbl_1=.true.   !ditto
      do i=1,nstatn-1
        i0=ixref(i)
        if(i0 .ne. 0) then
          do j=i+1,nstatn
            j0=ixref(j)
            if(j0 .ne. 0) then
              do iband=1,nband
                ibl0=ibnum(i0,j0)
                ibl=ibnum(i,j)
                isnrbl(iband,ibl) = isnrbl_save(iband,ibl0)
                isnrbl_1(iband,ibl) = isnrbl_1_save(iband,ibl0)
                if(kfirst_snr(iband)) then
                  kfirst_snr(iband)=.false.
                  ifirst=ibl
                else
                  if(isnrbl(iband,ifirst).ne.isnrbl(iband,ibl)) then
                     ksame_snrbl(iband)=.false.
                  endif
                  if(isnrbl_1(iband,ifirst).ne.isnrbl_1(iband,ibl)) then
                     ksame_snrbl_1(iband)=.false.
                  endif
                endif
              end do
            endif
          end do
        endif
      end do

! If all the old SNRS in a band are the same, assume all the news should be.
      do iband=1,nband
! Make sure at least one of the stations is old.
        if(.not.kfirst_snr(iband)) then
          if(ksame_snrbl(iband)) then
            isnrbl(iband,1:numbl)=isnrbl(iband,ifirst)
          end if
          if(ksame_snrbl_1(iband)) then
            isnrbl_1(iband,1:numbl)=isnrbl_1(iband,ifirst)
          end if
        endif
      end do

! now set up up optimization stuff.

      do i=1,nstatn ! i=index for new stations
        i0=ixref(i)
        if(i0 .ne. 0) then
          do itype=1,2
             ki=5+2*i-1
             ki0=5+2*i0-1
             lpara(ki,itype)   = lpara_save(ki0,itype)   !atm0
             lpara(ki+1,itype) = lpara_save(ki0+1,itype) !atm1
             ki=5+2*nstatn+(3*i)-2
             ki0=5+2*nstatn_save+(3*i0)-2
             lpara(ki,itype) = lpara_save(ki0,itype)     !clock0
             lpara(ki+1,itype) = lpara_save(ki0+1,itype) !clock1
             lpara(ki+2,itype) = lpara_save(ki0+2,itype) !clock2
             ki=5+5*nstatn+(3*i)-2
             ki0=5+5*nstatn_save+(3*i0)-2
             lpara(ki,itype) = lpara_save(ki0,itype)     !x
             lpara(ki+1,itype) = lpara_save(ki0+1,itype) !y
             lpara(ki+2,itype) = lpara_save(ki0+2,itype) !z
          enddo
        end if  !
      END DO  ! 

! Restore subnet info.
! update the subnet array, isubst, removing stations which are no longer here.
      nsubst=0
      do i=1,nsubst_save
! check to see if a station name in the previous subnet is in the current list of
! stations. If so, save it.
        iwhere=iwhere_in_string_list(cpocod,nstatn,cposub_save(i))
        if(iwhere .ne.0) then
           nsubst=nsubst+1
           isubst(nsubst)=iwhere
        endif
      end do

! If no stations in the subnet, turn them all on.
! This might happen if all the stations are new, for example.
      if(nsubst .eq. 0) then
        nsubst=nstatn
        do i=1,nsubst
          isubst(i)=i
        end do
      endif

! Restore downtime info.
      num_down_save=num_down

      num_down=0
      do i=1,num_down_save
        iwhere=iwhere_in_string_list(cpocod,nstatn,cpo_down_save(i))
        if(iwhere .ne.0) then
          num_down=num_down+1
          idown_stat(num_down)=iwhere
          mjd_down_beg(num_down) =mjd_down_beg_save(i)
          ut_down_beg(num_down)  =ut_down_beg_save(i)
          mjd_down_end(num_down) =mjd_down_end_save(i)
          ut_down_end(num_down)  =ut_down_end_save(i)
        endif
      end do      


      if(knew_station) then
        il=0
        do i=1,nstatn
          il=max(il,trimlen(tape_motion_type(i)))
        end do

        write(luscn,'(a)')
     >    "Opt est parameters initialized to off."

        write(luscn,'(a)')
     >    "Following stations are new:"

        write(luscn,'(a)') "Name      EL  Early   Tape"
        do i=1,nstatn
          if(knew(i)) then
            write(luscn,'(a8,1x,f4.1,1x,i4,1x,a)')
     >         cstnna(i),stnelv(i)*180.0/pi,itearl(i),
     >         tape_motion_type(i)(1:il)
          endif
        end do
! Update subnet information.
        do i=1,nstatn
          if(knew(i)) then
             nsubst=nsubst+1
             isubst(nsubst)=i
          endif
        end do     
! ***** End of Fixup*****************
      endif


      return
      end
