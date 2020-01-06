      SUBROUTINE vtrout
C
C  This routine writes out the VEX $TRACKS section.
C
C   HISTORY:
C 990611 nrv New. 
C 990922 nrv Use VEX utilities
C 990929 nrv Add track_frame_format
C 991130 nrv Include LSB channels too.
C 000317 nrv Write out track_frame_format defs.
C 000522 nrv Change "Ch" to "CH" for consistency in ref names with
C            SCHED VEX files.
C 000523 nrv Increment channel for SB not for bits
C 000602 nrv Add data_modulation = on for VLBA format.
! 2010.06.16 Leave spaces between names
! 2010.09.02 Fixed bug with checking for rack types. Now capitalize before checking. 
!            Also added new rack types. 
! 2011.12.02 Made MARK5 a valid rack type.
! 2012Sep07  JMG.  Added Mark5_format 
! 2014Aug22  JMG. Made data_modultation=off for VLBA format and VLBA correlator. 
! 2017Feb27  JMG. Added "DBBC_DDC", "DBBC_PFB","DBBC_DDC/FILA10G", "DBBC_PFB/FILA10G" to valid rack types
    
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
! function
      integer itras
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer ic,ist(max_stn),npx,ipr(max_stn),ipx
      integer itype,il,isp,ichan,isb,is
      integer ibit,it,ipass,ib,ich,ihd
      character*36 csubpass
      logical km3,km4,kv,km3mode
      logical km5b_rec               !Mark5B  recorder 
      logical kvdif                  !Vdif recorder
      data csubpass/'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'/
      integer ptr_ch,trimlen
      character*28 cp,ctr,cbit,chd,cit,cfr

      character*8 cstrack_tmp, cstrec_tmp     !temporary variable holds rack, recorder 
      character*1 lq/"'"/
      integer ind 

C  1. TRACKS

      call fcreate_block(ptr_ch("TRACKS"//char(0)))
      write(luscn,'("TRACKS: ")')

C  2. Write each fanout_def line.

      itype=4 ! FREQ, BBC, IFD, TRACKS, HEAD_POS, PASS_ORDER
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
! Don't have to put out track commands for VDIF
          ind=index(refdef_name(itype,isp,ic),"VDIF")
          if(ind .ne. 0) cycle 
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)

C Track assignment array has these fields:
C itras(isb,ibit,ihd,chan,subpass,stn,code)

          do ipass=1,npassf(isp,ic) ! each subpass
            ichan = 0
            do ihd=1,max_headstack
              do ib=1,nchan(isp,ic)
                ich=invcx(ib,isp,ic) ! channel index number
                do isb=1,2 ! sidebands
                  do ibit=1,2
                    it=itras(isb,ibit,ihd,ich,ipass,isp,ic)
C Indenting is getting too deep, so bring it out again
            if (it.gt.-3) then ! do this def
C subpass
C             NCH = ichmv_ch(IBUF,nch,csubpass(ipass:ipass))
              cp = csubpass(ipass:ipass)
              call null_term(cp)
C Track ID
              if (ibit.eq.1) ichan = ichan+1 ! increment channel 
              write(ctr,'("CH",i2.2)') ichan
              call null_term(ctr)
C sign/mag
              if (ibit.eq.1) cbit='sign'
              if (ibit.eq.2) cbit='mag'
              call null_term(cbit)
C headstack
C             nch = nch + ib2as(ihd,ibuf,nch,2)
              write(chd,'(i1)') ihd
              call null_term(chd)
C write the def ...
              call fcreate_fanout_def_subpass(ptr_ch(cp))
              call fcreate_fanout_bitstream_list(ptr_ch(ctr))
              call fcreate_fanout_bitstream_list(ptr_ch(cbit))
              call fcreate_fanout_bitstream_list(ptr_ch(char(0)))
              call fcreate_fanout_def_headstack(ptr_ch(chd))
C ... and add the track list
              write(cit,'(i2.2)') it
              call null_term(cit)
              call fcreate_fanout_trksID_list(ptr_ch(cit))
              if (ifan(isp,ic).eq.2.or.ifan(isp,ic).eq.4) then
                write(cit,'(i2.2)') it+2
                call null_term(cit)
                call fcreate_fanout_trksID_list(ptr_ch(cit))
              endif 
              if (ifan(isp,ic).eq.4) then
                write(cit,'(i2.2)') it+4
                call null_term(cit)
                call fcreate_fanout_trksID_list(ptr_ch(cit))
                write(cit,'(i2.2)') it+6
                call null_term(cit)
                call fcreate_fanout_trksID_list(ptr_ch(cit))
              endif 
              call fcreate_fanout_trksID_list(ptr_ch(char(0)))
                  endif ! do this def
                  enddo ! ibit
                enddo ! isb sidebands
              enddo ! ich channels
            enddo ! ihd headstacks
          enddo ! ipass subpasses
        enddo ! each group
      enddo ! codes
    
      write(*,*) " " 
      itype=9
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
! Don't have to put out track commands for VDIF
!          ind=index(refdef_name(itype,isp,ic),"VDIF")
!          if(ind .ne. 0) cycle 
C def
!          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
!          il=trimlen(refdef_name(itype,isp,ic))
          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)
       end do 
      end do      
      write(*,*) " " 

C 3. Write track_frame_format for each type in this schedule.

      km3=.false.
      km4=.false.
      kv=.false.
      km3mode=.false.
      km5b_rec =.false. 
      kvdif =.false. 
      do is=1,nstatn
        cstrack_tmp=cstrack(is)
        call capitalize(cstrack_tmp)
        cstrec_tmp=cstrec(is,1)
        call capitalize(cstrec_tmp)   

        select case (cstrack_tmp)
        case("MARK3")
          km3=.true.
        case("VLBA","VLBAG")
          kv=.true.
        case("MARK4","VLBA4","VLBA5","K4-1","K4-2","K4-1/K3","K4-2/K3",
     >       "K4-1/M4","K4-2/M4","MARK5", "DBBC","NONE",
     >   "DBBC_DDC", "DBBC_PFB","DBBC_DDC/FILA10G","DDC_PFB/FILA10G")  
! Now look at the kind of recorder...
          select case(cstrec_tmp)
          case("MARK5B")
             km5b_rec=.true.
          case("K5")
             if(kvlba_corr) then
                km4=.true.
             else
                km5b_rec=.true.
             endif
          case("MARK4")
             km4=.true.
          case("MARK5C","FLEXBUFF")
             kvdif=.true.
          end select 
 
        case default
          write(*,*) "Warning! Unknown rack type: "
     >        //lq//trim(cstrack(is))//lq
        end select
        do ic=1,ncodes
           if(cmode(is,ic)(1:1) .ge. "A" .and.
     >        cmode(is,ic)(1:1) .le. "E") km3mode=.true.
        enddo
      enddo

      if (km3.or.(km3mode.and.kv)) then
        call fcreate_def(ptr_ch('Mark3A_format'//char(0)))
        cfr = 'Mark3A'
        call null_term(cfr)
        call fcreate_track_frame_format(ptr_ch(cfr))
      endif
      if (km4) then
        call fcreate_def(ptr_ch('Mark4_format'//char(0)))
        cfr = 'Mark4'
        call null_term(cfr)
        call fcreate_track_frame_format(ptr_ch(cfr))
      endif
      if (km5b_rec) then
        call fcreate_def(ptr_ch('Mark5B_format'//char(0)))
        cfr = 'Mark5B'
        call null_term(cfr)
        call fcreate_track_frame_format(ptr_ch(cfr))
      endif

      if (kvdif) then
        call fcreate_def(ptr_ch('VDIF'//char(0)))
        if(kvlba_corr) then 
           cfr = "VDIF5032"
        else
           cfr = 'VDIF/8032/2'
        endif 
        
        call null_term(cfr)
        call fcreate_track_frame_format(ptr_ch(cfr))
      endif

      kv=.true. 
      if (kv) then
        call fcreate_def(ptr_ch('VLBA_format'//char(0)))
        cfr = 'VLBA'
        call null_term(cfr)
        call fcreate_track_frame_format(ptr_ch(cfr))   
        if(kvlba_corr) then 
           cfr = 'off'
        else 
           cfr = 'on'
        endif 
  
        call null_term(cfr)
        call fcreate_data_modulation(ptr_ch(cfr))
      endif

      write(luscn,'()')
      RETURN
      END
