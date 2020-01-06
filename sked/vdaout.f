      SUBROUTINE vdaout 
C
C  This routine writes out the VEX $DAS section.
C
C   HISTORY:
C 990606 nrv New. 
C 990920 nrv Change to using VEX utilities.
C 991130 nrv Add record_density and tape_length.
! 2006Nov30  Do match using iwhere_in_String_list
!            use cstrec(istn,irec) rather than 2 arrays.
! 2014May22  JMG. Don't write out tape stuff for VLBA
! 2014Aug22  JMG. Don't write out VLBA_rack for VLBA stations and VLBA_CORR. 
! 2018OCT09  JMG. No longer output . Bit density, tape length.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/valid_hardware.ftni'
! function
      integer iwhere_in_string_list

C
C     CALLED BY:   SKCLS
C
C  LOCAL
      logical kdone_rec(max_rec_type),kdone_rack(max_rack_type)
      character*128 cter,cid,cr
      integer is,ilt,it
      integer trimlen,ptr_ch

      logical kvlba_station
  
C  1. DAS

      call fcreate_block(ptr_ch("DAS"//char(0)))
      write(luscn,'("DAS",$)')

C  2. Rack. Check rack type for each station and write the def
C     if it hasn't been written already.

      do it=1,max_rack_type
        kdone_rack(it)=.false.
      enddo
      do is=1,nstatn ! racks
        it=iwhere_in_string_list(crack_type,max_rack_type,cstrack(is))    
        if (it.ne.0.and..not.kdone_rack(it) .and. .not. 
     >     (cstrack(is) .eq. "VLBA" .and. kvlba_corr)) then
C def <rack type>_rack
          ilt=trimlen(crack_type(it))
          cr = crack_type(it)(1:ilt)//'_rack'//char(0)
          call null_term(cr)
          call fcreate_def(ptr_ch(cr))
C electronics_rack_type
          cr = crack_type(it)(1:ilt)//char(0)
          call null_term(cr)
          call fcreate_electronics_rack_type(ptr_ch(cr))
          kdone_rack(it)=.true.
        else ! not recognized
C         this can't happen ;>)
        endif
10      continue

      enddo ! racks

C  3. Recorder. Check rec type for each station and write the def
C     if it hasn't been written already.

      do it=1,max_rec_type
        kdone_rec(it)=.false.
      enddo
      do is=1,nstatn ! recorders
        it=iwhere_in_string_list(crec_type,max_rec_type,cstrec(is,1))
        if (it .ne. 0 .and. .not. kdone_rec(it)) then
C def <rec type>_recorder
          ilt=trimlen(crec_type(it))
          cr = crec_type(it)(1:ilt)//'_recorder'//char(0)
          call null_term(cr)
          call fcreate_def(ptr_ch(cr))
C record_transport_type
          cr = crec_type(it)(1:ilt)//char(0)
          call null_term(cr)
          call fcreate_record_transport_type(ptr_ch(cr))
          kdone_rec(it)=.true.
        else ! not recognized
C         this can't happen
        endif
      enddo ! recorders

      if(kvlba_corr) then
         cr="RDBE"
         call null_term(cr)
         call fcreate_def(ptr_ch(cr))
         cr="Mark5C"
         call null_term(cr)
         call fcreate_record_transport_type(ptr_ch(cr))
         cr="RDBE2"
         call null_term(cr)
         call fcreate_electronics_rack_type(ptr_ch(cr))
         call fcreate_number_drives(ptr_ch('2'//char(0)))
!         call fcreate_headstack('1'//char(0),'0'//char(0))
         call fcreate_headstack(ptr_ch('1'//char(0)),
     >              ptr_ch(char(0)),   ptr_ch('0'//char(0)))
         call fcreate_headstack(ptr_ch('2'//char(0)),
     >              ptr_ch(char(0)),   ptr_ch('1'//char(0)))
  
! Tape motion takes 7 arguments!
         call fcreate_tape_motion(
     >        ptr_ch('adaptive'//char(0)),
     >        ptr_ch('0'//char(0)),  ptr_ch('min'//char(0)),
     >        ptr_ch('0'//char(0)),  ptr_ch('min'//char(0)),
     >        ptr_ch('10'//char(0)), ptr_ch('sec'//char(0)))    
      endif     

C  8. Write the defs and IDs for each Mk/VLBA rack.
      do is=1,nstatn ! IDs    
      if(.not.kvlba_station(cpocod(is))) then 
C def <two-letter-code>_<terminal>
        cid = cpocod(is)(1:2)//'_'//cterid(is)
        call null_term(cid)
        call fcreate_def(ptr_ch(cid))
C recording_system_id
        cter=cterid(is)
        call null_term(cter)
        call fcreate_recording_system_id(ptr_ch(cter))
C electronics_rack_name
        cter=cterna(is)
        call null_term(cter)
      endif 
C NOTE: these should exist in next version
C       call fcreate_electronics_rack_name(ptr_ch(cter))
C record_transport_name
C       call fcreate_record_transport_name(ptr_ch(cter))
      enddo

C 8. Drives. Always write out the number_drives commands.

C def <drives>
      call fcreate_def(ptr_ch('1_recorder'//char(0)))
C number_drives
      call fcreate_number_drives(ptr_ch('1'//char(0)))
C def <drives>
      call fcreate_def(ptr_ch('2_recorder'//char(0)))
C number_drives
      call fcreate_number_drives(ptr_ch('2'//char(0)))
C NOTE: these will be scheduling parameters in the next version

C 9. Bit density, tape length.
C 
      if(.false.) then 
!      if(.not.kvlba_corr) then 
        call fcreate_def(ptr_ch('low_density'//char(0)))
        call fcreate_record_density(ptr_ch('33333'//char(0)),
     .               ptr_ch('bpi'//char(0)))
        call fcreate_def(ptr_ch('high_density'//char(0)))
        call fcreate_record_density(ptr_ch('56250'//char(0)),
     .               ptr_ch('bpi'//char(0)))
        call fcreate_def(ptr_ch('thick_tape'//char(0)))
        call fcreate_tape_length(ptr_ch('8800'//char(0)),
     .   ptr_ch('ft'//char(0)),ptr_ch(char(0)),ptr_ch(char(0)))
        call fcreate_def(ptr_ch('thin_tape'//char(0)))
        call fcreate_tape_length(ptr_ch('17400'//char(0)),
     .   ptr_ch('ft'//char(0)),ptr_ch(char(0)),ptr_ch(char(0)))
      endif 

      write(luscn,'()')
      RETURN
      END
