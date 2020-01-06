      SUBROUTINE vstout
C
C  This routine writes out the VEX $STATION section.
C  NOTE: If a non-VEX input schedule is being written into a VEX-format
C  output then the user must first select stations from the catalogs to
c  retrieve catalog information needed in the VEX file.
C
C   HISTORY:
C 990606 nrv New. Copied from vsiout.
C 990914 nrv Use new VEX writing utilitites.
c 991130 nrv Add bit density, tape length.
! 2006Nov30 JMG.  Use cstrec(istn,irec)
! 2010.06.16 JMG Leave spaces between names when writing to screen.
! 2014May22 JMG.  Don't write out tape stuff if VLBA station.
! 2014Aug22 JMG.  Make determining if VLBA station a function call.
! 2018Oct09 JMG. Don't need to write out tape type or density.  
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C     CALLED BY:   SKCLS
C
! functions
      integer trimlen
      integer ptr_ch
      integer iwhere_in_string_list
      logical kvlba_station      !true if a VLBA station. 
      
C  LOCAL
      character*128 cdas,crack,crec
      character*3 cpo
      character*9 cstn,cant,cid,cids
! holds       
      logical kvlba_stat    
       
      integer is,icd

C  1. STATION

      call fcreate_block(ptr_ch('STATION'//char(0)))
      write(luscn,'("STATION: ",$)')

C  2. each station

      do is=1,nstatn
        kvlba_stat=kvlba_station(cpocod(is))
            
        write(luscn,'(a," ",$)') cstnna(is)
C def
        cpo=cpocod(is)
        call null_term(cpo)
        call fcreate_def(ptr_ch(cpo))
C ref $SITE=<name>
        cstn=cstnna(is)
        call null_term(cstn)
        call fcreate_ref(ptr_ch('SITE'//char(0)),ptr_ch(cstn))
C ref $ANTENNA=<name>
        cant=cantna(is)
        call null_term(cant)
        call fcreate_ref(ptr_ch('ANTENNA'//char(0)),ptr_ch(cant))       
C ref $DAS=<rack>_rack
        crack=cstrack(is)
        icd = trimlen(crack)
        if (icd.gt.0) then          
          cdas = crack(1:icd) // '_rack'
          if(kvlba_stat) cdas="RDBE"
          call null_term(cdas)
          call fcreate_ref(ptr_ch('DAS'//char(0)),ptr_ch(cdas))
        endif
        if(kvlba_stat) goto 100 
C ref $DAS=stn_<ID>
        cid=cterid(is)
        cids = cpo(1:2)// '_' // cid    
        call null_term(cids)
        call fcreate_ref(ptr_ch('DAS'//char(0)),ptr_ch(cids))
C ref $DAS=<rec>_recorder
        crec=cstrec(is,1)
        icd = trimlen(crec)
        if (icd.gt.0) then
          cdas = crec(1:icd) // '_recorder'
          call null_term(cdas)
          call fcreate_ref(ptr_ch('DAS'//char(0)),ptr_ch(cdas))
        endif
C ref $DAS=<drives>
        if(cstrec(is,1)(1:4) .eq. "Mark4" .or.
     >     cstrec(is,1)(1:4) .eq. "VLBA") then
          if (nrecst(is).eq.1) then
            call fcreate_ref(ptr_ch('DAS'//char(0)),
     .                        ptr_ch('1_recorder'//char(0)))
          else if (nrecst(is).eq.2) then
            call fcreate_ref(ptr_ch('DAS'//char(0)),
     .                        ptr_ch('2_recorder'//char(0)))
          endif
        endif

! With transition to disk, don't need this. 
      if(.false.) then 
        if(.not. kvlba_corr) then
C ref $DAS=<tape>
        if (maxtap(is).gt.17000) then ! thin tape
          call fcreate_ref(ptr_ch('DAS'//char(0)),
     .         ptr_ch('thin_tape'//char(0)))
        else ! thick
          call fcreate_ref(ptr_ch('DAS'//char(0)),
     .         ptr_ch('thick_tape'//char(0)))
        endif
C ref $DAS=<density> 
        if (bitdens(is,1).gt.56000) then ! high density on CODE 1
          call fcreate_ref(ptr_ch('DAS'//char(0)),
     .         ptr_ch('high_density'//char(0)))
        else
          call fcreate_ref(ptr_ch('DAS'//char(0)),
     .         ptr_ch('low_density'//char(0)))
        endif
        endif 
      endif 
100   continue 

      enddo ! stations
      write(luscn,'()')

      RETURN
      END
