      SUBROUTINE vmoout 
C
C  This routine writes out the VEX $MODE section.
C  NOTE: If a non-VEX file is being written out as a VEX-format file,
C  then the user must select frequencies from the catalogs first in
c  order to retrieve the catalog information not found in the old-format 
C  schedule file. 
C
C   HISTORY:
C 990609 nrv New. 
C 990914 nrv Change to using VEX utilities.
! 2005Oct28 JMGipson. Got rid of hollerith.
! 2014May02 JMGipson. Added comments. 
! 2018Oct03 JMGipson. No longer output HEAD_POS or PASS_ORDER.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer is,ic,ist(max_stn),ipr(max_stn),ilc
      integer npx,isp,itype,ipx
      character*3 c2
      character*20 ctype(9)
      integer trimlen,ptr_ch

! Question: Why do I have two tracks?
! Second tracks is 'Track_frame_format' 
      data ctype/'FREQ','BBC','IF','TRACKS','HEAD_POS','PASS_ORDER',
     >  'ROLL','PHASE_CAL_DETECT','TRACKS'/

!      write(*,*) "VLBA:", kvlba 
C  1. MODE
      call fcreate_block(ptr_ch('MODE'//char(0)))
      write(luscn,'("MODE")')
      call make_defnames
C  2. each code
      do ic=1,ncodes
        call fcreate_def(ptr_ch(modedef_name(ic)))
        ilc=trimlen(modedef_name(ic))
        write(*,'("CODE: ",a)') trim(modedef_name(ic))
 
        do itype=1,9 ! types
! skip HEAD_POS and PASS_ORDER
          if(itype .eq. 5 .or. itype .eq. 6) goto 100
!          if(kvlba_corr .and. (itype .eq. 5 .or. itype .eq. 6)) goto 100
!          if(kbonn_corr .and. itype .eq. 9) goto 100 
! Write on the screen the mode we are making...  
          write(luscn,'("  ",a,":")') trim(ctype(itype))    
          call null_term(ctype(itype))
          call getist(ic,itype,ist,ipr,npx)  
          do ipx=1,npx ! each group
            isp=ipr(ipx)
            call fcreate_qref(ptr_ch(ctype(itype)),
     >             ptr_ch(refdef_name(itype,isp,ic)))
            do is=1,nstatn ! check stations
              if (ist(is).eq.isp) then ! add qualifier
                c2=cpocod(is)
                call null_term(c2)
                call fcreate_qref_qualifier(ptr_ch(c2))
              endif ! add qualifier
            enddo ! check stations
            call fcreate_qref_qualifier(ptr_ch(char(0)))
          enddo ! each group
100     continue     
        enddo ! types
      enddo ! codes
!      write(luscn,'(1x)')

      RETURN
      END
