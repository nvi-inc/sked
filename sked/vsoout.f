      SUBROUTINE vsoout
C
C  This routine writes out the VEX $SOURCE section.
C NOTE: this code should really pull the strings from the catalog
C position entries directly so that the full precision is retained.
C
C   HISTORY:
C 990606 nrv New. Copied from vexout.
C 990923 nrv Use VEX utilities
C 991206 nrv More decimal places for RA and DEC.
C
! 2005Apr27 JMGipson. Converted to ascii
! 2014Sep16 JMGipson. Only write out iau name if there is one. 
!
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer irah,iram,idcd,idcm
      integer isec                 !fractional part of second
      integer lds,l,is,i
      real*4 ras,dcs,d
      character*50 cna
      integer ptr_ch
      character*1 lsq,ldq           !single and double qoutes.
      integer j

      lsq="'"
      ldq='"'

C  1. SOURCE

      call fcreate_block(ptr_ch('SOURCE'//char(0)))
      write(luscn,'("SOURCE")')

C  2. each source

      cna=" "  ! this keeps non-printing characters from apearing if len(cna)<len(csorna)
      do is=1,nceles
C def
        cna(1:Max_sorlen)=csorna(is)
        call null_term(cna)
        call fcreate_def(ptr_ch(cna))
C source_type
        call fcreate_source_type(ptr_ch('star'//char(0)),
     .      ptr_ch(char(0)))
C source_name
        call fcreate_source_name(ptr_ch(cna))
C IAU name
        if(len_trim(ciauna(is)) .gt. 0) then 
          cna(1:max_sorlen)=ciauna(is)
          call null_term(cna)
          call fcreate_IAU_name(ptr_ch(cna))
         endif 

C ra, dec, epoch
        CALL RADED(SORP50(1,is),SORP50(2,is),0.0d0,IRAH,IRAM,RAS,
     .   LDS,IDCD,IDCM,DCS,L,I,I,D)
        if (ras+0.00005d0.ge.60.d0) then
          ras=0.d0
          iram=iram+1
          if (iram.ge.60) then
            iram=iram-60
            irah=irah+1
          endif
        endif
        if (dcs+0.0005d0.ge.60.d0) then
          dcs=0.d0
          idcm=idcm+1
          if (idcm.ge.60) then
            idcm=idcm-60
            idcd=idcd+1
          endif
        endif
C ra  hhhmmss.sssss, eg. 12h32m43.12345s
        isec=ras
        ras=ras-isec
        write(cna,'(i2.2,"h",i2.2,"m",i2.2,f6.5,"s")')
     >    irah,iram,isec,ras
        call null_term(cna)
        call fcreate_ra(ptr_ch(cna))
C dec sddmmss.ssss, eg. -26d18'43.3860""

        if(sorp50(2,is) .lt. 0) then
          j=2
          cna(1:1)="-"
        else
          j=1
        endif
        isec=dcs
        dcs=dcs-isec

        write(cna(j:),'(i2.2,"d",i2.2,a1,i2.2,f5.4,a1)')
     >     idcd,idcm,lsq,isec,dcs,ldq

        call null_term(cna)
        call fcreate_dec(ptr_ch(cna))
C source_position_epoch
C       call fcreate_source_position_epoch(ptr_ch('J2000'//char(0)))
        call fcreate_ref_coord_frame(ptr_ch('J2000'//char(0)))
      enddo

      do is=1,nsatel
C def
        cna(1:max_sorlen)=csorna(is)
        call null_term(cna)
        call fcreate_def(ptr_ch(cna))
C source_type
        call fcreate_source_type(ptr_ch('earth_satellite'//char(0)),
     .      ptr_ch(char(0)))
C source_name
        call fcreate_source_name(ptr_ch(cna))
C inclination
        write(cna,'(f7.2)') satp50(1,is)
        call null_term(cna)
        call fcreate_inclination(ptr_ch(cna),ptr_ch('deg'//char(0)))
C eccentricity
        write(cna,'(f7.2)') satp50(2,is)
        call null_term(cna)
        call fcreate_eccentricity(ptr_ch(cna))
C arg_perigee
        write(cna,'(f7.2)') satp50(3,is)
        call null_term(cna)
        call fcreate_arg_perigee(ptr_ch(cna),ptr_ch('deg'//char(0)))
C ascending_node
        write(cna,'(f7.2)') satp50(4,is)
        call null_term(cna)
        call fcreate_ascending_node(ptr_ch(cna),ptr_ch('deg'//char(0)))
C mean_anomaly
        write(cna,'(f7.2)') satp50(5,is)
        call null_term(cna)
        call fcreate_mean_anomaly(ptr_ch(cna),ptr_ch('deg'//char(0)))
C semi-major_axis
        write(cna,'(f11.1)') satp50(6,is)
        call null_term(cna)
        call fcreate_semi_major_axis(ptr_ch(cna),ptr_ch('km'//char(0)))
C mean_motion
        write(cna,'(f8.3)') satp50(7,is)
        call null_term(cna)
        call fcreate_mean_motion(ptr_ch(cna))
C orbit_epoch
        write(cna,"(i4,'y',f6.2,'d')") isaty(is),satdy(is)
        if(cna(5:5) .eq. " ") cna(5:5)="0"
        if(cna(6:6) .eq. " ") cna(6:6)="0"

        call null_term(cna)
        call fcreate_orbit_epoch(ptr_ch(cna))

      enddo

      RETURN
      END
