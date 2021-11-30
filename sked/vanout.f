      SUBROUTINE vanout 
C
C  This routine writes out the VEX $ANTENNA section.
C
C   HISTORY:
C 990606 nrv New. Copied from vsiout.
C 990916 nrv Use VEX writing utilities.
! 2010.06.16 JMG Leave spaces between names when writing to screen.
! 2021-04-02  JMG Renamed STNRAT-->slew_rate, istcon-->slew_off.  Made slew_off real
! 2021-11-10  Renamed slew_rate-->slew_vel
! 2021-11-10 Modify slew_off when writing out to include acceleration erm.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      real x
      character*9 cant
      character*8 cax
      character*8 cax_vec(2)      
      integer i12    !count over axis. 
      character*128 cstr,cstr2,cstr3,cstr4 ! converting to strings
      integer is
      integer ptr_ch
      character*4 laxis
      real t_acc 

C  1. ANTENNA

      call fcreate_block(ptr_ch('ANTENNA'//char(0)))
      write(luscn,'("ANTENNA: ",$)')

C  2. each antenna

      do is=1,nstatn
        write(luscn,'(a," ",$)') cantna(is)
        cant=cantna(is)
        call null_term(cant)
C def
        call fcreate_def(ptr_ch(cant))

C antenna_diam
        write(cstr,'(f6.2)') diaman(is)
C NOTE: should be antenna_diam in next version
        call null_term(cstr)
        call fcreate_antenna_diam(ptr_ch(cstr),ptr_ch('m'//char(0)))
C antenna_name
C NOTE: should be in the next version
C       call fcreate_antenna_name(ptr_ch(cant))
C axis_type
        call axtyp(laxis,iaxis(is),2)
        call c2lower(laxis,cax)
        if (cax(1:4).eq.'hadc') then
          cax_vec(1) = cax(1:2)
          cax_vec(2) = 'dec'
        else if (cax(1:1).eq.'x') then
          cax_vec(1) = cax(1:1)
          cax_vec(2) = cax(2:4)
        else
          cax_vec(1) = cax(1:2)
          cax_vec(2) = cax(3:4)
        endif
        call null_term(cax_vec(1))
        call null_term(cax_vec(2))
        call fcreate_axis_type(ptr_ch(cax_vec(1)),ptr_ch(cax_vec(2)))
C axis_offset
        x=axisof(is)
        write(cstr,'(f10.5)') x
        call null_term(cstr)
        call fcreate_axis_offset(ptr_ch(cstr),
     .        ptr_ch('m'//char(0)))
! Antenna motion over both axis.     
        do i12=1,2
           x = slew_vel(i12,is)*rad2deg*60.0 ! deg/min
          write(cstr,'(f5.1)') x
          call null_term(cstr)
          t_acc=0.d0 
          if(slew_off(i12,is) .ne. 0 .and. slew_acc(i12,is) .ne. 0) then 
             t_acc=slew_vel(i12,is)/slew_acc(i12,is)
          endif 
           
          write(cstr2,'(f5.1)') slew_off(i12,is)+t_acc
          call null_term(cstr2)
          call fcreate_antenna_motion(ptr_ch(cax_vec(i12)),ptr_ch(cstr),
     .          ptr_ch('deg/min'//char(0)),
     .          ptr_ch(cstr2),ptr_ch('sec'//char(0)))
        end do 

C pointing_sector 
C   first axis
        x = stnlim(1,1,is)*rad2deg
        write(cstr,'(f6.1)') x
        call null_term(cstr)
        x = stnlim(2,1,is)*rad2deg
        write(cstr2,'(f6.1)') x
        call null_term(cstr2)
C   second axis
        x = stnlim(1,2,is)*rad2deg
        write(cstr3,'(f6.1)') x
        call null_term(cstr3)
        x = stnlim(2,2,is)*rad2deg
        write(cstr4,'(f6.1)') x
        call null_term(cstr4)
        call fcreate_pointing_sector(ptr_ch('n'//char(0)),
     .         ptr_ch(cax_vec(1)),ptr_ch(cstr),ptr_ch('deg'//char(0)),
     .         ptr_ch(cstr2),ptr_ch('deg'//char(0)),
     .         ptr_ch(cax_vec(2)),ptr_ch(cstr3),ptr_ch('deg'//char(0)),
     .         ptr_ch(cstr4),ptr_ch('deg'//char(0)))

      enddo
      write(luscn,'()')

      RETURN
      END
