      SUBROUTINE vpcout 
C
C  This routine writes out the VEX $PHASE_CAL_DETECT section.
C
C   HISTORY:
C 990929 nrv New. 
! 2014May22 JMGipson.  If correaled at VLBA modify phase-cal stuff. 
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer ptr_ch

C  1. PHASE_CAL_DETECT

      call fcreate_block(ptr_ch('PHASE_CAL_DETECT'//char(0)))
      write(luscn,'("PHASE_CAL_DETECT: ")')

C  2. Contents

      call fcreate_def(ptr_ch('Standard'//char(0)))
      call fcreate_phase_cal_detect(ptr_ch('U_cal'//char(0)))
      call fcreate_phase_cal_detect_list(ptr_ch('1'//char(0)))
      if(kvlba_corr) then
         call fcreate_phase_cal_detect_list(ptr_ch('8'//char(0)))
      endif 
      call fcreate_phase_cal_detect_list(ptr_ch(char(0)))

      RETURN
      END
