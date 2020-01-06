      SUBROUTINE vskout 
C
C  This routine writes out the VEX $SCHEDULING_PARAMS section.
C
C   HISTORY:
C 990607 nrv New. 
C
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
      integer ptr_ch

C  1. SCHEDULING_PARAMS

      call fcreate_block(ptr_ch('SCHEDULING_PARAMS'//char(0)))
      write(luscn,'("SCHEDULING_PARAMS")')
C def
      call fcreate_def(ptr_ch('SKED_PARAMS'//char(0)))

      call fcreate_literal(ptr_ch(' '//char(0)))
      call prout('v')

      call fcreate_literal(ptr_ch(char(0)))
      RETURN
      END
