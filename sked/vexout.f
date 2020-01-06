      SUBROUTINE vexout 
C
C  This routine writes out the VEX $EXPER section.
C
C   HISTORY:
C 990606 nrv New. Copied from exout.
C 990913 nrv Write 'unknown' for null strings.
C 990913 nrv Change WRITF_ASC calls to fcreate_xxx.
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
      character*9 cex
      integer ptr_ch

C  3. $EXPER

      call fcreate_block(ptr_ch('EXPER'//char(0)))
      write(luscn,'("EXPER")')
      cex=cexper
      call null_term(cex)
      call fcreate_def(ptr_ch(cex))
C exper_name
      call fcreate_exper_name(ptr_ch(cex))
C exper_description
      call null_term(cexperdes)
      call fcreate_exper_description(ptr_ch(cexperdes))
C PI_name
      call null_term(cpiname)
      call fcreate_pi_name(ptr_ch(cpiname))
C target_correlator
      call null_term(ccorname)
      call fcreate_target_correlator(ptr_ch(ccorname))

      RETURN
      END
