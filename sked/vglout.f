      SUBROUTINE vglout 
C
C  This routine writes out the VEX $GLOBAL section.
C
C   HISTORY:
C 990606 nrv New. Copied from exout.
C 990913 nrv Write 'unknown' for null strings.
C 990913 nrv Change WRITF_ASC calls to fcreate_xxx.
C 000529 nrv Remove writing $EXPER to vexout.
C 020713 nrv Add ref to SKED_PARAM
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
      character*50 ccom
      integer ptr_ch
      character*9 cex

C  1. VEX rev and sked rev comment

      call fcreate_version(ptr_ch('1.5'//char(0)))
      ccom = ' sked version '//skversion
      write(luscn,'(a)') ccom
      call vex_comment(ccom)

C  2. $GLOBAL

      call fcreate_block(ptr_ch('GLOBAL'//char(0)))
      write(luscn,'("GLOBAL")')
          
      cex=cexper
      call null_term(cex)
      call fcreate_ref(ptr_ch('EXPER'//char(0)), 
     >  ptr_ch(cex))   
      call fcreate_ref(ptr_ch('SCHEDULING_PARAMS'//char(0)),
     >  ptr_ch('SKED_PARAMS'//char(0))) 

      RETURN
      END
