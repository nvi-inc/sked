      subroutine exread

C   EXREAD reads the experiment name from the $EXPER  line
C   IBUF is already read and is in common.

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
C History
C 000326 nrv Removed from SKOPN.
! 2004May18 JMGipson completely rewritten
! 2010Jan21 JMG. Capitalize experiment code
! functions
      integer trimlen
!  local variables
      integer istart,inext
      logical knospace,keol,ktoken
      integer i

      istart=1
      do i=1,2
        call ExtractNextToken(cbuf,istart,inext,cexper,   !read in "$EXPERIMENT"
     >        ktoken,knospace,keol)
        istart=inext
      end do

      if(.not.ktoken) then
         cexper=" "
         write(luscn,'(a)') "No session name found"
      else
        call capitalize(cexper)
        write(luscn,'("Reading session: ",a)')
     >  cexper(1:trimlen(cexper))
      endif

      return
      end
