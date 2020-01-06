      SUBROUTINE wrrblines(samprate,cfrcode,cbarrelname,ierr)
C
C  WRRBLINES writes the "R" and "B" lines for the $CODES section,
C  for one frequency code.
C
C   HISTORY:
C 960709 nrv New. Copied from wrfclines.
C 970402 nrv Check only that the first character of the barel is blank.
C 970418 nrv If no stations have barrel roll, don't write the line.
C
! 2005Apr26 JMGipson. Made completely ascii.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C Input:
      character*2 cfrcode
      real*8 samprate
      character*4 cbarrelname(max_Stn)

! functions
      integer trimlen

C  OUTPUT:
      integer ierr ! if error writing scratch file
C
C   SUBROUTINES
C     CALLED BY: WRFRS
C
C  LOCAL VARIABLES
      integer iptr,is
      integer nch
C
C  1. Write out one "R" line with the sample rate.

      write(lutmp,'("R ",a2,1x,f8.3)') cfrcode,samprate

C  2. Write out a "B" line if there are barrel rolls
      cbuf="B "//cfrcode
      iptr=6
      do is=1,nstatn
        if (cbarrelname(is).ne. " ".and.
     >       cbarrelname(is).ne. "none") then ! non-blank
          nch=trimlen(cstnna(is))
          cbuf(iptr:)=cstnna(is)(1:nch)//" "//cbarrelname(is)
          iptr=iptr+nch+1+4+1                   !the ones are for blank spaces.
        endif
      enddo
      write(lutmp,'(a)') cbuf(1:iptr)

      close(lucat)

      RETURN
      END
