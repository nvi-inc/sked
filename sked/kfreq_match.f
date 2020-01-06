      logical function kfreq_match(istat1,icode1,istat2,icode2)
! subroutine to see if two frequences match up.
! 2006Jun22.  JMGipson. First version.
! Needed commons.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      integer istat1,icode1,istat2,icode2

! local
      integer i

! default is no match.
      kfreq_match=.false.

      if(nchan(istat1,icode1) .ne. nchan(istat2,icode2)) return
      do i=1,nchan(istat1,icode1)
        if(freqrf(i,istat1,icode1) .ne. freqrf(i,istat2,icode2)) return
      enddo

! all tests were true--must be a match.
      kfreq_match=.true.
      return
      end
