      subroutine getist(ic,itype,ist,ipr,npx)

C GETIST returns an array with the indices for which to
C make REF statements and the stations each applies to.
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/statn.ftni'
      include 'skcom.ftni'
C 990921 nrv New. Called by vmoout.
C 000830 nrv Don't use refdef_name if it's null.
! 2014May02 JMG. Added comments
! 2018Oct09 JMG. when writing out first station did not have extra space.
C Input
      integer ic                 !code
      integer itype              !type (1-9: $FREQ, $BBC, ...$TRACKS, . See vmoout)
! Return 
      integer ist(max_stn)       !station list
      integer ipr(max_stn)       !
      integer npx
C Local
      integer is,is1,is2

! Initialize
      npx=0
      do is=1,nstatn
        ist(is)=0
      enddo

!  
      do is=1,nstatn ! stations
        if (ist(is).eq.0.and.                             ! haven't done the station.   
     >     refdef_name(itype,is,ic)(1:1).ne.char(0)) then ! new group      
          write(*,'("    ",a," ",a," ",$)') 
     >       refdef_name(itype,is,ic),  cstnna(is)
          is1=is
          ist(is1)=is1 ! first in this group
          npx=npx+1 ! number of refs or qrefs needed
          ipr(npx)=is ! station index from which to get the info
          do is2=is1+1,nstatn ! find the rest for this qref
            if (refdef_name(itype,is1,ic).eq.
     >          refdef_name(itype,is2,ic)) then
                ist(is2)=is1
                write(*,'(a," ",$)')cstnna(is2)
            endif 
          enddo ! find the rest for this qref
          write(*,*) " " 
        endif ! new group       
      enddo ! stations
      
      return
      end
