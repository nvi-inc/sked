      subroutine clear_close_sources(isrcvec,NsrcUse,istnSub,numsub)
! Mark sources that are too close (or too far) as invisible

C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include 'major.ftni'
      integer istnsub(*)
      integer NumSub
      integer NsrcUse
      integer isrcvec(*)

! Clear sources which are too close.
! Functions
      real*8 dot8

! History
! 2008Jun20 JMGipson.  First version
! 2011Apr25  Added MaxAngle

! functions

! local
      integer isrc,isrcj
      integer i,j
      integer istn
      double precision dist                     !distance between two sources
      double precision src_unit(3)              !unit vector to source
      double precision srcj_unit(3)
      character*2 CVSMap(max_Stn)  !Holds map of station visibility

      do i=1,NsrcUse
        isrc=isrcvec(i)
        call make_unit_vector(
     >       sorp50(1,isrc),sorp50(2,isrc),src_unit)
!            write(*,"('XXX',i3,3f8.2)") isrc, src_unit
        cVSMap=" "
        do j=1,NumSub
          istn=istnsub(j)
          isrcj=nsorcur(istn)
          if(kvs(isrc,istn)) CvsMap(j)="X"

          if(kvs(isrc,istn).and.          !src0 is visible at station istn
     >       isrcj .gt. 0) then             ! we previously observed a source.
            if(isrc .eq. isrcj) then       ! Check to see if the distance between the two sources is smaller than we want.
              dist=0.d0
            else
!            write(*,"(i3,3f8.2)") isrc, src_unit
              call make_unit_vector(
     >            sorp50(1,isrcj),sorp50(2,isrcj),srcj_unit)
              dist=dot8(src_unit,srcj_unit)
            endif
            dist=acos(dist)*rad2deg
!            writE(*,*) dist, minangle, csorna(isrc),csorna(isrcj)
            if(dist .lt. rMinAngle) then
               kvs(isrc,istn) = .false.
             endif
            if(dist .gt. rMaxAngle) then
               kvs(isrc,istn)=.false.
            endif 

          endif
        end do
!        write(ludsp,'(a8,1x,34a2)') csorna(isrc)," |",
!     >           (Cvsmap(j),j=1,NumSub), "| "

      end do
      return
      end
