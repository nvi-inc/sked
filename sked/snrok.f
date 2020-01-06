      subroutine snrok(istn_orig,nst_orig,nsor,icod,lu,iokst,mjd,ut)
C
C      SNROK checks that each station can see the source and
C      that it has sufficient SNR. 
C
      include '../skdrincl/skparm.ftni'
C
C  Input:
      integer istn_orig(max_stn) ! station indices to check
      integer nst_orig          ! Number of stations
      integer nsor          ! source
      integer lu            ! LU.  <0 means no messages. 
      integer icod 
      integer mjd 
      real*8 ut
!
C
C  Output:
      integer iokst(max_stn) ! error codes for each station
C                              0=ok, -1=no flux, -2=low SNR
C
C  Common:
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'major.ftni'

! functions
      integer iwhere_in_int_list
C
C  History:
C  891206 NRV Created
C  891207 NRV Modified to do more of the work
!  2004Feb25  JMG Rewritten.
!  2010Apr07  If ends with 1 station, mark it bad.
!             Also, use iwhere_in_int_list   function.
!  2014Mar26  Compare NumBadSNRSNR with NumGoodSNR


C
C  Called by: NEWOB, NEXTC
C  Calls: SNRSK
C
C  Local variables
      integer nst             ! current number of good stations
      integer ist(max_stn)    ! current good indices
      logical kok             ! false while we're still elim. stations
      integer i,j,ierr
      integer ii
      integer ikey(max_stn)
      integer NumBadSNRIn(max_stn)
      integer NumGoodSNRIn(max_stn)
      integer iBadThres
      integer iwhere
C
C
C  1. First copy the original stations into the local array,
C     stations are deleted from IST as they are found bad.
C     Copy original stations into output array. 
C     Call SNRSK to get IDURST and IACTBL computed.  
C     Loop back if a station is eliminated.
C
      do i=1,nst_orig
        ist(i)=istn_orig(i)
        iokst(i)=0
      enddo
!      write(*,*) "Start: ", nst_orig, "| ", istn_orig(1:nst_orig) 
      nst=nst_orig
      kok=.false.  !this means we go through loop at least once.

! Loop where we eliminate bad stations. 
      do while (.not.kok.and. nst .ge. 2) ! calculate dur/SNR and eliminate
        kok=.true.
!        write(*,*) "Before SNRSK"
        CALL SNRSK(ISSCAN(NSOR),nst,ist,nsor,icod,IERR,lu,mjd,ut)
     
! NumBadSNR  now has number of stations that link to current one that have low SNRS.
! NumGoodSNR now has number of stations that link to a station with good SNRs
!        write(*,'(a,12i4)') "SNROK: bad  ",NumBadSNR(1:nst)
!        write(*,'(a,12i4)') "SNORK: Good ",NumGoodSNR(1:Nst)
        if (ierr.lt.0) then !not enough information
          do i=1,nst_orig 
            iokst(i)=-3
          enddo
          return
        endif
C
C  2. Check for IDURST<0 meaning no flux to that station.
C
        do i=1,nst
          j=ist(i)
          if (idurst(j).lt.0) then !no flux to station.
! Mark as bad in original list.
            iwhere=iwhere_in_int_list(istn_orig,nst_orig, ist(i))
            if(iwhere .ne. 0) then
              iokst(iwhere)=-1
            else
              write(*,*) "SNROK: Should never get here!"
            endif 
            ist(i)=-abs(ist(i))
            kok=.false.
          endif !not valid
        enddo        
! extract the number of bad links per station.

! Sort.
       call indexxint(nst,NumBadSNR,ikey)

       j=ikey(nst)
       if(kAllblGood) then
         ibadThres=0            !A single bad baseline marks it as bad.
       else
         ibadThres=NumGoodSNR(j) !must have at least as man ygood links as Badd. 
       endif

! Remove the station with the highest number of BADSNRs if it is greater than the threshold.    
       if(NumBadSNR(j).gt.iBadThres) then
! Mark it bad in original list.           
          iwhere=iwhere_in_int_list(istn_orig,nst_orig, ist(j))
          if(iwhere .ne. 0) then
              iokst(iwhere)=-2
           endif          
           ist(j)=-abs(ist(j))
           kok=.false.
       endif  
   
!       write(*,'("Orig: ",i4," | ",12i4)')nst_orig,istn_orig(1:nst_orig)
!       write(*,'("New:  ",i4," | ",12i4)')nst,ist(1:nst)
       do j=1,nst
         if(NumGoodSNR(j) .eq. 0) then 
           iwhere=iwhere_in_int_list(istn_orig,nst_orig, ist(j))
           if(iwhere .ne. 0) then
              iokst(iwhere)=-4
              ist(j) = -abs(ist(j))       !flag as bad
              kok=.false. 
           endif
         endif
      end do 


C
C  5. Delete stations marked.
C
        if (.not.kok) call destn(nst,ist)     
C  6. Return to top of loop and re-calculate dur/SNR for new subnet.
      enddo !calculate dur/SNR and eliminate bad stations


      if(nst .eq. 1) then    !if only have 1 station, also mark it as bad. 
         iwhere=iwhere_in_int_list(istn_orig,nst_orig,ist(1))              
         if(iwhere .le. 0) then
            write(*,*) "Whoa!!--No stations left." 
         endif 
         iokst(iwhere)=-1
      endif 
!      write(*,*) "SNROK: Done!" 
!      write(*,'("IOKST",10x, 32i4)') iokst(1:nst_orig)

         
C
      return
      end
