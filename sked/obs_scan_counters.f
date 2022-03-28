      Module Obs_Scan_Counters      
      ! Arrays dealing with number of observations and scans
      integer*4  NumObs
      integer, allocatable :: NumObsSource(:)     !Number of observations so far for source
      integer, allocatable :: NumScanSource(:)    !Number of scans so far for source
      integer, allocatable :: NumObsStat(:)       !Ditto for stations      
      integer, allocatable :: NumScanStat(:)     
      
      integer, allocatable :: NumObsBaseline(:)
      integer, allocatable :: NumObsSourceBaseline(:,:)
      
!      integer nsorobs(max_sor,max_baseline)             !# of obs of this source on this  baseline     
!      integer NOBSSO(MAX_SOR)           !# of obs on this source (Really # of scans)
      
      Contains      
!-----------------------------------------------------------------------
      subroutine Init_Obs_scan_counters(max_sor,max_stn)
      implicit none 
! Initialize these counters. 
! 2021-09-24 JMGipson  First version       
      integer max_sor       !maximum number of staitons
      integer max_stn       !maximum  number of sourcee
! local
      integer max_baseline
               
      max_baseline=max_stn*(max_stn-1)/2
! Allocate if neccessary      
      if(.not. allocated(NumObsSource)) then
!         write(*,*) "OBs_scan_counters Allocating space!"    
        allocate (NumObsSource(Max_sor))     !Number of observations so far for source
        allocate (NumScanSource(Max_sor))    !Number of scans so far for source
        allocate(NumObsStat(Max_stn))       !Ditto for stations      
        allocate(NumScanStat(Max_stn))     
        allocate(NumObsBaseline(Max_baseline))
        allocate(NumObsSourceBaseline(Max_sor,Max_baseline))                          
      endif 
 
! Intiailize to zero.  
      NumOBs=0         
      NumObsSource=0
      NumScanSource=0
      NumObsStat=0
      NumScanStat=0
      NumObsBaseline=0
      NumObsSourceBaseline=0
      return
      end subroutine
!******************************************************************************
      subroutine clean_up_obs_scan_counters
! get rid of stuff that was allocated.      
      if(allocateD(NumObsSource))  deallocate(NumObsSource)
      if(allocated(NumScanSource)) deallocate(NumScanSource)      
      if(allocated(NumObsStat))    deallocate(NumObsStat)
      if(allocated(NumScanStat))   deallocate(NumScanStat)
      if(allocated(NumObsBaseline)) deallocate(NumObsBaseline)
      if(allocated(NumObsSourceBaseline)) 
     &  deallocate(NumObsSourceBaseline)

      end subroutine 
      
!*****************************************************************************      
      subroutine update_obs_scan_counters(isor,istcur,nstncur)
      implicit none 
      integer nstncur
      integer istcur(*)
      integer isor
! Function
      integer ibnum       
! local
      integer i,j
      integer istn,jstn  
      integer ibl    
      
!      write(*,'(2i4,"| ", 30i4)') isor, nstncur, istcur(1:nstncur) 
      NumObs=NumObs+(nstncur-1)*nstncur/2
      
      NumScanSource(isor)=NumScanSource(Isor)+1
      NumObsSource(isor) =NumObsSource(isor)+((nstncur-1)*nstncur)/2

      do i=1,Nstncur
        istn=istcur(i)
        NumScanStat(istn)=NumScanStat(istn)+1
        NumObsStat(istn) =NumObsStat(istn)+Nstncur-1
        do j=i+1,NstnCur
           jstn=istcur(j) 
           ibl =ibnum(istn,jstn) 
           NumObsSourceBaseline(isor,ibl)=NumObsSourceBaseline(isor,ibl)+1
        end do
      end do  
      return
      end subroutine
! *************************************************************************************
      end module 
      
    
