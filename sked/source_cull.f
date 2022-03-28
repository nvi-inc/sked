      subroutine source_cull(luout,min_obs)
! Remove sources from astrometric list that
!    Either have <min_obs     

C   COMMON BLOCKS USED
 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'   
      include 'astro.ftni'
      include 'skcom.ftni'

! History:
!  2012Apr09 JMGipson. First version based on astro_cull.
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  


! passed
      integer luout
      integer min_obs          !minimum # of accepatable obs
      real*8  rmin_ratio       !min ratio:   #obs/#scans

! local
      Integer*4 NumObsSrc(Max_Sor)      
      integer*4 NumScansSrc(Max_sor)
      integer*4 NumObsTot, NumScansTot
      logical kfirst                    !first line output.
      integer i
     
! This is the number of sources we keep....
      integer NumKeep                   !number to keep
      integer ikpSrc(Max_sor)           !pointer to the ones to keep.

      integer ierr                      !error 
      integer min_keep
      min_keep=8

! This is total over astrometric sources which are removed      
      if(luout .le. 0) return

      write(*,'("Culling sources with numObs < ",i4)')   Min_obs     
 
      call find_obs_per_src(NumObsSrc,NumScansSrc,NumObsTot,NumScansTot)

      NumKeep=0
      do i=1,Nsourc
! Keep all astrometric sources
         if(kastro_src(i)) then
!           continue
           NumKeep=NumKeep+1
           ikpsrc(NumKeep)=i
! Non-astrometric sources. 
         else if(NumObsSrc(i) .ge. Min_Obs) then
           NumKeep=NumKeep+1
           ikpsrc(NumKeep)=i
         endif     
       end do   
       if(NumKeep .ge. min_keep) then 
         write(*,*) "NumKeep ", NumKeep   
         call delete_all_obs()
         call keep_some_Srcs(ikpsrc,NumKeep,csofil,ierr)
         call rsini
       else
         write(*,*) "After cull would only have ",NumKeep
         write(*,*) "Need at least ",min_keep
       endif 

      return
      end
