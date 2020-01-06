      subroutine astro_cull(luout,min_obs,rmin_ratio)
! Remove sources from astrometric list that
!    Either have <min_obs     OR  #obs/#scans   <rmin_ratio 

C   COMMON BLOCKS USED
 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'

! History:
!  2008Dec01   JMGipson. First version.
!  2010Mar19  JMGipson. Changed formatting. 
!            Use subroutine find_obs_per_src instead of doing calculation internally. 
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
    
! This is total over astrometric sources which are removed      
      integer NumScansAst
      Integer NumObsAst

      if(luout .le. 0) return

      write(*,'("Culling sources with numObs < ",i4)')   Min_obs
      write(*,'("         or  numObs/NumScan < ",f4.1)') rmin_ratio
 

      call find_obs_per_src(NumObsSrc,NumScansSrc,
     >     NumObsTot,NumScansTot)

100   continue

! Now extract the astrometric sources.
      kfirst=.true.
      NumObsAst=0
      NumScansAst=0
      do i=1,Nsourc
! Astrometric sources.
 
        if(kastro_src(i) .and. 
     >     (NumObsSrc(i) .lt. Min_obs .or. 
     >      dble(NumObsSrc(i))/dble(NumScansSrc(i)).lt.rmin_ratio)) then
!            write(*,*) NumObsSrc(i),  NumScansSrc(i),
!     >            dble(NumObsSrc(i))/dble(NumScansSrc(i))
       
          if(kfirst) then
            write(luout,*)
     >        "Source      Min     Max    Actual  Num #Scans"
            kfirst=.false.
          endif
          NumObsAst=NumObsAst+NumObsSrc(i)
          NumScansAst=NumScansAst+NumScansSrc(i)
          write(luout,'(a,1x,3f8.2,1x,i5,1x,i4)') csorna(i)(1:8),
     >       rmin_astro(i)*100.,rmax_astro(i)*100.,
     >       dble(NumObsSrc(i))/dble(NumObsTot)*100.,
     >       NumOBsSrc(i),NumScansSrc(i)
! remove from astrometric list
          rmin_astro(i) =0.
          rmax_astro(i) =1. 
        endif
      end do
      if(.not. kfirst) then
        write(luout,'(a,1x,16x,f8.2,1x,i5,1x,i4)') "Total   ",
     >    dble(NumObsAst)/dble(NumObsTot)*100.,NumObsAst,NumScansAst
      else
        write(luout,'(A)') "Did not cull any sources." 
      endif 

      return
      end
