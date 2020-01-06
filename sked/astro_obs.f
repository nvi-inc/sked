      subroutine astro_obs(luout)
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'

! passed
      integer luout

! local 

      Integer*4 NumObsSrc(Max_Sor)
      integer*4  NumScansSrc(Max_sor)
      Integer*4 NumObsTot,NumScansTot
      logical kfirst                    !first line output.
! AEM undo     logical i
! 040726  ZMM  changed from logical to integer
! 2009Nov03  JMGipson. Wasn't computing totals correctly. Changed krwnd to .true., set jdstcm to 0
! 2010Mar19  JMGipson. Changed formatting. 
!            Use subroutine find_obs_per_src instead of doing calculation internally.
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  

      integer i
      integer NumScansAst          !Number astrometric scans
      Integer NumObsAst            !Number astrometric obs 

      if(luout .le. 0) return

      call find_obs_per_src(NumObsSrc,NumScansSrc,
     >     NumObsTot,NumScansTot)
    
100   continue

! Now extract the astrometric sources.
      kfirst=.true.
      NumObsAst=0
      NumScansAst=0
      do i=1,Nsourc
        if(rmax_astro(i) .eq. 0) then
            continue
        else if(kastro_src(i)) then
          if(kfirst) then
            write(luout,*)
     >        "Source      Min     Max    Actual  #Num #Scans"
            kfirst=.false.
          endif
          NumObsAst=NumObsAst+NumObsSrc(i)
          NumScansAst=NumScansAst+NumScansSrc(i)
          write(luout,'(a,1x,3f8.2,1x,i6,1x,i4)') csorna(i)(1:8),
     >       rmin_astro(i)*100.,rmax_astro(i)*100.,
     >       dble(NumObsSrc(i))/dble(NumObsTot)*100.,
     >       NumOBsSrc(i),NumScansSrc(i)
        endif
      end do
      write(luout,'(a,1x,16x,f8.2,1x,i6,1x,i4)') "Total   ",
     > dble(NumObsAst)/dble(NumObsTot)*100.,NumObsAst,NumScansAst
      return
      end
