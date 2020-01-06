      subroutine snr_find_numgood_numbad(icod,isrc,istn,nstn,lu)
! Include
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/constants.ftni'
      include '../skdrincl/freqs.ftni'
      include 'astro.ftni'

! History:
!  first version???
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  
      
! Passed
      integer icod 
      integer isrc
      integer istn(*)
      integer nstn
      integer lu
! functions
      integer ibnum !function

! Local
      integer i,j,k         !counters
      integer is,js         !
      integer iba           ! iband
      integer NumBand
      integer iband(max_band) ! band indices for this freq. code
      integer ibl           ! baseline 

      integer iSNR_target ! SNR target
      integer imarg_use 
      logical ktarget_snr_zero  ! are all of the target snrs zero?
      logical kbadSNR                        !Bad SNR on this baseline


! Initialize 
      NumBadSNR=0
      NumGoodSNR=0     

! Get the number of bands.
      call gtban(icod,NumBand,iband)
    
C     Use single-bl target SNR if it's been set.
     
      DO I=1,NSTN-1 ! first station
        IS=ISTN(I)          
        DO J=I+1,NSTN ! second station
          JS=ISTN(J)
          ibl=ibnum(is,js)
          kbadsnr=.false. 
          ktarget_snr_zero=.false. 
          do k=1,NumBand
            iba = iband(k)  
! Should we use the single or multi-baseline target?
            if (nstn.eq.2.and.isnrbl_1(iba,ibl).gt.0) then ! single
              iSNR_target = isnrbl_1(iba,ibl)
            else ! multi
              iSNR_target = isnrbl(iba,ibl)
            endif ! single/multi
C     If achieved SNR is less than target + margin, then set 
C      ierr=1 and write message about this baseline.
!      Also indicate bad snr in KbadSNR    
            if(kastro_src(isrc) .and. imarg_ast(iba) .ne. 0) then 
              imarg_use=imarg_ast(iba)
            else
              imarg_use=imarg(iba)
            endif 
            if(iSNR_target .eq. 0) then
                ktarget_snr_zero = .true.
            endif 
            if(iactbl(iba,ibl)+imarg_use.lt.iSNR_target) then      
              kbadsnr=.true.                  
              if(lu.gt.0.and..not.kauto) then
                write(lu, 
     >         "('WARNING! (snr_find_numgood_numbad):  For source: ',a,
     >        ' SNR of', i3, $)")    trim(csorna(isrc)),iactbl(iba,ibl)
                if (imarg_use .gt.0) then
                  write(lu,'("+",i3," (margin) ",$)') imarg_use
                endif 
                write(lu,'(" is less than minimum required SNR ",
     >             i3," for ",  A2,"-",A2, " at ", A1, "-band")')  
     >          isnr_target,  cpocod(is),cpocod(js),cband(iba)                
              endif             
            endif
          enddo 
          if(.not. ktarget_snr_zero) then 
            if(kbadSNR) then
               numbadsnr(i)=numbadsnr(i)+1
               numbadsnr(j)=numbadsnr(j)+1
             else
               numgoodsnr(i)=numgoodsnr(i)+1
               numgoodsnr(j)=numgoodsnr(j)+1
             endif 
          endif 
        end do
      end do
      return
      end 

