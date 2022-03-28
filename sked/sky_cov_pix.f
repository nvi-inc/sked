      subroutine sky_cov_pix(iobs_in)
! Include files
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni'

! input
      integer iobs_in
! output

! 2021-01-20 JMGipson. Changed test on elevations. Used to test on both iobs and iobs_in 
! 2022-02-02 JMGipson.  Happy groundhogs day
   

! local
      integer i         	!counter over obs
      integer iobs      	!observation
      integer istn      	!stations
      integer nsta              !number of stations
      logical kfound_pix(max_stn,max_pix)   !Did we find this pixel at this station?
      integer isphere_pix       !which pixel did we find.
      integer ierr
      real*8 temp

!      write(*,'("In ",i3, 20f8.2)') iobs_in, eleva(iobs_in,1:nstatn)
     
      kfound_pix=.false.
      inum_pix_obs(iobs_in,1:max_stn)=0
    
      do i=1,nobs
        iobs=iskrec(i)
        if(iobs .gt. iobs_in) goto 100          !all done
!        write(*,'("Ob ",i3, 20f8.2)') iobs, eleva(iobs,1:nstatn)
        if(kobc(iobs)) then
          avg_pix_obs(iobs)=0
          nsta=0
          do istn=1,nstatn           
            if(eleva(iobs,istn) .ne. -99.d0) then             
               nsta=nsta+1
               call sphere_pix(azimu(iobs,istn),eleva(iobs,istn),
     >            num_pix_bands, ipix_bands,dang_pix_band,
     >            isphere_pix,ierr)
              if(ierr .ne.  0) then
                 write(*,*) "Sky_cov_pix: Should never get here!"
                 stop
              endif
              if(.not.kfound_pix(istn,isphere_pix)) then
                 kfound_pix(istn,isphere_pix)=.true.
                 inum_pix_obs(iobs,istn)=inum_pix_obs(iobs,istn)+1
                 avg_pix_obs(iobs)=avg_pix_obs(iobs)+1
              endif
            endif
          end do
          if(nsta .gt. 0) avg_pix_obs(iobs)=avg_pix_obs(iobs)/nsta
        endif
       end do

! Exit from loop
100   continue

      
      end
