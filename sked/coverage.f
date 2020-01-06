      subroutine coverage(isrc_num,istn_vec,nstn,NumSubNet,distsum)
CHS--------------------------------------------------------------------
CHS General purpose
CHS Coverage was created in order to evaluate a subconfiguration
CHS due to a sky coverage optimization criterion. 
C
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/constants.ftni' 

! 2002Nov11 JMG A little cleanup.
! 2007OCt05 JMG totally different algorithm.
! 2008Jun18 JMG Modified to take into account all of the subnets.
! 2010Jan25 JMG Changed index order of eleva, azimu
!               Also changed compuation of angle to make a little clear.

C INPUT:
      integer isrc_num(*)           !
      integer istn_vec(max_stn,*)
      integer nstn(*)
      integer NumSubNet     
C
C OUTPUT:
      real*8 distsum
! functions
      real*8 dot8

C LOCAL:
      real*8 dist(max_stn),aa
      integer nsta,i,ii,j
      integer isub
      integer istn
      real*8 src_now(3),src_prv(3)
      real*8 dist_tmp     

!      distsum=sky_cov(isor)
!      return
      nsta=0
      distsum=0.d0
C
CHS--------------------------------------------------------------------
CHS if any of the previous observations is taken into account (kobc = true)
CHS for coverage computation, on each station the spherical
CHS distance between the possible new observation and each
CHS previous observation is computed. The smallest mean value is
CHS selected as a station representative. The average value of all the
CHS stations (distsum) is the optimization criterion for sky coverage.

! This is for debugging
      if(.false.) then
        do isub=1,NumSubNet     
          write(ludsp,'("SubNet ",i1," len ", i4)') isub,nstn(isub)
          do j=1,nstn(isub)
            istn=istn_vec(j,isub)
            write(ludsp,'(a8," ",$)') cpocod(istn)
          end do
          write(*,*) " "
          do j=1,nstn(isub)
            istn=istn_vec(j,isub)
            write(ludsp,'(f8.2," ",$)') elev(istn)*rad2deg
          end do
          write(*,*) " "
        end do
        pause
      endif
!      stop 
! End of debugging        
      do isub=1,NumSubNet
        dist=twopi            !Set angular distance to 2-pi.  This will get reduced later. 
        do j=1,nstn(isub)     !The large value ensures that unobserved  stations are upweighted       
          istn=istn_vec(j,isub)
          if(elev(istn) .eq. -99.d0) then
!             write(ludsp,*) "What a surprise!", cpocod(istn)
!             pause
          endif
          if(elev(istn).ne.-99.d0) then
             nsta=nsta+1
             call make_src_vector(azim(istn),elev(istn),src_now)
!            write(*,*) src_now
            do i=1,nobs
              ii=iskrec(i)
              if(kobc(ii) .and. (eleva(ii,istn).ne.-99.d0)) then
                call make_src_vector(azimu(ii,istn),eleva(ii,istn),
     >                                src_prv)                
!                write(*,*) src_prv
                aa=dot8(src_now,src_prv)   !find cosine of angle between two sources. 
!                write(*,*) aa, azimu(ii,istn),eleva(ii,istn)
!                pause
                if(abs(aa).ge.1.d0) then
                  dist_tmp=0.d0
                else
                  dist_tmp=dacos(aa)
                endif
                dist(j)=min(dist(j),dist_tmp)                !Minimum distance?
              endif ! eleva(istn(j),ii).ne.-99.d0
              distsum=distsum+dist(j)
            enddo ! i=1,nobs
            distsum = distsum+dist(j) 
          endif ! elev(istn(j)).ne.-99.d0
        enddo ! j=1,nst
      end do

      if(nsta.eq.0) then
         write(luscn,*) 'COVERAGE01 - nsta = 0 !!!'
         distsum=-1.d10
         return
      endif

      distsum=distsum/nsta

!
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      return
      end
