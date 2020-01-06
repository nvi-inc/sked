      subroutine subcon(nst,istn,ikey)
CHS-------------------------------------------------------------
CHS General purpose
CHS Subcon was created in order to have an overview of the
CHS best subconfigurations due to the parameters to be optimized.
CHS The maximum number of displayed configurations is 15.
C 930930 nrv Use index array to pick entries from kchart
C 931012 nrv Add printout of sum of parameter improvements.
C            Remove index array. Assume kchart is ordered already.
C            Remove printing of tape waste numbers.
C 2003Oct16 JMGipson. Removed itwas parameter from call list, since no longer used.
! 2007Jan05 JMGipson. Cleaned up a bit.
!           Modified to use sort key=ikey
!2011May03  Put in some changes requested by Arno. 
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'major.ftni'
      include 'covar.ftni'
C
C INPUT:
      integer nst,istn(*)
      integer ikey(*)
! Function    
      integer isrc
      integer istat

C LOCAL:
      integer NumDisplay           !number to display.
      integer NumPerRow
      integer icnt
      integer ibeg
      integer j
      integer k
      integer max_sub
      integer kerr
      character*200 ldum
      integer istart,iend,iwidth
      integer iscan
      logical ksubnet
C
      NumDisplay=min(NumTrial,15)  !number to display.

      goto 100
      ibeg=0
      do while(ibeg .lt. NumDisplay)
        write(ludsp,'(110("="))')
        NumPerRow=Min(NumDisplay-ibeg,5)
! Find maximum number of subnets.
        max_sub=0
        do j=ibeg+1, ibeg+NumPerRow
          max_sub=max(max_sub,nsub_trial_vec(ikey(j)))
        end do
        do j=ibeg+1, ibeg+NumPerRow
          do k=nsub_trial_vec(ikey(j))+1,max_sub
             ctrial_vec(k,ikey(j))=" "
          end do
        end do

        do j=1,max_sub
        write(ludsp,'("Source   Dur: |",5(a,3x,a,3x,"|"))')
     >  (ctrial_vec(j,ikey(ibeg+icnt))(1:8),
     >   ctrial_vec(j,ikey(ibeg+icnt))(39:42),
     >     icnt=1,NumPerRow)
        write(ludsp,'("Stations:     |",5(a16,2x,"|"))')
     >    (ctrial_vec(j,ikey(ibeg+icnt))(63:78), icnt=1,NumPerRow)
        write(ludsp,'("Start time:   | ",5(a12,"     | "))')
     >    (ctrial_vec(j,ikey(ibeg+icnt))(24:36), icnt=1,NumPerRow)
        end do
        ibeg=ibeg+5

 
!        write(ludsp,'(110("-"))')
      enddo
      writE(*,*) " " 
  

100   continue      
       ksubnet=.false.
       do iscan=1,NumDisplay
          if(nsub_trial_vec(ikey(iscan)) .gt. 1) then
             ksubnet=.true.
           endif
       end do 
       if(ksubnet) then
         write(*,*) "Best scans (may include subnetting):"
       else
         write(*,*) "Best candidate scans: "
       endif 
       write(*,*) "                  Source                   " 
       write(*,*) "       Start      #  Name      Dur  Stations"
       
        

       iwidth=4+1+13+1+3+1+8+1+2*nstatn+3
       do iscan=1,NumDisplay
          write(ldum,'(i4,1x," | ")') iscan 
          do j=1,nsub_trial_vec(ikey(iscan))
            cbuf=ctrial_vec(j,ikey(iscan)) 
            call unpak(kerr,j)
            istart=(j-1)*iwidth+9
            iend=istart+iwidth-1
            isrc=nsortst(isttst(1))
           write(ldum(istart:iend),
     >     '(a,1x,i3,1x,a8,1x,a,2x,40a2)')  
     >        ctrial_vec(j,ikey(iscan))(24:36), isrc, csorna(isrc), 
     >        ctrial_vec(j,ikey(iscan))(39:42),
     >        (cpocod(isttst(k)),k=1,nstntst)
            ldum(iend-1:iend-1)="|"
          end do 
          writE(ludsp,'(a)') trim(ldum)
        end do            
     


      return
      end
