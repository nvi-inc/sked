      subroutine rsini

C  Initialize the baseline and station rise/set arrays. Call this routine 
C  whenever sources or stations are re-selected. Both sources and stations
C  must have been selected. 

C  930715 NRV Created
C  930722 nrv Compute possibly multiple mutual time-up using IUTRIS, IUTSET
C  931020 nrv Add arrays for saving station rise/set times
C  931109 nrv Replaced fill-in of station rise/set arrays with a subroutine
C             that does it right.
C  940112 nrv Only do calculations for celestial sources. For satellites,
C             use the normal (old) stuff.
C  950405 nrv Use 2-letter codes for error message.
!  2008Oct03 JMG. Change in formating
!  2016Sep26 JMG. Minor changes. 

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/constants.ftni' 
C funcions
!      logical kup               !is a source up?

C LOCAL
      integer i,j,k,ib,istn(max_stn),iutris(max_stn),iutset(max_stn)
      integer mutris,mutset,im
      integer ibnum
!      logical kup1,kup2,kmut,kupp1,kupp2
      integer LineSegJ(2,2),LineSegK(2,2),numj,numk
      integer LineSeg(2),j2,k2
      integer intTime

C The result of this routine is the baseline arrays itris and itset, 
C ntrisset, and itimeup, and the station arrays itsris, itsset, and ntsrisset.
C itris(source,baseline,n) - the minute during the day when the source mutually
C                            rises on the baseline for the nth time
C itset(source,baseline,n) - the minute during the day when the source mutually
C                            sets on the baseline for the nth time
C ntrisset(source,baseline) - a count of how many rise/set entries are in the
C                             above two arrays
C itimeup(source,baseline) - the number of minutes that the source is mutually
C                            visible on the baseline
C itsris(source,station,n) - the minute during the day when the source rises
C                            at the station for the nth time
C itsset(source,station,n) - the minute during the day when the source sets
C                            at the station for the nth time
C ntsrisset(source,station) - a count of how many rise/set entries are in the
C                             above two arrays

      if (nstatn.eq.0.or.nsourc.eq.0) then
        write(luscn,*) ' RSINI99 - Select sources and stations first.'
        return
      endif
      if (mjdcur(1).lt.0) then
        write(luscn,*)' RSINI98 - Date not initialized for station 1.'
        return
      endif

      write(luscn,'(a)') "Calculating rise/set times:"

      do i=1,Max_sor
       do j=1,Max_baseline
         do k=1,Max_nrs
            itris(i,j,k)=0
            itset(i,j,k)=0
         end do
         itimeup(i,j)=0
         ntrisset(i,j)=0  !initialize.
        end do
      end do
!      itris=0
!      itset=0
!      itimeup=0
      do i=1,Max_stn
        iutris(i)=0
        iutset(i)=0
      end do

C     do i=1,nsourc
      do i=1,nceles
        write(luscn,"(i4,$)") i
        if(mod(i,20) .eq. 0) write(luscn,'()') 
        do j=1,nstatn
          istn(j)=j
        enddo
        call visss(i,nstatn,ISTN,IUTRIS,IUTSET,MUTRIS,MUTSET)
C *NOTE* visss only finds the first rise/set time. If the source goes behind
C an obstruction and rises again, it won't be found.

C* Replaced the following ...
C* Fill in the rise/set arrays for each station.
C*      do j=1,nstatn
C*        itsris(i,j,1) = iutris(j)  
C*        itsset(i,j,1) = iutset(j)  
C*        ntsrisset(i,j) = 1
C*      enddo
C* with ...
        do j=1,nstatn
          call allday(mjdcur(1),i,j)
        enddo

C  Fill in the mutual rise/set arrays for each baseline.
        do j=1,nstatn-1
           if(iutset(j) .eq. 0) goto 110  !source is not up. skip loop.
           call MakeLineSegs(iutris(j),iutset(j),LineSegJ,NumJ)
           do k=j+1,nstatn
             ib=ibnum(j,k)
             im=0
             if(iutset(k) .eq.0) goto 100  !source is not up. skip loop.
             call MakeLineSegs(iutris(k),iutset(k),LineSegK,NumK)
             do j2=1,NumJ
             do k2=1,NumK
                call FindSegOverlap(LineSegJ(1,j2),LineSegK(1,k2),
     >              LineSeg)
                intTime=LineSeg(2)-LineSeg(1)
                if(intTime .gt. 0) then
                  im=im+1
                  if (im.gt.3) then
                    write(luscn,9901) cpocod(j),cpocod(k)
9901                format('RSINI01 - WARNING: More than 3 rise/sets ',
     >              'on baseline ',a2,'-',a2)
                  else
                    itimeup(i,ib)=itimeup(i,ib)+intTime
                    itris(i,ib,im)=LineSeg(1)
                    itset(i,ib,im)=LineSeg(2)
                  endif
                endif
              end do
              end do
100           continue
              ntrisset(i,ib) = im
              if(ntrisset(i,ib) .eq. 4) then  !serious problems. Should never get here.
                write(*,*) " "
                write(*,*) "**************"
                write(*,*) i,j,k
                write(*,'("1st ",2i10)') iutris(j),iutset(j)
                write(*,'("2nd ",2i10)') iutris(k),iutset(k)
                do im=1,ntrisset(i,ib)
                  write(*,'("Lap ",2i6)') itris(i,ib,im),itset(i,ib,im)
                end do
                write(*,*) "**************"
            end if
            end do  !k loop
110     continue
        end do    !j loop
!         Determine up to 3 mutual rise/set times in case the stations
!         up-times overlap more than once.
!            do m=1,1440
!              if (iutris(j).lt.iutset(j)).and.
!     .        (m.ge.iutris(j).and.m.le.iutset(j))) kup1=.true.
!              if (iutris(j).gt.iutset(j).and.
!     .        (m.ge.iutris(j).or.m.le.iutset(j))) kup1=.true.
!              if (iutris(k).lt.iutset(k)).and.
!     .        (m.ge.iutris(k).and.m.le.iutset(k))) kup2=.true.
!              if (iutris(k).gt.iutset(k).and.
!     .        (m.ge.iutris(k).or.m.le.iutset(k))) kup2=.true.
!              kup1=kup(m,iutris(j),iutset(j))
!              kup2=kup(m,iutris(k),iutset(k))
!              if (kup1.and.kup2) then
!                kmut = .true.
!                itimeup(i,ib)=itimeup(i,ib)+1
!              else
!                kmut = .false.
!              endif
!              if (m.eq.1) then !initialize
!                if (kmut) itris(i,ib,1) = 1
!                if (.not.kmut) itset(i,ib,1) = 1
!              else if (kup1.ne.kupp1.or.kup2.ne.kupp2) then !a change
!                im=im+1
!                if (im.gt.3) then
!                  write(luscn,9901) lpocod(j),lpocod(k)
!9901              format('RSINI01 - WARNING: More than 3 rise/sets ',
!     .            'on baseline ',a2,'-',a2)
!                else
!                  if (kmut) then ! mutual setting
!                    itset(i,ib,im) = m
!                  else
!                    itris(i,ib,im) = m
!                  endif
!                  ntrisset(i,ib) = im
!                endif
!              endif !a change
!              kupp1=kup1
!              kupp2=kup2
!            enddo
!            if(im .ne. 0) then
!              write(*,*) i,j,k
!              write(*,*) itset(1,ib,1:im)
!              write(*,*) itris(1,ib,1:im)
!             endif
      enddo
      krsini = .true.
      write(luscn, *) '....done'

! for debugging
      if(.false.) then 
      do j=1,nstatn
      do i=1,nceles
        write(*,'(i3,1x,a,1x,a,1x,6f8.2)'),i,cstnna(j),csorna(i),
     &      tsris(i,j,1:ntsrisset(i,j))*1440.d0/twopi, 
     &      tsset(i,j,1:ntsrisset(i,j))*1440.d0/twopi
      end do
      end do 
      endif 



      return
      end
! ***************************************************************
      subroutine MakeLineSegs(iutris,iutset,LineSeg,NumJ)
      implicit none
! given rise and set times, either return 1 or two LineSegs.
      integer iutris
      integer iutset
      integer LineSeg(2,2)
      Integer NumJ

      if(iutris .lt. iutset) then  !case   |  [------]   |
         numj=1
        LineSeg(1,1)=iutris
         LineSeg(2,1)=iutset
      else                          !case   |---]    [----|
         numj=2                     !treat as two LineSegs.
         LineSeg(1,1)=0
         LineSeg(2,1)=iutset
         LineSeg(1,2)=iutris
         LineSeg(2,2)=1440
      endif
      return
      end
