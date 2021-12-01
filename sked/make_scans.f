      subroutine make_scans(istat_xref,NumStat,isrc_xref,NumSrc,
     >  NumSubNet,MinSubNetSize)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
C passed
      integer NumStat           !Number of stations
      integer NumSrc
      integer istat_xref(max_stn)       !vector containing station crossreference
      integer isrc_xref(max_sor)        !Vector containing source crossreference
      integer MinSubNetSize
      integer NumSubNet                 !maximum number of subnets

C local variables
      integer istat                	!counter
      integer isrc1,isrc2,isrc3,isrc4 	!Src counter
      integer isrc5,isrc6,isrc7,isrc8 	!Src counter
      integer isrc9,isrc10
! Can have upto 4 subnets.
      integer isrc_num(NumSubNet)           	!Src number
      integer isub_xref(Max_stn,NumSubNet)	!stations in a particular subnet
      integer iSubNetSize(NumSubNet)          	!Number of stations in subnet
      integer isubnet

      logical kproceed
      logical kstatFree0(max_stn),KstatFree(max_stn)    !stations free?
      integer NumStatFree,NumStatFree0                  !number of stations free.
      integer itemp

! if you make this true, do the same in testcon.

      kStatFree=.False.               !Mark all stations as busy.  Some will be marked free below.
      kStatFree0=.false. 
      kproceed =.false.
! Go through quickly and see how many stations can see some source.
      do isrc1=1,NumSrc
        itemp=0
        do istat=1,NumStat                                 !Compute number of stations that can see this source.
           if(kvs(isrc_xref(isrc1),istat_xref(istat))) then
              itemp=itemp+1
           endif
        end do
        if(itemp .ge. MinSubNetSize) then
           kproceed=.true.
           do istat=1,NumStat
             kstatFree0(istat_xref(istat))=.true.           !Stations are only available if can participate in minimum subnet.
           end do
        endif
      end do

      if(.not.kproceed) then
         if(iverbose_level .ge. 1) then 
          write(luscn,*)
     >      "Make_scans: Not enough observing stats to proceed"
          endif 
         return
      endif

      NumStatFree0=0
      do istat=1,NumStat
        if(kstatFree0(istat_xref(istat))) NumStatFree0=NumStatFree0+1
      end do

! SUBNET1
      do isrc1=1,NumSrc                !loop over all sources.
! Initialize for first source of scan.
        NumStatFree=NumStatFree0
        kstatFree(1:Max_stn)=kstatFree0(1:Max_stn)
        isubnet=1
        isrc_num(isubnet)=isrc_xref(isrc1)
        call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >    NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >    MinSubNetSize)
! See if done.
        if(NumStatFree .lt. MinSubNetSize .or.
     >     iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >     NumSubNet .lt. 2) goto 100

! SUBNET2
        do isrc2=1,NumSrc
          isubnet=2
          isrc_num(isubnet)=isrc_xref(isrc2)
          call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >      NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >      MinSubNetSize)

          if(NumStatFree .lt. MinSubNetSize .or.
     >       iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >       NumSubNet .lt. 3) goto 200

! SUBNET3
          do isrc3=1,NumSrc
            isubnet=3
            isrc_num(isubnet)=isrc_xref(isrc3)
            call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >         NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >         MinSubNetSize)
! See if done.
            if(NumStatFree .lt. MinSubNetSize .or.
     >         iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >         NumSubNet .lt. 4) goto 300
! SUBNET4
            do isrc4=1,NumSrc
              isubnet=4
              isrc_num(isubnet)=isrc_xref(isrc4)
              call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >           NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >           MinSubNetSize)
! See if done.
              if(NumStatFree .lt. MinSubNetSize .or.
     >           iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >           NumSubNet .lt. 5) goto 400
! SUBNET5
              do isrc5=1,NumSrc
                isubnet=5
                isrc_num(isubnet)=isrc_xref(isrc5)
                call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >             NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >             MinSubNetSize)
! See if done.
                if(NumStatFree .lt. MinSubNetSize .or.
     >            iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >            NumSubNet .lt. 6) goto 500
! SUBNET6
                do isrc6=1,NumSrc
                  isubnet=6
                  isrc_num(isubnet)=isrc_xref(isrc6)
                  call make_and_test_scan(istat_xref,NumStat,kStatFree,
     >              NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >              MinSubNetSize)
! See if done.
                  if(NumStatFree .lt. MinSubNetSize .or.
     >              iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >              NumSubNet .lt. 7) goto 600
! SUBNET7
                  do isrc7=1,NumSrc
                    isubnet=7
                    isrc_num(isubnet)=isrc_xref(isrc7)
                    call make_and_test_scan(istat_xref,NumStat,
     >                kStatFree,NumStatFree, isrc_num,isub_xref,
     >                isubNetSize,isubnet,MinSubNetSize)
! See if done.
                    if(NumStatFree .lt. MinSubNetSize .or.
     >                iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >                NumSubNet .lt. 8) goto 700
! SUBNET8
                    do isrc8=1,NumSrc
                      isubnet=8
                      isrc_num(isubnet)=isrc_xref(isrc8)
                      call make_and_test_scan(istat_xref,NumStat,
     >                  kStatFree,NumStatFree, isrc_num,isub_xref,
     >                  isubNetSize,isubnet,MinSubNetSize)
                      if(NumStatFree .lt. MinSubNetSize .or.
     >                  iSubNetSize(isubnet) .lt. MinSubNetSize .or.
     >                  NumSubNet .lt. 9) goto 800
! SUBNET9
                      do isrc9=1,NumSrc
                        isubnet=9
                        isrc_num(isubnet)=isrc_xref(isrc9)
                        call make_and_test_scan(istat_xref,NumStat,
     >                    kStatFree,NumStatFree, isrc_num,isub_xref,
     >                    isubNetSize,isubnet,MinSubNetSize)
                        if(NumStatFree .lt. MinSubNetSize .or.
     >                    iSubNetSize(isubnet) .lt. MinSubNetSize.or.
     >                    NumSubNet .lt. 10) goto 900
! SUBNET9
                        do isrc10=1,NumSrc
                           isubnet=10
                           isrc_num(isubnet)=isrc_xref(isrc10)
                           call make_and_test_scan(istat_xref,NumStat,
     >                       kStatFree,NumStatFree, isrc_num,isub_xref,
     >                       isubNetSize,isubnet,MinSubNetSize)
                           if(NumStatFree .lt. MinSubNetSize .or.
     >                       iSubNetSize(isubnet) .lt. MinSubNetSize.or.
     >                       NumSubNet .lt. 11) goto 1000
1000                    end do
900                   end do
800                 end do
700               end do
600             end do
500           end do
400         end do
300       end do
200     end do
100   end do         !Loop over isrc

      if(kdissub)  close(14)
      end
! *******************************************************************************************
      subroutine make_and_test_scan(istat_xref,NumStat,kStatFree,
     >   NumStatFree, isrc_num,isub_xref,isubNetSize,isubnet,
     >   MinSubNetSize)

! make a scan using isrc_num and test it.
! Include blocks
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
! passed
      integer istat_Xref(*)             !possible stations
      integer NumStat                   !stations in list
      logical kstatFree(max_stn)        !True if station is free
      integer NumStatFree               !number of free stations. (Changed)
      integer isrc_num(*)               !source number.

      integer isub_xref(max_stn,*)      !Stations in subnet  (returned)
      integer isubNetsize(*)            !size of subnet      (returned)
      integer isubnet                   !which subnet.
      integer MinSubNetSize             !smallest subnet allowed to schedule

! local
      integer istat                     !counter
      integer istat_num
      integer itemp

      itemp=0
      do istat=1,NumStat
        istat_num=istat_xref(istat)
        if(kvs(isrc_num(isubnet),istat_num) .and.
     >      kstatFree(istat_num)) then
           itemp=itemp+1
           iSub_xref(itemp,isubnet)=istat_num
        endif
      end do
      iSubNetSize(isubnet)=itemp
      if(iSubNetSize(isubnet).lt.MinSubNetSize) return

      call schedule_scan(isrc_num(isubnet), isub_xref(1,isubnet),
     >  iSubNetSize(isubnet),  MinSubNetSize,isubnet)
      if(iSubNetSize(isubnet).lt.MinSubNetSize) return

      do istat=1,iSubNetSize(isubnet)
        kstatfree(isub_xref(istat,isubnet))=.false.
      end do

      if(.false.) then
      write(*,*) "subxref", isub_xref(1:isubnetsize(isubnet),isubnet)
      write(*,'("Clearing ",$)')
      do istat=1,iSubNetSize(isubnet)
!        write(*,'(a2,"-",$)') cpocod(isub_xref(istat,isubnet))
      end do
!      write(*,*) " "
      endif

      NumStatFree=NumStatFree-iSubNetSize(isubnet)
! Check this configuration w/o subnetting.
      call testcon(isrc_num,isub_xref,iSubNetSize,isubnet)
      return
      end
! **********************************************************************************
      subroutine schedule_scan(isrc_num, isub_xref,iSubNetSize,
     >  MinSubNetSize, iSubNet)
! Try to generate the scan using source isrc_num and subnet specified by isub_xref.
! On exit:
!   isub_xref    =stations succefully scheduled
!   iSubNetSize  =# of stations used.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'covar.ftni'

! History
!   2008Jun17 JMGipson.

! Passed
      integer isrc_num            	!source we want to schedule
      integer isub_xref(*)        	!Cross reference
      integer iSubNetSize               !# stations in subnet
      integer MinSubNetSize             !Mininum number of stations.
      integer isubnet                   !which subnet: 1,2,3,4

! Local
      integer istat
      integer isub_xref2(max_stn)    !returned form Newcm2. Contains stats in obs.
      integer iSubNetSize2
      integer ilen                     !size of buffer (returned by pakup)

      character*200 cscan_cmd           !Scan command string
      integer iptr                      !Used in writing this string.
! how many tries do we do?
      integer itry
      Integer MaxTry

! Initialize these arrays to passed arrays

! We try to schedule this source MaxTry times, each time reducing participating stations.
      Maxtry=3
      do itry=1,MaxTry
        isub_xref2(1:iSubNetSize)=isub_xref(1:iSubNetSize)  !initialize
        iSubNetSize2=iSubNetSize
! Generate the command to try to schedule a source with a subnet, and start with first station.
        write(cscan_cmd,'(i3," SUB ")') isrc_num
        iptr=9    !this points to where we start writing the subnet.
        do istat=1,iSubNetSize2
          cscan_cmd(iptr:iptr+2)=cpocod(isub_xref2(istat))//"-"
          iptr=iptr+3
        end do
        cscan_cmd(iptr-1:iptr-1)=" "                !get rid of trailing "-"
! At this point should have a string that looks like:
!           write(luscn,'(i4," | ",a)') isubnet, cscan_cmd(1:40)
!    123 SUB Aa-Bb-Cc-Dd              etc. where

        CALL NEWOB (cscan_Cmd,isub_xref2,iSubNetSize2,IERRCM,isubnet)
        IF  (IERRCM.NE.0) THEN  !
           IF (IERRCM.GT.1) CALL WRERR(IERRCM,INUMCM)
           iSubNetSize=0
           RETURN
        END IF  !
        call pakup(ilen,isubnet)
        ctrial_scan(isubnet)=cbuf

! Extract stations that were sucessfully scheduled.
        iSubNetSize=0
        do istat=1,iSubNetSize2
          if(isub_xref2(istat) .gt. 0) then
             iSubNetSize=iSubNetSize+1
             isub_xref(iSubNetSize)=isub_xref2(istat)
           endif
        end do
! Fewer stations than minimum exit.
        if(iSubNetSize .lt.MinSubNetSize) return
        if(iSubNetSize .eq. iSubNetSize2) then
          if(.false.) then
          write(cscan_cmd,'(i3," SUB ")')  isrc_num
          iptr=9    !this points to where we start writing the subnet.
          do istat=1,iSubNetSize2
            cscan_cmd(iptr:iptr+2)=cpocod(isub_xref2(istat))//"-"
            iptr=iptr+3
          end do
! Space in the subscans 
          do istat=1,isubnet-1               
            write(ludsp,'("  ",$)')
          end do
          write(ludsp,'(i4,1x,a," | ",a)')
     > isubnet,csorna(isrc_num), cscan_cmd(1:iptr)
          endif
          return  !no bad stations
        endif
      enddo

      return
      end

