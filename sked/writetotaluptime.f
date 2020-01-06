      subroutine WriteTotalUpTime(isor,nsrc,iStnAll,NumAll)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'

! Passed
      integer isor(*)
      Integer nsrc
      integer iStnAll(*)
      Integer NumAll
! local
      integer i
      integer isrc

      Double Precision UpTime(Max_sor)
      Double Precision UpSum

      call FindTotalUpTime(istnAll,NumAll,isor,Nsrc,itimeup,
     >  Max_sor, Uptime,UpSum)


      open(1,file="UpSource.out")
      write(1,*) "  #  Source            %      ObsDays"

      do i=1,nsrc
        isrc=isor(i)
        write(1,'(i4,1x,a8,1x,5f12.3)') isrc, cSORNA(isrc),
     >       UpTime(isrc)/UpSum*100.d0, UpTime(isrc)/1440.d0
      end do
      close(1)
      return
      end

