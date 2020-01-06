      SUBROUTINE write_vs(isrcvec,NsrcUse,istnsub,NumSub) 
C   COMMON BLOCKS USED
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni'
      integer isrcvec(*)
      integer istnsub(*)
      integer NsrcUse, NumSub
      integer isrc,istn,i,j


      character*2 cVSMap(max_Stn)  !Holds map of station visibility

      write(ludsp,'(a)') "Visibility"
      write(ludsp,'("Source   ",34a2)')
     >      " |", (cpocod(istnsub(j)),j=1,NumSub),"| "
      do i=1,NsrcUse
        isrc=isrcvec(i)
        cVsMap=" "
        do j=1,NumSub
          istn=istnsub(j)
          if(kvs(isrc,istn)) CvsMap(j)="X"
        end do
        write(ludsp,'(a8,1x,34a2)') csorna(isrc)," |",
     >           (Cvsmap(j),j=1,NumSub), "| "
      end do
      return
      end

