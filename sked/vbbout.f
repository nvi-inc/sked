      SUBROUTINE vbbout 
C
C  This routine writes out the VEX $BBC section.
C
C   HISTORY:
C 990611 nrv New. 
C 990922 nrv Change to using VEX writing utilities.
C
!  2004Oct26 Removed "fcreate_comment"
!  2006Nov07 JMG.  Allowed BBC#s not to be the same as Channel #.
!                  Removed all hollerith.
!  2010.06.16 Leave spaces between names
!  2010.06.16 Swapped order of physical and logical BBC
!  2010.11.04 Unswapped. 
!  2011.12.03 3rd times the charm? Now BBC# is same as BBC identifier, which is different than channel number
!  2012.09.17 4th time.  Now for BBCxx   xx is the channel number. 
!  2014May07  Very minor change so that output conforms with vmmout. 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL

      character*128 cbbid,cbb,cbbif
      integer itype,ib,ic,ipx,isp,npx,ist(max_stn),ipr(max_stn)
      integer il,ich,is
      integer trimlen,ptr_ch     

C  1. BBC
      call fcreate_block(ptr_ch("BBC"//char(0)))
      write(luscn,'("BBC: ")')

C  2. Write each BBC_assign line.

      itype=2 ! BBC type
      do is=1,nstatn
        ist(is)=0
      enddo
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def 
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)
C BBC_assign
          do ib=1,nchan(isp,ic)
            ich = invcx(ib,isp,ic)
C BBC ID, physical number: 
            write(cbbid,'("BBC",i2.2)') ich
            write(cbb,'(i2.2)') ibbcx(ich,isp,ic) 
! Label and BBC have same value.
!            write(cbbid,'("BBC",i2.2)') ibbcx(ich,isp,ic)        
!            write(cbb,'(i2.2)') ich 
            call null_term(cbbid)
            call null_term(cbb)

C IF ID
            cbbif="IF_"//cifinp(ich,isp,ic)
            call null_term(cbbif)
C write the line now 
!            write(*,*) "FEE"
            call fcreate_bbc_assign(ptr_ch(cbbid),ptr_ch(cbb),
     .      ptr_ch(cbbif))
!            write(*,*) "FI"
          enddo
        enddo ! each group
      enddo ! codes
!      write(luscn,'()')
!      write(*,*) "HI HI~"

      RETURN
      END
