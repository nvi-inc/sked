      SUBROUTINE vpoout 
C
C  This routine writes out the VEX $PASS_ORDER section.
C
C   HISTORY:
C 990611 nrv New. 
C 990922 nrv Use VEX utilities.
! 2010.06.16 JMG Leave spaces between names
!  2014May07  Very minor change so that output conforms with vmmout. 

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer isub,nh,nch,ihd(max_pass)
      character*36 cpasslist
      data cpasslist/'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'/
      integer ic,ist(max_stn),npx,ipr(max_stn),ipx
      integer itype,il,isp,ih,ip
      integer ptr_ch,trimlen,ichmv_ch,ib2as
      character*28 cp
      logical kpass_found

C  1. HEAD_POS

      call fcreate_block(ptr_ch("PASS_ORDER"//char(0)))
      write(luscn,'("PASS_ORDER: ")')

C  2. Write each headstack_pos line.

      itype=6 ! FREQ, BBC, IFD, TRACKS, HEAD_POS, PASS_ORDER
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)
C pass_order
            nh=0          
            kpass_found =.false. 
            do ip=1,max_pass
              if (ihddir(1,ip,isp,ic).ne.0) then ! valid subpass
                if (ihddir(1,ip,isp,ic).eq.1) then ! first subpass
                  nh=nh+1
                  ihd(nh)=ihdpos(1,ip,isp,ic) ! save the head position
                  ih=nh
                else ! find the index
                  ih=1
                  do while (ih.le.nh.and.ihdpos(1,ip,isp,ic).ne.ihd(ih))
                    ih=ih+1
                  enddo
                endif
                NCH = ib2as(ih,IBUF,1,3)
                isub=ihddir(1,ip,isp,ic)
                nch = ichmv_ch(ibuf,nch+1,cpasslist(isub:isub))
                call hol2char(ibuf,1,nch-1,cp)
                call null_term(cp)
                call fcreate_pass_order(ptr_ch(cp))
                kpass_found=.true. 
              endif ! valid subpass
            enddo
            if(kpass_found) then 
               call fcreate_pass_order(ptr_ch(char(0))) 
            endif 
        enddo ! each group
      enddo ! codes

!      write(luscn,'()')
      RETURN
      END
