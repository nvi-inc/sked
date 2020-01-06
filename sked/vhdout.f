      SUBROUTINE vhdout 
C
C  This routine writes out the VEX $HEAD_POS section.
C
C   HISTORY:
C 990611 nrv New. 
C 990922 nrv Use VEX utilities.
C 020815 nrv Write positions for two headstacks on same output.
! 2010.06.16 Leave spaces between names
!  2014May07  Very minor change so that output conforms with vmmout. 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer ic,ist(max_stn),npx,ipr(max_stn),ipx
      integer itype,il,isp
      integer ih,ip,ip2
      logical kh(max_headstack)
      integer ptr_ch,trimlen
      character*28 cp,cpos

C  1. HEAD_POS

      call fcreate_block(ptr_ch("HEAD_POS"//char(0)))
      write(luscn,'("HEAD_POS: ")')

C  2. Write each headstack_pos line.

      itype=5 ! FREQ, BBC, IFD, TRACKS, HEAD_POS, PASS_ORDER
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)
C Count headstacks in use.
          do ih=1,max_headstack
            kh(ih)=.false.
            do ip=1,max_pass
              if (ihddir(ih,ip,isp,ic).ne.0) kh(ih)=.true.
            enddo
          enddo
C headstack_pos
          ip2=0
          do ip=1,max_pass
            if (ihddir(1,ip,isp,ic).eq.1) then ! position for subpass 1 only
              ip2=ip2+1
              write(cp,'(i2.0)') ip2
              call null_term(cp)
              call fcreate_headstack_reference(ptr_ch(cp))
              do ih=1,max_headstack
                if (kh(ih)) then ! this headstack
                  write(cpos,'(i5)') ihdpos(ih,ip,isp,ic)
                  call null_term(cpos)
                  call fcreate_headstack_pos(ptr_ch(cpos),
     .                      ptr_ch('um'//char(0)))
                endif ! this headstack
              enddo
              call fcreate_headstack_pos(ptr_ch(char(0)),
     .                  ptr_ch(char(0)))
            endif ! position for subpass 1 only
          enddo
        enddo ! each group
      enddo ! codes

!      write(luscn,'()')
      RETURN
      END
