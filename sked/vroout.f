      SUBROUTINE vroout
C
C  This routine writes out the VEX $ROLL section.
C
C   HISTORY:
C 990929 nrv New. Write out 'off' as default.
C 000317 nrv Write out the standard tables from ARW's VEX
C            document for VLBA/8 and VLBA/16.
C 000523 nrv Use roll_def calls to put each step onto each line.
C 001004 nrv Use new roll defs from R. Cappallo.
C 020113 nrv Use new common variables.
!  2014May07  Very minor change so that output conforms with vmmout. 
!
C Need to fix: this routine should write out only the roll
C groups actually in use over all frequencies and stations.
C This should also be done for head positions and fanouts,
C but those are tied to mode names and so the vex file is
C correct if over-repetitive. nrv 020327
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'

! function
      integer iroll_def
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer ic,ist(max_stn),npx,ipr(max_stn),ipx
      integer istep,idef,itype,il,isp
      integer ptr_ch,trimlen
      character*128 croll,cr

C  1. ROLL

      call fcreate_block(ptr_ch("ROLL"//char(0)))
      write(luscn,'("ROLL: ")')

C  2. Write each roll_def line.

      itype=7 ! FREQ, BBC, IFD, TRACKS, HEAD_POS, PASS_ORDER, ROLL
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a,1x,$)') refdef_name(itype,isp,ic)(1:il)
C roll=off or roll_def
          cr=cbarrel(isp,ic)
          if (cr.eq.'    '.or.cr.eq.'NONE'.or.
     .        cr.eq.'off ') then ! no roll
            call fcreate_roll(ptr_ch('off'//char(0)))
          else ! roll table
C    Write the roll statements
            call fcreate_roll(ptr_ch('on'//char(0)))
            write(croll,'(i1)') iroll_inc_period(isp,ic)
            call null_term(croll)
            call fcreate_roll_inc_period(ptr_ch(croll))
            write(croll,'(i1)') iroll_reinit_period(isp,ic)
            call null_term(croll)
            call fcreate_roll_reinit_period(ptr_ch(croll),
     .             ptr_ch('sec'//char(0)))
C Don't write a separate set for each headstack. This is embedded
C in the arrays already.
            do idef=1,nrolldefs(isp,ic) ! each roll_def statement
              do istep=1,2+nrollsteps(isp,ic) ! head,home,tracks
                write(croll,'(i3.3)') iroll_def(istep,idef,isp,ic)
                call null_term(croll)
                call fcreate_roll_def(ptr_ch(croll)) 
              enddo ! each step
              call fcreate_roll_def(ptr_ch(char(0))) ! end this line
            enddo ! each roll_def statement
          endif ! roll or not
        enddo ! each group
      enddo ! codes

!      write(luscn,'()')
      RETURN
      END
