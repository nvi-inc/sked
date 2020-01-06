      SUBROUTINE vifout
C
C  This routine writes out the VEX $IF section.
C
C   HISTORY:
C 990611 nrv New. 
C 990922 nrv Use VEX writing utilities
C 991130 nrv For IF 3N substitute 3I for VEX reader in drudg.
! 2006Jun22 JMG.  More space in writing out IF frequency to avoid ****.
! 2011Dec02 JMG. Fixed problem with writing out IF assignments if lower sideband. 
! 2014May07  Very minor change so that output conforms with vmmout. 
! 2018Oct01 JMG. Modified so that it plays better with the VLBA correlator which likes comments at the end of the line. 
! 2019Jan30 JMG. Modified order of oscillators at end of VLBA IF lines.
! 2019Jan31 JMG. Fixed LO sideband.
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C     CALLED BY:   SKCLS
C
! functions

      integer ichcm_ch,ib2as
C  LOCAL

      real*4 fr
      character*12 cid,cif,clo,cpolx,csb
      integer ich,ic,il,i,ib,igig,i1,i2,i3,i4,ix
      integer nch,itype,ist(max_stn),ipr(max_stn),npx,isp,ipx
      integer ptr_ch,trimlen
      character*1 lchar
      real*8 rlo_min, rlo_max, rlo_mid,rlo_old 
      character*50 ldum  
      logical kvlBa_if  ! true if possible VLBA IF (named "A", "B", "C", "D"...) 
   

C  1. IF
      call fcreate_block(ptr_ch("IF"//char(0)))
      write(luscn,'("IF: ")') 
 
C  2. Write each IF_assign line.
      itype=3 ! FREQ, BBC, IF, TRACKS, HEAD_POS, PASS_ORDER
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a," ",$)') refdef_name(itype,isp,ic)(1:il)

C Find out which LOs are in use for this code and the first channel index.
            i1=0
            i2=0
            i3=0
            i4=0
            do ib=1,nchan(isp,ic)
              ich = invcx(ib,isp,ic)
              lchar=cifinp(ich,isp,ic)(1:1)
              if(i1.eq.0 .and. lchar.eq. '1'.or.lchar.eq.'A') i1=ich
              if(i2.eq.0 .and. lchar.eq. '2'.or.lchar.eq.'B') i2=ich
              if(i3.eq.0 .and. lchar.eq. '3'.or.lchar.eq.'C') i3=ich
              if(i4.eq.0 .and. lchar.eq. '4'.or.lchar.eq.'D') i4=ich
            enddo

! do some pre-checking if VLBA....
           if(kvlba_corr) then
             rlo_max=9600.d0 
             rlo_min=1e12
             rlo_mid=1e0
             do i=1,4  !up to 4 Los.    
                if (i.eq.1) ix=i1
                if (i.eq.2) ix=i2
                if (i.eq.3) ix=i3
                if (i.eq.4) ix=i4
                if (ix.ne.0) then ! this LO in use
                  fr = freqlo(ix,isp,ic)
                  if (freqlo(ix,isp,ic).gt.100000.d0) then
                    igig=freqlo(ix,isp,ic)/100000.d0
                    nch=nch+ib2as(igig,ibuf,nch,1)
                    fr=freqlo(ix,isp,ic)-igig*100000.d0
                  endif
!                write(*,*) fr
                  rlo_max=max(rlo_max,fr)
                  rlo_min=min(rlo_min,fr)
                  if(rlo_old .ne. rlo_max) then
                     rlo_mid=fr
                  endif
                endif 
              end do 
            endif 
         
!            write(*,*) rlo_min,rlo_mid,rlo_max
!            stop 

            do i=1,4 ! up to 4 LOs
              if (i.eq.1) ix=i1
              if (i.eq.2) ix=i2
              if (i.eq.3) ix=i3
              if (i.eq.4) ix=i4
              if (ix.ne.0) then ! this LO in use
C IF ID
                cid="IF_"//cifinp(ix,isp,ic)
                call null_term(cid)

                cif=cifinp(ix,isp,ic)
                kvlba_IF =cif .eq. "A" .or. cif .eq. "B" .or. 
     >                    cif .eq. "C" .or. cif .eq. "D"
                call null_term(cif)
C pol
                cpolx='R'//char(0)
C LO frequency
                fr = freqlo(ix,isp,ic)
                if (freqlo(ix,isp,ic).gt.100000.d0) then
                  igig=freqlo(ix,isp,ic)/100000.d0
                  nch=nch+ib2as(igig,ibuf,nch,1)
                  fr=freqlo(ix,isp,ic)-igig*100000.d0
                endif
                write(clo,'(f7.1)') fr
                call null_term(clo)

C sideband
                call hol2char(losb(ix,isp,ic),1,1,csb)       

           
                call null_term(csb)
C phase cal
C write the line now 
                call fcreate_if_def(ptr_ch(cid),ptr_ch(cif),
     .          ptr_ch(cpolx),ptr_ch(clo),ptr_ch('MHz'//char(0)),
     .          ptr_ch(csb),
     .          ptr_ch('1'//char(0)),ptr_ch('MHz'//char(0)),
     .          ptr_ch('0'//char(0)),ptr_ch('Hz'//char(0)))    

                if(kvlba_corr .and. kvlba_if) then
                  if(fr .le. 3100) then
                    write(ldum,'(2f10.2, " 13cm ", f10.2, " NA")')
     >                 rlo_mid,rlo_min,rlo_max
                  else
                    write(ldum,'(2f10.2, "  4cm ", f10.2, " NA")')
     >                 rlo_mid,rlo_min,rlo_max
                  endif
                  call vex_trailing_comment(ldum) 
                endif 
              endif ! this LO in use
            enddo
        enddo ! stations
      enddo ! codes

!      write(luscn,'()')
      RETURN
      END
