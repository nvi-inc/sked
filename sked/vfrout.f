      SUBROUTINE vfrout
C
C  This routine writes out the VEX $FREQ section.
C
C   HISTORY:
C 990611 nrv New. 
C 990921 nrv Use VEX utilities.
C 991130 nrv Include LSB channels too.
C 000522 nrv Change "Ch" to "CH" for consistency in ref names with
C            SCHED VEX files.
C 020815 nrv If the channel is being recorded on head 2, it was being ignored.
! 2010.06.16 JMG Leave spaces between names when writing to screen.
! 2014May07  Very minor change so that output conforms with vmmout. 
! 2018Oct10  JMG. Modified to handle VDIF format correctly. 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'

! function
      integer itras
C
C     CALLED BY:   SKCLS
C
C  LOCAL

      real fr
      integer igig,ib,ic,ich,ist(max_stn),npx,ipr(max_stn),ipx
      integer itype,il,isp,ichan,isb
      integer ptr_ch,trimlen
      character*28 cbandx,cfr,csb,cbw,ctr,cbb,cpc,csamp
      integer ind

! These are used to figure out the order to write things out. Introduced to handle VDIF format
      integer num_band 
      integer iband_beg(4), iband_end(4)   !beginning and end of band...
      character*28 cbandx_old


C  1. FREQ
      call fcreate_block(ptr_ch("FREQ"//char(0)))
      write(luscn,'("FREQ: ")')

C  2. Write each chan_def line.

      itype=1 ! FREQ, BBC, IFD, TRACKS, HEAD_POS, PASS_ORDER
      do ic=1,ncodes ! codes
        call getist(ic,itype,ist,ipr,npx)
        do ipx=1,npx ! each group
          isp=ipr(ipx) ! station index to use to write out this group
C def
          call fcreate_def(ptr_ch(refdef_name(itype,isp,ic)))
          il=trimlen(refdef_name(itype,isp,ic))
!          write(luscn,'(a," " ,$)') refdef_name(itype,isp,ic)(1:il)
C chan_def

! Default is just to go throught the channels, outputing Upper, then Lower sidebands (if lower recorded).
! So looks like this:
!  X U XL  XU XU ... XU XL  SU  SU  SL  ....
! However, for VDIF the order is different. Within each band order by frequency.
!  Then output upper band and then lower band.
! So looks something like this  X U  X U  ... X U  X L .. XL   SU SU   ... SL

! Because of this we have to do things a little more complicated.

! First find out if VDIF.
         ind=index(refdef_name(itype,isp,ic),"VDIF")

! Not VDIF.  Do default order. 
! *******Do in standard order*****************************************
        if(ind .eq. 0) then 
          ichan=0 ! channel counter
          do ib=1,nchan(isp,ic)
            do isb=1,2 ! one chandef for each sideband channel
              ich = invcx(ib,isp,ic)
C itras(sideband,bit,head,channel,subpass,station,code)
              if (itras(isb,1,1,ich,1,isp,ic).ne.-99 .or.
     .            itras(isb,1,2,ich,1,isp,ic).ne.-99) then ! this sideband
C band
              cbandx=csubvc(ich,isp,ic)(1:1)
              call null_term(cbandx)
C RF freq
              fr = freqrf(ich,isp,ic)
              if (freqrf(ich,isp,ic).gt.100000.d0) then
                igig=freqlo(ich,isp,ic)/100000.d0
                fr=freqrf(ich,isp,ic)-igig*100000.d0
              endif
              write(cfr,'(f8.2)') fr
              call null_term(cfr)
C net SB
              csb=cnetsb(ich,isp,ic)(1:1)
              if (csb.eq.'U'.and.isb.eq.2) csb='L'
              call null_term(csb)
C chan bw
              write(cbw,'(f6.3)') vcband(ich,isp,ic)
              call null_term(cbw)
C Track ID
              ichan=ichan+1
              write(ctr,'("CH",i2.2)') ichan
              call null_term(ctr)
C BBC ID
              write(cbb,'("BBC",i2.2)') ibbcx(ich,isp,ic)
              call null_term(cbb)
C phase cal ID
              cpc=cnetsb(ich,isp,ic)(1:1)//'_cal'
              call null_term(cpc)
C write chan_def
              call fcreate_chan_def(ptr_ch(cbandx),
     .           ptr_ch(cfr),ptr_ch('MHz'//char(0)),
     .           ptr_ch(csb),ptr_ch(cbw),ptr_ch('MHz'//char(0)),
     .           ptr_ch(ctr),ptr_ch(cbb),ptr_ch(cpc))
              call fcreate_chan_def_states(ptr_ch(char(0)))
              endif ! this sideband
            enddo ! two sidebands
          enddo ! nchan
         else
! ********* VDIF order

! First task is to find out where the differnent bands stop and end. 
          num_band=1
          iband_beg(1)=1
          do ib=1,nchan(isp,ic)
            ich=invcx(ib,isp,ic)
            cbandx=csubvc(ich,isp,ic)(1:1) 
            if(ib .gt. 1) then
              if(cbandx_old .eq. cbandx) then
                iband_end(num_band)=ib
               else
                iband_end(num_band)=ib-1
                num_band=num_band+1
                iband_beg(num_band)=ib
               endif
            endif
            cbandx_old=cbandx
          end do
!          write(*,*) iband_beg(1:num_band)
!          write(*,*) iband_end(1:num_band)
!          stop 
! At this stage should know where the different bands are...

          ichan=0
          do iband=1,num_band
             do isb=1,2
             do ib=iband_beg(iband),iband_end(iband)
              ich = invcx(ib,isp,ic)
C itras(sideband,bit,head,channel,subpass,station,code)
              if (itras(isb,1,1,ich,1,isp,ic).ne.-99 .or.
     .            itras(isb,1,2,ich,1,isp,ic).ne.-99) then ! 
C band
              cbandx=csubvc(ich,isp,ic)(1:1)
              call null_term(cbandx)
C RF freq
              fr = freqrf(ich,isp,ic)
              if (freqrf(ich,isp,ic).gt.100000.d0) then
                igig=freqlo(ich,isp,ic)/100000.d0
                fr=freqrf(ich,isp,ic)-igig*100000.d0
              endif
              write(cfr,'(f8.2)') fr
              call null_term(cfr)
C net SB
              csb=cnetsb(ich,isp,ic)(1:1)
              if (csb.eq.'U'.and.isb.eq.2) csb='L'
              call null_term(csb)
C chan bw
              write(cbw,'(f6.3)') vcband(ich,isp,ic)
              call null_term(cbw)
C Track ID
              ichan=ichan+1
              write(ctr,'("CH",i2.2)') ichan
              call null_term(ctr)
C BBC ID
              write(cbb,'("BBC",i2.2)') ibbcx(ich,isp,ic)
              call null_term(cbb)
C phase cal ID
              cpc=cnetsb(ich,isp,ic)(1:1)//'_cal'
              call null_term(cpc)
C write chan_def
              call fcreate_chan_def(ptr_ch(cbandx),
     .           ptr_ch(cfr),ptr_ch('MHz'//char(0)),
     .           ptr_ch(csb),ptr_ch(cbw),ptr_ch('MHz'//char(0)),
     .           ptr_ch(ctr),ptr_ch(cbb),ptr_ch(cpc))
              call fcreate_chan_def_states(ptr_ch(char(0)))
              endif ! 
            enddo    ! ib
            end do   ! isb 
          enddo     ! iband
        endif       ! VDIF 

C sample rate
          write(csamp,'(f4.1)') samprate(isp,ic)
          call null_term(csamp)
          call fcreate_sample_rate(ptr_ch(csamp),
     >              ptr_ch('Ms/sec'//char(0)))
        enddo ! each group
      enddo ! codes






!      write(luscn,'()')
      RETURN
      END
