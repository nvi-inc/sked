      subroutine printfr(is,ic,itype)
C
C PRINTFR prints the guts of the frequency information for one
C station and one code.

C History
C 960209 nrv New. Extracted from FRLIS.
C 960510 nrv Add ITYPE to call.
C 960725 nrv Calculate total bandwidth from number of recorded
C            tracks, not number of channels.
C 970219 nrv Add head index to itras, change indices and use max_subpass
C            instead of max_pass
C 000126 nrv Use ntrkn instead of trkn to compute total bandwidth.
C 000326 nrv Add S2, K4 speed display.
C 010119 nrv Forgot to print Mk3 speed.
! 2006Jun22 JMGipson.  Modified to use only freqs where freqrf>0
! 2013Jul02 JMGipson. Previously used trkn in calculated Rate, BW. This had correction for bit-sampling.
!                     Now use ntrakn, but use trkn for calculating effective # of channels. 
! 2013Oct31 JMGipson. Had TotRate and BandWidth reversed. Also changed #chan to #BBC
! 2015Sep22 JMGipson. Corrected printing of Bandwidth. Now accounts for number of bits!
! 2018Jul05 JMGipson. Previously did not accont for headstack in calculating number of bits. Now does

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'

C
C  Input
      integer is ! station index
      integer ic ! code index
      integer itype ! 1=mode only, 2=also LO setup
      integer npass,ntrks,nhead   !returned from itras_param      

C  CALLING SUBROUTINES: FRLIS
C
C  LOCAL VARIABLES
      integer isub,ivc,j,k,iv,ibit,i
      integer ittrack
      logical ks2,kk4
      real*4 tot,speed
      real*8 fmax,fmin
      real*8 ChanBW
      integer ntot   
      real*8 bit_eff_1bit
      real*8 bit_eff_2bit
      real*8 fudge

      bit_eff_1bit=0.637
      bit_eff_2bit=0.881
      
C
C 1. Summary information

      ks2=cterna(is)(1:2) .eq. "S2"
      KK4=cterna(is)(1:2) .eq. "K4"
      ibit=1
      tot=0.
    
      do j=1,nchan(is,ic)
        if(vcband(j,is,ic) .ne. 0 .and. freqrf(j,is,ic).gt. 0) then
          ChanBw=vcband(j,is,ic)
        endif
      enddo
      call itras_params(is,ic,npass,ntrks,nhead,ibit)

      ntot=ntrakf(is,ic)
      tot=trkn(1,is,ic)+trkn(2,is,ic)
   
      if (ifan(is,ic).gt.0) then
        ittrack=npassf(is,ic)*ntrakf(is,ic)*ifan(is,ic)
      else
        ittrack=npassf(is,ic)*ntrakf(is,ic)
      endif
      write(ludsp,
     > "('  Mode      Tot.Rate    Tot.BandW   #BBC #bits  Barrel')")
      write(ludsp,
     > "(1x,a8,1x,i5,' Mbits  ',i5,' MHz     ',i2,5x,i1,5x,a4)")
     >   cMODE(is,Ic), nint(samprate(is,ic)*ntot*nhstack(is,ic)),
     >   nint(ChanBW*ntot)/ibit,
     >   nchan(is,ic), ibit,cbarrel(is,ic)
        write(ludsp,
     > "('  Chan.BW  #Subpasses  Tracks(*fan)  Tot.tracks ')")
      write(ludsp,
     > "(1x,f5.2,' MHz',5x,i2,9x,i2,'(*',i1,')',7x,i3)") 
     > ChanBW,NPASSF(is,Ic),ntrakf(is,ic),ifan(is,ic),ittrack     
   
      do j=1,nband !# of bands
        fmax=0.0
        fmin=100.0d6
        if (nfreq(j,is,ic).gt.0) then
          do k=1,nfreq(j,is,ic) ! each VC
            if (j.eq.1) iv=k
            if (j.eq.2) iv=k+nfreq(1,is,ic)
            ivc=invcx(iv,is,ic)
            if(freqrf(ivc,is,ic) .gt. 0) then
               fmax=max(fmax,freqrf(ivc,is,ic))
               fmin=min(fmin,freqrf(ivc,is,ic))
            endif
          enddo ! each VC
          write(ludsp,"(2x,a1,'-band spanned bw=',f8.1,' MHz',7x,'rms ',
     .     'spanned bw=',f8.1,' MHz')")lband(j),fmax-fmin,bwrms(j,is,ic)
        endif
      enddo !# of bands
      write(ludsp,
     > "('  Effective number of 1-bit channels recorded per sub-pass')")
      write(ludsp,"(2x,9x,a1,8x,a1,6x,'Total')") (lband(i),i=1,nband)
!      write(ludsp,'(5x,3f9.2)') (trkn(isub,is,ic),isub=1,2),tot

      if(ibit .eq. 2) then
        fudge=(bit_eff_2bit/bit_eff_1bit)**2/2.d0  
      else
        fudge=1.d0
      endif 
      write(ludsp,'(5x,3f9.2)') (ntrkn(isub,is,ic)*fudge,isub=1,2), 
     >   fudge*(ntrkn(1,is,ic)+ntrkn(2,is,ic)) 

      
      if (itype.eq.1) return
      write(ludsp,"('  Chan#    Skyfreq  BBC#   LOfreq   IF   Switch')")
      do iv=1,nchan(is,ic)
         ivc=invcx(iv,is,ic)
         if (ibbcx(ivc,is,ic).gt.0) then
           write(ludsp,'(5x,i2,3x,f9.2,2x,i2,2x,f8.1,3x,a2,3x,a3)')
     >      ivc,freqrf(ivc,is,ic),ibbcx(ivc,is,ic),freqlo(ivc,is,ic),
     >      cifinp(ivc,is,ic),cset(ivc,is,ic)
         endif 
       enddo
      return
      end
