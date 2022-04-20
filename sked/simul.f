      SUBROUTINE SIMUL(nsubc,iobs,IADD,klist,ksolve)
CHS--------------------------------------------------------------------
CHS General purpose
CHS Simul was created in order to compute the partial derivatives of
CHS the observations. First, it unpacks the scan in the buffer.
CHS Then it defins the weights, the different observations should get
CHS in the covariance analysis. After this, the partial derivatives are
CHS stored in the cc array. In order to have better conditions for the
CHS matrix of normal equations, certain scale factors are extracted. Those
CHS are defined in opfill and the formal errors are corrected in result.
CHS At the end the contribution, the observations make to the mne, is
CHS added to the yet existing mne. Of course there are 2 modes:
CHS
CHS nsubc = 0 means, that the contribution is really taken over
CHS nsubc = 1,2,3... 10  means, that we are just testing configurations
CHS
CHS iadd    1 for adding contribution, -1 for subtracting contribution
CHS
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'covar.ftni'

C   INPUT
      integer nsubc,iobs,iadd
      logical klist ! true to list the observation as it is added
C                     or deleted
      logical ksolve ! true to make an output file for SOLVE

C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/broadband.ftni'
      include 'minor.ftni'
      include 'major.ftni'

! function
      double precision hms2seconds
      integer*4 indx4
      integer trimlen
      double precision dot8                 !dot product of 2 vectors
      integer igetsrcnum
      integer igetstatnum
      integer ibnum,igtfr,julda

C  LOCAL VARIABLES
      double precision t0,t_now,del_t
      save t0

! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=2*Max_stn+12)
      character*(2*Max_stn) ltoken(2*MaxToken)
! The widest token is station_code*cable_wrap,e.g. AWB_...ZC  etc. This is 2*Max_stn
! Number of tokens is 11+2*Max_stn

! Used to compute UEN partials for station.
      double precision s_hat(3),U_hat(3),E_hat(3),N_hat(3)
      double precision s_lat,c_lat,s_long,c_long

      integer*2 i1,i2
      integer i,j,k,l,ib,ic,jc
      integer ical,lfrq,isc,mjd,imon,iday,idurx,nst
      character*2 cfrq
      equivalence (lfrq,cfrq)
      integer ix,icod,ix_band,isor,ierr,is_band
      integer iptr
      double precision dtemp
      double precision ST0,GST,UT,wt,sigma,x_snr,ssigma,s_snr
 
      double precision sigma_snr,sigmaion,sigmaphase
      double precision HA(MAX_STN)
      double precision sin_el(MAX_STN),  cos_el(MAX_STN)   
      double precision H,RAAP,DEAP,SHS,CHS,SD,CD,SE,atm_part

      integer isign
      double precision partial(MAX_DIM_ESTI)            !partial deriviatives.
      double precision partial_scaled(Max_dim_esti)     !scaled partials.
      double precision src_part_ra, src_part_dec        !Source RA and DEC partials 

      integer iyr,ida,ihr,imin
      integer iyr2
      integer*2 LSrcnam(max_sorlen/2)
      character*(max_sorlen) csrcnam
      equivalence (csrcnam,lsrcnam)
C      - holders for source, procedure names
      integer*2 LST(MAX_STN),ICB(MAX_STN)
      character*2 cst(max_stn),ccb(max_stn)
      equivalence (lst,cst),(icb,ccb)
      integer istn_tmp(max_stn)                !hold station info. Order from sked file.
      integer istn(max_stn)                    !stations in scan. Ordered.
      integer IDUR(MAX_STN)                    !Durations.
      integer ikey(max_stn)
      integer istat
      integer itemp
      character*1 c1
      equivalence(itemp,c1)

       integer job
      double precision rcond 
C
C  HISTORY
! 2022-04-07 JMGipson. If radd_noise = 0.0 and ksnrwts =.false. then effectively use radd_noise=15ps.
! 2021-05-29 JMGipson. Renamed some variables. Add in calculation for group delay uncertainty for VGOS. Hardware frequency sequence.
! 2021-01-20 JMGipson. Don' try to invert normal equations if num_est=0

C     880315 NRV DE-COMPC'D
C     890502 NRV Added reading durations
C     911026 NRV Added klist to list/not list observations
C                Changed index on filling qmne array
C                Added call to snrac to calculate SNRs instead
C                  of doing it explicitly here.
C                Added rss with 15 ps to sigma calculation.
C                Used rms bandwidth in sigma calculation.
C*****920624 NRV Temporary test with 30 ps
C     920706 nrv added inoise
C     921005 NRV Added SOLVE output
C     930225 NRV implicit none
C     930610 nrv Calculate only the partials that are needed.
C     950228 nrv Add duration to output line for solve
C     950302 nrv Correct X-band sigmas by ion correction.
C     950511 nrv Add calculation of sigma for phase delay to
C                solve output.
C 970312 nrv Change 4 to max_sorlen/2
C 000212 nrv Remove flush.
C 000817 nrv Check S-band SNR before computing sigma ion.
C 001020 nrv Add section for single band calculations.
C 020904 nrv Don't call igtso(lsn,isor) if isor=-1 already.
C 020904 nrv Only first 8 characters of source name were being unpacked.
! 2003Dec08 JMGipson replaced igtso by igetsrcnum
! AEM 20041216 Add common string buffer to write in it
!	       and then print it on screen using no FORMAT
!	       statement and '$' sign
! 2005May13 JMGipson Computation of MJD was done wrong. Fixed.
! 2005Oct07 JMGipson.  Made sure that all scans are put out in the same order.
!                      Previously order was what was in sked file.
!                      Also changed parsing of input line.
! 2007Sep13 JMGipson. Fixed problem with call to cover. Was always getting
!                     first time. Should get time of observation.
! 2007Oct05 JMGipson. Got rid of nsubc in call to cover. (Not used.) Added isor
! 2008May22 JMGipson. Replaced iobswtmode with ksnrwts
! 2009Nov05 JMGipson.  fixed bug treating logical (ksnrwts) as integer.
! 2013Oct03 JMGipson.  Changed some source names to make it easier to read , eg. cc-->partial.
! 2014Jul09 JMGipson.  Removed idur/2  from epoch. This makes it easier for comparison with other things and has minimal effect on formal error.
! 2016Oct31 LeBail.    Removed multiplication by sin(eps) on nutation partial to make consistent with calc. 
! 2020Apr15 JMGipson.  Fixed error in SIGN of atmsophere rate partial



C     1. For our first trick, we decode all of the entries in  the buffer.
C     **CAUTION** No error checking is done.  It is assumed
C                 that the schedule entries were written by
C                 SKED originally and so should not have to
C                 be checked.
C     The format of the entries is the following:
C
C     source   cal code preob start duration midob idle postob    scscsc PDft1 PDFt2 ....
C     Example:
!     1          2   3  4     5             6   7       8  9      10
C     3C84      120 SX PREOB 800923120000  780 MIDOB    0 POSTOB K-F-G-OW 1F
C     where all items are not restricted to specific columns.
C     **NOTE: "code" IS REALLY ONLY CODE NOW, NOT MODE&BANDWIDTH TOO.
C

      if(nsubc.eq.0) then
        if(iverbose_level .ge. 1) then 
          if (klist) then !list observation in its entirety
            if(iadd.eq.-1) then
              write(ludsp,'(a,$)')'SIMUL01: Removing  | '
            else
              write(ludsp,'(a,$)')'SIMUL02: Inserting | '
            endif
!          nc=trimlen(cbuf)
            write(ludsp,'(a)') cbuf(1:70)
          else !list observation index only
            if (iadd.eq.-1) then
             write(ludsp,'("delete",i5,"  ",$)') iobs
            else
             write(ludsp,'("insert",i5,"  ",$)') iobs
            endif
          endif
        else
          write(ludsp,'(i4," ",$)') iobs          
          if(mod(iobs,20) .eq. 0) write(ludsp,'()')
        endif 
      endif

! 1. *****PARSE INPUT LINE *************
! split the input line completely up.
      call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
      csrcnam=ltoken(1)
      read(ltoken(2),*) ical
      cfrq=ltoken(3)
! extrac the time.
      read(ltoken(5),'(i2,i3,i2,i2,i2)') iyr,ida,ihr,imin,isc

! Code below fixes bug noticed by AEM.
! Note that Julda arg is years since 1900.
      if (iyr.lt.70) then
        iyr2 = iyr + 100
      else
        iyr2 = iyr
      endif
! AEM undo     MJD = JULDA(1,IDA,IYR)
      MJD = JULDA(1,IDA,IYR2)
! AEM comment: problem fixed
      call ymday(iyr,ida,imon,iday)

! Common duration.
      read(ltoken(6),*) idurx
! AEM 20041227 leave only one similar calculation
!      UT = IHR*3600.D0+IMIN*60.D0+ISC+(idurx/2)

!      ut =hms2seconds(ihr,imin,isc+idurx/2)

       ut =hms2seconds(ihr,imin,isc)
! Convert to Days.
      t_now=ut/86400.d0
      if(iobs .eq. 1 .and. nsubc .eq. 0) then
	t0=t_now
      endif
      if(t_now .lt. t0) then
         t_now=t_now+1
      endif

! time offset.
      del_t=t_now-t0

      CALL SIDTM(MJD,ST0,FRAC)
      GST = DMOD(ST0 + UT*FRAC, twopi)

! skp  midob=token 7
! Skip idle =token 8
! skip postob=token 9
! Now we are at the station/cablewrap token.
      nst=trimlen(ltoken(10))/2         !find out number of stations.

      do i=1,nst
        j=2*i
        cst(i)=ltoken(10)(j-1:j-1)
        ccb(i)=ltoken(10)(j:j)
      end do
! next NSTN tokens are footage counters. Following one is procedure flag. Then durations.
!
! Start decoding the durations.
      do i=1,nst
         read(ltoken(i+11+nst),*) idur(i)
      end do
! 1. DONE **************** Done Parsing input line**********************

! 911026 NRV Compute SNRs here
      do i=1,nst
        itemp=lst(i)
        j=igetstatnum(c1)
        idurst(j)=idur(i)
        istn_tmp(i)=j
      enddo
! Fix up. Make sure all stations are in the same order.
      call indexxint(nst,istn_tmp,ikey)
! Now put in correct order
      do i=1,nst
         iptr=ikey(i)
         istn(i)=istn_tmp(iptr)
      end do

      icod=igtfr(lfrq,ix)
C     ix_band is the band index for X-band
      ix_band=1
      if(cband(2) .eq. "X ") ix_band=2
      isor=igetsrcnum(csrcnam)
      if(isor.le.0) then
        write(ludsp,9200) csrcnam
9200    format('SIMUL05: Source ',a,' not selected')
        return
      endif
      call snrac(nst,istn,isor,icod,-1,mjd,ut,ierr)

      if(dnorm_tri(1,nsubc) .eq. 0) then
          do i1=1,num_est
           iptr=indx4(i1,i1)  
           dnorm_tri(iptr,nsubc)=small  !this keeps non-singular by adding small diagonal term
         end do
      endif   
      if(nsubc .ne. 0) then 
        dnorm_tri(1:num_tri_est,nsubc)=dnorm_tri(1:num_tri_est,nsubc-1)          
      endif

      if(nsubc.eq.0) then ! insert/delete mode
        do i=1,nstatn
          elev(i)=-99.d0
          azim(i)=-99.d0
        enddo
      endif

! Get ready to compute the partials.  Compute terms used many times.
! source stuff
      raap=sorp_now(1,isor)
      deap=sorp_now(2,isor)
      sd=dsin(deap)
      cd=dcos(deap)
      h=gst-raap
      if(h.gt.twopi) h=h-(twopi)
      if(h.lt.twopi) h=h+(twopi)
      shs=dsin(h)
      chs=dcos(h)
      S_hat(1)=cd*chs          !unit vector in the direction of the source.
      S_hat(2)=-cd*shs
      S_hat(3)=sd
      se=dsin(eps)

! station stuff
      do k=1,nst
        i=istn(k)
        ha(i)=h-stnpos(1,i)
        if(ha(i).lt.0.d0) ha(i)=ha(i)+twopi
        ha(i)=dmod(ha(i),twopi) 
        call elevat(deap,ha(i),stnpos(2,i),elev(i),azim(i))
        cos_el(i)=dcos(elev(i))
        sin_el(i)=dsin(elev(i))
      end do

!********NOTE***************************
!  The calculations below should be done on a per-station basis because
!  the frequency sequence could be different if different stations are
!  observing a subset of frequencychannels. For now, use the frequencies for station 1.

! This is big loop where we compute the partials.
      do k=1,nst-1
      do l=k+1,nst
        i=istn(k)
        j=istn(l)

        ib=ibnum(i,j)
! Compute the wt to use.
! CHS kwsnr   weights by SNR
! CHS kwbas   weights by baseline
        if(kSnrWts) then
          x_snr=iactbl(ix_band,ib)          
          if(bb_bw(i) .eq. 512.0 .and. bb_bw(j) .eq. 512.0) then    
! Special kludge for broadband schedules.
!            x_snr_per_channel=x_snr/4.    !Each 512 has 16 32MHz channels. Get the SNR per channel.
! See memo by Roger Capalo Covariance Analysis ... 2015 August 14.            
!           sigma_snr=2.65*1.e6/(twopi*2612.d0*x_snr_per_channel)   !2612 = bwrms for current (~2021) VGOS setup over 4 bands in MHz.
                                                                    !2.65 is increase in Sigma because the correlator estimates dtec.
            x_snr=2*iactbl(ix_band,ib)                              !Factor of 2  comes from 4bands.                                                                  
            sigma_snr=2.65*1e6/(twopi*2612.d0*x_snr)                !See equation for X-band sigma. 
            sigma=dsqrt( sigma_snr**2+radd_noise**2+
     >                    (rel_noise/sin_el(i))**2  +
     >                    (rel_noise/sin_el(j))**2  ) !now rss with noise     
            wt=1./sigma                                                                             
          else if (nband.eq.2) then ! X/S calculations           
            is_band=2
            if (ix_band.eq.2) is_band=1
            s_snr=iactbl(is_band,ib)
            if (x_snr.gt.0.0.and.s_snr.gt.0.0) then            !valid X/S SNR
              sigmaphase=1.d6/(pi*freqrf(ix_band,1,icod)*x_snr)  !ps
              sigma=1.d6/(twopi*bwrms(ix_band,1,icod)*x_snr)     !ps
C************ Compute S-band sigma and ionosphere correction.
              ssigma=1.d6/(twopi*bwrms(is_band,1,icod)*s_snr)  !ps
              sigmaion = ffact(1,icod)*dsqrt(sigma**2 + ssigma**2)
              sigma_snr = dsqrt(sigmaion**2 + sigma**2)       !NOT noise weighted for solve
C************ s-band sigma and iono  
              sigma=dsqrt( sigma_snr**2+radd_noise**2+
     >                    (rel_noise/sin_el(i))**2  +
     >                    (rel_noise/sin_el(j))**2  ) !now rss with noise     
              wt=1./sigma
            else
              write(ludsp,9901) x_snr,s_snr,csorna(isor),
     .           ida,ihr,imin,cstcod(i),cstcod(j)
9901          format('SIMUL06: Computed X-band snr = ',f5.1,
     .          ' S-band snr = ',f5.1,' for ',a8,
     .          ' at ',i3,'d',i2.2,'h',i2.2,'m',
     .          ' on baseline ',a1,'-',a1,', observation downweighted!')
                wt=1.d-5
            endif ! valid/not X/S SNR
          else ! single-band calculations
            if (x_snr.gt.0.0) then !valid SNR
              sigmaphase=1.d6/(pi*freqrf(ix_band,1,icod)*x_snr) !ps
              sigma=1.d6/(twopi*bwrms(ix_band,1,icod)*x_snr)    !ps
              sigma_snr=sigma*1.1                             !can't compute iono with 1 baseline,  but put in kludge so you can do something!
              sigma=dsqrt(sigma_snr**2+radd_noise**2)         !now rss with noise
              wt=1./sigma
            else
              write(ludsp,9902) x_snr,csorna(isor),
     .        ida,ihr,imin,cstcod(i),cstcod(j)
9902          format('SIMUL06: Computed snr = ',f5.1,
     .          ' for ',a, ' at ',i3,'d',i2.2,'h',i2.2,'m',
     .          ' on baseline ',a1,'-',a1,', observation downweighted!')
                wt=1.d-5
            endif ! valid/not SNR
          endif ! dual/single calculations
        else ! SNR wt mode.
          if(radd_noise .ge. 1) then 
             wt=1./radd_noise
          else
             wt=1./15.d0
          endif 
        endif ! kweq,kwbas,kwsnr

! Initialize Partials.
        partial=0.d0 
! Start computation.
! 1. First compute EOP partials.
        if (lpara(1,2).or.ksolve)
     .       partial(1)=(bz(ib)*cd*chs-bx(ib)*sd)                !partial(1) =Xwob
        if (lpara(2,2).or.ksolve)
     .       partial(2)=(bz(ib)*cd*shs+by(ib)*sd)                !partial(2) =Ywob
        if (lpara(3,2).or.ksolve)
     .       partial(3)=-omega*cd*(bx(ib)*shs+by(ib)*chs)        !partial(3) =UT1
! this is supposed to be the Nutation partials 
        if (lpara(4,2).or.ksolve)
!     .       partial(4)=se*(bx(ib)*dcos(gst)*sd-by(ib)*dsin(gst)*sd
!     >                  -bz(ib)*cd*dcos(raap))
! 31OCT16 - removed multiplication by se
     .       partial(4)=bx(ib)*dcos(gst)*sd-by(ib)*dsin(gst)*sd
     >                  -bz(ib)*cd*dcos(raap)
        if (lpara(5,2).or.ksolve)
     .       partial(5)=(bx(ib)*dsin(gst)*sd+by(ib)*dcos(gst)
     .       *sd-bz(ib)*cd*dsin(raap))

! Now compute partials that depend on each site.
        do i1=1,2
          if(i1 .eq. 1) then
            istat=i
            isign=1
          else
            istat=j
            isign=-1
          endif
! Atmosphere
          ic=5+(2*istat)-1
          call atmos(sin_el(istat),cos_el(istat),atm_part)
          if(lpara(ic,2))   partial(ic)=atm_part*isign
          if(lpara(ic+1,2)) partial(ic+1)=atm_part*del_t*24*isign   !convert rate to hours, not days
! Clock
          ic = 5+2*nstatn+(3*istat)-2
          if (lpara(ic,2))   partial(ic)=1*isign
          if (lpara(ic+1,2)) partial(ic+1)=del_t*isign
          if (lpara(ic+2,2)) partial(ic+2)=del_t*del_t*isign
! Site coordinates.
          ic = 5+(5*nstatn)+(3*istat)-2
          if(kcar.or.ksolve) then
! Do it XYZ.
            if (lpara(ic  ,2).or.ksolve) partial(ic  )=cd*chs*isign
            if (lpara(ic+1,2).or.ksolve) partial(ic+1)=-cd*shs*isign
            if (lpara(ic+2,2).or.ksolve) partial(ic+2)=sd*isign
          else
! Do it in UEN.  use the rule that U_part=U_hat*S_hat, etc.
            c_long=cos(stnpos(1,istat))
            s_long=-sin(stnpos(1,istat))  !Minus sign becuase stnpos is West longitude
            c_lat =cos(stnpos(2,istat))
            s_lat =sin(stnpos(2,istat))
! U partial
            if(lpara(ic,2)) then
! U_hat=(cos_lat*cos_long,cos_lat*sin_long,sin_lat)
               U_hat(1)=c_lat*c_long
               U_hat(2)=c_lat*s_long
               U_hat(3)=s_lat
               partial(ic)=dot8(S_hat,U_hat,3)*isign
            endif
! E_partial
            if(lpara(ic+1,2)) then
! E_hat=(sin_long,-cos_long,0)
               E_hat(1)=-s_long
               E_hat(2)=c_long
               E_hat(3)=0
               partial(ic+1)=dot8(S_hat,E_hat,2)*isign      !note: only need to do the first 2.
            endif
            if(lpara(ic+2,2)) then
! N_hat=(-sin_lat*cos_long,sin_lat*sin_long,cos_lat)
               N_hat(1)=-s_lat*c_long
               N_hat(2)=-s_lat*s_long
               N_hat(3)=c_lat
               partial(ic+2)=dot8(S_hat,N_hat,3)*isign
            endif
          endif
        end do
! Source positions
        ic = 5+(8*nstatn)+(2*isor)-1
        if (lpara(ic,2))
     >      partial(ic)= -cd*(bx(ib)*shs+by(ib)*chs)
        if (lpara(ic+1,2))
     >      partial(ic+1)=(-sd*(bx(ib)*chs+by(ib)*shs)-bz(ib)*cd)
    
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C   Write out the observation record for the SOLVE output file.

        if(ksolve) then
          ic = 5+(5*nstatn)+(3*i)-2                            !points to station index.
          jc = 5+(5*nstatn)+(3*j)-2
          src_part_ra=-cd*(bx(ib)*shs+by(ib)*chs)
          src_part_dec=(-sd*(bx(ib)*chs+by(ib)*shs)-bz(ib)*cd)         

          write(lutmp,9191) 
     >      csorna(isor)(1:8),iyr,imon,iday,ihr,Imin,isc,
     >      cstnna(i),cstnna(j),sigma_snr,
     >      azim(i)*rad2deg,elev(i)*rad2deg,
     >      azim(j)*rad2deg,elev(j)*rad2deg,
     >      (partial(i1)/c,i1=1,5),                                    !EOP partials 
     >      (partial(i1)/c,i1=ic,ic+2), (partial(i2)/c,i2=jc,jc+2),    !Station Position partials
     >      src_part_ra/c,src_part_dec/c,                              !Source  Partials
     >      min(idurst(i),idurst(j)),sigmaphase
9191        format(a,1x,6(i2,1x),a8,1x,a8,1x,f10.5,1x,2(f7.2,f6.2),
     >          13d12.5,1x,i5,1x,f7.2)
         endif

! The contribution to the normal matrix is either added or subtracted.
! The vector D is the partials appropriately scaled.
         do i1=1,num_est
           iptr=ixref_est2all(i1)
           if(iptr .eq. 3) then
              partial_scaled(i1)=partial(iptr)*(1.d6/c)*wt            !UT1        units ps/(us of EOP)
           else if(iptr .le. 5) then
              partial_scaled(i1)=partial(iptr)*(1.d6/c)*secrad*wt     !other EOP  units ps/(uas of EOP)
           else if(iptr .le. 5+2*nstatn) then
              partial_scaled(i1)=partial(iptr)*wt                     !Atmospheres units 1.
           else if(iptr .le. 5+5*nstatn) then
              partial_scaled(i1)=partial(iptr)*1.d3*wt                !clocks      ps/ns
           else if(iptr .le. 5+8*nstatn) then
              partial_scaled(i1)=partial(iptr)*(1.d9/c)*wt            !XYZ units   ps/mm  (CC is in meters, c in km/sec)
           else
              partial_scaled(i1)=partial(iptr)*(1.d3/c)*secrad*wt     !Source ps/mas
           endif
         end do      

         do i1=1,num_est
           do i2=i1,num_est
             iptr=indx4(i1,i2)
             dtemp=partial_scaled(i1)*partial_scaled(i2)
             if(iadd .ne.1) dtemp=-dtemp
! note, for nsubc>0, then should only have add
!             write(*,*) i1,i2, dnorm_tri(iptr,nsubc),dtemp
             dnorm_tri(iptr,nsubc)=dnorm_tri(iptr,nsubc)+dtemp
           enddo ! i2=1,num_est
         enddo ! i1=1,num_est
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      enddo ! j=i+1,nstatn
      enddo ! i=1,nstatn-1

! Invert the matrix for use in optimization.
      if(nsubc .eq. 0 .and. num_est .ge. 1) then 
        dnorm_inv(1:num_tri_est)=dnorm_tri(1:num_tri_est,0)  !normal equations so far
! Invert the normal matrix.
        job=11           !compute the inverse AND the rcond
        call invert_and_con_tri(dnorm_inv,rcond,num_est,job)
      endif 


CHS-------------------------------------------------------------
CHS Cover is called in order to determine the scans involved in
CHS sky coverage computations. This only is done, when a scan is
CHS really taken over into the schedule. Besides cover computes an
CHS sky coverage evaluation number covs.
C
      if((iadd.ne.-1).and.(nsubc.eq.0)) then !just insert mode
          call cover(iobs,t_now,isor)
      endif
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      return
      end
