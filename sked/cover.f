      subroutine cover(iobs,t0,isor)
C
CHS-----------------------------------------------------------------
CHS General purpose
CHS Cover was created in order to determine the scans, which
CHS shall be taken into account for sky coverage optimization (Cov ONLY)
CHS or evaluation (LocalCov). Beyond this, an evaluation number can be 
CHS computed in order to judge the sky coverage of any schedule.
C 930930 nrv Change hard-coded 4-hour window for sky coverage
C            calculations to "iwin", same as LAST nnHR
C 931005 nrv Changed final calculation of "covs" to move the sum
C            out of the station loop. Add subscript to ncheck.
! 2007Nov02 JMG. Completely re-written
! 2010Jan25 JMG. Changed index order of eleva, azimu 
!
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/skobs.ftni'
      include 'major.ftni'

 
C
C INPUT:
      integer iobs        !current obs number
      real*8 t0         !current time
      integer isor      !current source

C LOCAL:
      real*8 t_win      !covariance window
      integer i,k,kk
     

! Store some stuff about the current obs
      do i=1,nstatn
        eleva(iobs,i)=elev(i)
        azimu(iobs,i)=azim(i)
      enddo
      tim(iobs)=t0
      isrc_obs(iobs)=isor

! find which observations are  within the time window.
      t_win=rcovar_win/24.    !Convert to fraction of a day.
      do k=1,nobs
        kk=iskrec(k)
        kobc(kk)=.false.
        if(kk.le.iobs) then
          if((tim(iobs)-tim(kk)).le.t_win) then
            kobc(kk)=.true.
          endif
        endif
      enddo

      call sky_cov_pix(iobs)

      return
      end
