      subroutine op_refresh
! Subroutine to check the optimized parameters and update the name list etc.
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'covar.ftni'

! This routine goes through the list of estimated and optimized paramters and does:
! 1. Ensures that all optimized parameters are also estimated.
!    If not, turns on estimation and issues warning message.
! 2. Makes a cross reference vector from Estimated params into all possible params.
! 3. Makes a cross reference from optimized into estimated.
! 4. Makes parameter names and scales.

!structure of optimized parameters is:
!   5        EOP
!   2*nstatn  Atm
!   3*nstatn  CLK
!   3*nstatn  XYZ
!   2*Nrc    Source RA,DEC

! History
!  2005May26 JMGipson

! local parameters
      integer iprm       !pointer into all possible parameters.
      logical kwarn      !warning flag if a station is optimized and not estiamted.
      integer itemp      !temporary variable.
      integer istat      !which station
      integer isrc       !wich source
      logical kall_set   !all clock parameters set?
      integer iclk       !clock parameters
      integer ioff

      character*1 lxyz(3),luen(3)
      character*3 lradec(2)

      data lradec/"RA","DEC"/

      data lxyz/"X","Y","Z"/
      data luen/"U","E","N"/
!
      ixref_est2all=0
      ixref_opt2est=0

      num_est=0
      num_opt=0

      do iprm=1,max_dim_esti
        kwarn=.false.
        if(lpara(iprm,1).and. .not.lpara(iprm,2)) then  !parameter optimized but not estimated!
          kwarn=.true.
          lpara(iprm,2)=.true.
        endif

        if(lpara(iprm,2)) then
! update num_est, num_opt and cross references.
          num_est=num_est+1
          ixref_est2all(num_est)=iprm
          if(lpara(iprm,1)) then
            num_opt=num_opt+1
            ixref_opt2est(num_opt)=num_est
          endif
! update names, scales, etc.
          if(iprm .eq. 1) then
             cparname(num_est)='XPOL'
             cpardim(num_est)='uas'
          else if(iprm .eq. 2) then
             cparname(num_est)='YPOL'
             cpardim(num_est)='uas'
          else if(iprm .eq. 3) then
             cparname(num_est)='UT1'
             cpardim(num_est)='us '
          else if(iprm .eq. 4) then
             cparname(num_est)='PHI'
             cpardim(num_est)='uas'
          else if(iprm .eq. 5) then
             cparname(num_est)='EPS'
             cpardim(num_est)='uas'
          else if(iprm .le. 5+2*nstatn) then
! atmosphere
            itemp=iprm-5
            istat=(itemp+1)/2
            ioff=itemp-(istat-1)*2
            write(cparname(num_est),'(a,a,i1)')
     >           cstnna(istat)," ATM",ioff-1
             if(ioff .eq. 1) then
                cpardim(num_est)='ps'
             else
                cpardim(num_est)='ps/D'
             endif
          else if(iprm .le. 5+5*nstatn) then
! clock parameters.
            itemp=iprm-2*nstatn-5
            istat=(itemp+2)/3
            ioff=itemp-(istat-1)*3
            write(cparname(num_est),'(a,a,i1)')
     >         cstnna(istat)," CLK",ioff-1
            if(ioff .eq. 1) then
               cpardim(num_est)='ns'
            else if(ioff .eq. 2) then
               cpardim(num_est)='D-14'
            else if(ioff .eq. 3) then
               cpardim(num_est)='D-14/D'
            endif
          else if(iprm .le. 5+8*nstatn) then
! station coords.
            itemp=iprm-5*nstatn-5
            istat=(itemp+2)/3
            ioff=itemp-(istat-1)*3

            cpardim(num_est)='mm'
            if(kcar) then
              cparname(num_est)=cstnna(istat)//" "//lxyz(ioff)
            else
              cparname(num_est)=cstnna(istat)//" "//luen(ioff)
            endif
          else
! Source coords
            itemp=iprm-8*nstatn-5
            isrc=(itemp+1)/2
            ioff=itemp-(isrc-1)*2
            cparname(num_est)=csorna(isrc)(1:8)//" "//lradec(ioff)
            cpardim(num_est)='mas'
          endif   !If over iprm
        endif     !if over lparm(iprm,2)
        if(kwarn) then
          write(luscn,'("OP_REFRESH: Warning! Optimized parameter ",a,
     >           "not estimated. Turning it on!")') cparname(num_est)
        endif
      end do
      num_tri_est=num_est*(num_est+1)/2
! Check for case where all the clock parameters are turned on.  Need at least one reference.
      do iclk=1,3
        kall_set=.true.
        do istat=1,nstatn
           iprm=5+2*nstatn+(istat-1)*3+iclk
           if(.not.lpara(iprm,2)) then
              kall_set=.false.
           endif
        end do
        if(kall_set) then
           write(luscn,
     >      '("OP_REFRESH: All CLK",i1, " parameters turned on. ",
     >       "Turning  off: ", a," CLK",i1)')  iclk-1,cstnna(1),iclk-1
          iprm=5+2*nstatn+iclk
          lpara(iprm,1)=.false.
          lpara(iprm,2)=.false.
        endif
      end do
      return
      end
