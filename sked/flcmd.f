      SUBROUTINE FLCMD(LINSTQ)
C
C   FLCMD determines the function requested in the FLUX command,
C              and then callsthe appropriate subroutine to do it.
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'

C
! functions
      integer istringminmatch
C     INPUT VARIABLES:
      integer*2 LINSTQ(*)
C        LINSTR - input string from user, beginning after the command
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
C
C     CALLING SUBROUTINES: SKED (main program)
C     CALLED SUBROUTINES: IGTKY (to decode the function)
C                         FLLIS (to list the fluxes selected)
C                         FLGET (to get fluxes from catalog)
C
C   LOCAL VARIABLES
C        IFUNC  - Function requested code
      integer nc,ichmv,ifunc,ich,nch,ic1,ic2,idum
      integer*2 lkeywd(12) ! holds fields picked from command line
      character*128 cfluxname

      integer is,ib
      integer i1,i2
      logical kfirst
      integer itemp
      character*24 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=4)
      character*6 list(ilist_len)
      data list/"LIST","SELECT","CHECK","FIX"/


C   History
C   891114 NRV Created, modeled after SOCMD
C   891214 NRV Added option for additional file name after SELECT
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fix gtfld call to remove linstq
! 2008Jul28 JMG. Added "Check" command. 
! 2008Sep03 JMG. Added "Fix" command:  if flux is missing for one band, use other band. 
! 2009Sep22 JMG. Did not use correct LU to display results so that we could capture. luscn->ludsp 
! 2009Sep29 JMG. Modified fix command so that if both fluxes were missing, used 0.25  
!                Set flag indicating fluxes have changed so that they will be written out correctly.
! 2010Mar24 JMG. Wasn't correctly copying fluxes from one band to the other with flux fix. 
! 2013Jul24 JMG. Still a problem with flux fix. Wasn't copying all of the flux model values
C
C
C     1. First call the function IGTKY to decode the input string which
C        the user typed.
C
      ich=1
      nch=linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,nch,IC1,IC2)
      IF  (IC1.EQ.0) THEN
        IFUNC = -1
      ELSE
        nch=ic2-ic1+1
        ckeywd=" "
        idum=ichmv(lkeywd,1,linstq(2),ic1,min0(nch,20))
        ifunc=istringMinMatch(list,ilist_len,ckeywd)
      END IF
      if (ifunc.le.0) then
        write(luscn,
     >  "('FLCMD00: Invalid option! Must be one of: ',5(a,1x))")
     >     list(1:ilist_len)
        RETURN
      END IF  !
      IF  (list(ifunc) .eq. "LIST") then
        CALL FLLIS
        RETURN
      else if(list(ifunc) .eq. "CHECK") then
        kfirst=.true.
        do is=1,nsourc
          do ib=1,nband
            if(nflux(ib,is) .eq. 0) then
              if(kfirst) then
                 write(ludsp,'(a)') 
     >            "WARNING!  Following sources have missing fluxes:"
                 write(ludsp,'(a)') 
     >            "Source    Band"
                 kfirst=.false.
              endif
              write(ludsp,'(a8,2x,a2)') csorna(is),lband(ib)
            endif
         end do
        end do
        if(kfirst) then
           write(luscn,'("All sources have flux on both bands")')
        endif
      else if(list(ifunc) .eq. "FIX") then
        kfirst=.true.
        do is=1,nsourc
          if(nflux(2,is) .eq. 0 .and. nflux(1,is) .eq. 0) then
          if(kfirst) then
             write(luscn,'(a)') 
     >         "WARNING!  Following sources have missing fluxes:"
              write(luscn,'(a)')  "Source    Band"
               kfirst=.false.
            endif
            do ib=1,2
               write(luscn,'(i4,1x,a8,2x,a2)') is, csorna(is),lband(ib)
               nflux(ib,is)=1
               cfltype(ib,is)="B"
               flux(1,ib,is)=0.0
               flux(2,ib,is)=0.25
               flux(3,ib,is)=13000.d0
            end do
          endif     

          do ib=1,nband
            if(nflux(ib,is) .eq. 0) then
              if(kfirst) then
                 write(luscn,'(a)') 
     >            "WARNING!  Following sources have missing fluxes:"
                 write(luscn,'(a)') 
     >            "Source    Band"
                 kfirst=.false.
              endif
              write(luscn,'(i4,1x,a8,2x,a2,2x,$)')
     >             is,csorna(is),lband(ib)
              if(nflux(2,is) .ne. 0 .or. nflux(1,is) .ne. 0) then
                if(nflux(2,is) .ne. 0) then
                  i2=2
                  i1=1
                else
                  i2=1
                  i1=2
                endif
                         
                nflux(i1,is)=nflux(i2,is)
                cfltype(i1,is)=cfltype(i2,is)
! Should really only copy non-zero components...
! But this is easier and makes sure all components are copied. 
                flux(1:max_flux,i1,is)=flux(1:max_flux,i2,is)           
                write(luscn,'("copied from ", a)') cband(i2) 
                 
              endif      
            endif
         end do
        end do
        if(kfirst) then
           write(luscn,'("All sources have flux on both bands")')
        else
           knewfl=.true.      !Set flag indicating fluxes have changed
        endif

      ELSE
          nch=linstq(1)
          call gtfld(linstq(2),ich,nch,ic1,ic2)
          cfluxname=" "
          if (ic1.gt.0) then !alternate name specified
            nc = ic2 - ic1 + 1
            idum=ichmv(lkeywd(1),1,linstq(2),ic1,min0(nc,20))
            cfluxname(1:nc)=ckeywd(1:nc)
          endif !alternate name specified
          CALL FLGET(cfluxname)
        return
      END IF
C
      RETURN
      END
