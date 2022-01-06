      SUBROUTINE thin_cmd(cmdline)  !FILL COMMAND

! History

! Thin a schedule by removing Station-X from N scans. 
! This was devised becase svetloe can only observe 350 scans in a day.
! The station is removed to try to keep the gap between scans as smooth as possible.

! 2020Jun16. JMG First version. Code borrowed from fillcmd.f 

      implicit none 
      include "../skdrincl/skparm.ftni"
      include "skcom.ftni"
      include "../skdrincl/sourc.ftni"
      include "../skdrincl/statn.ftni"
      include "../skdrincl/freqs.ftni"
      include "../skdrincl/skobs.ftni"
      include "major.ftni"
      include "../skdrincl/constants.ftni"
      include 'astro.ftni'
      include 'downtime.ftni'
C
C  INPUT VARIABLES:
C

! Input
      character*(*) cmdline

! Functions
      integer*4 isecdif        !Difference between two times in seconds
      integer trimlen        !non-blank length of string
      integer igetsrcnum     !get the source number 
      integer iwhere_in_int_list
      logical kcont          !check to see if source is continous 
      logical kstatup        !check if station in downtime     
      
C
C  CALLING SUBROUTINES: SKED
C  CALLED SUBROUTINES: splitntokens, gtdtr, wrerr, UNPAK, 
C                      when_at_next_source, cvpos, seconds2hms,
C                      pakup, indexxint,isecdif
C
C  LOCAL VARIABLES

!     Variables used to calculate idle time
      integer iobs_stat(nobs)                      !this contains observations to consider
      integer num_obs                              !This is the number of observation. 
      integer iStat_Thin 
      integer NumThin                              !number to thin 
      integer NumLeft                              !number left to delete 
      real*8 ut_dif, ut_dif_min
      integer mjd_prev                             !time of previous obsevation. 
      real*8  ut_prev
      

      integer i,j                                  ! indices for loops
      integer kerr                                 ! variable for unpak errors
      integer iobs_delete                          !observation to delete.
      integer iobs                                 !counter 
      integer ithin 
      integer ilen 
      integer istat                                !counter over stations.
      integer ierr 
     
! Local variables to get info from the command ( from licmd.f )
      integer*2 lkeywd(12)
      character*22 ckeywd
      equivalence (lkeywd(2),ckeywd) 
      integer ich 
      
      logical kbegin 
  
      integer*4 itemp          !temporary variable 

! Variable dealing with tokens
      integer MaxToken
      integer NumToken, iToken
      parameter(MaxToken=4)
      character*(2*max_stn) ltoken(MaxToken)


! Used to store token information
      integer*2 itemp_vec(10) 
      character*30 ltemp
      equivalence(ltemp,itemp_vec)     
      integer nst    !number of stations 
      integer  istn(max_stn) 

! Some initialization
      isorcm=0     ! all sources
      nst=0        ! all stations 

      call splitNtokens(cmdline,ltoken,Maxtoken,NumToken)
      if(NumToken .ne. 3) goto 900

! Decode the TimeRange ...
      lkeywd(1)=trimlen(ltoken(1))
      ltemp=ltoken(1)
      do i=1,(lkeywd(1)+1)/2
         lkeywd(1+i)=itemp_vec(i)
      enddo
      call gtdtr(lkeywd,ierrcm)
      if (ierrcm.ne.0) then
        call wrerr(ierrcm,inumcm)
         return
      endif
! Start and end time now in common.
!    MJstCM,UTstCM  & MJenCM, UTenCM
  
! Get the station list.
       lkeywd(1)=trimlen(ltoken(2))
       ltemp=ltoken(2)
       do i=1,(lkeywd(1)+1)/2
         lkeywd(1+i)=itemp_vec(i)
       enddo 
       ich=1    !start at the first character
       CALL gtsti(lkeywd,ich,nst,istn,ierr,ludsp)
       if (ierr.ne.0) then 
          write(ludsp,*) "Thin_Cmd: Revise station names of the subnet"
          return
       endif
       if(nst .ne. 1) then 
          write(ludsp,*) "Thin_Cmd: can only process one station"
          return
       endif 
       istat_thin=istn(1) 
! And the number of observations to thin= remove.  
       read(ltoken(3), *, err=900) NumThin

! make a list of the observations that involve the station to thin. 
      num_obs=0
      kbegin=.false.
      do iobs=1,nobs
        cbuf=cskobs(iskrec(iobs))
        call unpak(kerr, 0)
        if (((mjdcur(istcur(1))-jdstcm)*86400+
     >       (utcur(istcur(1))-utstcm)).ge. 0) then 
            kbegin=.true. 
        endif
        if(kbegin) then 
           do istat=1,nstncur
             if(istcur(istat) .eq. istat_thin) then
                num_obs=num_obs+1
                iobs_stat(num_obs)=iobs
!                write(ludsp,*) "OBS",num_obs,
!     >            mjdcur(istcur(1)),utcur(istcur(1))
                exit 
             endif
           end do 
        endif
        if (((mjdcur(istcur(1))-jdencm)*86400+
     >       (utcur(istcur(1))-utencm)).ge. 0) exit   ! done with loop
      end do 
! At this point 
!  iobs_stat   points to the number of observations that involve this station.
!
      NumLeft=NumThin 
      do ithin=1,NumThin
        mjd_prev=0
        ut_prev=0
        ut_dif_min= 86400.   !set the time diffrence to 1 day.  Below we 
        do iobs=1,num_obs
          if(iobs_stat(iobs) .eq. 0) cycle
          cbuf=cskobs(iskrec(iobs_stat(iobs)))
          call unpak(kerr,0)
          if(ut_prev .ne. 0) then
            ut_dif=(mjdcur(istcur(1))-mjd_prev)*86400+
     >             (utcur(istcur(1))-ut_prev) 
            if(ut_dif .lt. ut_dif_min) then
             iobs_delete=iobs
             ut_dif_min=ut_dif
            endif
!            write(*,*) iobs, ut_dif
          endif
          ut_prev =utcur(istcur(1))
          mjd_prev=mjdcur(istcur(1))
        end do
!        write(*,*) "iobs_delete",iobs_delete, ut_dif_min 
!        pause 
! At this point iobs_delete is the observation to delete.
        cbuf=cskobs(iskrec(iobs_stat(iobs_delete)))
        call unpak(kerr,0)
!        write(ludsp,*) "Del ",ithin, mjdcur(istcur(1)),utcur(istcur(1))
! Find out where the station is in the list.
        if(.true.) then 
! This makes a new list, removing the station istat_thin 
          if(nstncur .eq. 2) then 
            write(*,*) cbuf(1:50) 
          endif 
          DO  I=1,nstnCur                                              
            IF (istcur(I).EQ.istat_thin) then
              do j=i,nstncur-1
                istcur(j)=istcur(j+1)
              end  do
              exit 
            endif 
          END DO   
          nstncur=nstncur-1                                                         
          call pakup(ilen,0)
          cskobs(iskrec(iobs_stat(iobs_delete)))=cbuf
          if(nstncur .eq. 1) then
             iskrec(iobs_stat(iobs_delete))=0
          endif 
        endif
        iobs_stat(iobs_delete)=0
        NumLeft=NumLeft-1
        if(NumLeft .eq. 0) cycle 
      end do
! Now we have to do some cleanup.  Get rid of scans with only 1 station.
!  iskrec(iobs)=-negative number.

        j=0
        do iobs=1,nobs
          if(iskrec(iobs) .gt. 0) then 
             j=j+1
             iskrec(j) = iskrec(iobs)
          endif
        end do
        nobs=j 

      return
! common error return
900   continue
      write(*,*) "SYNTAX: THIN Time-Range Station #-obs-to-delete"
      return
      end
