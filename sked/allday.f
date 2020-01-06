      subroutine allday(mjd,nsor,istn)

C  Initialize the STATION rise/set arrays in common. Calculations are
C  done for the current UT date and then converted to sidereal time.

C  History:
C  931109 NRV Created
C  931110 nrv Check only 23h56m of the UT day = 1 sidereal day.
C             Convert to GST at the end.
C  931124 nrv Simplify logic at the end to make sure rise/set pairs match up.
! 2016Sep26. Changed handling of always/never up.  
! 2016Sep26  Changed the logic of finding rise/set to use finer intervals (5 minutes)because we were missing some 

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'

C INPUT:
      integer mjd,nsor,istn
C OUTPUT:
C The result of this routine is the station arrays tsris, tsset,
C and ntsrisset.
C tsris(source,station,n) - the GST during the day when the source
C                            rises at the station for the nth time
C                            (in radians)
C tsset(source,station,n) - the GST during the day when the source
C                            sets at the station for the nth time
C                            (in radians)
C ntsrisset(source,station) - a count of how many rise/set entries are
C                             in the above two arrays

C  Called by: RSINI
C  Calls: CVPOS

      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'

C LOCAL
      integer i,isec,imin,iout_cnt 
      integer nr,ns ! number of times source rises/sets
      real*8 ut,setsave
      real*4 az,el,ha,dc,x30,y30,x85,y85
C         - az,el,ha,dc,and x,y at date and time
      LOGICAL KUP ! TRUE if source is above limits at MJD,UT
      logical kuppre ! previous value of kup
      integer maxmin,maxsec

! This sets the limits of the loop.
      
      integer iout_limit    !outer loop limit

      integer idelta_out_min     !
      integer idelta_out_sec 
  

C By brute force, call CVPOS for every second of the day (!!) and keep
C track when the KUP changes.
C Alternative: call CVPOS for every minute of the day, and if there is a
C change during that minute check every second.
C Step back again: call CVPOS for every hour of the day, and if there
C is a change then go for the minutes then seconds.

! instead of incrementing by hour, do so by 5 minutes.
! 

      nr=0
      ns=0
      ut=0.d0
      call CVPOS(NSOR,ISTN,MJD,UT,AZ,EL,HA,DC,X30,Y30,X85,Y85,KUP)
      kuppre = kup
!     do iout_cnt =1,24
!       ut = iout_cnt *3600.d0
      idelta_out_min=5
      idelta_out_sec=idelta_out_min*60
      iout_limit=1440/idelta_out_min
     
      do iout_cnt =1,iout_limit
        ut=iout_cnt*idelta_out_sec
       
        if(iout_cnt .eq. iout_limit) ut=ut-180-56.
   
        call CVPOS(NSOR,ISTN,MJD,UT,AZ,EL,HA,DC,X30,Y30,X85,Y85,KUP)
C 040627  ZMM  changed .ne. to .neqv.
        if (kup.neqv.kuppre) then ! a change during the previous hour
!         maxmin=60
          maxmin=idelta_out_min
          if (iout_cnt .eq.iout_limit) maxmin=idelta_out_min-3
          do imin=1,maxmin
            ut = (iout_cnt -1)*idelta_out_sec+imin*60.d0
            call CVPOS(NSOR,ISTN,MJD,UT,AZ,EL,HA,DC,X30,Y30,X85,Y85,KUP)
C 040627  ZMM  changed .ne. to .neqv.
            if (kup.neqv.kuppre) then ! a change during the previous minute
              maxsec=60
              if (iout_cnt .eq.iout_limit) maxsec=4
              do isec=1,maxsec
                ut = (iout_cnt -1)*idelta_out_sec+(imin-1)*60.d0+isec
                call CVPOS(NSOR,ISTN,MJD,UT,AZ,EL,HA,DC,X30,Y30,X85,Y85,
     .          KUP)
                if (kup.and..not.kuppre) then ! it rose
                  nr=nr+1
                  if (nr.gt.MAX_NRS) then
                    write(luscn,9991) csorna(nsor),max_nrs,cstnna(istn)
9991                format(' Source ',a,' rises more than ',i3,
     .              ' times at ',a)
                    stop
                  endif
                  tsris(nsor,istn,nr) = st0cur(istn) + frac*ut
                  if (tsris(nsor,istn,nr).gt.TWOPI)
     .            tsris(nsor,istn,nr)=tsris(nsor,istn,nr)-TWOPI
                else if (.not.kup.and.kuppre) then ! it set
                  ns=ns+1
                  tsset(nsor,istn,ns) = st0cur(istn) + frac*ut
                  if (tsset(nsor,istn,ns).gt.TWOPI)
     .            tsset(nsor,istn,ns)=tsset(nsor,istn,ns)-TWOPI
                endif
                kuppre = kup
              enddo
            endif ! a change during the previous minute
          enddo
        endif ! a change during the previous hour
        kuppre = kup
      enddo
      ntsrisset(nsor,istn) = ns
      if (nr.ne.ns) then
        write(luscn,9100) nsor,nr,ns,istn
9100    format(' ALLDAY01 - Warning: source ',i3,' rose ',i2,' times',
     .  ' and set ',i2,' times at station ',i2)
      endif

C  Handle the never rise/always up cases
!  This was modified 2016Sep26 so that the values do not depend on the day of year. 
      if (ns.eq.0.and.nr.eq.0) then ! always/never up
        tsris(nsor,istn,1)=0.d0
        if (kup) then ! always up
!          tsris(nsor,istn,1) = st0cur(istn)
!          tsset(nsor,istn,1) = st0cur(istn) + frac*(86400.d0-236.d0)
!          if (tsset(nsor,istn,1).gt.TWOPI)
!     .    tsset(nsor,istn,1)=tsset(nsor,istn,1)-TWOPI
           tsset(nsor,istn,1)=twopi 
        else if (.not.kup) then ! never up
!         tsris(nsor,istn,1) = st0cur(istn)
!         tsset(nsor,istn,1) = st0cur(istn)
!         ntsrisset(nsor,istn) = 1
          tsset(nsor,istn,1)=0.d0 
        endif
         ntsrisset(nsor,istn) = 1
C  Handle the case where the source is already up at the start of
C  the day, then sets, then rises again.
      else if (ns.gt.1) then ! more than one rise/set
C       if ((tsris(nsor,istn,1).gt.tsset(nsor,istn,1).and.
C    .      tsris(nsor,istn,1).lt.tsris(nsor,istn,2)).or.
C    .      (tsris(nsor,istn,1).lt.tsset(nsor,istn,1).and.
C    .      tsris(nsor,istn,2).gt.tsset(nsor,istn,2))) then
C Simpler logic. If the first set is earlier than the first
C rise, then the list needs cycling.
        if ((tsris(nsor,istn,1).gt.tsset(nsor,istn,1))) then
          setsave=tsset(nsor,istn,1)
          do i=1,ns-1
            tsset(nsor,istn,i)=tsset(nsor,istn,i+1)
          enddo
          tsset(nsor,istn,ns)=setsave
        endif
      endif

      end
