      DO  I=1,NSTN !get latest start time
        J = ISTN(I)   
        ! Do some calculations.
        if(mjdcmd .ne. -1) then    
! observation time specified.
          mjdbeg=mjdcmd
          utbeg =utcmd 
        else
! no time specified.  Initialize to current time of first station in scan. 
           iStat1 = ISTN(1)
           mjdbeg=mjdcur(istat1)
           utbeg= utcur(istat1)
        endif              
       
        call when_at_next_source2(j,nsorcur(j),nsor,mjdbeg,utbeg,
     >   idur(j),idle(j),ical,iset,
     >   cwrap(j),cwrap_new(j),tslew,imaxsl,mjd_out(j),ut_out(j),
     >    az_now,az_new,el_now,el_new,isrc_time, buf_time,ierror)      
     
        write(*,*) cstnna(j), mjd_out(j), ut(j), mjd_out(j), ut_out(j)

        IF (mjd_scan.EQ.MJD_out(J)) ut_scan = DMAX1(ut_scan,UT_out(J))
C                   If dates are equal, pick up the latest time-of-day
        IF (MJD_out(J).GT.mjd_scan) THEN  !got a later time
          mjd_scan = MJD_out(J)
          ut_scan = UT_out(J)
        END IF  !got a later time
      end do 
      stop 
