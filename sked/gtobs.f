C@GTOBS
      SUBROUTINE GTOBS(KSTART,KRWND,KGOT,KERR)  !GET NEXT OBSERVATION 
C*********************SELECTION BY ISORCM NOT DONE!                         
C                                                                           
C   GTOBS gets the next valid observation.                                  
C     The selection criteria are time range, number of                      
C     observations, and source name.                                        
C     If an observation does not meet these criteria, it                    
C     is not selected.  However, the observations all                       
C     "pass through" the CUR variables so that the subnet                   
C     information is always up to date.                                     
C     The file is left positirecordd just AFTER the record which applies    
C     to the CURRENT observation.                                           
C                                                                           
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
C
C  INPUT VARIABLES:                                                      
      LOGICAL KSTART                                                        
C              - TRUE if we must initialize counters, etc., i.e., the       
C                first call of a loop of requests                           
      LOGICAL KRWND                                                         
C               - TRUE if we should start searching for valid records       
C                 at the beginning of the file.  FALSE for starting at      
C                 the current observation.                                  
C                                                                           
C  OUTPUT VARIABLES:
      LOGICAL KGOT
C               - Returned as TRUE when there is a valid record in the
C                 CUR variables.  FALSE if no records are found, or if
C                 we hit the end of the requested range.
      integer kerr
C
C    COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'skcom_vec.ftni'
C
C     CALLING SUBROUTINES: LICMD,CHCMD, ETC.
C     CALLED SUBROUTINES: FMP subroutines: READF, RWNDF
C
C  LOCAL VARIABLES
      integer i,j,iunerr,is
      integer mjd1,mjd2
C     J - index to the first station in the current subnet
      LOGICAL KDTLT,KDTLE
C           - compare two date/times                                        
      real*8 UT1,UT2
C           - UTs for comparison
      logical knot_obs

C
      KDTLT(MJD1,UT1,MJD2,UT2) = (MJD1.LT.MJD2).OR.
     .                           (MJD1.EQ.MJD2.AND.UT1.LT.UT2)
      KDTLE(MJD1,UT1,MJD2,UT2) = (MJD1.LT.MJD2).OR.
     .                           (MJD1.EQ.MJD2.AND.UT1.LE.UT2)
C         - true if date/time1 is earlier than date/time2
C
C     880310 NRV DE-COMPC'D
C     890425 NRV Initialize summary variables
C     891109 GAG Put the break call, IFBRK, back in action
C     910227 NRV Cleaned up indentation.
C     930219 nrv merge sked/autosked: add nsubc to UNPAK calls
C     930803 nrv Initialize nsorobs upon rewind
! 2010Mar26 JMG. Changed stutcm ->utstcm, enutcm->utencm for consistency with jdstcm and jdencm
!        Also changed start scan to be => Utstcm, jdstcm.  Previously was strictly >. Hence if 
!        you did an li 170000 and the first scan was at 170000, you wouldn't list it.

C     1. First make sure that sources and stations have been selected.
C
      KGOT = .FALSE.
      IF (NOBS.EQ.0) then
        DO I=1,NSOURC
          NOBSSO(I)=0
          UTPRSO(I)=0
          MJPRSO(I)=0
        ENDDO 
        RETURN
      endif
      IF  (NSTATN.EQ.0.OR.NSOURC.EQ.0) THEN  !
          KERR = 23
          RETURN
      END IF  !                                                         

C                                                                           
C     2. If this is the first request, then initialize record count.        
C     Rewind if necessary and initialize everything. 
C     Initialize summary variables here.         
C
      J = ISTCUR(1) 
      IF (NSTNCUr.EQ.0) THEN  !starting fresh
! AEM 20041228 if j==0 we've got an bounds error in array
!              so as it must be set to 1 at least
        J = 1
        IRECGO=1
        IRCUR=MAX_OBS
        DO I=1,NSOURC
          NOBSSO(I)=0
          UTPRSO(I)=0
          MJPRSO(I)=0
        ENDDO 
      END IF  !starting fresh 
C
      IF  (KSTART) THEN  !starting to get a range
! initialize things to the start of the experiment.
! if we are actually further along, this will be fixed as we run through the obs.
        IRCNT = 0  
        KSTART = .FALSE.
        IF (KRWND.OR.(IRECGO.GT.0.AND.IRECGO.LT.IRCUR).OR.               
     >     (IRECGO.EQ.0.AND.KDTLT(JDSTCM,UTSTCM,MJDCUR(J),UTCUR(J))))
     &      THEN  !need to rewind

         call init_time_arrays(iyr_start,ida_start,ihr_start,
     >       imin_start,isc_start)
          IRCUR = 0
          DO I=1,NSTATN
! initialize "current variables"
            NSORcur(I)= -1
            IFTCUR(I)=  0
            itucur(i)=  1
            IPAScur(I)= 1
            IDIRcur(I)= 1
            ICODcur(I)= 0
          END DO
          KRWND = .FALSE.
          DO I=1,NSOURC
            NOBSSO(I)=0
            do is=1,max_baseline
              nsorobs(i,is)=0
            enddo
            UTPRSO(I)=0
            MJPRSO(I)=0
          ENDDO                                               
        END IF  !need to rewind                                     
C                                                                           
C     2. If IRECGO is set to non-zero, then go to that record number.       
C     Set back to zero when done.                                           
C                                                                           
        IF (IRECGO.GT.0) THEN  !going to specific record number
          iunerr = 0
          DO WHILE (IRCUR.LT.IRECGO.AND.IRCUR.LT.NOBS)    
            IRCUR = IRCUR+1
            cbuf=cskobs(iskrec(ircur))
            CALL UNPAK(IUNERR,0)        !move into Curs variables
            call copy_cur2vec() !and from curs into vec.
          END DO
          IF (IUNERR.NE.0) THEN ! requested record is bad
            write(luscn,'(a)') "GTOBS10 - Invalid record requested."//
     >         "Current observation not changed."
            RETURN
          END IF  
          IRECGO = 0                                                    
          IRCNT = 1                                                     
          KGOT = .TRUE.                                                 
          RETURN                                                        
        END IF  !going to specific record number                    
C                                                                           
C     3. If it happens that the current record is wanted, then we           
C     don't need to read anything because all is resident in common.        
C                                                                           
        IF (JDSTCM.EQ.MJDCUR(J).AND.UTSTCM.EQ.UTCUR(J)) THEN !current is resident
          IF (NSORcur(J).EQ.0) RETURN 
          KGOT = .TRUE.
          IRCNT = 1                                                     
          RETURN                                                        
        END IF !current is resident
C                                                                           
C     4. If we got to here, then we have to read up until the               
C     starting date/time.                                                   
C                                                                           
        IF (IRCUR.LT.NOBS) IRCUR = IRCUR+1
        cbuf=cskobs(iskrec(ircur))
        CALL UNPAK(IUNERR,0)            !move into Curs variables
        call copy_cur2vec()   		!and from curs into vec.

        J = ISTCUR(1)
        DO WHILE (IRCUR.LT.NOBS.AND.                        
     >    KDTLE(MJDCUR(J),UTCUR(J),JDSTCM,UTstCM)) !read up to start
          IRCUR = IRCUR+1
          cbuf=cskobs(iskrec(ircur))

          CALL UNPAK(IUNERR,0)        	!move into Curs variables
          call copy_cur2vec() 	     	!and from curs into vec.
          J = ISTCUR(1)
        END DO  !read up to start                                   
        IF (IUNERR.NE.0) THEN
          write(luscn,'(a)') "GTOBS11 - Invalid record requested."//
     >      "  Current observation left at previous record."
        RETURN
        END IF  !                                                    
        IRCNT = 1                                                        
        KGOT = .TRUE.                                                    
C                                                                           
C                                                                           
C     5. At this point, we have been called again, to continue              
C     getting records.                                                      
C                                                                           
      ELSE !continuing to get a range
        do while (.not.kgot)
          J = ISTCUR(1)  
C  If we have retrieved the desired number of obs, then get no more.
          IF (NOBSCM.GT.0.AND.IRCNT.GE.NOBSCM) RETURN 
C  If the current observation is exactly at the requested ending time,
C  then get no more.       
          IF (NOBSCM.EQ.0.AND.KDTLE(JDENCM,UTenCM,MJDCUR(J),UTCUR(J)))
     .    return
C**NEW**NOT IMPLEMENTED YET.
C  It may be desireable to have GTOBS quit returning observations when
C  it has reached the observation that comes close to, but does not
C  overshoot, the ending time. Now, GTOBS must actually unpack the
C  observation, which updates the CUR variables, before it can check
C  whether the ending time has been exceeded. There is no way to "unget"
C  an observation once the CUR variables have been updated.
          IF (IRCUR.GE.NOBS) RETURN 
C         Move the next record into the buffer for unpacking.
          IRCUR = IRCUR+1
          cbuf=cskobs(iskrec(ircur))

          CALL UNPAK(IUNERR,0)        !move into Curs variables
          call copy_cur2vec() !and from curs into vec.

          IF (IUNERR.EQ.0)  THEN ! check the observation
            IRCNT = IRCNT + 1                                                 
            KGOT = .TRUE.                                                     
            RETURN
          ENDIF
        enddo
      END IF  !starting/continuing to get a range
C                                                                           
      RETURN                                                                
      END                                                                   
