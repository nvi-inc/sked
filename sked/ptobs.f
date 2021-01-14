      SUBROUTINE PTOBS(CFUNC,NREC,KERR)
CHS----------------------------------------------------------------
CHS Added calls to simul and inv, but just for inserting and 
CHS deleting of observations.
C
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C PTOBS edits the schedule file with functions copy, delete,
C              insert, replace.
C
      include '../skdrincl/skparm.ftni'
C
C INPUT VARIABLES:
      character*2 cfunc
      integer nrec
C        CFUNC  - Function 2-char code: DE(delete),
C                 IN(insert), RE(replace).
C        NREC   - Number of records for delete.

C OUTPUT VARIABLES:
      integer kerr
C
C COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/skobs.ftni'
C
C CALLING SUBROUTINES: NEWOB,DELCO, AUCHK
C Called: PAKUP,SIMUL,INV
C
C LOCAL VARIABLES
      integer ilen,i,kk
C
C  DATE  WHO  WHAT
C 841018 MWH  IMPLEMENTED POINTER ARRAY FOR WORK FILE
C 880311 NRV  DE-COMPC'D
C 880525 PMR  revised for workstation
C 900328 gag  added time ordering
C 930219 nrv  merge sked/autosked: ichmv calls change from 128 to ibuf,
C             time change to sktime
C 910911 NRV Removed dleq, added inv and indx4
C 921005 NRV Add last parameter to SIMUL call
C 930225 nrv implicit none
C 950328 nrv Change test from npara=0 to kopgo=true
C 021209 JMG A little cleanup
! 2005Nov30 JMGipson.  Previously sorted obs internally.
!           Now does with a call.
! 2010Mar15 JMGipson. Modified so that if it deletes an observation, leaves
!            counter pointing to the correct location.
C
      KERR = 0
      KNEWSK = .TRUE.
      IF  (CFUNC.EQ.'RE') THEN  !
!        call copy_cur2vec()
        CALL PAKUP(ILEN,0)
        cskobs(iskrec(ircur))=cbuf
      END IF  !
C
      IF(CFUNC.EQ.'IN' .or. CFUNC .eq. 'AU') THEN  !
         IF  (NOBS.GE.MAX_OBS) THEN  !
           KERR = 30
           RETURN
         END IF  !
         IRCUR = IRCUR+1
         IF  (IRCUR.LE.NOBS) THEN  !
           DO  I=NOBS,IRCUR,-1
             ISKREC(I+1) = ISKREC(I)
           END DO
         END IF  !
         ISKREC(IRCUR) = NXTREC
         NXTREC = NXTREC+1
         NOBS = NOBS+1
!         call copy_cur2vec()
         if(CFUNC .eq. 'IN') then
!           call copy_cur2vec()
           CALL PAKUP(ILEN,0)
         endif
         cskobs(iskrec(ircur))=cbuf

C  Why call simul if we're not in the optimization mode?
         if (kopgo) then
           call simul(0,iskrec(ircur),1,.true.,.false.)
         endif
C
C  This next section orders the index array, iskrec, in time order.
C
         call obs_sort(luscn,ircur)  !sort obs 1:ircur in time order.
       END IF  !

      IF(CFUNC.EQ.'DE') THEN  ! delete
        kk = MIN(NREC,NOBS)
        kk = nrec
!       If in auto mode, remove from matrix
        if (kopgo) then
          do kk=0,nrec-1
            cbuf=cskobs(iskrec(ircur+kk))
            call simul(0,iskrec(ircur+kk),-1,.true.,.false.)
          enddo
        endif  
        IF  (ircur.gt.0.and.NREC.LE.NOBS-IRCUR) THEN     
          DO  I=IRCUR,NOBS-NREC
            ISKREC(I) = ISKREC(I+NREC)
          END DO
        END IF  !
        NOBS = NOBS-MIN0(NREC,NOBS-IRCUR+1)
        IF (NOBS.EQ.0) IRCUR = 0
        if(ircur .gt. 1) ircur=ircur-1
      END IF  ! delete

      END
C
