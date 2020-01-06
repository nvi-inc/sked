      SUBROUTINE nextpr(LINSTQ,nst,istn,kmin,ierr)
C
C    NEXTpr decodes the parameter line for the WHATSUP command
C
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include 'major.ftni'
      include 'covar.ftni'

! fucntion
      integer iStringMinMatch
      integer iwhere_in_int_list
C
C     CALLING ROUTINES: NEXTC
C
C  INPUT:
      integer*2 LINSTQ(*)
      integer nst,istn(max_stn)
C
C  OUTPUT
      integer ierr
      logical kmin

C  LOCAL VARIABLES
      integer i,j,ikey,nchar,ich,ic1,ic2
      integer*2 lkeywd(12)
      character*10 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ipar,no,ne,idum,ichmv,i2long
      integer itemp
      logical koe

      integer ilist_len
      parameter (ilist_len=3)
      character*8 list(ilist_len)
      data list/"FULL","MINIMUM","NO"/
C
C
C  History
C    DATE   WHO    CHANGES
C  930319   nrv    Created
C  930324   nrv    check that all parameters being optimized are also
C                  being estimated
C  950421   nrv    Let GTSTI write error messages.
C 951017 nrv Change igtky call to use lkey
C 951017 nrv Fixed gtfld call to remove linstq
C 960415 nrv Don't check parameters if coverage only
! 2019.05.23  JMG.  If AUTO mode is on, make sure all stations are in the subnet. 
C
      ierr=0
C
C     1. First get the subnet.
C     
      ICH = 1
      CALL GTSTI(LINSTQ,ICH,NST,ISTN,IERR,luscn)
      IF (IERR.NE.0) THEN  
C       CALL WRERR(IERR,INUMCM)
        RETURN
      END IF  

      IF  (NST.EQ.0) THEN 
        NST = NSUBST
        DO  I = 1,NST
          ISTN(I) = ISUBST(I)
        END DO
      END IF     
C
C     2. Next check for "FULL", "MIN", or "NO" key word.
C
      KMIN = .TRUE.
      kdiswh = .true.
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
      IF (IC1.NE.0) THEN !check key word
        ckeywd=" "
        idum= ICHMV(LKEYWD,3,LINSTQ(2),IC1,IC2-IC1+1)
        ikey=istringminmatch(list,ilist_len,ckeywd)
        IF (IKEY.LE.0) THEN !error
          ierr=1
          WRITE(LUSCN,8901)
8901      FORMAT('NEXTPR01 - Key word must be FULL, MIN, or NO.')
          RETURN
        ENDIF !error
        if(list(ikey) .eq. "FULL") kmin=.false.
        if(list(ikey) .eq. "MINIMUM") kmin=.true.
        if(list(ikey) .eq. "NO")  kdiswh=.false.
      ENDIF !check key word
C
C     3. Now get the ending time
C
      nchar=linstq(1)
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IC1,IC2)
      kauto=.false.
      if(ic1.gt.0) then !ending time field means autosked
        CALL IFILL(LKEYWD(2),1,12,oblank)
        idum= ichmv(lkeywd(2),1,linstq(2),ic1,nchar-ic1+1)
        lkeywd(1)=nchar-ic1+1
        CALL GTDTR (lkeywd,IERR)
        IF (IERR.NE.0) then
          CALL WRERR(IERR,INUMCM)
          return
        endif
        kauto=.true.
        koe=.true.
        no=0
        ne=0
        if (.not.kOptBySky) then ! optimize for parameters, not coverage
        do ipar=1,max_dim_esti
          if (lpara(ipar,1)) no=no+1
          if (lpara(ipar,2)) ne=ne+1
          if (lpara(ipar,1).and..not.lpara(ipar,2)) koe=.false.
        enddo
        if (ne.gt.max_par_esti) then
          write(luscn,9101) ne,max_par_esti
9101      format('NEXTPR02 - ',i3,' is too many parameters to ',
     .    'estimate. Maximum is ',i3,'.')
          ierr=1
          return
        endif
        if (no.gt.max_par_opti) then
          write(luscn,9102) no,max_par_opti
9102      format('NEXTPR03 - ',i3,' is too many parameters to ',
     .    'optimize. Maximum is ',i3,'.')
          ierr=1
          return
        endif
        if (.not.koe) then
          write(luscn,9100)
9100      format('NEXTPR04 - All parameters to be optimized must also',
     .    ' be estimated.')
          ierr=1
        endif
        endif ! optimize for parameters, not coverage
      endif ! ending time field means autosked

! If in AUTO mode need to do some cleanup. Make sure all stations are in ISUBST which is set in major.
! Do this by copying istn back to itself, omitting stations which are not in isubst  
      if(kauto) then  
        j=0
        do i=1,nst
          itemp=iwhere_in_int_list(isubst,nsubst,istn(i))
          if(itemp .ne.0) then
             j=j+1
             istn(j)=istn(i)
          endif 
        end do
        nst=j
      endif 

C
C
      return
      end
