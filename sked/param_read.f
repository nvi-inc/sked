      subroutine param_read(luin)

C   PRREAD reads the lines in the $PARAM section.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C History
C 000326 nrv Removed from SKOPN.
C 000605 nrv Add call to ATAPE.
C 001003 nrv Clear buffer before moving parameters to front. With
C            the extra-long tape_allocation command there are letters
C            left at the end.
C 020713 nrv Get lines from both vex or sk file. Add kvex parameter.
! 2012Sep25  JMG.  Get rid of VEX stuff. 
! 2019Feb05  JMG. Ignore comment lines. 

C Input
      integer luin 
 
C Local
      integer ilen,ic1,ic2,ich,ncout,idummy    
      integer ichmv,i2long,trimlen
      integer fget_literal,iret,ptr_ch,fget_all_lowl
      integer nch

! Start at the beginning of the file.  
      rewind(luin)
      cbuf="NOT PARAM"

! Read until we get to the the start of the $PARAM section. 
      do while(cbuf .ne. "$PARAM")
        read(luin,'(a)',end=900) cbuf
      end do
    

C  Loop on parameter section lines
      DO WHILE (.true.) !decode an entry
10      continue        
        read(luin,'(a)',end=500) cbuf
        if(cbuf(1:1) .eq. "*") goto 10
        if(cbuf(1:1) .eq. "$") goto 500    !Exit if we hit another section
        ibufq(1)=trimlen(cbuf)

        ICH=1
        CALL GTFLD(IBUF,ICH,i2long(IBUFQ(1)),IC1,IC2)
        nch=ic2-ic1+1
        IF  (cbuf(1:nch) .eq.'SUBNET') THEN  !SUB line
          CALL PRSET(IBUFQ)
        ELSE IF (cbuf(1:nch) .eq.'SCAN') THEN !SCAN line
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL SSCAN(IBUFQ,'o')
        ELSE IF (cbuf(1:nch) .eq.'WEIGHT') THEN 
! This is obsolete command.
        ELSE IF (cbuf(1:nch) .eq.'TAPE_TYPE') THEN 
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL TTAPE(IBUFQ,luscn,ludsp)
        ELSE IF (cbuf(1:nch) .eq.'TAPE_MOTION') THEN 
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL STAPE(IBUFQ,luscn,ludsp)
        ELSE IF (cbuf(1:nch) .eq.'TAPE_ALLOCATION') THEN 
          cbuf(1:15) =" "  !this blanks out "TAPE_ALLOCATION"
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL ATAPE(IBUFQ,luscn,ludsp)
        ELSE IF (cbuf(1:nch) .eq.'ELEVATION') THEN 
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL SELEV(IBUFQ,luscn,ludsp)
        ELSE IF (cbuf(1:nch) .eq.'EARLY_START') THEN 
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL SEARL(IBUFQ,luscn,ludsp)
        ELSE IF (cbuf(1:nch) .eq.'SNR') THEN !SNR
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL SNRCM(IBUFQ,'o',' ')
        ELSE IF (cbuf(1:nch) .eq.'SNR_1') THEN !SNR_1
          IDUMMY = ICHMV(IBUF,1,IBUF,ic2+2,i2long(IBUFQ(1))-ic2-1)
          IBUFQ(1)=IBUFQ(1)-ic2-1
          CALL SNRCM(IBUFQ,'o','1')
        ELSE
          CALL PRSET(IBUFQ)
        ENDIF  
      enddo

! Come here when we are done reading the $PARAM section 
500   continue
      return

900   continue
      write(luscn,'(a)') "Param_read: Never found $PARAM section!"
      return 
      

      return
      end
