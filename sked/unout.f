      SUBROUTINE unout

C  unout writes out sections of the schedule that hasn't changed. 
C
C   HISTORY:
C     GAG   891116 CREATED
C     GAG   891121 Changed ISCUN(2) to lutmp
C 990915 nrv Replace REIO with WRITE
C 2004Jan27  JMG Got rid of filling with blanks before "Call READS" since READS blank fills already
! 2005Aug17  JMG.  Skip writing blank lines if any in the file.
! 2010Apr22  JMG. Changed so that Statwt, srcwt are not written out again. 
! 2012Nov19  JMG. Changed so that $CATALOG_USED is not written out again. 
! 2015Mar31  JMG.  Don't write out $PROCS section. This is done elsewhere. 
! 2021-12-28 JMGipson. Don't write out $MAX_STAT_SCAN section

C
C   parameter file
      include '../skdrincl/skparm.ftni'
C
C   CALLED BY: SKCLS
C   CALLED: READS, WRITF_ASC, READF_ASC, IFILL
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
C
C LOCAL:
      integer ierr,ilchar
      integer il
      character*30 lsection
C
      IERR = 0
      WRITE(LUSCN,9100)
9100  FORMAT('Unchanged sections from original file:')
C
      CALL READS(luskd,IERR,IBUF,IBLEN,ILCHAR,1)
      DO WHILE (ILCHAR.GE.0.AND.IERR.GE.0) !read original file
        read(cbuf,*) lsection
        IF (lsection  .eq.'$EXPER'  .OR.   !These are always written out before here.
     .      lsection  .eq.'$PARAM'  .OR.
     >      lsection  .eq.'$ASTROMETRIC'  .OR.
     >      lsection .eq.'$TWIN_TELESCOPES'  .OR.
     >      lsection .eq.'$MAX_STAT_SCAN'    .or. 
     >      lsection  .eq.'$GROUP'  .OR.
     >      lsection  .eq.'$STATWT'  .OR.
     >      lsection  .eq.'$SRCWT'  .OR.
     >      lsection  .eq.'$MINOR'  .or.
     >      lsection  .eq.'$MAJOR'  .or.
     >      lsection  .eq.'$DOWNTIME' .or.
     >      lsection  .eq.'$OP'     .or.
     >      lsection .eq.'$CATALOGS_USED' .or.
     >      lsection .eq.'$BROADBAND' .or.
     >      lsection  .eq.'$PROCS' .or. 
     .     (lsection  .eq.'$SOURCES' .AND.KNEWSO).OR.  !these may or may not be.
     .     (lsection  .eq.'$STATIONS'.AND.(KNEWST.or.Knewfi)).OR.
     .     (lsection  .eq.'$CODES'  .AND.KNEWFR).OR.
     .     (lsection  .eq.'$VLBA'   .and.knewfr).OR.
     .     (lsection  .eq.'$HEAD'   .and.knewfr).OR.
     .     (lsection  .eq.'$SKED'   .AND.KNEWSK).or.
     .     (lsection  .eq.'$FLUX'   .AND.KNEWFL)
     .                           ) THEN  !get next $ section
!           write(*,*) "Skipping ", trim(cbuf) 
           CALL READS(luskd,IERR,IBUF,IBLEN,ILCHAR,1)
        ELSE  !copy this section
          if(ilchar .ne. 0) then         !skip blank lines
            write(luscn,'(a)') trim(cbuf) 
            write(lutmp,'(a)') cbuf(1:ilchar)
            
          endif
          IF (IERR.NE.0) THEN
            WRITE(LUSCN,9110) IERR
9110        FORMAT('ERROR',I5,' writing schedule: re-issue EC,ER,WR, or'
     .     ' WC')
            CLOSE(lutmp)
            RETURN
          END IF
          CALL READF_ASC(luskd,IERR,IBUF,IBLEN,IL)
          ILCHAR=IL*2
          DO WHILE (cbuf(1:1) .ne. "$".AND.IL.GT.0)
            write(lutmp,'(a)') cbuf(1:ilchar)
            IF  (IERR.NE.0) THEN 
              WRITE(LUSCN,9110) IERR
              CLOSE(lutmp)
              RETURN
            END IF
            CALL READF_ASC(luskd,IERR,IBUF,IBLEN,IL)
            ILCHAR = IL*2
          END DO
        END IF  !copy this section
      END DO  !read original file
      CLOSE(lutmp)
C
      RETURN
      END
