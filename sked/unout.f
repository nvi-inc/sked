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
C
      IERR = 0
      WRITE(LUSCN,9100)
9100  FORMAT('Unchanged sections from original file:')
C
      CALL READS(luskd,IERR,IBUF,IBLEN,ILCHAR,1)
      DO WHILE (ILCHAR.GE.0.AND.IERR.GE.0) !read original file
        IF (cbuf(1:6) .eq.'$EXPER'  .OR.   !These are always written out before here.
     .      cbuf(1:6) .eq.'$PARAM'  .OR.
     >      cbuf(1:6) .eq.'$ASTRO'  .OR.
     >      cbuf(1:16) .eq.'$TWIN_TELESCOPES'  .OR.
     >      cbuf(1:6) .eq.'$GROUP'  .OR.
     >      cbuf(1:7) .eq.'$STATWT'  .OR.
     >      cbuf(1:6) .eq.'$SRCWT'  .OR.
     >      cbuf(1:6) .eq.'$MINOR'  .or.
     >      cbuf(1:6) .eq.'$MAJOR'  .or.
     >      cbuf(1:9) .eq.'$DOWNTIME' .or.
     >      cbuf(1:3) .eq.'$OP'     .or.
     >      cbuf(1:14) .eq.'$CATALOGS_USED' .or.
     >      cbuf(1:10) .eq.'$BROADBAND' .or.
     >      cbuf(1:6) .eq. '$PROCS' .or. 
     .     (cbuf(1:7) .eq.'$SOURCE' .AND.KNEWSO).OR.  !these may or may not be.
     .     (cbuf(1:8) .eq.'$STATION'.AND.(KNEWST.or.Knewfi)).OR.
     .     (cbuf(1:6) .eq.'$CODES'  .AND.KNEWFR).OR.
     .     (cbuf(1:5) .eq.'$VLBA'   .and.knewfr).OR.
     .     (cbuf(1:5) .eq.'$HEAD'   .and.knewfr).OR.
     .     (cbuf(1:5) .eq.'$SKED'   .AND.KNEWSK).or.
     .     (cbuf(1:5) .eq.'$FLUX'   .AND.KNEWFL)
     .                           ) THEN  !get next $ section
           CALL READS(luskd,IERR,IBUF,IBLEN,ILCHAR,1)
        ELSE  !copy this section
          if(ilchar .ne. 0) then         !skip blank lines
            write(luscn,'(a)') cbuf(1:20)
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
