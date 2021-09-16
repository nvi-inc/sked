C
      subroutine chstn
C
C     CHSTN - checks station info for completeness
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'

C HISTORY:
C nrv 930315 Changed error message name to CHSTN
C 990524 nrv Initialize tape_length, tape_dens from station info.
C 990621 nrv Remove tape_length, tape_dens

C LOCAL:
      integer i
C
      IF  (NSTATN.GT.0) THEN 
        DO  I=1,NSTATN
          IF  (Slew_RATe(2,I).EQ.0.0) THEN  !
            write(luscn,"(a,a8)")
     >     "CHSTN04 - Antenna information not complete for ",cantna(i)
            goto 900
          END IF  !
          if (stnpos(2,i).eq.0.0) then
            write(luscn,"(a,a8)")
     >     "CHSTN05 - Position information not complete for ",cstnna(i)
            goto 900
          endif
          if (cterna(i) .eq. '  ') then
            write(luscn,'(a,a8)')
     >      "CHSTN06 - Missing or inconsistent DAT information for ",
     >      cantna(i)
            write(luscn,'("Default tape length and passes used.")')
            maxtap(i)=MAX_TAPE
            maxpas(i)=MAX_PASS
          endif
          if (maxtap(i).eq.0) maxtap(i)=MAX_TAPE
          if (maxpas(i).eq.0) maxpas(i)=MAX_PASS
C       Set tape type defaults.
C         if (tape_length(i).eq.0) tape_length(i)=maxtap(i)
C         if (tape_dens(i,1).eq.0) tape_dens(i,1)=bitdens(i,1)
        END DO  !
      endif
      return

900   continue
      write(luscn,"('CHSTN - Can''t schedule.  No stations selected.')")
      NSTATN = 0
      RETURN

      end

