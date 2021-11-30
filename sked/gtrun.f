      subroutine gtrun(idirpr,nspre,lcbpre,iftpre,icodpr,itupr)

C GTRUN calculates additional information needed for
C       continuous or adaptive tape motion.
C       It is called after GTOBS. You must call
C       GTPRE before calling GTOBS to save information
C       from the previous scan.

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/skobs.ftni'
      include '../skdrincl/statn.ftni'

C History
C 970401 nrv New.
C 970403 nrv Add iftpre to call. Check footages for ADAPTIVE.
C 970406 nrv Add itupr to call.
C 970715 nrv Calculate itrun for the bot/eot case as the running time
C            for the tape to move from end of last scan on the pass
C            to bot/eot

! 2021-02-19 JMG slewt now returns az_now, az_new 
! 2008Jun18 JMGipson.  Modified calculation of ituse for CONTINUOUS

C Called by: SUMCM, CHCMD, LICMD

C Input:
      integer nspre(max_stn),idirpr(max_stn),iftpre(max_stn),
     .icodpr(max_stn),itupr(max_stn)
      integer*2 lcbpre(max_stn)
      integer isecdif ! function

C Output:
C     itrun, idurxt, and itucur are set in common

C Local:
      integer idft,i,j,blnk,look,mjdend
      real tslew,trise
      double precision utend
      integer islew_info        !info about slewing
      real az_now,az_new 

C  1. Initialize

      do i=1,nstncur  ! all stations
        j=istcur(i)
        itucur(j)=1
        itrun(j)=0
        idurxt(j)=0
        if(tape_motion_type(j) .eq. 'CONTINUOUS') then
          itucur(j)=0
        else if (nspre(j).gt.0) then !initialized
            CALL SLEWT(NSPRE(J),mjdstart(J),utstart(J),NSORcur(J),J,
     >      LCBPRE(J),BLNK,TSLEW,look,trise,tsris,st0cur,frac,
     >      knov,islew_info,az_now,az_new) 
C         UTCUR is the mutal good data start time.
C         UTSTART is the time this station has data start on the new source
C         This may be earlier than UTCUR for continuous or adaptive motion.
            utend=utstart(j)
            mjdend=mjdstart(j)
            call addsec2ut(mjdstart(j),utstart(j),
     >         isortm+nint(tslew)+icalcur(j),mjdstart(j),utstart(j))
C         If the tape is at BOT or EOT then CUR=START
            if (iftcur(j).eq.0.or.iftcur(j).eq.maxtap(j)) then ! at bot/eot
              itucur(j)=1
            else ! not at bot/eot
              itucur(j)=0
              idurxt(j) = isecdif(mjdcur(j),utcur(j),
     .                           mjdstart(j),utstart(j))
              if (idurxt(j).lt.0) idurxt(j)=0
              itrun(j) = isecdif(mjdcur(j),utcur(j),
     .                           mjdend,utend)
              if (itrun(j).lt.0) itrun(j)=0
            endif ! bot/eot         
        else                            !not initialized
          if((tape_allocation(j) .eq. "AUTO"  .or.
     >        tape_allocation(j) .eq. "SCHEDULED")
     >       .and. iftcur(j) .ne. 0) then
             itucur(j)=0
          endif
        endif ! initialized
        if(itucur(j) .eq. 1) then
!            write(*,*) "here"
        endif
        ituse(j)=itucur(j)
      enddo  ! all stations

      return
      end
