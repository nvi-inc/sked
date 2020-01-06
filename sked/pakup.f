C@PAKUP
C
      SUBROUTINE PAKUP(ILEN,nsubc)
CHS------------------------------------------------------------------------
CHS Pakup was extended for the parameter nsubc in the parameter list. This 
CHS parameter can either be 0 (insert/delete mode) or 1,2,3 or 4 (optimization
CHS mode).
C
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C   PAKUP packs up CUR variables into IBUF
C
      include '../skdrincl/skparm.ftni'
C
! funcitons
      integer trimlen
      integer ibnum,ib2as,ichmv

C  INPUT:
      integer nsubc

C  OUTPUT VARIABLES:
      integer ILEN ! length of output buffer in words
C
C  COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
C
C   LOCAL VARIABLES
      integer iyr,ihr,imin,isc,idurx,i,j,isor,nch,idumy,ib
      integer*2 lc
      integer ibllen
      integer maxdu
      double precision UT
      character*1 lchar
      character*1 cbl
      equivalence (lc,cbl)
C          - local variable to hold current UT
C
C   MODIFICATIONS
C   880315 NRV DE-COMPC'D
C   890426 NRV Added incrementing summary variables for WHASTUP display
C   890516 NRV Duration is max of all station durations
C   890526 NRV Number of words in output buffer is (NCH+1)/2
C   90.... gag write out footages with 4 or 5 digits
C   930219 nrv merge sked/autosked
C   930225 nrv implicit none
C   930401 nrv removed octal constants from ib2as calls
C   930715 nrv increment nsorobs
C   940415 nrv Write out durations with 5 spaces, allowing for long
C              scans on thin tapes (max 1568 seconds)
C   950515 nrv Generate durations in separate buffers, then move into 
C              the long buffer. IB2AS can only handle buffer index
C              values up to 256!
C 960325 nrv Same as above for tape footages!
C 970627 nrv Write source names with actual lengths.
C 980217 nrv Write source names with actual lengths, minimum 8.
C 981113 nrv Write out 2-digit year. Use mod(year,100).
C 990528 nrv Make tape footages 6 digits to accommodate S2 tapes.
C 000510 nrv Always pass 0 for S2, pass 1 for K4.
C 000601 nrv Make durations 6 characters for very long S2 scans.
C 000602 nrv Leave S2, K4 as they are. Handle S2 always "pass" 0 in drudg.
! 2005Jun13 JMgipson.  Modified to add in duration for UTPRSO.
C
C
C     1. All we do is code the CUR variables into
C        the output buffer IBUF.
C
C        ****NOTE****
C        This is the subroutine which effectively defines the output
C        file format.  It must agree with the subroutine UNPAK, which
C        decodes this format.  SEE UNPAK FOR THE FORMAT.
C
      ibllen=2*4*max_stn
C     First clear out the entire buffer
      cbuf=" "
C     The index into the CUR variables will be the
C     first station in the current subnet.
C 
CHS------------------------------------------------------------------
CHS Insert/delete mode
CHS The CUR-variables are packed up into ibuf.
C
      if(nsubc.eq.0) then ! insert/delete mode
C
      J = ISTCUR(1)
      ISOR = NSORcur(J)
      UT=UTCUR(J)
      i=max(8,trimlen(csorna(isor)))
      cbuf=csorna(isor)(1:i)
      nch=i
C  Increment source summary info
      NOBSSO(ISOR)=NOBSSO(ISOR)+1
      UTPRSO(ISOR)=UT +idurx
      MJPRSO(ISOR)=MJDCUR(J)
      IDUMY = IB2AS(ICALcur(J),IBUF,NCH+1,4)
      NCH = NCH + 6
      NCH = ICHMV(IBUF,NCH,LCODE(ICODcur(J)),1,2)
      cbuf(nch+1:nch+6)=cprecur(j)
      nch=nch+8

      iyr = mod(iyrcur(j),100)
      call seconds2hms(ut,ihr,imin,isc)
      write(cbuf(nch:nch+10),'(i2.2,i3.3,3i2.2)')
     >   iyr,idacur(j),ihr,imin,isc
      nch=nch+12

      IDurx = MAXDU(IDURCUr,nstncur,ISTCUR)
      NCH = NCH + 1+IB2AS(IDurx,IBUF,NCH+1,6)
      cbuf(nch+1:nch+6)=cmidcur(j)
      nch=nch+8

      NCH = NCH + 1+IB2AS(IDLCUR(J),IBUF,NCH+1,5)
      cbuf(nch+1:nch+6)=cpstcur(j)
      nch=nch+8

      DO I=1,nstncur
        LC = LCBLcur(ISTCUR(I))
        if(cbl .eq. " ") cbl="-"     !cbl is the same as lc
        cbuf(nch:nch+1)=cstcod(istcur(i))//cbl
        nch=nch+2
      END DO
      do i=1,nstncur-1
        do j=i+1,nstncur
          ib=ibnum(istcur(i),istcur(j))
          nsorobs(isor,ib) = nsorobs(isor,ib)+1
        enddo
      enddo

C   Tape pass, direction, footage for each station
C  Generate footages in local buffer, then move in.
      nch=nch+1
      do i=1,Nstncur
        J = ISTcur(I)
        if(idircur(j) .eq. -1) then
         lchar="R"
        else
         lchar="F"
        endif
        write(cbuf(nch:nch+8),'(a1,a1,i6.6)')
     >      cpass(ipascur(j):ipascur(j)),  lchar,iftcur(j)
         nch=nch+9
      end do

C  Procedure flags
      DO I = 1,4
        IF(KFLG(I)) then
           cbuf(nch:nch)="Y"
        else
           cbuf(nch:nch)="N"
        endif
        nch=nch+1
      END DO
C  Tape motion flags
C     do i=1,nstncur
C       if (kstart_tape(i)) then
C         NCH = ichmv_ch(IBUF,NCH,'Y')
C       else
C         NCH = ichmv_ch(IBUF,NCH,'N')
C       endif
C     enddo
C  Generate durations in a local buffer, then move them  
C  to end of line in IBUF.
      nch=nch+1
      DO I = 1,NSTNcur
        J = ISTcur(I)
        write(cbuf(nch:nch+5),'(i5)') idurcur(j)
        nch=nch+6
      ENDDO
      ILEN = (NCH+1)/2
C
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
CHS-----------------------------------------------------------
CHS Optimization mode
CHS The tst-variables are packed up into ibuf.
C
      else ! optimization algorithm
C
      J = ISTtst(1)
      ISOR = NSORtst(J)
      UT=UTtst(J)
      i=max(8,trimlen(csorna(isor)))
      cbuf=csorna(isor)(1:i)
      nch=i
      IDUMY = IB2AS(ICALtst(J),IBUF,NCH+1,4)
      NCH = NCH + 6
      NCH = ICHMV(IBUF,NCH,LCODE(ICODtst(J)),1,2)

      cbuf(nch+1:nch+6)=cpretst(j)
      nch=nch+8

      iyr = mod(iyrtst(j),100)
      call seconds2hms(ut,ihr,imin,isc)
      write(cbuf(nch:nch+10),'(i2.2,i3.3,3i2.2)')
     >   iyr,idatst(j),ihr,imin,isc
      nch=nch+12

      IDurx = MAXDU(IDURtst,nstntst,ISTtst)
      NCH = NCH + 1+IB2AS(IDurx,IBUF,NCH+1,6)
      cbuf(nch+1:nch+6)=cmidtst(j)
      nch=nch+8

      NCH = NCH + 1+IB2AS(IDLtst(J),IBUF,NCH+1,5)
      cbuf(nch+1:nch+6)=cpsttst(j)
      nch=nch+8


      DO I=1,NSTNtst
        LC = LCBLtst(ISTtst(I))
        if(cbl .eq. " ") cbl="-"     !cbl is the same as lc
        cbuf(nch:nch+1)=cstcod(isttst(i))//cbl
        nch=nch+2
      END DO

C   Tape pass, direction, footage for each station
C  Generate footages in separate buffer, then move in
      nch=nch+1
      do i=1,Nstntst
        J = ISTtst(I)
        if(idirtst(j) .eq. -1) then
         lchar="R"
        else
         lchar="F"
        endif
        write(cbuf(nch:nch+8),'(a1,a1,i6.6)')
     >      cpass(ipastst(j):ipastst(j)),  lchar,ifttst(j)
         nch=nch+9
      end do

C  Procedure flags
      DO I = 1,4
        IF(KFLG(I)) then
           cbuf(nch:nch)="Y"
        else
           cbuf(nch:nch)="N"
        endif
        nch=nch+1
      END DO
C  Write out durations at end of line
C  Generate durations in a local buffer, then move them  
C  to end of line in IBUF.
      nch=nch+1
      DO I = 1,NSTNtst
        J = ISTtst(I)
        write(cbuf(nch:nch+5),'(i5)') idurtst(j)
        nch=nch+6
      ENDDO
      ILEN = (NCH+1)/2
C
      endif ! insert/delete mode , optimization algorithm
CHS+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      RETURN
      END
