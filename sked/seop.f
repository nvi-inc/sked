      subroutine seop(cfunc,ierr)
C
C     SEOP sets parameters for optimization
C
C   History

!  Now recent at top
! 2020Oct13 JMG.  Initialized ierr4 to 0. 
C   NRV 910905 Replacement for Heinz's many-question version of PARAM
C   NRV 911026 Changed first line with major/minor options
C   nrv 920706 added 2nd line of parameters, for noise floor
C   nrv 930210 Changed to curses
C   nrv 930315 Shorten each column to 7 chars to fit on screen
C   nrv 930513 Check error return from curses start
C   nrv 930602 Add list option
C   nrv 930930 Add low el option
C   nrv 931013 Add expand option
C   931019 nrv Remove "Wt" from Loel
C   931028 nrv Add "Riseset"
C   931029 nrv Add "MinSlew" and "Betwnnm"
C
C   Common/include
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'covar.ftni'
C  CALLING SUBROUTINE: PARAM
C  CALLED SUBROUTINES: SETOP,SENCR,SETCR,DSPOP,DSPOPS
C
C  INPUT: cfunc - 2-characters, PA for param page, SO for source page
      character*2 cfunc
      integer ierr

C  LOCAL:
      integer nrows,nsrows,icol,irow,inx,ival,i,j,k,index,itype
! AEM 20041217 int->int*2
      integer*2 ic
      character*1 cc
! AEM 20041217 int->int*4 passed to C-module
      integer*4 izero,ierr4,ix,iy,ixcol,i4,i4x
! AEM 20041217 int->int*2 passed to C-module
      integer*2 ikey
! AEM 20050113 add new variables for precise positioning in "Optimize"
!              and "Estimate" sections and trimlen
      integer icol_p
      integer*4 ixcol_p
      integer trimlen
      
C     ixcol - value of x to set the cursor at for the next selection
C     inx - index within a column for setting typed values
      LOGICAL klite
      character*8 cstpna(8),cerpna(5)
      integer ias2b
      integer np(2),maxnp(2)

      data cstpna/'AtmOffs ','AtmRate','ClkOffs ','ClkRat1 ','ClkRat2 ',
     >            '  U    ','  E    ','  N     '/
      data cerpna/'   XP   ','   YP  ','  DUT  ','  PSI   ','  EPS   '/
  

      ierr4=0
      ierr=0
      izero=0
      call start_mn(ierr4)
      if (ierr4.ne.1) then
        write(luscn,9901)
9901    format('Invalid terminal type, can''t use cursor sensing.')
        ierr=-1
        return
      endif
      np(2)=num_est
      np(1)=num_opt
      maxnp(2)=MAX_PAR_ESTI
      maxnp(1)=MAX_PAR_OPTI

100   continue
      if (kcar) then
         cstpna(6)='   X   '
         cstpna(7)='   Y   '
         cstpna(8)='   Z   '
      else
         cstpna(6)='   U   '
         cstpna(7)='   E   '
         cstpna(8)='   N   '
      endif


C  1. Display the page.

      if (cfunc.eq.'PA') then
        CALL DSPOP(cstpna,cerpna)
        nrows = nstatn*2 + 2                !the 2 extra rows are EOP. 
      else if (cfunc.eq.'SO') then
        CALL DSPOPS
        nsrows = 1+(nsourc-1)/8
        nrows = 2*(1+nsrows) + 1
      endif

      cbuf='<E>nd   <U>=<X> toggle between UEN/XYZ   Cursor or h,j,k,l'
      call nl_mn
      call addstr_f(cbuf(1:trimlen(cbuf)))
      call refresh_mn
      CALL SETCR_mn(izero,izero)

C  2. Now sense each cursor position

      DO WHILE (.TRUE.)  !get selection
200     call senkr_mn(ix,iy,ikey)
        cc=char(ikey)
        call capitalize(cc)
        IF (ix.GT.80) ix=80
        irow = iy+1
        if (cfunc.eq.'SO') then
          icol = 1 + ix/9
          inx = ix-(icol-1)*9+1
          ixcol = (icol-1)*9
        else
          icol = 1 + ix/8
      	  icol_p = (ix-1)/8 !AEM column next to the station name, 1,2..7
!          inx = ix-(icol-1)*8 + 1
! AEM 20041222 replace with equivalent
      	  inx = mod(ix,8) + 1 !AEM cursor position in current selection
          ixcol = (icol-1)*8 + 1
          ixcol_p = (icol_p)*8 + 1
          if (irow.eq.1.or.irow.eq.2) ixcol=ixcol-1
        endif

        if(cc.eq."U" .or. cc.eq. "X") then
          kcar=.not.kcar
          goto 100
        endif
        IF (IROW.GT.NROWS .or. cc.eq."E") THEN !end of selection     
          cbuf='End of parameter selection.'
          call nl_mn
          call addstr_f(cbuf(1:trimlen(cbuf)))
          call nl_mn
          goto 999
        END IF !end of selection
9202    format(a8)
9207    format(a1)

C  3. Station parameters
        if (cfunc.eq.'PA') then !parameters
          knewop = .true.
! AEM 20041222 make changes for correct processing of line 3 and below
          if ((irow.eq.1.or.irow.eq.1+nstatn+1).and.
! AEM undo    .    icol.ge.3.and.icol.le.7) then !EOP switches
     .    icol_p.ge.2.and.icol_p.le.6) then !EOP switches
            if (irow.eq.1) itype=1
            if (irow.eq.1+nstatn+1) itype=2
! AEM undo          if (lpara(icol-2,itype)) then !turn off
            if (lpara(icol_p-1,itype)) then !turn off
! AEM undo            lpara(icol-2,itype) = .false.
              lpara(icol_p-1,itype) = .false.
              np(itype)=np(itype)-1
              klite = .false.
            else
              if (np(itype).lt.maxnp(itype)) then !turn on
! AEM undo                lpara(icol-2,itype) = .true.
                lpara(icol_p-1,itype) = .true.
                klite = .true.
                np(itype)=np(itype)+1
              else
                klite=.false.
              endif
            endif
! AEM undo            call setcr_mn(ixcol,iy)
            call setcr_mn(ixcol_p,iy)
            if (klite) call reverse_on_mn
! AEM undo           write(cbuf,9202) cerpna(icol-2)
            write(cbuf,9202) cerpna(icol_p-1)
            call addstr_f(cbuf(1:7))
            if (klite) call reverse_off_mn
            call addstr_f(' ')
            i4=9
            IF (icol.eq.7) CALL setcr_mn(i4,IY+1)
          endif !EOP switches

          if (((irow.ge.2.and.irow.le.1+nstatn).or.
     .        (irow.ge.3+nstatn.and.irow.le.nstatn*2+2)).and.
! AEM undo     .         icol.ge.2.and.icol.le.9) then !station rows
     .         icol_p.ge.1.and.icol_p.le.8) then !station rows
            if (irow.ge.2.and.irow.le.1+nstatn) itype=1
            if (irow.ge.nstatn+3.and.irow.le.nstatn*2+2) itype=2
            if (itype.eq.1) i=irow-1 !station index
            if (itype.eq.2) i=irow-nstatn-2 !station index
! AEM undo           j=icol-1 !parameter index
            j=icol_p !parameter index
            if (j.eq.1) k=5+2*i-1
            if (j.eq.2) k=5+2*i
            if (j.eq.3) k=5+2*nstatn+(3*i)-2
            if (j.eq.4) k=5+2*nstatn+(3*i)-1
            if (j.eq.5) k=5+2*nstatn+(3*i)
            if (j.eq.6) k=5+5*nstatn+(3*i)-2
            if (j.eq.7) k=5+5*nstatn+(3*i)-1
            if (j.eq.8) k=5+5*nstatn+(3*i)
            if (lpara(k,itype)) then !turn off
              lpara(k,itype) = .false.
              np(itype)=np(itype)-1
              klite = .false.
            else
              if (np(itype).lt.maxnp(itype)) then !turn on
                lpara(k,itype) = .true.
                np(itype)=np(itype)+1
                klite = .true.
              else
                klite=.false.
              endif
            endif
! AEM undo            call setcr_mn(ixcol,iy)
            call setcr_mn(ixcol_p,iy)
            if (klite) call reverse_on_mn
! AEM undo            write(cbuf,9202) cstpna(icol-1)
            write(cbuf,9202) cstpna(icol_p)
            call addstr_f(cbuf(1:7))
            if (klite) call reverse_off_mn
            call addstr_f(' ')
	    
!            if (icol.eq.9) call nl_mn ! AEM commented
            if (icol.eq.9) then
              if (i.lt.nstatn) i4=9 !AEM ix -> i4
              if (i.eq.nstatn) i4=17 !AEM ix -> i4
              i4x = iy + 1
! AEM 20041222 add jump to the "End selection" at the end	      
              if (irow.eq.nstatn*2+2) then
                i4x = iy + 2
                i4 = 0
              endif
              call setcr_mn(i4,i4x)
            endif
          endif !station rows

C   4. Source parameters

        else if (cfunc.eq.'SO'.and.irow.ge.2) then !sources
          knewop = .true.
          if (irow.ge.2.and.irow.le.1+nsrows) itype=1 !optimize
          if (irow.ge.3+nsrows.and.irow.le.2+2*nsrows) itype=2 !estimate
          if (itype.eq.1) i=irow-1
          if (itype.eq.2) i=irow-2-nsrows
          index = (i-1)*8 + icol !source index
          k = 5+nstatn*8+index*2-1
          if (lpara(k,itype)) then !turn off
             lpara(k,itype) = .false.
             lpara(k+1,itype) = .false.
             np(itype)=np(itype)-2
             klite = .false.
          else
            if (np(itype).lt.maxnp(itype)-1) then !turn on
              lpara(k,itype) = .true.
              lpara(k+1,itype) = .true.
              np(itype)=np(itype)+2
              klite = .true.
            else
              klite=.false.
            endif
          endif
          call setcr_mn(ixcol,iy)
          if (klite) call reverse_on_mn
          write(cbuf,9202) csorna(index)
          call addstr_f(cbuf(1:8))
          if (klite) call reverse_off_mn
          call addstr_f(' ')
          if (icol.eq.8.or.index.eq.nsourc) then
            if (irow.lt.1+nsrows) CALL SETCR_mn(izero,IY+1)
            if (irow.eq.1+nsrows) CALL SETCR_mn(izero,IY+2)
            if (irow.ge.2+nsrows.and.irow.lt.2+2*nsrows)
     .      CALL SETCR_mn(izero,IY+1)
            if (irow.eq.2+2*nsrows) CALL SETCR_mn(izero,IY+2)
          endif
        endif !PA/SO
      END DO      ! selection loop

999   call end_mn
      num_est=np(2)
      num_opt=np(1)
      return

      END
