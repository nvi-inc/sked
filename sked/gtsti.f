      SUBROUTINE GTSTI(LINSTQ,ICH,NST,ISTN,IERR,lu)  
C
C     Decodes user input fields which are station ID's.
C
C History
C 950411 nrv New version for 2-letter codes.
C 951017 nrv Fix gtfld call.
C 991120 nrv Check for duplicate stations and remove them.

      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C  INPUT:
      integer ich,lu
      integer*2 LINSTQ(*)
C     ICH - first character to scan in linstq
C
C  OUTPUT:
      integer nst ! number of stations
C     If ierr=0 and nst=0 then all stations were selected
      integer ierr ! non-zero for errors
      integer ISTN(max_stn) ! array holding station indices
! fuctions
      integer i2long,jchar,ichmv,iscnc !functions
      integer igetstatnum2

C  LOCAL:
      integer is,nchar,iec,ifc,i,idum,n,ix1,ix2,nix,j
      integer itemp
      character*2 c2
      equivalence (Itemp,c2)
C
C     1. Get the next field.  May be a subnet or "_" to indicate all.
C
      NST = 0
      IERR = 0
!      writE(*,'("GTSTI: ",30a2)') linstq(2:31)
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IFC,IEC)
      NCHAR = IEC-IFC+1
      if (ifc.gt.0) then ! something there
        if (nchar.eq.1.and.jchar(linstq(2),ifc).eq.ounderscore) then
           do is=1,nstatn
             istn(is)=is
          end do
          nst=nstatn
          return ! all stations selected 
        else ! interpret the list

C     2. Interpret the list of stations. Valid examples are:
C        Ko-Gi
C        KoGi
C        kg
C        k-g
C        Not case sensitive. The command has been all upper-cased.

          n=1
          ix1=ifc
          ix2=iscnc(linstq(2),ifc,iec,ominus)
          if (ix2.gt.0) then ! some dashes
            do while (ix1.le.iec)
              nix=ix2-ix1
              c2=" "
              idum = ichmv(itemp,1,linstq(2),ix1,min0(2,nix))  !itemp is same as c2
              is = igetstatnum2(c2)
              if (is.eq.0) then
                if (lu.ne.0) write(lu,9100) c2
9100            format(' GTSTI01 - Invalid station: ',a2)
                ierr=1
              else if (is.lt.0) then
                if (lu.ne.0) write(lu,9101) c2
9101            format(' GTSTI02 - Ambiguous station: ',a2)
                ierr=1
              else
                istn(n)=is
                n=n+1
              endif
              ix1=ix2+1
              ix2=iscnc(linstq(2),ix1,iec,ominus)
              if (ix2.eq.0) ix2=iec+1 ! last one without a dash
            enddo
          else ! only characters
            do i=1,nchar/2 ! 2-character IDs
              c2=" "
              idum = ichmv(itemp,1,linstq(2),ifc+(i-1)*2,2)
              is = igetstatnum2(c2)
              if (is.eq.0) then
                if (lu.ne.0) write(lu,9110) c2
9110            format(' GTSTI03 - Invalid station: ',a2)
                ierr=1
              else if (is.lt.0) then
                if (lu.ne.0) write(lu,9111) c2
9111            format(' GTSTI04 - Invalid station: ',a2)
                ierr=1
              else
                istn(n)=is
                n=n+1
              endif
            enddo
            if (nchar.eq.1.or.ierr.ne.0) then ! try single characters
              ierr=0
              do i=1,nchar
                c2=" "
                idum = ichmv(itemp,1,linstq(2),ifc+i-1,1)
                is = igetstatnum2(c2)
                if (is.eq.0) then
                  if (lu.ne.0) write(lu,9120) c2
9120            format(' GTSTI05 - Invalid station: ',a2)
                  ierr=1
                else if (is.lt.0) then
                  if (lu.ne.0) write(lu,9121) c2
9121            format(' GTSTI06 - Invalid station: ',a2)
                  ierr=1
                else
                  istn(n)=is
                  n=n+1
                endif
              enddo
            endif
          endif ! dashes/characters
          if (ierr.eq.0) NST = n-1
        endif ! interpret the list
      endif ! something there
C
C Now check for duplicate stations, in case the user mistyped.

      if (nst.gt.0.and.ierr.eq.0) then
        do i=1,nst-1
          do j=i+1,nst
            if (istn(i).eq.istn(j)) then ! duplicate
              istn(i)=-istn(i)
            endif
          enddo
        enddo
        call destn(nst,istn) ! remove the negative ones
      endif

      RETURN
      END
