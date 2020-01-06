      SUBROUTINE GTSOI(LINSTQ,ICH,IERR,lu)
C
C     Decodes user input field, which is a source name.
C
C 970224 nrv Change 4 and 8 to max_sorlen/2 and max_sorlen
C 020904 nrv Change "invalid" message to print full source name.
! 2003Dec08 JMGipson. Replaced igtso by igetsrcnum

      INCLUDE '../skdrincl/skparm.ftni'
      INCLUDE 'skcom.ftni'
C
C  INPUT:
      integer*2 LINSTQ(*)
      integer ich,lu
C     ICH - general character counter
C
C  OUTPUT:
      integer ierr
! functions
      integer igetsrcnum

C
C  LOCAL:
      integer ifc,iec,nchar,idumy
      integer i2long,ichmv
      LOGICAL KALL
      integer*2 LNAM(max_sorlen/2)
      character*(max_sorlen) cnam
      equivalence (lnam,cnam)
C
C     1. Get the first field now.  May be a source name or
C     "_" to indicate all.
C
      IERR = 0
      ISORCM = 0
      CALL GTFLD(LINSTQ(2),ICH,i2long(LINSTQ(1)),IFC,IEC)
      if(ifc .eq. 0) return        !nothing left on rest of line.
      NCHAR = IEC-IFC+1
      KALL = .FALSE.

      cnam=" "
      IDUMY = ICHMV(LNAM,1,LINSTQ(2),IFC,NCHAR)
      if(cnam(1:1) .eq. "_") then
        kall=.true.
      else
        isorcm=igetsrcnum(cnam)
        IF  (isorcm .eq. 0) then
          IERR = 1
          if(lu .gt. 0) then
            write(lu,'(a)') "GTSOI:  Source name not found."
          endif
        else if(isorcm .lt. 0) then
          IERR = 2
          if(lu .gt. 0) then
            write(lu,'(a)') "GTSOI:  Ambiguous source name."
          endif
        endif
      endif
      RETURN
      END
