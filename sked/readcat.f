      SUBROUTINE readcat(cname,ierr)
C
C  readcat reads the catalog file searching for the CNAME section.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
C  INPUT:
      character*(*) cname
C  OUTPUT:
      integer ierr
C 
C  LOCAL:
      integer ilname,il,trimlen
C HISTORY:
C 991115 nrv New.
C CALLED BY: wrsts_new

      iERR = 0
      IL = 0
C
      rewind(lucat)
      read (lucat,'(a)',end=990,iostat=ierr) cbuf
      ilname = trimlen(cname)
      DO WHILE (cbuf(1:ilname).ne.cname(1:ilname))
        read (lucat,'(a)',end=990,err=991,iostat=ierr) cbuf
      END DO  !
      return
C
991   if (ierr.ne.0) then
        write(luscn,'("READCAT01 - Error ",i5,
     .  " reading catalog file.")') ierr
      endif
990   ierr=-1 ! end of file
      RETURN
      END
