      subroutine replace_sksrc()
! replace the solvesksrc file with current source list.
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/sourc.ftni'
      include 'skcom.ftni'

      character*8 ccom_name
      integer isrc
      integer irahr,iramin         !right ascension in hours, minutes
      real RAsec
      integer idecdeg,idecmin
      real DecSec
      integer l1,ldum,idum
      character*1 lplusminus
      equivalence (l1,lplusminus)
      real dum

! delete the old file.
      open(lusel,file=csofil)
      close(lusel,status="delete")

! now reopen, and write out.
      open(lusel,file=csofil)
! and begin writing the new one.
      do isrc=1,Nsourc
        CALL RADED(sorp2000(1,isrc),sorp2000(2,isrc),0.D0,
     >    irahr,iramin,rasec,
     >    L1,Idecdeg,Idecmin,decsec,LDUM,IDUM,IDUM,DUM)
        if(csorna(isrc)(1:8) .eq. ciauna(isrc)(1:8)) then
           ccom_name="$"
        else
          ccom_name=csorna(isrc)
        endif
        write(lusel,
     >   '(1x,2(a8,1x),2(i2.2,1x),f12.8,1x,a1,2(i2.2,1x),f12.8, a)')
     >    ciauna(isrc),ccom_name, irahr,iramin,rasec,
     >    lplusminus,idecdeg,idecmin,decsec, " 2000 0.0  SKED"
      end do
      close(lusel)
      return
      end
