      SUBROUTINE twrst(cname,istat_rack,istat_rec,istat_bw,num_sel,ierr)
C
C  Subroutine twrst gets the equipment information from the equipment
C  catalog for the antennas that were selected with stcat and writes
C  it out in the working file.
C
C   HISTORY:
C     WHO   WHEN   WHAT
C     gag   900110 created
C     nrv   930225 implicit none
C 000124 nrv Pick only the first terminal found in the catalog, in case
c            there are multiple ones.
!   2015Feb12 JMG. Don't write out catalog name.
C
C   parameter file
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_stat.ftni'
      include '../skdrincl/valid_hardware.ftni'
C
C  INPUT:
      integer num_sel                    !number selected stations
      character*8 cname(num_sel)        !names of stations
      integer istat_rack(num_sel)       !rack index
      integer istat_rec(num_sel)        !recorder index
      integer istat_bw(num_sel)          !Bandwidth index

C  OUTPUT:
      integer ierr
C     ierr - error return
C
C   SUBROUTINES
C     CALLED BY: WRSTS
C     CALLED: INITF,CATAS,GTFLD,IFILL,UNPVT,CHAR2HOL,WRITF_ASC,INC
C
C  LOCAL VARIABLES
! function
      integer iwhere_in_string_list
      integer trimlen


! Used to hold tokenized line
      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)
      character*8 ltemp      

      character*125 ldum   !input buffer

      integer irack,irec,ibw    !
      integer iwhere
      logical kdone(num_sel)
      integer num_done
      integer nbeg

      integer nch  !variable and function for var length
      integer i

!  1. Open the equipment catalog and read the first line.
      open(lucat,file=equip_cat,status='old',iostat=ierr)
      nch = trimlen(equip_cat)
      if (ierr.ne.0) then
        write(luscn,9100) ierr,equip_cat
9100    format('Error ',i5,' opening catalog ',a)
        close(lucat)
        close(lutmp)
        return
      end if
!      write(luscn,'(A,": ",$)') equip_cat(1:nch)

      kdone=.false.
      num_done=0
     
! 2. Go through catalog until all of information is obtained  or reach end of file.
100   continue
      read(lucat,'(a125)',end=190) ldum
      if(ldum(1:1) .eq. "*" .or. ldum .eq. " ") goto 100
      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)

      ltemp=ltoken(1)
      iwhere=iwhere_in_string_list(cname,num_sel,ltemp)  !see if we have a match.
      if(iwhere .eq. 0) goto 100
! Possible match. Check rack and equipment.
     
! Check Rack.
      ltemp=ltoken(NumToken-1)
      call capitalize(ltemp)
      if(ltemp .eq. "DBBC") ltemp="DBBC_DDC" 
      irack=iwhere_in_string_list(crack_type_cap,max_rack_type,ltemp)
      if(irack .ne. istat_rack(iwhere)) goto 100       !doesn't match on rack.
      
! And recorder.
      ltemp=ltoken(NumToken)
      call check_rec_type(ltemp)    
      call capitalize(ltemp) 
      irec=iwhere_in_string_list(crec_type_cap,max_rec_type,ltemp)   
      if(irec .ne. istat_rec(iwhere)) goto 100       !doesn't match on recorder
! This is the "Band" token
      ltemp=ltoken(6)(1:1)//ltoken(8)(1:1)
      ibw=iwhere_in_string_list(cat_equip_band,num_equip_band,ltemp)
      if(ibw .ne. istat_bw(iwhere)) goto 100
      

!matches. This is the line!
      if(.not. kdone(iwhere)) then
!        write(luscn,'(a," ",$)') cname(iwhere)(1:trimlen(cname(iwhere)))
        nbeg=index(ldum,cname(iwhere))+8
        write(lutmp,'(a)')"T "//ldum(nbeg:trimlen(ldum))
        num_done=num_done+1
        kdone(iwhere)=.true.
      endif    
      if(num_done .ne. num_sel) goto 100      !more to do?

190   continue
      close(lucat)   

200   continue
      if(num_done .ne. num_sel) then
        do i=1,num_sel
          if(.not.kdone(i)) then
            write(luscn,9600) cname(i)
9600        format('TWRST: WARNING - Equipment entry not found for: ',A)
    
          endif
        end do
      endif

      RETURN
      END
