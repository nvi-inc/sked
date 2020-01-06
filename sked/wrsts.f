       SUBROUTINE WRSTS(IERR) !WRITE STATION SELECT FILE C#880422:14:34#
C
C     WRSTS calls the catalog write subroutines to get the needed 
C     information of the selected stations. The Antenna and Position
C     information are either from the original sked file or from the
C     catalogs as selected by the user. The equipment and mask information
C     is always gotten from the catalogs.
C
C  HISTORY:
C     WHO  WHEN    WHAT
C     gag  900108  Changed to make compatible with new catalog routines
!    2010Apr21 JMG Better handling of collisions for 1 station character codes. 
!    2014May27 JMG. Even better handling of collisions. Keeps old code if have obs. Else code in catalog. 
C
C  PARAMETER FILE
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/skobs.ftni' 
C
C  CALLING SUBROUTINES: STCAT
C  CALLED SUBROUTINES: INITF,AWRST,PWRST,TWRST,HWRST
C
C
C  OUTPUT: IERR - error return
      integer IERR
C
! fuctions
      integer renam ! function
      integer iwhere_in_String_list
C  LOCAL

      character*8 cname(max_stn)                !name
      character*4 cterm(max_stn)                !terminal
      character*2 cpos(max_stn)                 !2-char ID
      character*1 cstnid(max_stn)               !1-char ID
      integer istat_rack(max_stn)
      integer istat_rec(max_stn)
      integer istat_bw(max_stn)
      integer num_sel
      integer iptr
      character*1 c1_cat(max_stn)               !1-char ID from catalog
      character*1 c1_sel(max_stn)               !1-actual one character ID. 

      character*8 ctmp_name
 
      integer iwhere 
! Used for 1 character CSTCOD.  These are the valid codes. 
      integer i,j               !counters
      integer max_valid 
      parameter (max_valid=72)
      character*(max_valid) cvalid
      character*1 cvec(max_valid)
      equivalence (cvec,cvalid)         
 
! Initialization
      cvalid=
     >"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"//      !36
     >"@#$%<>[]()abcdefghijklmnopqrstuvwxyz"      !36


!  1. Open the temporary files to work with.
      OPEN (lutmp,file=CtmFIL,status='UNKNOWN',iostat=ierr)
C             SKW*
      IF (IERR.NE.0)  THEN
        CLOSE(lutmp)
        write(luscn,'("Error opening temp file.")')
        RETURN
      END IF

! 2. Find which stations selected.
      num_sel=0
      do iptr=1,num_cat_stat
       if(kcat_stat_sel(iptr)) then
         num_sel=num_sel+1
         cname(num_sel) =cat_ant_name(icat_stat_vec(1,iptr))
         cpos(num_sel)  =cat_ant_id2(icat_stat_vec(1,iptr))
         c1_cat(num_sel)=cat_ant_id1(icat_stat_vec(1,iptr))              
         istat_rack(num_sel)=icat_stat_vec(2,iptr)
         istat_rec(num_sel) =icat_stat_vec(3,iptr)
         istat_bw(num_sel)  =icat_stat_vec(4,iptr)    
         cterm(num_sel)=cat_term(iptr) 
!         write(*,'(3(a,1x))') 
!     >      cname(num_sel), cpos(num_sel), cstnid(num_sel)
       endif
      end do
      if(num_sel .eq. 0) then
         write(luscn,'(a)') "WRSTS: No stations selected!"
         close(lutmp)
         goto 500
      endif
    
! Now we select the 1-character station IDs.  
! Initialize to not found. 
      c1_sel="-"     
     
   
! We always try to preserve the 1-character IDs in the schedule if we have some observations. 
      if(nobs .gt. 0) then 
        do i=1,num_sel
          iwhere=iwhere_in_string_list(cpocod,nstatn,cpos(i))
          if(iwhere .ne. 0) then
             c1_sel(i)=cstcod(iwhere)    
          endif
        end do 
      endif 

! Now we select the remainder of the 1-char codes.
      do i=1,num_sel
         if(c1_sel(i) .eq. "-") then ! Haven't selected it yet.
! See if the default 1-char code is available.
           iwhere=iwhere_in_string_list(c1_sel,num_sel,c1_cat(i))
           if(iwhere .eq. 0) then
              c1_sel(i)=c1_cat(i)     !is available. Use it. 
           else
! not available. Get another 1-letter code. 
             do j=1,max_valid
               iwhere=iwhere_in_string_list(c1_sel,num_sel,cvec(j))
               if(iwhere .eq. 0) then
                  c1_sel(i)=cvec(j)
                  goto 50
               endif
             end do
50           continue
           endif
        endif
        write(*,'(i4,1x,a,1x,a)') i, cpos(i), c1_sel(i)
      end do 

         
      write(luscn,'(a,$)') "Writing out station select file for SKED: "

! 3.1 Write out antenna information.
      write(luscn,'("Antenna ",$)') 
      call awrst(cname,cterm,c1_sel,num_sel,ierr)
      IF (IERR.lt.0)  THEN
        CLOSE(lutmp) 
        write(luscn,'("Error writing A lines.")')
        RETURN
      END IF

! 3.2 Write out position.
      write(luscn,'("Position ",$)') 
      call pwrst(cpos,num_sel,ierr)
      IF (IERR.lt.0)  THEN
        CLOSE(lutmp)
        write(luscn,'("Error writing P lines.")')
        RETURN
      END IF

     
! 3.3 Write out equipment.
      write(luscn,'("Equipment ",$)') 
      call twrst(cname,istat_rack,istat_rec,istat_bw,num_sel,ierr)
      IF (IERR.lt.0)  THEN
        CLOSE(lutmp)
        write(luscn,'("Error writing T lines.")')
        RETURN
      END IF 

! 3.4 Write out mask 
      write(luscn,'("Mask ")') 
      call hwrst(cname,num_sel,ierr)  
      IF (IERR.lt.0)  THEN
        CLOSE(lutmp)
        write(luscn,'("Error writing H lines.")')
        RETURN
      END IF 

      knewst=.true.

!4. Rename the temp file with all the information (SKW*) to the sked working file (SKY*).
500   continue   
      open(lusel,file=CSTFIL,status='old',iostat=ierr)
      close(lusel,status='delete')
      ierr = renam(ctmfil,cstfil)
      if (ierr.lt.0) then
        write(luscn,9300) ierr
9300    format('Error ',i5,' renaming working file to scratch file.')
        return
      end if
      
      
      ierr = 0
      RETURN
      END
