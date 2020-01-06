      SUBROUTINE hwrst(cname,num_sel,ierr)
C
C   This subroutine gets the mask information for the selected antennas
C   from the horizon/mask catalog.
C
C   HISTORY:
C     gag   900109  created
C     nrv   930225  implicit none
!    2012Sep19 JMG. Fixed bug on reading in continuation lines.
!                   Now only check the first few characters for "- ". Previously checked whole line.
!                   This occaisonally resulted in an incorrect line being read in.
!  2015Feb12  JMG.  Fixed bug which would cause an infinite loop if two lines in the mask file were identical.
!  2019Sep03  JMG.  Implicit none again 
!  2019Sep03  JMG.  Fixed a bug where if two stations were adjacent in the catalog, would skip getting mask of second. 
C
C   parameter file
      implicit none 
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
C
!   input
      integer num_sel
      character*8 cname(num_sel)

C  OUTPUT:
      integer ierr

! functions
      integer iwhere_in_string_list
      integer trimlen
      integer ifirst_non_blank

      integer MaxToken
      integer NumToken
      parameter(MaxToken=20)
      character*12 ltoken(MaxToken)
! local
      integer iwhere
      character*125 ldum
      logical kdone(num_sel)
      integer num_done
      character*8 cmask_name
      character*2 cid2
      integer nch,nch2
      integer nbeg
      integer i
      integer ind
      character*4 ltemp

! 1.   Open the catalog and get the first line.
      call open_cat(mask_cat, ierr)
      if(ierr .ne. 0) then
        close(lutmp)
        return
      endif

      kdone=.false.
      num_done=0
! 2. Go through catalog until all of information is obtained  or reach end of file.
100   continue
      read(lucat,'(a125)',end=190) ldum 
110   continue
      if(ldum(1:1) .eq. "*" .or. ldum .eq. " ") goto 100

      call splitNtokens(ldum,ltoken,Maxtoken,NumToken)
      if(ltoken(1) .eq. "-") goto 100     ! a continuation line.
      cmask_name=ltoken(2)
      cid2=ltoken(3)

      iwhere=iwhere_in_string_list(cname,num_sel,cmask_name)       
      if(iwhere .eq. 0) goto 100    
      if(.not.kdone(iwhere)) then
        num_done=num_done+1
        kdone(iwhere)=.true.

        nch=trimlen(ldum)
        nbeg=ifirst_non_blank(ldum)
        ltemp=" "//cid2//" "
        nbeg=index(ldum,ltemp)+1
        cbuf="H "//ldum(nbeg:nch)               !get the first part  of this
        nch=nch-nbeg+1+2

! now read the continuation lines
        ind=-1
        do while(ind .ne. 0)
          read(lucat,'(a125)',end=190) ldum
          ind=index(ldum(1:3),'- ')
          if(ind .eq. 0) then
            write(lutmp,'(a)') cbuf(1:trimlen(cbuf))
            goto 110 
          else
            nch2=trimlen(ldum)
            cbuf(nch+1:nch+nch2-ind)=ldum(ind+1:nch2)
            nch=nch+nch2-ind+1
          endif
        end do
      endif
      goto 100                !go to the top of the loop 

190   continue
      close(lucat)
      if(kverbose) write(luscn,'(a)') ! write the EOL

200   continue
      if(num_done .ne. num_sel) then
        do i=1,num_sel
          if(.not.kdone(i) .and. kverbose)  write(luscn,
     >       "('HWRST: Note - Mask entry not found for: ',A)") cname(i)
        end do
      endif

      close(lutmp)
      return
      end

