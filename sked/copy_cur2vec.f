      subroutine copy_cur2vec()

      include '../skdrincl/skparm.ftni'
      include '../skdrincl/skobs.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'skcom_vec.ftni'
! functions
      real speed
!     JMG 2003May29
! 2020Nov05 JMGipson. Git rid of 
! copy current measurement into old measurement.
      integer istat
      integer i
      integer inew,iold
      integer ifeet_dif
      integer itime_dif

      inew=iskrec(ircur)
      if(ircur .gt. 1) then
        iold=iskrec(ircur-1)
! default: Copy iold to inew.
        do istat=1,nstatn
          NSOR_vec(istat,inew) =   NSOR_vec(istat,iold)
          IFT_vec(istat,inew)  =   IFT_vec(istat,iold)
          iftend_vec(istat,inew) = iftend_vec(istat,iold)
          itu_vec(istat,inew)  =   itu_vec(istat,iold)
          Idur_vec(istat,inew) =   Idur_vec(istat,iold)
          Idl_vec(istat,inew)  =   Idl_vec(istat,iold)
          ICOD_vec(istat,inew) =   ICOD_vec(istat,iold)
          mjd_vec(istat,inew)  =   mjd_vec(istat,iold)
          ut_vec(istat,inew)   =   ut_vec(istat,iold)
          cpre_vec(istat,inew) =   cpre_vec(istat,iold)
          cmid_vec(istat,inew) =   cmid_vec(istat,iold)
          cpst_vec(istat,inew) =   cpst_vec(istat,iold)
          lcbl_vec(istat,inew) =   lcbl_vec(istat,iold)
        end do
      else     !First obs. Initialize everything to 0.
        iold=0
        do istat=1,nstatn
          NSOR_vec(istat,inew) = 0
          IFT_vec(istat,inew)  = 0
          iftend_vec(istat,inew) = 0
          itu_vec(istat,inew)  = 0   
          Idur_vec(istat,inew) = 0
          Idl_vec(istat,inew)  = 0
          ICOD_vec(istat,inew) = 0
          mjd_vec(istat,inew)  = 0
          ut_vec(istat,inew)   = 0
          cpre_vec(istat,inew) = " "
          cmid_veC(istat,inew) = " "
          cpst_vec(istat,inew) = " "
        end do
      endif

! Find itucur.  This is 1 if tape stopped.
      do i=1,nstncur
         istat=istcur(i)
         if(nsor_vec(istat,inew) .eq. 0) then  !no prior obs.
            itucur(istat)=1
         else if(iftcur(istat) .eq. 0 .or.          	!start or end of tape.
     >      iftcur(istat) .eq. maxtap(istat)) then
            itucur(istat)=1
         else if (tape_motion_type(istat).eq.'START&STOP'.or.
     >            tape_motion_type(istat).eq.'ADAPTIVE') then
            if(iold .eq. 0) then
               itucur(istat)=1
               write(*,*) "Copy_cur2vec. Shouldn't be here!"
            else
               ifeet_dif=iftend_vec(istat,iold)-iftcur(istat)
               itime_dif=ifeet_dif/speed(icod_vec(istat,iold),istat)
               if(itime_dif .lt. 10) itucur(istat)=1
            endif
         else if(tape_motion_type(istat) .eq. "CONTINUOUS"
     >       .and. iftcur(istat) .ne. 0) then
             itucur(istat)=0
         else
!            write(*,*) "Surprise 2!"
         endif
      end do

! Compute tape end.
      do i=1,nstncur
         istat=istcur(i)
         IFTEND_cur(istat) = IFTCUR(istat)+ 
     >    (IDURcur(istat)+
     >     itucur(istat)*ITEARL(istat))*SPEED(ICODcur(istat),istat)
      end do

      DO I=1,Nstncur
         ist_vec(i,inew)=istcur(i)
         istat=istcur(i)
         NSOR_vec(istat,inew) =   NSORcur(istat)
         IFT_vec(istat,inew)  =   IFTcur(istat)
         iftend_vec(istat,inew) = iftend_cur(istat)
         itu_vec(istat,inew)  =   itucur(istat) 
         Idur_vec(istat,inew) =   Idurcur(istat)
         Idl_vec(istat,inew)  =   Idlcur(istat)
         ICOD_vec(istat,inew) =   ICODcur(istat)
         mjd_vec(istat,inew)  =   mjdcur(istat)
         ut_vec(istat,inew)   =   utcur(istat)
         cpre_vec(istat,inew) =   cprecur(istat)
         cmid_vec(istat,inew) =   cmidcur(istat)
         cpst_vec(istat,inew) =   cpstcur(istat)
         lcbl_vec(istat,inew) =   lcblcur(istat)
       END DO

       return
       end

