      subroutine downtime_out(luout,lkind)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include 'downtime.ftni'
! functions
      integer trimlen
! passed
      integer luout
      character*1 lkind 
! local
      integer i,j
      integer mjd_temp
      double precision ut_temp
      integer iyear,iday,ihr,isec,imin
      integer nch

      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$DOWNTIME"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

! Write out source dowtimes.
! This is downtime line.
! idown_stat is array of stations which are down.
! if the value is >0, then hasn't been written out yet.
!
      do i=1,num_down
        if(idown_stat(i) .gt. 0) then  !Find a station not processed
          cbuf=cpocod(idown_stat(i))
          idown_Stat(i)=-idown_stat(i)
          nch=3
          do j=i+1,num_down           !see if other stations with same down time.
            if(idown_Stat(j) .gt. 0 .and.
     >        mjd_down_beg(i).eq. mjd_down_beg(j) .and.
     >        ut_down_beg(i) .eq. ut_down_beg(j)  .and.
     >        mjd_down_end(i).eq. mjd_down_end(j) .and.
     >        ut_down_end(i) .eq. ut_down_end(j)) then
              cbuf(nch:nch+2)="-"//cpocod(idown_Stat(j))  !found one. Append list.
              idown_Stat(j) = -idown_Stat(j)
              nch=nch+3
            endif
          end do
          nch=nch+1     !blank space
! now write out the time intervals
          do j=1,2
            if(j.eq. 1) then
              ut_temp=ut_down_beg(i)
              mjd_temp=mjd_down_beg(i)
            else
              ut_temp=ut_down_end(i)
              mjd_temp=mjd_down_end(i)
            endif
            call seconds2hms(ut_temp,ihr,imin,isec)
            call mjd2YrDoy(mjd_temp,iyear,iday)
            write(cbuf(nch:nch+17),'(i4,"-",i3.3,"-",2(i2.2,":"),i2.2)')
     >           iyear,iday,ihr,imin,isec
            nch=nch+18
          end do
          call wrt_param_line(cbuf,luout,lkind)           
        end if
      end do
      idown_Stat=-idown_Stat        !restore the flags.
      return
      end
