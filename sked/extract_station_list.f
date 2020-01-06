      subroutine extract_station_list(luscn,csub,istat_list,num_sub)
! Common blocks
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/statn.ftni'
! Parse the string csub and return a vector containing a list of stations.
! History
! 2008May22. First Version.
! 2011Aug12. Recognize "_" and "ALL" as special tokens. 
! 2015Oct21. Make sure all station codes are unique! This fixex problems with strings like: KkWfWsGGsYjWs which have the same station twice!
! 
      integer luscn
      character*(*) csub
      integer istat_list(*)
      integer num_sub
! functions
      integer trimlen
      integer iwhere_in_string_list
! local
      character*2 cpo_cap(max_stn)      !capitalized list of station codes
      integer i,j
      integer nch
      integer iwhere

      if(csub .eq. "_" .or. csub .eq. "ALL") then 
        do i=1,nstatn
          istat_list(i)=i
        end do
        num_sub=nstatn 
        goto 120
      endif 
 
      do i=1,nstatn
         cpo_cap(i)=cpocod(i)
         call capitalize(cpo_cap(i))
      end do

      call capitalize(csub)
      nch=trimlen(csub)

      i=1
      num_sub=0
      do while(i.lt. nch)
        iwhere=iwhere_in_string_list(cpo_cap,nstatn,csub(i:i+1))
        if(iwhere .eq. 0) then
          write(luscn,'(a)') "Unknown station ", csub(i:i+1)
          num_sub=0
          return
        endif
! Here we do a check to make sure that this station is not already in the list.
        do j=1,num_sub
          if(istat_list(j) .eq. iwhere) goto 100   !Was already in list. Don't add it again. 
        end do          

        num_sub=num_sub+1
        istat_list(num_sub)=iwhere
100     continue
        i=i+2
        if(csub(i:i).eq. "-") i=i+1
      end do

120   continue 

! Remake the two letter list.
      csub=" "
      do j=1,num_sub
        i=2*j-1
        csub(i:i+1)=cpo_cap(istat_list(j))
      end do 

      return
      end







