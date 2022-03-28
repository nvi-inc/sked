      subroutine make_source_list(cdo,ierr)
! Read in source catalog, and find matches with schedule.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'cat_src.ftni'
! history
!    2005Aug05  JMGipson.  Extracted from socat.f
!    2005Nov22  JMGipson.  Removed fname from call.
!    2008Nov11  JMGipson.  Replaced call to RSPYN by call to kyes_to_prompt

! Passed
      character*1 cdo !s=standard. Query if sources are missing.
                      !a=automatic. Ignore missing sources.

! returned
      integer ierr

! functions
      integer iwhere_in_string_list
      logical kyes_to_prompt

! local
     
      logical kmissing
      integer iwhere
      integer i

      call read_source_cat(ierr)
      if(ierr .ne. 0) return

      kcat_src_sel=.false.
! now check for match.
      kMissing=.false.
      do i=1,nsourc
        iwhere=iwhere_in_string_list(cat_src_iau,num_cat_src,csorna(i))
        if(iwhere .eq. 0)  then     !not found by IAU name, check common name.
          iwhere=iwhere_in_string_list(cat_src_name,num_cat_src,
     >       csorna(i))
          if(iwhere .eq. 0) then
             write(luscn,'(" No entry found for ",a)') csorna(i)
             kMissing =.true.
          else
             kcat_src_sel(iwhere)=.true.     !found!
          endif
        else
          kcat_src_sel(iwhere)=.true.        !Found
        endif
      end do

      if(kMissing) then  !unrecognized code
       ierr=10
       write(luscn,'(a)')
     >  "ERR: Make_source_list: Unrecognized sources in catalogs."
       if(cdo .eq. "a" .or. cdo .eq. "A") goto 100
         write(luscn,'(a)')
     >     " If you continue these will be deleted from the schedule."
        if(.not. kyes_to_prompt("Continue? (Y/N) ")) return
                    
! make list only with the number of sources found
      end if !unrecognized code

100   continue
      cat_src_grade="__"
! Obsolete       
! Now get the grades
!      call grget(flux_comments,cdo,ierr)
      return
      end
