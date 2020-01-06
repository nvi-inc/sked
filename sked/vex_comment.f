!********************************************************************************************
      subroutine vex_comment(lstring)
! function
      implicit none
      integer ptr_ch
      character*(*) lstring
      character*80 ldum
! 2017Feb27 JMG. Modified call to fcreate_comment because did not work with gfortran. 
 
      ldum="* "//trim(lstring)//char(0) 
      call fcreate_comment(ptr_ch(" "//char(0)),ptr_ch(ldum))
 
! For some reason this does not work???? 
!      call fcreate_comment(ptr_ch(" "//char(0)),
!     >   ptr_ch("* "//trim(lstring)//char(0)))
    
      return
      end
!********************************************************************************************
      subroutine vex_trailing_comment(lstring)
! function
      implicit none
      integer ptr_ch
      character*(*) lstring
      character*80 ldum 
! 2018Oct01. JMG. Modified to be consistent with vex_comment aobout. 

      ldum="* "//trim(lstring)//char(0) 
      call fcreate_comment(ptr_ch("t"//char(0)),ptr_ch(ldum))

!      call fcreate_comment(ptr_ch("t"//char(0)),
!     >   ptr_ch("* "//lstring//char(0)))
      return
      end

!********************************************************************************************
      subroutine vex_end_section_comment(lstring)
! Modified to be consistent with older version of comments.
! function
      implicit none
      integer ptr_ch
      character*(*) lstring
      character*81 csepcom

! make a string that looks like:
!     *--------------------  end lstring   -------------*
      call fend_def
      csepcom="*-----------------------   end "//trim(lstring)//
     > " ----------------------*"//char(0)

      call fcreate_comment(ptr_ch(" "//char(0)),ptr_ch(csepcom))
      return
      end
!********************************************************************************************
      subroutine vex_begin_section_comment(lstring)
! function
! Modified to be consistent with older version of comments.
      implicit none
      integer ptr_ch
      character*(*) lstring
      character*81 csepcom

! make a string that looks like:
!     *-------------------- begin lstring   -------------*
      csepcom="*----------------------- begin "//trim(lstring)//
     > " ----------------------*"//char(0)
    

      call fcreate_comment(ptr_ch(" "//char(0)),ptr_ch(csepcom))
      return
      end


