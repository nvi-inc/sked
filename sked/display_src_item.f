!****************************************************************************************
      subroutine display_src_item(cname,iptr)

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_src.ftni'
      character*(*) cname
      integer iptr

      if(iptr .eq. 0) then
        cname="Source    Grade "
      else
        if(cat_src_name(iptr) .ne. "$") then
          cname=  cat_src_name(iptr)//"  ("//cat_src_grade(iptr)//") "
        else
          cname=  cat_src_iau(iptr)//"  ("//cat_src_grade(iptr)//") "
        endif
      endif
      return
      end
