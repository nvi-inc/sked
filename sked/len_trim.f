      integer function len_trim(cstring)
      character*(*) cstring

      len_trim=len(cstring)

      do len_trim=len(cstring),0,-1
         if(cstring(len_trim:len_trim) .ne. " ") return
      end do
      return
      end



