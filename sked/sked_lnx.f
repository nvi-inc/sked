      program sked
      character file_name*128
      if ( iargc() .le. 0 ) then
           file_name = ' '
      else
           call getarg(1,file_name)
      end if
      call fsked(file_name)
      end
