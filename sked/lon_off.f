      character*3 function lon_off(klogical)
      logical klogical

      if(klogical) then
         lon_off="on"
      else
         lon_off="off"
      endif
      return
      end
