      subroutine proc_thread(cproc_thread)
      implicit none 
      include 'drcom.ftni'
      include 'hardware.ftni' 
! generate new thread procedure
      character*(*) cproc_thread
     
      call proc_write_define(lu_outfile, luscn,cproc_thread)
      if(kflexbuff) then 
        write(lu_outfile,'(a)') "fb=datastream=clear"
        if(lvdif_thread .eq. "YES") then
          write(lu_outfile,'(a)') "fb=datastream=add:{thread}:*"
        endif
        write(lu_outfile,'(a)') "fb=datastream=reset"
      else
        write(lu_outfile,'(a)') "mk5=datastream=clear"
        if(lvdif_thread .eq. "YES") then
          write(lu_outfile,'(a)') "mk5=datastream=add:{thread}:*"
        endif
        write(lu_outfile,'(a)') "mk5=datastream=reset"
      endif
      write(lu_outfile,'(a)') "endef"
      end 
   


