      subroutine astro_out(luout,kall,knumber,lkind)
! write out the astrometric sources.
      include '../skdrincl/skparm.ftni'
C
C   COMMON BLOCKS USED
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include 'astro.ftni'

! 2007Jul02 JMG. Added astro.ftni which was separated from sourc.ftni
! 2012Sep24  JMG.  Modified to output to VEX as well. 
! 2019Mar16 JMG. Use kastro_src instead of checking rmin_astro, rmax_astro  

! passed
      integer luout
      logical kall      !list all sources, or just ones set.
      logical knumber   !number the sources
      character*1 lkind 

! local
      integer i
      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$ASTROMETRIC"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      do i=1,nsourc
       if(kastro_src(i) .or. kall) then
         if(knumber) then
           write(cbuf,'(i4," ",a," ",2f8.2)') i, csorna(i),
     >          rmin_astro(i)*100., rmax_astro(i)*100.
         else 
           write(cbuf,'(a,1x,2f8.2)') csorna(i),rmin_astro(i)*100.,
     >          rmax_astro(i)*100.
         endif
         call wrt_param_line(cbuf,luout,lkind) 
        endif
      end do
      return
      end

