      subroutine check_stations(ierr)
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
!      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include 'flux.ftni'
! History
! 2008Jun10  JMG. First version. Taken from snrac

! input    
      integer ierr

! local
      integer i
      integer is
      integer ib
      integer nba
      integer iband(max_band) ! bands in this freq. code
      integer icod/1/

! Get the number of bands. 
      call gtban(icod,nba,iband)

      ierr=0
! check to see that tracks ahve been set, and that we have flux and SEFDs
      do i=1,nba !check for tracks
        ib=iband(i)
        do is=1,nstatn      
          if(trkn(ib,is,icod) .le. 0)  WRITE(LUscn,
     >"('check_stations: ERROR! Track assignments not set up for band ',
     >     a2,' at ',a8, '. Look at $CODES section of scedule.')") 
     >cband(ib), cstnna(is)          
           
          if(sefdst(ib,is).le.0) write(luscn,          
     >"('check_stations: ERROR! SEFDs not present for band ',
     >    a2,' at ',a8, '. Look at T-lines in $STATIONS section')") 
     >  cband(ib),cstnna(is)          
        end do
      enddo !check for tracks
      return
      end
      
   
