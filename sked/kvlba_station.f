      logical function kvlba_station(cpo)
      implicit none
! passed
      character*2 cpo    ! two char station ID
            
! Function
      integer iwhere_in_string_list

! local 
      integer num_vlba_stat
      parameter(num_vlba_stat=10)
      character*2 cvlba_stat(num_vlba_stat)
     >  /"Br","Fd","Hn","Kp","La","Mk","Nl","Ov","Pt","Sc"/

      kvlba_station  = 
     >      iwhere_in_string_list(cvlba_stat,num_vlba_stat,cpo).ne.0         
      return
      end 
