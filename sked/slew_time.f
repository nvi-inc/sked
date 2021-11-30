!*************************************************************************************************      
      real function slew_time(x1,x2,off,vel,acc)      
! Passed      
      real x1,x2   !starting stopping point
      real off     !settling time
      real vel     !velocity          
      real acc     !acceleration

! local
      real dist 
      real t_acc   !time to accelerate to terminal velocity
      
      dist=abs(x1-x2)
      t_acc=vel/acc
      
      if(dist  .le.  acc*t_acc*t_acc) then
         slew_time=sqrt(dist/acc)
      else
         slew_time=dist/vel+t_acc
      endif
      slew_time=slew_time+off 
      return
      end 
