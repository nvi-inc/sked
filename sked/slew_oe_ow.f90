!**************************************************
double precision function slew_oe_ow(az_beg,el_beg,az_end,el_end,az_slew_time,el_slew_time)
!subroutine oe_ow_slew(az_beg,el_beg,az_end,el_end,cwrap,az_slew_time,el_slew_time)
implicit none
! Return the slewing time in seconds. 
! All angles in DEGREES. 
double precision  :: az_beg,el_beg    !starting Az,el  in degrees.
double precision  :: az_end,el_end    !ending  az, el in degrees

! Az limits are -90 to 450 degrees
! El limits are 0 to 100. Although in practice go from 5 to 90.

! Use the following slewing model. Copied directly from an email from Eskil Varenius at Onsala. 
! Date ~ 2021-December. 
!Azimuth region             Speed
!  -90 to -65 deg          12 deg/s CW, 1.0 deg/s CCW (towards -90 limit)
!  -65 to -40 deg          12 deg/s CW, 3.5 deg/s CCW (towards -65 limit)
!  -40 to +400 deg         12 deg/s both CW and CCW
! +400 to +425 deg         12 deg/s CCW, 3.5 deg/s CW (towards +425 limit)
! +425 to +450 deg         12 deg/s CCW, 1.0 deg/s CW (towards +450 limit)
integer, parameter :: num_az_pt=5
! Below are points directly from email. 
!double precision, dimension(num_az_pt+1) :: az_ang  = (/-90.0d0, -65.0d0, -40.0d0, 400.0d0, 425.0d0, 450.0d0/) 
! this adds 360 to be consistent with sked which assumes all az >0. 
double precision, dimension(num_az_pt+1) :: az_ang  = (/270.0d0, 295.0d0, 320.0d0, 760.0d0, 785.0d0, 810.0d0/) 

double precision, dimension(num_az_pt)   :: az_vel_up = (/12.0d0, 12.0d0, 12.0d0, 3.5d0,   1.0d0/)
double precision, dimension(num_az_pt)   :: az_vel_dn = (/1.0d0,   3.50d0, 12.d0, 12.0d0, 12.0d0/)

!------------------------------------------------
!  Elevation region     Speed
! 0 to 5 deg              6 deg/s up, 0.3 deg/s down
! 5 to 15 deg             6 deg/s up, 3.5 deg/s down7
! 15 to 85 deg            6 deg/s both up/down
! 85 to 95 deg            3.5 deg/s up, 6 deg/s down
! 95 to 100 deg           0.3 deg/s up, 6 deg/s down
!--------------------
integer, parameter :: num_el_pt = 5 
double precision, dimension(num_el_pt+1) :: el_ang  =(/0.0d0, 5.0d0, 15.d0,85.0d0, 95.0d0, 100.0d0/) 
double precision, dimension(num_el_pt) :: el_vel_up=(/6.0d0, 6.0d0, 6.d0, 3.5d0, 0.3d0/)
double precision, dimension(num_el_pt) :: el_vel_dn=(/0.3d0, 3.5d0, 6.d0, 6.0d0, 6.0d0/)

double precision step_slew_time         !function to calculate slew if slewtime varies in steps. 

! local variables

double precision az_slew_time                     !Az slew time
double precision el_slew_time                     !El slew_time 
double precision, parameter :: slew_off=6.d0      !Constant term to be added to all slew calculations.
!double precision  az_beg_in, az_end_in            !Positions corrected for cable wrap.

! Calculate the total azimuth time. 
!Write(*,*) "Az ", az_beg, az_end
if(az_end .gt. az_beg) then   !Azimuth is increasing. 
   az_slew_time=step_slew_time(az_beg,az_end,az_ang,az_vel_up,num_az_pt)
else if(az_end .lt. az_beg) then  !Azimuth is decreasing. Note that we swap arguments below. 
   az_slew_time=step_slew_time(az_end, az_beg, az_ang,az_vel_dn,num_az_pt)
else
   az_slew_time=0.d0
endif
    
!write(*,*) "El " , el_Beg, el_end    
if(el_end .gt. el_beg) then      !Elevation is increasing
   el_slew_time=step_slew_time(el_beg,el_end,el_ang,el_vel_up,num_el_pt)
else if(el_end .lt. el_beg) then  !Elevation is decreasing. Note that we swap arguments below. 
   el_slew_time=step_slew_time(el_end,el_beg,el_ang,el_vel_dn,num_el_pt) 
else
   el_slew_time=0.d0
endif 

!Return the slew time (including offset) 
slew_oe_ow=max(az_slew_time,el_slew_time)+slew_off
return
end function 
 
       
!*****************************************************
double precision function step_slew_time(ang_lo, ang_hi, ang_vec, rate,num_ang)
implicit none 
! Function to calculate slew time.  
! Slew rate is a function of region:
!              Region               |    Velocity
!  ang_vec(i)   to  <= ang_vec(i)   |    rate(i)    

! On entry.
double precision ang_lo, ang_hi              !ang_lo < ang_hi

double precision ang_vec(num_ang+1)          !strictly increasing series of points
double precision rate(num_ang)               !rates
integer num_ang 

! Local
integer ilo, ihi                              !which interval is ang_lo and  ang_hi in. 
integer i                                     !i counter 
double precision step_time                    !Time to travel  some step

! some error checking
if(ang_lo .gt. ang_hi) then
   write(*,*) "Step_slew_time: ang_lo>ang_hi please fix ", ang_lo, ang_hi
   stop
endif
if(ang_lo .lt. ang_vec(1) .or. ang_lo .gt. ang_vec(num_ang+1)) then
  write(*,*) "Ang_lo ", ang_lo, " out of limits ", ang_vec(1), ang_vec(num_ang+1)
  stop
endif
if(ang_hi .lt. ang_vec(1) .or. ang_hi .gt. ang_vec(num_ang+1)) then
  write(*,*) "Ang_hi ", ang_lo, " out of limits ", ang_vec(1), ang_vec(num_ang+1)
  stop
endif

! Find what region ang_lo and ang_hi are in.  Start in middle.  
! Since only have a few points just step. If we had a lot could use bi-jection.
ilo=(num_ang+1)/2 
do while(ilo .gt. 1 .and. ilo .le. num_ang) 
   if(ang_lo .ge. ang_vec(ilo) .and. ang_lo .lt. ang_vec(ilo+1)) exit     !In region. Exit
   if(ang_lo .ge. ang_vec(ilo+1)) then                                    !Need to move up.
     ilo=ilo+1
   else
     ilo=ilo-1                                                            !Need to move down.
   endif 
end do

ihi=ilo                                 !Note that ang_hi >ang_lo.  this means ihi >= ilo
do ihi=1,num_ang
   if(ang_hi .ge. ang_vec(ihi) .and. ang_hi .lt. ang_vec(ihi+1)) exit
end do     


!write(*,'(i3,4f8.2)') ilo, ang_vec(ilo), ang_lo, ang_vec(ilo+1), rate(ilo)
!write(*,'(i3,4f8.2)') ihi, ang_vec(ihi), ang_hi, ang_Vec(ihi+1), rate(ihi)

! Two possible cases. 
! 1. In the same interval.  ihi=ilo
! 2. In different intervals. 
! 3. Some intermediate intervals.  

if(ilo .eq. ihi) then
  step_slew_time=(ang_hi-ang_lo)/rate(ilo) 
  return
endif

! Start computation. 
!     First term is time to get from ang_lo to top of 'ilo' interval. 
!     Second  is time to get from bottom of 'ihi' interval to ang_hi 
step_slew_time=(ang_vec(ilo+1)-ang_lo)/rate(ilo)+ (ang_hi-ang_vec(ihi))/rate(ihi)

! now add in the slew times for the intervening intervals. 

do i=ilo+1,ihi-1
  step_time= (ang_vec(i+1)-ang_vec(i))/rate(i)
  step_slew_time=step_slew_time+step_time 
end do 
return
end function 
 










