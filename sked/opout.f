C@OPOUT
      SUBROUTINE OPOUT(luout,lkind)  !write out $OP section
      include '../skdrincl/skparm.ftni'
C
C   INPUT:
      integer luout
      character*1 lkind     !'s','d','v'
C
C   OUTPUT:

C  COMMON BLOCKS USED:
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include 'minor.ftni'
      include 'major.ftni'
      include 'covar.ftni'
C
C  LOCAL VARIABLES:
      logical l1,l2,l3,l4,l5,l6,l7,l8
      integer i,ii,itype
      integer nch 
      integer isrc
    
C
C  HISTORY:
C  nrv  910906  created
C  nrv  910910  add source parameters out
C  nrv  911028  Change first line of switches
C  nrv  920706  added noise floor and line 2 of parameters
C  nrv  930720  added even # sources
C  nrv  930930  Add low el option
C  nrv  931013  Add expand option
C  nrv  931028  Add rise/set option
C  nrv  931029  Add min slew and min between
! JMG   2003Mar12 Fixed bug in printing kWtStat.
!                Added new options.
! JMG   2003???  Now only covariance stuff. 
! JMG 2012Sep24  Modified to output to VEX. 


      if(lkind .eq. "v" .or. lkind .eq. "s") then
        cbuf="$OP"
        write(luscn,'(a)') trim(cbuf) 
        call wrt_param_line(cbuf,luout,lkind) 
      endif 

      do itype=1,2
        if(lkind .eq. "d") then 
          if(itype .eq. 1) then
             write(ludsp,'("Parameters to Optimize: ")') 
          else
             write(ludsp,'("Parameters to Estimate: ")')
          endif 
        endif 
        write(cbuf,9300) (lpara(i,itype),i=1,5)
9300    format('XP ',L1,' YP ',L1,' DUT ',L1,' PSI ',L1,' EPS ',L1)
        call wrt_param_line(cbuf,luout,lkind) 

        do i=1,nstatn
          L1=lpara(5+2*i-1,itype)
          L2=lpara(5+2*i,itype)
          L3=lpara(5+2*nstatn+(3*i)-2,itype)
          L4=lpara(5+2*nstatn+(3*i)-1,itype)
          L5=lpara(5+2*nstatn+(3*i),itype)
          L6=lpara(5+5*nstatn+(3*i)-2,itype)
          L7=lpara(5+5*nstatn+(3*i)-1,itype)
          L8=lpara(5+5*nstatn+(3*i),itype)
          write(cbuf,9200) cpocod(i),l1,l2,l3,l4,l5,l6,l7,l8
9200      format(a,' AOFF ',L1,' ARAT ',L1,' COFF ',L1,' CRT1 ',L1,
     .    ' CRT2 ',L1,' X ',L1,' Y ',L1,' Z ',L1)
          call wrt_param_line(cbuf,luout,lkind) 
        enddo
        cbuf=" "
               
        nch=1
        do isrc=1,nsourc
          if(lkind .eq. "s".or.lkind .eq. "v") then 
            write(cbuf(nch:nch+5),'(i3," ",l1)') 
     >        isrc, lpara(5+8*nstatn+isrc*2-1,itype)     !have isrc*2-1 because each source has two coordinates.
             nch=nch+6                                      ! If we want to estimat one, then we also estimate the other. 
          else
            write(cbuf(nch:nch+10),'(a," ",l1)') 
     >       csorna(isrc)(1:8), lpara(5+8*nstatn+isrc*2-1,itype)     !have isrc*2-1 because each source has two coordinates.
             nch=nch+11   
          endif  

          if(nch .ge. 60) then
             call wrt_param_line(cbuf,luout,lkind) 
             cbuf=" "
             nch=1
          endif 
        enddo
        if(nch .ne. 1) then
          call wrt_param_line(cbuf,luout,lkind) 
        endif
       

        if(lkind .eq. "d") then
          if(itype .eq.1) then
            write(ludsp,'("Number parameters optimized ",i4)') num_est
          else
            write(ludsp,'("Number paramters estimated ", i4)') num_opt
          endif
        endif 
         

 
      enddo
C
      RETURN
      END
