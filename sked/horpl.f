      subroutine horpl(is,xmin,xmax,ymin,ymax,ctype,
     .luplt)
C
C     HORPL fills in the plotting arrays with horizon information
C  930439 nrv Write out plotting info one station at a time
C 960409 nrv Add line-segment ability
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
C
C  Input
      integer is ! station index
      real*4 xmin,xmax,ymin,ymax !plotting limits
      character*2 ctype ! PO or XY (polar or xy plot)
      integer luplt ! output file unit
C
C  Common
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C  Local
      integer nhz,nh,j
      integer iy,iz
      real*4 azd,eld,x,y
      real*4 el1,el2,el,elsave,elinc,azinc,az1
      real*4 skw_az(MAX_HOR),skw_el(MAX_HOR) ! values to plot
C
C
! AEM20051102 init skw_az and skw_el to 0
      skw_az = 0
      skw_el = 0
C 1. Check plot limits and adjust values to plot.
        nhz=nhorz(is)
        nh=0
        if (nhz.gt.0) then
          do j=1,nhz !loop on horizon points
            azd=azhorz(j,is)*rad2deg
            eld=elhorz(j,is)*rad2deg
            if (azd.ge.xmin.and.azd.le.xmax) then !within az limits
              nh=nh+1
              if (nh.eq.1.and.azd.ne.xmin) then !create new start point
                skw_az(1)=xmin
                skw_el(1)=min(real(elhorz(j-1,is)*rad2deg),
     .                          real(ymax))
                skw_el(1)=max(ymin,skw_el(1))
                nh=2
              end if
              skw_az(nh)=azd
              skw_el(nh)=min(eld,ymax)
              skw_el(nh)=max(ymin,skw_el(nh))
            else !beyond limits
              if (nh.gt.0) then !create new end point
                nh=nh+1
                skw_az(nh)=xmax
              endif !create new end point
            endif !within/beyond az limits
          enddo !loop on horizon points
        endif
C
C 2. Someday add plot of coordinate mask

C       if (ncord(is).gt.0) then
C         do j=1,ncord(is)
C           skw_az(j,i)=f(co1mask(j,is),co2mask(j,is))
C           skw_el(j,i)=f(co1mask(j,is),co2mask(j,is))
C         enddo
C       endif
C
C  3. Write out plotting points

        elinc = (ymax-ymin)/300.0
        azinc = (xmax-xmin)/700.0
        az1=xmin
        do while (az1.le.xmax)
          iy=1
C         Find the range that az1 falls into.
          DO WHILE(iy.Lt.NHZ.AND.
     .      (az1.LT.skw_az(iy).OR.az1.GE.skw_az(iy+1)))
            iy=iy+1
          ENDDO
          if (klineseg(is)) then ! interpolate
            if (skw_az(iy).eq.skw_az(iy+1)) then ! inf slope
              el = skw_el(iy)
            else ! interpolate
              el = ((skw_el(iy+1)-skw_el(iy))/(skw_az(iy+1)-skw_az(iy)))
     .             *(az1-skw_az(iy)) + skw_el(iy)
            endif
          else ! use step function value
            el = skw_el(iy)
            if (az1.gt.xmin.and.az1.lt.xmax.and.abs(elsave-el).gt.elinc)
     .        then !a step
              el1=el
              el2=elsave
              if (el2.lt.el1) then
                el1=elsave
                el2=el
              endif
              el1 = el1+elinc
              do while (el1.lt.el2)
                if (ctype.eq.'XY') then
                  write(luplt,9100) az1,el1 
                else !PO
                  call azel2xy(az1,el1,x,y)
                  write(luplt,9100) x,y
                endif
                el1 = el1+elinc
              enddo
            endif !a step
          endif ! interpolate or step function
          if (ctype.eq.'XY') then
            write(luplt,9100) az1,el
          else !PO
            call azel2xy(az1,el,x,y)
            write(luplt,9100) x,y
          endif
9100      format('1   ',4f8.3)
          elsave = el
          az1=az1+azinc
        enddo ! az1.le.xmax
       
        if (ctype.eq.'PO') then !draw outer circle
          do iz=1,360
            x = 90.0*sin(iz*deg2rad)
            y = 90.0*cos(iz*deg2rad)
            write(luplt,9102) x,y
9102        format('1  ',2f8.3)
          enddo
        endif
C     enddo
C
      return
      end
