      SUBROUTINE vsiout
C
C  This routine writes out the VEX $SITE section.
C
C   HISTORY:
C 990606 nrv New. Copied from vsoout.
C 990922 nrv Use VEX utilities.
C 991006 nrv Add call with null to end az/el lists.
! 2009Sep15  JMGipson. Removed debugging statement
! 2014Sep16. JMG. Only write out occupation code if we have one!
C
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
C
C     CALLED BY:   SKCLS
C
C  LOCAL
      integer is,j
      integer ptr_ch
      character*20 cst,cx,cy,cz
      integer iazel

C  1. SITE block

      call fcreate_block(ptr_ch("SITE"//char(0)))
      write(luscn,'("SITE",$)')

C  2. each position

      do is=1,nstatn

C def
        cst=cstnna(is)(1:8)
        call null_term(cst)
        call fcreate_def(ptr_ch(cst))
C site_type
        call fcreate_site_type(ptr_ch('fixed'//char(0)))
C site_name
        cst=cstnna(is)(1:8)
        call null_term(cst)
        call fcreate_site_name(ptr_ch(cst))
C site_ID
        cst=cpocod(is)(1:2)
        call null_term(cst)
        call fcreate_site_ID(ptr_ch(cst))
C site_position
        write(cx,'(f12.3)') stnxyz(1,is)
        call null_term(cx)
        write(cy,'(f12.3)') stnxyz(2,is)
        call null_term(cy)
        write(cz,'(f12.3)') stnxyz(3,is)
        call null_term(cz)
        call fcreate_site_position(ptr_ch(cx),ptr_ch('m'//char(0)),
     .                             ptr_ch(cy),ptr_ch('m'//char(0)),
     .                             ptr_ch(cz),ptr_ch('m'//char(0)))
C horizon_map_az
        if (nhorz(is).gt.0) then
          do iazel=1,2
!            write(*,*) " "
            if(iazel .eq. 1) then
!             write(*,'("AZ ",$)')
            else
!             write(*,'("EL ",$)')  
            endif
            do j=1,nhorz(is)
              if(iazel .eq. 1) then
                 write(cx,'(f5.1)') azhorz(j,is)*rad2deg
!                 if(is .eq. 8) write(*,'(a,1x,$)') cx(1:6)
              else
                 write(cx,'(f5.1)') elhorz(j,is)*rad2deg
!                 if(is .eq. 8) write(*,'(a,1x,$)') cx(1:6)
              endif
              call null_term(cx)
              if (j.eq.1) then
                call fcreate_horizon_map(ptr_ch(cx),
     .                  ptr_ch('deg'//char(0)))
              else ! don't need deg
                call fcreate_horizon_map(ptr_ch(cx),
     .                  ptr_ch(char(0)))
              endif
            enddo
            if(iazel .eq. 1) then
              call fcreate_horizon_map_az
            else
              call fcreate_horizon_map_el
            endif
          end do
        endif
C occupation_code
        cst=coccup(is)
        if(len_trim(cst) .gt. 0) then         
          call null_term(cst)
          call fcreate_occupation_code(ptr_ch(cst))
        endif 

      enddo

      write(luscn,'()')
      RETURN
      END
