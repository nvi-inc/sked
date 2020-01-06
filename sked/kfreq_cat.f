      logical function kfreq_cat(cat_rec_in,bw_default,icode)
! See if the BWs match for all stations for this code.
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/statn.ftni'
      include 'cat_mode.ftni'
      include 'cat_rec.ftni'
      include 'cat_stat.ftni'
! 2006Jun22 JMGipson. Changed name. kfreq_match-->kfreq_cat
! 2007Sep10 JMGipson. Changed minor mis-spelling in error message.

! On entry
      character*(*) cat_rec_in          !key into rec.cat
      real*8  bw_default                  !default bandwidth
      integer icode                     !code index in sked file.

! functions
      integer iwhere_in_string_list

! local
      integer irec
      integer istat_skd
      integer i,j

      irec=iwhere_in_string_list(cat_mode_rec,num_cat_mode_rec,
     >    cat_rec_in)

      if(irec .eq. 0) then
        write(luscn, '("kfreq_cat: Unknown receiver line key: ",a)')
     >       cat_rec_in
        stop
      endif

      kfreq_cat=.false.
      do i=1,nstatn
        istat_skd=
     >     iwhere_in_string_list(cat_ant_name,num_cat_ant,cantna(i))
         if(istat_skd .eq. 0) then
            write(luscn,'("kfreq_cat: ",a, " is not in antenna.cat")')
     >      cstnna(i)
            return
         endif
! find a match for this station.
        do j=irec_cat_off(irec),irec_cat_off(irec)+irec_cat_num(irec)
          if(istat_skd .eq. irec_stat(j)) then  !found a match for the stations.
            if(rcat_rec_bw(j) .lt. 0) then  !use default
              if(vcband(1,i,icode) .ne. bw_default) return
            else
              if(vcband(1,i,icode).ne. min(rcat_rec_bw(j),bw_default))
     >                    return
            endif
            goto 200
          endif
        end do
200     continue
      end do
      kfreq_cat=.true.
      end

