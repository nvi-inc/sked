      SUBROUTINE FRLIS(istn,nst)
C
C     FRLIS lists the selected frequencies
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'      
C
C  Input
      integer istn(max_stn) ! list of stations
      integer nst ! how many in list

C  CALLING SUBROUTINES: FRCMD
C
C  LOCAL VARIABLES
      integer is1            !station list
      integer is2           !station list 
      integer ic            !code counter   
      logical kmatch            !Found a match
      logical kdone(max_stn)    !Have we done this station. 
      integer num_in_grp        !number in group

  
C  History
C     880310 NRV DE-COMPC'D
C     891128 NRV Added more information
C     940620 nrv Added number of recorded tracks
C 951019 nrv Add indices on VCBAND, LMODE
C 951020 nrv Add #tracks/pass to listing
C 951019 nrv Add station per channel listing
C 951116 nrv Change to per station frequency listings
C 960209 nrv Print only unique sets of frequencyes.
C            Add option to print complete list for one station.
C 960510 nrv Don't list LO setup with default option, can get
C            it only for a single station.
! 2006Nov02  JMG Pretty print.
! 2009Oct01-15  JMG. More pretty print
! 2015Feb12  JMG. Simplified....

C
C
C     1. Simply list the frequencies selected by the user, getting the
C        names from COMMON.  First check that there are some to list.
C
      IF (NCODES.EQ. 0) THEN
        WRITE(LUSCN, '("FRLIS01 - No frequencies selected.")')
        return
      END IF     

! Simplest case is to list out this info station by station.
      if(nst .ne. 0) then
        do i=1,nst
          is1=istn(i)
          DO Ic=1,NCODES !each code
            write(LUDSP,"('  Name  Code '/a8,1x,a2)") cNAFRQ(IC),
     >        LCODE(IC)
            write(ludsp,'("Recording mode setup for ",a8)') cstnna(is1)
            call printfr(is1,ic,2)
          enddo ! each code
        enddo
        return
      endif

! Alternative is to print out all stations that have same setup.  
      do ic=1,ncodes ! codes
        if(ic .ne. 1) then
          write(ludsp,'(a)') " "
          write(ludsp,'(a)') "**********************************"
        endif
        do is1=1,nstatn
          kdone(is1)=.false.         
        enddo  
        write(LUDSP,"('  Name  Code '/a8,1x,a2)") cNAFRQ(Ic),LCODE(Ic)
! Now we loop over stations for this code.
        do is1=1,nstatn ! stations
          num_in_grp=0
          if (.not.kdone(is1)) then
            kdone(is1)=.true. 
            num_in_grp=1
            write(ludsp,'("Recording mode for: ",a," ",$)')
     >                 cstnna(is1)     
! Now check other stations for a match...          
            do is2=is1+1,nstatn ! check remaining stations            
              if (.not.kdone(is2)) then ! try this one
                if(cmode(is1,ic) .eq. cmode(is2,ic) .and.
     .             nchan(is1,ic).eq.nchan(is2,ic).and.
     .             npassf(is1,ic).eq.npassf(is2,ic).and.
     >             cbarrel(is1,ic) .eq. cbarrel(is2,ic) .and.   
     .             ifan(is1,ic).eq.ifan(is2,ic).and.
     .             ntrakf(is1,ic).eq.ntrakf(is2,ic).and.
     .             nfreq(1,is1,ic).eq.nfreq(1,is2,ic).and.
     .             nfreq(2,is1,ic).eq.nfreq(2,is2,ic).and.
     .             trkn(1,is1,ic).eq.trkn(1,is2,ic).and.
     .             trkn(2,is1,ic).eq.trkn(2,is2,ic).and.
     .             vcband(1,is1,ic).eq.vcband(1,is2,ic)) then ! check channels
                   kmatch=.true. 
!                 Sky frequency separately.
                  do i=1,nchan(is1,ic)      
                    if (freqrf(i,is1,ic).ne.freqrf(i,is2,ic)) 
     >                    kmatch=.false.
                  enddo
                  if(kmatch) then
                     num_in_grp=num_in_grp+1
                     if(num_in_grp .eq. 10) then              
                       write(ludsp,'(/,"                    ",$)')  !
                       num_in_grp=1
                     endif
                     write(ludsp,'(a8," ", $)') cstnna(is2)
                     kdone(is2) = .true. 
                  endif                 
                endif
              endif
            enddo ! check remaining stations'
          endif ! new group      
          if(num_in_grp .ne. 0) then 
            write(ludsp,'()')
            call printfr(is1,ic,1)
          endif 
        enddo ! stations
      end do  ! codes 
   
      RETURN
      END
