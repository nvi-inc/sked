      subroutine make_defnames

C MAKE_DEFNAMES determines the names for the defs,
C using names and group numbers. This lets each
C writing subroutine use the same names.


! Start putting in new changes at the top....
! 2017Jun22  JMG. Added new rules for Mark5C and Flexbuff recorders (which record using VDIF format.)

C 990921 nrv New.
C 991110 nrv If the catalog mode is non-blank, use that name.
C 000202 nrv Remove optional mode names (if catalog names are present),
C            and always make them the same way.
C 002020 nrv Squeeze out the ':' character in mode names. Add bit.
C 000317 nrv Need another type for the track_frame_format. May need
C            even more in the future for data_modulation, etc.
C            Make defnames. Add 2-letter frequency code to def names.
C 000328 nrv Bug in referring to lmode.
C 000509 nrv Fix typo in K4 rack name.
C 000830 nrv Make defnames only for stations that have the mode defined.i
C 000906 nrv Use the recording format specified in the modes as they
C            come from the catalog. Formerly, the mode was being
C            defined according to the equipment at the station.
C 001004 nrv Make def names for roll be ROLL8, ROLL16, NO_ROLL.
C 010402 nrv Add a loop to check sideband usage for determining
C            whether a station has the same recorded tracks, and the
C            same frequency channels.
C 020327 nrv Collect rolls over all codes and stations.
C 040630 ZMM uncommented statements setting km3mode;
C            added declaration of jchar
! 2005Apr26  Got rid of jchar, holleriths
! 2005Oct28  Fixed bug. Was setting track_format to VLBA if VLBA4, should be
!            set to Mark4
! 2005Nov17  JMGipson.  Used new function ktrack_match to see if two modes
!            use the same track mapping.
!            Also replaced laborious test to see if was 2-bit sampling.
! 2006Jan19  JMGipson. Variabls, ip,ih were defined as logical. Changed to integer.
! 2006Nov07  JMGipson.  Changed logic of creating names.
!            For Freqs make sure that BBCs also match up.
!            This required checking Freq after BBCs.
! 2012Sep10  Various changes for Mark5B.  Also got rid of logical km3mode which was only used once.    
! 2013Oct11  Changed way BBC and IF mode names are made. Now compares TWO characters if cifinp 
! 2014Jun03  Modified to make K5 use Mark5B track format   
! 2014Sep30  Modified to make K5 use Mark5A track format if correlated at VLBA  
! 2018Dec26  JMG. Fixed bug in writting out $IF for VDIF stations.
! 2020NOV04  JMG. Removed some stuff about Headstacks, passes
! 2020Dec02  JMG. removed debugging statements.

      implicit none 

      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'

! function
      logical ktrack_match
      logical kfreq_match

C Local
      integer npass,ntrks,nhead   !returned from itras_param

      integer is,ic,isx,ib
      integer ilf,ilc,il,ilh         !length of some strings
      integer igroup(9),itype
      integer ich,i1,i2,i3,i4,ix,i,ibit,im,iroll
      logical kok
      character*20 cfrq,chd,cfm,cg,cmx,cbit,cform
      character*20 cmo 
      character*20 ctemp
      character*4 cr
      integer trimlen
      character*12 codtmp
      character*1 lchar
      integer ih,ip        !ipass, iheadstack
      character*128 refdef_tmp             !temporary name. 
      character*8 cstrec_cap 
      integer ioutput_type(max_stn)     !0 = normal, 1=vdif...
      character*5 loutput
     

! Initialze
      do i=1,9
        igroup(i)=0
      end do

         iroll = 0
      do ic=1,ncodes
        cfrq=cnafrq(ic)
        ilf=trimlen(cfrq)
        codtmp=ccode(ic)
        ilc=trimlen(ccode(ic))
        modedef_name(ic) = cfrq(1:ilf)//'.'//codtmp(1:ilc)//char(0)
       
        do is=1,nstatn
          if (nchan(is,ic).gt.0) then ! this mode defined

          cstrec_cap=cstrec(is,1)
          call capitalize(cstrec_cap)
          ioutput_type(is)=0   !default is no special output          
          loutput=" "

          if(cstrec_cap.eq."MARK5C" .or.cstrec_cap.eq."FLEXBUFF") then
             ioutput_type(is) =1         
             loutput="VDIF"
          endif 
        
          cmo=cmode(is,ic)      

C         Squeeze out any ':' character
          chd=cmo
          ix = index(cmo,":")        !this usually indicates fanout. 
          if (ix.gt.0) then 
             chd=cmo(1:ix-1)          !make the head position name the first part of this. 
             il = trimlen(cmo)
             cmo=cmo(1:ix-1)//"_"//cmo(ix+1:il)//"f"   !Put the fanout in the name.
          endif !
          ilh=trimlen(chd) 


! find out how many bits
          call itras_params(is,ic,npass,ntrks,nhead,ibit)
          write(cbit,'(i1)') ibit
          il=trimlen(cmo)
          cmo = cmo(1:il)//"_"//cbit(1:1)//"b"         !put the bitmap in the name  
          cfm=cmfmt(is,ic)
          im=trimlen(cmo)

C 2. BBC names
          itype=2
          do isx=1,is-1
            if (nchan(is,ic).eq.nchan(isx,ic)) then ! nchan matches
              kok=.true.
              do ib=1,nchan(is,ic) ! check each channel
                ich = invcx(ib,is,ic)
                if (cifinp(ich,is,ic)(1:2).eq.cifinp(ich,isx,ic)(1:2)
     >             .and. ibbcx(ich,is,ic).eq.  ibbcx(ich,isx,ic)) then ! matches
                else ! no match
                  kok=.false.
                endif ! match/none
              enddo ! check each channel
!              if (kok.and. ioutput_type(is) .eq. ioutput_type(isx)) then
              if(kok) then 
                refdef_name(itype,is,ic)=refdef_name(itype,isx,ic)             
                goto 200   !exit loop.
              endif ! all channels match
            endif ! nchan matches
          enddo ! check each group so far
! Went through loop without a match.
          igroup(itype)=igroup(itype)+1
          write(cg,'(i2.2)') igroup(itype)
          il=trimlen(cg)
          refdef_name(itype,is,ic) =
     >          cfrq(1:ilf)//'-'//codtmp(1:ilc)//cg(1:il)   
200       continue

! Interchange BBC and FREQ because we now check that BBC is the same.
! To do so, must have alredy checked BBC.
C 1. FREQ names
          itype=1
          if(loutput .ne. " ") then
!             write(*,*) "Fast exit ", cstnna(is) 
             refdef_name(itype,is,ic)=loutput
             goto 100 
          endif 

          do isx=1,is-1
             if(kfreq_match(is,ic,isx,ic) .and. 
     >        ioutput_type(is) .eq. ioutput_type(isx)) then
               refdef_name(itype,is,ic)=refdef_name(itype,isx,ic)          
              goto 100
            endif ! nchan matches
          enddo ! check each group so far
! Went through loop without a match.
          igroup(itype)=igroup(itype)+1
          write(cg,'(i2.2)') igroup(itype)
          il=trimlen(cg)     
          refdef_name(itype,is,ic) =
     >       trim(cfrq)//'-'//trim(codtmp)//trim(cg)
       
!          write(*,*) "----> ",refdef_name(itype,is,ic)        
100       continue

C 3. IF names
          itype=3
          do isx=1,is-1                           
            if (nchan(is,ic).eq.nchan(isx,ic)) then ! nchan matches
              kok=.true.
              i1=0
              i2=0
              i3=0
              i4=0              
              do ib=1,nchan(is,ic)
                ich = invcx(ib,is,ic)
                lchar=cifinp(ich,is,ic)(1:1)
                if(i1.eq.0.and. lchar.eq.'1'.or.lchar.eq.'A') i1=ich
                if(i2.eq.0.and. lchar.eq.'2'.or.lchar.eq.'B') i2=ich
                if(i3.eq.0.and. lchar.eq.'3'.or.lchar.eq.'C') i3=ich
                if(i4.eq.0.and. lchar.eq.'4'.or.lchar.eq.'D') i4=ich
              enddo
           
              do i=1,4 !
                if (i.eq.1) ix=i1
                if (i.eq.2) ix=i2
                if (i.eq.3) ix=i3
                if (i.eq.4) ix=i4   
                if (ix.ne.0) then ! compare
                  if (cifinp(ix,is,ic)(1:2).eq.cifinp(ix,isx,ic)(1:2)
     .              .and. freqlo(ix,is,ic).eq.freqlo(ix,isx,ic)
     .              .and.losb(ix,is,ic).eq.losb(ix,isx,ic))   then
                  else
                    kok=.false.
                  endif
                endif ! compare
              enddo !
              if (kok) then ! all IFs match
                refdef_name(itype,is,ic)=refdef_name(itype,isx,ic)     
                goto 300
              endif ! all IFs match
            endif ! nchan matches
          enddo ! check each group so far
          igroup(itype)=igroup(itype)+1
          write(cg,'(i2.2)') igroup(itype)
          il=trimlen(cg)
          refdef_name(itype,is,ic) =
     >         cfrq(1:ilf)//'-'//codtmp(1:ilc)//cg(1:il)       
!          write(*,*) "Hi there", refdef_name(itype,is,ic),cstnna(is) 
300       continue

C 4. TRACK names
          itype=4
! Previously treated VDIF special
!          if(loutput .eq. "VDIF") then
!             refdef_name(itype,is,ic) = "VDIF"     
!              refdef_name(itype,is,ic)= " " 
!             goto 400
!          endif 
          do isx=1,is-1             
            if(ktrack_match(is,ic,isx,ic) .and. 
     >        ioutput_type(is) .eq. ioutput_type(isx)) then 
              refdef_name(itype,is,ic)=refdef_name(itype,isx,ic)    
              goto 400
            endif ! all tracks match
          enddo ! check each group so far  
          igroup(itype)=igroup(itype)+1
          write(cg,'(i2.2)') igroup(itype) 
          il=trimlen(cg)
          refdef_name(itype,is,ic) =
     >          cmo(1:im)//'-'//codtmp(1:ilc)//cg(1:il)
       
400       continue

C 5. HDPOS names
! Skip. 
         

C 6. PASS_ORDER names (same as HEAD_POS)
          

C 7. ROLL names
! skip
 
C 8. PHASE_CAL_DETECT names (STANDARD is the only one implemented)
          itype=8
          refdef_name(itype,is,ic)='Standard'
         
C 9. TRACK_FRAME_FORMAT names
          itype=9
          cform='unknown'

! Make temporary mode name...
C  Mark3 format for Mk3 formatters and for VLBA formatters recording a standard
C  Mk3 mode (A,B,C,D,E) or recording a mode that starts with "M".
         if(cmode(is,ic)(1:1).GE."A".and.cmode(is,ic)(1:1).LE."D") 
     >                                   cform='Mark3A'
         if(cmode(is,ic)(1:1) .eq. "V") cform="VLBA"
         if(cmode(is,ic)(1:1) .eq. "M") cform="Mark4"
         if(cstrack(is) .eq. "VLBA4")   cform="Mark4"
  
! some exceptions which depend on recorder type...
         select case(cstrec_cap)
         case("MARK5B")        
           cform="Mark5B"
         case("MARK5C", "FLEXBUFF") 
           cform="VDIF"
         case("K5") 
           if(kvlba_corr) then
              cform="Mark4"
           else
             cform="Mark5B"
           endif
         case default
         end select 

     
C***********************************************************
         refdef_name(itype,is,ic)= trim(cform)//'_format'  
        

        endif ! this mode defined
        enddo ! stations
      enddo ! codes
! Now null-terminate all the names
      do ic=1,ncodes
      do is=1,nstatn
      do itype=1,9
         call null_term(refdef_name(itype,is,ic))
      end do
      end do
      end do 
       


      end
