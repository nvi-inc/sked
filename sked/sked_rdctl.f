      SUBROUTINE sked_rdctl(luscn,                                    
     >   source_cat,station_cat,antenna_cat,position_cat,           
     >   equip_cat, mask_cat,   freq_cat,   rx_cat,                
     >   loif_cat,  modes_cat,  modes_description_cat,  rec_cat,
     >   hdpos_cat, tracks_cat,flux_cat,flux_comments,
     >   cmaster_dir, cat_program_path, par_program_path,          
     >   csked, cscratch,cprtlan,cprtpor,cprttyp,cprport)
   
   
! Read in the control file to set preferences.
! Does this twice:
!  First time trying to find global control file.
!  Second time local copy which overrides global settings.
!
! In September 2012 modified to only read sked settings. 
! Previous version (=rdctl.f) read in both sked and drudg. 
! 
! 2019Apr24 JGipson.  Default control file is /etc/skedf.ctl
! 2019Nov08 JGipson.  Corrected alternate from /usr/local/bin to /usr/local/bin/skedf.ctl
! 2020Oct01 JGipson.  Location of control file is now in skedf_ctl.ftni

      include 'mysql_common.i'
      include 'skedf_ctl.ftni'

! Returned
      character*128  source_cat,station_cat,antenna_cat,position_cat,
     >               equip_cat,modes_description_cat,cat_program_path,
     >               par_program_path,
     >               mask_cat,freq_cat,rx_cat,loif_cat,modes_cat,
     >               hdpos_cat,tracks_cat,flux_cat,flux_comments,
     >               rec_cat,cmaster_dir
      character*128 csked               !default schedule directory
      character*128 cscratch            !default scratch direcotry
      character*(*) cprtlan,cprtpor,cprttyp, cprport 
       
               
      integer luscn
! functions
      integer iwhere_in_string_list
      integer  trimlen     !function call
C
C  LOCAL VARIABLES
      integer ind,nch
      integer itmplen          !variable for filename length
      logical*4 kexist         !control file existence
      character*128 ctemp      !temporary control file variable
      character*10 lsecname
      character*3 lprompt
      integer itemp
      character*20 lkeyword     !keyword
      character*128 lvalue      !value
      character*128 cat_dir     !default catalog directory
      integer nch_cat_dir       !length

      integer MaxToken
      integer NumToken
      parameter(MaxToken=6)
      character*128 ltoken(MaxToken)
      equivalence (lkeyword,ltoken(1))
      equivalence (lvalue,ltoken(2))

      integer lu          !open lu
      integer ic,i,j,ilen,ierr
      character*256 cbuf
      logical ktoken
      logical keof      !EOF reached in reading in file
      logical kfound_global_file
      logical kfirst_skip    


C  1. Open the default control file if it exists.   

! Initialization

      kfound_global_file=.false. 

      ilen = 0
      ierr = 0
      lu = 11
 
      cmaster_dir="NONE"
      cat_dir=" "
      nch_cat_dir=0

! 2. Process the control file if it exists. Loop throug 3 times.
!     The first two times check for the global skedf.ctl
!     The last time for the local file. 

      kfirst_skip=.true. 
      do j=1,2    
! We write out sections that are skipped. Code below makes sure that we close out the lines
!   before starting to read another control file. 
         if(.not.kfirst_skip) write(*,*) " "   !close out 'skipping non-sked' line  
         kfirst_skip=.true. 
! write a warning message but try to read the local file. 
        if(j .eq. 2 .and. .not.kfound_global_file) then
         write(luscn,
     > '("WARNING! sked_rdctl: Did not find global skedf.ctl file:",a)')
     >       trim(cskedf(1))
        end if

        itmplen = trimlen(cskedf(j))
        kexist = .false.
        inquire(file=cskedf(j),exist=kexist)      
        if(.not.kexist) goto 500                !quick exit. 

        open(lu,file=cskedf(j),iostat=ierr,status='old')
        if (ierr.ne.0) then
          write(luscn,9100) cskedf(j)(1:itmplen)
9100      format("sked_rdctl: ERROR: Error opening control file ",A)
          close(lu)
          return
        end if 
 
        if(j .le. 2) then
          write(luscn,'("sked_rdctl: Reading system control file ",A)')
     >       cskedf(j)(1:itmplen)
             kfound_global_file=.true. 
        else
          write(luscn,'("sked_rdctl: Reading local control file ",A)')
     >       cskedf(j)(1:itmplen)
        endif

! File exists, and we have opened it.    
        call readline_skdrut(lu,cbuf,keof,ierr,1) !read first $
        do while (.not.keof)
          read(cbuf,'(a)') lsecname
          call capitalize(lsecname)
          call readline_skdrut(lu,cbuf,keof,ierr,2)  !space to next valid line.

C  $CATALOGS
          if (lsecname .eq. "$CATALOGS") then
            do while(.not.keof .and.(cbuf(1:1) .ne. "$"))
              call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
              call capitalize(lkeyword)
! Special case--setting default catalog.
              if(lkeyword .eq. 'CAT_DIR') then
                cat_dir=lvalue
                nch_cat_dir=trimlen(cat_dir)
                if(cat_dir(nch_cat_dir:nch_cat_dir) .ne. "/") then
                   nch_cat_dir=nch_cat_dir+1
                   cat_dir(nch_cat_dir:nch_cat_dir)="/"
                endif
                goto 9200
              endif
! It is assumed that the rest of the values are catalog paths
              if(lvalue(1:1) .ne. "/") then     ! If we don't start with '/', prepend catalog directory
                  nch=trimlen(lvalue)
                  ctemp=cat_dir(1:nch_cat_dir)//lvalue(1:nch)
                  lvalue=ctemp
              endif

              if (lkeyword.eq.'SOURCE') then
                 source_cat=lvalue
              else if (lkeyword.eq.'STATION') then
                 station_cat=lvalue
              else if (lkeyword.eq.'ANTENNA') then
                 antenna_cat=lvalue
              else if (lkeyword.eq.'POSITION') then
                 position_cat=lvalue
              else if (lkeyword.eq.'EQUIP') then
                 equip_cat=lvalue
              else if (lkeyword.eq.'MASK') then
                 mask_cat=lvalue
              else if (lkeyword.eq.'FREQ') then
                 freq_cat=lvalue
              else if (lkeyword.eq.'RX') then
                 rx_cat=lvalue
              else if (lkeyword.eq.'LOIF') then
                 loif_cat=lvalue
              else if (lkeyword.eq.'MODES') then
                 modes_cat=lvalue
              else if (lkeyword.eq.'MODES_DESCRIPTION') then
                 modes_description_cat=lvalue
              else if (lkeyword.eq.'REC') then
                 rec_cat=lvalue
              else if (lkeyword.eq.'HDPOS') then
                 hdpos_cat=lvalue
              else if (lkeyword.eq.'TRACKS') then
                 tracks_cat=lvalue
              else if (lkeyword.eq.'FLUX') then
                 flux_cat=lvalue
              else if (lkeyword.eq.'COMMENTS') then
                 flux_comments=lvalue
              else if (lkeyword.eq.'PROGRAM') then
                cat_program_path=lvalue
              else if (lkeyword.eq.'PARAMETER') then
                par_program_path=lvalue
              else if(lkeyword .eq. 'MASTER') then
                cmaster_dir=lvalue                                
              else
                write(luscn,'("sked_rdctl: Unknown catalog ", A)') lkeyword
              end if
9200          continue
              call readline_skdrut(lu,cbuf,keof,ierr,2)
            end do
! $MYSQL
          else if(lsecname .eq. "$MYSQL") then
            do while(.not.keof .and.(cbuf(1:1) .ne. "$"))
              call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
              call capitalize(lkeyword)
              select case(lkeyword)
              case('HOST')
                 lmysql_host=trim(lvalue)//char(0)
              case('USER')
                 lmysql_user=trim(lvalue)//char(0)
              case('PASSWORD')
                 lmysql_password=trim(lvalue)//char(0)
              case('DATABASE')
                 lmysql_db=trim(lvalue)//char(0)
              case('PORT') 
                 read(lvalue, *) iport_mysql
              case('FLAG')
                 read(lvalue,*) iclient_flag
              case default
                 write(luscn,9201) lkeyword
9201             format("sked_rdctl: ERROR: Unknown mysql keyword: ",A)
              end select
              call readline_skdrut(lu,cbuf,keof,ierr,2)
            end do
C  $SCHEDULES
          else if (lsecname .eq.'$SCHEDULES') then
              if ((cbuf(1:1) .ne. '$').and..not.keof) then
                read(cbuf,'(a)') csked
                call add_slash_if_needed(csked)
                call readline_skdrut(lu,cbuf,keof,ierr,1)
              end if

C  $SCRATCH
            else if (lsecname .eq. '$SCRATCH') then
              if ((cbuf(1:1) .ne. '$').and..not.keof) then
                read(cbuf,'(a)') cscratch
                call add_slash_if_needed(cscratch)
                call readline_skdrut(lu,cbuf,keof,ierr,1)
              end if
C  $PRINT
            else if (lsecname .eq.'$PRINT') then
              do while(.not.keof.and.(cbuf(1:1) .ne. '$'))
                call splitNtokens(cbuf,ltoken,Maxtoken,NumToken)
                call capitalize(lkeyword)

                ktoken=.false.
                if(lvalue .ne. " ") then
                  nch=trimlen(lvalue)
                  ind=index(cbuf,lvalue(1:nch))
                  nch=trimlen(cbuf)
                  ktoken=.true.
                endif
               
                if (lkeyword .eq.'PORTRAIT') then
                  if(ktoken) then
                    cprtpor=cbuf(ind:nch)
                    call null_term(cprtpor)
                  else ! null
                    cprtpor=' '
                  endif
                else if (lkeyword .eq.'LANDSCAPE') then
                  if(ktoken) then
                    cprtlan=cbuf(ind:nch)
                    call null_term(cprtlan)
                  else ! null
                    cprtlan=' '
                  endif
                else if (lkeyword .eq. 'PRINTER') then ! printer line
                  call capitalize(lvalue)
                  if (lvalue.eq.'EPSON'.or.lvalue.eq.'LASER'.or.
     >                lvalue.eq.'EPSON24') then
                      cprttyp=lvalue
                  else
                     write(luscn,9211) lvalue(1:trimlen(lvalue))
9211              format('sked_rdctl: ERROR: Unknown printer type ',A)
                  endif               
                else
                   continue  
                endif
9216            continue
                call readline_skdrut(lu,cbuf,keof,ierr,2)
              end do
            else ! unrecognized
! Unrecognized sectiojn. Probably belonging to drudg. 
              if(kfirst_skip) then
                 write(luscn,'("Skipping non-sked section: ",$)') 
                 kfirst_skip=.false.
               endif
               write(luscn,'(a," ", $)') lsecname 
              do while(.not.keof.and.(cbuf(1:1) .ne. '$'))
                call readline_skdrut(lu,cbuf,keof,ierr,1)
              end do 
            end if
        end do
        close (lu)   
500     continue           !quick exit. 
      end do  !"do 1,2"
      if(.not.kfirst_skip) write(*,*) " "   !close out 'skipping non-sked' line 

! save original state 
 

      RETURN
      END
