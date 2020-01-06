      subroutine opcmd(linstr)
C
C  PARAM is the top level routine for parameter setting
C
C   History
C   NRV 910910 Created
C   NRV 910912 Add "GO" option for non-interactive use.
C   nrv 930602 Add "LI" option for documenting.
C 951017 nrv Change igtky call to use lkey

C   Common/include:
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/sourc.ftni'
      include 'covar.ftni'
! functions
      integer istringminmatch

C Input:
      integer*2 linstr(*)
C
C   Called by :SKED
C   Calls: SEOP

C Local:
      integer i,ikey,ierr
      character*2 cfunc,ckey
C
      integer*2 lkeywd(6)
      character*12 ckeywd
      equivalence (lkeywd,ckeywd)
      integer ilist_len
      parameter (ilist_len=3)
      character*6 list(ilist_len),listshort(ilist_len)
      data list/ "GO","LIST","SET"/
      data listshort/"GO","LI","SE"/

C  0. check for enough info first

      if (nstatn.le.0) then
        write(luscn,'(" OPCMD01 - Select stations first.")')
        return
      endif
      if (nsourc.le.0) then
        write(luscn,'(" OPCMD02 - Select sources first.")')
        return
      endif

      if (linstr(1).le.0) then
        ikey = -1
      else
        do i=1,(linstr(1)+1)/2
          lkeywd(i)=linstr(i+1)
        end do
        ckeywd(linstr(1)+1:)=" "
        ikey = istringMinMatch(list,ilist_len,ckeywd)
      endif
      if (ikey.le.0) then !invalid
        write(luscn,'("OPCMD04 - Choose GO, LIST, or SET.")')
        return
      endif
      ckey=listshort(ikey)

      if (ckey.eq.'GO') then !non-interactive
        call opfill       
        return

      else if (ckey.eq.'LI') then !list
        call opout(ludsp,'d')        !display 
        return

      else if (ckey.eq.'SE') then !interactive      
        call seop('PA',ierr)
        if (ierr .ne. 0) return
        do while (.true.)
C Check number of parameters
          call op_refresh
          write(luscn,9243) num_est,num_opt
9243      format(/' Number of parameters to estimate: ',i3/
     .          ' Number of parameters to optimize: ',i3)
          if (num_est.gt.max_par_esti) write(luscn,9241) max_par_esti
9241      format(/' WARNING: A maximum of ',i3,' parameters can be ',
     .    'estimated!')
          if (num_opt.gt.max_par_opti) write(luscn,9242) max_par_opti
9242      format(/' WARNING: A maximum of ',i3,' parameters can be ',
     .    'optimized!')
          WRITE(LUSCN,9200)
9200      FORMAT(/'  PA - display station parameters for selection'/
     .    '  SO - display source parameters for selection'/
     .    '  GO - return to SKED, create new normal equations'/
     .    '  EN - return to SKED, do not create new normal equations')
          WRITE(LUSCN,'("> ",$)')
          READ(LUUSR,'(a)') cfunc
          call capitalize(cfunc)
          IF (cfunc.EQ.'PA'.or.cfunc.eq.'SO') then
             call seop(cfunc,ierr)
          ELSE IF (cfunc.EQ.'GO') THEN
            call opfill     
            return
          ELSE IF (cfunc.EQ.'EN') THEN
            return
          END IF
        end do
      else
        write(luscn,'(" OPCMD04 - The only option is GO")')
      endif
      end

