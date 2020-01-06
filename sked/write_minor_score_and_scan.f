      subroutine write_minor_score_and_Scan(lu,lreason,itst,Score)

      implicit none 
      include '../skdrincl/skparm.ftni'
      include '../skdrincl/constants.ftni'
      include 'skcom.ftni'
      include '../skdrincl/sourc.ftni'
      include '../skdrincl/statn.ftni'
      include '../skdrincl/freqs.ftni'
      include '../skdrincl/skobs.ftni'
      include 'minor.ftni'
      include 'minor_score.ftni'
      include 'covar.ftni'

      integer lu
      character*18 lreason
      integer itst
      integer istn
      double precision score

! local
      integer ihr,imin,isec   !time
      integer itrial, i, ierr,isub,isor 

9111   format(g8.2," ",$)                 !this is for the values

      write(lu,"(1x,a,1x,i4,'  ',$)") lreason,itst
      if(kastro)          write(lu,9111)  TstAstro(itst)
      if(kBegScan)        write(lu,9111)  TstBegScan(itst)
      if(kCovar  )        write(lu,9111)  TstBegScan(itst)
      if(kDurScan)        write(lu,9111)  TstDurScan(itst)
      if(kEndScan)        write(lu,9111)  TstEndScan(itst)
      if(kLowDec)         write(lu,9111)  TstLowDec(itst)
      if(kNumLoel)        write(lu,9111)  TstNumLoel(itst)
      if(KNumObs)         write(lu,9111)  TstNumObs(itst)
      if(kNumRiseSet)     write(lu,9111)  TstNumRiseSet(itst)
      if(kSkyCov)         write(lu,9111)  TstSkyCov(itst)
      if(kSrcEvn .and. iSrcEvnMode.gt.0)
     >                    write(lu,9111)  TstSrcEvn(itst)
      if(kSrcWt)          write(lu,9111)  TstStatWt(itst) 
      if(kStatEvn)        write(lu,9111)  TstStatEvn(itst)
      if(kStatIdle)       write(lu,9111)  TstStatIdle(itst)

      if(kStatWt)         write(lu,9111)  TstStatWt(itst)
      if(kTimeVar)        write(lu,9111)  TstTimeVar(itst)
      write(lu,9111) Score

! unpack the scan and display the results
      isub=1
      itrial=itrial_key(itst)
      cbuf=ctrial_vec(isub,itrial)
      call unpak(ierr,isub)
      istn=isttst(1)
      isor=nsortst(istn)
      call seconds2hms(Uttst(istn),ihr,imin,isec)
      write(ludsp,'(a8,1x,i3,1x,i2.2,":",i2.2,":",i2.2,1x,32a2)')
     > csorna(isor),nint(utobss-utobs), ihr,imin,isec,
     > (cpocod(isttst(i)),i=1,nstntst)
      return
      end

