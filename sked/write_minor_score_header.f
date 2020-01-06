       subroutine write_minor_score_header(lu)
       implicit none 
       include 'minor.ftni'
       integer lu

9110   format(a8," ",$)                   !this is for the header

       write(lu,'(4x,a15,4x,a3,$)') 'Reason/TimeSpan','i  '
       if(kastro)      write(lu,9110) "Astro   "
       if(kBegScan)    write(lu,9110) "BegScan "
       if(kCovar)      write(lu,9110) "Covar   " 
       if(kDurScan)    write(lu,9110) "DurScan "
       if(kEndScan)    write(lu,9110) "EndScan "
       if(kLowDec)     write(lu,9110) "LowDec  "
       if(kNumLoEl)    write(lu,9110) "NumLoEl "
       if(KNumObs)     write(lu,9110) "NumObs  "
       if(kNumRiseSet) write(lu,9110) "NRiseSet"
       if(kSkyCov)     write(lu,9110) "SkyCov  "
       if(KSrcEvn .and. iSrcEvnMode.gt.0)  write(lu,9110) "SrcEvn  "
       if(kStatEvn .and.istatEvnMode.gt.0) write(lu,9110) "StatEvn "
       if(kStatIdle)   write(lu,9110) "StatIdle"
       if(kStatWt)     write(lu,9110) "StatWt  "
       if(kTimeVar)    write(lu,9110) "TimeVar "
       write(lu,*) "Score  "
       return
       end

