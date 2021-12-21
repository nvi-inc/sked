      SUBROUTINE MXLIS
C
C MXLIS lists the values of the parameters that define the
C array sizes.
C
      include '../skdrincl/skparm.ftni'
      include 'skcom.ftni'
      include 'cat_stat.ftni'
      include 'cat_mode.ftni'
      include 'cat_src.ftni'
C
C  LOCAL VARIABLES
C
C  History
C 960627 nrv New.
C 970409 nrv Add subpasses.
C
C     1. We simply write out the parameter name keyword, followed by
C        the value of the parameter, and finally a brief description
C        of the parameter.

      write(ludsp,9100) 
9100  format('Maximum array sizes currently set in sked and drudg')
      write(ludsp,9121) max_sor,max_cel,max_sat,max_sorlen,
     .max_cat_src,max_stn,max_hor,max_cat_stat,max_code,
     .max_subpass,max_cat_mode,max_band,max_obs,
     .max_par_opti,max_sor_esti,max_trial
9121  format(
     .' Maximum number of sources   ',i5,
     .'     (',i5,' celestial, ',i5,' satellite)',
     ./' Maximum source name length ',i5,' characters',
     ./' Maximum number of source names in catalog ',i5,
     ./' Maximum number of stations  ',i5,
     ./' Maximum number of horizon mask pairs  ',i5,
     ./' Maximum number of station names in catalog ',i5,
     ./' Maximum number of observing modes  ',i5,
     ./' Maximum number of subpasses per head position  ',i5,
     ./' Maximum number of observing mode names in catalog ',i5,
     ./' Maximum number of bands (e.g. X, S) ',i5,
     ./' Maximum number of observations ',i5,
     ./' Maximum number of parameters that can be optimized  ',i5,
     ./' Maximum number of sources positions that can be optimized  ',
     .i5,
     ./' Maximum number of configurations considered for',
     .' optimization ',i5)
      RETURN
      END
