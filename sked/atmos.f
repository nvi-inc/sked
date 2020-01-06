      subroutine atmos(sin_el,cos_el,atm_part)
CHS----------------------------------------------------
CHS Atmos was created in order to compute the coefficients
CHS of the tropospheric "Mapping-function".
C
CHS++++++++++++++++++++++++++++++++++++++++++++++++++++++
! 2005May25 JMGipson.  Make it single station.
      double precision sin_el      !sine of atmosphere
      double precision cos_el
! returned
      double precision atm_part           !delay partial
      double precision a,b
      a=0.00143d0
      b=0.0445d0
      atm_part=1.d0/(sin_el+(a/(sin_el/cos_el+b)))
      return
      end
