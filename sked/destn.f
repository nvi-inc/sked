C@DESTN
      SUBROUTINE DESTN(NSTN,ISTN)
C
C    DESTN deletes a bad station from the subnet list
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT/OUTPUT:  NOTE*** These variables are modified!
C
      integer ISTN(MAX_STN),nstn
C     - indices of subnet stations
C     NSTN - number of stations in ISTN
C
C  LOCAL:
C     NSTNNE - number of new stations
      integer ISTNNE(MAX_STN),nstnne,i
C     - indices of new list
C
C  HISTORY
C  NRV 890428 Created, called by NEWOB
C
C
C  1. Find the non-negative stations and save in NE array
C
      NSTNNE=0
      DO I=1,NSTN
        IF (ISTN(I).GT.0) THEN !keep this one
          NSTNNE=NSTNNE+1
          ISTNNE(NSTNNE)=ISTN(I)
        ENDIF
      ENDDO
C
C 2. Replace inputs with new values
C
      DO I=1,NSTNNE
        ISTN(I)=ISTNNE(I)
      ENDDO
      NSTN=NSTNNE
C
      RETURN
      END
