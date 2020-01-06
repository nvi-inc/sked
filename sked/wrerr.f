C@WRERR
      SUBROUTINE WRERR(IMSG,IERR)  !WRITE ERROR MESSAGES
C
      include '../skdrincl/skparm.ftni'
C
C  INPUT:
      integer imsg,ierr
C     IMSG - MESSAGE NUMBER TO BE PRINTED
C     IERR - ERROR NUMBER TO BE PRINTED FOLLOWING MESSAGE
C
      include 'skcom.ftni'
C
C  LOCAL VARIABLES:
      integer ii
C
C  MODIFICATIONS
C  880314  NRV  DE-COMPC'D
C
C
C     1. WRITE OUT IMSG AND APPEND ASCII CONVERSION OF IERR.
C
      II = IMSG
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     .21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     .41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     .61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
     .81,82,83,84,85,86,87,88,89,90,91
     .)II
C
1     WRITE(LUSCN,901) IERR
901    FORMAT(' SKED01 - ERROR ',I5,' (?).  Please',
     .' note circumstances in detail and notify NRV.')
      GOTO 999
C
2     WRITE(LUSCN,902)
902    FORMAT(' SKED02 - Unrecognized command.  Type ?? for help.')
      GOTO 999
C
3     WRITE(LUSCN,903)
903    FORMAT(' SKED03 - Ambiguous command.')
      GOTO 999
C
4     WRITE(LUSCN,904)
904    FORMAT(' GTDTR01 - Command requires date/time range.')
      GOTO 999
C
5     WRITE(LUSCN,905)
905    FORMAT(' GTDTR02 - Start date/time must be of form YYDDDHHMMSS.',
     .' YY and DDD optional.')
      GOTO 999
C
6     WRITE(LUSCN,906)
906    FORMAT(' GTDTR03 - Number of observations must be numeric.')
      GOTO 999
C
7     WRITE(LUSCN,907)
907    FORMAT(' GTDTR04 - Improperly specified stopping date/time.')
      GOTO 999
C
8     WRITE(LUSCN,908)
908    FORMAT(' SELECT - Can''t RP SOCAT, STCAT, or FRCAT.')
      GOTO 999
C
9     WRITE(LUSCN,909)
909    FORMAT(' SELECT - Source/station/frequency selection only supporte
     .d at 264x terminals.')
      GOTO 999
C
10    WRITE(LUSCN,9010)
9010   FORMAT(' No sources selected.')
      GOTO 999
C
11    WRITE(LUSCN,9011) IERR
9011   FORMAT(' UNIT01 - Unit must be numeric.  Current unit ',I5)
      GOTO 999
C
12    WRITE(LUSCN,9012) IERR
9012   FORMAT(' UNIT02 - Invalid display unit. Can''t write to ',I5)
      GOTO 999
C
13    WRITE(LUSCN,9013)
9013   FORMAT(' SKED03 - Select sources and stations first.')
      GOTO 999
C
14    WRITE(LUSCN,9014)
9014   FORMAT(' SO/ST/FRCMD01 - Function must be LIST or SELECT')
      GOTO 999
C
15    WRITE(LUSCN,9015)
9015   FORMAT(' SSCAN - Invalid duration.')
      GOTO 999
C
16    WRITE(LUSCN,9016)
9016   FORMAT(' SELECT - Incomplete station information.',
     .'  No stations selected.')
      GOTO 999
C
17    WRITE(LUSCN,9017)
9017   FORMAT(' XLCMD00 - Function must be ON or OFF.')
      GOTO 999
C
18    WRITE(LUSCN,9018) IERR
9018   FORMAT(' FRINP02 - Error in field number ',I5,' in code entry')
      GOTO 999
C
19    WRITE(LUSCN,9019) IERR
9019   FORMAT(' FRINP03 - Error in field number ',I5,' in LO entry')
      GOTO 999
C
20    WRITE(LUSCN,9020)
9020   FORMAT(' LICMD05 - Invalid source name.')
      GOTO 999
C
21    WRITE(LUSCN,9021)
9021   FORMAT(' LICMD01 - Invalid station name.')
      GOTO 999
C
22    WRITE(LUSCN,9022)
9022   FORMAT(' LICMD02 - End of listing.')
      GOTO 999
C
23    WRITE(LUSCN,9023)
9023   FORMAT(' GTOBS00 - Select sources, stations and frequencies first.
     .')
      GOTO 999
C
24    WRITE(LUSCN,9024) IERR
9024   FORMAT(' GTOBS01 - Error ',I5,' getting an observation.')
      GOTO 999
C
25    WRITE(LUSCN,9025) IERR
9025   FORMAT(' GTOBS02 - Error ',I3,' reading record.')
      GOTO 999
C
26    WRITE(LUSCN,9026)
9026   FORMAT(' GTOBS03 - Error unpacking.')
      GOTO 999
C
27    WRITE(LUSCN,9027)
9027   FORMAT(' DELCM01 - No observations in range.')
      GOTO 999
C
28    WRITE(LUSCN,9028) IERR
9028   FORMAT(' DELCM02 - Error ',I3,' backing up with POSNT')
      GOTO 999
C
29    GOTO 999
C
30    WRITE(LUSCN,9030) IERR
9030   FORMAT(' PTOBS01 - No room for additional observations.'/
     .       ' Maximum allowed: ',I4)
      GOTO 999
C
31    WRITE(LUSCN,9031) IERR
9031   FORMAT(' PTOBS11 - Error ',I3,' creating scratch file.')
      GOTO 999
C
32    WRITE(LUSCN,9032) IERR
9032   FORMAT(' SKOPN05 - Only ',I4,' observations have been read in.',
     .      /' That is the maximum SKED can currently handle')
      GOTO 999
C
33    WRITE(LUSCN,9033) IERR
9033   FORMAT(' PTOBS31 - Error ',I3,' in APOSN after copy.')
      GOTO 999
C
34    WRITE(LUSCN,9034)
9034   FORMAT(' PTOBS41 - Error in PAKUP.')
      GOTO 999
C
35    WRITE(LUSCN,9035) IERR
9035   FORMAT(' PTOBS51 - Error ',I3,' writing new record.')
      GOTO 999
C
36    WRITE(LUSCN,9036) IERR
9036   FORMAT(' STINP - Error in field number ',I5,' in terminal entry')
      GOTO 999
C
37    WRITE(LUSCN,9037)
9037   FORMAT(' STINP - Terminal entry found before matching antenna ',
     .'entry.')
      GOTO 999
C
38    WRITE(LUSCN,9038) IERR
9038   FORMAT(' PTOBS81 - Error ',I3,' in APOSN after writing EOF.')
      GOTO 999
C
39    WRITE(LUSCN,9039) IERR
9039   FORMAT(' PTOBS91 - Error ',i3,' purging scratch file.')
      GOTO 999
C
40    WRITE(LUSCN,9040)
9040   FORMAT(' SKOPN01 - Invalid file name from NAMR')
      GOTO 999
C
41    WRITE(LUSCN,9041) IERR
9041   FORMAT(' SKOPN02 - Error ',i3,' opening schedule file.')
      GOTO 999
C
42    WRITE(LUSCN,9042) IERR
9042   FORMAT(' SKOPN03 - Error ',i3,' reading schedule file.')
      GOTO 999
C
43    WRITE(LUSCN,9043) IERR
9043   FORMAT(' SKOPN04 - Error ',i3,' writing working file.')
      GOTO 999
C
44    WRITE(LUSCN,9044) IERR
9044   FORMAT(' SOINP01 - Internal error, maximum number of sources ',
     .       'incorrectly defined as ',I5,'.')
      GOTO 999
C
45    WRITE(LUSCN,9045)  IERR
9045   FORMAT(' SOINP02 - Error in field number ',i5,'.')
      GOTO 999
C
46    WRITE(LUSCN,9046)
9046   FORMAT(' SOINP03 - Only epoch 1950 or J2000 can be used ',
     .       'at this time.')
      GOTO 999
C
47    WRITE(LUSCN,9047) IERR
9047   FORMAT(' STINP01 - Too many stations selected.  Max is ',i5,'.')
      GOTO 999
C
48    WRITE(LUSCN,9048) IERR
9048   FORMAT(' STINP02 - Error in field number ',i5,
     . ' in antenna entry')
      GOTO 999
C
49    WRITE(LUSCN,9049) IERR
9049   FORMAT(' STINP03 - Error in field number ',i5,
     . ' in position entry')
      GOTO 999
C
50    WRITE(LUSCN,9050)
9050   FORMAT(' STINP04 - Error in converting the latitude.')
      GOTO 999
C
51    WRITE(LUSCN,9051) IERR
9051   FORMAT(' STINP05 - Error in converting slew rate field #',i1)
      GOTO 999
C
52    WRITE(LUSCN,9052) IERR
9052   FORMAT(' STINP06 - Error in converting limit stop field #',i1)
      GOTO 999
C
53    WRITE(LUSCN,9053) IERR
9053   FORMAT(' SEGRP02 - Error ',i3,' creating working file')
      GOTO 999
C
54    WRITE(LUSCN,9054) IERR
9054   FORMAT(' LICMDXX - Too many stations for listing.  Maximum ',i3)
      GOTO 999
C
55    WRITE(LUSCN,9055) IERR
9055   FORMAT(' SEGRP01 - Error ',i3,' creating select file.')
      GOTO 999
C
56    WRITE(LUSCN,9056) IERR
9056   FORMAT(' SOSEL02 - Error ',i3,' opening select file.')
      GOTO 999
C
57    WRITE(LUSCN,9057)
9057   FORMAT('NEWOB - Keyword invalid for new observation.')
      GOTO 999
C
58    WRITE(LUSCN,9058) IERR
9058   FORMAT(' FRINP01 - Too many codes.  Max is ',i5)
      GOTO 999
C
59    WRITE(LUSCN,9059)
9059   FORMAT(' FRINP04 - Inconsistency in frequency code number ',i5)
      GOTO 999
C
60    WRITE(LUSCN,9060) IERR
9060   FORMAT(' FRINP05 - Error in field ',i5,' in code-name entry')
      GOTO 999
C
61    WRITE(LUSCN,9061)
9061   FORMAT(' IGTKY01 - Ambiguous key word.')
      GOTO 999
C
62    WRITE(LUSCN,9062)
9062   FORMAT(' PRSET - Key word invalid for parameter setting.')
      GOTO 999
C
63    WRITE(LUSCN,9063)
9063   FORMAT(' IGTKY - Bad value for CALIBRATION or DURATION.')
      GOTO 999
C
64    WRITE(LUSCN,9064)
9064   FORMAT(' IGTKY - Invalid cable specification.  Must be ',
     .'<station ID><C,W,->')
      GOTO 999
C
65    WRITE(LUSCN,9065)
9065   FORMAT(' IGTKY - Frequency code not selected.')
      GOTO 999
C
66    WRITE(LUSCN,9066)
9066   FORMAT(' IGTKY - Default frequency code not selected!')
      GOTO 999
C
67    WRITE(LUSCN,9067)
9067   FORMAT(' PRSET - Invalid number for parameter CALIBRATION, ',
     .'DURATION, LOOKAHEAD, MINIMUM, MODULAR,or CHANGE.')
      GOTO 999
C
68    WRITE(LUSCN,9068)
9068   FORMAT(' SSCAN - No matching time for source.')
      GOTO 999
C
69    WRITE(LUSCN,9069)
9069   FORMAT(' STINP - Position entry found before matching antenna ',
     .'entry.')
      GOTO 999
C
70    WRITE(LUSCN,9070)
9070   FORMAT(' Invalid number for parameter ELEVATION')
      GOTO 999
C
71    WRITE(LUSCN,9071)
9071   FORMAT(' SYNCHRONIZE parameter must be ON or OFF')
      GOTO 999
C
72    WRITE(LUSCN,9072)
9072   FORMAT(' No matching elevation for station.')
      GOTO 999
C
73    WRITE(LUSCN,9073)
9073   FORMAT(' Invalid number for elevation.')
      GOTO 999
C
74    WRITE(LUSCN,9074) IERR
9074   FORMAT(' SOINP04 - Invalid number of celestial sources. ',
     .       'Max is ',I5,'.')
      GOTO 999
C
75    WRITE(LUSCN,9075) IERR
9075   FORMAT(' SOINP05 - Invalid number of satellite sources. ',
     .       'Max is ',I5,'.')
      GOTO 999
C
76    WRITE(LUSCN,9076)
9076   FORMAT(' PRCMD01 - Error: No parameters specified')
      GOTO 999
C
77    WRITE(LUSCN,9077)
9077   FORMAT(' PRNCM01 - Error: No parameters specified')
      GOTO 999
C
78    WRITE(LUSCN,9078)
9078   FORMAT(' PRNCM02 - Error: Printer not found')
      GOTO 999
C
79    WRITE(LUSCN,9079)
9079   FORMAT(' PRNCM03 - Error: Print file still active; Use UN screen 
     .to deactivate')
      GOTO 999
C
80    WRITE(LUSCN,9080)
9080   FORMAT(' PRNCM04 - Error: No print file has been created; Save out
     .put with UN print')
      GOTO 999
C
81    WRITE(LUSCN,9081)
9081   FORMAT(' PRNCM05 - Error closing temporary printing file')
      GOTO 999
C
82    WRITE(LUSCN,9082)
9082   FORMAT(' LUCMD01 - Error: Printing has not been turned on')
      GOTO 999
C
83    WRITE(LUSCN,9083)
9083   FORMAT(' LUCMD02 - Error: Printing is already active')
      GOTO 999
C
84    WRITE(LUSCN,9084)
9084   FORMAT(' LUCMD03 - Error opening temporary print file')
      GOTO 999
C
85    WRITE(LUSCN,9085)
9085   FORMAT(' LUCMD04 - File exists')
      GOTO 999
C
86    WRITE(LUSCN,9086)
9086   FORMAT(' LUCMD05 - Error opening file')
      GOTO 999
C
87    WRITE(LUSCN,9087)
9087   FORMAT(' LUCMD06 - Error: No parameters specified')
      GOTO 999
C
88    WRITE(LUSCN,9088)
9088   FORMAT(' PRNCM06 - Error calling printer routine')
      GOTO 999
C
89    WRITE(LUSCN,9089)
9089   FORMAT(' LUCMD07 - Error: Output already being captured to a file
     .')
      GOTO 999
C
90    write(luscn,9090)
9090   format(' SKW01 - No windows available.')
      goto 999
C
91    write(luscn,9091)
9091   format(' SKW02 - XWindows is not running!')
      goto 999
C
999   RETURN
      END

