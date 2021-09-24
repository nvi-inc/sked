      block data
      include 'cmdcmn.ftni'
! AEM 20041130 remove COMMON from here to place in cmdcmn.ftni
!      common /cmdlist/cmdshort,cmdlist,cmdbrief,cmdsyntax
! 2008Jun02  Allocation and autoshift brief descriptions were swapped! Fixed.
! 2009Apr30  Changed a command
! 2010Jan04  Added "media" command.
! 2010Mar20  Removed BASLINE command. 
! 2011Aug12 Added Now
! 2017Feb23 Some strings were too long. Modified so that they would fit.
! 2018Jan02 Added twin_telescopes
! 2018Oct05 Added functionalities to STATIONS command

! Note: We need to break this up because of limits of continue lines.
      data (cmdshort(i),cmdlist(i),cmdbrief(i),cmdsyntax(i),
     > i=1,MxCmd1)/
!CMD:    1 ----------
     >"! ",
     >"! ",
     >"Shell to system          ",
     >"Temporarily shell out of SKED.  Use D or 'exit' to return  ",
!CMD:    2 ----------
     >"/ ",
     >"/ ",
     >"Insert new scan        ",
     >"Src [START <time> SUBNET <subnet> DUR <time>]             ",
!CMD:    3 ----------
     >"??",
     >"? ",
     >"Info for <command>     ",
     >"?  <command>                                    ",
!CMD:    4 ----------
     >"^ ",
     >"^                 ",
     >"Previous line          ",
     >"^ number                                                       ",
!CMD:    5 ----------
     >"AB",
     >"ABORT             ",
     >"Abandon all changes    ",
     >"Command takes no parameters                                    ",
!CMD:    6 ----------
     >"AD",
     >"ADD               ",
     >"Add station to scan    ",
     >"<range> <station>                                          ",
!CMD:    7 ----------
     >"AL",
     >"ALLOCATION        ",
     >"Set, list tape allocation    ",
     >"[AUTO | SCHEDULED]                                  ",
!CMD:    8 ----------
     >"AS",
     >"ASTROMETRIC      ",
     >"Set desired #obs limits",  
     >"[List|Obs|Add Src Min% [Max%]|Set Source Min% [Max%]| Delete Sou
     >rce | Cull MinObs [MinRatio]]",
!CMD:    9 ----------
     >"AU",
     >"AUTOSKED         ",
     >"Auto generate schedule",
     >"<subnet>  EndTime                                 ",
!CMD:   10 ----------
     >"BK",
     >"BACK              ",
     >"Back up in the schedule",
     >"[<number>]                                                ",

!CMD:  11
     >"BS",
     >"BESTSOURCE        ",
     >"Find best sources",
     >"#N(sources) [#M(mode) [#C(cover)]]                  ",
!CMD:  12
     >"BB",
     >"BROADBAND        ",
     >"Set broadband stations",   
     >" LIST | ADD <stat> <BW> <data> <sink> | SET <stat> <BW> <data> 
     > <sink> | DELETE <stat> <data> <sink>",
!CMD:   13 ----------
     >"CA",
     >"CATALOG           ",
     >"List, Set catalogs  ",
     >"| ? | LIST | CAT_NAME [<cat_name>|<cat_ver> <cat_name>]|  ",
!CMD:   14 ----------
     >"CH",
     >"CHECK             ",
     >"Check schedule         ",
     >"[<range> [IDLE <value>]]                                 ",
!CMD:   15 ----------
     >"CO",
     >"COMMENT           ",
     >"only used in scripts   ",
     >"Command takes no parameters                                    ",
! CMD: 16
     >"  ",
     >"COVERAGE          ",
     >"Report coverage by station",
     >"[<range> ]",   
!CMD:   17 ----------
     >"CU",
     >"CURRENT           ",
     >"List current scan      ",
     >"Command takes no parameters                                    ",
!CMD:   18 ----------
     >"DE",
     >"DELETE            ",
     >"Delete scan(s)          ",
     >"<range>                                                 ",
!CMD:  19  --------
     >"",
     >"DOWNTIME          ",
     >"Set/list station Downtime",
     >"[? | subnet [<start_time end_time>|<OFF>]          ",
!CMD: 20  -----------
     >"EX",
     >"EXPER          ",
     >"List/Change experiment code",
     >"[exper]",
CMD: 21  -----------
     >"FI",
     >"FILL          ",
     >"Fill idle time",
     >"[<range> [<source> [<subnet>]]]"/

! second set of commands.
      data (cmdshort(i),cmdlist(i),cmdbrief(i),cmdsyntax(i),
     >  i= MxCmd1+1,MxCmd1+MxCmd2)/
!CMD:  1 ----------
     >"TE",
     >"EARLY             ",
     >"Set, list early start",
     >"[<station> <time> <station> <time> ...]                  ",
!CMD:  2 ----------
     >"EC",
     >"EC                ",
     >"Create sked file & exit",
     >"<filename>                                                  ",
!CMD:   3 ----------
     >"EL",
     >"ELEVATION         ",
     >"Set, list el limits  ",
     >"[<station> <limit> <station> <limit> ...]            ",
!CMD:   4 ----------
     >"ER",
     >"ER                ",
     >"Write sked file & exit ",
     >"[<filename>]                                                ",
!CMD:   5 ----------
     >"FL",
     >"FLUX              ",
     >"Select or list flux         ",
     >"SELECT [<catalog>] | LIST | CHECK | FIX                      ",
!CMD:   6 ----------
     >"FR",
     >"FREQUENCY        ",
     >"Set,list freq. modes        ",
     >"SELECT | LIST [<subnet>]                           ",
!CMD:  7
     >"GR",
     >"GROUP         ",
     >"Handle groups of sources",
     >"[List | Obs | Add Src Group# | Set Src Group# | Delete Src Group#
     >| / Group# OPTIONS]",
!CMD:   8 ----------
     >"??",
     >"HELP              ",
     >"List this screen       ",
     >"Command takes no parameters                                    ",
!CMD:   9 ----------
     >"LI",
     >"LIST              ",
     >"List scans             ",
     >"[<range> [<source> [<subnet> [<ellim>]]]]                 ",
!CMD: 10
     >"MA",
     >"MAJOR            ",
     >"List, select major opts",
     >"[ LIST| Name [ON|OFF]                                    ",
!CMD:  11 ----------
     >"MX",
     >"MAX               ",
     >"List max parameter values   ",
     >"Command takes no parameters                                    ",
!CMD:  12 ----------
     >"MS",
     >"MASTER            ",
     >"Compare schedule, master ",
     >"[CHECK | GET ]                                          ",
!CMD:   13 ----------
!     >"--",
!     >"MAX_STAT_SCAN     ",
!     > "Select, list Max_stat_scan   ",
!     >" LIST | ADD <stat> <max_stat_scan> | SET <stat> <max_stat_scan> ",
!     > //"|" DELETE <stat>", 

!CMD:  13 ----------
     >"--",
     >"MAX_STAT_SCAN     ",
     > "Select, list Max_stat_scan   ",
     > "LISTA | ADD <stat> <#scan> | SET  <stat> <#scan> | DEL <stat>",
!     >"Compare schedule, master ",
!     >"[CHECK | GET ]                           ",           

   
!CMD:   14 ----------
     >"ME",
     >"MEDIA             ",
     >"Set, list media types ",
     >"special syntax                                                 ",
!CMD: 15
     >"MI",
     >"MINOR             ",
     >"List, select minor opts",
     >"[ LIST| Name [ON|OFF] NORM Weight AuxValue1 ...          ",
!CMD:  16 ----------
     >"MO",
     >"MODIFY            ",
     >"Modify current scan    ",
     >"Command takes no parameters                                    ",
!CMD   17
     >"MN",
     >"MONITOR           ",
     >"Get sources to monitor ",
     >"<Number of Sources>",
!CMD:   18 ----------
     >"MT",
     >"MOTION          ",
     >"Set, list tape motion",
     >"[<station> <type> [<gaptime>] <station> <type> ...]     ",
!CMD:  19----------
     >"MU",
     >"MUTUALVIS         ",
     >"Display mutual vis.    ",
     >"[<source> [<subnet> [TOTAL|XYAZEL|POLAZEL]]]        ",
!CMD:   20 ----------
     >"NE",
     >"NEXT              ",
     >"List next scan         ",
     >"[<number>]                                                ",
!CMD:   21 ----------
     >"NW",
     >"NOW              ",
     >"Set current time        ",
     >" [ ? | <subnet> <time> ]                                    ",
!CMD:   22 ----------
     >"OP",
     >"OPTIMIZATION     ",
     >"Set, list optimization      ",
     >"GO | SET | LIST                            ",
!CMD:   23 ----------
     >"PA",
     >"PARAMETERS       ",
     >"Set, list parameters ",
     >"[LIST[SNR|PROCEDURE|GENERAL|ALL]] |<name><value>... "/
!CMD:   22 -----------
 

! 3rd list
      data (cmdshort(i),cmdlist(i),cmdbrief(i),cmdsyntax(i),
     >  i=MxCmd1+MxCmd2+1,MxCmd1+MxCmd2+MxCmd3)/
!CMD:  1 ----------
     >"PD",
     >"PID        ",
     >"List Process ID #      ",
     >"Command takes no arguments.                             ",
!CMD:  2 ----------
     >"PR",
     >"PREVIOUS          ",
     >"List previous scan(s)    ",
     >"[<number>]                                            ",
!CMD:  3 ----------
     >"PT",
     >"PRINTL            ",
     >"Print file - landscape ",
     >"[<file> | PRINT]                                        ",
!CMD:  4 ----------
     >"PP",
     >"PRINTP            ",
     >"Print file - portrait  ",
     >"[<file> | PRINT]                                        ",
!CMD:  5 ----------
     >"QU",
     >"QUIT              ",
     >"Immediately w/o asking ",
     >"Command takes no arguments                                     ",
!CMD:  6 ----------
     >"RA",
     >"RANDOM            ",
     >"Schedule Random sources",
     >"<subnet> [#num scans | stop time]                              ",
!CMD:  7 ----------
     >"RM",
     >"REMOVE            ",
     >"Remove station         ",
     >"<range> <station>                                      ",
!CMD:  8 ----------
     >"RE",
     >"RESULT            ",
     >"Display fe or matrices ",
     >"[FE|COVARIANCE|CORRELATION]                            ",
!CMD:   9 ----------
     >"RW",
     >"REWRITE           ",
     >"Rewrite scans    ",
     >"Command takes no parameters                                    ",
!CMD:   10 ----------
     >"SC",
     >"SCAN              ",
     >"Set source scan times  ",
     >"[<source> <time> <source> <time> ...]                     ",
!CMD:  11  ----------
     >"SH",
     >"SHIFT         ",
     >"Shift start times",
     >"[<range> [TAPE [<station>] | TIME]]                  ",

!CMD:  12----------
     >"SI",
     >"SITEVIS           ",
     >"Display station vis.   ",
     >"[<source> [<subnet> [ LINE|XYAZEL|POLAZEL ]]]         ",
!CMD:   13----------
     >"S1",
     >"1SNR              ",
     >"Set, list 1-BL SNRs ",
     >"[<subnet> <band> <value> | MARGIN <band> <value>]         ",
!CMD:   14 ----------
     >"SN",
     >"SNR               ",
     >"Set, list SNRs       ",
     >"[<subnet> <band> <val> | <MARGIN|AST_MARGIN> <band> <val>] ",
!CMD:   15 ----------
     >"SL",
     >"SOLVE             ",
     >"Make output for solve  ",
     >"[<file>]                                                 ",
!CMD:   16 ----------
     >"SO",
     >"SOURCES           ",
     >"Select,list, plot           ",
     >"SELECT [<catalog>] | LIST | XY_PLOT  POL_PLOT          ",
!CMD:   17 --------
     >" ",
     >"SRCWT            ",
     >"Select, list SrcWt   ",
     >" LIST | ADD <src> <wt> | SET <src> <wt> | DELETE <src>",

!CMD:   18 --------
     >"ST",
     >"STATIONS          ",
     >"Select, list               ",
     >"SELECT | LIST | ADD <stat> | DEL <stat>               ",
!CMD:   19 --------
     >" ",
     >"STATWT            ",
     >"Select, list StatWt   ",
     >" LIST | ADD <stat> <wt> | SET <stat> <wt> | DELETE <stat>",
!CMD:   20 ----------
     >"SS",
     >"STREAMS           ",
     >"Show processing streams",
     >"[EXPAND]                                               ",
!CMD:   22 ----------
     >"SB",
     >"SUBCON            ",
     >"Set subconfig display  ",
     >"[ON | OFF]                                              ",
!CMD:   23 ----------
     >"SU",
     >"SUMMARY           ",
     >"Schedule summary       ",
     >"special syntax                                                 ",
!CMD:   24 ----------
     >"SF",
     >"SUMOUT            ",
     >"Write summary file     ",
     >"[<file>]                                                "/

! 4rd list
      data (cmdshort(i),cmdlist(i),cmdbrief(i),cmdsyntax(i),
     >  i=MxCmd1+MxCmd2+MxCmd3+1,MxCmd1+MxCmd2+MxCmd3+MxCmd4)/
!CMD:   1 ----------
     >"TA",
     >"TAGALONG          ",
     >"Add station to scans   ",
     >"<range> <station> [<subnet>]                         ",
!CMD:   2 ----------
     >"TT",
     >"TAPE              ",
     >"Set, list tape types ",
     >"special syntax                                                 ",
! CMD  3
     >"TH",
     <"THIN ",
     >"Thin by removing obs",
     >"<range> <station> <#obs to remove>",
!CMD:   4 ----------
     >"TI",
     >"TIMELINE          ",
     >"Set time line display  ",
     >"ON | OFF                                              ",
!CMD:   5 --------
     >"TW",
     >"TWIN_TELESCOPES  ",
     >"Select, list, del Twin_Telescopes",
     >" LIST | ADD <stat-stat> [SPLIT|TOGETHER] | DELETE <stat-stat>",
!CMD:   6 ----------
     >"UN",
     >"UNIT              ",
     >"Change output device   ",
     >"[PRINT | SCREEN | SAVEPS | <file> [APPEND|OVERWRITE]] ",
!CMD:   7 ----------
     >"UT",
     >"UNTAG             ",
     >"Remove any bad obs.    ",
     >"[<range>]                                                ",
!CMD:   8 ----------
     >"VS",
     >"VCC               ",
     >"Vex create clean & exit ",
     >"<filename>                                           ",      
!CMD:   9 ---------------    
     >"VC",
     >"VEC               ",
     >"Vex create & exit       ",
     >"<filename>                                                 ",
!CMD:   10 ----------
     >"VE",
     >"VER               ",
     >"Vex write & exit        ",
     >"[<filename>]                                            ",     
!CMD:  11 ----------
     >"VL",
     >"VLBA              ",
     >"Toggle full-obs. mode  ",
     >"[ON | OFF]                                                ",
!CMD:   12 ----------
     >"VS",
     >"VSCAN             ",
     >"Display variable scan lengths",
     >"[<source> [<subnet>]]                                    ",
!CMD:   13 ----------
     >"VW",
     >"VWC               ",
     >"Create Vex file        ",
     >"<filename>                                                 ",
!CMD:   14 ----------
     >"VR",
     >"VWR               ",
     >"Write Vex file         ",
     >"[<filename>]                                               ",
!CMD:   15 ----------
     >"WC",
     >"WC                ",
     >"Create sked file       ",
     >"<filename>                                                  ",
!CMD:   16 ----------
     >"WH",
     >"WHATSUP           ",
     >"Display sources 'up'   ",
     >"[<subnet> [FULL|MIN|NO [<time>]]]                      ",
!CMD:   17 ----------
     >"WR",
     >"WR                ",
     >"Write sked file        ",
     >"[<filename>]                                                ",
!CMD:   18 ----------
     >"WP",
     >"DISPLAY_WRAP      ",
     >"Display wraps      ",
     >"                                               ",

!CMD:   19 ----------
     >"XL",
     >"XLIST             ",
     >"Extended listings      ",
     >"[?|LIST|CLEAR|ON|OFF |FEET|AZEL|WRAP|HA|DUR|SNR|MAX|FLUX|FREQ|
     >  SKY|LONG]",
!CMD:   20 ----------
     >"XN",
     >"XNEW              ",
     >"New scan extended list ",
     >"[ ON | OFF | [SNR|FLUX|BASE|SEFD]]                        "/


! end block
      end
