
/*  A Bison parser, made from vex.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	T_VEX_REV	257
#define	T_REF	258
#define	T_DEF	259
#define	T_ENDDEF	260
#define	T_SCAN	261
#define	T_ENDSCAN	262
#define	T_CHAN_DEF	263
#define	T_SAMPLE_RATE	264
#define	T_BITS_PER_SAMPLE	265
#define	T_SWITCHING_CYCLE	266
#define	T_START	267
#define	T_SOURCE	268
#define	T_MODE	269
#define	T_STATION	270
#define	T_ANTENNA_DIAM	271
#define	T_AXIS_OFFSET	272
#define	T_ANTENNA_MOTION	273
#define	T_POINTING_SECTOR	274
#define	T_AXIS_TYPE	275
#define	T_BBC_ASSIGN	276
#define	T_CLOCK_EARLY	277
#define	T_RECORD_TRANSPORT_TYPE	278
#define	T_ELECTRONICS_RACK_TYPE	279
#define	T_NUMBER_DRIVES	280
#define	T_HEADSTACK	281
#define	T_RECORD_DENSITY	282
#define	T_TAPE_LENGTH	283
#define	T_RECORDING_SYSTEM_ID	284
#define	T_TAPE_MOTION	285
#define	T_TAPE_CONTROL	286
#define	T_TAI_UTC	287
#define	T_A1_TAI	288
#define	T_EOP_REF_EPOCH	289
#define	T_NUM_EOP_POINTS	290
#define	T_EOP_INTERVAL	291
#define	T_UT1_UTC	292
#define	T_X_WOBBLE	293
#define	T_Y_WOBBLE	294
#define	T_EXPER_NUM	295
#define	T_EXPER_NAME	296
#define	T_EXPER_NOMINAL_START	297
#define	T_EXPER_NOMINAL_STOP	298
#define	T_PI_NAME	299
#define	T_PI_EMAIL	300
#define	T_CONTACT_NAME	301
#define	T_CONTACT_EMAIL	302
#define	T_SCHEDULER_NAME	303
#define	T_SCHEDULER_EMAIL	304
#define	T_TARGET_CORRELATOR	305
#define	T_EXPER_DESCRIPTION	306
#define	T_HEADSTACK_POS	307
#define	T_IF_DEF	308
#define	T_PASS_ORDER	309
#define	T_S2_GROUP_ORDER	310
#define	T_PHASE_CAL_DETECT	311
#define	T_TAPE_CHANGE	312
#define	T_NEW_SOURCE_COMMAND	313
#define	T_NEW_TAPE_SETUP	314
#define	T_SETUP_ALWAYS	315
#define	T_PARITY_CHECK	316
#define	T_TAPE_PREPASS	317
#define	T_PREOB_CAL	318
#define	T_MIDOB_CAL	319
#define	T_POSTOB_CAL	320
#define	T_HEADSTACK_MOTION	321
#define	T_PROCEDURE_NAME_PREFIX	322
#define	T_ROLL_REINIT_PERIOD	323
#define	T_ROLL_INC_PERIOD	324
#define	T_ROLL	325
#define	T_ROLL_DEF	326
#define	T_SEFD_MODEL	327
#define	T_SEFD	328
#define	T_SITE_TYPE	329
#define	T_SITE_NAME	330
#define	T_SITE_ID	331
#define	T_SITE_POSITION	332
#define	T_SITE_POSITION_EPOCH	333
#define	T_SITE_POSITION_REF	334
#define	T_SITE_VELOCITY	335
#define	T_HORIZON_MAP_AZ	336
#define	T_HORIZON_MAP_EL	337
#define	T_ZEN_ATMOS	338
#define	T_OCEAN_LOAD_VERT	339
#define	T_OCEAN_LOAD_HORIZ	340
#define	T_OCCUPATION_CODE	341
#define	T_INCLINATION	342
#define	T_ECCENTRICITY	343
#define	T_ARG_PERIGEE	344
#define	T_ASCENDING_NODE	345
#define	T_MEAN_ANOMALY	346
#define	T_SEMI_MAJOR_AXIS	347
#define	T_MEAN_MOTION	348
#define	T_ORBIT_EPOCH	349
#define	T_SOURCE_TYPE	350
#define	T_SOURCE_NAME	351
#define	T_IAU_NAME	352
#define	T_RA	353
#define	T_DEC	354
#define	T_SOURCE_POSITION_REF	355
#define	T_RA_RATE	356
#define	T_DEC_RATE	357
#define	T_SOURCE_POSITION_EPOCH	358
#define	T_REF_COORD_FRAME	359
#define	T_VELOCITY_WRT_LSR	360
#define	T_SOURCE_MODEL	361
#define	T_VSN	362
#define	T_FANIN_DEF	363
#define	T_FANOUT_DEF	364
#define	T_TRACK_FRAME_FORMAT	365
#define	T_DATA_MODULATION	366
#define	T_VLBA_FRMTR_SYS_TRK	367
#define	T_VLBA_TRNSPRT_SYS_TRK	368
#define	T_S2_RECORDING_MODE	369
#define	T_S2_DATA_SOURCE	370
#define	B_GLOBAL	371
#define	B_STATION	372
#define	B_MODE	373
#define	B_SCHED	374
#define	B_EXPER	375
#define	B_SCHEDULING_PARAMS	376
#define	B_PROCEDURES	377
#define	B_EOP	378
#define	B_FREQ	379
#define	B_CLOCK	380
#define	B_ANTENNA	381
#define	B_BBC	382
#define	B_CORR	383
#define	B_DAS	384
#define	B_HEAD_POS	385
#define	B_PASS_ORDER	386
#define	B_PHASE_CAL_DETECT	387
#define	B_ROLL	388
#define	B_IF	389
#define	B_SEFD	390
#define	B_SITE	391
#define	B_SOURCE	392
#define	B_TRACKS	393
#define	B_TAPELOG_OBS	394
#define	T_LITERAL	395
#define	T_NAME	396
#define	T_LINK	397
#define	T_ANGLE	398
#define	T_COMMENT	399
#define	T_COMMENT_TRAILING	400

#line 1 "vex.y"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vex.h"

#define YYDEBUG 1

/* globals */

struct vex *vex_ptr=NULL;
extern int lines;

#line 16 "vex.y"
typedef union
{
int                     ival;
char                   *sval;
struct llist           *llptr;
struct qref            *qrptr;
struct def             *dfptr;
struct block           *blptr;
struct lowl            *lwptr;
struct dvalue          *dvptr;
struct external        *exptr;

struct chan_def        *cdptr;
struct switching_cycle *scptr;

struct station         *snptr;

struct axis_type       *atptr;
struct antenna_motion  *amptr;
struct pointing_sector *psptr;

struct bbc_assign      *baptr;

struct headstack       *hsptr;

struct clock_early     *ceptr;

struct tape_length     *tlptr;
struct tape_motion     *tmptr;

struct headstack_pos   *hpptr;

struct if_def          *ifptr;

struct phase_cal_detect *pdptr;

struct setup_always    *saptr;
struct parity_check    *pcptr;
struct tape_prepass    *tpptr;
struct preob_cal       *prptr;
struct midob_cal       *miptr;
struct postob_cal      *poptr;

struct sefd            *septr;

struct site_position   *spptr;
struct site_velocity   *svptr;
struct ocean_load_vert *ovptr;
struct ocean_load_horiz *ohptr;

struct source_model    *smptr;

struct vsn             *vsptr;

struct fanin_def	*fiptr;
struct fanout_def	*foptr;
struct vlba_frmtr_sys_trk	*fsptr;
struct s2_data_source  *dsptr;

} YYSTYPE;
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		1340
#define	YYFLAG		-32768
#define	YYNTBASE	150

#define YYTRANSLATE(x) ((unsigned)(x) <= 400 ? yytranslate[x] : 418)

static const short yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,   149,   148,     2,
   147,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
    97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
   107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
   117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
   127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
   137,   138,   139,   140,   141,   142,   143,   144,   145,   146
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     3,     5,     8,    10,    12,    14,    16,    21,    24,
    26,    28,    30,    32,    34,    36,    38,    40,    42,    44,
    46,    48,    50,    52,    54,    56,    58,    60,    62,    64,
    66,    68,    70,    72,    76,    79,    83,    86,    89,    91,
    93,    95,    97,   104,   110,   114,   117,   120,   122,   124,
   126,   128,   135,   141,   144,   146,   148,   150,   152,   158,
   160,   162,   164,   166,   168,   170,   172,   174,   176,   178,
   180,   182,   184,   186,   188,   190,   192,   194,   196,   198,
   201,   203,   205,   207,   209,   216,   222,   226,   229,   233,
   236,   239,   241,   243,   245,   247,   254,   260,   263,   265,
   267,   269,   271,   273,   275,   277,   282,   287,   292,   309,
   310,   312,   313,   315,   316,   318,   319,   321,   325,   329,
   332,   335,   337,   339,   341,   343,   350,   356,   359,   361,
   363,   365,   367,   369,   371,   373,   375,   377,   382,   389,
   394,   403,   420,   424,   427,   430,   432,   434,   436,   438,
   445,   451,   454,   456,   458,   460,   462,   464,   473,   477,
   480,   483,   485,   487,   489,   491,   498,   504,   507,   509,
   511,   513,   515,   517,   523,   530,   541,   551,   555,   558,
   561,   563,   565,   567,   569,   576,   582,   585,   587,   589,
   591,   593,   595,   597,   599,   601,   603,   605,   607,   609,
   611,   616,   621,   626,   635,   641,   646,   655,   660,   665,
   672,   683,   688,   692,   695,   698,   700,   702,   704,   706,
   713,   719,   722,   724,   726,   728,   730,   732,   734,   736,
   738,   740,   742,   744,   746,   751,   756,   761,   766,   771,
   776,   780,   785,   789,   794,   798,   802,   805,   808,   810,
   812,   814,   816,   823,   829,   832,   834,   836,   838,   840,
   842,   844,   846,   848,   850,   852,   854,   856,   858,   860,
   862,   864,   869,   874,   879,   884,   889,   894,   899,   904,
   909,   914,   919,   924,   928,   931,   934,   936,   938,   940,
   942,   949,   955,   958,   960,   962,   964,   966,   968,   970,
   972,   974,   991,  1009,  1025,  1042,  1045,  1047,  1050,  1055,
  1060,  1067,  1071,  1074,  1077,  1079,  1081,  1083,  1085,  1092,
  1098,  1101,  1103,  1105,  1107,  1109,  1111,  1118,  1122,  1125,
  1128,  1130,  1132,  1134,  1136,  1143,  1149,  1152,  1154,  1156,
  1158,  1160,  1162,  1179,  1192,  1207,  1221,  1236,  1252,  1256,
  1259,  1262,  1264,  1266,  1268,  1270,  1277,  1283,  1286,  1288,
  1290,  1292,  1294,  1296,  1298,  1303,  1308,  1312,  1315,  1318,
  1320,  1322,  1324,  1326,  1333,  1339,  1342,  1344,  1346,  1348,
  1350,  1352,  1359,  1364,  1368,  1371,  1374,  1376,  1378,  1380,
  1382,  1389,  1395,  1398,  1400,  1402,  1404,  1406,  1408,  1410,
  1412,  1414,  1416,  1418,  1420,  1422,  1424,  1426,  1428,  1433,
  1438,  1443,  1448,  1455,  1462,  1469,  1478,  1487,  1496,  1501,
  1505,  1508,  1511,  1513,  1515,  1517,  1519,  1526,  1532,  1535,
  1537,  1539,  1541,  1543,  1545,  1547,  1549,  1551,  1556,  1561,
  1566,  1571,  1575,  1578,  1581,  1583,  1585,  1587,  1589,  1596,
  1602,  1605,  1607,  1609,  1611,  1613,  1615,  1619,  1622,  1625,
  1627,  1629,  1631,  1633,  1640,  1646,  1649,  1651,  1653,  1655,
  1657,  1659,  1661,  1666,  1675,  1679,  1682,  1685,  1687,  1689,
  1691,  1693,  1700,  1706,  1709,  1711,  1713,  1715,  1717,  1719,
  1721,  1723,  1725,  1727,  1729,  1731,  1733,  1735,  1737,  1739,
  1741,  1743,  1745,  1747,  1749,  1751,  1753,  1755,  1757,  1759,
  1764,  1769,  1774,  1783,  1788,  1793,  1802,  1807,  1812,  1817,
  1824,  1831,  1836,  1841,  1846,  1851,  1856,  1861,  1866,  1871,
  1876,  1880,  1883,  1886,  1888,  1890,  1892,  1894,  1901,  1907,
  1910,  1912,  1914,  1916,  1918,  1920,  1922,  1924,  1926,  1928,
  1930,  1932,  1934,  1936,  1938,  1940,  1942,  1944,  1946,  1948,
  1950,  1952,  1954,  1956,  1958,  1963,  1970,  1975,  1980,  1985,
  1990,  1995,  2000,  2005,  2010,  2015,  2020,  2039,  2043,  2046,
  2049,  2051,  2053,  2055,  2057,  2064,  2070,  2073,  2075,  2077,
  2079,  2081,  2083,  2094,  2098,  2101,  2104,  2106,  2108,  2110,
  2112,  2119,  2125,  2128,  2130,  2132,  2134,  2136,  2138,  2140,
  2142,  2144,  2146,  2148,  2150,  2152,  2163,  2174,  2179,  2184,
  2195,  2204,  2211,  2216,  2225,  2230,  2236,  2240,  2248,  2251,
  2255,  2257,  2261,  2263,  2265,  2267,  2270,  2274,  2276,  2278,
  2282,  2284
};

static const short yyrhs[] = {   151,
   154,     0,   151,     0,   151,   152,     0,   152,     0,   153,
     0,   145,     0,   146,     0,     3,   147,   417,   148,     0,
   154,   155,     0,   155,     0,   156,     0,   157,     0,   161,
     0,   259,     0,   173,     0,   187,     0,   198,     0,   205,
     0,   212,     0,   227,     0,   241,     0,   271,     0,   278,
     0,   285,     0,   293,     0,   300,     0,   317,     0,   327,
     0,   333,     0,   341,     0,   368,     0,   386,     0,   393,
     0,   117,   148,   165,     0,   117,   148,     0,   118,   148,
   158,     0,   118,   148,     0,   158,   159,     0,   159,     0,
   160,     0,   145,     0,   146,     0,     5,   142,   148,   165,
     6,   148,     0,     5,   142,   148,     6,   148,     0,   119,
   148,   162,     0,   119,   148,     0,   162,   163,     0,   163,
     0,   164,     0,   145,     0,   146,     0,     5,   142,   148,
   169,     6,   148,     0,     5,   142,   148,     6,   148,     0,
   165,   166,     0,   166,     0,   167,     0,   145,     0,   146,
     0,     4,   168,   147,   142,   148,     0,   121,     0,   122,
     0,   123,     0,   124,     0,   125,     0,   127,     0,   128,
     0,   126,     0,   129,     0,   130,     0,   131,     0,   132,
     0,   133,     0,   134,     0,   135,     0,   136,     0,   137,
     0,   138,     0,   139,     0,   140,     0,   169,   170,     0,
   170,     0,   171,     0,   145,     0,   146,     0,     4,   168,
   147,   142,   172,   148,     0,     4,   168,   147,   142,   148,
     0,   172,   149,   142,     0,   149,   142,     0,   120,   148,
   174,     0,   120,   148,     0,   174,   175,     0,   175,     0,
   176,     0,   145,     0,   146,     0,     7,   142,   148,   177,
     8,   148,     0,     7,   142,   148,     8,   148,     0,   177,
   178,     0,   178,     0,   179,     0,   180,     0,   181,     0,
   182,     0,   145,     0,   146,     0,    13,   147,   142,   148,
     0,    15,   147,   142,   148,     0,    14,   147,   142,   148,
     0,    16,   147,   142,   149,   413,   149,   413,   149,   183,
   149,   184,   149,   185,   149,   186,   148,     0,     0,   413,
     0,     0,   142,     0,     0,   143,     0,     0,   417,     0,
   417,   149,   417,     0,   127,   148,   188,     0,   127,   148,
     0,   188,   189,     0,   189,     0,   190,     0,   145,     0,
   146,     0,     5,   142,   148,   191,     6,   148,     0,     5,
   142,   148,     6,   148,     0,   191,   192,     0,   192,     0,
   193,     0,   194,     0,   195,     0,   196,     0,   197,     0,
   408,     0,   145,     0,   146,     0,    17,   147,   413,   148,
     0,    21,   147,   142,   149,   142,   148,     0,    18,   147,
   413,   148,     0,    19,   147,   142,   149,   413,   149,   413,
   148,     0,    20,   147,   143,   149,   142,   149,   413,   149,
   413,   149,   142,   149,   413,   149,   413,   148,     0,   128,
   148,   199,     0,   128,   148,     0,   199,   200,     0,   200,
     0,   201,     0,   145,     0,   146,     0,     5,   142,   148,
   202,     6,   148,     0,     5,   142,   148,     6,   148,     0,
   202,   203,     0,   203,     0,   204,     0,   408,     0,   145,
     0,   146,     0,    22,   147,   143,   149,   417,   149,   143,
   148,     0,   126,   148,   206,     0,   126,   148,     0,   206,
   207,     0,   207,     0,   208,     0,   145,     0,   146,     0,
     5,   142,   148,   209,     6,   148,     0,     5,   142,   148,
     6,   148,     0,   209,   210,     0,   210,     0,   211,     0,
   408,     0,   145,     0,   146,     0,    23,   147,   149,   413,
   148,     0,    23,   147,   142,   149,   413,   148,     0,    23,
   147,   142,   149,   413,   149,   142,   149,   417,   148,     0,
    23,   147,   149,   413,   149,   142,   149,   417,   148,     0,
   130,   148,   213,     0,   130,   148,     0,   213,   214,     0,
   214,     0,   215,     0,   145,     0,   146,     0,     5,   142,
   148,   216,     6,   148,     0,     5,   142,   148,     6,   148,
     0,   216,   217,     0,   217,     0,   218,     0,   219,     0,
   220,     0,   221,     0,   222,     0,   223,     0,   224,     0,
   225,     0,   226,     0,   408,     0,   145,     0,   146,     0,
    24,   147,   142,   148,     0,    25,   147,   142,   148,     0,
    26,   147,   417,   148,     0,    27,   147,   417,   149,   142,
   149,   417,   148,     0,    28,   147,   142,   142,   148,     0,
    29,   147,   413,   148,     0,    29,   147,   413,   149,   142,
   149,   417,   148,     0,    30,   147,   417,   148,     0,    31,
   147,   142,   148,     0,    31,   147,   142,   149,   413,   148,
     0,    31,   147,   142,   149,   413,   149,   413,   149,   413,
   148,     0,    32,   147,   142,   148,     0,   124,   148,   228,
     0,   124,   148,     0,   228,   229,     0,   229,     0,   230,
     0,   145,     0,   146,     0,     5,   142,   148,   231,     6,
   148,     0,     5,   142,   148,     6,   148,     0,   231,   232,
     0,   232,     0,   233,     0,   234,     0,   235,     0,   236,
     0,   237,     0,   238,     0,   239,     0,   240,     0,   408,
     0,   145,     0,   146,     0,    33,   147,   413,   148,     0,
    34,   147,   413,   148,     0,    35,   147,   142,   148,     0,
    36,   147,   417,   148,     0,    37,   147,   413,   148,     0,
    38,   147,   410,   148,     0,    38,   147,   148,     0,    39,
   147,   410,   148,     0,    39,   147,   148,     0,    40,   147,
   410,   148,     0,    40,   147,   148,     0,   121,   148,   242,
     0,   121,   148,     0,   242,   243,     0,   243,     0,   244,
     0,   145,     0,   146,     0,     5,   142,   148,   245,     6,
   148,     0,     5,   142,   148,     6,   148,     0,   245,   246,
     0,   246,     0,   247,     0,   248,     0,   249,     0,   250,
     0,   251,     0,   252,     0,   253,     0,   254,     0,   255,
     0,   256,     0,   257,     0,   258,     0,   408,     0,   145,
     0,   146,     0,    41,   147,   417,   148,     0,    42,   147,
   142,   148,     0,    52,   147,   142,   148,     0,    43,   147,
   142,   148,     0,    44,   147,   142,   148,     0,    45,   147,
   142,   148,     0,    46,   147,   142,   148,     0,    47,   147,
   142,   148,     0,    48,   147,   142,   148,     0,    49,   147,
   142,   148,     0,    50,   147,   142,   148,     0,    51,   147,
   142,   148,     0,   125,   148,   260,     0,   125,   148,     0,
   260,   261,     0,   261,     0,   262,     0,   145,     0,   146,
     0,     5,   142,   148,   263,     6,   148,     0,     5,   142,
   148,     6,   148,     0,   263,   264,     0,   264,     0,   265,
     0,   268,     0,   269,     0,   270,     0,   408,     0,   145,
     0,   146,     0,     9,   147,   143,   149,   413,   149,   142,
   149,   413,   149,   143,   149,   143,   149,   143,   148,     0,
     9,   147,   143,   149,   413,   149,   142,   149,   413,   149,
   143,   149,   143,   149,   143,   266,   148,     0,     9,   147,
   149,   413,   149,   142,   149,   413,   149,   143,   149,   143,
   149,   143,   148,     0,     9,   147,   149,   413,   149,   142,
   149,   413,   149,   143,   149,   143,   149,   143,   266,   148,
     0,   266,   267,     0,   267,     0,   149,   417,     0,    10,
   147,   413,   148,     0,    11,   147,   417,   148,     0,    12,
   147,   142,   149,   410,   148,     0,   131,   148,   272,     0,
   131,   148,     0,   272,   273,     0,   273,     0,   274,     0,
   145,     0,   146,     0,     5,   142,   148,   275,     6,   148,
     0,     5,   142,   148,     6,   148,     0,   275,   276,     0,
   276,     0,   277,     0,   408,     0,   145,     0,   146,     0,
    53,   147,   417,   149,   410,   148,     0,   135,   148,   279,
     0,   135,   148,     0,   279,   280,     0,   280,     0,   281,
     0,   145,     0,   146,     0,     5,   142,   148,   282,     6,
   148,     0,     5,   142,   148,     6,   148,     0,   282,   283,
     0,   283,     0,   284,     0,   408,     0,   145,     0,   146,
     0,    54,   147,   143,   149,   142,   149,   142,   149,   413,
   149,   142,   149,   413,   149,   413,   148,     0,    54,   147,
   143,   149,   142,   149,   142,   149,   413,   149,   142,   148,
     0,    54,   147,   143,   149,   142,   149,   142,   149,   413,
   149,   142,   149,   149,   148,     0,    54,   147,   143,   149,
   142,   149,   142,   149,   413,   149,   142,   149,   148,     0,
    54,   147,   143,   149,   142,   149,   142,   149,   413,   149,
   142,   149,   413,   148,     0,    54,   147,   143,   149,   142,
   149,   142,   149,   413,   149,   142,   149,   413,   149,   148,
     0,   132,   148,   286,     0,   132,   148,     0,   286,   287,
     0,   287,     0,   288,     0,   145,     0,   146,     0,     5,
   142,   148,   289,     6,   148,     0,     5,   142,   148,     6,
   148,     0,   289,   290,     0,   290,     0,   291,     0,   292,
     0,   408,     0,   145,     0,   146,     0,    55,   147,   414,
   148,     0,    56,   147,   416,   148,     0,   133,   148,   294,
     0,   133,   148,     0,   294,   295,     0,   295,     0,   296,
     0,   145,     0,   146,     0,     5,   142,   148,   297,     6,
   148,     0,     5,   142,   148,     6,   148,     0,   297,   298,
     0,   298,     0,   299,     0,   408,     0,   145,     0,   146,
     0,    57,   147,   143,   149,   416,   148,     0,    57,   147,
   143,   148,     0,   123,   148,   301,     0,   123,   148,     0,
   301,   302,     0,   302,     0,   303,     0,   145,     0,   146,
     0,     5,   142,   148,   304,     6,   148,     0,     5,   142,
   148,     6,   148,     0,   304,   305,     0,   305,     0,   306,
     0,   307,     0,   308,     0,   309,     0,   310,     0,   311,
     0,   312,     0,   313,     0,   314,     0,   315,     0,   316,
     0,   408,     0,   145,     0,   146,     0,    58,   147,   413,
   148,     0,    67,   147,   413,   148,     0,    59,   147,   413,
   148,     0,    60,   147,   413,   148,     0,    61,   147,   415,
   149,   413,   148,     0,    62,   147,   415,   149,   413,   148,
     0,    63,   147,   415,   149,   413,   148,     0,    64,   147,
   415,   149,   413,   149,   415,   148,     0,    65,   147,   415,
   149,   413,   149,   415,   148,     0,    66,   147,   415,   149,
   413,   149,   415,   148,     0,    68,   147,   142,   148,     0,
   134,   148,   318,     0,   134,   148,     0,   318,   319,     0,
   319,     0,   320,     0,   145,     0,   146,     0,     5,   142,
   148,   321,     6,   148,     0,     5,   142,   148,     6,   148,
     0,   321,   322,     0,   322,     0,   323,     0,   324,     0,
   325,     0,   326,     0,   408,     0,   145,     0,   146,     0,
    69,   147,   413,   148,     0,    70,   147,   417,   148,     0,
    71,   147,   142,   148,     0,    72,   147,   416,   148,     0,
   122,   148,   328,     0,   122,   148,     0,   328,   329,     0,
   329,     0,   330,     0,   145,     0,   146,     0,     5,   142,
   148,   331,     6,   148,     0,     5,   142,   148,     6,   148,
     0,   331,   332,     0,   332,     0,   408,     0,   409,     0,
   145,     0,   146,     0,   136,   148,   334,     0,   136,   148,
     0,   334,   335,     0,   335,     0,   336,     0,   145,     0,
   146,     0,     5,   142,   148,   337,     6,   148,     0,     5,
   142,   148,     6,   148,     0,   337,   338,     0,   338,     0,
   339,     0,   340,     0,   408,     0,   145,     0,   146,     0,
    73,   147,   142,   148,     0,    74,   147,   143,   149,   413,
   149,   416,   148,     0,   137,   148,   342,     0,   137,   148,
     0,   342,   343,     0,   343,     0,   344,     0,   145,     0,
   146,     0,     5,   142,   148,   345,     6,   148,     0,     5,
   142,   148,     6,   148,     0,   345,   346,     0,   346,     0,
   347,     0,   348,     0,   349,     0,   350,     0,   351,     0,
   352,     0,   353,     0,   354,     0,   355,     0,   356,     0,
   357,     0,   358,     0,   359,     0,   360,     0,   361,     0,
   362,     0,   363,     0,   364,     0,   365,     0,   366,     0,
   367,     0,   408,     0,   145,     0,   146,     0,    75,   147,
   142,   148,     0,    76,   147,   142,   148,     0,    77,   147,
   142,   148,     0,    78,   147,   413,   149,   413,   149,   413,
   148,     0,    79,   147,   142,   148,     0,    80,   147,   142,
   148,     0,    81,   147,   413,   149,   413,   149,   413,   148,
     0,    82,   147,   410,   148,     0,    83,   147,   410,   148,
     0,    84,   147,   413,   148,     0,    85,   147,   413,   149,
   413,   148,     0,    86,   147,   413,   149,   413,   148,     0,
    87,   147,   415,   148,     0,    88,   147,   413,   148,     0,
    89,   147,   417,   148,     0,    90,   147,   413,   148,     0,
    91,   147,   413,   148,     0,    92,   147,   413,   148,     0,
    93,   147,   413,   148,     0,    94,   147,   417,   148,     0,
    95,   147,   142,   148,     0,   138,   148,   369,     0,   138,
   148,     0,   369,   370,     0,   370,     0,   371,     0,   145,
     0,   146,     0,     5,   142,   148,   372,     6,   148,     0,
     5,   142,   148,     6,   148,     0,   372,   373,     0,   373,
     0,   374,     0,   375,     0,   376,     0,   377,     0,   378,
     0,   379,     0,   380,     0,   381,     0,   382,     0,   383,
     0,   384,     0,   385,     0,   360,     0,   361,     0,   362,
     0,   363,     0,   364,     0,   365,     0,   366,     0,   367,
     0,   408,     0,   145,     0,   146,     0,    96,   147,   142,
   148,     0,    96,   147,   142,   149,   142,   148,     0,    97,
   147,   142,   148,     0,    98,   147,   142,   148,     0,    99,
   147,   142,   148,     0,   100,   147,   144,   148,     0,   105,
   147,   142,   148,     0,   101,   147,   142,   148,     0,   104,
   147,   142,   148,     0,   102,   147,   413,   148,     0,   103,
   147,   413,   148,     0,   106,   147,   413,   148,     0,   107,
   147,   417,   149,   143,   149,   413,   149,   413,   149,   417,
   149,   413,   149,   413,   149,   413,   148,     0,   140,   148,
   387,     0,   140,   148,     0,   387,   388,     0,   388,     0,
   389,     0,   145,     0,   146,     0,     5,   142,   148,   390,
     6,   148,     0,     5,   142,   148,     6,   148,     0,   390,
   391,     0,   391,     0,   392,     0,   408,     0,   145,     0,
   146,     0,   108,   147,   417,   149,   142,   149,   142,   149,
   142,   148,     0,   139,   148,   394,     0,   139,   148,     0,
   394,   395,     0,   395,     0,   396,     0,   145,     0,   146,
     0,     5,   142,   148,   397,     6,   148,     0,     5,   142,
   148,     6,   148,     0,   397,   398,     0,   398,     0,   399,
     0,   400,     0,   401,     0,   402,     0,   403,     0,   404,
     0,   405,     0,   406,     0,   408,     0,   145,     0,   146,
     0,   109,   147,   142,   149,   417,   149,   417,   149,   407,
   148,     0,   110,   147,   142,   149,   407,   149,   417,   149,
   416,   148,     0,   111,   147,   142,   148,     0,   112,   147,
   142,   148,     0,   113,   147,   417,   149,   142,   149,   417,
   149,   417,   148,     0,   113,   147,   417,   149,   142,   149,
   417,   148,     0,   114,   147,   417,   149,   417,   148,     0,
   115,   147,   142,   148,     0,   116,   147,   142,   149,   143,
   149,   143,   148,     0,   116,   147,   142,   148,     0,   407,
   149,   143,   149,   142,     0,   143,   149,   142,     0,     4,
   142,   149,   168,   147,   142,   148,     0,   141,   148,     0,
   413,   149,   411,     0,   413,     0,   411,   149,   412,     0,
   412,     0,   413,     0,   417,     0,   142,   142,     0,   414,
   149,   415,     0,   415,     0,   142,     0,   416,   149,   417,
     0,   417,     0,   142,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   332,   333,   335,   336,   338,   339,   340,   344,   349,   350,
   352,   353,   354,   355,   356,   357,   358,   359,   360,   361,
   362,   363,   364,   365,   366,   367,   368,   369,   371,   372,
   373,   374,   375,   379,   380,   384,   385,   387,   388,   390,
   391,   392,   394,   395,   399,   400,   402,   403,   405,   406,
   407,   409,   410,   415,   416,   418,   419,   420,   422,   424,
   425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
   435,   436,   437,   438,   439,   440,   441,   442,   443,   445,
   446,   448,   449,   450,   452,   453,   455,   456,   460,   461,
   463,   464,   466,   467,   468,   470,   472,   474,   475,   477,
   478,   479,   480,   481,   482,   484,   486,   488,   490,   499,
   500,   502,   503,   505,   506,   508,   509,   510,   514,   515,
   517,   518,   520,   521,   522,   524,   526,   528,   529,   531,
   532,   533,   534,   535,   536,   537,   538,   540,   542,   545,
   547,   552,   563,   564,   566,   567,   569,   570,   571,   573,
   574,   577,   578,   580,   581,   582,   583,   585,   590,   591,
   593,   594,   596,   597,   598,   600,   602,   605,   606,   608,
   609,   610,   611,   613,   615,   617,   619,   624,   625,   627,
   628,   630,   631,   632,   634,   635,   638,   639,   641,   642,
   643,   644,   645,   646,   647,   649,   650,   651,   652,   653,
   655,   657,   659,   661,   664,   667,   669,   672,   674,   676,
   678,   682,   686,   687,   689,   690,   692,   693,   694,   696,
   697,   700,   701,   703,   704,   705,   706,   707,   708,   709,
   710,   711,   712,   713,   715,   717,   719,   721,   723,   725,
   726,   728,   729,   731,   732,   736,   737,   739,   740,   742,
   743,   744,   746,   748,   750,   751,   753,   754,   755,   756,
   758,   760,   761,   762,   763,   764,   765,   766,   768,   769,
   770,   772,   774,   776,   778,   780,   782,   784,   786,   788,
   790,   792,   794,   798,   799,   801,   802,   804,   805,   806,
   808,   809,   812,   813,   815,   816,   817,   818,   819,   820,
   821,   823,   831,   839,   847,   856,   857,   859,   861,   863,
   865,   870,   871,   873,   874,   876,   877,   878,   880,   882,
   885,   886,   888,   889,   890,   891,   893,   898,   899,   901,
   902,   904,   905,   906,   908,   909,   912,   913,   915,   916,
   917,   918,   920,   922,   924,   926,   928,   930,   935,   936,
   938,   939,   942,   943,   944,   946,   948,   951,   953,   955,
   956,   958,   959,   960,   962,   964,   968,   969,   971,   973,
   975,   976,   977,   979,   981,   983,   985,   987,   988,   989,
   990,   992,   994,   999,  1000,  1002,  1004,  1006,  1007,  1008,
  1010,  1012,  1015,  1017,  1019,  1021,  1023,  1025,  1027,  1029,
  1031,  1033,  1035,  1037,  1039,  1041,  1042,  1043,  1045,  1047,
  1049,  1051,  1053,  1056,  1059,  1062,  1065,  1068,  1071,  1075,
  1076,  1078,  1079,  1081,  1082,  1083,  1085,  1087,  1090,  1091,
  1093,  1094,  1095,  1096,  1097,  1098,  1099,  1101,  1103,  1105,
  1107,  1111,  1113,  1115,  1117,  1120,  1121,  1122,  1124,  1126,
  1129,  1131,  1134,  1135,  1136,  1137,  1141,  1142,  1144,  1145,
  1147,  1148,  1149,  1151,  1153,  1156,  1157,  1159,  1160,  1161,
  1162,  1163,  1165,  1167,  1172,  1173,  1175,  1176,  1178,  1179,
  1180,  1182,  1184,  1186,  1187,  1189,  1190,  1191,  1192,  1193,
  1194,  1195,  1196,  1197,  1198,  1199,  1200,  1201,  1202,  1203,
  1204,  1205,  1206,  1207,  1208,  1209,  1210,  1211,  1212,  1214,
  1216,  1218,  1220,  1224,  1226,  1228,  1232,  1234,  1236,  1238,
  1242,  1246,  1248,  1250,  1252,  1254,  1256,  1258,  1260,  1262,
  1266,  1267,  1269,  1270,  1272,  1273,  1274,  1276,  1278,  1281,
  1282,  1284,  1285,  1286,  1287,  1288,  1289,  1290,  1291,  1292,
  1293,  1294,  1295,  1296,  1297,  1298,  1299,  1300,  1301,  1302,
  1303,  1304,  1305,  1306,  1308,  1309,  1312,  1314,  1316,  1318,
  1320,  1322,  1324,  1326,  1328,  1330,  1333,  1345,  1346,  1348,
  1350,  1352,  1353,  1354,  1357,  1359,  1362,  1364,  1366,  1367,
  1368,  1369,  1372,  1377,  1378,  1380,  1381,  1383,  1384,  1385,
  1387,  1389,  1392,  1393,  1395,  1396,  1397,  1399,  1400,  1402,
  1404,  1405,  1406,  1407,  1408,  1410,  1413,  1417,  1419,  1421,
  1424,  1428,  1431,  1433,  1435,  1438,  1440,  1445,  1448,  1450,
  1451,  1453,  1454,  1456,  1457,  1459,  1461,  1462,  1464,  1466,
  1467,  1469
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","T_VEX_REV",
"T_REF","T_DEF","T_ENDDEF","T_SCAN","T_ENDSCAN","T_CHAN_DEF","T_SAMPLE_RATE",
"T_BITS_PER_SAMPLE","T_SWITCHING_CYCLE","T_START","T_SOURCE","T_MODE","T_STATION",
"T_ANTENNA_DIAM","T_AXIS_OFFSET","T_ANTENNA_MOTION","T_POINTING_SECTOR","T_AXIS_TYPE",
"T_BBC_ASSIGN","T_CLOCK_EARLY","T_RECORD_TRANSPORT_TYPE","T_ELECTRONICS_RACK_TYPE",
"T_NUMBER_DRIVES","T_HEADSTACK","T_RECORD_DENSITY","T_TAPE_LENGTH","T_RECORDING_SYSTEM_ID",
"T_TAPE_MOTION","T_TAPE_CONTROL","T_TAI_UTC","T_A1_TAI","T_EOP_REF_EPOCH","T_NUM_EOP_POINTS",
"T_EOP_INTERVAL","T_UT1_UTC","T_X_WOBBLE","T_Y_WOBBLE","T_EXPER_NUM","T_EXPER_NAME",
"T_EXPER_NOMINAL_START","T_EXPER_NOMINAL_STOP","T_PI_NAME","T_PI_EMAIL","T_CONTACT_NAME",
"T_CONTACT_EMAIL","T_SCHEDULER_NAME","T_SCHEDULER_EMAIL","T_TARGET_CORRELATOR",
"T_EXPER_DESCRIPTION","T_HEADSTACK_POS","T_IF_DEF","T_PASS_ORDER","T_S2_GROUP_ORDER",
"T_PHASE_CAL_DETECT","T_TAPE_CHANGE","T_NEW_SOURCE_COMMAND","T_NEW_TAPE_SETUP",
"T_SETUP_ALWAYS","T_PARITY_CHECK","T_TAPE_PREPASS","T_PREOB_CAL","T_MIDOB_CAL",
"T_POSTOB_CAL","T_HEADSTACK_MOTION","T_PROCEDURE_NAME_PREFIX","T_ROLL_REINIT_PERIOD",
"T_ROLL_INC_PERIOD","T_ROLL","T_ROLL_DEF","T_SEFD_MODEL","T_SEFD","T_SITE_TYPE",
"T_SITE_NAME","T_SITE_ID","T_SITE_POSITION","T_SITE_POSITION_EPOCH","T_SITE_POSITION_REF",
"T_SITE_VELOCITY","T_HORIZON_MAP_AZ","T_HORIZON_MAP_EL","T_ZEN_ATMOS","T_OCEAN_LOAD_VERT",
"T_OCEAN_LOAD_HORIZ","T_OCCUPATION_CODE","T_INCLINATION","T_ECCENTRICITY","T_ARG_PERIGEE",
"T_ASCENDING_NODE","T_MEAN_ANOMALY","T_SEMI_MAJOR_AXIS","T_MEAN_MOTION","T_ORBIT_EPOCH",
"T_SOURCE_TYPE","T_SOURCE_NAME","T_IAU_NAME","T_RA","T_DEC","T_SOURCE_POSITION_REF",
"T_RA_RATE","T_DEC_RATE","T_SOURCE_POSITION_EPOCH","T_REF_COORD_FRAME","T_VELOCITY_WRT_LSR",
"T_SOURCE_MODEL","T_VSN","T_FANIN_DEF","T_FANOUT_DEF","T_TRACK_FRAME_FORMAT",
"T_DATA_MODULATION","T_VLBA_FRMTR_SYS_TRK","T_VLBA_TRNSPRT_SYS_TRK","T_S2_RECORDING_MODE",
"T_S2_DATA_SOURCE","B_GLOBAL","B_STATION","B_MODE","B_SCHED","B_EXPER","B_SCHEDULING_PARAMS",
"B_PROCEDURES","B_EOP","B_FREQ","B_CLOCK","B_ANTENNA","B_BBC","B_CORR","B_DAS",
"B_HEAD_POS","B_PASS_ORDER","B_PHASE_CAL_DETECT","B_ROLL","B_IF","B_SEFD","B_SITE",
"B_SOURCE","B_TRACKS","B_TAPELOG_OBS","T_LITERAL","T_NAME","T_LINK","T_ANGLE",
"T_COMMENT","T_COMMENT_TRAILING","'='","';'","':'","vex","version_lowls","version_lowl",
"version","blocks","block","global_block","station_block","station_defs","station_defx",
"station_def","mode_block","mode_defs","mode_defx","mode_def","refs","refx",
"ref","primitive","qrefs","qrefx","qref","qualifiers","sched_block","sched_defs",
"sched_defx","sched_def","sched_lowls","sched_lowl","start","mode","source",
"station","start_position","pass","sector","drives","antenna_block","antenna_defs",
"antenna_defx","antenna_def","antenna_lowls","antenna_lowl","antenna_diam","axis_type",
"axis_offset","antenna_motion","pointing_sector","bbc_block","bbc_defs","bbc_defx",
"bbc_def","bbc_lowls","bbc_lowl","bbc_assign","clock_block","clock_defs","clock_defx",
"clock_def","clock_lowls","clock_lowl","clock_early","das_block","das_defs",
"das_defx","das_def","das_lowls","das_lowl","record_transport_type","electronics_rack_type",
"number_drives","headstack","record_density","tape_length","recording_system_id",
"tape_motion","tape_control","eop_block","eop_defs","eop_defx","eop_def","eop_lowls",
"eop_lowl","tai_utc","a1_tai","eop_ref_epoch","num_eop_points","eop_interval",
"ut1_utc","x_wobble","y_wobble","exper_block","exper_defs","exper_defx","exper_def",
"exper_lowls","exper_lowl","exper_num","exper_name","exper_description","exper_nominal_start",
"exper_nominal_stop","pi_name","pi_email","contact_name","contact_email","scheduler_name",
"scheduler_email","target_correlator","freq_block","freq_defs","freq_defx","freq_def",
"freq_lowls","freq_lowl","chan_def","switch_states","switch_state","sample_rate",
"bits_per_sample","switching_cycle","head_pos_block","head_pos_defs","head_pos_defx",
"head_pos_def","head_pos_lowls","head_pos_lowl","headstack_pos","if_block","if_defs",
"if_defx","if_def","if_lowls","if_lowl","if_def_st","pass_order_block","pass_order_defs",
"pass_order_defx","pass_order_def","pass_order_lowls","pass_order_lowl","pass_order",
"s2_group_order","phase_cal_detect_block","phase_cal_detect_defs","phase_cal_detect_defx",
"phase_cal_detect_def","phase_cal_detect_lowls","phase_cal_detect_lowl","phase_cal_detect",
"procedures_block","procedures_defs","procedures_defx","procedures_def","procedures_lowls",
"procedures_lowl","tape_change","headstack_motion","new_source_command","new_tape_setup",
"setup_always","parity_check","tape_prepass","preob_cal","midob_cal","postob_cal",
"procedure_name_prefix","roll_block","roll_defs","roll_defx","roll_def","roll_lowls",
"roll_lowl","roll_reinit_period","roll_inc_period","roll","roll_def_st","scheduling_params_block",
"scheduling_params_defs","scheduling_params_defx","scheduling_params_def","scheduling_params_lowls",
"scheduling_params_lowl","sefd_block","sefd_defs","sefd_defx","sefd_def","sefd_lowls",
"sefd_lowl","sefd_model","sefd","site_block","site_defs","site_defx","site_def",
"site_lowls","site_lowl","site_type","site_name","site_id","site_position","site_position_epoch",
"site_position_ref","site_velocity","horizon_map_az","horizon_map_el","zen_atmos",
"ocean_load_vert","ocean_load_horiz","occupation_code","inclination","eccentricity",
"arg_perigee","ascending_node","mean_anomaly","semi_major_axis","mean_motion",
"orbit_epoch","source_block","source_defs","source_defx","source_def","source_lowls",
"source_lowl","source_type","source_name","iau_name","ra","dec","ref_coord_frame",
"source_position_ref","source_position_epoch","ra_rate","dec_rate","velocity_wrt_lsr",
"source_model","tapelog_obs_block","tapelog_obs_defs","tapelog_obs_defx","tapelog_obs_def",
"tapelog_obs_lowls","tapelog_obs_lowl","vsn","tracks_block","tracks_defs","tracks_defx",
"tracks_def","tracks_lowls","tracks_lowl","fanin_def","fanout_def","track_frame_format",
"data_modulation","vlba_frmtr_sys_trk","vlba_trnsprt_sys_trk","s2_recording_mode",
"s2_data_source","bit_stream_list","external_ref","literal","unit_list","unit_more",
"unit_option","unit_value","name_list","name_value","value_list","value", NULL
};
#endif

static const short yyr1[] = {     0,
   150,   150,   151,   151,   152,   152,   152,   153,   154,   154,
   155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
   155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
   155,   155,   155,   156,   156,   157,   157,   158,   158,   159,
   159,   159,   160,   160,   161,   161,   162,   162,   163,   163,
   163,   164,   164,   165,   165,   166,   166,   166,   167,   168,
   168,   168,   168,   168,   168,   168,   168,   168,   168,   168,
   168,   168,   168,   168,   168,   168,   168,   168,   168,   169,
   169,   170,   170,   170,   171,   171,   172,   172,   173,   173,
   174,   174,   175,   175,   175,   176,   176,   177,   177,   178,
   178,   178,   178,   178,   178,   179,   180,   181,   182,   183,
   183,   184,   184,   185,   185,   186,   186,   186,   187,   187,
   188,   188,   189,   189,   189,   190,   190,   191,   191,   192,
   192,   192,   192,   192,   192,   192,   192,   193,   194,   195,
   196,   197,   198,   198,   199,   199,   200,   200,   200,   201,
   201,   202,   202,   203,   203,   203,   203,   204,   205,   205,
   206,   206,   207,   207,   207,   208,   208,   209,   209,   210,
   210,   210,   210,   211,   211,   211,   211,   212,   212,   213,
   213,   214,   214,   214,   215,   215,   216,   216,   217,   217,
   217,   217,   217,   217,   217,   217,   217,   217,   217,   217,
   218,   219,   220,   221,   222,   223,   223,   224,   225,   225,
   225,   226,   227,   227,   228,   228,   229,   229,   229,   230,
   230,   231,   231,   232,   232,   232,   232,   232,   232,   232,
   232,   232,   232,   232,   233,   234,   235,   236,   237,   238,
   238,   239,   239,   240,   240,   241,   241,   242,   242,   243,
   243,   243,   244,   244,   245,   245,   246,   246,   246,   246,
   246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
   246,   247,   248,   249,   250,   251,   252,   253,   254,   255,
   256,   257,   258,   259,   259,   260,   260,   261,   261,   261,
   262,   262,   263,   263,   264,   264,   264,   264,   264,   264,
   264,   265,   265,   265,   265,   266,   266,   267,   268,   269,
   270,   271,   271,   272,   272,   273,   273,   273,   274,   274,
   275,   275,   276,   276,   276,   276,   277,   278,   278,   279,
   279,   280,   280,   280,   281,   281,   282,   282,   283,   283,
   283,   283,   284,   284,   284,   284,   284,   284,   285,   285,
   286,   286,   287,   287,   287,   288,   288,   289,   289,   290,
   290,   290,   290,   290,   291,   292,   293,   293,   294,   294,
   295,   295,   295,   296,   296,   297,   297,   298,   298,   298,
   298,   299,   299,   300,   300,   301,   301,   302,   302,   302,
   303,   303,   304,   304,   305,   305,   305,   305,   305,   305,
   305,   305,   305,   305,   305,   305,   305,   305,   306,   307,
   308,   309,   310,   311,   312,   313,   314,   315,   316,   317,
   317,   318,   318,   319,   319,   319,   320,   320,   321,   321,
   322,   322,   322,   322,   322,   322,   322,   323,   324,   325,
   326,   327,   327,   328,   328,   329,   329,   329,   330,   330,
   331,   331,   332,   332,   332,   332,   333,   333,   334,   334,
   335,   335,   335,   336,   336,   337,   337,   338,   338,   338,
   338,   338,   339,   340,   341,   341,   342,   342,   343,   343,
   343,   344,   344,   345,   345,   346,   346,   346,   346,   346,
   346,   346,   346,   346,   346,   346,   346,   346,   346,   346,
   346,   346,   346,   346,   346,   346,   346,   346,   346,   347,
   348,   349,   350,   351,   352,   353,   354,   355,   356,   357,
   358,   359,   360,   361,   362,   363,   364,   365,   366,   367,
   368,   368,   369,   369,   370,   370,   370,   371,   371,   372,
   372,   373,   373,   373,   373,   373,   373,   373,   373,   373,
   373,   373,   373,   373,   373,   373,   373,   373,   373,   373,
   373,   373,   373,   373,   374,   374,   375,   376,   377,   378,
   379,   380,   381,   382,   383,   384,   385,   386,   386,   387,
   387,   388,   388,   388,   389,   389,   390,   390,   391,   391,
   391,   391,   392,   393,   393,   394,   394,   395,   395,   395,
   396,   396,   397,   397,   398,   398,   398,   398,   398,   398,
   398,   398,   398,   398,   398,   399,   400,   401,   402,   403,
   403,   404,   405,   406,   406,   407,   407,   408,   409,   410,
   410,   411,   411,   412,   412,   413,   414,   414,   415,   416,
   416,   417
};

static const short yyr2[] = {     0,
     2,     1,     2,     1,     1,     1,     1,     4,     2,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     3,     2,     3,     2,     2,     1,     1,
     1,     1,     6,     5,     3,     2,     2,     1,     1,     1,
     1,     6,     5,     2,     1,     1,     1,     1,     5,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
     1,     1,     1,     1,     6,     5,     3,     2,     3,     2,
     2,     1,     1,     1,     1,     6,     5,     2,     1,     1,
     1,     1,     1,     1,     1,     4,     4,     4,    16,     0,
     1,     0,     1,     0,     1,     0,     1,     3,     3,     2,
     2,     1,     1,     1,     1,     6,     5,     2,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     4,     6,     4,
     8,    16,     3,     2,     2,     1,     1,     1,     1,     6,
     5,     2,     1,     1,     1,     1,     1,     8,     3,     2,
     2,     1,     1,     1,     1,     6,     5,     2,     1,     1,
     1,     1,     1,     5,     6,    10,     9,     3,     2,     2,
     1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     4,     4,     4,     8,     5,     4,     8,     4,     4,     6,
    10,     4,     3,     2,     2,     1,     1,     1,     1,     6,
     5,     2,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     4,     4,     4,     4,     4,     4,
     3,     4,     3,     4,     3,     3,     2,     2,     1,     1,
     1,     1,     6,     5,     2,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     4,     4,     4,     3,     2,     2,     1,     1,     1,     1,
     6,     5,     2,     1,     1,     1,     1,     1,     1,     1,
     1,    16,    17,    15,    16,     2,     1,     2,     4,     4,
     6,     3,     2,     2,     1,     1,     1,     1,     6,     5,
     2,     1,     1,     1,     1,     1,     6,     3,     2,     2,
     1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
     1,     1,    16,    12,    14,    13,    14,    15,     3,     2,
     2,     1,     1,     1,     1,     6,     5,     2,     1,     1,
     1,     1,     1,     1,     4,     4,     3,     2,     2,     1,
     1,     1,     1,     6,     5,     2,     1,     1,     1,     1,
     1,     6,     4,     3,     2,     2,     1,     1,     1,     1,
     6,     5,     2,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     4,     4,
     4,     4,     6,     6,     6,     8,     8,     8,     4,     3,
     2,     2,     1,     1,     1,     1,     6,     5,     2,     1,
     1,     1,     1,     1,     1,     1,     1,     4,     4,     4,
     4,     3,     2,     2,     1,     1,     1,     1,     6,     5,
     2,     1,     1,     1,     1,     1,     3,     2,     2,     1,
     1,     1,     1,     6,     5,     2,     1,     1,     1,     1,
     1,     1,     4,     8,     3,     2,     2,     1,     1,     1,
     1,     6,     5,     2,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
     4,     4,     8,     4,     4,     8,     4,     4,     4,     6,
     6,     4,     4,     4,     4,     4,     4,     4,     4,     4,
     3,     2,     2,     1,     1,     1,     1,     6,     5,     2,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     4,     6,     4,     4,     4,     4,
     4,     4,     4,     4,     4,     4,    18,     3,     2,     2,
     1,     1,     1,     1,     6,     5,     2,     1,     1,     1,
     1,     1,    10,     3,     2,     2,     1,     1,     1,     1,
     6,     5,     2,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,    10,    10,     4,     4,    10,
     8,     6,     4,     8,     4,     5,     3,     7,     2,     3,
     1,     3,     1,     1,     1,     2,     3,     1,     1,     3,
     1,     1
};

static const short yydefact[] = {     0,
     0,     6,     7,     2,     4,     5,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     3,     1,    10,    11,    12,    13,    15,    16,    17,    18,
    19,    20,    21,    14,    22,    23,    24,    25,    26,    27,
    28,    29,    30,    31,    32,    33,   642,     0,    35,    37,
    46,    90,   247,   443,   385,   214,   285,   160,   120,   144,
   179,   313,   350,   368,   421,   329,   458,   476,   532,   595,
   579,     9,     8,     0,    57,    58,    34,    55,    56,     0,
    41,    42,    36,    39,    40,     0,    50,    51,    45,    48,
    49,     0,    94,    95,    89,    92,    93,     0,   251,   252,
   246,   249,   250,     0,   447,   448,   442,   445,   446,     0,
   389,   390,   384,   387,   388,     0,   218,   219,   213,   216,
   217,     0,   289,   290,   284,   287,   288,     0,   164,   165,
   159,   162,   163,     0,   124,   125,   119,   122,   123,     0,
   148,   149,   143,   146,   147,     0,   183,   184,   178,   181,
   182,     0,   317,   318,   312,   315,   316,     0,   354,   355,
   349,   352,   353,     0,   372,   373,   367,   370,   371,     0,
   425,   426,   420,   423,   424,     0,   333,   334,   328,   331,
   332,     0,   462,   463,   457,   460,   461,     0,   480,   481,
   475,   478,   479,     0,   536,   537,   531,   534,   535,     0,
   599,   600,   594,   597,   598,     0,   583,   584,   578,   581,
   582,    60,    61,    62,    63,    64,    67,    65,    66,    68,
    69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
    79,     0,    54,     0,    38,     0,    47,     0,    91,     0,
   248,     0,   444,     0,   386,     0,   215,     0,   286,     0,
   161,     0,   121,     0,   145,     0,   180,     0,   314,     0,
   351,     0,   369,     0,   422,     0,   330,     0,   459,     0,
   477,     0,   533,     0,   596,     0,   580,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    83,    84,     0,    81,    82,
     0,     0,     0,     0,     0,   104,   105,     0,    99,   100,
   101,   102,   103,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   270,   271,     0,
   256,   257,   258,   259,   260,   261,   262,   263,   264,   265,
   266,   267,   268,   269,     0,     0,   455,   456,     0,   452,
   453,   454,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   407,   408,     0,   394,   395,   396,
   397,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   233,
   234,     0,   223,   224,   225,   226,   227,   228,   229,   230,
   231,   232,     0,     0,     0,     0,     0,   300,   301,     0,
   294,   295,   296,   297,   298,   299,     0,     0,   172,   173,
     0,   169,   170,   171,     0,     0,     0,     0,     0,     0,
   136,   137,     0,   129,   130,   131,   132,   133,   134,   135,
     0,     0,   156,   157,     0,   153,   154,   155,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   199,   200,
     0,   188,   189,   190,   191,   192,   193,   194,   195,   196,
   197,   198,     0,     0,   325,   326,     0,   322,   323,   324,
     0,     0,     0,   363,   364,     0,   359,   360,   361,   362,
     0,     0,   380,   381,     0,   377,   378,   379,     0,     0,
     0,     0,     0,   436,   437,     0,   430,   431,   432,   433,
   434,   435,     0,     0,   341,   342,     0,   338,   339,   340,
     0,     0,     0,   471,   472,     0,   467,   468,   469,   470,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   508,   509,     0,   485,   486,   487,   488,   489,
   490,   491,   492,   493,   494,   495,   496,   497,   498,   499,
   500,   501,   502,   503,   504,   505,   506,   507,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   563,   564,   554,   555,   556,   557,   558,   559,   560,
   561,     0,   541,   542,   543,   544,   545,   546,   547,   548,
   549,   550,   551,   552,   553,   562,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   614,   615,     0,   604,   605,
   606,   607,   608,   609,   610,   611,   612,   613,     0,     0,
   591,   592,     0,   588,   589,   590,    59,    44,     0,     0,
    53,     0,    80,    97,     0,     0,     0,     0,     0,    98,
     0,   254,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   255,   450,   629,     0,   451,
   392,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   393,   221,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   222,   292,     0,     0,     0,     0,
     0,   293,   167,     0,     0,   168,   127,     0,     0,     0,
     0,     0,     0,   128,   151,     0,     0,   152,   186,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
   320,     0,     0,   321,   357,     0,     0,     0,   358,   375,
     0,     0,   376,   428,     0,     0,     0,     0,     0,   429,
   336,     0,     0,   337,   465,     0,     0,     0,   466,   483,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   484,   539,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   540,   602,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   603,   586,
     0,     0,   587,    43,     0,    52,     0,     0,     0,     0,
    96,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   253,   449,     0,     0,     0,     0,
   639,     0,     0,     0,     0,     0,     0,     0,     0,   391,
     0,     0,     0,     0,     0,   241,     0,   631,   243,     0,
   245,     0,   220,     0,     0,     0,     0,     0,   291,     0,
     0,   166,     0,     0,     0,     0,     0,   126,     0,   150,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   185,
     0,   319,     0,   638,     0,   641,   356,     0,   374,     0,
     0,     0,     0,   427,     0,   335,     0,     0,   464,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   538,     0,     0,     0,     0,     0,     0,
     0,     0,   601,     0,   585,     0,   106,   108,   107,     0,
     0,   272,   273,   275,   276,   277,   278,   279,   280,   281,
   282,   283,   274,   636,   409,   411,   412,     0,     0,     0,
     0,     0,     0,   410,   419,   235,   236,   237,   238,   239,
   240,     0,   242,   244,     0,     0,   309,   310,     0,     0,
     0,   138,   140,     0,     0,     0,     0,   201,   202,   203,
     0,     0,   206,     0,   208,   209,     0,   212,     0,   365,
     0,   366,     0,   383,     0,   438,   439,   440,   441,     0,
   473,     0,   510,   511,   512,     0,   514,   515,     0,   517,
   518,   519,     0,     0,   522,   523,   524,   525,   526,   527,
   528,   529,   530,   565,     0,   567,   568,   569,   570,   572,
   574,   575,   573,   571,   576,     0,     0,     0,   618,   619,
     0,     0,   623,   625,     0,     0,    86,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   642,   630,   633,
   634,   635,     0,     0,     0,     0,   174,     0,     0,     0,
     0,     0,     0,   205,     0,     0,     0,   637,   640,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    88,    85,     0,     0,     0,
   413,   414,   415,     0,     0,     0,     0,     0,     0,   311,
   175,     0,     0,     0,     0,   139,     0,     0,     0,   210,
     0,   327,   382,     0,     0,     0,     0,   520,   521,   566,
     0,     0,     0,     0,     0,   622,     0,     0,    87,     0,
   628,     0,     0,     0,   632,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   627,     0,     0,     0,     0,     0,   110,   416,   417,
   418,     0,     0,     0,     0,   141,     0,   158,   204,   207,
     0,     0,   474,   513,   516,     0,     0,     0,     0,   621,
     0,   624,     0,     0,   111,     0,     0,     0,   177,     0,
     0,     0,     0,     0,   626,     0,     0,     0,   112,     0,
     0,   176,     0,   211,     0,     0,   616,     0,   617,   620,
   593,   113,     0,     0,     0,     0,     0,     0,   114,     0,
     0,     0,   344,     0,     0,   115,     0,     0,     0,     0,
   346,     0,     0,     0,   116,     0,     0,     0,   345,   347,
     0,     0,     0,   117,     0,   304,     0,     0,   307,     0,
   348,     0,     0,   109,     0,   302,     0,   308,   305,   306,
   142,   343,     0,   118,   303,     0,   577,     0,     0,     0
};

static const short yydefgoto[] = {  1338,
     4,     5,     6,    32,    33,    34,    35,    93,    94,    95,
    36,    99,   100,   101,    87,    88,    89,   242,   318,   319,
   320,  1109,    37,   105,   106,   107,   328,   329,   330,   331,
   332,   333,  1254,  1283,  1297,  1313,    38,   147,   148,   149,
   453,   454,   455,   456,   457,   458,   459,    39,   153,   154,
   155,   465,   466,   467,    40,   141,   142,   143,   441,   442,
   443,    41,   159,   160,   161,   481,   482,   483,   484,   485,
   486,   487,   488,   489,   490,   491,    42,   129,   130,   131,
   412,   413,   414,   415,   416,   417,   418,   419,   420,   421,
    43,   111,   112,   113,   350,   351,   352,   353,   354,   355,
   356,   357,   358,   359,   360,   361,   362,   363,    44,   135,
   136,   137,   430,   431,   432,  1318,  1319,   433,   434,   435,
    45,   165,   166,   167,   497,   498,   499,    46,   189,   190,
   191,   537,   538,   539,    47,   171,   172,   173,   506,   507,
   508,   509,    48,   177,   178,   179,   515,   516,   517,    49,
   123,   124,   125,   387,   388,   389,   390,   391,   392,   393,
   394,   395,   396,   397,   398,   399,    50,   183,   184,   185,
   526,   527,   528,   529,   530,   531,    51,   117,   118,   119,
   369,   370,    52,   195,   196,   197,   546,   547,   548,   549,
    53,   201,   202,   203,   575,   576,   577,   578,   579,   580,
   581,   582,   583,   584,   585,   586,   587,   588,   589,   590,
   591,   592,   593,   594,   595,   596,   597,    54,   207,   208,
   209,   622,   623,   624,   625,   626,   627,   628,   629,   630,
   631,   632,   633,   634,   635,    55,   219,   220,   221,   663,
   664,   665,    56,   213,   214,   215,   648,   649,   650,   651,
   652,   653,   654,   655,   656,   657,  1151,   364,   372,   887,
  1119,  1120,   888,   923,   872,   925,   926
};

static const short yypact[] = {    33,
   -59,-32768,-32768,    31,-32768,-32768,  -117,   -58,   -51,   -46,
   -25,   -23,    12,    24,    27,    54,    61,    63,    66,    70,
    72,    89,   113,   144,   146,   149,   170,   182,   188,   190,
-32768,   747,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   193,    94,    37,
    47,    28,    53,    99,   117,   140,   211,   230,   291,   308,
   314,   317,   581,   590,   613,   619,   625,   672,   678,   692,
   701,-32768,-32768,   767,-32768,-32768,    94,-32768,-32768,   201,
-32768,-32768,    37,-32768,-32768,   203,-32768,-32768,    47,-32768,
-32768,   208,-32768,-32768,    28,-32768,-32768,   222,-32768,-32768,
    53,-32768,-32768,   261,-32768,-32768,    99,-32768,-32768,   296,
-32768,-32768,   117,-32768,-32768,   299,-32768,-32768,   140,-32768,
-32768,   310,-32768,-32768,   211,-32768,-32768,   319,-32768,-32768,
   230,-32768,-32768,   339,-32768,-32768,   291,-32768,-32768,   349,
-32768,-32768,   308,-32768,-32768,   350,-32768,-32768,   314,-32768,
-32768,   352,-32768,-32768,   317,-32768,-32768,   357,-32768,-32768,
   581,-32768,-32768,   358,-32768,-32768,   590,-32768,-32768,   366,
-32768,-32768,   613,-32768,-32768,   380,-32768,-32768,   619,-32768,
-32768,   381,-32768,-32768,   625,-32768,-32768,   383,-32768,-32768,
   672,-32768,-32768,   384,-32768,-32768,   678,-32768,-32768,   386,
-32768,-32768,   692,-32768,-32768,   387,-32768,-32768,   701,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   141,-32768,   205,-32768,   382,-32768,   390,-32768,   420,
-32768,   434,-32768,   436,-32768,   439,-32768,   461,-32768,   465,
-32768,   467,-32768,   493,-32768,   496,-32768,   524,-32768,   537,
-32768,   539,-32768,   540,-32768,   544,-32768,   547,-32768,   550,
-32768,   556,-32768,   559,-32768,   562,-32768,   392,   375,   710,
   778,   206,    45,    51,    42,   213,   501,   602,   667,    35,
   586,    39,   579,   209,   636,   289,   338,   294,   333,   644,
   567,   570,   713,   767,   572,-32768,-32768,   717,-32768,-32768,
   580,   385,   502,   534,   582,-32768,-32768,   791,-32768,-32768,
-32768,-32768,-32768,   588,   585,   587,   591,   592,   594,   595,
   596,   603,   604,   610,   614,   615,   616,-32768,-32768,   529,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   589,   612,-32768,-32768,   639,-32768,
-32768,-32768,   618,   620,   621,   628,   629,   630,   631,   632,
   640,   641,   648,   649,-32768,-32768,   451,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   626,   650,   653,   654,   655,   661,   662,   663,   664,-32768,
-32768,   232,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   635,   669,   673,   682,   683,-32768,-32768,   492,
-32768,-32768,-32768,-32768,-32768,-32768,   666,   684,-32768,-32768,
   676,-32768,-32768,-32768,   691,   693,   694,   695,   696,   702,
-32768,-32768,   608,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   700,   703,-32768,-32768,   680,-32768,-32768,-32768,   706,   704,
   714,   729,   761,   762,   763,   764,   765,   766,-32768,-32768,
   202,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   709,   768,-32768,-32768,   687,-32768,-32768,-32768,
   712,   769,   770,-32768,-32768,   533,-32768,-32768,-32768,-32768,
   771,   773,-32768,-32768,   699,-32768,-32768,-32768,   774,   779,
   780,   781,   782,-32768,-32768,   398,-32768,-32768,-32768,-32768,
-32768,-32768,   777,   783,-32768,-32768,   690,-32768,-32768,-32768,
   784,   786,   787,-32768,-32768,   627,-32768,-32768,-32768,-32768,
   790,   788,   792,   793,   794,   795,   796,   797,   798,   799,
   800,   801,   802,   803,   804,   805,   806,   807,   808,   809,
   810,   811,-32768,-32768,   471,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   812,   814,
   815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
   825,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   563,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,   826,   828,   829,   830,
   831,   832,   833,   834,   835,-32768,-32768,   489,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   836,   838,
-32768,-32768,   707,-32768,-32768,-32768,-32768,-32768,   839,   841,
-32768,   842,-32768,-32768,   772,   776,   789,   844,   843,-32768,
   840,-32768,  -117,   850,   851,   852,   853,   854,   855,   856,
   857,   858,   859,   860,   861,-32768,-32768,-32768,   862,-32768,
-32768,   863,   863,   863,   864,   864,   864,   864,   864,   864,
   863,   865,   866,-32768,-32768,   863,   863,   869,  -117,   863,
  -122,  -120,   -92,   868,-32768,-32768,    46,   863,  -117,   870,
   871,-32768,-32768,  -102,   872,-32768,-32768,   863,   863,   875,
   878,   876,   874,-32768,-32768,   880,   877,-32768,-32768,   882,
   884,  -117,  -117,   885,   863,  -117,   886,   887,   883,-32768,
-32768,  -117,   888,-32768,-32768,   864,  -117,   889,-32768,-32768,
   890,   891,-32768,-32768,   863,  -117,   892,  -117,   893,-32768,
-32768,   895,   894,-32768,-32768,   898,   900,   896,-32768,-32768,
   903,   905,   906,   863,   907,   908,   863,   863,   863,   863,
   863,   863,   864,   863,  -117,   863,   863,   863,   863,  -117,
   909,   904,-32768,-32768,   911,   912,   913,   914,   915,   916,
   863,   863,   918,   919,   863,  -117,   917,-32768,-32768,   920,
   921,   922,   924,  -117,  -117,   925,   926,   923,-32768,-32768,
  -117,   927,-32768,-32768,   928,-32768,   929,   930,   931,   932,
-32768,   767,   934,   935,   936,   937,   938,   939,   940,   941,
   943,   944,   945,   946,-32768,-32768,   954,   949,   950,   951,
-32768,   952,   953,   955,   956,   957,   958,   960,   961,-32768,
   962,   963,   964,   965,   966,-32768,   967,   968,-32768,   970,
-32768,   971,-32768,   972,   863,   974,   975,   976,-32768,   977,
   863,-32768,   979,   980,   981,   982,   983,-32768,   984,-32768,
   986,   987,   988,   989,   978,  -125,   991,   -80,   992,-32768,
   993,-32768,   -78,-32768,   -76,-32768,-32768,   -62,-32768,   995,
   996,   997,    -2,-32768,   998,-32768,  1000,  1001,-32768,  1003,
  1004,  1005,  1006,  1008,  1009,  1010,  1013,  1014,  1015,  1016,
  1017,  1019,  1020,  1021,  1022,  1025,  1026,  1027,  1028,  1029,
-32768,    56,  1030,  1031,  1032,  1033,  1034,  1035,  1036,  1038,
  1041,  1042,  1043,-32768,  1044,  1045,  1047,  1048,  1049,  1050,
  1052,    93,-32768,  1054,-32768,   111,-32768,-32768,-32768,   863,
   910,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   863,   863,   863,
   863,   863,   863,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   999,-32768,-32768,   863,  1055,-32768,-32768,   863,   863,
   125,-32768,-32768,   863,  1007,  1012,  -117,-32768,-32768,-32768,
  1059,  1057,-32768,  1064,-32768,-32768,   863,-32768,   863,-32768,
   864,-32768,  -117,-32768,  -117,-32768,-32768,-32768,-32768,  1065,
-32768,   863,-32768,-32768,-32768,   863,-32768,-32768,   863,-32768,
-32768,-32768,   863,   863,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,  1066,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   933,  -117,   973,-32768,-32768,
  1067,  -117,-32768,-32768,   994,  1068,-32768,  1069,   128,  1063,
  1071,  1070,  1072,  1073,  1074,  1075,  1076,   954,  1077,-32768,
-32768,-32768,  1079,  1080,  1081,   135,-32768,  1088,  1082,  1083,
  1085,  1086,  1087,-32768,  1089,   142,  1091,-32768,-32768,   153,
  1092,  1093,  1094,  1096,  1098,  1099,  1100,  1101,  1102,  1103,
  1104,  1105,  1107,  1108,  1109,-32768,-32768,  1095,   863,  1111,
-32768,-32768,-32768,   864,   864,   864,   999,  1114,  1112,-32768,
-32768,  1118,  1113,   863,   863,-32768,  1097,  -117,  -117,-32768,
   863,-32768,-32768,  1121,  -117,   863,   863,-32768,-32768,-32768,
   863,  -117,  1122,   161,  -117,-32768,  1106,  1123,-32768,  1117,
-32768,  1119,  1120,  1124,-32768,  1125,   863,  1126,  -117,  1128,
  1129,  1131,  1132,  1133,  1134,  1135,   157,  1137,  1138,  1139,
  1140,-32768,  1141,  1142,   168,  1144,  1145,   863,-32768,-32768,
-32768,   863,  1146,  -117,  1148,-32768,   863,-32768,-32768,-32768,
   863,   863,-32768,-32768,-32768,   863,   973,  1151,  -117,-32768,
  -117,-32768,  1155,  1150,-32768,  1152,  1127,  1154,-32768,  1156,
  1158,  1159,  1160,   218,-32768,   221,  1163,  1164,  1161,  1130,
  1165,-32768,  1162,-32768,  1171,  -117,-32768,  1157,-32768,-32768,
-32768,-32768,  1166,  1167,  1174,  1170,   257,  1172,  1177,  1179,
  1175,   863,-32768,   -57,   863,-32768,  1176,  1178,  1180,  1181,
-32768,  1183,   259,  1184,  -117,  1185,   262,   863,-32768,-32768,
    59,   863,  1186,  1187,   302,-32768,  -117,   316,-32768,  1189,
-32768,  1190,  1191,-32768,  -117,-32768,   323,-32768,-32768,-32768,
-32768,-32768,   863,-32768,-32768,  1193,-32768,   959,  1197,-32768
};

static const short yypgoto[] = {-32768,
-32768,   969,-32768,-32768,  1037,-32768,-32768,-32768,   942,-32768,
-32768,-32768,  1115,-32768,   715,   -70,-32768,  -310,-32768,   665,
-32768,-32768,-32768,-32768,  1024,-32768,-32768,   593,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1011,-32768,
-32768,   555,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   879,
-32768,-32768,   538,-32768,-32768,-32768,  1023,-32768,-32768,   633,
-32768,-32768,-32768,  1056,-32768,-32768,   532,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1062,-32768,
-32768,   660,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,  1196,-32768,-32768,   723,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  1084,-32768,-32768,   670,-32768,  -235, -1274,-32768,-32768,-32768,
-32768,-32768,  1051,-32768,-32768,   606,-32768,-32768,-32768,  1143,
-32768,-32768,   609,-32768,-32768,-32768,  1046,-32768,-32768,   728,
-32768,-32768,-32768,-32768,   947,-32768,-32768,   756,-32768,-32768,
-32768,  1203,-32768,-32768,   948,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1168,-32768,
-32768,   751,-32768,-32768,-32768,-32768,-32768,-32768,  1212,-32768,
-32768,   985,-32768,-32768,  1147,-32768,-32768,   736,-32768,-32768,
-32768,-32768,  1149,-32768,-32768,   785,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  -302,
  -299,  -297,  -296,  -295,  -294,  -290,  -287,-32768,-32768,  1136,
-32768,-32768,   722,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1153,-32768,-32768,
   624,-32768,-32768,-32768,  1169,-32768,-32768,   697,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    92,  -166,-32768,  -715,
-32768,   179,  -701,-32768,  -677,  -773,    -7
};


#define	YYLAST		1382


static const short yytable[] = {    58,
   868,   869,   870,   670,   933,   614,   890,   892,   615,   878,
   616,   617,   618,   619,   881,   882,   243,   620,   885,   867,
   621,   867,  1043,  1044,    57,   886,   896,   889,   873,   874,
   875,   876,   877,     1,   102,     1,   903,   904,   334,   900,
   469,    90,   334,  1330,   501,   334,   901,   401,   334,   867,
   365,    96,  1330,   916,   334,   891,   373,   108,   470,   471,
   472,   473,   474,   475,   476,   477,   478,  1046,  1047,  1050,
  1051,  1052,  1053,   930,   402,   403,   404,   405,   406,   407,
   408,   409,   947,   948,   867,  1054,  1055,     7,   924,    59,
  1301,  1302,   943,   502,   503,   946,    60,    84,   949,   950,
   951,    61,   953,   114,   955,   956,   957,   958,   374,   375,
   376,   377,   378,   379,   380,   381,   382,   383,   384,   968,
   969,   120,    62,   972,    63,   952,   371,   400,   422,   436,
   444,   460,   468,   492,   500,   510,   518,   532,   540,   550,
   598,   636,   658,   666,   126,  1059,  1053,     8,     9,    10,
    11,    12,    13,    14,    15,    16,    17,    18,    19,    64,
    20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
    30,    65,   103,   104,    66,     2,     3,     2,     3,   479,
   480,    91,    92,   504,   505,   366,   410,   411,   894,   367,
   368,    97,    98,  1026,   895,   385,   386,   109,   110,  1031,
   867,    67,   371,  1084,  1085,   334,  1321,   759,    68,   334,
    69,   335,   334,    70,   519,   132,   334,    71,   423,    72,
   400,   424,   425,   426,   427,   470,   471,   472,   473,   474,
   475,   476,   477,   478,   138,   334,    73,   724,    85,    86,
  1104,  1105,   243,   115,   116,   422,   336,   337,   338,   339,
   340,   341,   342,   343,   344,   345,   346,   347,  1107,  1108,
    74,   121,   122,   436,   402,   403,   404,   405,   406,   407,
   408,   409,  1127,  1128,   444,  1157,  1158,   520,   521,   522,
   523,  1140,  1171,  1172,   127,   128,   460,   288,  1110,  1180,
  1181,    75,   334,    76,   541,   144,    77,   334,   468,   599,
  1183,  1053,    57,  1223,  1243,  1053,  1112,  1113,  1114,  1115,
  1116,  1117,   150,  1125,   492,  1250,  1251,    78,   156,   614,
  1121,   162,   615,  1123,   616,   617,   618,   619,  1126,    79,
   500,   620,  1129,  1137,   621,    80,   334,    81,   637,   510,
    83,   334,   244,   551,   246,  1136,   479,   480,   518,   248,
   348,   349,   289,   524,   525,   133,   134,   428,   429,   532,
  1142,   542,   543,   250,  1143,  1277,  1278,  1144,  1279,  1053,
   540,  1145,  1146,  1138,   139,   140,   410,   411,    84,   550,
   312,   565,   566,   567,   568,   569,   570,   571,   572,   600,
   601,   602,   603,   604,   605,   606,   607,   608,   609,   610,
   611,   334,   252,   779,  1293,  1294,  1310,  1311,   598,  1316,
  1317,  1217,   552,   553,   554,   555,   556,   557,   558,   559,
   560,   561,   562,   563,   564,   565,   566,   567,   568,   569,
   570,   571,   572,   544,   545,   145,   146,   254,   612,   613,
   256,   638,   639,   640,   641,   642,   643,   644,   645,  1326,
  1317,   258,   151,   152,   334,   636,   713,  1200,   157,   158,
   260,   163,   164,  1329,  1317,  1121,   520,   521,   522,   523,
  1335,  1317,  1210,  1211,   334,  1266,   812,   646,   647,  1215,
   262,   658,   573,   574,  1218,  1219,  1202,  1203,  1204,  1220,
   264,   266,   334,   268,   838,   334,   666,   731,   270,   272,
   424,   425,   426,   427,   334,  1233,   437,   274,   374,   375,
   376,   377,   378,   379,   380,   381,   382,   383,   384,    85,
    86,   276,   278,   438,   280,   282,  1255,   284,   286,   290,
  1256,   675,   334,   311,   695,  1260,   334,   291,   768,  1261,
  1262,   991,   524,   525,  1263,   552,   553,   554,   555,   556,
   557,   558,   559,   560,   561,   562,   563,   564,   565,   566,
   567,   568,   569,   570,   571,   572,   334,   292,   827,   336,
   337,   338,   339,   340,   341,   342,   343,   344,   345,   346,
   347,   293,   334,   294,   511,   168,   295,   502,   503,   334,
  1300,   493,  1303,  1304,   174,   385,   386,   638,   639,   640,
   641,   642,   643,   644,   645,   334,  1320,   445,   296,  1322,
  1323,   334,   297,   743,   298,   573,   574,   180,   446,   447,
   448,   449,   450,   186,   446,   447,   448,   449,   450,   192,
   334,  1336,   788,   646,   647,   512,   428,   429,   494,   334,
   299,   533,   334,   300,   699,   439,   440,   334,   676,   659,
   565,   566,   567,   568,   569,   570,   571,   572,   600,   601,
   602,   603,   604,   605,   606,   607,   608,   609,   610,   611,
   334,   301,   461,   348,   349,   853,   198,   504,   505,   334,
   677,   735,   204,   334,   302,   747,   303,   304,   462,   534,
   334,   305,   763,   334,   306,   783,   210,   307,   438,   542,
   543,   462,   334,   308,   772,   216,   309,   612,   613,   310,
   334,   884,   842,   314,   667,   315,    84,   668,   669,   671,
   314,   897,   672,   513,   514,   169,   170,   674,   678,   681,
   495,   496,   682,   683,   175,   176,   697,   684,   685,   494,
   686,   687,   688,   534,   913,   914,   451,   452,   917,   689,
   690,   660,   451,   452,   921,   512,   691,   181,   182,   698,
   692,   693,   694,   187,   188,   701,   702,   703,   931,   193,
   194,   544,   545,   715,   704,   705,   706,   707,   708,   366,
   535,   536,   726,   367,   368,   321,   709,   710,   661,   662,
   322,   323,   324,   325,   711,   712,   716,   954,   679,   717,
   718,   719,   959,   322,   323,   324,   325,   720,   721,   722,
   723,   463,   464,   733,   660,   727,   199,   200,   973,   728,
   439,   440,   205,   206,   463,   464,   979,   980,   729,   730,
   734,   495,   496,   984,   535,   536,   211,   212,   737,   738,
   739,   740,   741,   513,   514,   217,   218,   745,   742,   746,
   750,   661,   662,   749,   316,   317,   761,    85,    86,   765,
   751,   316,   317,     8,     9,    10,    11,    12,    13,    14,
    15,    16,    17,    18,    19,   752,    20,    21,    22,    23,
    24,    25,    26,    27,    28,    29,    30,   222,   223,   224,
   225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
   235,   236,   237,   238,   239,   240,   241,   753,   754,   755,
   756,   757,   758,   847,   762,   766,   767,   848,   770,   771,
   680,   774,   326,   327,   781,   775,   776,   777,   778,   782,
   849,   785,   786,   787,   791,   326,   327,   790,   792,   793,
   794,   795,   796,   797,   798,   799,   800,   801,   802,   803,
   804,   805,   806,   807,   808,   809,   810,   811,  1339,   814,
   815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
   825,   826,    31,   829,   830,   831,   832,   833,   834,   835,
   836,   837,   673,   840,   841,   850,   844,   845,   852,   846,
   851,   854,   855,   856,   857,   858,   859,   860,   861,   862,
   863,   864,   748,   313,   867,   871,   879,   744,   865,   866,
   883,   898,   760,   880,  1122,   893,   905,   907,   899,   902,
   906,   908,   909,   911,   910,   912,   915,   918,   919,  1132,
   920,   265,   928,   932,   245,   922,   927,   935,   929,   937,
   934,   936,   938,   939,   940,  1139,   941,   942,   944,   945,
   960,   961,   962,   963,   964,   965,  1111,   967,   966,   970,
   971,   975,   976,   977,   974,   978,   981,   982,    82,   986,
   983,   725,   696,   736,   985,  1148,   987,   988,   989,  1327,
   990,   992,   993,   994,   995,   996,   997,   998,   999,  1149,
  1000,  1001,  1002,  1003,  1153,  1004,  1005,  1006,  1007,   732,
  1008,  1009,   764,  1010,  1011,  1012,  1013,  1014,  1015,  1016,
  1017,  1018,  1019,  1020,  1021,  1150,  1022,  1023,  1024,  1042,
  1025,  1027,  1028,   273,  1029,  1030,  1032,  1033,   249,  1034,
  1035,  1036,  1037,  1038,  1039,  1040,  1154,  1041,  1045,  1048,
  1118,  1049,  1056,  1057,  1058,   784,  1060,  1061,  1130,  1062,
  1063,  1064,  1065,  1131,  1066,  1067,  1068,   263,  1069,  1122,
  1070,  1071,  1072,   261,  1073,  1074,  1075,  1076,  1077,  1078,
  1213,  1214,  1079,  1080,  1081,  1082,  1083,  1086,  1087,  1088,
  1089,  1090,  1091,  1092,  1221,  1093,  1224,  1225,  1094,  1095,
   257,  1096,  1097,  1098,  1099,  1100,  1340,  1101,  1102,  1103,
  1133,  1235,  1106,  1124,  1134,  1135,  1141,  1147,  1152,  1155,
  1156,  1159,  1160,   247,   267,   269,   271,  1161,   259,  1162,
  1163,  1169,  1164,  1165,  1166,  1167,  1258,  1168,  1170,  1173,
  1174,  1175,  1176,   769,  1177,  1178,  1199,  1179,  1182,  1212,
  1184,  1185,  1186,  1267,  1187,  1188,  1189,  1190,  1226,  1191,
  1192,  1193,  1194,  1195,  1196,  1206,  1197,  1198,  1201,  1208,
  1207,  1209,  1216,  1222,  1227,  1228,  1229,  1230,  1288,  1271,
   773,  1231,  1284,  1232,  1234,  1236,   780,  1237,  1238,  1239,
  1240,   789,  1241,  1242,  1244,  1245,   843,  1246,  1247,  1248,
  1249,  1252,  1265,  1253,  1257,  1259,  1268,  1314,  1269,  1223,
  1270,  1272,  1282,  1286,  1273,  1274,   251,  1275,  1276,  1328,
  1280,  1281,  1287,  1285,  1289,  1290,  1291,  1334,  1292,  1296,
  1295,  1298,  1307,  1299,  1305,   255,  1306,  1315,   253,  1308,
  1309,   277,  1312,  1324,   714,  1325,  1331,  1332,  1264,  1333,
  1337,   279,   283,   828,   839,  1205,     0,     0,     0,   281,
   275,     0,     0,   700,     0,     0,     0,     0,     0,   813,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   287,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   285
};

static const short yycheck[] = {     7,
   702,   703,   704,   314,   778,   308,   722,   723,   308,   711,
   308,   308,   308,   308,   716,   717,    87,   308,   720,   142,
   308,   142,   148,   149,   142,   148,   728,   148,   706,   707,
   708,   709,   710,     3,     7,     3,   738,   739,     4,   142,
     6,     5,     4,  1318,     6,     4,   149,     6,     4,   142,
     6,     5,  1327,   755,     4,   148,     6,     5,    24,    25,
    26,    27,    28,    29,    30,    31,    32,   148,   149,   148,
   149,   148,   149,   775,    33,    34,    35,    36,    37,    38,
    39,    40,   798,   799,   142,   148,   149,   147,   766,   148,
   148,   149,   794,    55,    56,   797,   148,     4,   800,   801,
   802,   148,   804,     5,   806,   807,   808,   809,    58,    59,
    60,    61,    62,    63,    64,    65,    66,    67,    68,   821,
   822,     5,   148,   825,   148,   803,   293,   294,   295,   296,
   297,   298,   299,   300,   301,   302,   303,   304,   305,   306,
   307,   308,   309,   310,     5,   148,   149,   117,   118,   119,
   120,   121,   122,   123,   124,   125,   126,   127,   128,   148,
   130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
   140,   148,   145,   146,   148,   145,   146,   145,   146,   145,
   146,   145,   146,   145,   146,   141,   145,   146,   143,   145,
   146,   145,   146,   895,   149,   145,   146,   145,   146,   901,
   142,   148,   369,   148,   149,     4,   148,     6,   148,     4,
   148,     6,     4,   148,     6,     5,     4,   148,     6,   148,
   387,     9,    10,    11,    12,    24,    25,    26,    27,    28,
    29,    30,    31,    32,     5,     4,   148,     6,   145,   146,
   148,   149,   313,   145,   146,   412,    41,    42,    43,    44,
    45,    46,    47,    48,    49,    50,    51,    52,   148,   149,
   148,   145,   146,   430,    33,    34,    35,    36,    37,    38,
    39,    40,   148,   149,   441,   148,   149,    69,    70,    71,
    72,  1055,   148,   149,   145,   146,   453,   147,   990,   148,
   149,   148,     4,   148,     6,     5,   148,     4,   465,     6,
   148,   149,   142,   143,   148,   149,  1008,  1009,  1010,  1011,
  1012,  1013,     5,  1029,   481,   148,   149,   148,     5,   622,
  1022,     5,   622,  1025,   622,   622,   622,   622,  1030,   148,
   497,   622,  1034,  1049,   622,   148,     4,   148,     6,   506,
   148,     4,   142,     6,   142,  1047,   145,   146,   515,   142,
   145,   146,   148,   145,   146,   145,   146,   145,   146,   526,
  1062,    73,    74,   142,  1066,   148,   149,  1069,   148,   149,
   537,  1073,  1074,  1051,   145,   146,   145,   146,     4,   546,
     6,    88,    89,    90,    91,    92,    93,    94,    95,    96,
    97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
   107,     4,   142,     6,   148,   149,   148,   149,   575,   148,
   149,  1185,    75,    76,    77,    78,    79,    80,    81,    82,
    83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
    93,    94,    95,   145,   146,   145,   146,   142,   145,   146,
   142,   109,   110,   111,   112,   113,   114,   115,   116,   148,
   149,   142,   145,   146,     4,   622,     6,  1159,   145,   146,
   142,   145,   146,   148,   149,  1167,    69,    70,    71,    72,
   148,   149,  1174,  1175,     4,  1249,     6,   145,   146,  1181,
   142,   648,   145,   146,  1186,  1187,  1164,  1165,  1166,  1191,
   142,   142,     4,   142,     6,     4,   663,     6,   142,   142,
     9,    10,    11,    12,     4,  1207,     6,   142,    58,    59,
    60,    61,    62,    63,    64,    65,    66,    67,    68,   145,
   146,   142,   142,    23,   142,   142,  1228,   142,   142,   148,
  1232,   147,     4,   142,     6,  1237,     4,   148,     6,  1241,
  1242,   852,   145,   146,  1246,    75,    76,    77,    78,    79,
    80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
    90,    91,    92,    93,    94,    95,     4,   148,     6,    41,
    42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
    52,   148,     4,   148,     6,     5,   148,    55,    56,     4,
  1292,     6,  1294,  1295,     5,   145,   146,   109,   110,   111,
   112,   113,   114,   115,   116,     4,  1308,     6,   148,  1311,
  1312,     4,   148,     6,   148,   145,   146,     5,    17,    18,
    19,    20,    21,     5,    17,    18,    19,    20,    21,     5,
     4,  1333,     6,   145,   146,    57,   145,   146,    53,     4,
   148,     6,     4,   148,     6,   145,   146,     4,   147,     6,
    88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
    98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     4,   148,     6,   145,   146,   683,     5,   145,   146,     4,
   147,     6,     5,     4,   148,     6,   148,   148,    22,    54,
     4,   148,     6,     4,   148,     6,     5,   148,    23,    73,
    74,    22,     4,   148,     6,     5,   148,   145,   146,   148,
     4,   719,     6,     4,   148,     6,     4,   148,     6,   148,
     4,   729,     6,   145,   146,   145,   146,   148,   147,   142,
   145,   146,   148,   147,   145,   146,   148,   147,   147,    53,
   147,   147,   147,    54,   752,   753,   145,   146,   756,   147,
   147,   108,   145,   146,   762,    57,   147,   145,   146,   148,
   147,   147,   147,   145,   146,   148,   147,   147,   776,   145,
   146,   145,   146,   148,   147,   147,   147,   147,   147,   141,
   145,   146,   148,   145,   146,     8,   147,   147,   145,   146,
    13,    14,    15,    16,   147,   147,   147,   805,     8,   147,
   147,   147,   810,    13,    14,    15,    16,   147,   147,   147,
   147,   145,   146,   148,   108,   147,   145,   146,   826,   147,
   145,   146,   145,   146,   145,   146,   834,   835,   147,   147,
   147,   145,   146,   841,   145,   146,   145,   146,   148,   147,
   147,   147,   147,   145,   146,   145,   146,   148,   147,   147,
   147,   145,   146,   148,   145,   146,   148,   145,   146,   148,
   147,   145,   146,   117,   118,   119,   120,   121,   122,   123,
   124,   125,   126,   127,   128,   147,   130,   131,   132,   133,
   134,   135,   136,   137,   138,   139,   140,   121,   122,   123,
   124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
   134,   135,   136,   137,   138,   139,   140,   147,   147,   147,
   147,   147,   147,   142,   147,   147,   147,   142,   148,   147,
   328,   148,   145,   146,   148,   147,   147,   147,   147,   147,
   142,   148,   147,   147,   147,   145,   146,   148,   147,   147,
   147,   147,   147,   147,   147,   147,   147,   147,   147,   147,
   147,   147,   147,   147,   147,   147,   147,   147,     0,   148,
   147,   147,   147,   147,   147,   147,   147,   147,   147,   147,
   147,   147,     4,   148,   147,   147,   147,   147,   147,   147,
   147,   147,   318,   148,   147,   142,   148,   147,   149,   148,
   148,   142,   142,   142,   142,   142,   142,   142,   142,   142,
   142,   142,   465,   289,   142,   142,   142,   453,   148,   148,
   142,   142,   481,   148,  1022,   148,   142,   142,   148,   148,
   143,   148,   143,   142,   148,   142,   142,   142,   142,  1037,
   148,   153,   143,   142,    93,   148,   148,   143,   148,   142,
   148,   148,   143,   148,   142,  1053,   142,   142,   142,   142,
   142,   148,   142,   142,   142,   142,   147,   142,   144,   142,
   142,   142,   142,   142,   148,   142,   142,   142,    32,   142,
   148,   412,   350,   441,   148,   143,   148,   148,   148,  1315,
   149,   148,   148,   148,   148,   148,   148,   148,   148,  1097,
   148,   148,   148,   148,  1102,   142,   148,   148,   148,   430,
   149,   149,   497,   149,   149,   149,   149,   148,   148,   148,
   148,   148,   148,   148,   148,   143,   149,   148,   148,   142,
   149,   148,   148,   177,   149,   149,   148,   148,   105,   149,
   149,   149,   149,   148,   148,   148,   143,   149,   148,   148,
   142,   149,   148,   148,   148,   537,   149,   148,   142,   149,
   148,   148,   148,   142,   149,   148,   148,   147,   149,  1167,
   148,   148,   148,   141,   149,   149,   148,   148,   148,   148,
  1178,  1179,   148,   148,   148,   148,   148,   148,   148,   148,
   148,   148,   148,   148,  1192,   148,  1194,  1195,   148,   148,
   129,   149,   149,   149,   148,   148,     0,   149,   149,   148,
   142,  1209,   149,   149,   148,   142,   142,   142,   142,   142,
   142,   149,   142,    99,   159,   165,   171,   148,   135,   148,
   148,   142,   149,   149,   149,   149,  1234,   149,   148,   142,
   149,   149,   148,   506,   149,   149,   142,   149,   148,   143,
   149,   149,   149,  1251,   149,   148,   148,   148,   143,   149,
   149,   149,   149,   149,   148,   142,   149,   149,   148,   142,
   149,   149,   142,   142,   142,   149,   148,   148,  1276,   143,
   515,   148,   143,   149,   149,   148,   526,   149,   148,   148,
   148,   546,   149,   149,   148,   148,   663,   149,   149,   149,
   149,   148,   142,   149,   149,   148,   142,  1305,   149,   143,
   149,   148,   142,   142,   149,   148,   111,   149,   149,  1317,
   148,   148,   142,   149,   149,   149,   143,  1325,   149,   143,
   149,   143,   143,   149,   149,   123,   149,   143,   117,   149,
   148,   189,   149,   148,   387,   149,   148,   148,  1247,   149,
   148,   195,   207,   622,   648,  1167,    -1,    -1,    -1,   201,
   183,    -1,    -1,   369,    -1,    -1,    -1,    -1,    -1,   575,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,   219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,   213
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/opt/bison/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/opt/bison/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 332 "vex.y"
{vex_ptr=make_vex(yyvsp[-1].llptr,yyvsp[0].llptr);;
    break;}
case 2:
#line 333 "vex.y"
{vex_ptr=make_vex(yyvsp[0].llptr,NULL);;
    break;}
case 3:
#line 335 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 4:
#line 336 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 5:
#line 338 "vex.y"
{yyval.lwptr=make_lowl(T_VEX_REV,yyvsp[0].dvptr);;
    break;}
case 6:
#line 339 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 7:
#line 340 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 8:
#line 344 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 9:
#line 349 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].blptr);;
    break;}
case 10:
#line 350 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].blptr);;
    break;}
case 11:
#line 352 "vex.y"
{yyval.blptr=make_block(B_GLOBAL,yyvsp[0].llptr);;
    break;}
case 12:
#line 353 "vex.y"
{yyval.blptr=make_block(B_STATION,yyvsp[0].llptr);;
    break;}
case 13:
#line 354 "vex.y"
{yyval.blptr=make_block(B_MODE,yyvsp[0].llptr);;
    break;}
case 14:
#line 355 "vex.y"
{yyval.blptr=make_block(B_FREQ,yyvsp[0].llptr);;
    break;}
case 15:
#line 356 "vex.y"
{yyval.blptr=make_block(B_SCHED,yyvsp[0].llptr);;
    break;}
case 16:
#line 357 "vex.y"
{yyval.blptr=make_block(B_ANTENNA,yyvsp[0].llptr);;
    break;}
case 17:
#line 358 "vex.y"
{yyval.blptr=make_block(B_BBC,yyvsp[0].llptr);;
    break;}
case 18:
#line 359 "vex.y"
{yyval.blptr=make_block(B_CLOCK,yyvsp[0].llptr);;
    break;}
case 19:
#line 360 "vex.y"
{yyval.blptr=make_block(B_DAS,yyvsp[0].llptr);;
    break;}
case 20:
#line 361 "vex.y"
{yyval.blptr=make_block(B_EOP,yyvsp[0].llptr);;
    break;}
case 21:
#line 362 "vex.y"
{yyval.blptr=make_block(B_EXPER,yyvsp[0].llptr);;
    break;}
case 22:
#line 363 "vex.y"
{yyval.blptr=make_block(B_HEAD_POS,yyvsp[0].llptr);;
    break;}
case 23:
#line 364 "vex.y"
{yyval.blptr=make_block(B_IF,yyvsp[0].llptr);;
    break;}
case 24:
#line 365 "vex.y"
{yyval.blptr=make_block(B_PASS_ORDER,yyvsp[0].llptr);;
    break;}
case 25:
#line 366 "vex.y"
{yyval.blptr=make_block(B_PHASE_CAL_DETECT,yyvsp[0].llptr);;
    break;}
case 26:
#line 367 "vex.y"
{yyval.blptr=make_block(B_PROCEDURES,yyvsp[0].llptr);;
    break;}
case 27:
#line 368 "vex.y"
{yyval.blptr=make_block(B_ROLL,yyvsp[0].llptr);;
    break;}
case 28:
#line 370 "vex.y"
{yyval.blptr=make_block(B_SCHEDULING_PARAMS,yyvsp[0].llptr);;
    break;}
case 29:
#line 371 "vex.y"
{yyval.blptr=make_block(B_SEFD,yyvsp[0].llptr);;
    break;}
case 30:
#line 372 "vex.y"
{yyval.blptr=make_block(B_SITE,yyvsp[0].llptr);;
    break;}
case 31:
#line 373 "vex.y"
{yyval.blptr=make_block(B_SOURCE,yyvsp[0].llptr);;
    break;}
case 32:
#line 374 "vex.y"
{yyval.blptr=make_block(B_TAPELOG_OBS,yyvsp[0].llptr);;
    break;}
case 33:
#line 375 "vex.y"
{yyval.blptr=make_block(B_TRACKS,yyvsp[0].llptr);;
    break;}
case 34:
#line 379 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 35:
#line 380 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 36:
#line 384 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 37:
#line 385 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 38:
#line 387 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 39:
#line 388 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 40:
#line 390 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 41:
#line 391 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 42:
#line 392 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 43:
#line 394 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 44:
#line 395 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 45:
#line 399 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 46:
#line 400 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 47:
#line 402 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 48:
#line 403 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 49:
#line 405 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 50:
#line 406 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 51:
#line 407 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 52:
#line 409 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 53:
#line 411 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 54:
#line 415 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 55:
#line 416 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 56:
#line 418 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].qrptr);;
    break;}
case 57:
#line 419 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 58:
#line 420 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 59:
#line 422 "vex.y"
{yyval.qrptr=make_qref(yyvsp[-3].ival,yyvsp[-1].sval,NULL);;
    break;}
case 60:
#line 424 "vex.y"
{yyval.ival=B_EXPER;;
    break;}
case 61:
#line 425 "vex.y"
{yyval.ival=B_SCHEDULING_PARAMS;;
    break;}
case 62:
#line 426 "vex.y"
{yyval.ival=B_PROCEDURES;;
    break;}
case 63:
#line 427 "vex.y"
{yyval.ival=B_EOP;;
    break;}
case 64:
#line 428 "vex.y"
{yyval.ival=B_FREQ;;
    break;}
case 65:
#line 429 "vex.y"
{yyval.ival=B_ANTENNA;;
    break;}
case 66:
#line 430 "vex.y"
{yyval.ival=B_BBC;;
    break;}
case 67:
#line 431 "vex.y"
{yyval.ival=B_CLOCK;;
    break;}
case 68:
#line 432 "vex.y"
{yyval.ival=B_CORR;;
    break;}
case 69:
#line 433 "vex.y"
{yyval.ival=B_DAS;;
    break;}
case 70:
#line 434 "vex.y"
{yyval.ival=B_HEAD_POS;;
    break;}
case 71:
#line 435 "vex.y"
{yyval.ival=B_PASS_ORDER;;
    break;}
case 72:
#line 436 "vex.y"
{yyval.ival=B_PHASE_CAL_DETECT;;
    break;}
case 73:
#line 437 "vex.y"
{yyval.ival=B_ROLL;;
    break;}
case 74:
#line 438 "vex.y"
{yyval.ival=B_IF;;
    break;}
case 75:
#line 439 "vex.y"
{yyval.ival=B_SEFD;;
    break;}
case 76:
#line 440 "vex.y"
{yyval.ival=B_SITE;;
    break;}
case 77:
#line 441 "vex.y"
{yyval.ival=B_SOURCE;;
    break;}
case 78:
#line 442 "vex.y"
{yyval.ival=B_TRACKS;;
    break;}
case 79:
#line 443 "vex.y"
{yyval.ival=B_TAPELOG_OBS;;
    break;}
case 80:
#line 445 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 81:
#line 446 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 82:
#line 448 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].qrptr);;
    break;}
case 83:
#line 449 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 84:
#line 450 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 85:
#line 452 "vex.y"
{yyval.qrptr=make_qref(yyvsp[-4].ival,yyvsp[-2].sval,yyvsp[-1].llptr);;
    break;}
case 86:
#line 453 "vex.y"
{yyval.qrptr=make_qref(yyvsp[-3].ival,yyvsp[-1].sval,NULL);;
    break;}
case 87:
#line 455 "vex.y"
{yyval.llptr=add_list(yyvsp[-2].llptr,yyvsp[0].sval);;
    break;}
case 88:
#line 456 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].sval);;
    break;}
case 89:
#line 460 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 90:
#line 461 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 91:
#line 463 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 92:
#line 464 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 93:
#line 466 "vex.y"
{yyval.lwptr=make_lowl(T_SCAN,yyvsp[0].dfptr);;
    break;}
case 94:
#line 467 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 95:
#line 468 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 96:
#line 471 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 97:
#line 472 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 98:
#line 474 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 99:
#line 475 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 100:
#line 477 "vex.y"
{yyval.lwptr=make_lowl(T_START,yyvsp[0].sval);;
    break;}
case 101:
#line 478 "vex.y"
{yyval.lwptr=make_lowl(T_MODE,yyvsp[0].sval);;
    break;}
case 102:
#line 479 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE,yyvsp[0].sval);;
    break;}
case 103:
#line 480 "vex.y"
{yyval.lwptr=make_lowl(T_STATION,yyvsp[0].snptr);;
    break;}
case 104:
#line 481 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 105:
#line 482 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 106:
#line 484 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 107:
#line 486 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 108:
#line 488 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 109:
#line 497 "vex.y"
{yyval.snptr=make_station(yyvsp[-13].sval,yyvsp[-11].dvptr,yyvsp[-9].dvptr,yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].sval,yyvsp[-1].llptr);;
    break;}
case 110:
#line 499 "vex.y"
{yyval.dvptr=NULL;;
    break;}
case 111:
#line 500 "vex.y"
{yyval.dvptr=yyvsp[0].dvptr;;
    break;}
case 112:
#line 502 "vex.y"
{yyval.sval=NULL;;
    break;}
case 113:
#line 503 "vex.y"
{yyval.sval=yyvsp[0].sval;;
    break;}
case 114:
#line 505 "vex.y"
{yyval.sval=NULL;;
    break;}
case 115:
#line 506 "vex.y"
{yyval.sval=yyvsp[0].sval;;
    break;}
case 116:
#line 508 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 117:
#line 509 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].dvptr);;
    break;}
case 118:
#line 510 "vex.y"
{yyval.llptr=add_list(add_list(NULL,yyvsp[-2].dvptr),yyvsp[0].dvptr);;
    break;}
case 119:
#line 514 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 120:
#line 515 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 121:
#line 517 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 122:
#line 518 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 123:
#line 520 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 124:
#line 521 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 125:
#line 522 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 126:
#line 525 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 127:
#line 526 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 128:
#line 528 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 129:
#line 529 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 130:
#line 531 "vex.y"
{yyval.lwptr=make_lowl(T_ANTENNA_DIAM,yyvsp[0].dvptr);;
    break;}
case 131:
#line 532 "vex.y"
{yyval.lwptr=make_lowl(T_AXIS_TYPE,yyvsp[0].atptr);;
    break;}
case 132:
#line 533 "vex.y"
{yyval.lwptr=make_lowl(T_AXIS_OFFSET,yyvsp[0].dvptr);;
    break;}
case 133:
#line 534 "vex.y"
{yyval.lwptr=make_lowl(T_ANTENNA_MOTION,yyvsp[0].amptr);;
    break;}
case 134:
#line 535 "vex.y"
{yyval.lwptr=make_lowl(T_POINTING_SECTOR,yyvsp[0].psptr);;
    break;}
case 135:
#line 536 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 136:
#line 537 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 137:
#line 538 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 138:
#line 540 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 139:
#line 543 "vex.y"
{yyval.atptr=make_axis_type(yyvsp[-3].sval,yyvsp[-1].sval);;
    break;}
case 140:
#line 545 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 141:
#line 550 "vex.y"
{yyval.amptr=make_antenna_motion(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 142:
#line 559 "vex.y"
{yyval.psptr=make_pointing_sector(yyvsp[-13].sval,yyvsp[-11].sval,yyvsp[-9].dvptr,yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 143:
#line 563 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 144:
#line 564 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 145:
#line 566 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 146:
#line 567 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 147:
#line 569 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 148:
#line 570 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 149:
#line 571 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 150:
#line 573 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 151:
#line 575 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 152:
#line 577 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 153:
#line 578 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 154:
#line 580 "vex.y"
{yyval.lwptr=make_lowl(T_BBC_ASSIGN,yyvsp[0].baptr);;
    break;}
case 155:
#line 581 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 156:
#line 582 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 157:
#line 583 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 158:
#line 586 "vex.y"
{yyval.baptr=make_bbc_assign(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].sval);;
    break;}
case 159:
#line 590 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 160:
#line 591 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 161:
#line 593 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 162:
#line 594 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 163:
#line 596 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 164:
#line 597 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 165:
#line 598 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 166:
#line 601 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 167:
#line 603 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 168:
#line 605 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 169:
#line 606 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 170:
#line 608 "vex.y"
{yyval.lwptr=make_lowl(T_CLOCK_EARLY,yyvsp[0].ceptr);;
    break;}
case 171:
#line 609 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 172:
#line 610 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 173:
#line 611 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 174:
#line 614 "vex.y"
{yyval.ceptr=make_clock_early(NULL,yyvsp[-1].dvptr,NULL,NULL);;
    break;}
case 175:
#line 616 "vex.y"
{yyval.ceptr=make_clock_early(yyvsp[-3].sval,yyvsp[-1].dvptr,NULL,NULL);;
    break;}
case 176:
#line 618 "vex.y"
{yyval.ceptr=make_clock_early(yyvsp[-7].sval,yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 177:
#line 620 "vex.y"
{yyval.ceptr=make_clock_early(NULL,yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 178:
#line 624 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 179:
#line 625 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 180:
#line 627 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 181:
#line 628 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 182:
#line 630 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 183:
#line 631 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 184:
#line 632 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 185:
#line 634 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 186:
#line 636 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 187:
#line 638 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 188:
#line 639 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 189:
#line 641 "vex.y"
{yyval.lwptr=make_lowl(T_RECORD_TRANSPORT_TYPE,yyvsp[0].sval);;
    break;}
case 190:
#line 642 "vex.y"
{yyval.lwptr=make_lowl(T_ELECTRONICS_RACK_TYPE,yyvsp[0].sval);;
    break;}
case 191:
#line 643 "vex.y"
{yyval.lwptr=make_lowl(T_NUMBER_DRIVES,yyvsp[0].dvptr);;
    break;}
case 192:
#line 644 "vex.y"
{yyval.lwptr=make_lowl(T_HEADSTACK,yyvsp[0].hsptr);;
    break;}
case 193:
#line 645 "vex.y"
{yyval.lwptr=make_lowl(T_RECORD_DENSITY,yyvsp[0].dvptr);;
    break;}
case 194:
#line 646 "vex.y"
{yyval.lwptr=make_lowl(T_TAPE_LENGTH,yyvsp[0].tlptr);;
    break;}
case 195:
#line 648 "vex.y"
{yyval.lwptr=make_lowl(T_RECORDING_SYSTEM_ID,yyvsp[0].dvptr);;
    break;}
case 196:
#line 649 "vex.y"
{yyval.lwptr=make_lowl(T_TAPE_MOTION,yyvsp[0].tmptr);;
    break;}
case 197:
#line 650 "vex.y"
{yyval.lwptr=make_lowl(T_TAPE_CONTROL,yyvsp[0].sval);;
    break;}
case 198:
#line 651 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 199:
#line 652 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 200:
#line 653 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 201:
#line 655 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 202:
#line 657 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 203:
#line 659 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 204:
#line 662 "vex.y"
{yyval.hsptr=make_headstack(yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 205:
#line 665 "vex.y"
{yyval.dvptr=make_dvalue(yyvsp[-2].sval,yyvsp[-1].sval);;
    break;}
case 206:
#line 668 "vex.y"
{yyval.tlptr=make_tape_length(yyvsp[-1].dvptr,NULL,NULL);;
    break;}
case 207:
#line 670 "vex.y"
{yyval.tlptr=make_tape_length(yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 208:
#line 672 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 209:
#line 675 "vex.y"
{yyval.tmptr=make_tape_motion(yyvsp[-1].sval,NULL,NULL,NULL);;
    break;}
case 210:
#line 677 "vex.y"
{yyval.tmptr=make_tape_motion(yyvsp[-3].sval,yyvsp[-1].dvptr,NULL,NULL);;
    break;}
case 211:
#line 680 "vex.y"
{yyval.tmptr=make_tape_motion(yyvsp[-7].sval,yyvsp[-5].dvptr,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 212:
#line 682 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 213:
#line 686 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 214:
#line 687 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 215:
#line 689 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 216:
#line 690 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 217:
#line 692 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 218:
#line 693 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 219:
#line 694 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 220:
#line 696 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 221:
#line 698 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 222:
#line 700 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 223:
#line 701 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 224:
#line 703 "vex.y"
{yyval.lwptr=make_lowl(T_TAI_UTC,yyvsp[0].dvptr);;
    break;}
case 225:
#line 704 "vex.y"
{yyval.lwptr=make_lowl(T_A1_TAI,yyvsp[0].dvptr);;
    break;}
case 226:
#line 705 "vex.y"
{yyval.lwptr=make_lowl(T_EOP_REF_EPOCH,yyvsp[0].sval);;
    break;}
case 227:
#line 706 "vex.y"
{yyval.lwptr=make_lowl(T_NUM_EOP_POINTS,yyvsp[0].dvptr);;
    break;}
case 228:
#line 707 "vex.y"
{yyval.lwptr=make_lowl(T_EOP_INTERVAL,yyvsp[0].dvptr);;
    break;}
case 229:
#line 708 "vex.y"
{yyval.lwptr=make_lowl(T_UT1_UTC,yyvsp[0].llptr);;
    break;}
case 230:
#line 709 "vex.y"
{yyval.lwptr=make_lowl(T_X_WOBBLE,yyvsp[0].llptr);;
    break;}
case 231:
#line 710 "vex.y"
{yyval.lwptr=make_lowl(T_Y_WOBBLE,yyvsp[0].llptr);;
    break;}
case 232:
#line 711 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 233:
#line 712 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 234:
#line 713 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 235:
#line 715 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 236:
#line 717 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 237:
#line 719 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 238:
#line 721 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 239:
#line 723 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 240:
#line 725 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 241:
#line 726 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 242:
#line 728 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 243:
#line 729 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 244:
#line 731 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 245:
#line 732 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 246:
#line 736 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 247:
#line 737 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 248:
#line 739 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 249:
#line 740 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 250:
#line 742 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 251:
#line 743 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 252:
#line 744 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 253:
#line 747 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 254:
#line 748 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 255:
#line 750 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 256:
#line 751 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 257:
#line 753 "vex.y"
{yyval.lwptr=make_lowl(T_EXPER_NUM,yyvsp[0].dvptr);;
    break;}
case 258:
#line 754 "vex.y"
{yyval.lwptr=make_lowl(T_EXPER_NAME,yyvsp[0].sval);;
    break;}
case 259:
#line 755 "vex.y"
{yyval.lwptr=make_lowl(T_EXPER_DESCRIPTION,yyvsp[0].sval);;
    break;}
case 260:
#line 757 "vex.y"
{yyval.lwptr=make_lowl(T_EXPER_NOMINAL_START,yyvsp[0].sval);;
    break;}
case 261:
#line 759 "vex.y"
{yyval.lwptr=make_lowl(T_EXPER_NOMINAL_STOP,yyvsp[0].sval);;
    break;}
case 262:
#line 760 "vex.y"
{yyval.lwptr=make_lowl(T_PI_NAME,yyvsp[0].sval);;
    break;}
case 263:
#line 761 "vex.y"
{yyval.lwptr=make_lowl(T_PI_EMAIL,yyvsp[0].sval);;
    break;}
case 264:
#line 762 "vex.y"
{yyval.lwptr=make_lowl(T_CONTACT_NAME,yyvsp[0].sval);;
    break;}
case 265:
#line 763 "vex.y"
{yyval.lwptr=make_lowl(T_CONTACT_EMAIL,yyvsp[0].sval);;
    break;}
case 266:
#line 764 "vex.y"
{yyval.lwptr=make_lowl(T_SCHEDULER_NAME,yyvsp[0].sval);;
    break;}
case 267:
#line 765 "vex.y"
{yyval.lwptr=make_lowl(T_SCHEDULER_EMAIL,yyvsp[0].sval);;
    break;}
case 268:
#line 767 "vex.y"
{yyval.lwptr=make_lowl(T_TARGET_CORRELATOR,yyvsp[0].sval);;
    break;}
case 269:
#line 768 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 270:
#line 769 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 271:
#line 770 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 272:
#line 772 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 273:
#line 774 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 274:
#line 776 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 275:
#line 778 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 276:
#line 780 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 277:
#line 782 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 278:
#line 784 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 279:
#line 786 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 280:
#line 788 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 281:
#line 790 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 282:
#line 792 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 283:
#line 794 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 284:
#line 798 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 285:
#line 799 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 286:
#line 801 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 287:
#line 802 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 288:
#line 804 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 289:
#line 805 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 290:
#line 806 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 291:
#line 808 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 292:
#line 810 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 293:
#line 812 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 294:
#line 813 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 295:
#line 815 "vex.y"
{yyval.lwptr=make_lowl(T_CHAN_DEF,yyvsp[0].cdptr);;
    break;}
case 296:
#line 816 "vex.y"
{yyval.lwptr=make_lowl(T_SAMPLE_RATE,yyvsp[0].dvptr);;
    break;}
case 297:
#line 817 "vex.y"
{yyval.lwptr=make_lowl(T_BITS_PER_SAMPLE,yyvsp[0].dvptr);;
    break;}
case 298:
#line 818 "vex.y"
{yyval.lwptr=make_lowl(T_SWITCHING_CYCLE,yyvsp[0].scptr);;
    break;}
case 299:
#line 819 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 300:
#line 820 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 301:
#line 821 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 302:
#line 830 "vex.y"
{yyval.cdptr=make_chan_def(yyvsp[-13].sval,yyvsp[-11].dvptr,yyvsp[-9].sval,yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].sval,yyvsp[-1].sval,NULL);;
    break;}
case 303:
#line 838 "vex.y"
{yyval.cdptr=make_chan_def(yyvsp[-14].sval,yyvsp[-12].dvptr,yyvsp[-10].sval,yyvsp[-8].dvptr,yyvsp[-6].sval,yyvsp[-4].sval,yyvsp[-2].sval,yyvsp[-1].llptr);;
    break;}
case 304:
#line 846 "vex.y"
{yyval.cdptr=make_chan_def(NULL,yyvsp[-11].dvptr,yyvsp[-9].sval,yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].sval,yyvsp[-1].sval,NULL);;
    break;}
case 305:
#line 854 "vex.y"
{yyval.cdptr=make_chan_def(NULL,yyvsp[-12].dvptr,yyvsp[-10].sval,yyvsp[-8].dvptr,yyvsp[-6].sval,yyvsp[-4].sval,yyvsp[-2].sval,yyvsp[-1].llptr);;
    break;}
case 306:
#line 856 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].dvptr);;
    break;}
case 307:
#line 857 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].dvptr);;
    break;}
case 308:
#line 859 "vex.y"
{yyval.dvptr=yyvsp[0].dvptr;;
    break;}
case 309:
#line 861 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 310:
#line 863 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 311:
#line 866 "vex.y"
{yyval.scptr=make_switching_cycle(yyvsp[-3].sval,yyvsp[-1].llptr);;
    break;}
case 312:
#line 870 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 313:
#line 871 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 314:
#line 873 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 315:
#line 874 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 316:
#line 876 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 317:
#line 877 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 318:
#line 878 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 319:
#line 881 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 320:
#line 883 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 321:
#line 885 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 322:
#line 886 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 323:
#line 888 "vex.y"
{yyval.lwptr=make_lowl(T_HEADSTACK_POS,yyvsp[0].hpptr);;
    break;}
case 324:
#line 889 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 325:
#line 890 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 326:
#line 891 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 327:
#line 894 "vex.y"
{yyval.hpptr=make_headstack_pos(yyvsp[-3].dvptr,yyvsp[-1].llptr);;
    break;}
case 328:
#line 898 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 329:
#line 899 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 330:
#line 901 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 331:
#line 902 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 332:
#line 904 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 333:
#line 905 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 334:
#line 906 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 335:
#line 908 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 336:
#line 910 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 337:
#line 912 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 338:
#line 913 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 339:
#line 915 "vex.y"
{yyval.lwptr=make_lowl(T_IF_DEF,yyvsp[0].ifptr);;
    break;}
case 340:
#line 916 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 341:
#line 917 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 342:
#line 918 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 343:
#line 921 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-13].sval,yyvsp[-11].sval,yyvsp[-9].sval,yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 344:
#line 923 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-9].sval,yyvsp[-7].sval,yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].sval,NULL,NULL);;
    break;}
case 345:
#line 925 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-11].sval,yyvsp[-9].sval,yyvsp[-7].sval,yyvsp[-5].dvptr,yyvsp[-3].sval,NULL,NULL);;
    break;}
case 346:
#line 927 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-10].sval,yyvsp[-8].sval,yyvsp[-6].sval,yyvsp[-4].dvptr,yyvsp[-2].sval,NULL,NULL);;
    break;}
case 347:
#line 929 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-11].sval,yyvsp[-9].sval,yyvsp[-7].sval,yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr,NULL);;
    break;}
case 348:
#line 931 "vex.y"
{yyval.ifptr=make_if_def(yyvsp[-12].sval,yyvsp[-10].sval,yyvsp[-8].sval,yyvsp[-6].dvptr,yyvsp[-4].sval,yyvsp[-2].dvptr,NULL);;
    break;}
case 349:
#line 935 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 350:
#line 936 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 351:
#line 938 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 352:
#line 940 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 353:
#line 942 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 354:
#line 943 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 355:
#line 944 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 356:
#line 947 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 357:
#line 949 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 358:
#line 952 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 359:
#line 953 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 360:
#line 955 "vex.y"
{yyval.lwptr=make_lowl(T_PASS_ORDER,yyvsp[0].llptr);;
    break;}
case 361:
#line 957 "vex.y"
{yyval.lwptr=make_lowl(T_S2_GROUP_ORDER,yyvsp[0].llptr);;
    break;}
case 362:
#line 958 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 363:
#line 959 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 364:
#line 960 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 365:
#line 962 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 366:
#line 964 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 367:
#line 968 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 368:
#line 969 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 369:
#line 972 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 370:
#line 973 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 371:
#line 975 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 372:
#line 976 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 373:
#line 977 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 374:
#line 980 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 375:
#line 981 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 376:
#line 984 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 377:
#line 985 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 378:
#line 987 "vex.y"
{yyval.lwptr=make_lowl(T_PHASE_CAL_DETECT,yyvsp[0].pdptr);;
    break;}
case 379:
#line 988 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 380:
#line 989 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 381:
#line 990 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 382:
#line 993 "vex.y"
{yyval.pdptr=make_phase_cal_detect(yyvsp[-3].sval,yyvsp[-1].llptr);;
    break;}
case 383:
#line 995 "vex.y"
{yyval.pdptr=make_phase_cal_detect(yyvsp[-1].sval,NULL);;
    break;}
case 384:
#line 999 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 385:
#line 1000 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 386:
#line 1003 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 387:
#line 1004 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 388:
#line 1006 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 389:
#line 1007 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 390:
#line 1008 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 391:
#line 1011 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 392:
#line 1013 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 393:
#line 1016 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 394:
#line 1017 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 395:
#line 1020 "vex.y"
{yyval.lwptr=make_lowl(T_TAPE_CHANGE,yyvsp[0].dvptr);;
    break;}
case 396:
#line 1022 "vex.y"
{yyval.lwptr=make_lowl(T_HEADSTACK_MOTION,yyvsp[0].dvptr);;
    break;}
case 397:
#line 1024 "vex.y"
{yyval.lwptr=make_lowl(T_NEW_SOURCE_COMMAND,yyvsp[0].dvptr);;
    break;}
case 398:
#line 1026 "vex.y"
{yyval.lwptr=make_lowl(T_NEW_TAPE_SETUP,yyvsp[0].dvptr);;
    break;}
case 399:
#line 1028 "vex.y"
{yyval.lwptr=make_lowl(T_SETUP_ALWAYS,yyvsp[0].saptr);;
    break;}
case 400:
#line 1030 "vex.y"
{yyval.lwptr=make_lowl(T_PARITY_CHECK,yyvsp[0].pcptr);;
    break;}
case 401:
#line 1032 "vex.y"
{yyval.lwptr=make_lowl(T_TAPE_PREPASS,yyvsp[0].tpptr);;
    break;}
case 402:
#line 1034 "vex.y"
{yyval.lwptr=make_lowl(T_PREOB_CAL,yyvsp[0].prptr);;
    break;}
case 403:
#line 1036 "vex.y"
{yyval.lwptr=make_lowl(T_MIDOB_CAL,yyvsp[0].miptr);;
    break;}
case 404:
#line 1038 "vex.y"
{yyval.lwptr=make_lowl(T_POSTOB_CAL,yyvsp[0].poptr);;
    break;}
case 405:
#line 1040 "vex.y"
{yyval.lwptr=make_lowl(T_PROCEDURE_NAME_PREFIX,yyvsp[0].sval);;
    break;}
case 406:
#line 1041 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 407:
#line 1042 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 408:
#line 1043 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 409:
#line 1045 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 410:
#line 1047 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 411:
#line 1049 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 412:
#line 1051 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 413:
#line 1054 "vex.y"
{yyval.saptr=make_setup_always(yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 414:
#line 1057 "vex.y"
{yyval.pcptr=make_parity_check(yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 415:
#line 1060 "vex.y"
{yyval.tpptr=make_tape_prepass(yyvsp[-3].sval,yyvsp[-1].dvptr);;
    break;}
case 416:
#line 1063 "vex.y"
{yyval.prptr=make_preob_cal(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].sval);;
    break;}
case 417:
#line 1066 "vex.y"
{yyval.miptr=make_midob_cal(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].sval);;
    break;}
case 418:
#line 1069 "vex.y"
{yyval.poptr=make_postob_cal(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].sval);;
    break;}
case 419:
#line 1071 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 420:
#line 1075 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 421:
#line 1076 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 422:
#line 1078 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 423:
#line 1079 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 424:
#line 1081 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 425:
#line 1082 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 426:
#line 1083 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 427:
#line 1086 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 428:
#line 1088 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 429:
#line 1090 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 430:
#line 1091 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 431:
#line 1093 "vex.y"
{yyval.lwptr=make_lowl(T_ROLL_REINIT_PERIOD,yyvsp[0].dvptr);;
    break;}
case 432:
#line 1094 "vex.y"
{yyval.lwptr=make_lowl(T_ROLL_INC_PERIOD,yyvsp[0].dvptr);;
    break;}
case 433:
#line 1095 "vex.y"
{yyval.lwptr=make_lowl(T_ROLL,yyvsp[0].sval);;
    break;}
case 434:
#line 1096 "vex.y"
{yyval.lwptr=make_lowl(T_ROLL_DEF,yyvsp[0].llptr);;
    break;}
case 435:
#line 1097 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 436:
#line 1098 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 437:
#line 1099 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 438:
#line 1101 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 439:
#line 1103 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 440:
#line 1105 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 441:
#line 1107 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 442:
#line 1112 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 443:
#line 1113 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 444:
#line 1116 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 445:
#line 1118 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 446:
#line 1120 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 447:
#line 1121 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 448:
#line 1122 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 449:
#line 1125 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 450:
#line 1127 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 451:
#line 1130 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 452:
#line 1132 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 453:
#line 1134 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 454:
#line 1135 "vex.y"
{yyval.lwptr=make_lowl(T_LITERAL,yyvsp[0].llptr);;
    break;}
case 455:
#line 1136 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 456:
#line 1137 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 457:
#line 1141 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 458:
#line 1142 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 459:
#line 1144 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 460:
#line 1145 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 461:
#line 1147 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 462:
#line 1148 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 463:
#line 1149 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 464:
#line 1152 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 465:
#line 1154 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 466:
#line 1156 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 467:
#line 1157 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 468:
#line 1159 "vex.y"
{yyval.lwptr=make_lowl(T_SEFD_MODEL,yyvsp[0].sval);;
    break;}
case 469:
#line 1160 "vex.y"
{yyval.lwptr=make_lowl(T_SEFD,yyvsp[0].septr);;
    break;}
case 470:
#line 1161 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 471:
#line 1162 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 472:
#line 1163 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 473:
#line 1165 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 474:
#line 1168 "vex.y"
{yyval.septr=make_sefd(yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].llptr);;
    break;}
case 475:
#line 1172 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 476:
#line 1173 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 477:
#line 1175 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 478:
#line 1176 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 479:
#line 1178 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 480:
#line 1179 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 481:
#line 1180 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 482:
#line 1183 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 483:
#line 1184 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 484:
#line 1186 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 485:
#line 1187 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 486:
#line 1189 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_TYPE,yyvsp[0].sval);;
    break;}
case 487:
#line 1190 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_NAME,yyvsp[0].sval);;
    break;}
case 488:
#line 1191 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_ID,yyvsp[0].sval);;
    break;}
case 489:
#line 1192 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_POSITION,yyvsp[0].spptr);;
    break;}
case 490:
#line 1193 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_POSITION_EPOCH,yyvsp[0].sval);;
    break;}
case 491:
#line 1194 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_POSITION_REF,yyvsp[0].sval);;
    break;}
case 492:
#line 1195 "vex.y"
{yyval.lwptr=make_lowl(T_SITE_VELOCITY,yyvsp[0].svptr);;
    break;}
case 493:
#line 1196 "vex.y"
{yyval.lwptr=make_lowl(T_HORIZON_MAP_AZ,yyvsp[0].llptr);;
    break;}
case 494:
#line 1197 "vex.y"
{yyval.lwptr=make_lowl(T_HORIZON_MAP_EL,yyvsp[0].llptr);;
    break;}
case 495:
#line 1198 "vex.y"
{yyval.lwptr=make_lowl(T_ZEN_ATMOS,yyvsp[0].dvptr);;
    break;}
case 496:
#line 1199 "vex.y"
{yyval.lwptr=make_lowl(T_OCEAN_LOAD_VERT,yyvsp[0].ovptr);;
    break;}
case 497:
#line 1200 "vex.y"
{yyval.lwptr=make_lowl(T_OCEAN_LOAD_HORIZ,yyvsp[0].ohptr);;
    break;}
case 498:
#line 1201 "vex.y"
{yyval.lwptr=make_lowl(T_OCCUPATION_CODE,yyvsp[0].sval);;
    break;}
case 499:
#line 1202 "vex.y"
{yyval.lwptr=make_lowl(T_INCLINATION,yyvsp[0].dvptr);;
    break;}
case 500:
#line 1203 "vex.y"
{yyval.lwptr=make_lowl(T_ECCENTRICITY,yyvsp[0].dvptr);;
    break;}
case 501:
#line 1204 "vex.y"
{yyval.lwptr=make_lowl(T_ARG_PERIGEE,yyvsp[0].dvptr);;
    break;}
case 502:
#line 1205 "vex.y"
{yyval.lwptr=make_lowl(T_ASCENDING_NODE,yyvsp[0].dvptr);;
    break;}
case 503:
#line 1206 "vex.y"
{yyval.lwptr=make_lowl(T_MEAN_ANOMALY,yyvsp[0].dvptr);;
    break;}
case 504:
#line 1207 "vex.y"
{yyval.lwptr=make_lowl(T_SEMI_MAJOR_AXIS,yyvsp[0].dvptr);;
    break;}
case 505:
#line 1208 "vex.y"
{yyval.lwptr=make_lowl(T_MEAN_MOTION,yyvsp[0].dvptr);;
    break;}
case 506:
#line 1209 "vex.y"
{yyval.lwptr=make_lowl(T_ORBIT_EPOCH,yyvsp[0].sval);;
    break;}
case 507:
#line 1210 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 508:
#line 1211 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 509:
#line 1212 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 510:
#line 1214 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 511:
#line 1216 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 512:
#line 1218 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 513:
#line 1222 "vex.y"
{yyval.spptr=make_site_position(yyvsp[-5].dvptr,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 514:
#line 1224 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 515:
#line 1226 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 516:
#line 1230 "vex.y"
{yyval.svptr=make_site_velocity(yyvsp[-5].dvptr,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 517:
#line 1232 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 518:
#line 1234 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 519:
#line 1236 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 520:
#line 1240 "vex.y"
{yyval.ovptr=make_ocean_load_vert(yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 521:
#line 1244 "vex.y"
{yyval.ohptr=make_ocean_load_horiz(yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 522:
#line 1246 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 523:
#line 1248 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 524:
#line 1250 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 525:
#line 1252 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 526:
#line 1254 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 527:
#line 1256 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 528:
#line 1258 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 529:
#line 1260 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 530:
#line 1262 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 531:
#line 1266 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 532:
#line 1267 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 533:
#line 1269 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 534:
#line 1270 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 535:
#line 1272 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 536:
#line 1273 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 537:
#line 1274 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 538:
#line 1277 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 539:
#line 1279 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 540:
#line 1281 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 541:
#line 1282 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 542:
#line 1284 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE_TYPE,yyvsp[0].llptr);;
    break;}
case 543:
#line 1285 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE_NAME,yyvsp[0].sval);;
    break;}
case 544:
#line 1286 "vex.y"
{yyval.lwptr=make_lowl(T_IAU_NAME,yyvsp[0].sval);;
    break;}
case 545:
#line 1287 "vex.y"
{yyval.lwptr=make_lowl(T_RA,yyvsp[0].sval);;
    break;}
case 546:
#line 1288 "vex.y"
{yyval.lwptr=make_lowl(T_DEC,yyvsp[0].sval);;
    break;}
case 547:
#line 1289 "vex.y"
{yyval.lwptr=make_lowl(T_REF_COORD_FRAME,yyvsp[0].sval);;
    break;}
case 548:
#line 1290 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE_POSITION_REF,yyvsp[0].sval);;
    break;}
case 549:
#line 1291 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE_POSITION_EPOCH,yyvsp[0].sval);;
    break;}
case 550:
#line 1292 "vex.y"
{yyval.lwptr=make_lowl(T_RA_RATE,yyvsp[0].dvptr);;
    break;}
case 551:
#line 1293 "vex.y"
{yyval.lwptr=make_lowl(T_DEC_RATE,yyvsp[0].dvptr);;
    break;}
case 552:
#line 1294 "vex.y"
{yyval.lwptr=make_lowl(T_VELOCITY_WRT_LSR,yyvsp[0].dvptr);;
    break;}
case 553:
#line 1295 "vex.y"
{yyval.lwptr=make_lowl(T_SOURCE_MODEL,yyvsp[0].smptr);;
    break;}
case 554:
#line 1296 "vex.y"
{yyval.lwptr=make_lowl(T_INCLINATION,yyvsp[0].dvptr);;
    break;}
case 555:
#line 1297 "vex.y"
{yyval.lwptr=make_lowl(T_ECCENTRICITY,yyvsp[0].dvptr);;
    break;}
case 556:
#line 1298 "vex.y"
{yyval.lwptr=make_lowl(T_ARG_PERIGEE,yyvsp[0].dvptr);;
    break;}
case 557:
#line 1299 "vex.y"
{yyval.lwptr=make_lowl(T_ASCENDING_NODE,yyvsp[0].dvptr);;
    break;}
case 558:
#line 1300 "vex.y"
{yyval.lwptr=make_lowl(T_MEAN_ANOMALY,yyvsp[0].dvptr);;
    break;}
case 559:
#line 1301 "vex.y"
{yyval.lwptr=make_lowl(T_SEMI_MAJOR_AXIS,yyvsp[0].dvptr);;
    break;}
case 560:
#line 1302 "vex.y"
{yyval.lwptr=make_lowl(T_MEAN_MOTION,yyvsp[0].dvptr);;
    break;}
case 561:
#line 1303 "vex.y"
{yyval.lwptr=make_lowl(T_ORBIT_EPOCH,yyvsp[0].sval);;
    break;}
case 562:
#line 1304 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 563:
#line 1305 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 564:
#line 1306 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 565:
#line 1308 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[-1].sval);;
    break;}
case 566:
#line 1310 "vex.y"
{yyval.llptr=add_list(add_list(NULL,yyvsp[-3].sval),yyvsp[-1].sval);;
    break;}
case 567:
#line 1312 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 568:
#line 1314 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 569:
#line 1316 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 570:
#line 1318 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 571:
#line 1320 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 572:
#line 1322 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 573:
#line 1324 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 574:
#line 1326 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 575:
#line 1328 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 576:
#line 1331 "vex.y"
{yyval.dvptr=yyvsp[-1].dvptr;;
    break;}
case 577:
#line 1341 "vex.y"
{yyval.smptr=make_source_model(yyvsp[-15].dvptr,yyvsp[-13].sval,yyvsp[-11].dvptr,yyvsp[-9].dvptr,yyvsp[-7].dvptr,yyvsp[-5].dvptr,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 578:
#line 1345 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 579:
#line 1346 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 580:
#line 1349 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 581:
#line 1350 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 582:
#line 1352 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 583:
#line 1353 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 584:
#line 1354 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 585:
#line 1358 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 586:
#line 1360 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 587:
#line 1363 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 588:
#line 1364 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 589:
#line 1366 "vex.y"
{yyval.lwptr=make_lowl(T_VSN,yyvsp[0].vsptr);;
    break;}
case 590:
#line 1367 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 591:
#line 1368 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 592:
#line 1370 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 593:
#line 1373 "vex.y"
{yyval.vsptr=make_vsn(yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].sval,yyvsp[-1].sval);;
    break;}
case 594:
#line 1377 "vex.y"
{yyval.llptr=yyvsp[0].llptr;;
    break;}
case 595:
#line 1378 "vex.y"
{yyval.llptr=NULL;;
    break;}
case 596:
#line 1380 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 597:
#line 1381 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 598:
#line 1383 "vex.y"
{yyval.lwptr=make_lowl(T_DEF,yyvsp[0].dfptr);;
    break;}
case 599:
#line 1384 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 600:
#line 1385 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 601:
#line 1388 "vex.y"
{yyval.dfptr=make_def(yyvsp[-4].sval,yyvsp[-2].llptr);;
    break;}
case 602:
#line 1390 "vex.y"
{yyval.dfptr=make_def(yyvsp[-3].sval,NULL);;
    break;}
case 603:
#line 1392 "vex.y"
{yyval.llptr=add_list(yyvsp[-1].llptr,yyvsp[0].lwptr);;
    break;}
case 604:
#line 1393 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].lwptr);;
    break;}
case 605:
#line 1395 "vex.y"
{yyval.lwptr=make_lowl(T_FANIN_DEF,yyvsp[0].fiptr);;
    break;}
case 606:
#line 1396 "vex.y"
{yyval.lwptr=make_lowl(T_FANOUT_DEF,yyvsp[0].foptr);;
    break;}
case 607:
#line 1398 "vex.y"
{yyval.lwptr=make_lowl(T_TRACK_FRAME_FORMAT,yyvsp[0].sval);;
    break;}
case 608:
#line 1399 "vex.y"
{yyval.lwptr=make_lowl(T_DATA_MODULATION,yyvsp[0].sval);;
    break;}
case 609:
#line 1401 "vex.y"
{yyval.lwptr=make_lowl(T_VLBA_FRMTR_SYS_TRK,yyvsp[0].fsptr);;
    break;}
case 610:
#line 1403 "vex.y"
{yyval.lwptr=make_lowl(T_VLBA_TRNSPRT_SYS_TRK,yyvsp[0].llptr);;
    break;}
case 611:
#line 1404 "vex.y"
{yyval.lwptr=make_lowl(T_S2_RECORDING_MODE,yyvsp[0].sval);;
    break;}
case 612:
#line 1405 "vex.y"
{yyval.lwptr=make_lowl(T_S2_DATA_SOURCE,yyvsp[0].dsptr);;
    break;}
case 613:
#line 1406 "vex.y"
{yyval.lwptr=make_lowl(T_REF,yyvsp[0].exptr);;
    break;}
case 614:
#line 1407 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT,yyvsp[0].sval);;
    break;}
case 615:
#line 1408 "vex.y"
{yyval.lwptr=make_lowl(T_COMMENT_TRAILING,yyvsp[0].sval);;
    break;}
case 616:
#line 1411 "vex.y"
{yyval.fiptr=make_fanin_def(yyvsp[-7].sval,yyvsp[-5].dvptr,yyvsp[-3].dvptr,yyvsp[-1].llptr);;
    break;}
case 617:
#line 1415 "vex.y"
{yyval.foptr=make_fanout_def(yyvsp[-7].sval,yyvsp[-5].llptr,yyvsp[-3].dvptr,yyvsp[-1].llptr);;
    break;}
case 618:
#line 1417 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 619:
#line 1419 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 620:
#line 1423 "vex.y"
{yyval.fsptr=make_vlba_frmtr_sys_trk(yyvsp[-7].dvptr,yyvsp[-5].sval,yyvsp[-3].dvptr,yyvsp[-1].dvptr);;
    break;}
case 621:
#line 1426 "vex.y"
{yyval.fsptr=make_vlba_frmtr_sys_trk(yyvsp[-5].dvptr,yyvsp[-3].sval,yyvsp[-1].dvptr,NULL);;
    break;}
case 622:
#line 1429 "vex.y"
{yyval.llptr=add_list(add_list(NULL,yyvsp[-3].dvptr),yyvsp[-1].dvptr);;
    break;}
case 623:
#line 1431 "vex.y"
{yyval.sval=yyvsp[-1].sval;;
    break;}
case 624:
#line 1434 "vex.y"
{yyval.dsptr=make_s2_data_source(yyvsp[-5].sval,yyvsp[-3].sval,yyvsp[-1].sval);;
    break;}
case 625:
#line 1436 "vex.y"
{yyval.dsptr=make_s2_data_source(yyvsp[-1].sval,NULL,NULL);;
    break;}
case 626:
#line 1439 "vex.y"
{yyval.llptr=add_list(add_list(yyvsp[-4].llptr,yyvsp[-2].sval),yyvsp[0].sval);;
    break;}
case 627:
#line 1441 "vex.y"
{yyval.llptr=add_list(add_list(NULL,yyvsp[-2].sval),yyvsp[0].sval);;
    break;}
case 628:
#line 1446 "vex.y"
{yyval.exptr=make_external(yyvsp[-5].sval,yyvsp[-3].ival,yyvsp[-1].sval);;
    break;}
case 629:
#line 1448 "vex.y"
{yyval.llptr=yyvsp[-1].llptr;;
    break;}
case 630:
#line 1450 "vex.y"
{yyval.llptr=ins_list(yyvsp[-2].dvptr,yyvsp[0].llptr);;
    break;}
case 631:
#line 1451 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].dvptr);;
    break;}
case 632:
#line 1453 "vex.y"
{yyval.llptr=add_list(yyvsp[-2].llptr,yyvsp[0].dvptr);;
    break;}
case 633:
#line 1454 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].dvptr);;
    break;}
case 634:
#line 1456 "vex.y"
{yyval.dvptr=yyvsp[0].dvptr;;
    break;}
case 636:
#line 1459 "vex.y"
{yyval.dvptr=make_dvalue(yyvsp[-1].sval,yyvsp[0].sval);;
    break;}
case 637:
#line 1461 "vex.y"
{yyval.llptr=add_list(yyvsp[-2].llptr,yyvsp[0].sval);;
    break;}
case 638:
#line 1462 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].sval);;
    break;}
case 640:
#line 1466 "vex.y"
{yyval.llptr=add_list(yyvsp[-2].llptr,yyvsp[0].dvptr);;
    break;}
case 641:
#line 1467 "vex.y"
{yyval.llptr=add_list(NULL,yyvsp[0].dvptr);;
    break;}
case 642:
#line 1469 "vex.y"
{yyval.dvptr=make_dvalue(yyvsp[0].sval,NULL);;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/opt/bison/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 1471 "vex.y"


yyerror(s)
char *s;
{
  fprintf(stderr,"%s at line %d\n",s,lines);
  exit(1);
}


