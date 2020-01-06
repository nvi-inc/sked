#ifndef BISON_Y_TAB_H
# define BISON_Y_TAB_H

#ifndef YYSTYPE
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
struct data_transfer   *dtptr;

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

} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	T_VEX_REV	257
# define	T_REF	258
# define	T_DEF	259
# define	T_ENDDEF	260
# define	T_SCAN	261
# define	T_ENDSCAN	262
# define	T_CHAN_DEF	263
# define	T_SAMPLE_RATE	264
# define	T_BITS_PER_SAMPLE	265
# define	T_SWITCHING_CYCLE	266
# define	T_START	267
# define	T_SOURCE	268
# define	T_MODE	269
# define	T_STATION	270
# define	T_DATA_TRANSFER	271
# define	T_ANTENNA_DIAM	272
# define	T_AXIS_OFFSET	273
# define	T_ANTENNA_MOTION	274
# define	T_POINTING_SECTOR	275
# define	T_AXIS_TYPE	276
# define	T_BBC_ASSIGN	277
# define	T_CLOCK_EARLY	278
# define	T_RECORD_TRANSPORT_TYPE	279
# define	T_ELECTRONICS_RACK_TYPE	280
# define	T_NUMBER_DRIVES	281
# define	T_HEADSTACK	282
# define	T_RECORD_DENSITY	283
# define	T_TAPE_LENGTH	284
# define	T_RECORDING_SYSTEM_ID	285
# define	T_TAPE_MOTION	286
# define	T_TAPE_CONTROL	287
# define	T_TAI_UTC	288
# define	T_A1_TAI	289
# define	T_EOP_REF_EPOCH	290
# define	T_NUM_EOP_POINTS	291
# define	T_EOP_INTERVAL	292
# define	T_UT1_UTC	293
# define	T_X_WOBBLE	294
# define	T_Y_WOBBLE	295
# define	T_NUT_REF_EPOCH	296
# define	T_NUM_NUT_POINTS	297
# define	T_NUT_INTERVAL	298
# define	T_DELTA_PSI	299
# define	T_DELTA_EPS	300
# define	T_NUT_MODEL	301
# define	T_EXPER_NUM	302
# define	T_EXPER_NAME	303
# define	T_EXPER_NOMINAL_START	304
# define	T_EXPER_NOMINAL_STOP	305
# define	T_PI_NAME	306
# define	T_PI_EMAIL	307
# define	T_CONTACT_NAME	308
# define	T_CONTACT_EMAIL	309
# define	T_SCHEDULER_NAME	310
# define	T_SCHEDULER_EMAIL	311
# define	T_TARGET_CORRELATOR	312
# define	T_EXPER_DESCRIPTION	313
# define	T_HEADSTACK_POS	314
# define	T_IF_DEF	315
# define	T_PASS_ORDER	316
# define	T_S2_GROUP_ORDER	317
# define	T_PHASE_CAL_DETECT	318
# define	T_TAPE_CHANGE	319
# define	T_NEW_SOURCE_COMMAND	320
# define	T_NEW_TAPE_SETUP	321
# define	T_SETUP_ALWAYS	322
# define	T_PARITY_CHECK	323
# define	T_TAPE_PREPASS	324
# define	T_PREOB_CAL	325
# define	T_MIDOB_CAL	326
# define	T_POSTOB_CAL	327
# define	T_HEADSTACK_MOTION	328
# define	T_PROCEDURE_NAME_PREFIX	329
# define	T_ROLL_REINIT_PERIOD	330
# define	T_ROLL_INC_PERIOD	331
# define	T_ROLL	332
# define	T_ROLL_DEF	333
# define	T_SEFD_MODEL	334
# define	T_SEFD	335
# define	T_SITE_TYPE	336
# define	T_SITE_NAME	337
# define	T_SITE_ID	338
# define	T_SITE_POSITION	339
# define	T_SITE_POSITION_EPOCH	340
# define	T_SITE_POSITION_REF	341
# define	T_SITE_VELOCITY	342
# define	T_HORIZON_MAP_AZ	343
# define	T_HORIZON_MAP_EL	344
# define	T_ZEN_ATMOS	345
# define	T_OCEAN_LOAD_VERT	346
# define	T_OCEAN_LOAD_HORIZ	347
# define	T_OCCUPATION_CODE	348
# define	T_INCLINATION	349
# define	T_ECCENTRICITY	350
# define	T_ARG_PERIGEE	351
# define	T_ASCENDING_NODE	352
# define	T_MEAN_ANOMALY	353
# define	T_SEMI_MAJOR_AXIS	354
# define	T_MEAN_MOTION	355
# define	T_ORBIT_EPOCH	356
# define	T_SOURCE_TYPE	357
# define	T_SOURCE_NAME	358
# define	T_IAU_NAME	359
# define	T_RA	360
# define	T_DEC	361
# define	T_SOURCE_POSITION_REF	362
# define	T_RA_RATE	363
# define	T_DEC_RATE	364
# define	T_SOURCE_POSITION_EPOCH	365
# define	T_REF_COORD_FRAME	366
# define	T_VELOCITY_WRT_LSR	367
# define	T_SOURCE_MODEL	368
# define	T_VSN	369
# define	T_FANIN_DEF	370
# define	T_FANOUT_DEF	371
# define	T_TRACK_FRAME_FORMAT	372
# define	T_DATA_MODULATION	373
# define	T_VLBA_FRMTR_SYS_TRK	374
# define	T_VLBA_TRNSPRT_SYS_TRK	375
# define	T_S2_RECORDING_MODE	376
# define	T_S2_DATA_SOURCE	377
# define	B_GLOBAL	378
# define	B_STATION	379
# define	B_MODE	380
# define	B_SCHED	381
# define	B_EXPER	382
# define	B_SCHEDULING_PARAMS	383
# define	B_PROCEDURES	384
# define	B_EOP	385
# define	B_FREQ	386
# define	B_CLOCK	387
# define	B_ANTENNA	388
# define	B_BBC	389
# define	B_CORR	390
# define	B_DAS	391
# define	B_HEAD_POS	392
# define	B_PASS_ORDER	393
# define	B_PHASE_CAL_DETECT	394
# define	B_ROLL	395
# define	B_IF	396
# define	B_SEFD	397
# define	B_SITE	398
# define	B_SOURCE	399
# define	B_TRACKS	400
# define	B_TAPELOG_OBS	401
# define	T_LITERAL	402
# define	T_NAME	403
# define	T_LINK	404
# define	T_ANGLE	405
# define	T_COMMENT	406
# define	T_COMMENT_TRAILING	407


extern YYSTYPE yylval;

#endif /* not BISON_Y_TAB_H */
