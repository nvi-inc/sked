#####################################################################
#
#                      makefile for linux sked.
# 2017Feb27  JMGipson.  
# 
#  XXX
#
#####################################################################

SRCDIR =..

PROGRAM =sked
SK =$(SRCDIR)/sked/

# Be sure to put the modules at the top since they are needed by others.
OBJ_MOD = \
   group_mod.o \
   twin_mod.o 

OBJ_FOR=  \
    $(SKED_HEAD) \
    $(READ_CMDLINE)  \
    gnpas.o       \
    adjustendtime.o \
    add_file_to_vex.o \
    addsec2ut.o \
    allday.o \
    auchk.o \
    autot.o \
    awrst.o \
    astro_cmd.o \
    astro_cull.o \
    astro_out.o \
    astro_obs.o \
    atmos.o \
    azel2xy.o \
    azwrap.o \
    baseds.o \
    bestsources.o \
    broadband_cmd.o \
    broadband_out.o \
    cablw.o  \
    cat_cmd.o \
    cat_out.o \
    check_barrel_match.o \
    cover.o \
    chksrcup4scan.o \
    chksundist.o \
    clear_close_sources.o \
    cabl1.o \
    chcmd.o \
    check_trk_flux_sefd.o \
    chout.o chpar.o \
    check_stations.o \
    chstn.o \
    chske.o \
    cmdblock.o \
    cochk.o \
    computestatsdouble.o \
    computestatsdoublexref.o \
    comcmd.o \
    copy_cur2vec.o \
    coverage.o \
    compute_coverage.o \
    delete_temp_files.o \
    dectl.o delcm.o \
    destn.o \
    delete_all_obs.o \
    display_list.o \
    dot8.o \
    downtime.o \
    display_scan_info.o \
    display_mode_item.o \
    display_stat_item.o \
    display_src_item.o \
    downtime_out.o \
    dspop.o \
    dspops.o \
    elevat.o \
    equal_area_proj.o \
    exper_cmd.o \
    exout.o \
    exread.o \
    extract_station_list.o \
    ffreq.o \
    fillcmd.o \
    find_best_scan.o  \
    findsegoverlap.o \
    find_obs_per_src.o \
    find_stat_vis.o \
    findtotaluptime.o \
    findallslewtimes.o \
    flcmd.o \
    flget.o \
    fllis.o \
    flinp.o \
    flout.o \
    fllis1.o \
    fluxds.o \
    frcat.o \
    frcmd.o \
    frlis.o \
    frsel.o \
    frx.o \
    frec.o \
    fsked.o \
    getist.o  \
    get_cat_version.o \
    get_new_fluxes.o \
    get_srcs_2_monitor.o \
    get_system_constant.o  \
    gtcmd.o \
    gtobs.o \
    gtsoi.o \
    gtsti.o \
    gtssi.o \
    gtban.o \
    gtdtr.o \
    gtrun.o \
    gtpre.o \
    grget.o \
    helpsk.o \
    horpl.o \
    hwrst.o \
    ibnum.o \
    indexx8.o \
    indexxint.o \
    init_time_arrays.o \
    incur.o \
    invert_and_con_tri.o\
    invert_or_det_tri.o \
    isecdif.o \
    isup.o \
    keep_some_srcs.o \
    kfreq_cat.o \
    kfreq_match.o \
    kstatup.o \
    kvlba_station.o \
    kyes_to_prompt.o \
    len_trim.o \
    licmd.o \
    looka.o \
    lscur.o \
    lshed.o \
    lucmd.o \
    lyn.o \
    lon_off.o \
    major_init.o \
    major_cmd.o \
    major_out.o \
    master_cmd.o  \
    make_defnames.o \
    make_source_list.o \
    make_stat_list.o \
    make_mode_list.o \
    make_scans.o \
    makesubset.o \
    makeobsper.o \
    makesrcobstarget.o \
    makestatobstarget.o \
    maxdu.o modcu.o muvis.o mxlis.o  \
    minor_init.o \
    minor_cmd_parse.o \
    minor_cmd.o \
    minor_out.o \
    nextc.o \
    nextpr.o \
    newcm.o \
    newob.o \
    newpr.o \
    norm_by_range.o \
    now_cmd.o \
    obs_sort.o  \
    obsfl.o \
    open_cat.o \
    op_read.o \
    opcmd.o \
    opout.o \
    opfill.o \
    op_refresh.o \
    pakup.o \
    parcmd.o \
    pixelation.o \
    prcmd.o \
    prlis.o \
    prncm.o \
    prout.o \
    printfr.o \
    prset.o \
    param_read.o \
    print_slew_info_warning.o proc_out.o \
    ptobs.o \
    pwrst.o \
    random_cmd.o \
    ranksources.o \
    readcat.o \
    readsourcecatalog.o \
    read_antenna_cat.o \
    read_freq_cat.o \
    read_loif_cat.o \
    read_mode_cat.o \
    read_rec_cat.o \
    read_rx_cat.o \
    read_source_cat.o \
    read_track_cat.o \
    remove_bad_snr.o \
    replace_sksrc.o \
    return_master_line.o \
    result.o \
    risst.o \
    rsini.o \
    read_cap_char.o \
    save_station_state.o \
    scctl.o \
    sefdel.o \
    sefdds.o \
    select_from_list.o \
    seop.o \
    setba.o  \
    setsc.o  \
    set_scan_param.o \
    set_signal_ctrlc.o \
    simul.o skcls.o sked_rdctl.o \
    skip_to_next_non_comment.o \
    skip_to_next_cat_group.o \
    skini.o \
    skopn.o \
    skout.o \
    sky_cov.o \
    sky_cov_pix.o \
    slewt.o \
    slewt2.o \
    snrac.o \
    snrcm.o \
    snrds.o \
    snrok.o \
    snrsk.o \
    snr_per_sec.o \
    snr_find_numgood_numbad.o \
    sochr.o \
    socat.o \
    source_cmd.o \
    source_cull.o  \
    solis.o \
    solve.o \
    sonum.o \
    soplt_gnu.o \
    soplt_pol.o \
    sosel.o sphere_pix.o \
    srcwt_cmd.o \
    srcwt_out.o \
    sscan.o \
    stcat.o \
    stcmd.o \
    stlis.o \
    stsel.o \
    stream_cmd.o \
    stream_anl.o \
    statwt_cmd.o \
    statwt_out.o \
    stat_sefd_out.o \
    store.o \
    subcon.o \
    subdis.o \
    sumcm.o \
    sumhd.o \
    sumout.o \
    sumpl_gnu.o \
    sumpr.o \
    swapint.o \
    testcon.o \
    thin_cmd.o \
    tmlin.o \
    tsincer.o \
    twrst.o \
    unout.o \
    unpak.o  \
    vex_comment.o \
    visss.o \
    vscan.o \
    vanout.o \
    vbbout.o \
    vdaout.o \
    vec_util.o \
    vexout.o \
    vfrout.o \
    vglout.o \
    vhdout.o \
    vifout.o \
    vlmode.o \
    vmoout.o \
    vpoout.o \
    vsoout.o \
    vsiout.o \
    vpcout.o \
    vroout.o \
    vscout.o \
    vstout.o \
    vtrout.o  \
    whatsup.o \
    when_at_next_source.o \
    wrerr.o wrfrs.o \
    writetotaluptime.o  \
    wrlog.o \
    wrsos.o \
    write_vs.o \
    write_minor_score_and_scan.o \
    write_minor_score_header.o \
    wrsts.o  \
    wrfclines.o \
    wrllines.o wrrblines.o \
    wrt_param_line.o \
    xlcmd.o \
    xncmd.o \
    ydhms.o 
    

OBJ_C = \
    cshell.o \
    date_and_time_sked.o \
    pc8.o \
    renam.o \
    rwopen.o \
    skcat.o \
    skpar.o \
    yywrap.o \
    $(MYSQL_INT) 

#FC =$(FORT)

#CC:=$(CC)

LDFLAGS =\
$(MYSQL_LIB) \
../skdrut/skdrut.a \
../lnfch/lnfch.a \
$(VEX_LIB) \
../curses/curses.a \
../matrix/matrix.a \
$(READLINE_LIB) \
$(BLAS_LIB) $(ATLAS_LIB) \
$(CURSES_LIB) \
$(FLEX) \
-lc  


# NOTE: Use /lib/crt0 if you are NOT using the profiler.
#       Use /lib/gcrt0 if you ARE using the profiler.
(PROGRAM): $(OBJ_MOD) $(OBJ_FOR) $(OBJ_C) 
	@echo Linking SKED...
	$(LINK) -o  $(SK)$(PROGRAM) $(OBJ_MOD) $(OBJ_FOR) $(OBJ_C) $(LDFLAGS)
	@echo done.

clean:
	rm -f *.o; rm -f *.a; rm -f sked
