Sys.setenv(GADGET_WORKING_DIR = normalizePath(gd$dir))
callGadget(s=1)

read.gadget.parameters(sprintf("%s/params.out", gd$dir)) %>%
    init_params("linf", 128.3374362, 75, 170, 1) %>%
    init_params("k", 0.1390399, 0.01, 0.30, 1) %>%
    init_params("recl", 23.5805761, 0, 40, 1) %>%
    init_params("bbin.mult", 100, 1e-08, 1e4, 1) %>%
    init_params("bbin", 100, 1e-08, 1e4, 1) %>%
    init_params("m.decay", 0, 0, 2, 0) %>%
    init_params("max.m", 0.15, 0.15, 0.75, 0) %>%
    init_params("min.m", 0.15, 0, 0.15, 0) %>%
    init_params("mult", 30, 1e-05, 50, 1) %>%
    init_params("init.abund", 1, 1e-05, 5, 1) %>%
    init_params("init.max", 50, 1, 75, 1) %>%
    init_params("init.min", 15, 0, 25, 1) %>%
    init_params("init.decay", 0.5, 1e-05, 2, 1) %>%
    #init_params("age[0-9]", 20, 1e-05, 100, 1) %>%
    #init_params("rec.scalar", 100, 1e-05, 1000, 1) %>%
    init_params("rec.sd", 2, 0.1, 10, 1) %>%
    #init_params("rec.[0-9]", 100, 1e-05, 10000, 1) %>%
    # init_params("discards.alpha", 0.066, 0.001, 3, 1) %>%
    # init_params("discards.l50", 70, 0, 120, 1) %>%
    init_params("spawn.alpha", -1, -999, 0, 1) %>%
    init_params("spawn.l50", 55, -200, 200, 1) %>%
    init_params("cod.spawn.m", 0.1, 0, 1, 1) %>%
    init_params("wl.alpha", -0.5, -999, 0, 1) %>%
    init_params("wl.l50", 50, -200, 200, 1) %>%
    init_params("bh.mu", 4e+08, 1e+08, 1e+09, 1) %>%
    init_params("bh.lam", 1.067416e+08, 0, 3e+09, 1) %>%
    init_params("spr.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("spr.l50", 49, 5, 120, 1) %>%
    init_params("aut.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("aut.l50", 49, 5, 120, 1) %>%
    init_params("comm.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("comm.l50", 66, 5, 120, 1) %>%
    write.gadget.parameters(.,file=sprintf("%s/params.in", gd$dir))
