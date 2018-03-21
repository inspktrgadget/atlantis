Sys.setenv(GADGET_WORKING_DIR = normalizePath(gd$dir))
callGadget(s=1)

read.gadget.parameters(sprintf("%s/params.out", gd$dir)) %>%
    init_params("linf", 130, 120, 135, 1) %>%
    init_params("k", 0.15, 0.11, 0.196, 1) %>%
    init_params("recl", 20, 20, 35, 1) %>%
    init_params("bbin.mult", 1e4, 1, 1e8, 1) %>%
    init_params("bbin", 1e4, 1, 1e8, 1) %>%
    init_params("init.mult", 1, 1, 1e+04, 1) %>%
    init_params("init.scalar", 5, 1, 1e+04, 1) %>%
    init_params("init.max", 1000, 1, 1e04, 1) %>%
    init_params("init.min", 500, 0, 5000, 1) %>%
    init_params("init.decay", 0.2, 1e-05, 5, 1) %>%
    init_params("rec.scalar", 100, 1e-05, 1000, 1) %>%
    init_params("rec.sd", 2, 0.1, 10, 1) %>%
    init_params("rec.[0-9]", 100, 1e-05, 10000, 1) %>%
    init_params(sprintf("rec.%s", end_year - 1), 10, 1e-05, 500, 1) %>%
    init_params(sprintf("rec.%s", end_year), 10, 1e-05, 500, 1) %>%
    init_params("spr.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("spr.l50", 49, 5, 60, 1) %>%
    init_params("aut.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("aut.l50", 49, 5, 60, 1) %>%
    init_params("comm.alpha", 0.046, 0.001, 3, 1) %>%
    init_params("comm.l50", 66, 40, 80, 1) %>%
    write.gadget.parameters(.,file=sprintf("%s/params.in", gd$dir))
