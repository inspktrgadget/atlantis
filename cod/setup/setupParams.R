Sys.setenv(GADGET_WORKING_DIR = normalizePath(gd$dir))
callGadget(s=1)

read.gadget.parameters(sprintf('%s/params.out', gd$dir)) %>%
    init.params('linf', 128.3374362, 120, 135, 1) %>%
    init.params('k', 0.1390399, 0.11, 0.196, 1) %>%
    init.params('recl', 23.5805761, 15, 35, 1) %>%
    init.params('bbin', 100, 1, 1e4, 1) %>%
    # init.params('age[0-9]', 0.2, 0.001, 1, 1) %>%
    init.params('m.decay', 0, 0.0001, 5, 1) %>%
    init.params('max.m', 0.35, 0.2, 1.2, 1) %>%
    init.params('min.m', 0.35, 0, 0.35, 1) %>%
    init.params('mult', 1, 1e-05, 2, 1) %>%
    init.params('init.abund', 5, 1e-05, 5, 1) %>%
    # init.params('init[0-9]', 100, 1e-05, 1000, 1) %>%
    init.params('init.decay', 0.2, 1e-05, 5, 1) %>%
    init.params('init.scalar', 1000, 1, 1e04, 1) %>%
    init.params('init.min', 500, 0, 5000, 1) %>%
    #init.params('age[0-9]', 20, 1e-05, 100, 1) %>%
    init.params('rec.scalar', 100, 1e-05, 1000, 1) %>%
    init.params('rec.sd', 2, 0.1, 10, 1) %>%
    init.params('rec.[0-9]', 100, 1e-05, 10000, 1) %>%
    init.params('comm.alpha', 0.046, 0.001, 3, 1) %>%
    init.params('comm.l50', 66, 40, 80, 1) %>%
    # init.params('discards.alpha', 0.066, 0.001, 3, 1) %>%
    # init.params('discards.l50', 70, 0, 120, 1) %>%
    init.params('spr.alpha', 0.046, 0.001, 3, 1) %>%
    init.params('spr.l50', 49, 5, 60, 1) %>%
    init.params('aut.alpha', 0.046, 0.001, 3, 1) %>%
    init.params('aut.l50', 49, 5, 60, 1) %>%
    init.params('spr.si.beta', -6.907755, -20, 0, 1) %>%
    init.params('aut.si.beta', -6.907755, -20, 0, 1) %>%
    write.gadget.parameters(.,file=sprintf('%s/params.in', gd$dir))
