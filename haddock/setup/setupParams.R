read.gadget.parameters(sprintf('%s/params.out', gd$dir)) %>%
    init.params('linf', 116.018, 75, 150, 1) %>%
    init.params('k', 0.1389627, 0.01, 0.30, 1) %>%
    init.params('bbin', 6, 1e-08, 100, 1) %>%
    init.params('mult', 5, 1e-05, 10, 1) %>%
    init.params('init.abund', 10, 1e-05, 1000, 1) %>%
    init.params('age.alpha', 1, 0, 1000, 1) %>%
    init.params('age.beta', 1, -100, 10, 1) %>%
    init.params('age.gamma', 1, -100, 10, 1) %>%
    init.params('age.delta', 1, 0, 1000, 1) %>%
    init.params('age[0-9]', 20, 1e-05, 100, 1) %>%
    init.params('recl', 15.2704, 0, 40, 1) %>%
    init.params('rec.scalar', 100, 1e-05, 1000, 1) %>%
    init.params('rec.sd', 2, 0.1, 10, 1) %>%
    init.params('rec.[0-9]', 100, 1e-05, 10000, 1) %>%
    init.params('comm.alpha', 0.066, 0.001, 3, 1) %>%
    init.params('comm.l50', 70, 5, 120, 1) %>%
    init.params('discards.alpha', 0.066, 0.001, 3, 1) %>%
    init.params('discards.l50', 70, 0, 120, 1) %>%
    init.params('spr.alpha', 0.046, 0.001, 3, 1) %>%
    init.params('spr.l50', 49, 5, 120, 1) %>%
    init.params('aut.alpha', 0.046, 0.001, 3, 1) %>%
    init.params('aut.l50', 49, 5, 120, 1) %>%
    write.gadget.parameters(.,file=sprintf('%s/params.in', gd$dir))
