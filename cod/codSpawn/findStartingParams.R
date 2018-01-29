source("~/gadget/models/functions/gadgetUtils.R")
mod_dir <- ('~/gadget/models/atlantis/cod/codSpawn/codSpawnModel')
setwd(mod_dir)
read.gadget.parameters("params.in") %>%
    init_params("linf", 128.3374362, 75, 170, 0) %>%
    init_params("k", 0.1390399, 0.01, 0.30, 0) %>%
    init_params("recl", 23.5805761, 0, 40, 0) %>%
    init_params("bbin.mult", 100, 1, 1e4, 1) %>%
    init_params("bbin", 100, 1, 1e4, 1) %>%
    init_params("m.decay", 0, 0, 2, 0) %>%
    init_params("max.m", 0.15, 0.15, 0.75, 0) %>%
    init_params("min.m", 0.15, 0, 0.15, 0) %>%
    init_params("mult", 30, 1e-05, 1e6, 0) %>%
    init_params("init.abund", 1, 1e-05, 1e6, 0) %>%
    init_params("init.decay", 1, 1e-05, 5, 0) %>%
    init_params("init.scalar", 50, 0, 1e6, 0) %>%
    init_params("init.min", 15, 0, 1e6, 0) %>%
    init_params("rec.sd", 2, 0.1, 10, 1) %>%
    init_params("spawn.alpha", -1, -999, 0, 0) %>%
    init_params("spawn.l50", 55, -200, 200, 0) %>%
    init_params("bh.mu", 4e+08, 1e+08, 1e+09, 0) %>%
    init_params("bh.lam", 1.067416e+08, 0, 3e+09, 0) %>%
    init_params("spr.alpha", 0.046, 0.001, 3, 0) %>%
    init_params("spr.l50", 49, 5, 120, 0) %>%
    init_params("aut.alpha", 0.046, 0.001, 3, 0) %>%
    init_params("aut.l50", 49, 5, 120, 0) %>%
    init_params("comm.alpha", 0.046, 0.001, 3, 0) %>%
    init_params("comm.l50", 66, 5, 120, 0) %>%
    write.gadget.parameters(.,file="params.in")

library(plyr)
library(tidyverse)
library(grid)
library(Rgadget)
fit <- gadget.fit(wgts = NULL, main.file = "main", params = "params.in",
                  printfile.steps = "all")

ggplot(data=filter(fit$stock.std, step == 2), 
       aes(x=year, y=number, color = stock)) + 
    geom_line() + facet_wrap(~age, scales = "free_y") + 
    theme_bw()

ssb <- 
    fit$stock.std %>%
    filter(age >= 4, year == min(year)) %>%
    summarize(ssb = sum(number * mean.weight))

ggplot(data=NULL, aes(x=seq(0, 1e09, by = 1e6))) + 
    stat_function(fun = bev_holt,
                  args = list(mu = 4e08, lambda = 1.67e08)) + 
    geom_vline(xintercept = ssb$ssb)

closeAllConnections()
