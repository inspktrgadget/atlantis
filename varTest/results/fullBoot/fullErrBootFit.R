library(plyr)
library(parallel)
library(tidyverse)
library(Rgadget)

bootDir <- "~/gadget/models/atlantis/varTest/errBootRuns/bootrun_var0.3"
setwd(bootDir)

bs_models <- list.dirs(paste(bootDir, "BS.WGTS", sep = "/"), 
                       full.names = FALSE, recursive = FALSE)

mclapply(bs_models[81:100], function(x) {
    cat("Fitting boot iteration", x, sep = " ")
    main_file <- sprintf("BS.WGTS/%s/WGTS/main.final", x)
    wgts <- sprintf("BS.WGTS/%s/WGTS", x)
    tmp_fit <- gadget.fit(wgts=wgts, main.file=main_file,
               fleet.predict = data.frame(fleet = 'comm', ratio=1),
               mat.par=c(-6.510198, 1.108594),
               printfile.printatstart = 0,
               printfile.steps = "all",
               rec.len.param = TRUE)
    out_fit_dir <- paste("BS.WGTS", x, "WGTS", "out.fit", sep = "/")
    dir.create(paste(out_fit_dir, "gadget.fit", sep = "/"))
    save(tmp_fit, file = paste(out_fit_dir, "gadget.fit", "bootFit.Rdata", sep = "/"))
    rm(tmp_fit)
}, mc.cores = 8)

