library(tidyverse)
library(Rgadget)
library(parallel)
library(methods)

basedir <- "~/gadget/models/atlantis/varTest"
setwd(basedir)

mod_dir <- "bootruns/bootrun_var0"
setwd(mod_dir)

null_list <- 
    mclapply(1:100, function(y) {
        fit <- gadget.fit(main = sprintf("%s/BS.%s/WGTS/main.final", sub_dir, y),
                          wgts = sprintf("%s/BS.%s/WGTS", sub_dir, y),
                          printfile.printatstart = 0,
                          printfile.steps = "all",
                          rec.len.param = TRUE)
        return(NULL)
}, mc.cores = 8)