library(plyr)
library(tidyverse)
library(parallel)
library(Rgadget)

homeDir <- "~/gadget/models/atlantis/varTest/varModels"
setwd(homeDir)
file.create("gadgetFitOutput")
#mod_dir <- dir("varModels")
mod_dir <- sprintf("varModel_%s", c(0.269, 0.276, 0.282, 0.288, 0.294, 0.3))
null_list <- 
    mclapply(mod_dir, function(x) {
        cat("Fitting ", x, "\n")
        sink(file = "gadgetFitOutput", append = TRUE)
        tmp_fit <- gadget.fit(wgts = sprintf("varModels/%s/WGTS", x),
                              main.file = sprintf("varModels/%s/WGTS/main.final", x),
                              printfile.printatstart = 0,
                              printfile.steps = "all",
                              rec.len.param = TRUE)
        sink()
    }, mc.cores = 4)
