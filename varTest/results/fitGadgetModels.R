library(tidyverse)
library(Rgadget)

basedir <- "~/gadget/models/atlantis/varTest/varModels"
setwd(basedir)

varmods <- list.dirs("varModels", recursive = FALSE, full.names = FALSE)
todo <- varmods[20]

null_list <- 
    lapply(todo, function(x) {
        fit <- gadget.fit(main = sprintf("varModels/%s/WGTS/main.final", x),
                          wgts = sprintf("varModels/%s/WGTS", x),
                          printfile.printatstart = 0,
                          printfile.steps = "all",
                          rec.len.param = TRUE)
        return(NULL)
    })