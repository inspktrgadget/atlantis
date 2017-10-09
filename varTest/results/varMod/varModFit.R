library(plyr)
library(tidyverse)
library(Rgadget)

homeDir <- "~/gadget/models/atlantis/varTest"
setwd(homeDir)
varModels <- round(seq(0, 0.3, length.out = 50), 3)
indx <- 47:50
for (x in varModels[indx]) {
    setwd(homeDir)
    setwd(sprintf("varModels/varModel_%s", x))
    cat("Fitting varModel ", x, "\n")
    tmp_fit <- gadget.fit(wgts="WGTS", main.file='WGTS/main.final',
                  fleet.predict = data.frame(fleet = 'comm', ratio=1),
                  mat.par=c(-6.510198, 1.108594),
                  printfile.printatstart = 0,
                  printfile.steps = "all",
                  rec.len.param = TRUE)
    dir.create("WGTS/out.fit/gadget.fit")
    save(tmp_fit, file = "WGTS/out.fit/gadget.fit/modelFit.Rdata")
    rm(tmp_fit)
    closeAllConnections()
}
