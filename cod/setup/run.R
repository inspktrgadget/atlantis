library(methods)
library(Rgadget)
setwd("~/gadget/models/atlantis/cod/codModel")
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(`0_18`=c("spr.si.0.18", "aut.si.0.18"),
					  `18_36`=c("spr.si.18.36", "aut.si.18.36"),
					  `36_48`=c("spr.si.36.48", "aut.si.36.48"),
					  `48_60`=c("spr.si.48.60", "aut.si.48.60"),
					  `60_70`=c("spr.si.60.70", "aut.si.60.70"),
					  `70_pl`=c("spr.si.70.pl", "aut.si.70.pl")),
                            wgts="WGTS")
)[3]

print(paste("Iterative model took",
            c(gt %/% 3600),
            "hours and",
            c((gt - (gt %/% 3600) * 3600) %/% 60),
            "minutes to run."))

rmarkdown::render("gadAtlResults.Rmd")
