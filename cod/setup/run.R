library(methods)
library(Rgadget)
setwd("~/gadget/models/atlantis/cod/codModel")
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(`age0`=c("spr.si.0.15", "aut.si.0.20"),
					  `age1`=c("spr.si.15.28", "aut.si.20.35"),
					  `age2`=c("spr.si.28.43", "aut.si.35.48"),
					  `age3`=c("spr.si.43.55", "aut.si.48.60"),
					  `age4`=c("spr.si.55.65", "aut.si.60.70"),
					  `age5+`=c("spr.si.65.pl", "aut.si.70.pl")),
                            wgts="WGTS")
)[3]

print(paste("Iterative model took",
            c(gt %/% 3600),
            "hours and",
            c((gt - (gt %/% 3600) * 3600) %/% 60),
            "minutes to run."))

#rmarkdown::render("gadAtlResults.Rmd")
