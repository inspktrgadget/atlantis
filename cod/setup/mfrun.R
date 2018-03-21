library(Rgadget)
setwd("~/gadget/atlantis/cod/codModel")
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(`18_34`=c("spr.si.18.34", "aut.si.18.34"),
					  `34_48`=c("spr.si.34.48", "aut.si.34.48"),
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
