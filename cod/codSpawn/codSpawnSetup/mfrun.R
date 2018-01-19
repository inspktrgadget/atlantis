library(Rgadget)
setwd("~/gadget/atlantis/cod/codSpawn")
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(short=c("spr_si_0_20", "aut_si_0_20",
                                                  "spr_si_20_35", "aut_si_20_35"),
                                          mid=c("spr_si_35_45", "aut_si_35_45",
                                                "spr_si_45_60", "aut_si_45_60"),
                                          long=c("spr_si_60_80", "aut_si_60_80",
                                                 "spr_si_80_pl", "aut_si_80_pl")),
                            wgts="WGTS")
)[3]

print(paste("Iterative model took",
            c(gt %/% 3600),
            "hours and",
            c((gt - (gt %/% 3600) * 3600) %/% 60),
            "minutes to run."))

# survey=c("ldist_igfs", "ldist_aut",
#          "aldist_igfs", "aldist_aut"),
# comm=c("ldist_bmt", "aldist_bmt")
