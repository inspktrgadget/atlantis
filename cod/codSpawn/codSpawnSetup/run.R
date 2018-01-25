library(Rgadget)
setwd("~/gadget/models/atlantis/cod/codSpawn/codSpawnModel")
gt <- system.time(
    tmp <- gadget.iterative(rew.sI=TRUE,
                            main="main",
                            grouping=list(`20_35`=c("spr_si_20_35", "aut_si_20_35"),
                                          `35_45`=c("spr_si_35_45", "aut_si_35_45"),
                                          `45_60`=c("spr_si_45_60", "aut_si_45_60"),
                                          `60_80`=c("spr_si_60_80", "aut_si_60_80"),
					  `80_pl`=c("spr_si_80_pl", "aut_si_80_pl")),
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
