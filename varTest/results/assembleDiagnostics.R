# code to assemble diagnostic data for varModels

library(plyr)
library(tidyverse)
library(grid)
library(Rgadget)

varmod_dir <- "~/gadget/models/atlantis/varTest"
modfiles <- dir(sprintf("%s/varModel/varModels", varmod_dir))

out_data <- 
    lapply(modfiles, function(x) {
        load(sprintf("%s/varModels/%s/WGTS/WGTS.Rdata", varmod_dir, x))
        return(out)
    })


# ## fit statistics - not a great way to do this for all varModels
# resTable <- fit$resTable[tail(head(names(fit$resTable),-2),-1)]
# 
# summary_plot <-
#     ggplot(filter(fit$likelihoodsummary, year != "all"),
#            aes(as.numeric(year), likelihood.value)) +
#     geom_point() + facet_wrap(~component, scales="free_y") +theme_bw()+
#     xlab("Year") + ylab("Score")

# put together survey indices for all varModels
si_dat <- 
    lapply(1:length(out_data), function(x) {
        tmp <- 
            out_data[[x]]$sidat %>%
            mutate(survey = ifelse(substr(name, 1, 3) == "aut",
                                   "aut", "igfs")) %>%
            rbind.fill(ddply(., ~year+survey, summarize,
                             observed = sum(observed*0.008249352*lower^3.026918),
                             predict = sum(predict*0.008249352*lower^3.026918),
                             upper = sum(upper*0.008249352*lower^3.026918),
                             lower = sum(lower*0.008249352*lower^3.026918),
                             length = "Biomass")) %>%
            mutate(model_ind = x) %>%
            ungroup()
    }) %>%
    do.call("rbind", .) %>%
    group_by(year, survey, length) %>%
    summarize(obs_median = median(observed),
              pred_median = median(predict),
              pred_lower = sort(predict)[length(predict)*0.025],
              pred_upper = sort(predict)[length(predict)*0.975])

ldist_data <- 
    lapply(1:length(out_data), function(x) {
        out_data[[x]]$catchdist.fleets
    }) %>%
    do.call("rbind", .) %>%
    group_by(name, year, step, lower) %>%
    summarize(observed = median(observed),
              pred_median = median(predicted),
              pred_lower = sort(predicted)[length(predicted)*0.025],
              pred_upper = sort(predicted)[length(predicted)*0.975])

aldist_data <- 
    lapply(1:length(out_data), function(x) {
        out_data[[x]]$catchdist.fleets %>%
            filter(name %in% c("aldist_spr", "aldist_aut", "aldist_comm")) %>%
            group_by(name, year, step, age) %>%
            summarize(observed = sum(observed, na.rm=T),
                      predicted = sum(predicted, na.rm=T)) %>%
            mutate(age = as.numeric(gsub("age", "", age)))
    }) %>%
    do.call("rbind", .) %>%
    group_by(name, year, step, age) %>%
    summarize(observed = median(observed), 
              pred_median = median(predicted), 
              pred_lower = sort(predicted)[length(predicted)*0.025],
              pred_upper = sort(predicted)[length(predicted)*0.975])

selection_data <- 
    lapply(1:length(out_data), function(x) {
        out_data[[x]]$suitability
    }) %>%
    do.call("rbind", .) %>%
    filter(suit > 0) %>%
    group_by(year, stock, fleet, length) %>%
    summarize(suit_median = median(suit),
              suit_lower = sort(suit)[ceiling(length(suit)*0.025)],
              suit_upper = sort(suit)[ceiling(length(suit)*0.0975)])

grw_data <- 
    lapply(1:length(out_data), function(x) {
        out_data[[x]]$stock.growth
    }) %>%
    do.call("rbind", .) %>%
    group_by(age) %>%
    summarize(length_median = median(length),
              length_lower = sort(length)[length(length)*0.025],
              length_upper = sort(length)[length(length)*0.975])

f_data <- 
    lapply(1:length(out_data), function(x) {
        out_data[[x]]$res.by.year
    }) %>%
    do.call("rbind", .) %>%
    filter(stock == "cod") %>%
    group_by(year) %>%
    summarize(f_median = median(F),
              f_lower = sort(F)[length(F)*0.025],
              f_upper = sort(F)[length(F)*0.975])

save(si_dat, ldist_data, aldist_data, grw_data, selection_data, f_data,
     file = sprintf("%s/results/varMod/diagnostic_data.RData", varmod_dir))