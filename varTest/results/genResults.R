# this script is to summarize results from variance and bootstrap models
# it is a generalization of the *Results.R files found in each of the
# respective model directories in this results folder

library(tidyverse)

basedir <- "~/gadget/models/atlantis/varTest"
setwd(basedir)

models <- c("varMod", "bootMod", "halfBoot", "fullBoot")

se <- function(x, na.rm = FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    return(sd(x) / sqrt(length(x)))
}

ci_lower <- function(x) {
	return(sort(x)[ceiling(length(x) * 0.025)])
}

ci_upper <- function(x) {
	return(sort(x)[floor(length(x) * 0.975)])
}


lapply(models, function(x) {
    model_dir <- switch(
        x,
        varMod = "varModels",
        bootMod = "bootstrapRun/BS.WGTS",
        halfBoot = "errBootRuns/bootrun_var0.147/BS.WGTS",
        fullBoot = "errBootRuns/bootrun_var0.3/BS.WGTS"
    )
    sub_models <- list.dirs(model_dir, recursive = FALSE)
    
    # read gadget.fit results into list
    fit <- lapply(sub_models, function(y) {
        load(paste(y, "WGTS/WGTS.Rdata", sep = "/"))
        stock_std <- out$stock.std
        model_num <- gsub("[^0-9]", "", y)
        stock_std$model_index <- model_num
        return(stock_std)
    })
    
    # merge datasets together and summarize at year/step
    res_by_step <- 
        do.call("rbind", fit) %>%
        rename(total.number = number) %>%
        mutate(total.biomass = mean.weight * total.number) %>%
        group_by(model_index, year, step, area) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                 .funs = funs(sum)) %>%
        ungroup()
    
    res_by_step_age <- 
        do.call("rbind", fit) %>%
        rename(total.number = number) %>%
        mutate(total.biomass = mean.weight * total.number) %>%
        group_by(model_index, year, step, area, age) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                     .funs = funs(sum)) %>%
        ungroup()
    
    # calculate summary stats for suite of models
    summary <- 
        res_by_step %>%
        group_by(year, step, area) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                     .funs = funs(min, mean, median, max, se, ci_lower, ci_upper)) %>%
        ungroup()
    
    summary_by_age <- 
        res_by_step_age %>%
        group_by(year, step, area, age) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                     .funs = funs(min, mean, median, max, se, ci_lower, ci_upper)) %>%
        ungroup()
    
    # reassign names for loading later on
    data2save <- new.env()
    assign(sprintf("%s_resByStep", x), 
           res_by_step, envir = data2save)
    assign(sprintf("%s_resByStep_age", x),
           res_by_step_age, envir = data2save)
    assign(sprintf("%s_summary", x), 
           summary, envir = data2save)
    assign(sprintf("%s_summary_by_age", x),
           summary_by_age, envir = data2save)
    
    # save data sets
    save(list = ls(envir = data2save), 
         file = sprintf("results/%1$s/%1$s.RData", x),
         envir = data2save)
})
