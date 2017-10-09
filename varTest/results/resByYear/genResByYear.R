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
        res_by_year <- out$res.by.year
        model_num <- gsub("[^0-9]", "", y)
        res_by_year$model_index <- model_num
        return(res_by_year)
    })
    
    # merge datasets together and summarize at year/step
    res_by_year <- 
        do.call("rbind", fit) %>%
        group_by(year, area, model_index) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                     .funs = funs(sum)) %>%
        ungroup()
    
    # calculate summary stats for suite of models
    res_by_year_summary <- 
        res_by_year %>%
        group_by(year, area) %>%
        summarize_at(.vars = vars(total.number, total.biomass),
                     .funs = funs(min, mean, median, max, se)) %>%
        ungroup()
    
    # reassign names for loading later on
    data2save <- new.env()
    assign(sprintf("%s_res_by_year", x), 
           res_by_year, envir = data2save)
    assign(sprintf("%s_res_by_year_summary", x), 
           res_by_year_summary, envir = data2save)
    
    # save data sets
    save(list = ls(envir = data2save), 
         file = sprintf("results/%1$s/%1$s_resByYear.RData", x),
         envir = data2save)
})
