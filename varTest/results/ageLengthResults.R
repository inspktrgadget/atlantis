# script to summarize the age and length data for the various different 
# error estimation models and bootstrap estimation models

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
    
    # data types to import
    data2import <- c("ldist.spr", "ldist.aut", "aldist.spr", "aldist.aut")
    env2save <- new.env(parent = parent.frame())
    
    # read gadget.fit results into list
    data_list <- lapply(data2import, function(y) {
        data <- lapply(sub_models, function(z) {
            input <- 
                read.table(paste(z, 
                                 sprintf("Data/surveydistribution.%s.", y), 
                                 sep = "/"), 
                           comment.char = ";") %>%
                mutate(model = x,
                       model_indx = gsub("[^0-9]", "", z), 
                       source = "data")
            output <-  
                read.table(paste(z, 
                                 sprintf("WGTS/out.fit/%s", y), 
                                 sep = "/"), 
                           comment.char = ";")  %>%
                mutate(model = x,
                       model_indx = gsub("[^0-9]", "", z), 
                       source = "model")
            return(rbind(input, output))
        })
        
        # establish environment from which to save objects
        
        if (grepl("aldist", y)) {
            aldist <- 
                do.call("rbind", data) %>%
                rename(year = V1, step = V2, area = V3, age = V4, length = V5, count = V6) %>%
                mutate(age = as.numeric(as.character(gsub("age", "", age)))) %>%
                group_by(model, model_indx, source, year, age) %>%
                summarize(count = sum(count)) %>%
                mutate(prop = count / sum(count)) %>%
		ungroup()
            
            al_summary <- 
                aldist %>%
                group_by(model, source, year, age) %>%
                summarize(median.prop = median(prop),
                          ci_lower = quantile(prop, 0.025),
                          ci_upper = quantile(prop, 0.975)) %>%
		ungroup()
            
            # assign names and save
            assign(sprintf("%1$s_%2$s", x, y), 
                   aldist, envir = env2save)
            assign(sprintf("%1$s_%2$s_summary", x, y), 
                   al_summary, envir = env2save)
        } else {
            ldist <- 
                do.call("rbind", data) %>%
                rename(year = V1, step = V2, area = V3, age = V4, length = V5, count = V6) %>%
                mutate(length = as.numeric(as.character(gsub("len", "", length)))) %>%
                group_by(model, model_indx, source, year) %>%
                mutate(prop = count / sum(count)) %>%
		ungroup()
            
            ldist_summary <- 
                ldist %>%
                group_by(model, source, year, length) %>%
                summarize(median.prop = median(prop),
                          ci_lower = quantile(prop, 0.025),
                          ci_upper = quantile(prop, 0.975)) %>%
		ungroup()
            
            assign(sprintf("%1$s_%2$s", x, y), 
                   ldist, envir = env2save)
            assign(sprintf("%1$s_%2$s_summary", x, y), 
                   ldist_summary, envir = env2save)
        }
    })
    save(list = ls(envir = env2save), 
         file = sprintf("results/%1$s/al_results.RData", x),
         envir = env2save)
})    
