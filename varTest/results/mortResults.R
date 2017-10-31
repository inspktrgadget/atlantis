# this is a script to get the mortality results from varTest estimation models
library(tidyverse)
source("~/gadget/models/functions/mDecayFunction.R")

basedir <- "~/gadget/models/atlantis/varTest"
setwd(basedir)

models <- c("varMod", "bootMod", "halfBoot", "fullBoot")

lapply(models, function(x) {
    model_dir <- switch(
        x,
        varMod = "varModels",
        bootMod = "bootstrapRun/BS.WGTS",
        halfBoot = "errBootRuns/bootrun_var0.147/BS.WGTS",
        fullBoot = "errBootRuns/bootrun_var0.3/BS.WGTS"
    )
    sub_models <- list.dirs(model_dir, recursive = FALSE)
    
    m_params <- lapply(sub_models, function(y) {
        params <- read.table(paste(y, "WGTS/params.final", sep = "/"), 
                             header=TRUE, comment.char = ";")
        find_switches <- c("m.decay", "max.m", "min.m")
        m_switches <- params[grep(paste(find_switches, collapse = "|"), params$switch), ]
        m_vals <- setNames(as.data.frame(t(m_switches$value)), m_switches$switch)
        m_vals$model <- gsub("[^0-9]", "", y)
        return(m_vals)
    }) 
    
    ages <- paste0("age", 0:19)
    
    m_param_vals <- 
        do.call("rbind", m_params) %>%
        rename(m.decay = cod.m.decay, max.m = cod.max.m, min.m = cod.min.m)
    
    m_vals <- 
        m_param_vals %>%
        cbind(setNames(lapply(ages, function(a) a = NA), ages)) %>%
        gather(key = age, m, age0:age19) %>%
        mutate(age = as.numeric(as.character(gsub("age", "", age)))) %>%
        mutate(m = m_decay_func(age, m.decay, max.m, min.m))
    
    
    env2save <- new.env()
    assign(sprintf("%s_m_param_vals", x), 
           m_param_vals, envir = env2save)
    assign(sprintf("%s_m_vals", x),
           m_vals, envir = env2save)
    save(list = ls(envir = env2save), 
         file = sprintf("results/%1$s/mResults.RData", x),
         envir = env2save)
})