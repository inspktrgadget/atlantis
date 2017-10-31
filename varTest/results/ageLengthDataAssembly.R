load("varMod/al_results.RData")
load("bootMod/al_results.RData")
load("halfBoot/al_results.RData")
load("fullBoot/al_results.RData")

# assemble and merge numbers data
models <- c("varMod", "bootMod", "halfBoot", "fullBoot")
lik_comp <- c("ldist.spr", "ldist.aut", "aldist.spr", "aldist.aut")

# pull together the raw age-length data
al_data <- 
    lapply(lik_comp, function(x) {
        lik_comp_data <- 
            lapply(models, function(y) {
                get(sprintf("%1$s_%2$s", y, x)) %>% 
                mutate(lik.comp = x,
                       area = 1)
            }) %>%
            do.call("rbind", .)
        return(lik_comp_data)
})
al_data <- setNames(al_data, lik_comp)

# pull together the summary data
al_summary <- 
    lapply(lik_comp, function(x) {
        lik_comp_data <- 
            lapply(models, function(y) {
                get(sprintf("%1$s_%2$s_summary", y, x)) %>% 
                    mutate(lik.comp = x,
                           area = 1)
            }) %>%
            do.call("rbind", .)
        return(lik_comp_data)
    })
al_summary <- setNames(al_summary, lik_comp)
