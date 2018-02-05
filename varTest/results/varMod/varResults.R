library(tidyverse)
library(parallel)

basedir <- "~/gadget/models/atlantis/varTest/varModels"
setwd(basedir)

varModels <- dir("varModels")

# for some reason varModel_0.208 did not fit well at all; throwing it out here
varModels <- subset(varModels, !grepl("0.208", varModels))

var_fit <- mclapply(varModels, function(x) {
    load(paste("varModels", x, "WGTS/WGTS.Rdata", sep = "/"))
    stock_std <- out$stock.std
    var <- gsub("varModel_", "", x)
    stock_std$error_variance <- var
    return(stock_std)
}, mc.cores = 4)

var_res_by_step <- 
    do.call("rbind", var_fit) %>%
    group_by(year, step, area, error_variance) %>%
    mutate(total.number = sum(number),
           total.biomass = sum(mean.weight * number)) %>%
    summarize_at(.vars = vars(total.number, total.biomass),
                 .funs = funs(sum))

varModelsPlot <- 
    ggplot(data=filter(var_res_by_step, step == 1), 
           aes(x=year, y=total.number/1e6, color=factor(error_variance))) +
    geom_line() + xlab("Year") + ylab("Total number (millions") +
    theme_bw() + theme(legend.position = "none")

se <- function(x, na.rm = FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    return(sd(x) / sqrt(length(x)))
}

var_summary <- 
    var_res_by_step %>%
    group_by(year, step, area) %>%
    summarize_at(.vars = vars(total.number, total.biomass),
                 .funs = funs(min, mean, median, max, se))

varModShadePlot <- 
    ggplot(data=filter(var_summary, step == 1), 
           aes(x = year, 
               ymin=(total.number_median - (1.96*total.number_se))/1e6, 
               ymax=(total.number_median + (1.96*total.number_se))/1e6)) + 
    geom_ribbon(alpha = 0.5) + 
    geom_line(aes(x = year, y=total.number_median/1e6)) +
    xlab("Year") + ylab("Total number (millions)") + 
    theme_bw()

save(var_res_by_step, file = "../results/varMod/varFit.Rdata")
save(var_summary, file = "../results/varMod/varSummary.Rdata")
