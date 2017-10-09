library(tidyverse)

basedir <- "~/gadget/models/atlantis/varTest"
setwd(basedir)

varModels <- list.dirs("varModels", recursive = FALSE)

var_fit <- lapply(varModels, function(x) {
    load(paste(x, "WGTS/out.fit/gadget.fit/modelFit.Rdata", sep = "/"))
    stock_std <- tmp_fit$stock.std
    var <- gsub("varModels/varModel_", "", x)
    stock_std$error_variance <- var
    return(stock_std)
})

var_res_by_step <- 
    do.call(rbind, var_fit) %>%
    group_by(year, step, area, error_variance) %>%
    mutate(total.number = sum(number),
           total.biomass = sum(mean.weight * number))
    summarize_at(.vars = vars(total.number, total.biomass),
                 .funs = funs(sum))

varModelsPlot <- 
    ggplot(data=var_res_by_year, aes(x=year, y=total.number/1e6, color=error_variance)) +
    geom_line() + xlab("Year") + ylab("Total number (millions") +
    theme_bw() + theme(legend.position = "none")

se <- function(x, na.rm = FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    return(sd(x) / sqrt(length(x)))
}

var_summary <- 
    var_res_by_year %>%
    group_by(year, area) %>%
    summarize_at(.vars = vars(total.number, total.biomass, harv.biomass, 
                              ssb, catch, num.catch, F),
                 .funs = funs(min, mean, median, max, se))

varModShadePlot <- 
    ggplot(data=var_summary, aes(x = year, 
                                    ymin=(total.number_median - (1.96*total.number_se))/1e6, 
                                    ymax=(total.number_median + (1.96*total.number_se))/1e6)) + 
    geom_ribbon(alpha = 0.5) + 
    geom_line(aes(x = year, y=total.number_median/1e6)) +
    xlab("Year") + ylab("Total number (millions)") + 
    theme_bw()

save(var_res_by_year, file = "results/varMod/varFit.Rdata")
save(var_summary, file = "results/varMod/varSummary.Rdata")
