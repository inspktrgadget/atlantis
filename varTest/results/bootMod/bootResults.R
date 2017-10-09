library(tidyverse)

basedir <- "~/gadget/models/atlantis/varTest"
setwd(basedir)

bs_models <- list.dirs("bootstrapRun/BS.WGTS", 
                       full.names = FALSE, recursive = FALSE)

boot_fit <- lapply(bs_models, function(x) {
    load(paste(basedir, "bootstrapRun", "BS.WGTS", x, 
               "WGTS/out.fit/gadget.fit/bootFit.Rdata", sep = "/"))
    res_by_year <- tmp_fit$res.by.year
    boot_iter <- gsub("BS.", "", x)
    res_by_year$boot_iter <- boot_iter
    return(res_by_year)
})

boot_res_by_year <- 
    do.call(rbind, boot_fit) %>%
    group_by(year, area, boot_iter) %>%
    summarize_at(.vars = vars(total.number, total.biomass, harv.biomass, 
                              ssb, catch, num.catch, F),
                 .funs = funs(sum))

bootModelsPlot <- 
    ggplot(data=boot_res_by_year, aes(x=year, y=total.number/1e6, 
                                     color=boot_iter)) + 
    geom_line() + xlab("Year") + ylab("Total number (millions") +
    theme_bw() + theme(legend.position = "none")

se <- function(x, na.rm = FALSE) {
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    return(sd(x) / sqrt(length(x)))
}

boot_summary <- 
    boot_res_by_year %>%
    group_by(year, area) %>%
    summarize_at(.vars = vars(total.number, total.biomass, harv.biomass, 
                              ssb, catch, num.catch, F),
                 .funs = funs(min, mean, median, max, se))

bootModShadePlot <- 
    ggplot(data=boot_summary, aes(x = year, 
                                    ymin=total.number_min/1e6, 
                                    ymax=total.number_max/1e6)) + 
    geom_ribbon(alpha = 0.5) + 
    geom_line(aes(x = year, y=total.number_median/1e6)) +
    xlab("Year") + ylab("Total number (millions)") + 
    theme_bw()

save(boot_res_by_year, file = "results/bootMod/bootFit.Rdata")
save(boot_summary, file = "results/bootMod/bootSummary.Rdata")
