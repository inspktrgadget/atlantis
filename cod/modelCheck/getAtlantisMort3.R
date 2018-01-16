# this is yet another way to calculate mortality
# I'm attempting to do it here more in line with how z is 
# is calculated in gadget, that is to look at the age structure,
# calculate z from this, and then calculate f independently and 
# skim that off the top to get m
library(ggplot2)
library(tidyr)
library(dplyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
setwd('~/gadget/models/atlantis')
source('functions/commCatchAges.R')
source('functions/discardAges.R')
is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF')

is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# assemble and import cod 
fgName <- 'Cod'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group) %>% filter(count > 0)

# group catch by year, month, age
yma_count <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    group_by(year, month, age) %>%
    summarize(total = sum(count))

# calculate z based on age structure, as in gadget
z_data <- 
    yma_count %>%
    mutate(log_total = log(total)) %>%
    group_by(year, month) %>%
    mutate(z = c(((log_total[1:9] - log_total[2:10])/2), 0))

# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# now get f for each year, month, and age combination
yma_catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    group_by(year, month, age) %>%
    summarize(total_catch = sum(count)) %>%
    ungroup() %>%
    mutate(month = ifelse(month == 1, 12, month - 1),
           year = ifelse(month == 1, year - 1, year))

f_data <- 
    left_join(yma_count, yma_catch) %>%
    mutate(total_af = total - total_catch) %>%
    mutate(f = (-1)*log(total_af / total)*12)

# now we subtract f from z to get m
m_data <- 
    left_join(z_data, f_data) %>%
    select(year, month, age, z, f) %>%
    mutate(m = z - f)

mean_m <- 
    m_data %>%
    filter(!is.na(m),
           m > 0) %>%
    group_by(age) %>%
    summarize(mean_m = mean(m),
              median_m = median(m))

even_m <- mean_m
for (i in 1:(nrow(mean_m)-1)) {
    med_n <- mean(c(mean_m$median_m[i], mean_m$median_m[i+1]));
    mn_n <- mean(c(mean_m$mean_m[i], mean_m$mean_m[i+1]));
    df <- data.frame(age = (mean_m$age[i] + 1),
                     mean_m = mn_n,
                     median_m = med_n)
    even_m <- rbind(even_m, df)
}
    
m_data <- arrange(even_m, age)

# optimize params of m decay function
source('../functions/mDecayFunction.R')

age <- c(mean_m$age[1:5], 10, 12, 14, 16)
morts <- c(mean_m$median_m[1:5], rep(mean_m$median_m[5], 4))

params <- nlm(m_decay_sse, c(0.25, 0, 0.3), 
              age = mean_m$age, vals = mean_m$median_m)
    
# plot(morts ~ age)
# curve(m_decay_optimizer(params$estimate, x), add=T)

# smoothing out the median m above to get m for each value
# I subjectively decided on the m_decay parameters (very bad...I know) by checking
# the age distributions after parsing ages in the atlantis data
# the following parameters resulted in the smoothest age distributions
m_vals <- 
    data.frame(age = 0:19,
               m=m_decay_optimizer(c(0.25, 0.8, 0.06), 0:19))
