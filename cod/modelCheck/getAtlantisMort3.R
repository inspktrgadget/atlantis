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
yma.count <- 
    is_fg_count %>%
    filter(count >= 1) %>%
    group_by(year, month, age) %>%
    summarize(total = sum(count))

# calculate z based on age structure, as in gadget
z.data <- 
    yma.count %>%
    mutate(log.total = log(total)) %>%
    group_by(year, month) %>%
    mutate(z = c(((log.total[1:9] - log.total[2:10])/2), 0))

# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# now get f for each year, month, and age combination
yma.catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    group_by(year, month, age) %>%
    summarize(total.catch = sum(count)) %>%
    ungroup() %>%
    mutate(month = ifelse(month == 1, 12, month - 1),
           year = ifelse(month == 1, year - 1, year))

f.data <- 
    left_join(yma.count, yma.catch) %>%
    mutate(total.af = total - total.catch) %>%
    mutate(f = (-1)*log(total.af / total)*12)

# now we subtract f from z to get m
m.data <- 
    left_join(z.data, f.data) %>%
    select(year, month, age, z, f) %>%
    mutate(m = z - f)

mean.m <- 
    m.data %>%
    filter(!is.na(m),
           m >= 0) %>%
    group_by(age) %>%
    summarize(mean.m = mean(m),
              median.m = median(m))

even.m <- mean.m
for (i in 1:(nrow(mean.m)-1)) {
    med.n <- mean(c(mean.m$median.m[i], mean.m$median.m[i+1]));
    mn.n <- mean(c(mean.m$mean.m[i], mean.m$mean.m[i+1]));
    df <- data.frame(age = (mean.m$age[i] + 1),
                     mean.m = mn.n,
                     median.m = med.n)
    even.m <- rbind(even.m, df)
}
    
m.data <- arrange(even.m, age)

# optimize params of m decay function
source('../functions/mDecayFunction.R')

age <- c(mean.m$age[1:5], 10, 12, 14, 16)
morts <- c(mean.m$median.m[1:5], rep(mean.m$median.m[5], 4))

params <- nlm(m_decay_sse, c(0.2, 0.3, 0.07), age = age, vals = morts)
    
# plot(morts ~ age)
# curve(m_decay_optimizer(params$estimate, x), add=T)

m.func.vals <- data.frame(age = 0:19,
                     m=m_decay_optimizer(params$estimate, 0:19)
                    )
