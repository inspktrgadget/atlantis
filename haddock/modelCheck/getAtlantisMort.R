library(ggplot2)
library(tidyr)
library(dplyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)
setwd('~/gadget/models/atlantis')
source('../functions/vbParams.R')
source('functions/commCatchAges.R')
source('functions/discardAges.R')
is_dir <- atlantis_directory('~/gadget/models/atlantis/OutM45BioV158FMV79_PF')

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
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
atl.had <- atlantis_fg_tracer(is_dir, is_area_data, fg_group) %>% filter(count > 0)
area.had <- 
    atl.had %>% 
    filter(count >= 1) %>%
    mutate(area = as.character(area)) %>%
    group_by(area, year, month, day, age) %>% 
    summarize(count = sum(count))

# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'long'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data
age.catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num.caught = count) %>%
    select(area, year, month, num.caught, age) %>%
    mutate(area = as.character(area))

age.discards <- 
    discardAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num.discard = count) %>%
    select(area, year, month, num.discard, age) %>%
    mutate(area = as.character(area))

area.had.catch <- 
    left_join(area.had, age.catch) %>%
    left_join(age.discards) %>%
    mutate(num.caught = ifelse(is.na(num.caught), 0, num.caught), 
           num.discard = ifelse(is.na(num.discard), 0, num.discard)) %>%
    mutate(total = count + num.caught + num.discard)

# calculate mortality on each age
# I used months 2:6 as these were the most consistent
box <- filter(area.had.catch, area == 'Box14')
months <- filter(box, year == 1948, month %in% 2:6)

m.prop <-
    months %>%
    group_by(age) %>%
    filter(month == 2) %>%
    select(year, age, total) %>%
    rename(init = total) %>%
    right_join(months) %>%
    mutate(vals = total / init)
ggplot(data=m.prop, aes(x=month, y=vals, color=factor(age))) + geom_line() +
    theme_bw() + xlab("Month") + ylab('Proportion of haddock still alive')

## trying to figure out the best fit to data
#box <- filter(area.had.catch, age == 0, area == 'Box14')
#months <- filter(box, year == 1948, month %in% c(2,3,4,5,6))
#plot(total ~ month, months)
#months <- 1:6
#n <- test$count[1]
#m <- 0.0132
#for (i in 2:5) {
#    n[i] <- n[i-1]*exp(-m);
#}
#points(n ~ months, type='b', col='red')


sub <- filter(area.had.catch, month %in% c(2,3,4,5,6))

# function to calculate M
calcM <- function(total, count) {
    m.vector <- NULL;
    for (i in 2:5) {
        m <- -log(total[i] / total[i-1]) + log(count[i-1] / total[i-1]);
        m.vector <- c(m.vector, m);
    }
    return(median(m.vector))
}

# calculate m for each year, area, age class and get mean and median by age
ann.m <-
    sub %>%
    group_by(year, area, age) %>%
    summarize(monthly.m = calcM(total, count)) %>%
    mutate(ann.m = monthly.m*12) %>%
    filter(ann.m > 0)

m.by.age <- 
    ann.m %>%
    group_by(age) %>%
    summarize(mean.m = mean(ann.m),
              median.m = median(ann.m))


# compute a vector of m for each year by taking mean in between age classes
# for gadget model
m.at.age <- m.by.age$median.m
age <- seq(0,18,by=2)
m.odd.ages <- NULL
odd.ages <- NULL
for (i in 2:length(m.at.age)) {
    m.odd <- mean(c(m.at.age[i-1], m.at.age[i]));
    odd.age <- mean(c(age[i-1], age[i]));
    m.odd.ages <- c(m.odd.ages, m.odd);
    odd.ages <- c(odd.ages, odd.age)
}
even <- data.frame(age = age, m = m.at.age)
odd <- data.frame(age = odd.ages, m = m.odd.ages)
m.data <- arrange(rbind(even, odd), age)
