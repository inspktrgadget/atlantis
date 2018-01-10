# the following code represents a slightly different way to calculate 
# mortality from the atlantis output
# previously it was done by months in Atlantis and multiplied by 12
# however, I was a bit worried that this wasn't accurate, and using these
# values for m in gadget produced inaccurate models
# this method calculates z from a cohort table and then f from a
# cohort table of catches and then m is calculated as m = z - f
# this provides quite a different result (like an order of 
# magnitude for the older age classes)

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

# make a cohort table
cohort.tbl <- 
    is_fg_count %>%
    filter(month == 6, count >= 1) %>%
    select(year, age, count) %>%
    group_by(year, age) %>%
    summarize(count = sum(count)) %>%
    spread(age, count)

# parse out even and odd years
even.yrs <- 
    cohort.tbl %>%
    filter(year %% 2 == 0)

odd.yrs <- 
    cohort.tbl %>%
    filter(year %% 2 == 1)

# calculate z for even and odd years
# this needs to be divided by 2 since they age classes are 2 years,
# which I do below
z.vec <- NULL
even.z.data <- data.frame(age=NULL, z = NULL)
for (i in 2:10) {
    z.yng <- log(even.yrs[-nrow(even.yrs),i]);
    z.old <- log(even.yrs[-1,i+1]);
    z.vec <- sapply(z.yng - z.old, as.vector);
    tmp <- data.frame(age = as.numeric(colnames(even.yrs)[i]),
                      z = as.vector(z.vec));
    even.z.data <- rbind(even.z.data, tmp);
}

z.vec <- NULL
odd.z.data <- data.frame(age=NULL, z = NULL)
for (i in 2:10) {
    z.yng <- log(odd.yrs[-nrow(odd.yrs),i]);
    z.old <- log(odd.yrs[-1,i+1]);
    z.vec <- sapply(z.yng - z.old, as.vector);
    tmp <- data.frame(age = as.numeric(colnames(odd.yrs)[i]),
                      z = as.vector(z.vec));
    odd.z.data <- rbind(odd.z.data, tmp);
}

z.data <- 
    rbind(even.z.data, odd.z.data) %>%
    arrange(age)

## now get f in a similar manner
# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data
catch.cohort.tbl <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num.caught = count) %>%
    select(area, year, month, num.caught, age) %>%
    mutate(area = as.character(area)) %>%
    group_by(year, age) %>%
    summarize(num.caught = sum(num.caught)) %>%
    spread(age, num.caught)

cohort.mat <- 
    cohort.tbl %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
catch.cohort.mat <- 
    catch.cohort.tbl %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
f <- -log(cohort.mat / (catch.cohort.mat[2:66,] + cohort.mat))

# merging the data.frames for z and f - subtracting f from z to get m
# this provides very different values than calculating by month
f.vals <- 
    gather(as.data.frame(f), key=age, value=value) %>%
    mutate(age = as.numeric(age)) %>%
    rename(f = value)
mort.data <- 
    left_join(z.data, f.vals) %>%
    mutate(m = (z/2) - f)
mean.m <- 
    mort.data %>% 
    group_by(age) %>%
    summarize(mean.m = mean(m),
              median.m = median(m))


# you need to add in discards if those are included
# age.discards <- 
#     discardAges(is_dir, is_area_data, fg_group, fishery) %>%
#     filter(count >= 1) %>%
#     rename(num.discard = count) %>%
#     select(area, year, month, num.discard, age) %>%
#     mutate(area = as.character(area))
