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
cohort_tbl <- 
    is_fg_count %>%
    filter(month == 6, count >= 1) %>%
    select(year, age, count) %>%
    group_by(year, age) %>%
    summarize(count = sum(count)) %>%
    spread(age, count)

# parse out even and odd years
even_yrs <- 
    cohort_tbl %>%
    filter(year %% 2 == 0)

odd_yrs <- 
    cohort_tbl %>%
    filter(year %% 2 == 1)

# calculate z for even and odd years
# this needs to be divided by 2 since they age classes are 2 years,
# which I do below
z_vec <- NULL
even_z_data <- data.frame(age=NULL, z = NULL)
for (i in 2:10) {
    z_yng <- log(even_yrs[-nrow(even_yrs),i]);
    z_old <- log(even_yrs[-1,i+1]);
    z_vec <- sapply(z_yng - z_old, as.vector);
    tmp <- data.frame(age = as.numeric(colnames(even_yrs)[i]),
                      z = as.vector(z_vec));
    even_z_data <- rbind(even_z_data, tmp);
}

z_vec <- NULL
odd_z_data <- data.frame(age=NULL, z = NULL)
for (i in 2:10) {
    z_yng <- log(odd_yrs[-nrow(odd_yrs),i]);
    z_old <- log(odd_yrs[-1,i+1]);
    z_vec <- sapply(z_yng - z_old, as.vector);
    tmp <- data.frame(age = as.numeric(colnames(odd_yrs)[i]),
                      z = as.vector(z_vec));
    odd_z_data <- rbind(odd_z_data, tmp);
}

z_data <- 
    rbind(even_z_data, odd_z_data) %>%
    arrange(age)

## now get f in a similar manner
# get the numbers caught at age and month
is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data
catch_cohort_tbl <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    filter(count >= 1) %>%
    rename(num_caught = count) %>%
    select(area, year, month, num_caught, age) %>%
    mutate(area = as.character(area)) %>%
    group_by(year, age) %>%
    summarize(num_caught = sum(num_caught)) %>%
    spread(age, num_caught)

cohort_mat <- 
    cohort_tbl %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
catch_cohort_mat <- 
    catch_cohort_tbl %>%
    ungroup() %>%
    select(-year) %>%
    as.matrix()
f <- -log(cohort_mat / (catch_cohort_mat[2:66,] + cohort_mat))

# merging the data.frames for z and f - subtracting f from z to get m
# this provides very different values than calculating by month
f_vals <- 
    gather(as.data.frame(f), key=age, value=value) %>%
    mutate(age = as.numeric(age)) %>%
    rename(f = value)
mort_data <- 
    left_join(z_data, f_vals) %>%
    mutate(m = (z/2) - f)
m_vals <- 
    mort_data %>% 
    group_by(age) %>%
    summarize(mean_m = mean(m),
              median_m = median(m))


# you need to add in discards if those are included
# age_discards <- 
#     discardAges(is_dir, is_area_data, fg_group, fishery) %>%
#     filter(count >= 1) %>%
#     rename(num_discard = count) %>%
#     select(area, year, month, num_discard, age) %>%
#     mutate(area = as.character(area))
