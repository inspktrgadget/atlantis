library(plyr)
library(dplyr)
library(tidyr)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)

setwd('~/gadget/models/atlantis')
# source files for both functions and outside data
source('functions/stripAgeLength.R')
source('functions/getHaddockDiscards.R')
source('functions/commCatchAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('functions/discardAges.R')
source('haddock/initdb/getHadLengthVar.R') # source cod length sd at age group
source('haddock/functions/getHaddockCatches.R')


mdb <- mfdb('Atlantis-Iceland')

is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM45BioV158FMV79_PF')
is_run_options <- atlantis_run_options(is_dir)

# Read in areas / surface temperatures, insert into mfdb
is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)['name',],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")


# assemble and import haddock 
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


# the following code distributes age groups among ages each year instead of cohorts
source('functions/calcGrowth.R')
source('functions/calcWtGrowth.R')
source('functions/parseAges.R')
source('functions/calcHadMort.R')

# add mortality and parse ages based on m
is_fg_count <- left_join(is_fg_count, m.by.age)
age.count <- parseAges(is_fg_count) %>% 
    arrange(year, area, month, day, depth, age)

# this re-distributes lengths based on the growth params found in calcGrowth.R
smooth.len <- 
    age.count %>%
    filter(count >= 1) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb.simple(linf, k, age, (t0-0.1)),
                           vb.simple(linf, k, age, t0))) %>%
    select(depth, area, year, month, day, group, cohort, weight, length, 
           maturity_stage, age, count)

# this re-distributes weight based on the growth params found in calcGrowth.R
# NOTE: YOU NEED TO FIX THIS BEFORE USING. WEIGHTS AT YOUNGER AGES ARE TOO HIGH!!!
# wt <-
#     age.count %>%
#     filter(count >= 1) %>%
#     left_join(exp.growth.min) %>%
#     mutate(test.wt = exp.growth(age, max.wt, a, c))


length_group <-  seq(0.5, 120.5, by=1)
sigma_per_cohort <- c(had.length.mn.sd$length.sd, 15)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 0
#survey_sigma <- 8.37e-06

# Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
is_fg_tracer <- smooth.len[
    # is_fg_count$year == attr(is_dir, 'start_year') &
        smooth.len$month %in% c(1),]
is_fg_tracer$species <- fg_group$MfdbCode
is_fg_tracer$areacell <- is_fg_tracer$area
is_fg_tracer$sampling_type <- 'Bio'
is_fg_tracer <- filter(is_fg_tracer, count >= 1)
mfdb_import_survey(mdb, is_fg_tracer, data_source = paste0('atlantis_tracer_', fg_group$Name))

# create survey from tracer values
is_fg_survey <- smooth.len[
    smooth.len$area %in% paste('Box', 0:52, sep='') &
        smooth.len$month %in% c(3,11),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, rep(0.1, length(length_group)), 0)

# ss.selector <- function(len, sel_b, sel_lsm) {
#     5e-04 / (1.0 + exp(-sel_b * (len - sel_lsm)))
# }
# 
# test.survey <- 
#     is_fg_count %>%
#     filter(area %in% paste('Box', 0:52, sep=''),
#            month %in% c(3,10),
#            count >= 1) %>%
#     mutate(sampling_type = ifelse(month == 3,
#                                   "SprSurvey",
#                                   "AutSurvey")) %>%
#     mutate(count = round(count * 5e-04))

survey <- filter(is_fg_survey, count >= 1)

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
al.survey <- stripAgeLength(survey, 0.44, 0.023)
al.survey$length <- round(al.survey$length)
al.survey$weight <- round(al.survey$weight)

# Throw away empty rows
al.survey <- al.survey[al.survey$count >= 1,]
al.survey$species <- fg_group$MfdbCode
al.survey$areacell <- al.survey$area
mfdb_import_survey(mdb, al.survey, data_source = paste0('atlantis_survey_', fg_group$Name))


##############################
# turning to importing catches
##############################

## don't want to do stomach contents yet
# # Fetch consumption and tracer indexes for functional group
# consumption <- atlantis_fg_tracer(
#     is_dir,
#     is_area_data,
#     fg_group = fg_group,
#     consumption = TRUE)
# 
# # Only survey the first quarter, and in 3 boxes
# consumption <- consumption[consumption$month == 1 & consumption$area %in% c("Box20", "Box21", "Box22"),]
# # Assume we only catch 0.0001% of possible available
# consumption$count <- round(consumption$count * 0.000001)
# 
# # Convert this into the 2 data.frames import_stomach requires
# stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FCD = 'COD'
# ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]]
# ))
# mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Cod"))
# 
# stomach <- atlantis_stomach_content(is_dir, consumption, predator_map = c(
#     FHA = 'HAD'
# ), prey_map = c(
#     # We're only interested in 2 species
#     FHE = mfdb_find_species('Clupea Harengus')['name',][[1]],
#     FCA = mfdb_find_species('Capelin')['name',][[1]],
#     PWN = mfdb_find_species('Pandalus borealis')['name',][[1]],
#     ZL = mfdb_find_species('euphausia')['name',][[1]]
# ))
# mfdb_import_stomach(mdb, stomach$predator_data, stomach$prey_data, data_source = paste0("stomach_Haddock"))


is_fisheries <- atlantis_fisheries(is_dir)
mfdb_import_vessel_taxonomy(mdb, data.frame(
    id = is_fisheries$Index,
    name = is_fisheries$Code,
    full_name = is_fisheries$Name,
    stringsAsFactors = FALSE))

fisheryCode <- 'long'
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data - note that this returns values in kg, not tons
age.catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    mutate(area = as.character(area)) %>%
    rename(group = functional_group)
wl <- getStructN(is_dir, is_area_data, fg_group)

age.catch.wl <- left_join(age.catch, wl)

# parse the catch age-length data to single year classes
age.catch.wl <- left_join(age.catch.wl, m.by.age)
parsed.age.catch.wl <- 
    parseCatchAges(age.catch.wl) %>% 
    arrange(year, area, month, age)

smooth.len.catch <- 
    parsed.age.catch.wl %>%
    filter(count >= 1) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb.simple(linf, k, age, (t0-0.1)),
                           vb.simple(linf, k, age, t0))) %>%
    select(area, year, month, group, fishery, cohort, weight, length, 
           age, count)

# see haddockSampleNumber.R - line 61 to EOF
fleet.suitability <- rep(0.001, length(length_group))
fleet.sigma <- 0
#fleet.sigma <- 4.3e-07

# testing out using just straight samples instead of adding error

comm.catch.samples <- 
    smooth.len.catch %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma) %>%
    filter(count >= 1)

# comm.catch.samples <-
#     age.catch.wl %>%
#     mutate(count = round(count * 0.001),
#            length = round(length),
#            weight = round(weight)) %>%
#     filter(count >= 1)


# strip age data out
comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
comm.al.samples$species <- "HAD"
comm.al.samples$sampling_type <- 'CommSurvey'
comm.al.samples$gear <- "LLN"
comm.al.samples <- rename(comm.al.samples, areacell = area, vessel = fishery)
comm.al.samples <- filter(comm.al.samples, count >= 1)

mfdb_import_survey(mdb,
                   comm.al.samples,
                   data_source=paste0("atlantisFishery_", fisheryCode, "_commSamples"))

# the following is to get landings data without age structure
is_catch <- getHaddockCatches(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == 'FHA')
is_catch$weight_total <- is_catch$weight_total*1000

########################################
## the following code is to correct
## the spikes that occur every 7-8 years
########################################
weird.yrs <- data.frame(year = sort(c(seq(1951,2011,15), seq(1959, 2004, 15))),
                        months = c(10,4,10,4,10,4,10,4,10))
annual.wt <-
    is_catch %>% 
    filter(!(month %in% c(4,10))) %>%
    group_by(year, month) %>%
    summarize(wt = sum(weight_total)) %>%
    group_by(year) %>% summarize(mn.ann.wt = mean(wt))
overcatch.rate <-
    is_catch %>%
    group_by(year, month) %>%
    summarize(monthly.wt = sum(weight_total)) %>%
    left_join(annual.wt) %>%
    mutate(oc.rate = monthly.wt / mn.ann.wt) %>%
    filter(oc.rate > 1.5) %>% select(year, month, oc.rate)
reg_catch <-
    is_catch %>%
    left_join(overcatch.rate) %>%
    mutate(weight = ifelse(is.na(oc.rate), weight_total, weight_total / oc.rate)) %>%
    select(-weight_total, -oc.rate)
is_catch <- reg_catch


# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight > 0)
is_catch$gear <- 'LLN'

mfdb_import_survey(mdb, 
                   is_catch, 
                   data_source = paste0("atlantisFishery_", fisheryCode))


##############################
# get discards data
##############################

hadDiscards <- getHaddockDiscards(is_dir, is_area_data, fishery)
hadDiscards <- mutate(hadDiscards, weight_total = weight_total * 1000)
hadDiscards <- filter(hadDiscards, functional_group == 'FHA')

# Species column that maps to MFDB code
hadDiscards$species <- hadDiscards$functional_group
levels(hadDiscards$species) <- is_functional_groups[match(
    levels(hadDiscards$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

hadDiscards$sampling_type <- "Discard"
hadDiscards <- rename(hadDiscards, areacell = area, vessel = fishery, weight = weight_total)
hadDiscards <- filter(hadDiscards, weight > 0)
hadDiscards$gear <- 'LLN'

mfdb_import_survey(mdb, 
                   hadDiscards, 
                   data_source = paste0("atlantisFishery_", fisheryCode, "_Discard"))


# import discard age structure and get weight/lengths
had.discard.ages <- 
    discardAges(is_dir, is_area_data, fg_group, fishery) %>%
    mutate(area = as.character(area)) %>%
    rename(group = functional_group)

# to set up as age structured data - note that this returns values in kg, not tons
age.discard.wl <- left_join(had.discard.ages, wl)

# parse the catch age-length data to single year classes
age.discard.wl <- left_join(age.discard.wl, m.by.age)
parsed.age.discard.wl <- 
    parseCatchAges(age.discard.wl) %>% 
    arrange(year, area, month, age)

smooth.len.discard <- 
    parsed.age.discard.wl %>%
    filter(count >= 1) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb.simple(linf, k, age, (t0-0.1)),
                           vb.simple(linf, k, age, t0))) %>%
    select(area, year, month, group, fishery, cohort, weight, length, 
           age, count)

# taking survey of ages/lengths of discards data
discard.samples <-
    smooth.len.discard %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma) %>%
    filter(count >= 1)

# strip age data out
discard.al.samples <- stripFleetAges(discard.samples, 0.05)
discard.al.samples$species <- "HAD"
discard.al.samples$sampling_type <- 'DiscardSurvey'
discard.al.samples$gear <- "LLN"
discard.al.samples <- rename(discard.al.samples, areacell = area, vessel = fishery)
discard.al.samples <- filter(discard.al.samples, count >= 1)

mfdb_import_survey(mdb,
                   discard.al.samples,
                   data_source=paste0("atlantisFishery_", fisheryCode, "_discardSamples"))

