library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mfdb)
library(mfdbatlantis)
library(utils)
library(magrittr)

setwd('~/gadget/models/atlantis')
# source files for both functions and outside data
source('functions/stripAgeLength.R')
source('functions/pauls_atlantis_tracer.R')
source('functions/getHaddockDiscards.R')
source('functions/commCatchAges.R')
source('functions/discardAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('haddock/initdb/getHadLengthVar.R') # source haddock length sd at age group

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

# assemble and import haddock 
fgName <- 'Haddock'
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
#is_fg_count <- pauls_atlantis_tracer(is_dir, is_area_data, fg_group)
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)

# this is a little test to distribute age groups among all ages instead of in cohorts
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

# # this re-distributes weight based on the growth params found in calcGrowth.R
# # NOTE: YOU NEED TO FIX THIS BEFORE USING. WEIGHTS AT YOUNGER AGES ARE TOO HIGH!!!
# wt <-
#     smooth.len %>%
#     #mutate(even.odd = ifelse((age %% 2)==1, 0, 1)) %>%
#     #left_join(lw.min) %>%
#     mutate(test.wt = lw(0.006, 3.10,length))


length_group <- seq(0.5,120.5,by=1)
sigma_per_cohort <- c(had.length.mn.sd$length.sd, 15)
# see ./surveySelectivity.R, ./getHaddockLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 8.37e-06

# Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
is_fg_tracer <- smooth.len[
    #smooth.len$year == attr(is_dir, 'start_year') &
        smooth.len$month %in% c(1),]
is_fg_tracer$species <- fg_group$MfdbCode
is_fg_tracer$areacell <- is_fg_tracer$area
is_fg_tracer$sampling_type <- 'Bio'

# create survey from tracer values
is_fg_survey <- smooth.len[
    smooth.len$area %in% paste('Box', 0:52, sep='') &
        is_fg_count$month %in% c(3,9),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, rep(0.1, length(length_group)), 0)

survey <- filter(is_fg_survey, count > 0)

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/haddock/initdb/haddockSampleNumbers.R
al.survey <- stripAgeLength(survey, 0.44, 0.023)

# Throw away empty rows
is_fg_survey <- al.survey[al.survey$count > 0,]

is_fg_survey$species <- fg_group$MfdbCode
is_fg_survey$areacell <- is_fg_survey$area


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
    select(area, year, month, group, cohort, weight, length, 
           age, count)

# see haddockSampleNumber.R - line 61 to EOF
fleet.suitability <- rep(0.001, length(length_group))
fleet.sigma <- 4.3e-07

comm.catch.samples <- 
    smooth.len.catch %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, fleet.sigma) %>%
    filter(count > 0)

# strip age data out
comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
comm.al.samples$species <- "HAD"
comm.al.samples$sampling_type <- 'CommSurvey'
comm.al.samples$gear <- "LLN"
comm.al.samples <- rename(comm.al.samples, areacell = area, vessel = fishery)
comm.al.samples <- filter(comm.al.samples, count > 0)

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == 'FHA')
is_catch$weight_total <- is_catch$weight_total*1000

# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery, weight = weight_total)
is_catch <- filter(is_catch, weight > 0)
is_catch$gear <- 'LLN'



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


# import discard age structure and get weight/lengths
had.discard.ages <- discardAges(is_dir, is_area_data, fg_group, fishery)

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
    select(area, year, month, group, cohort, weight, length, 
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
