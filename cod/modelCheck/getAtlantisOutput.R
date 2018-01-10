## this code is exactly the same as initdb.R in the initdb directory
## just without the mfdb import lines.
library(plyr)
library(tidyverse)
library(mfdb)
library(mfdbatlantis)

setwd('~/gadget/models/atlantis')
# source files for both functions and outside data
source('functions/stripAgeLength.R')
source('functions/pauls_atlantis_tracer.R')
source('functions/getCodDiscards.R')
source('functions/commCatchAges.R')
source('functions/discardAges.R')
source('functions/getStructN.R')
source('functions/stripFleetAges.R')
source('cod/initdb/getCodLengthVar.R') # source cod length sd at age group

is_dir <- atlantis_directory('~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF')
is_run_options <- atlantis_run_options(is_dir)

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
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


# distribute 2 year atlantis age groups to single year classes
source('functions/calcGrowth.R')
#source('functions/calcWtGrowth.R')
source('functions/parseAges.R')
#source('functions/calcCodMort.R')
source('cod/modelCheck/getAtlantisMort3.R')
# add mortality and parse ages based on m
# I changed m for age classes 0 and 2 to 3x and 2x the
# calculated mortalities, respectively. This just seemed to give a smoother
# age distribution. I'm trying this more as a test to see if gadget
# can fit to the 0 age class better this way. If it works you'll have to 
# come up with a more formal algorithm for computing m at younger ages
age.count <- 
    left_join(is_fg_count, m.func.vals) %>%
    parseAges(.) %>%
    arrange(year, area, month, day, depth, age)

# redistribute lengths based on growth params
smooth.len <- 
    age.count %>% 
    mutate(count = ifelse(count < 1, 0, count)) %>%
    left_join(vbMin) %>%
    mutate(length = ifelse(age == 0, vb(linf, k, (t0-0.20), age),
                           vb(linf, k, t0, age))) %>%
    select(depth, area, year, month, day, group, cohort, weight, length,
           maturity_stage, age, count)


# set up length groups and survey parameters
length_group <-  seq(0.5, 200.5, by=1)
#length_group <-  seq(0,max(is_fg_count$length, na.rm=T),by=10)
sigma_per_cohort <- c(cod.length.mn.sd$length.sd)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 1.5e-04 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 0 # 8.37e-06

# Import entire Cod/Haddock content for one sample point so we can use this as a tracer value
is_fg_tracer <- smooth.len[
    #is_fg_count$year == attr(is_dir, 'start_year') &
        smooth.len$month %in% c(1),]
is_fg_tracer$species <- fg_group$MfdbCode
is_fg_tracer$areacell <- is_fg_tracer$area
is_fg_tracer$sampling_type <- 'Bio'

# create survey from tracer values
is_fg_survey <- smooth.len[
    smooth.len$area %in% paste('Box', 0:52, sep='') &
        smooth.len$month %in% c(3,9),] %>%
    mutate(sampling_type = ifelse(month == 3,
                                  "SprSurvey",
                                  "AutSurvey")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, rep(0.001, length(length_group)), 0)

# ss.selector <- function(len, sel_b, sel_lsm) {
#     1.5e-04 / (1.0 + exp(-sel_b * (len - sel_lsm)))
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
#     mutate(count = count * 1.5e-04)

survey <- filter(is_fg_survey, count >= 1)

# strip ages and lengths from survey to mimic real world data
# see '~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
al.survey <- stripAgeLength(survey, 0.7072256, 0.07072157)


# Throw away empty rows
is_fg_survey <- al.survey[al.survey$count > 0,]

is_fg_survey$species <- fg_group$MfdbCode
is_fg_survey$areacell <- is_fg_survey$area




##############################
# get landings data
##############################

is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- 'bottrawl'
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


# see codSampleNumber.R - line 61 to EOF
fleet.suitability <- rep(0.001444, length(length_group))
fleet.sigma <- 5.7e-07

comm.catch.samples <- 
    smooth.len.catch %>%
    filter(month %in% c(2,3,5,6,7,8,9,11)) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet.suitability, 0) %>%
    filter(count >= 1)

# strip age data out
comm.al.samples <- stripFleetAges(comm.catch.samples, 0.05)
comm.al.samples$species <- "COD"
comm.al.samples$sampling_type <- 'CommSurvey'
comm.al.samples$gear <- "BMT"
comm.al.samples <- rename(comm.al.samples, areacell = area, vessel = fishery)
comm.al.samples <- filter(comm.al.samples, count >= 1)

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == 'FCD')
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
    filter(oc.rate > 1.6) %>% select(year, month, oc.rate)
is_catch <-
    is_catch %>%
    left_join(overcatch.rate) %>%
    mutate(weight = ifelse(is.na(oc.rate), weight_total, weight_total / oc.rate)) %>%
    select(-weight_total, -oc.rate)
    


# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight > 0)
is_catch$gear <- 'BMT'


##############################
# get discards data
##############################

codDiscards <- getCodDiscards(is_dir, is_area_data, fishery)
codDiscards <- mutate(codDiscards, weight_total = weight_total * 1000)
codDiscards <- filter(codDiscards, functional_group == 'FCD')

# Species column that maps to MFDB code
codDiscards$species <- codDiscards$functional_group
levels(codDiscards$species) <- is_functional_groups[match(
    levels(codDiscards$functional_group),
    is_functional_groups$GroupCode), 'MfdbCode']

codDiscards$sampling_type <- "Discard"
codDiscards <- rename(codDiscards, areacell = area, vessel = fishery, weight = weight_total)
codDiscards <- filter(codDiscards, weight > 0)
codDiscards$gear <- 'BMT'