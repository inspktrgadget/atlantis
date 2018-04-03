## this code is exactly the same as initdb.R in the initdb directory
## just without the mfdb import lines.
library(plyr)
library(tidyverse)
library(broom)
library(mfdb)
library(mfdbatlantis)

setwd("~/gadget/models/atlantis")
# source files for both functions and outside data
source("functions/smooth_age_weight.R")
source("functions/stripAgeLength.R")
source("functions/getCodDiscards.R")
source("functions/commCatchAges.R")
source("functions/discardAges.R")
source("functions/getStructN.R")
source("functions/stripFleetAges.R")
source("cod/initdb/getCodLengthVar.R") # source cod length sd at age group

is_dir <- atlantis_directory("~/Dropbox/Paul_IA/OutM57BioV225FMV88_PF")
is_run_options <- atlantis_run_options(is_dir)

is_area_data <- atlantis_read_areas(is_dir)
is_temp <- atlantis_temperature(is_dir, is_area_data)

# Read in all functional groups, assign MFDB shortcodes where possible
is_functional_groups <- atlantis_functional_groups(is_dir)
is_functional_groups$MfdbCode <- vapply(
    mfdb_find_species(is_functional_groups$LongName)["name",],
    function (x) if (length(x) > 0) x[[1]] else as.character(NA), "")

# assemble and import cod 
fgName <- "Cod"
fg_group <- is_functional_groups[c(is_functional_groups$Name == fgName),]
is_fg_count <- atlantis_fg_tracer(is_dir, is_area_data, fg_group)


# distribute atlantis age groups to single year classes and smooth weight among them
z_vals <- c(0.31, 0.325, 0.45, 0.54, 0.6, 0.62, 0.63, 0.63, 0.63, 0.01)
alpha <- fg_group$FLAG_LI_A
beta <- fg_group$FLAG_LI_B

smooth_len <- 
    is_fg_count %>%
    parse_ages(z_vals) %>%
    smooth_atl_weight() %>%
    mutate(length = wt2len(alpha, beta, weight))

# set up length groups and survey parameters
length_group <-  seq(0.5, 200.5, by=1)
sigma_per_cohort <- c(2.5,2.5,2.5,2.5,3,3,3,3,3,3.5,3.5,3.5,3.5,3.5,4,4,4,4.5,4.5,5)
#sigma_per_cohort <- c(cod.length.mn.sd$length.sd)
# see ./surveySelectivity.R, ./getCodLengthVar.R-lines 49-EOF for suitability params
sel_lsm <- 49
sel_b <- 0.046 # Controls the shape of the curve
survey_suitability <- 0.001 / (1.0 + exp(-sel_b * (length_group - sel_lsm)))
survey_sigma <- 0 # 8.37e-06

# create survey from tracer values
# keep spr survey as 4 because of gadget order of operations
# keep aut survey as 9 because of recruitment jumps in atlantis
is_fg_survey <- smooth_len[
    smooth_len$area %in% paste("Box", 0:52, sep="") &
        smooth_len$month %in% c(4,9),] %>%
    mutate(sampling_type = ifelse(month == 4,
                                  "SprSurveyTotals",
                                  "AutSurveyTotals")) %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, survey_suitability, survey_sigma) 

survey <- filter(is_fg_survey, count >= 1)
survey$species <- fg_group$MfdbCode
survey$areacell <- survey$area

# strip ages and lengths from survey to mimic real world data
# see "~gadget/gadget-models/atlantis/cod/initdb/codSampleNumbers.R
al_survey <- stripAgeLength(survey, 0.75, 0.25)
al_survey$length <- round(al_survey$length)
al_survey$weight <- round(al_survey$weight)



##############################
# get landings data
##############################

is_fisheries <- atlantis_fisheries(is_dir)
fisheryCode <- "bottrawl"
fishery <- is_fisheries[is_fisheries$Code == fisheryCode,]

# to set up as age structured data - note that this returns values in kg, not tons
age_catch <- 
    commCatchAges(is_dir, is_area_data, fg_group, fishery) %>%
    mutate(area = as.character(area)) %>%
    rename(group = functional_group)
wl <- getStructN(is_dir, is_area_data, fg_group)

age_catch_wl <- left_join(age_catch, wl)

# parse the catch age-length data to single year classes

# for catches we need to also take selectivity into account when parsing
# I painstakingly, by trial and error, found some parameters for parsing_selectivity()
# that are "good enough", these are by no means optimized
flt_lsm <- 3.25
flt_b <- 1.45 # Controls the shape of the curve
flt_int <- -0.2
parsing_selectivity <- function(z, age) {
    ((z - flt_int) / (1 + exp(-flt_b * (age - flt_lsm)))) + flt_int
}
fleet_z_vals <- parsing_selectivity(z_vals, seq(0, 18, by = 2))
parsed_age_catch_wl <- 
    age_catch_wl %>% 
    parse_ages(fleet_z_vals) %>%
    smooth_atl_weight() %>%
    mutate(length = wt2len(alpha, beta, weight))

# see codSampleNumber.R - line 61 to EOF
fleet_suitability <- rep(0.001, length(length_group))
fleet_sigma <- 5.7e-07

# trying to avoid adding error in lengths here as well

comm_catch_samples <- 
    smooth_len_catch %>%
    atlantis_tracer_add_lengthgroups(length_group, sigma_per_cohort) %>%
    atlantis_tracer_survey_select(length_group, fleet_suitability, 0) %>%
    filter(count >= 1)

# strip age data out
comm_al_samples <- stripFleetAges(comm_catch_samples, 0.25)
comm_al_samples$species <- "COD"
comm_al_samples$sampling_type <- "CommSurvey"
comm_al_samples$gear <- "BMT"
comm_al_samples <- rename(comm_al_samples, areacell = area, vessel = fishery)
comm_al_samples <- filter(comm_al_samples, count >= 1)

if (mfdb_import) {
    mfdb_import_survey(mdb,
                       comm_al_samples,
                       data_source=paste0("atlantisFishery_", fisheryCode, "_commSamples"))
    print("Commercial survey data imported")
}

# the following is to get landings data without age structure
is_catch <- atlantis_fisheries_catch(is_dir, is_area_data, fishery)
is_catch <- filter(is_catch, functional_group == "FCD")
is_catch$weight_total <- is_catch$weight_total*1000


# Species column that maps to MFDB code
is_catch$species <- is_catch$functional_group
levels(is_catch$species) <- is_functional_groups[match(
    levels(is_catch$functional_group),
    is_functional_groups$GroupCode), "MfdbCode"]

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight_total > 0)
is_catch$gear <- "BMT"

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
    is_functional_groups$GroupCode), "MfdbCode"]

is_catch$sampling_type <- "Cat"
is_catch <- rename(is_catch, areacell = area, vessel = fishery)
is_catch <- filter(is_catch, weight > 0)
is_catch$gear <- "BMT"


##############################
# get discards data
##############################

codDiscards <- getCodDiscards(is_dir, is_area_data, fishery)
codDiscards <- mutate(codDiscards, weight_total = weight_total * 1000)
codDiscards <- filter(codDiscards, functional_group == "FCD")

# Species column that maps to MFDB code
codDiscards$species <- codDiscards$functional_group
levels(codDiscards$species) <- is_functional_groups[match(
    levels(codDiscards$functional_group),
    is_functional_groups$GroupCode), "MfdbCode"]

codDiscards$sampling_type <- "Discard"
codDiscards <- rename(codDiscards, areacell = area, vessel = fishery, weight = weight_total)
codDiscards <- filter(codDiscards, weight > 0)
codDiscards$gear <- "BMT"